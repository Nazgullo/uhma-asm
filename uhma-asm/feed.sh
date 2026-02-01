#!/bin/bash
#
# Universal UHMA Feeding Script
# Replaces: feed_chunks.sh, feed_interactive.sh, feed_small.sh, feed_visible.sh,
#           train_loop.sh, train.sh, train_uhma.sh, mastery_loop.sh
#

set -euo pipefail

# Defaults
CORPUS_DIR="corpus/"
PAUSE_BETWEEN_FILES=5
CONSOLIDATE_INTERVAL=$((30 * 60))  # 30 minutes in seconds
SAVE_INTERVAL=$((30 * 60))         # 30 minutes in seconds
FILE_ORDER="alpha"
MAX_CYCLES=0
SELF_LEARN=0
DRY_RUN=0
LIVE_MODE=0
LIVE_PAUSE=10  # seconds to wait for user input before live mode

# UHMA ports
FEED_IN=9999
FEED_OUT=9998
QUERY_IN=9997
QUERY_OUT=9996

# State
LAST_CONSOLIDATE=0
LAST_SAVE=0
SAVE_NUM=0
CYCLE=0
CONSECUTIVE_FAILURES=0
DRAINER_PIDS=""

# Logging
LOG_FILE="feed.log"

log() {
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    echo "[$timestamp] $*" | tee -a "$LOG_FILE"
}

alert() {
    local message="$1"
    local timestamp=$(date '+%Y-%m-%d %H:%M:%S')

    echo "[$timestamp] ALERT: $message" >> ALERT_CLAUDE.txt
    echo "!!! ALERT: $message !!!"
    log "ALERT: $message"

    # Don't spawn multiple Claudes
    if [ -f claude_helper.pid ]; then
        if kill -0 $(cat claude_helper.pid) 2>/dev/null; then
            log "Claude already working on it"
            return
        fi
    fi

    # Spawn Claude to fix it
    claude --dangerously-skip-permissions \
        "UHMA training alert at $timestamp: $message

Read ALERT_CLAUDE.txt and feed.log for context.
Check: pgrep uhma, nc -z localhost 9999
Diagnose and fix. Training continues when fixed." &

    echo $! > claude_helper.pid
}

# Start persistent drainers for all output ports (prevents UHMA blocking)
start_drainers() {
    log "Starting output port drainers..."

    # Drain FEED_OUT to log file
    while true; do
        nc localhost $FEED_OUT >> /tmp/uhma_feed.out 2>/dev/null || sleep 1
    done &
    DRAINER_PIDS="$DRAINER_PIDS $!"

    # Drain QUERY_OUT
    while true; do
        nc localhost $QUERY_OUT >> /tmp/uhma_query.out 2>/dev/null || sleep 1
    done &
    DRAINER_PIDS="$DRAINER_PIDS $!"

    # Drain DEBUG_OUT
    while true; do
        nc localhost 9994 >> /tmp/uhma_debug.out 2>/dev/null || sleep 1
    done &
    DRAINER_PIDS="$DRAINER_PIDS $!"

    sleep 1  # Let drainers connect
    log "Drainers started: $DRAINER_PIDS"
}

stop_drainers() {
    for pid in $DRAINER_PIDS; do
        kill $pid 2>/dev/null
    done
    DRAINER_PIDS=""
}

# Send command to UHMA (fire and forget - drainers handle output)
uhma_send() {
    local in_port="$1"
    local cmd="$2"

    # Simple send with 2s timeout - drainers handle output
    # Use || true to ignore timeout exit code (124)
    echo "$cmd" | timeout 2 nc -N localhost "$in_port" 2>/dev/null || true
    return 0  # Always succeed - fire and forget
}

feed_cmd() {
    local cmd="$1"
    uhma_send "$FEED_IN" "$cmd"
}

query_cmd() {
    local cmd="$1"
    uhma_send "$QUERY_IN" "$cmd"
}

# Get UHMA maturity stage (0=Infant, 1=Child, 2=Adolescent, 3=Adult)
get_maturity_stage() {
    local status_out
    status_out=$(timeout 5 nc localhost "$QUERY_OUT" 2>/dev/null &
        sleep 0.3
        echo "status" | timeout 2 nc -N localhost "$QUERY_IN" 2>/dev/null || true
        sleep 2
        cat /tmp/uhma_query.out 2>/dev/null | tail -50)

    # Parse "Stage: N" from status output
    local stage
    stage=$(echo "$status_out" | grep -oP 'Stage:\s*\K\d+' | head -1)
    echo "${stage:-0}"
}

# Get files to feed based on maturity stage
get_exploration_files() {
    local stage="$1"
    local files=""

    case "$stage" in
        0)  # Infant - only uhma-asm folder
            files=$(find /home/peter/Desktop/STARWARS/uhma-asm -maxdepth 2 \
                -type f \( -name "*.txt" -o -name "*.asm" -o -name "*.md" \) \
                2>/dev/null | shuf | head -50)
            ;;
        1)  # Child - add Desktop
            files=$(find /home/peter/Desktop -maxdepth 3 \
                -type f \( -name "*.txt" -o -name "*.md" -o -name "*.py" -o -name "*.sh" \) \
                ! -path "*/\.*" 2>/dev/null | shuf | head -100)
            ;;
        2)  # Adolescent - add home folder
            files=$(find /home/peter -maxdepth 3 \
                -type f \( -name "*.txt" -o -name "*.md" -o -name "*.py" -o -name "*.sh" -o -name "*.c" -o -name "*.h" \) \
                ! -path "*/\.*" ! -path "*/.cache/*" ! -path "*/.local/*" \
                2>/dev/null | shuf | head -200)
            ;;
        *)  # Adult - broader exploration
            files=$(find /home/peter -maxdepth 4 \
                -type f \( -name "*.txt" -o -name "*.md" -o -name "*.py" -o -name "*.sh" -o -name "*.c" -o -name "*.h" -o -name "*.json" -o -name "*.yaml" -o -name "*.toml" \) \
                ! -path "*/\.*" ! -path "*/.cache/*" ! -path "*/.local/*" ! -path "*/node_modules/*" \
                2>/dev/null | shuf | head -300)
            ;;
    esac

    echo "$files"
}

# Live autonomous mode - self-feeding based on maturity
live_mode() {
    log "=== ENTERING LIVE MODE ==="
    log "UHMA will now self-feed and consolidate autonomously"

    # Disable batch mode for autonomous operation
    feed_cmd "batch"
    sleep 1

    local live_cycle=0
    local last_stage=-1

    while true; do
        live_cycle=$((live_cycle + 1))

        # Check maturity stage
        local stage
        stage=$(get_maturity_stage)

        if [ "$stage" != "$last_stage" ]; then
            log "=== MATURITY STAGE: $stage ==="
            case "$stage" in
                0) log "  Infant - exploring uhma-asm folder" ;;
                1) log "  Child - expanding to Desktop" ;;
                2) log "  Adolescent - exploring home folder" ;;
                *) log "  Adult - broad exploration enabled" ;;
            esac
            last_stage="$stage"
        fi

        log "=== LIVE CYCLE $live_cycle (Stage $stage) ==="

        # Get files appropriate for current maturity
        local files
        files=$(get_exploration_files "$stage")

        local file_count
        file_count=$(echo "$files" | grep -c . || echo 0)

        if [ "$file_count" -eq 0 ]; then
            log "No files found for exploration, waiting..."
            sleep 30
            continue
        fi

        log "Exploring $file_count files..."

        local file_num=0
        while IFS= read -r file; do
            [ -f "$file" ] || continue
            file_num=$((file_num + 1))

            # Skip very large files (>1MB)
            local fsize
            fsize=$(stat -c%s "$file" 2>/dev/null || echo 0)
            if [ "$fsize" -gt 1048576 ]; then
                continue
            fi

            log "  [$file_num] $(basename "$file")"
            feed_cmd "eat $file"

            # Shorter pause in live mode
            sleep 2

            # Periodic consolidation (every 20 files)
            if [ $((file_num % 20)) -eq 0 ]; then
                log "  Mini-consolidation..."
                feed_cmd "observe"
                sleep 5
            fi

            maybe_save
        done <<< "$files"

        # End of live cycle - consolidate
        log "=== LIVE CYCLE $live_cycle COMPLETE ==="
        feed_cmd "observe"
        sleep 10
        feed_cmd "dream"
        sleep 10
        feed_cmd "save live_$live_cycle"
        sleep 2

        # Cleanup old live saves (keep last 5)
        ls -t live_* 2>/dev/null | tail -n +6 | xargs -r rm -f || true

        LAST_CONSOLIDATE=$(date +%s)

        # Brief pause before next cycle
        sleep 5
    done
}

start_uhma() {
    if pgrep -f './uhma' > /dev/null || nc -z localhost $FEED_IN 2>/dev/null; then
        log "UHMA already running (batch mode patched in binary)"
        start_drainers
        return 0
    fi

    log "Starting UHMA..."
    ./uhma < /dev/null > run.log 2>&1 &
    echo $! > feed.pid

    # Wait for port
    for i in {1..20}; do
        nc -z localhost $FEED_IN 2>/dev/null && break
        sleep 0.5
    done

    if ! nc -z localhost $FEED_IN; then
        alert "UHMA failed to start - port $FEED_IN not responding"
        exit 1
    fi

    # NOTE: batch_mode=1 patched in binary, no toggle needed
    log "UHMA started (batch mode enabled via binary patch)"

    # Start drainers to keep output ports clear
    start_drainers
}

feed_file() {
    local filepath="$1"

    log "Feeding: $filepath"

    if [ "$DRY_RUN" = "1" ]; then
        log "  [DRY-RUN] Would feed: $filepath"
        return 0
    fi

    feed_cmd "eat $filepath"
    # Give UHMA time to process
    sleep 2
    return 0
}

maybe_consolidate() {
    local now
    now=$(date +%s)
    local elapsed=$((now - LAST_CONSOLIDATE))

    if [ $elapsed -ge $CONSOLIDATE_INTERVAL ]; then
        log "=== CONSOLIDATE (${elapsed}s elapsed) ==="

        if [ "$DRY_RUN" = "1" ]; then
            log "  [DRY-RUN] Would run: observe, dream"
            LAST_CONSOLIDATE=$(date +%s)
            return 0
        fi

        feed_cmd "observe"
        sleep 10

        feed_cmd "dream"
        sleep 10

        query_cmd "status"
        LAST_CONSOLIDATE=$(date +%s)
        log "=== CONSOLIDATE DONE ==="
    fi
}

maybe_save() {
    local now
    now=$(date +%s)
    local elapsed=$((now - LAST_SAVE))

    if [ $elapsed -ge $SAVE_INTERVAL ]; then
        SAVE_NUM=$((SAVE_NUM + 1))
        log "=== SAVE checkpoint_$SAVE_NUM ==="

        if [ "$DRY_RUN" = "1" ]; then
            log "  [DRY-RUN] Would save: checkpoint_$SAVE_NUM"
            LAST_SAVE=$(date +%s)
            return 0
        fi

        feed_cmd "save checkpoint_$SAVE_NUM"
        LAST_SAVE=$(date +%s)

        # Keep only last 3
        ls -t checkpoint_* 2>/dev/null | tail -n +4 | xargs -r rm -f || true
    fi
}

usage() {
    cat <<EOF
Usage: $0 [OPTIONS]

Universal UHMA feeding script. Feeds corpus files, manages consolidation cycles,
handles checkpoints, and spawns Claude for error recovery.

Options:
  --corpus DIR        Directory with .txt files (default: corpus/)
  --pause N           Seconds between files (default: 5)
  --consolidate N     Minutes between observe+dream (default: 30)
  --save-every N      Minutes between checkpoints (default: 30)
  --order ORDER       alpha|random|reverse (default: alpha)
  --cycles N          Number of cycles, 0=infinite (default: 0)
  --self-learn        Feed UHMA's responses back
  --mastery           Alias for --cycles 0 --self-learn
  --live              Enter live autonomous mode after training
  --live-pause N      Seconds to wait for user input before live mode (default: 10)
  --dry-run           Show what would happen
  --help              Show this help

Examples:
  $0 --dry-run                    # Preview without running
  $0 --cycles 1                   # One full cycle through corpus
  $0 --cycles 1 --live            # Train then enter live autonomous mode
  $0 --mastery                    # Infinite self-learning mode
  $0 --corpus data/ --pause 10    # Custom corpus, slower pace

Live Mode:
  After training completes, UHMA enters autonomous exploration:
  - Stage 0 (Infant): Explores uhma-asm folder only
  - Stage 1 (Child): Expands to Desktop
  - Stage 2 (Adolescent): Explores home folder
  - Stage 3+ (Adult): Broad exploration with more file types
EOF
    exit 0
}

parse_args() {
    while [ $# -gt 0 ]; do
        case "$1" in
            --corpus)
                CORPUS_DIR="$2"
                shift 2
                ;;
            --pause)
                PAUSE_BETWEEN_FILES="$2"
                shift 2
                ;;
            --consolidate)
                CONSOLIDATE_INTERVAL=$(($2 * 60))
                shift 2
                ;;
            --save-every)
                SAVE_INTERVAL=$(($2 * 60))
                shift 2
                ;;
            --order)
                FILE_ORDER="$2"
                shift 2
                ;;
            --cycles)
                MAX_CYCLES="$2"
                shift 2
                ;;
            --self-learn)
                SELF_LEARN=1
                shift
                ;;
            --mastery)
                MAX_CYCLES=0
                SELF_LEARN=1
                shift
                ;;
            --live)
                LIVE_MODE=1
                shift
                ;;
            --live-pause)
                LIVE_PAUSE="$2"
                shift 2
                ;;
            --dry-run)
                DRY_RUN=1
                shift
                ;;
            --help|-h)
                usage
                ;;
            *)
                echo "Unknown option: $1"
                usage
                ;;
        esac
    done
}

cleanup() {
    log "Interrupted - saving..."
    if [ "$DRY_RUN" != "1" ]; then
        feed_cmd "save interrupted"
        sleep 2
    fi
    stop_drainers
    exit 0
}

main() {
    parse_args "$@"

    log "=== UHMA Universal Feeder ==="
    log "Corpus: $CORPUS_DIR"
    log "Pause: ${PAUSE_BETWEEN_FILES}s between files"
    log "Consolidate: every $((CONSOLIDATE_INTERVAL / 60))m"
    log "Save: every $((SAVE_INTERVAL / 60))m"
    log "Order: $FILE_ORDER"
    log "Self-learn: $SELF_LEARN"
    log "Dry-run: $DRY_RUN"

    if [ ! -d "$CORPUS_DIR" ]; then
        log "ERROR: Corpus directory not found: $CORPUS_DIR"
        exit 1
    fi

    if [ "$DRY_RUN" != "1" ]; then
        start_uhma
    else
        log "[DRY-RUN] Would start UHMA and enable batch mode"
    fi

    LAST_CONSOLIDATE=$(date +%s)
    LAST_SAVE=$(date +%s)
    SAVE_NUM=0
    CYCLE=0
    CONSECUTIVE_FAILURES=0

    while true; do
        CYCLE=$((CYCLE + 1))

        if [ "$MAX_CYCLES" -gt 0 ] && [ "$CYCLE" -gt "$MAX_CYCLES" ]; then
            log "Completed $MAX_CYCLES cycles"
            break
        fi

        log "=== CYCLE $CYCLE ==="

        # Get files
        local files
        case "$FILE_ORDER" in
            alpha)   files=$(find "$CORPUS_DIR" -name "*.txt" -type f | sort) ;;
            random)  files=$(find "$CORPUS_DIR" -name "*.txt" -type f | shuf) ;;
            reverse) files=$(find "$CORPUS_DIR" -name "*.txt" -type f | sort -r) ;;
            *)       files=$(find "$CORPUS_DIR" -name "*.txt" -type f | sort) ;;
        esac

        local file_count
        file_count=$(echo "$files" | grep -c . || echo 0)

        if [ "$file_count" -eq 0 ]; then
            log "No .txt files found in $CORPUS_DIR"
            exit 1
        fi

        local file_num=0

        while IFS= read -r file; do
            [ -f "$file" ] || continue
            file_num=$((file_num + 1))

            log "--- File $file_num/$file_count: $(basename "$file") ---"

            if feed_file "$file"; then
                CONSECUTIVE_FAILURES=0
            else
                CONSECUTIVE_FAILURES=$((CONSECUTIVE_FAILURES + 1))
                if [ $CONSECUTIVE_FAILURES -ge 5 ]; then
                    alert "5 consecutive failures"
                    CONSECUTIVE_FAILURES=0
                fi
            fi

            sleep "$PAUSE_BETWEEN_FILES"
            maybe_consolidate
            maybe_save
        done <<< "$files"

        # End of cycle
        log "=== CYCLE $CYCLE COMPLETE ==="

        if [ "$DRY_RUN" != "1" ]; then
            log "Running observe..."
            feed_cmd "observe"
            sleep 10
            log "Running dream..."
            feed_cmd "dream"
            sleep 10
            log "Saving cycle_$CYCLE..."
            feed_cmd "save cycle_$CYCLE"
            sleep 2

            # Keep only last 3 cycle saves
            ls -t cycle_* 2>/dev/null | tail -n +4 | xargs -r rm -f || true
        else
            log "[DRY-RUN] Would run: observe, dream, save cycle_$CYCLE"
        fi

        LAST_CONSOLIDATE=$(date +%s)
    done

    if [ "$DRY_RUN" != "1" ]; then
        log "Saving final..."
        feed_cmd "save final"
        sleep 2
    fi

    # Check if we should enter live mode
    if [ "$LIVE_MODE" = "1" ] && [ "$MAX_CYCLES" -gt 0 ] && [ "$DRY_RUN" != "1" ]; then
        log ""
        log "Training complete. Entering LIVE MODE in $LIVE_PAUSE seconds..."
        log "Press Ctrl+C to exit, or wait for autonomous exploration."
        log ""

        # Wait for user input with timeout
        if read -t "$LIVE_PAUSE" -r user_input 2>/dev/null; then
            if [ -n "$user_input" ]; then
                log "User input received: $user_input"
                log "Exiting instead of live mode."
                stop_drainers
                log "=== DONE ==="
                return
            fi
        fi

        # No input - enter live mode
        live_mode
        # live_mode runs forever, but if interrupted:
    fi

    log "Stopping drainers..."
    stop_drainers
    log "=== DONE ==="
}

trap cleanup INT TERM

main "$@"
