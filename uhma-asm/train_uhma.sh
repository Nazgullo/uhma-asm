#!/bin/bash
# train_uhma.sh — Feed corpus with interleaved self-learning
#
# Flow per file:
#   1. Chunk file into ~40 lines (stay under dream trigger)
#   2. Feed each chunk, capture UHMA's stdout
#   3. After file done, feed captured stdout back (self-learning)
#   4. Move to next file
#
# Consolidation: observe+dream every ~30 minutes (not per file)

cd /home/peter/Desktop/STARWARS/uhma-asm

CHUNK_LINES=40
PAUSE_BETWEEN_CHUNKS=2
CONSOLIDATE_INTERVAL=1800  # 30 minutes in seconds

# Start UHMA headless if not running
if ! pgrep -x uhma > /dev/null; then
    pkill -9 uhma 2>/dev/null
    sleep 1
    ./uhma < /dev/null > /dev/null 2>&1 &
    UHMA_PID=$!
    echo "Started UHMA PID: $UHMA_PID"
    sleep 3
fi

# Temp files
CHUNK_FILE="/tmp/uhma_chunk.txt"
RUN_LOG="/tmp/uhma_run_log.txt"

# Track consolidation time
LAST_CONSOLIDATE=$(date +%s)

log() { echo "[$(date +%H:%M:%S)] $1"; }

# Feed a chunk, capture output
feed_chunk() {
    local chunk="$1"
    echo "eat $chunk" | nc -q 1 localhost 9999 2>/dev/null
    # Small delay for processing
    sleep $PAUSE_BETWEEN_CHUNKS
    # Capture any output from query channel
    timeout 1 nc -w 1 localhost 9996 2>/dev/null || true
}

# Check if consolidation needed (every 30 min)
maybe_consolidate() {
    local now=$(date +%s)
    local elapsed=$((now - LAST_CONSOLIDATE))
    if [ $elapsed -ge $CONSOLIDATE_INTERVAL ]; then
        log "=== CONSOLIDATION (${elapsed}s elapsed) ==="
        echo "observe" | nc -q 1 localhost 9999 2>/dev/null
        sleep 10
        echo "dream" | nc -q 1 localhost 9999 2>/dev/null
        sleep 10
        echo "status" | nc -q 1 localhost 9997 2>/dev/null
        timeout 2 nc -w 2 localhost 9996 2>/dev/null || true
        LAST_CONSOLIDATE=$(date +%s)
        log "=== CONSOLIDATION DONE ==="
    fi
}

# Feed a file in chunks, capture run log, feed it back
feed_file() {
    local file="$1"
    local total_lines=$(wc -l < "$file" 2>/dev/null || echo 0)
    local chunks=$(( (total_lines + CHUNK_LINES - 1) / CHUNK_LINES ))

    log "Feeding: $file ($total_lines lines, $chunks chunks)"

    # Clear run log for this file
    > "$RUN_LOG"

    local offset=1
    local chunk_num=0

    while [ $offset -le $total_lines ]; do
        chunk_num=$((chunk_num + 1))
        local end=$((offset + CHUNK_LINES - 1))

        # Extract chunk
        sed -n "${offset},${end}p" "$file" > "$CHUNK_FILE"

        # Feed chunk and capture output to run log
        echo "eat $CHUNK_FILE" | nc -q 1 localhost 9999 2>/dev/null >> "$RUN_LOG"
        sleep $PAUSE_BETWEEN_CHUNKS

        # Progress every 10 chunks
        if [ $((chunk_num % 10)) -eq 0 ]; then
            log "  Chunk $chunk_num/$chunks"
        fi

        offset=$((offset + CHUNK_LINES))

        # Check consolidation
        maybe_consolidate
    done

    log "  Done feeding $file"

    # Now feed the run log back (self-learning)
    local run_log_lines=$(wc -l < "$RUN_LOG" 2>/dev/null || echo 0)
    if [ "$run_log_lines" -gt 5 ]; then
        log "  Feeding run log back ($run_log_lines lines)"

        # Chunk the run log too
        local rl_offset=1
        while [ $rl_offset -le $run_log_lines ]; do
            sed -n "${rl_offset},$((rl_offset + CHUNK_LINES - 1))p" "$RUN_LOG" > "$CHUNK_FILE"
            echo "eat $CHUNK_FILE" | nc -q 1 localhost 9999 2>/dev/null
            sleep 1
            rl_offset=$((rl_offset + CHUNK_LINES))
        done
        log "  Run log fed back"
    fi
}

# Main loop
main() {
    local corpus_dir="${1:-corpus}"

    log "=== UHMA Training ==="
    log "Corpus: $corpus_dir"
    log "Chunk size: $CHUNK_LINES lines"
    log "Consolidate every: ${CONSOLIDATE_INTERVAL}s"

    local cycle=0
    while true; do
        cycle=$((cycle + 1))
        log "=== CYCLE $cycle ==="

        # Get all corpus files
        local files=$(find "$corpus_dir" -type f -name "*.txt" | sort)
        local file_count=$(echo "$files" | wc -l)
        local file_num=0

        for file in $files; do
            file_num=$((file_num + 1))
            log "--- File $file_num/$file_count ---"
            feed_file "$file"
        done

        # End of cycle consolidation
        log "=== CYCLE $cycle COMPLETE - CONSOLIDATING ==="
        echo "observe" | nc -q 1 localhost 9999 2>/dev/null
        sleep 10
        echo "dream" | nc -q 1 localhost 9999 2>/dev/null
        sleep 10
        echo "save cycle$cycle" | nc -q 1 localhost 9999 2>/dev/null
        sleep 5
        LAST_CONSOLIDATE=$(date +%s)

        # Keep only last 3 cycle saves
        ls -t cycle[0-9]* 2>/dev/null | tail -n +4 | xargs -r rm -f

        log "=== Starting next cycle ==="
    done
}

# Handle interrupt
trap 'log "Interrupted"; echo "save interrupted" | nc -q 1 localhost 9999 2>/dev/null; exit 0' INT

main "$@"
