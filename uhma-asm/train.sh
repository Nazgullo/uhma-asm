#!/bin/bash
# train.sh — UHMA training with proper consolidation
#
# UHMA needs observe+dream cycles for proper function:
#   - observe: builds self-model, marks regions for repair (4.5s)
#   - dream: schema extraction, pattern consolidation (2min)
#
# Schedule: consolidate every CONSOLIDATE_EVERY loops
# Timeout must accommodate consolidation (~150s)

cd /home/peter/Desktop/STARWARS/uhma-asm
CORPUS="${1:-corpus_tiny}"
LOOPS="${2:-999999}"
CONSOLIDATE_EVERY="${3:-10}"    # Consolidate every N loops (not 99999!)
SAVE_EVERY=10                   # Checkpoint every N loops
TIMEOUT_NORMAL=60               # Timeout for learning loops
TIMEOUT_CONSOLIDATE=300         # Timeout for consolidation loops (observe+dream)
ALERT="ALERT_CLAUDE.txt"
LOG="training.log"

log() { echo "[$(date +%H:%M:%S)] $*" | tee -a "$LOG"; }
rm -f "$ALERT" "$LOG"

log "=== UHMA Training ==="
log "Corpus: $CORPUS"
log "Loops: $LOOPS"
log "Consolidate every: $CONSOLIDATE_EVERY loops"
log "Save every: $SAVE_EVERY loops"

for ((loop=1; loop<=LOOPS; loop++)); do
    # Determine if this is a consolidation loop
    if [ $((loop % CONSOLIDATE_EVERY)) -eq 0 ]; then
        IS_CONSOLIDATE=1
        TIMEOUT=$TIMEOUT_CONSOLIDATE
        log "=== LOOP $loop (CONSOLIDATE) ==="
    else
        IS_CONSOLIDATE=0
        TIMEOUT=$TIMEOUT_NORMAL
        log "=== LOOP $loop ==="
    fi

    # Build command file
    {
        echo "batch"
        # Recursively find all .txt files in corpus and subdirectories
        find "$CORPUS" -type f -name "*.txt" | sort | while read f; do
            echo "eat $f"
        done

        # Consolidation: observe then dream
        if [ $IS_CONSOLIDATE -eq 1 ]; then
            echo "observe"
            echo "dream"
        fi

        echo "status"

        # Save checkpoint
        if [ $((loop % SAVE_EVERY)) -eq 0 ]; then
            echo "save loop$loop"
        fi

        echo "quit"
    } > /tmp/uhma_cmds.txt

    # Run UHMA
    OUT=$(timeout $TIMEOUT ./uhma < /tmp/uhma_cmds.txt 2>&1)
    RET=$?

    # Check for errors
    if [ $RET -eq 124 ]; then
        log "ALERT: timeout after ${TIMEOUT}s"
        echo "TIMEOUT loop=$loop" >> "$ALERT"
        exit 1
    fi
    if [ $RET -ne 0 ]; then
        log "ALERT: error code $RET"
        echo "ERROR $RET loop=$loop" >> "$ALERT"
        exit 1
    fi
    if echo "$OUT" | grep -qE 'SIGSEGV|SIGFPE|SIGBUS'; then
        log "ALERT: crash detected"
        echo "CRASH loop=$loop" >> "$ALERT"
        exit 1
    fi

    # Show progress
    echo "$OUT" | grep -E 'Regions:|Accuracy:|SELF-AWARE:|DREAM.*complete' | head -4

    # Rotate checkpoints (keep only 3 latest)
    if [ $((loop % SAVE_EVERY)) -eq 0 ]; then
        ls -t loop[0-9]* 2>/dev/null | tail -n +4 | xargs -r rm -f
    fi

    log "Loop $loop done"
done

log "=== Training Complete ==="
