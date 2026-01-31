#!/bin/bash
cd /home/peter/Desktop/STARWARS/uhma-asm
CORPUS="${1:-corpus_tiny}"
LOOPS="${2:-999999}"
CONSOLIDATE_EVERY=99999
ALERT="ALERT_CLAUDE.txt"
LOG="training.log"

log() { echo "[$(date +%H:%M:%S)] $*" | tee -a "$LOG"; }
rm -f "$ALERT" "$LOG"
log "Training: corpus=$CORPUS loops=$LOOPS consolidate=$CONSOLIDATE_EVERY"

for ((loop=1; loop<=LOOPS; loop++)); do
    log "=== LOOP $loop ==="
    {
        echo "batch"
        for f in "$CORPUS"/*.txt; do [ -f "$f" ] && echo "eat $f"; done
        [ $((loop % CONSOLIDATE_EVERY)) -eq 0 ] && echo "observe" && echo "dream"
        echo "status"
        if [ $((loop % 10)) -eq 0 ]; then
            echo "save loop$loop"
            # Keep only 3 latest checkpoints
            ls -t loop[0-9]* 2>/dev/null | tail -n +4 | xargs -r rm -f
        fi
        echo "quit"
    } > /tmp/uhma_cmds.txt
    
    OUT=$(timeout 300 ./uhma < /tmp/uhma_cmds.txt 2>&1)
    RET=$?
    [ $RET -eq 124 ] && { log "ALERT: timeout"; echo "TIMEOUT" >> "$ALERT"; exit 1; }
    [ $RET -ne 0 ] && { log "ALERT: error $RET"; exit 1; }
    echo "$OUT" | grep -qE 'SIGSEGV|SIGFPE' && { log "ALERT: crash"; exit 1; }
    echo "$OUT" | grep -E 'Regions:|Accuracy:' | head -2
    log "Loop $loop done"
done
log "Done"
