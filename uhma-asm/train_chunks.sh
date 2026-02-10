#!/bin/bash
# train_chunks.sh — Feed UHMA its own code in 115-line chunks
# Each chunk gets 2 dream cycles. Run log fed after each pass.
# Repeats 3 passes total.
#
# Uses single persistent feeder connection per pass (no zombie connections).
# UHMA stderr mirrors all processing output to the log file.
#
# Monitor: tail -f test_logs/train_*.log      (script progress)
#          tail -f test_logs/uhma_train_*.log  (UHMA processing)

set -uo pipefail

UHMA_DIR="/home/peter/Desktop/STARWARS/uhma-asm"
LOG_DIR="$UHMA_DIR/test_logs"
TS=$(date +%Y%m%d_%H%M%S)
MASTER="$LOG_DIR/train_${TS}.log"
UHMA_LOG="$LOG_DIR/uhma_train_${TS}.log"
FEEDER_LOG="$LOG_DIR/feeder_${TS}.log"
CHUNK_DIR="$LOG_DIR/corpus/chunks"
RUNLOG_DIR="$LOG_DIR/corpus/runlog"
PASSES=3

MAX_LOG_MB=200
DISK_MIN_GB=100

cd "$UHMA_DIR"
mkdir -p "$LOG_DIR" "$RUNLOG_DIR"

log() { echo "[$(date +%H:%M:%S)] $*" >> "$MASTER"; echo "[$(date +%H:%M:%S)] $*"; }

check_disk() {
    local free_gb=$(df -BG / | awk 'NR==2{print int($4)}')
    if [ "$free_gb" -lt "$DISK_MIN_GB" ]; then
        log "ABORT: Only ${free_gb}GB free (min=${DISK_MIN_GB}GB)"
        ./tools/feeder --shutdown 2>/dev/null || true
        sleep 3; kill $UHMA_PID 2>/dev/null || true
        exit 1
    fi
}

rotate_log() {
    local file="$1" max_mb="$2"
    [ ! -f "$file" ] && return
    local size_mb=$(du -m "$file" 2>/dev/null | cut -f1)
    if [ "${size_mb:-0}" -gt "$max_mb" ]; then
        log "Rotating $file (${size_mb}MB > ${max_mb}MB cap)"
        tail -5000 "$file" > "${file}.tmp"
        mv "${file}.tmp" "$file"
    fi
}

check_alive() {
    if ! kill -0 $UHMA_PID 2>/dev/null; then
        log "FATAL: UHMA died (PID $UHMA_PID)"
        tail -20 "$UHMA_LOG" >> "$MASTER" 2>/dev/null
        exit 1
    fi
}

# ── Cleanup old processes ──────────────────────────────────
pkill -9 -f './uhma' 2>/dev/null || true
sleep 1

cp -f uhma.surface uhma.surface.pre_train 2>/dev/null || true
log "Surface backed up. Disk: $(df -h / | awk 'NR==2{print $4}') free"

# ── Count chunks ───────────────────────────────────────────
NCHUNKS=$(ls "$CHUNK_DIR"/*.txt 2>/dev/null | wc -l)
log "Corpus: $NCHUNKS chunks of 115 lines each"

if [ "$NCHUNKS" -eq 0 ]; then
    log "FATAL: No chunks found in $CHUNK_DIR"
    exit 1
fi

# ── Start UHMA headless ───────────────────────────────────
log "Starting UHMA headless..."
./uhma < /dev/null > "$UHMA_LOG" 2>&1 &
UHMA_PID=$!
log "UHMA PID=$UHMA_PID"

for i in $(seq 1 30); do
    ss -tlnp 2>/dev/null | grep -q ':9999' && break
    sleep 1
done
if ! ss -tlnp 2>/dev/null | grep -q ':9999'; then
    log "FATAL: Gateway not ready after 30s"
    exit 1
fi
log "Gateway ready"

# ── Main training loop ────────────────────────────────────
for pass in $(seq 1 $PASSES); do
    log "══════════════════════════════════════"
    log "PASS $pass/$PASSES: $NCHUNKS chunks + 2 dreams each"
    log "══════════════════════════════════════"

    # Single feeder connection feeds ALL chunks with 2 dreams after each
    log "Feeding chunks (single connection, dream-per-file=2)..."
    ./tools/feeder \
        --corpus "$CHUNK_DIR" \
        --cycles 1 \
        --pause 1 \
        --dream-per-file 2 \
        --consolidate 999 \
        >> "$FEEDER_LOG" 2>&1

    check_alive
    rotate_log "$UHMA_LOG" "$MAX_LOG_MB"
    check_disk

    log "Pass $pass chunks complete. Feeding run log..."

    # Capture run log for feeding back
    tail -10000 "$UHMA_LOG" > "$RUNLOG_DIR/uhma_runlog.txt" 2>/dev/null
    local_lines=$(wc -l < "$RUNLOG_DIR/uhma_runlog.txt" 2>/dev/null || echo 0)
    log "Run log: $local_lines lines"

    # Feed run log with 2 dreams
    ./tools/feeder \
        --corpus "$RUNLOG_DIR" \
        --cycles 1 \
        --pause 1 \
        --dream-per-file 2 \
        --consolidate 999 \
        >> "$FEEDER_LOG" 2>&1

    check_alive

    log "Pass $pass complete"
    rotate_log "$UHMA_LOG" "$MAX_LOG_MB"
    check_disk
done

# ── Shutdown ──────────────────────────────────────────────
log "══════════════════════════════════════"
log "Graceful shutdown..."
log "══════════════════════════════════════"

./tools/feeder --shutdown >> "$MASTER" 2>&1 || true
sleep 5

if kill -0 $UHMA_PID 2>/dev/null; then
    log "UHMA still running, SIGTERM"
    kill $UHMA_PID 2>/dev/null; sleep 3
    kill -9 $UHMA_PID 2>/dev/null || true
fi

log "Final disk: $(df -h / | awk 'NR==2{print $4}') free"
log "Surface: $(du -h uhma.surface 2>/dev/null | cut -f1)"
log "══════════════════════════════════════"
log "TRAINING COMPLETE ($PASSES passes × $NCHUNKS chunks)"
log "  Master: $MASTER"
log "  UHMA:   $UHMA_LOG"
log "  Feeder: $FEEDER_LOG"
log "══════════════════════════════════════"
