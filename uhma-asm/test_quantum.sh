#!/bin/bash
# test_quantum.sh — Autonomous UHMA quantum circuits test suite
# Feeds own code (asm + lisp), run logs, full corpus with dream cycles
# Log-safe: rotates phase logs, caps UHMA output, monitors disk
#
# Monitor: tail -f test_logs/master_*.log
# Details: tail -f test_logs/uhma_output_*.log (capped at 50MB)

set -uo pipefail

UHMA_DIR="/home/peter/Desktop/STARWARS/uhma-asm"
STARWARS="/home/peter/Desktop/STARWARS"
LOG_DIR="$UHMA_DIR/test_logs"
TS=$(date +%Y%m%d_%H%M%S)
MASTER="$LOG_DIR/master_${TS}.log"
UHMA_LOG="$LOG_DIR/uhma_output_${TS}.log"
CORPUS="$LOG_DIR/corpus"

# Safety limits
MAX_LOG_MB=50         # Truncate UHMA output log above this
MAX_PHASE_MB=20       # Truncate individual phase logs above this
DISK_MIN_GB=100       # Abort if free disk drops below this

cd "$UHMA_DIR"
mkdir -p "$LOG_DIR"

log() { echo "[$(date +%H:%M:%S)] $*" >> "$MASTER"; echo "[$(date +%H:%M:%S)] $*"; }

# ── Disk & log safety ────────────────────────────────────────
check_disk() {
    local free_gb=$(df -BG / | awk 'NR==2{print int($4)}')
    if [ "$free_gb" -lt "$DISK_MIN_GB" ]; then
        log "ABORT: Only ${free_gb}GB free (min=${DISK_MIN_GB}GB)"
        ./tools/feeder --shutdown 2>/dev/null || true
        sleep 3
        kill $UHMA_PID 2>/dev/null || true
        exit 1
    fi
}

rotate_log() {
    # Truncate a log file if it exceeds limit, keeping last 1000 lines
    local file="$1" max_mb="$2"
    [ ! -f "$file" ] && return
    local size_mb=$(du -m "$file" 2>/dev/null | cut -f1)
    if [ "${size_mb:-0}" -gt "$max_mb" ]; then
        log "Rotating $file (${size_mb}MB > ${max_mb}MB cap)"
        tail -1000 "$file" > "${file}.tmp"
        mv "${file}.tmp" "$file"
    fi
}

# ── Cleanup ──────────────────────────────────────────────────
pkill -9 -f './uhma' 2>/dev/null || true
sleep 1
# Clean old test logs (keep only last run's backup)
find "$LOG_DIR" -name "phase_*.log" -mmin +60 -delete 2>/dev/null || true
find "$LOG_DIR" -name "uhma_output_*.log" -mmin +60 -delete 2>/dev/null || true

cp -f uhma.surface uhma.surface.pre_quantum_test 2>/dev/null || true
log "Surface backed up. Disk: $(df -h / | awk 'NR==2{print $4}') free"

# ── Build corpus dirs with .txt symlinks ─────────────────────
rm -rf "$CORPUS"
mkdir -p "$CORPUS"/{chunk1,chunk2,chunk3,chunk4,chunk5,full_asm,lisp,full_all,runlog,dream_trigger}

# ASM Chunk 1: Core + quantum (~900 lines)
for f in boot quantum hooks gate trace test_trace; do
    [ -f "$f.asm" ] && ln -sf "$UHMA_DIR/$f.asm" "$CORPUS/chunk1/$f.txt"
done
# ASM Chunk 2: Learning & memory (~2700 lines)
for f in emit learn drives dreams evolve genes; do
    [ -f "$f.asm" ] && ln -sf "$UHMA_DIR/$f.asm" "$CORPUS/chunk2/$f.txt"
done
# ASM Chunk 3: Self-awareness (~8500 lines)
for f in presence introspect observe receipt vsa_ops; do
    [ -f "$f.asm" ] && ln -sf "$UHMA_DIR/$f.asm" "$CORPUS/chunk3/$f.txt"
done
# ASM Chunk 4: Infrastructure (~9000 lines)
for f in surface dispatch io format repl gateway; do
    [ -f "$f.asm" ] && ln -sf "$UHMA_DIR/$f.asm" "$CORPUS/chunk4/$f.txt"
done
# ASM Chunk 5: Support (~8000 lines)
for f in vsa verify decode signal persist modify factor symbolic maturity narrate hub_client; do
    [ -f "$f.asm" ] && ln -sf "$UHMA_DIR/$f.asm" "$CORPUS/chunk5/$f.txt"
done
# Full ASM
for f in *.asm; do
    [ -f "$f" ] && ln -sf "$UHMA_DIR/$f" "$CORPUS/full_asm/${f%.asm}.txt"
done
# Lisp corpus (the whole UHMA lineage)
for f in "$STARWARS"/*.lisp; do
    [ -f "$f" ] && ln -sf "$f" "$CORPUS/lisp/$(basename "${f%.lisp}").txt"
done
# Full all: asm + lisp combined
for f in "$CORPUS"/full_asm/*.txt "$CORPUS"/lisp/*.txt; do
    [ -f "$f" ] && ln -sf "$(readlink -f "$f")" "$CORPUS/full_all/$(basename "$f")"
done
# Dream trigger
echo "dream" > "$CORPUS/dream_trigger/ping.txt"

log "Corpus: asm=$(ls "$CORPUS"/full_asm/*.txt 2>/dev/null | wc -l) lisp=$(ls "$CORPUS"/lisp/*.txt 2>/dev/null | wc -l) total=$(ls "$CORPUS"/full_all/*.txt 2>/dev/null | wc -l)"

# ── Start UHMA headless ──────────────────────────────────────
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

# ── Helpers ──────────────────────────────────────────────────
feed() {
    local label="$1" dir="$2" cyc="${3:-1}" pau="${4:-3}"
    local plog="$LOG_DIR/phase_${label}_${TS}.log"
    local fcount=$(ls "$dir"/*.txt 2>/dev/null | wc -l)
    log "START $label: $fcount files, ${cyc}x, pause=${pau}s"

    if ! ./tools/feeder --corpus "$dir" --cycles "$cyc" --pause "$pau" \
            --consolidate 999 >> "$plog" 2>&1; then
        log "WARN: feeder non-zero in $label"
    fi

    # Extract stats, then truncate phase log to save disk
    local lines=$(wc -l < "$plog" 2>/dev/null || echo 0)
    log "DONE $label: ${lines} output lines"
    rotate_log "$plog" "$MAX_PHASE_MB"
    rotate_log "$UHMA_LOG" "$MAX_LOG_MB"
    check_disk
}

dream2() {
    feed "dream_${1}" "$CORPUS/dream_trigger" 1 1
    feed "dream_${1}b" "$CORPUS/dream_trigger" 1 1
}

check_alive() {
    if ! kill -0 $UHMA_PID 2>/dev/null; then
        log "FATAL: UHMA died (PID $UHMA_PID)"
        tail -20 "$UHMA_LOG" >> "$MASTER" 2>/dev/null
        exit 1
    fi
}

# ── PHASE 1: Chunked ASM self-code + dream cycles ───────────
log "══════════════════════════════════════"
log "PHASE 1: ASM self-code (chunked + dreams)"
log "══════════════════════════════════════"

feed "1a_core" "$CORPUS/chunk1" 1 2
check_alive; dream2 "1a"; check_alive

feed "1b_learn" "$CORPUS/chunk2" 1 3
check_alive; dream2 "1b"; check_alive

feed "1c_self" "$CORPUS/chunk3" 1 5
check_alive; dream2 "1c"; check_alive

feed "1d_infra" "$CORPUS/chunk4" 1 5
check_alive; dream2 "1d"; check_alive

feed "1e_support" "$CORPUS/chunk5" 1 5
check_alive; dream2 "1e"; check_alive

log "Phase 1 complete"

# ── PHASE 2: Lisp heritage (chunked by generation) ──────────
log "══════════════════════════════════════"
log "PHASE 2: Lisp UHMA lineage"
log "══════════════════════════════════════"

feed "2_lisp" "$CORPUS/lisp" 1 3
check_alive; dream2 "2"; check_alive

log "Phase 2 complete"

# ── PHASE 3: Feed run log back ───────────────────────────────
log "══════════════════════════════════════"
log "PHASE 3: Run log self-digestion"
log "══════════════════════════════════════"

# Capture current output (truncated to last 10K lines to avoid feeding 50MB)
tail -10000 "$UHMA_LOG" > "$CORPUS/runlog/uhma_runlog.txt"
log "Run log: $(wc -l < "$CORPUS/runlog/uhma_runlog.txt") lines"

feed "3_runlog" "$CORPUS/runlog" 1 3
check_alive; dream2 "3"; check_alive

log "Phase 3 complete"

# ── PHASE 4: Full pass (asm + lisp) + run log ───────────────
log "══════════════════════════════════════"
log "PHASE 4: Full pass (all code + run log)"
log "══════════════════════════════════════"

feed "4a_full" "$CORPUS/full_all" 1 3
check_alive; dream2 "4a"; check_alive

tail -10000 "$UHMA_LOG" > "$CORPUS/runlog/uhma_runlog.txt"
feed "4b_runlog2" "$CORPUS/runlog" 1 3
check_alive; dream2 "4b"; check_alive

log "Phase 4 complete"

# ── PHASE 5: Full corpus ×3 (bulk training) ──────────────────
log "══════════════════════════════════════"
log "PHASE 5: Full corpus x3 (bulk)"
log "══════════════════════════════════════"

feed "5_bulk" "$CORPUS/full_all" 3 2
check_alive; dream2 "5"; check_alive

log "Phase 5 complete"

# ── Shutdown ─────────────────────────────────────────────────
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

# Final disk report
log "Final disk: $(df -h / | awk 'NR==2{print $4}') free"
log "Log sizes: master=$(du -h "$MASTER" | cut -f1) uhma=$(du -h "$UHMA_LOG" 2>/dev/null | cut -f1)"
log "Surface: $(du -h uhma.surface 2>/dev/null | cut -f1)"

log "══════════════════════════════════════"
log "ALL TESTS COMPLETE"
log "  Master: $MASTER"
log "  UHMA output: $UHMA_LOG"
log "══════════════════════════════════════"
