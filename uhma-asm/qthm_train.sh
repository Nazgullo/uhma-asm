#!/bin/bash
# qthm_train.sh — Per-chunk QTHM training with log feedback
#
# Per chunk:  feed → 2 dreams → capture UHMA output → feed it back → 3 dreams
# Per pass:   all chunks done → 10 minute dream session
# Total:      3 passes
#
# Logs on 10MB ring buffer (tail last 10MB, overwrite).
# Per-chunk capture files deleted after feeding back.
#
# Monitor:
#   tail -f test_logs/qthm_train/logs/master.log     (progress + stats)
#   tail -f test_logs/qthm_train/logs/uhma_live.log   (everything UHMA does)

set -uo pipefail

DIR="/home/peter/Desktop/STARWARS/uhma-asm"
CHUNK_DIR="$DIR/test_logs/qthm_train/chunks"
LOG_DIR="$DIR/test_logs/qthm_train/logs"
UHMA_LOG="$LOG_DIR/uhma_live.log"
FEEDER_LOG="$LOG_DIR/feeder.log"
MASTER="$LOG_DIR/master.log"
FEED_TMP="$LOG_DIR/feed_tmp"
PASSES=3

cd "$DIR"
mkdir -p "$LOG_DIR" "$FEED_TMP"
: > "$UHMA_LOG"
: > "$FEEDER_LOG"
: > "$MASTER"

log() { printf '[%s] %s\n' "$(date +%H:%M:%S)" "$*" | tee -a "$MASTER"; }

# 10MB ring buffer — truncate logs that exceed limit
LOG_MAX=$((10 * 1024 * 1024))  # 10MB in bytes
ring_trim() {
    local f="$1"
    local sz=$(stat -c%s "$f" 2>/dev/null || echo 0)
    if [ "$sz" -gt "$LOG_MAX" ]; then
        tail -c "$LOG_MAX" "$f" > "$f.tmp" && mv "$f.tmp" "$f"
    fi
}

# Feed single file with N dream-per-file via feeder
feed_one() {
    local file="$1" dreams="$2"
    local tmp=$(mktemp -d "$FEED_TMP/f.XXXXXX")
    cp "$file" "$tmp/input.txt"
    ./tools/feeder --corpus "$tmp" --cycles 1 --pause 0 \
        --dream-per-file "$dreams" --consolidate 999 >> "$FEEDER_LOG" 2>&1
    rm -rf "$tmp"
}

# Dream N cycles via feeding tiny dummy
dream_only() {
    local n="$1"
    local tmp=$(mktemp -d "$FEED_TMP/d.XXXXXX")
    printf '; dream\n' > "$tmp/pad.txt"
    ./tools/feeder --corpus "$tmp" --cycles 1 --pause 0 \
        --dream-per-file "$n" --consolidate 999 >> "$FEEDER_LOG" 2>&1
    rm -rf "$tmp"
}

# Stats from UHMA log — counts are cumulative within current ring window
stats() {
    local h m t pct d e
    h=$(grep -c '\[HIT\]' "$UHMA_LOG" 2>/dev/null || echo 0)
    m=$(grep -c '\[MISS\]' "$UHMA_LOG" 2>/dev/null || echo 0)
    t=$((h + m))
    if [ "$t" -gt 0 ]; then
        pct=$(echo "scale=1; $h * 100 / $t" | bc)
    else
        pct="0.0"
    fi
    d=$(grep -c 'DREAM.*Cycle complete' "$UHMA_LOG" 2>/dev/null || echo 0)
    e=$(grep -c '\[EMIT\]' "$UHMA_LOG" 2>/dev/null || echo 0)
    echo "${h}H/${m}M ${pct}% | ${d} dreams ${e} emits"
}

# ── Kill stale ──────────────────────────────────────────
pkill -9 -f './uhma' 2>/dev/null || true
pkill -9 -f 'feeder' 2>/dev/null || true
sleep 1

# ── Start UHMA headless ────────────────────────────────
log "Starting UHMA..."
stdbuf -oL -eL ./uhma < /dev/null > "$UHMA_LOG" 2>&1 &
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

# ── Chunks ──────────────────────────────────────────────
CHUNKS=($(ls "$CHUNK_DIR"/*.txt | sort))
N=${#CHUNKS[@]}
log "Corpus: $N chunks x 115 lines | $PASSES passes | 10MB log ring"

# ── Training ────────────────────────────────────────────
for pass in $(seq 1 $PASSES); do
    log "═══ PASS $pass/$PASSES — $N chunks ═══"

    for ((c=0; c<N; c++)); do
        chunk="${CHUNKS[$c]}"
        cname=$(basename "$chunk")

        if ! kill -0 $UHMA_PID 2>/dev/null; then
            log "FATAL: UHMA died at pass $pass chunk $((c+1))"
            exit 1
        fi

        # Record UHMA log byte position before feeding
        uhma_before=$(stat -c%s "$UHMA_LOG" 2>/dev/null || echo 0)

        # 1. Feed chunk (no dreams yet — capture processing output first)
        log "[$pass] $((c+1))/$N $cname"
        feed_one "$chunk" 0

        # 2. Capture UHMA's output for JUST the chunk processing (byte-offset safe across ring trims)
        uhma_after=$(stat -c%s "$UHMA_LOG" 2>/dev/null || echo 0)
        new_bytes=$((uhma_after - uhma_before))
        chunk_log="$LOG_DIR/clog_tmp.txt"
        if [ "$new_bytes" -gt 0 ]; then
            tail -c "$new_bytes" "$UHMA_LOG" > "$chunk_log" 2>/dev/null
            new_lines=$(wc -l < "$chunk_log")
        else
            : > "$chunk_log"
            new_lines=0
        fi

        # 3. Dream 2x on the chunk
        dream_only 2

        # 4. Feed processing log back (no dreams yet), then dream 3x
        if [ "$new_lines" -gt 10 ]; then
            log "  ${new_lines}L → feeding back + 3 dreams"
            feed_one "$chunk_log" 0
            dream_only 3
        else
            log "  ${new_lines}L → 3 dreams"
            dream_only 3
        fi
        rm -f "$chunk_log"

        # Ring buffer trim
        ring_trim "$UHMA_LOG"
        ring_trim "$FEEDER_LOG"

        # Stats every 10 chunks
        if (( (c+1) % 10 == 0 )); then
            ring_trim "$MASTER"
            log "  ── $(stats) ──"
        fi
    done

    log "Pass $pass chunks done: $(stats)"

    # 10-minute extended dream session
    log "10-minute dream session..."
    t_end=$((SECONDS + 600))
    dcnt=0
    while [ $SECONDS -lt $t_end ]; do
        if ! kill -0 $UHMA_PID 2>/dev/null; then
            log "FATAL: UHMA died during extended dream"
            exit 1
        fi
        dream_only 1
        ((dcnt++))
        (( dcnt % 20 == 0 )) && log "  dream #$dcnt..."
    done
    log "Extended dream: $dcnt cycles"
    log "═══ PASS $pass COMPLETE: $(stats) ═══"
done

# ── Shutdown ────────────────────────────────────────────
log "Saving and shutting down..."
./tools/feeder --shutdown >> "$MASTER" 2>&1 || true
sleep 5
kill -0 $UHMA_PID 2>/dev/null && kill $UHMA_PID 2>/dev/null
sleep 3
kill -9 $UHMA_PID 2>/dev/null || true
log "═══ TRAINING COMPLETE: $(stats) ═══"
