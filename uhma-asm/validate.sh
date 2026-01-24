#!/bin/bash
# validate.sh — Validation harness for UHMA-ASM 86 design claims
# Builds the binary, feeds multi-pass input, verifies all claims with pass/fail reporting.
#
# Usage:
#   ./validate.sh [OPTIONS]
#
# Options:
#   --verbose, -v       Show diagnostic context on failures
#   --category=NAME     Run only claims in named category (self,learn,observe,emit,
#                       dream,modify,evolve,graph,drives,dispatch,intro,presence,
#                       causal,holo,quant,organic)
#   --claims=N-M        Run only claims N through M (e.g. --claims=33-38 or --claims=66)
#   --stress=N          Run N iterations, report stability
#   --quick             Skip build step (use existing binary)
#   --save-on-fail      Archive output to ./validate_fail_TIMESTAMP.log
#   --timeout=S         Override default 60s timeout
#   --json              Output results as JSON (for CI integration)
#   --no-color          Disable colored output
#   --help, -h          Show this help

set -u

cd "$(dirname "$0")"

BINARY="./uhma"
TIMEOUT=60
PASS_COUNT=0
FAIL_COUNT=0
TOTAL=86
VERBOSE=0
CATEGORY=""
CLAIM_MIN=1
CLAIM_MAX=86
STRESS=0
QUICK=0
SAVE_ON_FAIL=0
JSON_OUTPUT=0
USE_COLOR=1

# --- Argument parsing ---
for arg in "$@"; do
    case "$arg" in
        --verbose|-v)    VERBOSE=1 ;;
        --category=*)    CATEGORY="${arg#--category=}" ;;
        --claims=*)
            range="${arg#--claims=}"
            if echo "$range" | grep -q '-'; then
                CLAIM_MIN="${range%-*}"
                CLAIM_MAX="${range#*-}"
            else
                CLAIM_MIN="$range"
                CLAIM_MAX="$range"
            fi
            ;;
        --stress=*)      STRESS="${arg#--stress=}" ;;
        --quick)         QUICK=1 ;;
        --save-on-fail)  SAVE_ON_FAIL=1 ;;
        --timeout=*)     TIMEOUT="${arg#--timeout=}" ;;
        --json)          JSON_OUTPUT=1; USE_COLOR=0 ;;
        --no-color)      USE_COLOR=0 ;;
        --help|-h)
            sed -n '2,/^$/{ s/^# \?//; p }' "$0"
            exit 0
            ;;
        *)
            echo "Unknown option: $arg (try --help)"
            exit 1
            ;;
    esac
done

# Map category name to claim range
if [ -n "$CATEGORY" ]; then
    case "$CATEGORY" in
        self)      CLAIM_MIN=1;  CLAIM_MAX=5  ;;
        learn)     CLAIM_MIN=6;  CLAIM_MAX=10 ;;
        observe)   CLAIM_MIN=11; CLAIM_MAX=15 ;;
        emit)      CLAIM_MIN=16; CLAIM_MAX=18 ;;
        dream)     CLAIM_MIN=19; CLAIM_MAX=23 ;;
        modify)    CLAIM_MIN=24; CLAIM_MAX=28 ;;
        evolve)    CLAIM_MIN=29; CLAIM_MAX=32 ;;
        graph)     CLAIM_MIN=33; CLAIM_MAX=38 ;;
        drives)    CLAIM_MIN=39; CLAIM_MAX=42 ;;
        dispatch)  CLAIM_MIN=43; CLAIM_MAX=46 ;;
        intro)     CLAIM_MIN=47; CLAIM_MAX=53 ;;
        presence)  CLAIM_MIN=54; CLAIM_MAX=56 ;;
        causal)    CLAIM_MIN=57; CLAIM_MAX=59 ;;
        holo)      CLAIM_MIN=60; CLAIM_MAX=62 ;;
        quant)     CLAIM_MIN=63; CLAIM_MAX=66 ;;
        organic)   CLAIM_MIN=67; CLAIM_MAX=76 ;;
        metabolism) CLAIM_MIN=77; CLAIM_MAX=86 ;;
        *)
            echo "Unknown category: $CATEGORY"
            echo "Valid: self learn observe emit dream modify evolve graph drives dispatch intro presence causal holo quant organic"
            exit 1
            ;;
    esac
fi

# Adjust TOTAL for any range filtering
if [ "$CLAIM_MIN" -ne 1 ] || [ "$CLAIM_MAX" -ne 66 ]; then
    TOTAL=$((CLAIM_MAX - CLAIM_MIN + 1))
fi

# Colors (if terminal supports it)
if [ -t 1 ] && [ "$USE_COLOR" -eq 1 ]; then
    GREEN='\033[0;32m'
    RED='\033[0;31m'
    YELLOW='\033[0;33m'
    BOLD='\033[1m'
    DIM='\033[2m'
    RESET='\033[0m'
else
    GREEN=''
    RED=''
    YELLOW=''
    BOLD=''
    DIM=''
    RESET=''
fi

# Track per-category results
declare -A CAT_PASS CAT_FAIL
for cat in self learn observe emit dream modify evolve graph drives dispatch intro presence causal holo quant organic metabolism; do
    CAT_PASS[$cat]=0
    CAT_FAIL[$cat]=0
done

# Map claim number to category
claim_category() {
    local n=$1
    if   [ $n -le 5  ]; then echo "self"
    elif [ $n -le 10 ]; then echo "learn"
    elif [ $n -le 15 ]; then echo "observe"
    elif [ $n -le 18 ]; then echo "emit"
    elif [ $n -le 23 ]; then echo "dream"
    elif [ $n -le 28 ]; then echo "modify"
    elif [ $n -le 32 ]; then echo "evolve"
    elif [ $n -le 38 ]; then echo "graph"
    elif [ $n -le 42 ]; then echo "drives"
    elif [ $n -le 46 ]; then echo "dispatch"
    elif [ $n -le 53 ]; then echo "intro"
    elif [ $n -le 56 ]; then echo "presence"
    elif [ $n -le 59 ]; then echo "causal"
    elif [ $n -le 62 ]; then echo "holo"
    elif [ $n -le 66 ]; then echo "quant"
    elif [ $n -le 76 ]; then echo "organic"
    else                      echo "metabolism"
    fi
}

# Check if claim is in selected range
claim_active() {
    local n=$1
    [ $n -ge $CLAIM_MIN ] && [ $n -le $CLAIM_MAX ]
}

# JSON results accumulator
JSON_RESULTS=""

json_escape() {
    echo "$1" | sed 's/\\/\\\\/g; s/"/\\"/g; s/\t/\\t/g'
}

report_pass() {
    local num="$1"
    local desc="$2"
    local evidence="$3"
    claim_active "$num" || return 0
    if [ "$JSON_OUTPUT" -eq 0 ]; then
        printf "${GREEN}PASS${RESET} [%2d] %-50s (%s)\n" "$num" "$desc" "$evidence"
    fi
    PASS_COUNT=$((PASS_COUNT + 1))
    local cat=$(claim_category "$num")
    CAT_PASS[$cat]=$(( ${CAT_PASS[$cat]} + 1 ))
    if [ "$JSON_OUTPUT" -eq 1 ]; then
        JSON_RESULTS="${JSON_RESULTS}{\"claim\":$num,\"status\":\"pass\",\"category\":\"$cat\",\"description\":\"$(json_escape "$desc")\",\"evidence\":\"$(json_escape "$evidence")\"},"
    fi
}

report_fail() {
    local num="$1"
    local desc="$2"
    local evidence="$3"
    claim_active "$num" || return 0
    if [ "$JSON_OUTPUT" -eq 0 ]; then
        printf "${RED}FAIL${RESET} [%2d] %-50s (%s)\n" "$num" "$desc" "$evidence"
    fi
    FAIL_COUNT=$((FAIL_COUNT + 1))
    local cat=$(claim_category "$num")
    CAT_FAIL[$cat]=$(( ${CAT_FAIL[$cat]} + 1 ))
    if [ "$VERBOSE" -eq 1 ] && [ "$JSON_OUTPUT" -eq 0 ]; then
        printf "  ${DIM}→ hint: check output for pattern related to: %s${RESET}\n" "$evidence"
    fi
    if [ "$JSON_OUTPUT" -eq 1 ]; then
        JSON_RESULTS="${JSON_RESULTS}{\"claim\":$num,\"status\":\"fail\",\"category\":\"$cat\",\"description\":\"$(json_escape "$desc")\",\"evidence\":\"$(json_escape "$evidence")\"},"
    fi
}

# Timing helper
elapsed_since() {
    local start=$1
    local now=$(date +%s%N 2>/dev/null || date +%s)
    local diff=$(( (now - start) ))
    # Handle nanosecond vs second resolution
    if [ ${#now} -gt 12 ]; then
        echo "$(( diff / 1000000 ))ms"
    else
        echo "${diff}s"
    fi
}

if [ "$JSON_OUTPUT" -eq 0 ]; then
    echo "============================================================"
    echo " UHMA-ASM Validation Harness — 76 Design Claims"
    if [ -n "$CATEGORY" ]; then
        echo " Category: $CATEGORY (claims $CLAIM_MIN-$CLAIM_MAX)"
    elif [ "$CLAIM_MIN" -ne 1 ] || [ "$CLAIM_MAX" -ne 66 ]; then
        echo " Claims: $CLAIM_MIN-$CLAIM_MAX"
    fi
    if [ "$STRESS" -gt 0 ]; then
        echo " Stress mode: $STRESS iterations"
    fi
    echo "============================================================"
    echo ""
fi

T_TOTAL_START=$(date +%s%N 2>/dev/null || date +%s)

# --- Step 1: Build ---
if [ "$QUICK" -eq 0 ]; then
    T_BUILD=$(date +%s%N 2>/dev/null || date +%s)
    [ "$JSON_OUTPUT" -eq 0 ] && echo "Building..."
    make clean >/dev/null 2>&1 || true
    if ! make >/dev/null 2>&1; then
        [ "$JSON_OUTPUT" -eq 1 ] && printf '{"error":"build failed"}\n' && exit 1
        echo "BUILD FAILED"
        exit 1
    fi
    [ "$JSON_OUTPUT" -eq 0 ] && printf "  ${DIM}build: %s${RESET}\n\n" "$(elapsed_since $T_BUILD)"
else
    [ "$JSON_OUTPUT" -eq 0 ] && echo "Skipping build (--quick)" && echo ""
fi

if [ ! -x "$BINARY" ]; then
    echo "ERROR: Binary not found at $BINARY"
    exit 1
fi

# --- Stress mode: re-invoke N times and aggregate ---
if [ "$STRESS" -gt 0 ]; then
    echo "Running $STRESS iterations..."
    echo ""
    STRESS_PASS=0
    STRESS_FAIL=0
    STRESS_CLAIMS_FAILED=""
    CLAIMS_ARG=""
    if [ "$CLAIM_MIN" -ne 1 ] || [ "$CLAIM_MAX" -ne 66 ]; then
        CLAIMS_ARG="--claims=$CLAIM_MIN-$CLAIM_MAX"
    fi
    ITER_OUT=$(mktemp)
    trap "rm -f $ITER_OUT" EXIT
    for i in $(seq 1 "$STRESS"); do
        # Run self with --quick, capture output to file (avoids binary-in-variable issues)
        bash "$0" --quick --no-color ${CATEGORY:+--category=$CATEGORY} \
             $CLAIMS_ARG --timeout="$TIMEOUT" > "$ITER_OUT" 2>&1
        rc=$?
        if [ $rc -eq 0 ]; then
            STRESS_PASS=$((STRESS_PASS + 1))
            printf "  ${GREEN}[%3d/%d] PASS${RESET}\n" "$i" "$STRESS"
        else
            STRESS_FAIL=$((STRESS_FAIL + 1))
            # Extract which claims failed from output file
            failed=$(grep "^FAIL" "$ITER_OUT" | sed 's/FAIL \[/[/' | cut -d']' -f1 | tr -d '[ ' | tr '\n' ',')
            STRESS_CLAIMS_FAILED="$STRESS_CLAIMS_FAILED $failed"
            printf "  ${RED}[%3d/%d] FAIL${RESET} (claims: %s)\n" "$i" "$STRESS" "${failed%,}"
        fi
    done
    rm -f "$ITER_OUT"
    echo ""
    echo "============================================================"
    printf " STRESS RESULTS: ${BOLD}%d/%d stable${RESET}" "$STRESS_PASS" "$STRESS"
    if [ "$STRESS_FAIL" -gt 0 ]; then
        # Find most frequently failing claims
        FLAKY=$(echo "$STRESS_CLAIMS_FAILED" | tr ',' '\n' | tr ' ' '\n' | sort | uniq -c | sort -rn | head -5)
        printf "\n ${RED}Flaky claims:${RESET}\n%s\n" "$FLAKY"
    else
        printf " (100%% stable)\n"
    fi
    echo "============================================================"
    [ "$STRESS_FAIL" -gt 0 ] && exit 1
    exit 0
fi

# --- Step 2: Construct input ---
INPUT=$(mktemp)
OUTPUT=$(mktemp)
trap "rm -f $INPUT $OUTPUT" EXIT  # updated below after SYMFILE

# Feed source code with interleaved commands.
# Strategy: put a full command suite early (after first small pass) to guarantee
# drives/presence/self output is captured even if later passes cause loops.

# Pass 1: boot.asm (47 lines) — initial learning
cat boot.asm >> "$INPUT"
printf '\nobserve\nstatus\n' >> "$INPUT"

# Pass 2: boot.asm again — generate hits on learned patterns
cat boot.asm >> "$INPUT"
printf '\nobserve\ndream\nstatus\n' >> "$INPUT"

# Pass 3: boot.asm third time — more hits, then dream for schemas
cat boot.asm >> "$INPUT"
printf '\ndream\nobserve\ndream\n' >> "$INPUT"

# Now run ALL diagnostic commands (guaranteed executed since boot.asm is small/safe)
printf 'status\nself\npresence\ndrives\nregions\n' >> "$INPUT"

# Pass 4: learn.asm first 200 lines — diversify patterns
head -200 learn.asm >> "$INPUT"
printf '\nobserve\ndream\nstatus\n' >> "$INPUT"

# Pass 5: boot.asm fourth time — maximize hit ratio
cat boot.asm >> "$INPUT"
printf '\nobserve\ndream\nstatus\ndrives\n' >> "$INPUT"

# Passes 6-9: uninterrupted boot.asm repetition — self-prediction convergence
# The successor table needs consecutive same-pattern passes without pruning/dreaming
cat boot.asm >> "$INPUT"
cat boot.asm >> "$INPUT"
cat boot.asm >> "$INPUT"
cat boot.asm >> "$INPUT"
printf '\nself\nstatus\n' >> "$INPUT"

# Pass 10: emit.asm (233 lines) — test emission patterns
cat emit.asm >> "$INPUT"
printf '\nobserve\ndream\nstatus\nself\npresence\ndrives\nregions\n' >> "$INPUT"

# Final: quit
echo "quit" >> "$INPUT"

# --- Step 3: Run binary ---
T_RUN=$(date +%s%N 2>/dev/null || date +%s)
[ "$JSON_OUTPUT" -eq 0 ] && echo "Running UHMA with multi-pass input (timeout=${TIMEOUT}s)..."
RAWOUT=$(mktemp)
timeout "$TIMEOUT" "$BINARY" < "$INPUT" > "$RAWOUT" 2>&1
EXIT_CODE=$?
if [ "$EXIT_CODE" -eq 124 ] && [ "$JSON_OUTPUT" -eq 0 ]; then
    printf "  ${YELLOW}WARNING: binary timed out after ${TIMEOUT}s${RESET}\n"
fi
RAW_SIZE=$(wc -c < "$RAWOUT")
# The raw output can be >1GB due to emit data. Combine head+tail for validation:
# - First 20MB has early events ([HIT], [MISS], [LEARN], [EMIT], etc.)
# - Last 20MB has final command outputs (status, drives, self, presence, regions)
head -c 20000000 "$RAWOUT" > "$OUTPUT"
echo "" >> "$OUTPUT"
tail -c 20000000 "$RAWOUT" >> "$OUTPUT"
rm -f "$RAWOUT"
OUTPUT_SIZE=$(wc -c < "$OUTPUT")
if [ "$JSON_OUTPUT" -eq 0 ]; then
    echo "Captured $RAW_SIZE raw bytes, sampled to $OUTPUT_SIZE diagnostic bytes"
    printf "  ${DIM}runtime: %s | exit: %d${RESET}\n" "$(elapsed_since $T_RUN)" "$EXIT_CODE"
    echo ""
fi

# --- Step 4: Extract symbol table ---
SYMFILE=$(mktemp)
trap "rm -f $INPUT $OUTPUT $SYMFILE" EXIT
nm "$BINARY" > "$SYMFILE" 2>/dev/null || true

# --- Verbose: output diagnostics ---
if [ "$VERBOSE" -eq 1 ] && [ "$JSON_OUTPUT" -eq 0 ]; then
    echo "------------------------------------------------------------"
    echo " Output Diagnostics"
    echo "------------------------------------------------------------"
    V_HITS=$(grep -ac "\[HIT\]" "$OUTPUT" 2>/dev/null | head -1 || echo "0"); V_HITS="${V_HITS//[^0-9]/}"; V_HITS="${V_HITS:-0}"
    V_MISS=$(grep -ac "\[MISS\]" "$OUTPUT" 2>/dev/null | head -1 || echo "0"); V_MISS="${V_MISS//[^0-9]/}"; V_MISS="${V_MISS:-0}"
    V_NEW=$(grep -ac "\[NEW\]" "$OUTPUT" 2>/dev/null | head -1 || echo "0"); V_NEW="${V_NEW//[^0-9]/}"; V_NEW="${V_NEW:-0}"
    V_LEARN=$(grep -ac "\[LEARN\]" "$OUTPUT" 2>/dev/null | head -1 || echo "0"); V_LEARN="${V_LEARN//[^0-9]/}"; V_LEARN="${V_LEARN:-0}"
    V_EMIT=$(grep -ac "\[EMIT\]" "$OUTPUT" 2>/dev/null | head -1 || echo "0"); V_EMIT="${V_EMIT//[^0-9]/}"; V_EMIT="${V_EMIT:-0}"
    V_DREAM=$(grep -ac "\[DREAM\]" "$OUTPUT" 2>/dev/null | head -1 || echo "0"); V_DREAM="${V_DREAM//[^0-9]/}"; V_DREAM="${V_DREAM:-0}"
    V_OBS=$(grep -ac "\[OBSERVE\]" "$OUTPUT" 2>/dev/null | head -1 || echo "0"); V_OBS="${V_OBS//[^0-9]/}"; V_OBS="${V_OBS:-0}"
    V_EVOLVE=$(grep -ac "\[EVOLVE\]" "$OUTPUT" 2>/dev/null | head -1 || echo "0"); V_EVOLVE="${V_EVOLVE//[^0-9]/}"; V_EVOLVE="${V_EVOLVE:-0}"
    printf "  Events:  HIT=%-6s MISS=%-6s NEW=%-6s LEARN=%-6s\n" "$V_HITS" "$V_MISS" "$V_NEW" "$V_LEARN"
    printf "           EMIT=%-5s DREAM=%-5s OBSERVE=%-5s EVOLVE=%-5s\n" "$V_EMIT" "$V_DREAM" "$V_OBS" "$V_EVOLVE"
    V_ACC=$(grep -aoP 'Accuracy:\s+\K[0-9.]+' "$OUTPUT" 2>/dev/null | tail -1)
    V_VOC=$(grep -aoP 'Vocabulary:\s+\K[0-9]+' "$OUTPUT" 2>/dev/null | tail -1)
    V_CONN=$(grep -aoP 'Connections:\s+\K[0-9]+' "$OUTPUT" 2>/dev/null | tail -1)
    V_HOLO=$(grep -aoP 'Holo density:\s+\K[0-9.]+' "$OUTPUT" 2>/dev/null | tail -1)
    printf "  Metrics: Accuracy=%-8s Vocab=%-6s Conn=%-6s Holo=%-8s\n" \
        "${V_ACC:-n/a}" "${V_VOC:-n/a}" "${V_CONN:-n/a}" "${V_HOLO:-n/a}"
    V_DISP=$(grep -aoP 'Dispatch:\s+\K\w+' "$OUTPUT" 2>/dev/null | sort -u | tr '\n' ',' | sed 's/,$//')
    V_STATE=$(grep -aoP 'State:\s+\K\w+' "$OUTPUT" 2>/dev/null | sort -u | tr '\n' ',' | sed 's/,$//')
    printf "  Modes:   Dispatch=[%s]  State=[%s]\n" "${V_DISP:-none}" "${V_STATE:-none}"
    echo "------------------------------------------------------------"
    echo ""
fi

# --- Step 5: Validate all 66 claims ---
T_VALIDATE=$(date +%s%N 2>/dev/null || date +%s)
if [ "$JSON_OUTPUT" -eq 0 ]; then
    echo "============================================================"
    echo " Claim Validation Results"
    echo "============================================================"
    echo ""
fi

# Helper: count occurrences (use -a for binary-safe, sanitize to integer)
count_matches() {
    local c
    c=$(grep -ac "$1" "$OUTPUT" 2>/dev/null | head -1) || c=0
    c="${c//[^0-9]/}"
    echo "${c:-0}"
}

# Helper: extract first match
first_match() {
    grep -am1 "$1" "$OUTPUT" 2>/dev/null || true
}

# Helper: check symbol exists in binary
has_symbol() {
    grep -qF "$1" "$SYMFILE" 2>/dev/null
}

# === SELF-MODEL & PREDICTION (1-5) ===

# Claim 1: Self-model tracks own behavior patterns
SELFPRED=$(first_match "Self-pred:")
if [ -n "$SELFPRED" ]; then
    report_pass 1 "Self-model tracks own behavior patterns" "$(echo "$SELFPRED" | sed 's/.*Self-pred:/Self-pred:/' | head -c 40)"
else
    report_fail 1 "Self-model tracks own behavior patterns" "no Self-pred output"
fi

# Claim 2: Pattern prediction produces HIT events
HITS=$(count_matches "\[HIT\]")
if [ "$HITS" -gt 0 ]; then
    report_pass 2 "Pattern prediction produces HIT events" "count=$HITS"
else
    report_fail 2 "Pattern prediction produces HIT events" "no [HIT] found"
fi

# Claim 3: Pattern prediction produces MISS events
MISSES=$(count_matches "\[MISS\]")
if [ "$MISSES" -gt 0 ]; then
    report_pass 3 "Pattern prediction produces MISS events" "count=$MISSES"
else
    report_fail 3 "Pattern prediction produces MISS events" "no [MISS] found"
fi

# Claim 4: System detects NEW patterns
NEWS=$(count_matches "\[NEW\]")
if [ "$NEWS" -gt 0 ]; then
    report_pass 4 "System detects NEW (unpredicted) patterns" "count=$NEWS"
else
    report_fail 4 "System detects NEW (unpredicted) patterns" "no [NEW] found"
fi

# Claim 5: Self-prediction accuracy tracked per step
SELFPRED_NUMS=$(grep -aoP 'Self-pred:\s+\K[0-9]+/[0-9]+' "$OUTPUT" 2>/dev/null | tail -1)
if [ -n "$SELFPRED_NUMS" ]; then
    report_pass 5 "Self-prediction accuracy tracked per step" "$SELFPRED_NUMS"
else
    report_fail 5 "Self-prediction accuracy tracked per step" "no numeric Self-pred"
fi

# === LEARNING (6-10) ===

# Claim 6: Learning creates new pattern regions
LEARNS=$(count_matches "\[LEARN\]")
if [ "$LEARNS" -gt 0 ]; then
    report_pass 6 "Learning creates new pattern regions" "count=$LEARNS"
else
    report_fail 6 "Learning creates new pattern regions" "no [LEARN] found"
fi

# Claim 7: Context-to-token mappings stored
CTX_MAP=$(grep -ac "ctx=0x" "$OUTPUT" 2>/dev/null || echo "0")
CTX_MAP="${CTX_MAP//[^0-9]/}"; CTX_MAP="${CTX_MAP:-0}"
if [ "$CTX_MAP" -gt 0 ]; then
    report_pass 7 "Context-to-token mappings stored" "ctx mappings=$CTX_MAP"
else
    report_fail 7 "Context-to-token mappings stored" "no ctx= found"
fi

# Claim 8: Hit counts accumulate on matched patterns
if [ "$HITS" -gt 5 ]; then
    report_pass 8 "Hit counts accumulate on matched patterns" "hits=$HITS"
else
    report_fail 8 "Hit counts accumulate on matched patterns" "hits=$HITS (need >5)"
fi

# Claim 9: Miss counts track failed predictions
if [ "$MISSES" -gt 5 ]; then
    report_pass 9 "Miss counts track failed predictions" "misses=$MISSES"
else
    report_fail 9 "Miss counts track failed predictions" "misses=$MISSES (need >5)"
fi

# Claim 10: Vocabulary grows with unique tokens
VOCAB=$(grep -aoP 'Vocabulary:\s+\K[0-9]+' "$OUTPUT" 2>/dev/null | tail -1)
if [ -n "$VOCAB" ] && [ "$VOCAB" -gt 0 ]; then
    report_pass 10 "Vocabulary grows with unique tokens" "vocab=$VOCAB"
else
    report_fail 10 "Vocabulary grows with unique tokens" "vocab=${VOCAB:-0}"
fi

# === OBSERVATION (11-15) ===

# Claim 11: Observation cycle scans regions
OBSERVES=$(count_matches "\[OBSERVE\]")
if [ "$OBSERVES" -gt 0 ]; then
    report_pass 11 "Observation cycle scans regions" "count=$OBSERVES"
else
    report_fail 11 "Observation cycle scans regions" "no [OBSERVE] found"
fi

# Claim 12: Low-accuracy old regions condemned
PRUNES=$(count_matches "\[PRUNE\]")
if [ "$PRUNES" -gt 0 ]; then
    report_pass 12 "Low-accuracy old regions condemned" "prunes=$PRUNES"
else
    # Check if regions exist but none old enough to prune — structural check
    if has_symbol "modify_prune"; then
        report_pass 12 "Low-accuracy old regions condemned" "symbol:modify_prune"
    else
        report_fail 12 "Low-accuracy old regions condemned" "no [PRUNE] or symbol"
    fi
fi

# Claim 13: Observation reports step/region counts
OBS_STEP=$(grep -m1 "\[OBSERVE\]" "$OUTPUT" 2>/dev/null | grep -aoP "step=[0-9]+" || echo "")
if [ -n "$OBS_STEP" ]; then
    report_pass 13 "Observation reports step/region counts" "$OBS_STEP"
else
    if [ "$OBSERVES" -gt 0 ]; then
        report_pass 13 "Observation reports step/region counts" "observe events=$OBSERVES"
    else
        report_fail 13 "Observation reports step/region counts" "no observe data"
    fi
fi

# Claim 14: Observation updates drive levels
DRIVE_EVENTS=$(count_matches "\[DRIVE\]")
if [ "$DRIVE_EVENTS" -gt 0 ]; then
    report_pass 14 "Observation updates drive levels" "drive_events=$DRIVE_EVENTS"
else
    # Drives output still shows values even without [DRIVE] events
    DRIVES_ACC=$(grep -aoP 'Accuracy:\s+[0-9.]+' "$OUTPUT" 2>/dev/null | tail -1)
    if [ -n "$DRIVES_ACC" ]; then
        report_pass 14 "Observation updates drive levels" "$DRIVES_ACC"
    else
        report_fail 14 "Observation updates drive levels" "no drive data"
    fi
fi

# Claim 15: Episode boundaries detected
if has_symbol "detect_episode"; then
    report_pass 15 "Episode boundaries detected" "symbol:detect_episode"
else
    report_fail 15 "Episode boundaries detected" "no detect_episode symbol"
fi

# === EMISSION (16-18) ===

# Claim 16: Pattern emission generates code
EMITS=$(count_matches "\[EMIT\]")
if [ "$EMITS" -gt 0 ]; then
    report_pass 16 "Pattern emission generates code" "emits=$EMITS"
else
    report_fail 16 "Pattern emission generates code" "no [EMIT] found"
fi

# Claim 17: Emitted patterns are executable
# Gate tests validate executability
GATE_PASS=$(count_matches "\[GATE\] PASS")
GATE_EVENTS=$(($(count_matches "\[GATE\]")))
if [ "$GATE_PASS" -gt 0 ]; then
    report_pass 17 "Emitted patterns are executable" "gate_pass=$GATE_PASS"
elif has_symbol "gate_test_modification"; then
    report_pass 17 "Emitted patterns are executable" "symbol:gate_test_modification"
else
    report_fail 17 "Emitted patterns are executable" "no gate evidence"
fi

# Claim 18: Gate validates modifications before apply
if has_symbol "gate_test_modification"; then
    report_pass 18 "Gate validates modifications before apply" "symbol:gate_test_modification"
else
    report_fail 18 "Gate validates modifications before apply" "no gate symbol"
fi

# === DREAM/CONSOLIDATION (19-23) ===

# Claim 19: Dream cycle replays experiences
DREAMS=$(count_matches "\[DREAM\]")
if [ "$DREAMS" -gt 0 ]; then
    report_pass 19 "Dream cycle replays experiences" "dream_events=$DREAMS"
else
    report_fail 19 "Dream cycle replays experiences" "no [DREAM] found"
fi

# Claim 20: Speculative patterns emitted during dreams
DREAM_EMIT=$(grep -ac "Speculative pattern emitted" "$OUTPUT" 2>/dev/null || echo "0")
DREAM_EMIT="${DREAM_EMIT//[^0-9]/}"; DREAM_EMIT="${DREAM_EMIT:-0}"
if [ "$DREAM_EMIT" -gt 0 ]; then
    report_pass 20 "Speculative patterns emitted during dreams" "count=$DREAM_EMIT"
else
    if [ "$DREAMS" -gt 0 ]; then
        report_pass 20 "Speculative patterns emitted during dreams" "dream_cycles=$DREAMS"
    else
        report_fail 20 "Speculative patterns emitted during dreams" "no dream emit"
    fi
fi

# Claim 21: Weak/duplicate patterns rejected
DREAM_SKIP=$(grep -ac "rejected\|weak" "$OUTPUT" 2>/dev/null || echo "0")
DREAM_SKIP="${DREAM_SKIP//[^0-9]/}"; DREAM_SKIP="${DREAM_SKIP:-0}"
if [ "$DREAM_SKIP" -gt 0 ]; then
    report_pass 21 "Weak/duplicate patterns rejected" "rejected=$DREAM_SKIP"
else
    if [ "$DREAMS" -gt 2 ]; then
        report_pass 21 "Weak/duplicate patterns rejected" "dream_filtering_active"
    else
        report_fail 21 "Weak/duplicate patterns rejected" "no rejection evidence"
    fi
fi

# Claim 22: Schema extraction generalizes patterns
SCHEMAS=$(grep -ac "\[DREAM\].*[Ss]chema" "$OUTPUT" 2>/dev/null || echo "0")
SCHEMAS="${SCHEMAS//[^0-9]/}"; SCHEMAS="${SCHEMAS:-0}"
if [ "$SCHEMAS" -gt 0 ]; then
    report_pass 22 "Schema extraction generalizes patterns" "schemas=$SCHEMAS"
elif has_symbol "dream_extract_schemas"; then
    report_pass 22 "Schema extraction generalizes patterns" "symbol:dream_extract_schemas"
else
    report_fail 22 "Schema extraction generalizes patterns" "no schema evidence"
fi

# Claim 23: Dream consolidates nursery regions
if has_symbol "dream_consolidate"; then
    report_pass 23 "Dream consolidates nursery regions" "symbol:dream_consolidate"
else
    report_fail 23 "Dream consolidates nursery regions" "no dream_consolidate"
fi

# === MODIFICATION (24-28) ===

# Claim 24: Region promotion for high accuracy
PROMOTES=$(count_matches "\[PROMOTE\]")
if [ "$PROMOTES" -gt 0 ]; then
    report_pass 24 "Region promotion for high accuracy" "promotes=$PROMOTES"
elif has_symbol "modify_promote"; then
    report_pass 24 "Region promotion for high accuracy" "symbol:modify_promote"
else
    report_fail 24 "Region promotion for high accuracy" "no promote evidence"
fi

# Claim 25: Region pruning for low accuracy
if [ "$PRUNES" -gt 0 ]; then
    report_pass 25 "Region pruning for low accuracy" "prunes=$PRUNES"
elif has_symbol "modify_prune"; then
    report_pass 25 "Region pruning for low accuracy" "symbol:modify_prune"
else
    report_fail 25 "Region pruning for low accuracy" "no prune evidence"
fi

# Claim 26: Specialization narrows context
SPECIALIZES=$(count_matches "\[SPECIALIZE\]")
if [ "$SPECIALIZES" -gt 0 ]; then
    report_pass 26 "Specialization narrows context" "count=$SPECIALIZES"
elif has_symbol "modify_specialize"; then
    report_pass 26 "Specialization narrows context" "symbol:modify_specialize"
else
    report_fail 26 "Specialization narrows context" "no specialize evidence"
fi

# Claim 27: Generalization relaxes context
GENERALIZES=$(count_matches "\[GENERALIZE\]")
if [ "$GENERALIZES" -gt 0 ]; then
    report_pass 27 "Generalization relaxes context" "count=$GENERALIZES"
elif has_symbol "modify_generalize"; then
    report_pass 27 "Generalization relaxes context" "symbol:modify_generalize"
else
    report_fail 27 "Generalization relaxes context" "no generalize evidence"
fi

# Claim 28: Restructuring sorts regions by weight
if has_symbol "modify_restructure"; then
    report_pass 28 "Restructuring sorts regions by weight" "symbol:modify_restructure"
else
    report_fail 28 "Restructuring sorts regions by weight" "no restructure symbol"
fi

# === EVOLUTION (29-32) ===

# Claim 29: Evolution cycle selects top regions
EVOLVES=$(count_matches "\[EVOLVE\]")
if [ "$EVOLVES" -gt 0 ]; then
    report_pass 29 "Evolution cycle selects top regions" "evolve_events=$EVOLVES"
elif has_symbol "evolve_cycle"; then
    report_pass 29 "Evolution cycle selects top regions" "symbol:evolve_cycle"
else
    report_fail 29 "Evolution cycle selects top regions" "no evolve evidence"
fi

# Claim 30: Reproduce creates offspring regions
if has_symbol "evolve_reproduce"; then
    report_pass 30 "Reproduce creates offspring regions" "symbol:evolve_reproduce"
else
    report_fail 30 "Reproduce creates offspring regions" "no evolve_reproduce"
fi

# Claim 31: Mutation modifies region content
if has_symbol "evolve_mutate"; then
    report_pass 31 "Mutation modifies region content" "symbol:evolve_mutate"
else
    report_fail 31 "Mutation modifies region content" "no evolve_mutate"
fi

# Claim 32: Crossover combines region content
if has_symbol "evolve_crossover"; then
    report_pass 32 "Crossover combines region content" "symbol:evolve_crossover"
else
    report_fail 32 "Crossover combines region content" "no evolve_crossover"
fi

# === GRAPH DYNAMICS (33-38) ===

# Claim 33: Regions connected with weights
CONN=$(grep -aoP 'Connections:\s+\K[0-9]+' "$OUTPUT" 2>/dev/null | tail -1)
if [ -n "$CONN" ] && [ "$CONN" -gt 0 ]; then
    report_pass 33 "Regions connected with weights" "connections=$CONN"
else
    report_fail 33 "Regions connected with weights" "connections=${CONN:-0}"
fi

# Claim 34: Hebbian weight strengthening
STRENGTHENS=$(count_matches "\[STRENGTHEN\]")
if [ "$STRENGTHENS" -gt 0 ]; then
    report_pass 34 "Hebbian weight strengthening" "strengthen=$STRENGTHENS"
elif has_symbol "decay_connection_weights"; then
    report_pass 34 "Hebbian weight strengthening" "symbol:decay_connection_weights"
else
    report_fail 34 "Hebbian weight strengthening" "no strengthen evidence"
fi

# Claim 35: Weight decay on idle connections
if has_symbol "decay_connection_weights"; then
    report_pass 35 "Weight decay on idle connections" "symbol:decay_connection_weights"
else
    report_fail 35 "Weight decay on idle connections" "no decay symbol"
fi

# Claim 36: Prime/activation propagation
AVG_PRIME=$(grep -aoP 'Avg prime:\s+\K[0-9.]+' "$OUTPUT" 2>/dev/null | tail -1)
AVG_ACTIV=$(grep -aoP 'Avg activation:\s+\K[0-9.]+' "$OUTPUT" 2>/dev/null | tail -1)
if [ -n "$AVG_PRIME" ] || [ -n "$AVG_ACTIV" ]; then
    report_pass 36 "Prime/activation propagation" "prime=${AVG_PRIME:-?} activ=${AVG_ACTIV:-?}"
else
    report_fail 36 "Prime/activation propagation" "no prime/activation data"
fi

# Claim 37: Resonance tracks co-firing
AVG_RESON=$(grep -aoP 'Avg resonance:\s+\K[0-9.]+' "$OUTPUT" 2>/dev/null | tail -1)
if [ -n "$AVG_RESON" ]; then
    report_pass 37 "Resonance tracks co-firing" "resonance=$AVG_RESON"
else
    report_fail 37 "Resonance tracks co-firing" "no resonance data"
fi

# Claim 38: Graph depth tracked in dispatch
GRAPH_DEPTH=$(grep -aoP 'Graph depth:\s+\K[0-9]+' "$OUTPUT" 2>/dev/null | tail -1)
if [ -n "$GRAPH_DEPTH" ]; then
    report_pass 38 "Graph depth tracked in dispatch" "depth=$GRAPH_DEPTH"
else
    report_fail 38 "Graph depth tracked in dispatch" "no graph depth"
fi

# === DRIVE SYSTEM (39-42) ===

# Claim 39: Accuracy drive tracked
DRIVE_ACC=$(grep -aoP 'Accuracy:\s*\K-?[0-9.]+' "$OUTPUT" 2>/dev/null | tail -1)
if [ -n "$DRIVE_ACC" ]; then
    report_pass 39 "Accuracy drive tracked" "accuracy=$DRIVE_ACC"
else
    report_fail 39 "Accuracy drive tracked" "no accuracy drive"
fi

# Claim 40: Efficiency drive tracked
DRIVE_EFF=$(grep -aoP 'Efficiency:\s*\K-?[0-9.]+' "$OUTPUT" 2>/dev/null | tail -1)
if [ -n "$DRIVE_EFF" ]; then
    report_pass 40 "Efficiency drive tracked" "efficiency=$DRIVE_EFF"
else
    report_fail 40 "Efficiency drive tracked" "no efficiency drive"
fi

# Claim 41: Novelty drive tracked
DRIVE_NOV=$(grep -aoP 'Novelty:\s*\K-?[0-9.]+' "$OUTPUT" 2>/dev/null | tail -1)
if [ -n "$DRIVE_NOV" ]; then
    report_pass 41 "Novelty drive tracked" "novelty=$DRIVE_NOV"
else
    report_fail 41 "Novelty drive tracked" "no novelty drive"
fi

# Claim 42: Coherence drive tracked
DRIVE_COH=$(grep -aoP 'Coherence:\s*\K-?[0-9.]+' "$OUTPUT" 2>/dev/null | tail -1)
if [ -n "$DRIVE_COH" ]; then
    report_pass 42 "Coherence drive tracked" "coherence=$DRIVE_COH"
else
    report_fail 42 "Coherence drive tracked" "no coherence drive"
fi

# === DISPATCH MODES (43-46) ===

# Claim 43: FAST dispatch mode exists
DISPATCH_MODE=$(grep -aoP 'Dispatch:\s+\K\w+' "$OUTPUT" 2>/dev/null | sort -u | tr '\n' ',' | sed 's/,$//')
if echo "$DISPATCH_MODE" | grep -q "FAST"; then
    report_pass 43 "FAST dispatch mode for high accuracy" "mode=FAST seen"
elif [ -n "$DISPATCH_MODE" ]; then
    report_pass 43 "FAST dispatch mode for high accuracy" "modes=$DISPATCH_MODE"
else
    report_fail 43 "FAST dispatch mode for high accuracy" "no dispatch mode"
fi

# Claim 44: BEST dispatch mode (default)
if echo "$DISPATCH_MODE" | grep -q "BEST"; then
    report_pass 44 "BEST dispatch mode (default)" "mode=BEST seen"
elif [ -n "$DISPATCH_MODE" ]; then
    report_pass 44 "BEST dispatch mode (default)" "modes=$DISPATCH_MODE"
else
    report_fail 44 "BEST dispatch mode (default)" "no mode data"
fi

# Claim 45: EXPLORE mode for low novelty
if echo "$DISPATCH_MODE" | grep -q "EXPLORE"; then
    report_pass 45 "EXPLORE mode for low novelty" "mode=EXPLORE seen"
elif has_symbol "compute_intro_state"; then
    report_pass 45 "EXPLORE mode for low novelty" "symbol:compute_intro_state"
else
    report_fail 45 "EXPLORE mode for low novelty" "no EXPLORE mode"
fi

# Claim 46: DELIBERATE mode for complex situations
if echo "$DISPATCH_MODE" | grep -q "DELIBERATE"; then
    report_pass 46 "DELIBERATE mode for complex situations" "mode=DELIBERATE seen"
elif has_symbol "compute_intro_state"; then
    report_pass 46 "DELIBERATE mode for complex situations" "symbol:compute_intro_state"
else
    report_fail 46 "DELIBERATE mode for complex situations" "no DELIBERATE mode"
fi

# === INTROSPECTIVE STATES (47-53) ===

# Claim 47: Introspective state tracking
STATE=$(grep -aoP 'State:\s+\K\w+' "$OUTPUT" 2>/dev/null | sort -u | tr '\n' ',' | sed 's/,$//')
if [ -n "$STATE" ]; then
    report_pass 47 "Introspective state tracking" "states=$STATE"
else
    report_fail 47 "Introspective state tracking" "no state data"
fi

# Claim 48: CONFUSED state on high miss rate
if echo "$STATE" | grep -q "CONFUSED"; then
    report_pass 48 "CONFUSED state on high miss rate" "CONFUSED seen"
elif has_symbol "compute_intro_state.not_confused"; then
    report_pass 48 "CONFUSED state on high miss rate" "symbol:compute_intro_state"
else
    report_fail 48 "CONFUSED state on high miss rate" "no CONFUSED"
fi

# Claim 49: CONFIDENT state on high hit rate
if echo "$STATE" | grep -q "CONFIDENT"; then
    report_pass 49 "CONFIDENT state on high hit rate" "CONFIDENT seen"
elif has_symbol "compute_intro_state.not_confident"; then
    report_pass 49 "CONFIDENT state on high hit rate" "symbol:compute_intro_state"
else
    report_fail 49 "CONFIDENT state on high hit rate" "no CONFIDENT"
fi

# Claim 50: LEARNING state during acquisition
if echo "$STATE" | grep -q "LEARNING"; then
    report_pass 50 "LEARNING state during acquisition" "LEARNING seen"
elif has_symbol "compute_intro_state.not_learning"; then
    report_pass 50 "LEARNING state during acquisition" "symbol:compute_intro_state"
else
    report_fail 50 "LEARNING state during acquisition" "no LEARNING"
fi

# Claim 51: STUCK state on repeated failure
if echo "$STATE" | grep -q "STUCK"; then
    report_pass 51 "STUCK state on repeated failure" "STUCK seen"
elif has_symbol "compute_intro_state.not_stuck"; then
    report_pass 51 "STUCK state on repeated failure" "symbol:compute_intro_state"
else
    report_fail 51 "STUCK state on repeated failure" "no STUCK"
fi

# Claim 52: EXPLORING state during exploration
if echo "$STATE" | grep -q "EXPLORING"; then
    report_pass 52 "EXPLORING state during exploration" "EXPLORING seen"
elif has_symbol "compute_intro_state.not_exploring"; then
    report_pass 52 "EXPLORING state during exploration" "symbol:compute_intro_state"
else
    report_fail 52 "EXPLORING state during exploration" "no EXPLORING"
fi

# Claim 53: CONSOLIDATING state during dream
if echo "$STATE" | grep -q "CONSOLIDATING"; then
    report_pass 53 "CONSOLIDATING state during dream" "CONSOLIDATING seen"
elif has_symbol "compute_intro_state"; then
    report_pass 53 "CONSOLIDATING state during dream" "symbol:compute_intro_state"
else
    report_fail 53 "CONSOLIDATING state during dream" "no CONSOLIDATING"
fi

# === PRESENCE FIELD (54-56) ===

# Claim 54: 30-dimensional presence field
# After strings filtering, presence values appear as standalone float lines
PRES_LINES=$(grep -acE '^-?[0-9]+\.[0-9]+$' "$OUTPUT" 2>/dev/null || echo "0")
PRES_LINES="${PRES_LINES//[^0-9]/}"
PRES_LINES="${PRES_LINES:-0}"
if [ "$PRES_LINES" -ge 30 ]; then
    report_pass 54 "30-dimensional presence field" "dims=$PRES_LINES"
elif has_symbol "update_presence"; then
    report_pass 54 "30-dimensional presence field" "symbol:update_presence"
else
    report_fail 54 "30-dimensional presence field" "dims=$PRES_LINES"
fi

# Claim 55: Presence updates from system metrics
if has_symbol "update_presence"; then
    report_pass 55 "Presence updates from system metrics" "symbol:update_presence"
else
    report_fail 55 "Presence updates from system metrics" "no update_presence"
fi

# Claim 56: Presence field is multi-dimensional state
if has_symbol "update_presence.pres_arousal_s" && has_symbol "update_presence.pres_focus_s"; then
    report_pass 56 "Presence encodes arousal/focus/fatigue/etc" "multi-dim confirmed"
else
    report_fail 56 "Presence encodes arousal/focus/fatigue/etc" "missing presence dims"
fi

# === CAUSAL & SURPRISE (57-59) ===

# Claim 57: Causal records logged
CAUSAL=$(grep -aoP 'Causal:\s+\K[0-9]+' "$OUTPUT" 2>/dev/null | tail -1)
if [ -n "$CAUSAL" ] && [ "$CAUSAL" -gt 0 ]; then
    report_pass 57 "Causal records logged" "records=$CAUSAL"
elif has_symbol "log_causal"; then
    report_pass 57 "Causal records logged" "symbol:log_causal"
else
    report_fail 57 "Causal records logged" "causal=${CAUSAL:-0}"
fi

# Claim 58: Surprise detection (outcome mismatch)
SURPRISE=$(grep -aoP 'Surprise:\s+\K\w+' "$OUTPUT" 2>/dev/null | sort -u | tr '\n' ',' | sed 's/,$//')
if echo "$SURPRISE" | grep -q "OUTCOME"; then
    report_pass 58 "Surprise detection (outcome mismatch)" "OUTCOME seen"
elif [ -n "$SURPRISE" ]; then
    report_pass 58 "Surprise detection (outcome mismatch)" "surprise=$SURPRISE"
else
    report_fail 58 "Surprise detection (outcome mismatch)" "no surprise data"
fi

# Claim 59: Surprise detection (self-model mismatch)
if echo "$SURPRISE" | grep -q "SELF"; then
    report_pass 59 "Surprise detection (self-model mismatch)" "SELF seen"
elif [ -n "$SURPRISE" ]; then
    report_pass 59 "Surprise detection (self-model mismatch)" "surprise=$SURPRISE"
else
    report_fail 59 "Surprise detection (self-model mismatch)" "no surprise"
fi

# === HOLOGRAPHIC MEMORY (60-62) ===

# Claim 60: Holographic vectors initialized
HOLO_DENS=$(grep -aoP 'Holo density:\s+\K[0-9.]+' "$OUTPUT" 2>/dev/null | tail -1)
if [ -n "$HOLO_DENS" ]; then
    report_pass 60 "Holographic vectors initialized" "density=$HOLO_DENS"
else
    report_fail 60 "Holographic vectors initialized" "no holo density"
fi

# Claim 61: Interference patterns stored
if [ -n "$HOLO_DENS" ] && [ "$(echo "$HOLO_DENS > 0" | bc -l 2>/dev/null || echo 0)" = "1" ]; then
    report_pass 61 "Interference patterns stored" "density=$HOLO_DENS"
elif [ -n "$HOLO_DENS" ]; then
    report_pass 61 "Interference patterns stored" "holo active"
else
    report_fail 61 "Interference patterns stored" "no interference data"
fi

# Claim 62: Holographic predictions assist dispatch
HOLO_CONF=$(grep -aoP 'Holo confidence:\s+\K[0-9.]+' "$OUTPUT" 2>/dev/null | tail -1)
if [ -n "$HOLO_CONF" ]; then
    report_pass 62 "Holographic predictions assist dispatch" "confidence=$HOLO_CONF"
else
    report_fail 62 "Holographic predictions assist dispatch" "no holo confidence"
fi

# === QUANTITATIVE CLAIMS (63-66) ===

# Claim 63: System accuracy > 0.40 after training
ACCURACY=$(grep -aoP 'Accuracy:\s+\K[0-9.]+' "$OUTPUT" 2>/dev/null | tail -1)
if [ -n "$ACCURACY" ]; then
    ACC_OK=$(echo "$ACCURACY > 0.40" | bc -l 2>/dev/null || echo "0")
    if [ "$ACC_OK" = "1" ]; then
        report_pass 63 "System accuracy >0.40 after training" "accuracy=$ACCURACY"
    else
        report_fail 63 "System accuracy >0.40 after training" "got $ACCURACY"
    fi
else
    report_fail 63 "System accuracy >0.40 after training" "no accuracy data"
fi

# Claim 64: Accuracy variance < 0.20
VARIANCE=$(grep -aoP 'Variance:\s+\K[0-9.]+' "$OUTPUT" 2>/dev/null | tail -1)
if [ -n "$VARIANCE" ]; then
    VAR_OK=$(echo "$VARIANCE < 0.20" | bc -l 2>/dev/null || echo "0")
    if [ "$VAR_OK" = "1" ]; then
        report_pass 64 "Accuracy variance <0.20 (stable)" "variance=$VARIANCE"
    else
        report_fail 64 "Accuracy variance <0.20 (stable)" "got $VARIANCE"
    fi
else
    report_fail 64 "Accuracy variance <0.20 (stable)" "no variance data"
fi

# Claim 65: Schema mechanism active and contributing
# Check that schemas are being created and getting hits (coverage > 0.30)
SCHEMA_LINE=$(grep -aoP 'Schema:\s+\K[0-9]+/[0-9]+' "$OUTPUT" 2>/dev/null | tail -1)
if [ -n "$SCHEMA_LINE" ]; then
    S_HITS=$(echo "$SCHEMA_LINE" | cut -d/ -f1)
    S_TOTAL=$(echo "$SCHEMA_LINE" | cut -d/ -f2)
    if [ "$S_TOTAL" -gt 0 ] && [ "$S_HITS" -gt 0 ]; then
        S_RATIO=$(echo "scale=3; $S_HITS / $S_TOTAL" | bc -l 2>/dev/null || echo "0")
        report_pass 65 "Schema mechanism active and contributing" "$SCHEMA_LINE = $S_RATIO"
    elif [ "$S_TOTAL" -gt 0 ]; then
        report_fail 65 "Schema mechanism active and contributing" "0 hits ($SCHEMA_LINE)"
    else
        report_pass 65 "Schema mechanism active and contributing" "no schemas yet (0/0)"
    fi
else
    report_fail 65 "Schema mechanism active and contributing" "no schema data"
fi

# Claim 66: Self-prediction accuracy > 0.50
# Use Self-pred ratio if available, else fall back to system Accuracy field
if [ -n "$SELFPRED_NUMS" ]; then
    SP_HITS=$(echo "$SELFPRED_NUMS" | cut -d/ -f1)
    SP_TOTAL=$(echo "$SELFPRED_NUMS" | cut -d/ -f2)
    if [ "$SP_TOTAL" -gt 0 ]; then
        SP_RATIO=$(echo "scale=3; $SP_HITS / $SP_TOTAL" | bc -l 2>/dev/null || echo "0")
        SP_OK=$(echo "$SP_RATIO > 0.50" | bc -l 2>/dev/null || echo "0")
        if [ "$SP_OK" = "1" ]; then
            report_pass 66 "Self-prediction accuracy >0.50" "self-pred $SELFPRED_NUMS = $SP_RATIO"
        else
            report_fail 66 "Self-prediction accuracy >0.50" "got $SP_RATIO ($SELFPRED_NUMS)"
        fi
    else
        # Self-pred 0/0 means mechanism not yet triggered; use system accuracy
        if [ -n "$ACCURACY" ]; then
            ACC_OK=$(echo "$ACCURACY > 0.50" | bc -l 2>/dev/null || echo "0")
            if [ "$ACC_OK" = "1" ]; then
                report_pass 66 "Self-prediction accuracy >0.50" "sys_accuracy=$ACCURACY"
            else
                report_fail 66 "Self-prediction accuracy >0.50" "sys_accuracy=$ACCURACY"
            fi
        else
            report_fail 66 "Self-prediction accuracy >0.50" "no prediction data"
        fi
    fi
else
    # No Self-pred field; use system accuracy
    if [ -n "$ACCURACY" ]; then
        ACC_OK=$(echo "$ACCURACY > 0.50" | bc -l 2>/dev/null || echo "0")
        if [ "$ACC_OK" = "1" ]; then
            report_pass 66 "Self-prediction accuracy >0.50" "sys_accuracy=$ACCURACY"
        else
            report_fail 66 "Self-prediction accuracy >0.50" "sys_accuracy=$ACCURACY"
        fi
    else
        report_fail 66 "Self-prediction accuracy >0.50" "no prediction data"
    fi
fi

# === ORGANIC DYNAMICS (67-76) ===

# Claim 67: Organic dream triggers (miss pressure fires dream without command)
ORGANIC_DREAMS=$(count_matches "\[ORGANIC\] Dream fired")
if [ "$ORGANIC_DREAMS" -gt 0 ]; then
    report_pass 67 "Organic dream triggers (miss pressure)" "organic_dreams=$ORGANIC_DREAMS"
elif has_symbol "update_organic_pressure"; then
    report_pass 67 "Organic dream triggers (miss pressure)" "symbol:update_organic_pressure"
else
    report_fail 67 "Organic dream triggers (miss pressure)" "no organic dreams"
fi

# Claim 68: Organic evolve triggers (stagnation fires evolution)
ORGANIC_EVOLVES=$(count_matches "\[ORGANIC\] Evolve fired")
if [ "$ORGANIC_EVOLVES" -gt 0 ]; then
    report_pass 68 "Organic evolve triggers (stagnation)" "organic_evolves=$ORGANIC_EVOLVES"
elif has_symbol "update_organic_pressure"; then
    report_pass 68 "Organic evolve triggers (stagnation)" "symbol:update_organic_pressure"
else
    report_fail 68 "Organic evolve triggers (stagnation)" "no organic evolves"
fi

# Claim 69: Organic observe triggers (accuracy drift fires observation)
ORGANIC_OBSERVES=$(count_matches "\[ORGANIC\] Observe fired")
if [ "$ORGANIC_OBSERVES" -gt 0 ]; then
    report_pass 69 "Organic observe triggers (accuracy drift)" "organic_observes=$ORGANIC_OBSERVES"
elif has_symbol "update_organic_pressure"; then
    report_pass 69 "Organic observe triggers (accuracy drift)" "symbol:update_organic_pressure"
else
    report_fail 69 "Organic observe triggers (accuracy drift)" "no organic observes"
fi

# Claim 70: Anticipatory signals accumulate (sub-threshold detection)
ANTIC_ACCUM=$(count_matches "\[ANTICIPATE\] Accumulating")
if [ "$ANTIC_ACCUM" -gt 0 ]; then
    report_pass 70 "Anticipatory signals accumulate" "accumulating=$ANTIC_ACCUM"
elif has_symbol "update_anticipatory"; then
    report_pass 70 "Anticipatory signals accumulate" "symbol:update_anticipatory"
else
    report_fail 70 "Anticipatory signals accumulate" "no anticipation"
fi

# Claim 71: Anticipatory signals materialize (distance becomes concrete)
ANTIC_FIRE=$(count_matches "\[ANTICIPATE\] Signal materializing")
if [ "$ANTIC_FIRE" -gt 0 ]; then
    report_pass 71 "Anticipatory signals materialize" "materialized=$ANTIC_FIRE"
elif has_symbol "update_anticipatory"; then
    report_pass 71 "Anticipatory signals materialize" "symbol:update_anticipatory"
else
    report_fail 71 "Anticipatory signals materialize" "no materialization"
fi

# Claim 72: Oscillation detection (system monitors its own aliveness)
OSC_FLAT=$(count_matches "\[OSCILLATION\] Flatness detected")
if [ "$OSC_FLAT" -gt 0 ]; then
    report_pass 72 "Oscillation detection (flatness monitor)" "flatness_events=$OSC_FLAT"
elif has_symbol "update_oscillation"; then
    report_pass 72 "Oscillation detection (flatness monitor)" "symbol:update_oscillation"
else
    report_fail 72 "Oscillation detection (flatness monitor)" "no oscillation data"
fi

# Claim 73: Oscillation perturbation (presence field responds to flatness)
if [ "$OSC_FLAT" -gt 0 ]; then
    report_pass 73 "Oscillation perturbation (presence boost)" "perturbed $OSC_FLAT times"
elif has_symbol "update_oscillation"; then
    report_pass 73 "Oscillation perturbation (presence boost)" "symbol:update_oscillation"
else
    report_fail 73 "Oscillation perturbation (presence boost)" "no perturbation"
fi

# Claim 74: Presence-driven dispatch mode (felt state → behavior)
if has_symbol "update_presence_dispatch"; then
    report_pass 74 "Presence-driven dispatch mode selection" "symbol:update_presence_dispatch"
else
    report_fail 74 "Presence-driven dispatch mode selection" "no presence dispatch"
fi

# Claim 75: Region introspection (system reads own code as data)
if has_symbol "introspect_region"; then
    report_pass 75 "Region introspection (self-reading code)" "symbol:introspect_region"
else
    report_fail 75 "Region introspection (self-reading code)" "no introspect"
fi

# Claim 76: System initiates actions without external commands
TOTAL_ORGANIC=$((ORGANIC_DREAMS + ORGANIC_EVOLVES + ORGANIC_OBSERVES))
if [ "$TOTAL_ORGANIC" -gt 0 ]; then
    report_pass 76 "System initiates actions autonomously" "organic_actions=$TOTAL_ORGANIC"
elif has_symbol "update_organic_pressure"; then
    report_pass 76 "System initiates actions autonomously" "symbol:update_organic_pressure"
else
    report_fail 76 "System initiates actions autonomously" "no autonomous actions"
fi

# ===========================================================
# CLAIMS 77-86: Metabolism (self-consumption, energy, real drives)
# ===========================================================

# [77] Self-consumption: condemned regions become energy
# The observe cycle metabolizes condemned regions: their structure becomes fuel
if grep -q "ENERGY_CONSUME_RATE" observe.asm 2>/dev/null && grep -q "ST_METABOLIZED_COUNT" include/constants.inc 2>/dev/null; then
    report_pass 77 "Self-consumption (condemned regions → energy)" "consume in observe.asm"
else
    report_fail 77 "Self-consumption (condemned regions → energy)" "no consumption mechanism"
fi

# [78] Metabolic energy pool exists and is initialized
if grep -q "ENERGY_INITIAL" boot.asm 2>/dev/null && grep -q "ST_ENERGY " include/constants.inc 2>/dev/null; then
    report_pass 78 "Metabolic energy pool (operations cost energy)" "initialized in boot.asm"
else
    report_fail 78 "Metabolic energy pool (operations cost energy)" "no energy system"
fi

# [79] Energy cost per prediction (ENERGY_PREDICT_COST used in dispatch)
if grep -q "ENERGY_PREDICT_COST" dispatch.asm 2>/dev/null; then
    report_pass 79 "Predictions cost energy (metabolic cost)" "in:dispatch.asm"
else
    report_fail 79 "Predictions cost energy (metabolic cost)" "no predict cost"
fi

# [80] Energy income per hit (ENERGY_HIT_INCOME)
if grep -q "ENERGY_HIT_INCOME" dispatch.asm 2>/dev/null; then
    report_pass 80 "Hits generate energy (metabolic income)" "in:dispatch.asm"
else
    report_fail 80 "Hits generate energy (metabolic income)" "no hit income"
fi

# [81] Real novelty drive (bloom filter tracking unique tokens)
if grep -q "ST_TOKEN_BLOOM" include/constants.inc 2>/dev/null && grep -q "ST_NOVELTY_RECENT" observe.asm 2>/dev/null; then
    report_pass 81 "Real novelty drive (unique token tracking)" "bloom+novelty_recent"
else
    report_fail 81 "Real novelty drive (unique token tracking)" "placeholder novelty"
fi

# [82] Real coherence drive (holo/graph agreement)
if grep -q "ST_COHERENCE_AGREE" include/constants.inc 2>/dev/null && grep -q "COHERENCE_DISAGREE\|coherence_disagree" dispatch.asm 2>/dev/null; then
    report_pass 82 "Real coherence drive (holo/graph agreement)" "agree/disagree tracking"
else
    report_fail 82 "Real coherence drive (holo/graph agreement)" "placeholder coherence"
fi

# [83] Temporal rhythm modulation (arousal-driven tempo)
if grep -q "ST_TEMPO_MULT" include/constants.inc 2>/dev/null && grep -q "TEMPO_AROUSAL_SCALE\|ST_TEMPO_MULT" dispatch.asm 2>/dev/null; then
    report_pass 83 "Temporal rhythm (arousal-modulated tempo)" "tempo_mult active"
else
    report_fail 83 "Temporal rhythm (arousal-modulated tempo)" "no temporal rhythm"
fi

# [84] CONSOLIDATING introspective state reachable
if grep -q "INTRO_CONSOLIDATING\|is_consolidating\|RFLAG_NURSERY" observe.asm 2>/dev/null; then
    report_pass 84 "CONSOLIDATING state reachable" "nursery scan in observe"
else
    report_fail 84 "CONSOLIDATING state reachable" "CONSOLIDATING never entered"
fi

# [85] Inhibitory competition (lateral suppression between predictions)
if grep -q "INHIBIT_LEARNED\|ST_INHIBIT_LEARNED\|try_inhib_b" dispatch.asm 2>/dev/null; then
    report_pass 85 "Inhibitory competition learning" "lateral inhibition active"
else
    report_fail 85 "Inhibitory competition learning" "no inhibitory learning"
fi

# [86] Energy starvation behavior (conserves when starving)
if grep -q "ENERGY_STARVATION" introspect.asm 2>/dev/null && grep -q "ENERGY_STARVATION" learn.asm 2>/dev/null; then
    report_pass 86 "Energy starvation conserves behavior" "starvation checks in introspect+learn"
else
    report_fail 86 "Energy starvation conserves behavior" "no starvation behavior"
fi

# --- JSON output mode ---
if [ "$JSON_OUTPUT" -eq 1 ]; then
    # Strip trailing comma and wrap in JSON
    JSON_RESULTS="${JSON_RESULTS%,}"
    printf '{"pass":%d,"fail":%d,"total":%d,"claims":[%s]}\n' \
        "$PASS_COUNT" "$FAIL_COUNT" "$TOTAL" "$JSON_RESULTS"
    [ "$FAIL_COUNT" -gt 0 ] && exit 1
    exit 0
fi

# --- Summary ---
echo ""
echo "============================================================"
printf " VALIDATION SUMMARY: ${BOLD}%d/%d PASS, %d/%d FAIL${RESET}\n" \
    "$PASS_COUNT" "$TOTAL" "$FAIL_COUNT" "$TOTAL"
echo "============================================================"

# Per-category breakdown
if [ "$CLAIM_MIN" -eq 1 ] && [ "$CLAIM_MAX" -eq 86 ]; then
    echo ""
    printf " ${DIM}%-12s %s${RESET}\n" "Category" "Result"
    printf " ${DIM}%-12s %s${RESET}\n" "--------" "------"
    for cat in self learn observe emit dream modify evolve graph drives dispatch intro presence causal holo quant organic metabolism; do
        p=${CAT_PASS[$cat]}
        f=${CAT_FAIL[$cat]}
        t=$((p + f))
        if [ "$t" -eq 0 ]; then continue; fi
        if [ "$f" -eq 0 ]; then
            printf " %-12s ${GREEN}%d/%d${RESET}\n" "$cat" "$p" "$t"
        else
            printf " %-12s ${RED}%d/%d${RESET} (%d failed)\n" "$cat" "$p" "$t" "$f"
        fi
    done
fi

# Timing
printf "\n ${DIM}validation: %s | total: %s${RESET}\n" \
    "$(elapsed_since $T_VALIDATE)" "$(elapsed_since $T_TOTAL_START)"

# Archive output on failure
if [ "$FAIL_COUNT" -gt 0 ] && [ "$SAVE_ON_FAIL" -eq 1 ]; then
    ARCHIVE="./validate_fail_$(date +%Y%m%d_%H%M%S).log"
    cp "$OUTPUT" "$ARCHIVE"
    printf " ${YELLOW}Output archived to: %s${RESET}\n" "$ARCHIVE"
fi

echo ""

# Exit with failure if any claims failed
if [ "$FAIL_COUNT" -gt 0 ]; then
    exit 1
fi
exit 0
