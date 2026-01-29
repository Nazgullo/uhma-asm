# UHMA Operations Manual

A complete guide for AI systems and humans to operate UHMA correctly.

---

## Table of Contents

1. [System Overview](#system-overview)
2. [Starting UHMA](#starting-uhma)
3. [Connection Methods](#connection-methods)
4. [Core Commands](#core-commands)
5. [Training Workflow](#training-workflow)
6. [Debugging & Diagnostics](#debugging--diagnostics)
7. [Files & Their Purpose](#files--their-purpose)
8. [Common Sequences](#common-sequences)
9. [Troubleshooting](#troubleshooting)

---

## System Overview

UHMA is a self-modifying x86-64 assembly program that learns patterns through holographic vector encoding. It runs as a single binary (`./uhma`) that:

- Reads input (text, files)
- Predicts next tokens using learned patterns
- Modifies its own executable code regions
- Persists all learning to `uhma.surface` (memory-mapped file)

**Key Properties:**
- 8192-dimensional f64 vectors (holographic encoding)
- Self-modifying dispatch regions (x86 machine code)
- 6-channel TCP I/O for headless operation
- Persistent memory across restarts

---

## Starting UHMA

### Fresh Start (New Surface)

```bash
# Kill any existing instances
pkill uhma
pkill hub

# Remove old surface (WARNING: loses all learning)
rm -f uhma.surface

# Build if needed
make

# Start UHMA interactively
./uhma
```

### Resume Previous Session

```bash
# Just start - it loads existing surface automatically
./uhma
```

You'll see:
```
[SURFACE] Loading persistent memory from uhma.surface
[SURFACE] Session #N (X total steps)
[SURFACE] Recovered Y regions, Z vocabulary entries
```

### Headless Mode (TCP Only)

```bash
# Start with stdin from /dev/null
./uhma < /dev/null &

# UHMA now listens on TCP ports only
# Channels:
#   FEED:  9999 (in) → 9998 (out)
#   QUERY: 9997 (in) → 9996 (out)
#   DEBUG: 9995 (in) → 9994 (out)
```

### With Hub (Multi-Agent Mode)

```bash
# Start hub first
./hub &
# Hub listens on port 7777

# Start UHMA (it auto-connects to hub if available)
./uhma

# Other agents connect to hub:
echo "HELLO claude" | nc localhost 7777
```

---

## Connection Methods

### Method 1: Interactive Terminal (Recommended for Testing)

Just run `./uhma` and type commands directly.

```
uhma> status
uhma> eat myfile.txt
uhma> hello world
```

### Method 2: TCP Channels (For Automation)

UHMA exposes 3 channel pairs:

| Channel | Input Port | Output Port | Purpose |
|---------|------------|-------------|---------|
| FEED | 9999 | 9998 | eat, dream, observe, compact |
| QUERY | 9997 | 9996 | status, why, misses, intro, self |
| DEBUG | 9995 | 9994 | receipts, trace |

**Usage Pattern:**
```bash
# Send command to input port
echo "status" | nc localhost 9997

# Read response from output port (in another terminal)
nc localhost 9996
```

**Important:** You must connect to BOTH ports of a channel pair.

### Method 3: Hub (Multi-Agent)

```bash
# Connect to hub
nc localhost 7777

# Register yourself
HELLO myname

# Send commands to UHMA (prefix with @uhma)
@uhma status

# Broadcast to all
MSG hello everyone
```

### Method 4: MCP Server (Claude Code Integration)

The MCP server (`tools/rag/server.py`) spawns UHMA as a subprocess and communicates via stdin/stdout.

```bash
# Start MCP server (usually done by Claude Code automatically)
python3 tools/rag/server.py
```

Then use MCP tools: `mcp__uhma__status`, `mcp__uhma__input`, etc.

---

## Core Commands

### Information Commands

| Command | Description | Example Output |
|---------|-------------|----------------|
| `help` | Show all commands | Command list |
| `status` | System state overview | Regions, accuracy, steps, drives |
| `intro` | Introspective state | CONFUSED/CONFIDENT/LEARNING readings |
| `self` | Strengths/weaknesses by context | Which patterns it's good/bad at |
| `presence` | 30-dim phenomenal state | Arousal, valence, novelty, etc. |
| `drives` | Drive levels | Accuracy, efficiency, novelty, coherence |

### Debugging Commands

| Command | Description |
|---------|-------------|
| `why` | Explain last prediction failure |
| `misses N` | Show last N misses with predicted vs actual |
| `receipts N` | Show last N trace entries |

### Action Commands

| Command | Description |
|---------|-------------|
| `eat <file>` | Digest file (extract tokens, learn patterns) |
| `dream` | Consolidation cycle (replay misses, extract schemas) |
| `observe` | Self-observation (build semantic self-model) |
| `compact` | Garbage collect condemned regions |
| `save <name>` | Save surface to file |
| `load <name>` | Load surface from file |
| `quit` | Exit (auto-saves) |

### Text Input

Any input that doesn't match a command is processed as text:
```
uhma> hello world
Processing: hello world
  #1 HIT ctx=0x12345678 tok=0xabcd1234
  ...
```

---

## Training Workflow

### Phase 1: Language Foundation

Feed natural language text to teach word patterns:

```bash
# In UHMA terminal:
eat movie_conversations.txt

# Wait for completion (83K lines ≈ 2-5 minutes)
# Watch accuracy climb from 0% → 20-40%

status
# Check: Accuracy should be > 0.2
```

### Phase 2: Self-Reference

Feed UHMA its own source code:

```bash
# Create codebase.txt first (outside UHMA):
cat *.asm include/*.inc > codebase.txt

# In UHMA:
eat codebase.txt

# This triggers self-referential mode
# ST_IS_SELF_REF flag is set for .asm files
```

### Phase 3: Consolidation

After feeding, consolidate learning:

```bash
dream
# Replays recent misses
# Extracts schemas from patterns
# Decays old traces

observe
# Builds semantic self-model
# Computes self-awareness score
# Prunes/promotes regions
```

### Phase 4: Repeat

For deeper learning, repeat the cycle:

```bash
# Round 2
eat movie_conversations.txt
dream
observe
status

# Round 3
eat codebase.txt
dream
observe
status

# Continue until accuracy stabilizes
```

### Typical Training Session

```bash
# Fresh start
rm uhma.surface
./uhma

# Feed language
eat movie_conversations.txt
status

# Feed self
eat codebase.txt
observe
status

# Consolidate
dream
status

# Second pass
eat movie_conversations.txt
eat codebase.txt
dream
observe

# Save good state
save trained_v1

# Check maturity
status
# Look for: Stage > 0, Accuracy > 0.3
```

---

## Debugging & Diagnostics

### When Accuracy is Low

```bash
why
# Shows: predicted X, got Y, context was Z, because...

misses 10
# Shows last 10 prediction failures
# Look for patterns in what it's getting wrong
```

### When System Seems Stuck

```bash
intro
# Check CONFUSED vs CONFIDENT readings
# High confusion = step back, simplify input

presence
# Check AROUSAL, FATIGUE, DISSONANCE
# High fatigue = let it rest (dream cycle)
```

### When System Crashes

1. Check if surface file exists: `ls -lh uhma.surface`
2. Restart: `./uhma` (it will recover)
3. If crashes repeatedly: `rm uhma.surface` and retrain

### Memory Issues

```bash
status
# Check: Regions count, Vocabulary size

compact
# Garbage collect condemned regions
# Frees memory from failed patterns
```

---

## Files & Their Purpose

### Core Files

| File | Purpose |
|------|---------|
| `uhma` | Main executable (built by `make`) |
| `hub` | Multi-agent hub executable |
| `uhma.surface` | Persistent memory (200GB sparse file) |
| `Makefile` | Build system |

### Source Files

| File | Purpose |
|------|---------|
| `boot.asm` | Entry point, initialization |
| `repl.asm` | Command loop, input handling |
| `dispatch.asm` | Token prediction, pattern matching |
| `learn.asm` | Pattern learning |
| `emit.asm` | x86 code generation |
| `vsa.asm` | Holographic vector operations |
| `dreams.asm` | Consolidation cycles |
| `observe.asm` | Self-observation |
| `introspect.asm` | Self-model, cognitive state |
| `receipt.asm` | Trace system (debugging) |
| `presence.asm` | Phenomenal state (30-dim) |
| `include/constants.inc` | All constants and memory layout |

### Training Files

| File | Purpose |
|------|---------|
| `movie_conversations.txt` | Natural language training corpus |
| `codebase.txt` | Concatenated .asm files for self-reference |

### Tool Files

| File | Purpose |
|------|---------|
| `tools/rag/server.py` | MCP server for Claude Code |
| `tools/rag/holo_memory.py` | Holographic memory extension |
| `tools/rag/hook.py` | Pre-tool context injection |
| `hub_claude.py` | Claude connector for hub |
| `hub_gemini.py` | Gemini connector for hub |

### Documentation

| File | Purpose |
|------|---------|
| `CLAUDE.md` | Project guidelines, gotchas, architecture |
| `OPERATIONS.md` | This file - how to operate UHMA |
| `HOLO-memory/README.md` | Holographic memory extension docs |

---

## Common Sequences

### Quick Test

```bash
./uhma
> hello world
> status
> quit
```

### Full Training Run

```bash
rm uhma.surface
./uhma
> eat movie_conversations.txt
> status
> eat codebase.txt
> observe
> dream
> status
> save checkpoint1
> quit
```

### Debugging a Problem

```bash
./uhma
> status          # Overview
> intro           # Cognitive state
> why             # Last failure
> misses 5        # Recent failures
> presence        # Phenomenal state
> observe         # Rebuild self-model
> status          # Check improvement
```

### Multi-Agent Session

```bash
# Terminal 1: Hub
./hub

# Terminal 2: UHMA
./uhma

# Terminal 3: Connect as Claude
python3 hub_claude.py
# or manually:
nc localhost 7777
HELLO claude
@uhma status
```

### Recovery After Crash

```bash
# Just restart - surface persists
./uhma
> status
# Check regions recovered, continue from there
```

---

## Troubleshooting

### "eat: error opening file"

- Use absolute path: `eat /full/path/to/file.txt`
- Or ensure file is in working directory

### UHMA uses only 5MB RAM

- Normal - the 4GB is mmap'd page cache from surface file
- Kernel manages it; UHMA's private memory is ~5MB

### System stuck after bootstrap

- Bootstrap observe can take time on first run
- Wait 30 seconds, then check if responding

### Accuracy stays at 0%

- Feed more data: `eat movie_conversations.txt`
- Run consolidation: `dream`
- Check for crashes: `misses 10`

### Segfaults during operation

- Usually memory layout issue
- Check `include/constants.inc` for correct offsets
- Rebuild: `make clean && make`

### Hub "init failed"

- Port 7777 might be in use
- Kill other processes: `pkill hub; pkill nc`
- Retry

### TCP channels not responding

- Ensure UHMA started headless: `./uhma < /dev/null`
- Check ports: `ss -tlnp | grep 999`
- Must connect to BOTH input and output ports

---

## Real-World Usage Examples

### Example 1: Automated Headless Training

When you can't use interactive mode (scripts, CI, other AI agents):

```bash
#!/bin/bash
# train_headless.sh - Non-interactive training

cd /home/peter/Desktop/STARWARS/uhma-asm

# 1. Kill old instances, start fresh
pkill -9 uhma 2>/dev/null
sleep 1

# 2. Start headless (enables TCP channels)
./uhma < /dev/null > uhma_training.log 2>&1 &
UHMA_PID=$!
sleep 3

# 3. Verify it's running
if ! kill -0 $UHMA_PID 2>/dev/null; then
    echo "UHMA failed to start"
    exit 1
fi

# 4. Send commands via TCP (FEED channel: 9999 in, 9998 out)
echo "eat codebase.txt" | nc -q 1 localhost 9999
sleep 60

echo "observe" | nc -q 1 localhost 9999
sleep 10

echo "dream" | nc -q 1 localhost 9999
sleep 10

# 5. Query status via TCP (QUERY channel: 9997 in, 9996 out)
echo "status" | nc -q 1 localhost 9997
nc -w 2 localhost 9996

echo "Training complete. UHMA running as PID $UHMA_PID"
```

---

### Example 2: Chunked Feeding for Large Files

Large files can overwhelm UHMA. Split into chunks:

```bash
#!/bin/bash
# feed_chunked.sh <file> [chunk_lines]

FILE="$1"
CHUNK_LINES="${2:-500}"  # Default 500 lines per chunk

if [ ! -f "$FILE" ]; then
    echo "Usage: $0 <file> [chunk_lines]"
    exit 1
fi

TOTAL=$(wc -l < "$FILE")
CHUNKS=$(( (TOTAL + CHUNK_LINES - 1) / CHUNK_LINES ))

echo "Feeding $FILE ($TOTAL lines) in $CHUNKS chunks..."

OFFSET=1
CHUNK=1

while [ $OFFSET -le $TOTAL ]; do
    # Extract chunk to temp file
    sed -n "${OFFSET},$((OFFSET + CHUNK_LINES - 1))p" "$FILE" > /tmp/chunk.txt

    # Feed via TCP
    echo "eat /tmp/chunk.txt" | nc -q 1 localhost 9999

    echo "  [$CHUNK/$CHUNKS] Lines $OFFSET-$((OFFSET + CHUNK_LINES - 1))"

    sleep 5  # Pause between chunks

    OFFSET=$((OFFSET + CHUNK_LINES))
    CHUNK=$((CHUNK + 1))
done

rm -f /tmp/chunk.txt
echo "Done feeding $FILE"
```

**Usage:**
```bash
# Start UHMA headless first
./uhma < /dev/null &
sleep 3

# Feed large file in 500-line chunks
./feed_chunked.sh movie_conversations.txt 500

# Feed codebase in 300-line chunks (denser content)
./feed_chunked.sh codebase.txt 300
```

---

### Example 3: Full Training Pipeline with Log Feedback

Train UHMA on content + its own learning logs (meta-learning):

```bash
#!/bin/bash
# full_training.sh - Complete training with log feedback

cd /home/peter/Desktop/STARWARS/uhma-asm

# Config
CHUNK_LINES=500
PAUSE_CHUNK=5
PAUSE_FILE=30

# Start fresh
pkill -9 uhma 2>/dev/null
rm -f uhma_training.log
sleep 1

./uhma < /dev/null > uhma_training.log 2>&1 &
sleep 3

# Helper: feed file in chunks
feed_chunked() {
    local file="$1"
    local lines=$(wc -l < "$file")
    local chunks=$(( (lines + CHUNK_LINES - 1) / CHUNK_LINES ))
    local offset=1
    local n=1

    echo "[$(date +%H:%M:%S)] Feeding $file ($lines lines, $chunks chunks)"

    while [ $offset -le $lines ]; do
        sed -n "${offset},$((offset + CHUNK_LINES - 1))p" "$file" > /tmp/chunk.txt
        echo "eat /tmp/chunk.txt" | nc -q 1 localhost 9999
        echo "  Chunk $n/$chunks"
        sleep $PAUSE_CHUNK
        offset=$((offset + CHUNK_LINES))
        n=$((n + 1))
    done
    rm -f /tmp/chunk.txt
}

# Helper: query status
status() {
    echo "status" | nc -q 1 localhost 9997
    nc -w 2 localhost 9996 2>/dev/null
}

# Helper: send command
cmd() {
    echo "$1" | nc -q 1 localhost 9999
    echo "[$(date +%H:%M:%S)] $1"
}

echo "=== Pass 1: Codebase ==="
feed_chunked codebase.txt
sleep $PAUSE_FILE
status

echo "=== Pass 1: Training Log (meta-learning) ==="
feed_chunked uhma_training.log
sleep $PAUSE_FILE

cmd "observe"
sleep 10
cmd "dream"
sleep 10
status

echo "=== Pass 2: Codebase ==="
feed_chunked codebase.txt
sleep $PAUSE_FILE

echo "=== Pass 2: Training Log ==="
feed_chunked uhma_training.log
sleep $PAUSE_FILE

cmd "observe"
sleep 10
cmd "dream"
sleep 10

echo "=== Training Complete ==="
status
```

---

### Example 4: Continuous Monitoring

Monitor UHMA while it's running:

```bash
# Option 1: Watch status every 2 minutes
watch -n 120 'echo "status" | nc -q 1 localhost 9997; nc -w 1 localhost 9996'

# Option 2: Log to file with timestamps
while true; do
    echo "=== $(date) ===" >> uhma_monitor.log
    echo "status" | nc -q 1 localhost 9997
    nc -w 2 localhost 9996 >> uhma_monitor.log 2>/dev/null
    sleep 120
done

# Option 3: Monitor emergence indicators
watch -n 120 'echo "intro" | nc -q 1 localhost 9997; nc -w 1 localhost 9996'
```

---

### Example 5: Safe File Feeding (Prevents Kills)

If UHMA gets killed during large file processing:

```bash
#!/bin/bash
# safe_feed.sh <file> - Ultra-safe feeding with health checks

FILE="$1"
CHUNK=200        # Smaller chunks
PAUSE=10         # Longer pauses
HEALTH_EVERY=5   # Health check every N chunks

feed_with_health() {
    local lines=$(wc -l < "$FILE")
    local offset=1
    local n=1

    while [ $offset -le $lines ]; do
        # Health check
        if [ $((n % HEALTH_EVERY)) -eq 0 ]; then
            if ! pgrep -x uhma > /dev/null; then
                echo "UHMA died! Restarting..."
                ./uhma < /dev/null >> uhma_training.log 2>&1 &
                sleep 3
            fi
        fi

        # Feed chunk
        sed -n "${offset},$((offset + CHUNK - 1))p" "$FILE" > /tmp/safe_chunk.txt
        echo "eat /tmp/safe_chunk.txt" | nc -q 1 localhost 9999 2>/dev/null

        echo "Chunk $n (lines $offset-$((offset + CHUNK - 1)))"
        sleep $PAUSE

        offset=$((offset + CHUNK))
        n=$((n + 1))
    done
}

feed_with_health
rm -f /tmp/safe_chunk.txt
```

---

### Example 6: Interactive Session via TCP

Connect interactively to headless UHMA:

```bash
# Terminal 1: Start UHMA headless
./uhma < /dev/null > uhma.log 2>&1 &

# Terminal 2: Interactive query session
# (Opens persistent connection to query channel)
while true; do
    read -p "uhma> " cmd
    [ "$cmd" = "quit" ] && break
    echo "$cmd" | nc -q 1 localhost 9997
    nc -w 2 localhost 9996
done

# Terminal 3: Watch feed output (optional)
nc localhost 9998
```

---

### Example 7: Prepare Training Files

```bash
# Create codebase.txt from all assembly files
cat *.asm include/*.inc > codebase.txt
echo "Created codebase.txt ($(wc -l < codebase.txt) lines)"

# Create smaller test file
head -1000 movie_conversations.txt > test_corpus.txt

# Verify files
ls -lh codebase.txt movie_conversations.txt test_corpus.txt
```

---

### Example 8: Checkpoint and Restore

```bash
# Save checkpoint before risky operation
echo "save checkpoint_before_experiment" | nc -q 1 localhost 9999
sleep 2

# Do experiment...
echo "eat experimental_data.txt" | nc -q 1 localhost 9999

# If things go wrong, restore
echo "load checkpoint_before_experiment" | nc -q 1 localhost 9999
```

---

### TCP Channel Quick Reference

| Action | Command |
|--------|---------|
| Send to FEED | `echo "CMD" \| nc -q 1 localhost 9999` |
| Read FEED output | `nc -w 2 localhost 9998` |
| Send to QUERY | `echo "CMD" \| nc -q 1 localhost 9997` |
| Read QUERY output | `nc -w 2 localhost 9996` |
| Send to DEBUG | `echo "CMD" \| nc -q 1 localhost 9995` |
| Read DEBUG output | `nc -w 2 localhost 9994` |

**Channel routing:**
- FEED (9999/9998): `eat`, `dream`, `observe`, `compact`, `save`, `load`
- QUERY (9997/9996): `status`, `why`, `misses`, `intro`, `self`, `presence`, `drives`
- DEBUG (9995/9994): `receipts`, `trace`

---

## Quick Reference Card

```
START:     ./uhma                    (interactive)
           ./uhma < /dev/null &      (headless)

COMMANDS:  status    - system state
           eat FILE  - digest file
           dream     - consolidate
           observe   - self-model
           why       - debug last miss
           quit      - exit

TCP:       9999→9998  FEED
           9997→9996  QUERY
           9995→9994  DEBUG

FILES:     uhma.surface              - memory
           movie_conversations.txt   - language training
           codebase.txt              - self-reference

WORKFLOW:  eat → observe → dream → repeat
```

---

## For AI Systems

If you are an AI system trying to operate UHMA:

1. **Start with fresh surface** for clean experiments
2. **Use interactive mode** (just run `./uhma`) for testing
3. **Feed files with absolute paths** to avoid path issues
4. **Check `status` frequently** to monitor progress
5. **Use `why` and `misses`** to understand failures
6. **Run `dream` and `observe`** after feeding data
7. **Save checkpoints** with `save name` before experiments

The key insight: UHMA learns by prediction error. Feed it text, let it fail, consolidate the failures into patterns. Repeat until accuracy climbs.

---

*Generated by Claude Opus 4.5 for UHMA project documentation*
