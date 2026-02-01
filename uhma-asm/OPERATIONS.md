# UHMA Operations Manual

A complete guide for operating UHMA correctly.

---

## Quick Start

```bash
cd /home/peter/Desktop/STARWARS/uhma-asm

# Build if needed
make

# Single training cycle
./feed.sh --cycles 1

# Training + autonomous live mode
./feed.sh --cycles 1 --live

# Graceful shutdown
./feed.sh --shutdown
```

---

## System Overview

UHMA is a self-modifying x86-64 assembly program that learns patterns through holographic vector encoding:

- **Self-modifying**: Generates and executes its own x86 machine code at runtime
- **Holographic**: 8192-dim f64 vectors for memory and prediction
- **Persistent**: Learning survives restarts via `uhma.surface` (memory-mapped)
- **6-channel TCP**: Headless operation via paired TCP ports

---

## Training with feed.sh

`feed.sh` is the universal training script. It handles all UHMA communication correctly.

### Basic Commands

```bash
# Preview what would happen (no actual training)
./feed.sh --dry-run

# Single cycle through corpus/
./feed.sh --cycles 1

# Infinite training (Ctrl+C to stop)
./feed.sh --cycles 0

# Self-learning mode (feeds UHMA's responses back)
./feed.sh --mastery

# Custom corpus directory
./feed.sh --corpus data/ --cycles 1

# Faster/slower pacing
./feed.sh --pause 2        # 2 seconds between files
./feed.sh --pause 30       # 30 seconds between files
```

### All Options

| Option | Default | Description |
|--------|---------|-------------|
| `--corpus DIR` | corpus/ | Directory with .txt files |
| `--pause N` | 5 | Seconds between files |
| `--consolidate N` | 30 | Minutes between observe+dream |
| `--save-every N` | 30 | Minutes between checkpoints |
| `--order ORDER` | alpha | alpha/random/reverse |
| `--cycles N` | 0 | Number of cycles (0=infinite) |
| `--self-learn` | off | Feed UHMA's responses back |
| `--mastery` | - | Alias for --cycles 0 --self-learn |
| `--live` | off | Enter autonomous mode after training |
| `--live-pause N` | 10 | Seconds to wait before live mode |
| `--shutdown [NAME]` | - | Graceful shutdown |
| `--dry-run` | off | Preview without running |

### What feed.sh Does

1. **Starts UHMA** (if not running)
2. **Starts persistent drainers** for output ports (9998, 9996, 9994)
3. **Feeds each .txt file** via `eat filepath` command
4. **Consolidates** (observe + dream) at intervals
5. **Saves checkpoints** with automatic cleanup (keeps last 3)
6. **Alerts Claude** on 5 consecutive failures (via ALERT_CLAUDE.txt)

---

## Shutdown

### Graceful Shutdown (Recommended)

```bash
# From command line
./feed.sh --shutdown              # Auto-named save
./feed.sh --shutdown my_save      # Custom name

# During training: Ctrl+C
# (Automatically saves and exits gracefully)
```

**Shutdown sequence:**
1. `save <name>` - saves state
2. `quit` - UHMA calls `surface_freeze` (sync to disk) then exits
3. Wait up to 10s for graceful exit
4. SIGTERM fallback, then SIGKILL if needed
5. Stop drainers, clean PID files

### Emergency Shutdown

```bash
pkill -TERM uhma    # Try graceful first
pkill -9 uhma       # Force kill if stuck
pkill -f feed.sh    # Kill training script
pkill -f "nc localhost"  # Kill drainers
```

---

## Live Autonomous Mode

After training, UHMA can self-explore:

```bash
# Train corpus, then enter live mode
./feed.sh --cycles 1 --live

# With custom wait time
./feed.sh --cycles 1 --live --live-pause 30
```

**How it works:**
1. Completes training cycles
2. Waits N seconds for user input (default 10)
3. If no input, enters autonomous exploration
4. Feeds ALL files in exploration path (no filtering)
5. Consolidates every 50 files
6. Saves every 100 files
7. Expands exploration based on maturity:
   - **Stage 0 (Infant)**: uhma-asm folder only
   - **Stage 1 (Child)**: Desktop
   - **Stage 2+ (Adolescent/Adult)**: Home folder

---

## Interactive Mode

For testing and debugging, run UHMA directly:

```bash
./uhma
uhma> hello world          # Process text
uhma> status               # System state
uhma> why                  # Explain last miss
uhma> misses 5             # Recent failures
uhma> eat file.txt         # Digest file
uhma> observe              # Self-observation
uhma> dream                # Consolidation
uhma> save mystate         # Save checkpoint
uhma> quit                 # Exit (saves automatically)
```

---

## TCP Channels

UHMA exposes 3 paired TCP channels:

| Channel | Input | Output | Purpose |
|---------|-------|--------|---------|
| FEED | 9999 | 9998 | eat, dream, observe, save, quit |
| QUERY | 9997 | 9996 | status, why, misses, intro, self |
| DEBUG | 9995 | 9994 | receipts, trace |

### Critical: Output Port Draining

**Output ports MUST be continuously drained or UHMA blocks.**

**Wrong** (per-command reader):
```bash
nc localhost 9998 > response.txt &  # Temporary reader
echo "eat file.txt" | nc localhost 9999
# Reader times out, UHMA blocks
```

**Right** (persistent drainers):
```bash
# Start drainers ONCE at session start
while true; do nc localhost 9998 >> feed.out || sleep 1; done &
while true; do nc localhost 9996 >> query.out || sleep 1; done &
while true; do nc localhost 9994 >> debug.out || sleep 1; done &

# Then send commands freely
echo "eat file.txt" | timeout 2 nc -N localhost 9999 || true
```

**feed.sh handles this automatically.** Only use raw TCP if you have a specific need.

---

## Commands Reference

### Information

| Command | Description |
|---------|-------------|
| `help` | Show all commands |
| `status` | System state (regions, accuracy, drives) |
| `intro` | Introspective state (CONFUSED/CONFIDENT/LEARNING) |
| `self` | Strengths/weaknesses by context type |
| `presence` | 30-dim phenomenal state |
| `drives` | Drive levels |

### Debugging

| Command | Description |
|---------|-------------|
| `why` | Explain last prediction failure |
| `misses N` | Show last N misses |
| `receipts N` | Show last N trace entries |

### Actions

| Command | Description |
|---------|-------------|
| `eat <file>` | Digest file |
| `dream` | Consolidation cycle |
| `observe` | Self-observation |
| `compact` | Garbage collect |
| `save <name>` | Save state |
| `load <name>` | Load state |
| `batch` | Toggle batch mode |
| `quit` | Exit (saves first) |

---

## Files

### Scripts

| File | Purpose |
|------|---------|
| `feed.sh` | Universal training script (replaces all old training scripts) |
| `validate.sh` | Validation and testing |
| `hub_claude.py` | Claude connector for multi-agent hub |
| `hub_gemini.py` | Gemini connector for multi-agent hub |

### Core

| File | Purpose |
|------|---------|
| `uhma` | Main executable |
| `uhma.surface` | Persistent memory (200GB sparse) |
| `hub` | Multi-agent hub executable |

### Corpus

| File | Purpose |
|------|---------|
| `corpus/*.txt` | Training files |
| `corpus_tiny/` | Small test corpus |

### Checkpoints

| Pattern | Purpose |
|---------|---------|
| `checkpoint_*` | Periodic saves (keeps last 3) |
| `cycle_*` | End-of-cycle saves (keeps last 3) |
| `live_*` | Live mode saves (keeps last 10) |
| `shutdown_*` | Graceful shutdown saves |
| `interrupted` | Ctrl+C save |

---

## Troubleshooting

### UHMA Hangs

**Cause:** Output ports not drained
**Solution:** Use feed.sh (handles draining automatically)

### Port Conflicts

**Cause:** Multiple UHMA instances
**Solution:**
```bash
pkill -9 uhma
pkill -f "nc localhost"
./feed.sh --cycles 1
```

### UHMA Blocks at Startup

**Cause:** Auto-dream triggered (batch_mode=0)
**Solution:** Ensure `batch_mode=1` in introspect.asm (default)

### Script Exits Early

**Cause:** `set -euo pipefail` + failing command
**Solution:** Commands in feed.sh have `|| true` - if still failing, check feed.log

### Can't Connect via TCP

**Cause:** UHMA not in headless mode
**Solution:** Start via `./feed.sh` or `./uhma < /dev/null`

---

## Typical Workflows

### Fresh Training

```bash
rm -f uhma.surface checkpoint_* cycle_*
./feed.sh --cycles 2 --pause 5
```

### Resume Training

```bash
./feed.sh --cycles 1  # Continues from existing surface
```

### Training + Live Mode

```bash
./feed.sh --cycles 1 --live --live-pause 5
```

### Debug Session

```bash
./uhma
> status
> intro
> why
> misses 10
> observe
> status
```

### Graceful Stop

```bash
./feed.sh --shutdown my_checkpoint
```

---

## For AI Systems

1. **Use feed.sh** - it handles all the TCP complexity
2. **Check status** frequently to monitor progress
3. **Use --dry-run** first to preview
4. **Graceful shutdown** with `--shutdown` or Ctrl+C
5. **Check feed.log** for debugging

The key insight: UHMA learns by prediction error. Feed it text, let it fail, consolidate the failures into patterns. Repeat until accuracy climbs.

---

*Last updated: 2026-02-01*
