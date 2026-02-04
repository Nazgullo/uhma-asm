# UHMA Operations Manual

A complete guide for operating UHMA correctly.

**Architecture**: Pure x86-64 assembly. No Python dependencies.

---

## Quick Start

```bash
cd /home/peter/Desktop/STARWARS/uhma-asm

# Build if needed
make

# Start UHMA interactively
./uhma

# Start UHMA headless (TCP-only)
./uhma < /dev/null &

# Training via feeder
./tools/feeder --corpus corpus/ --cycles 1

# Graceful shutdown
./tools/feeder --shutdown
```

---

## System Overview

UHMA is a self-modifying x86-64 assembly program that learns patterns through holographic vector encoding:

- **Self-modifying**: Generates and executes its own x86 machine code at runtime
- **Holographic**: 8192-dim f64 vectors for memory and prediction
- **Persistent**: Learning survives restarts via `uhma.surface` (memory-mapped)
- **6-channel TCP**: Headless operation via paired TCP ports

---

## Training with feeder (Assembly)

`tools/feeder` is the pure x86-64 assembly training client. No Python required.

### Basic Commands

```bash
# Single cycle through corpus/
./tools/feeder --corpus corpus/ --cycles 1

# Infinite training (Ctrl+C to stop)
./tools/feeder --cycles 0

# Custom corpus directory
./tools/feeder --corpus data/ --cycles 1

# Faster/slower pacing
./tools/feeder --pause 2        # 2 seconds between files
./tools/feeder --pause 30       # 30 seconds between files

# Spawn UHMA if not running
./tools/feeder --spawn --corpus corpus/ --cycles 1
```

### All Options

| Option | Default | Description |
|--------|---------|-------------|
| `--corpus DIR` | corpus/ | Directory with .txt files |
| `--pause N` | 5 | Seconds between files |
| `--consolidate N` | 30 | Minutes between observe+dream |
| `--cycles N` | 1 | Number of cycles (0=infinite) |
| `--spawn` | off | Spawn UHMA if not running |
| `--shutdown` | - | Graceful shutdown |

### What feeder Does

1. Connects to UHMA's TCP channels (ports 9999/9997/9995)
2. **Starts persistent drainers** for output ports (9998, 9996, 9994)
3. **Feeds each .txt file** via `eat filepath` command
4. **Consolidates** (observe + dream) at intervals
5. **Saves checkpoints** with automatic cleanup (keeps last 3)

---

## Shutdown

### Graceful Shutdown (Recommended)

```bash
# From command line
./tools/feeder --shutdown

# From REPL
./uhma
> save mystate
> quit
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

UHMA can self-explore via the GUI or by toggling batch_mode:

```bash
# Start GUI (enables autonomous mode automatically)
./gui/uhma-viz

# Or via REPL
./uhma
> batch     # toggle batch_mode off = autonomous
```

**How it works:**
1. When `batch_mode=0`, UHMA self-consolidates
2. Auto-dreams when dream_pressure exceeds threshold
3. Auto-observes to update self-model
4. Expands exploration based on maturity:
   - **Stage 0 (Infant)**: uhma-asm folder only
   - **Stage 1 (Child)**: Desktop
   - **Stage 2+ (Adolescent/Adult)**: Home folder

---

## GUI (Command & Control Center)

The GUI provides full visual monitoring and control:

```bash
cd gui && make
./uhma-viz
```

### Startup Flow

1. GUI starts without UHMA
2. **DREAM button** → spawns UHMA in live mode (batch OFF, autonomous)
3. **FEED menu** → spawns UHMA in feed mode (batch ON, externally driven)

### Features

| Feature | Description |
|---------|-------------|
| Mind Map | Central UHMA node with subsystems (BRAIN, REGIONS, TOKENS, etc.) |
| Carousel | Click node to expand, click outside to collapse |
| Side Panels | FEED/QUERY/DEBUG TCP streams |
| Auto-Polling | Sends status/receipts every ~3 seconds |
| Clipboard | Click panel or collapse node to copy content |

### Clipboard Copy

All content can be copied to clipboard for analysis:

- **Side panels** (FEED/QUERY/DEBUG): Click to toggle pause + copy
- **Carousel nodes**: Collapse (click outside) to copy
- Requires `xclip`: `apt install xclip`

### Panel Content

| Panel | Port | Commands | Content |
|-------|------|----------|---------|
| FEED | 9998 | eat, dream, observe | Action output, consolidation |
| QUERY | 9996 | status, why, intro | System state, diagnostics |
| DEBUG | 9994 | receipts, trace | Trace entries, debug info |

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

**tools/feeder handles this automatically.** Only use raw TCP if you have a specific need.

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

### Tools (Pure Assembly)

| File | Purpose |
|------|---------|
| `tools/feeder` | Training client (connects to UHMA TCP) |
| `tools/mcp_server` | MCP JSON-RPC server for Claude Code |

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
**Solution:** Use tools/feeder (handles draining automatically)

### Port Conflicts

**Cause:** Multiple UHMA instances
**Solution:**
```bash
pkill -9 uhma
pkill -f "nc localhost"
./tools/feeder --spawn --cycles 1
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
./uhma < /dev/null &
./tools/feeder --corpus corpus/ --cycles 2 --pause 5
```

### Resume Training

```bash
./tools/feeder --spawn --cycles 1  # Continues from existing surface
```

### Training + GUI Monitoring

```bash
./gui/uhma-viz &                   # Spawns UHMA in autonomous mode
./tools/feeder --cycles 1          # Feed corpus
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
./tools/feeder --shutdown
```

---

## For AI Systems (Claude Code)

1. **Use MCP tools** - `mcp__uhma__*` handles all communication
2. **Check status** frequently to monitor progress
3. **Use mem_add/mem_query** for cross-session memory
4. **Graceful shutdown** with `mcp__uhma__quit`

The key insight: UHMA learns by prediction error. Feed it text, let it fail, consolidate the failures into patterns. Repeat until accuracy climbs.

## Holographic Memory (Dual Purpose)

### Chat Sessions (11 categories)
finding, failed, success, insight, warning, session, location, question, todo, context, request

### 3-Layer Code RAG
| Layer | Category | Fidelity | Content |
|-------|----------|----------|---------|
| High | code_high | 0.98 decay | 1:1 raw source code |
| Mid | code_mid | 0.96 decay | Function signatures, @entry |
| Low | code_low | 0.92 decay | Summaries, gotchas, patterns |

All pure x86-64 assembly. No Python dependencies.

---

## Self-Awareness & Behavior (Observed 2026-02-03)

From a 6-minute observation session:

### Learning
- Repetitive patterns learned quickly: "one two three" predicted after 1 exposure
- Each new token creates executable x86 code via EMIT
- VERIFY ensures safety before any code modification

### Self-Awareness
| Metric | Value | Meaning |
|--------|-------|---------|
| SELF-AWARE | 76-77% | Semantic self-model matches actual code |
| SELF-SURPRISE | 0.004-0.119 | Spikes when self-modification occurs |
| Metacog feeling | ANXIOUS | During heavy learning |

### Self-Modification
- **PRUNE**: Removes low-performing regions (11 in one observe cycle)
- **FACTOR**: Finds common code patterns, creates shared subroutines
- **VERIFY**: Abstract interpretation approves all modifications

### Maturity Progression
- Session started: Stage 0 (Infant)
- Session ended: Stage 2 (Active)
- Growth: +61 regions, +109 vocabulary entries

---

*Last updated: 2026-02-03*
