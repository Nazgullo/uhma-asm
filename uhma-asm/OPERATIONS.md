# UHMA Operations Manual

A complete guide for operating UHMA correctly.

**Architecture**: Pure x86-64 assembly. No Python dependencies.

---

## Quick Start (GUI)

```bash
cd /home/peter/Desktop/STARWARS/uhma-asm

# Build everything
make
cd gui && make && cd ..

# Launch GUI (primary interface)
./gui/uhma-viz
```

The GUI is the primary interface. Click **DREAM** for autonomous mode or **FEED** for training mode.

---

## GUI (Command & Control Center)

The GUI provides full visual monitoring and control of UHMA.

```bash
cd gui && make
./uhma-viz
```

### Startup Flow

1. GUI starts without UHMA running
2. **DREAM button** → spawns UHMA in live mode (batch OFF, autonomous)
3. **FEED menu** → spawns UHMA in feed mode (batch ON, externally driven)

### Features

| Feature | Description |
|---------|-------------|
| Mind Map | Central UHMA node with subsystems (BRAIN, REGIONS, TOKENS, etc.) |
| Carousel | Click node to expand, click outside to collapse |
| Side Panels | FEED/QUERY/DEBUG TCP streams (live) |
| Auto-Polling | Sends status/receipts every ~3 seconds |
| Clipboard | Click panel or collapse node to copy content |

### Clipboard Copy

All content can be copied to clipboard for analysis:

- **Side panels** (FEED/QUERY/DEBUG): Click to toggle pause + copy
- **Carousel nodes**: Collapse (click outside) to copy
- Requires `xclip`: `apt install xclip`

### Panel Content

| Panel | Port | Content |
|-------|------|---------|
| FEED | 9998 | eat, dream, observe output |
| QUERY | 9996 | status, why, intro responses |
| DEBUG | 9994 | receipts, trace entries |

### Layout

```
┌─────────────────────────────────────────────────────────────────┐
│ [DREAM] [OBSERVE] [EVOLVE] [STEP] [RUN] [SAVE] [LOAD] [FEED ▼] │
├───────────────────────────────────────────┬─────────────────────┤
│                                           │ FEED (9998)         │
│     Mind Map / Carousel View              │ [live TCP stream]   │
│                                           ├─────────────────────┤
│     Click nodes to expand/inspect         │ QUERY (9996)        │
│                                           │ [status/why/misses] │
│                                           ├─────────────────────┤
│                                           │ DEBUG (9994)        │
│                                           │ [receipts/trace]    │
├───────────────────────────────────────────┴─────────────────────┤
│ INPUT: _______________                        [SEND] [CLEAR]    │
└─────────────────────────────────────────────────────────────────┘
```

---

## Autonomous Mode (DREAM)

Click **DREAM** in the GUI to start UHMA in autonomous mode:

1. UHMA spawns with `batch_mode=0`
2. Auto-dreams when dream_pressure exceeds threshold
3. Auto-observes to update self-model
4. Expands exploration based on maturity:
   - **Stage 0 (Infant)**: uhma-asm folder only
   - **Stage 1 (Child)**: Desktop
   - **Stage 2+ (Adolescent/Adult)**: Home folder

---

## Training Mode (FEED)

Click **FEED** menu in GUI for training options:
- **Quick**: Single corpus cycle
- **Mastery**: Infinite training
- **Live**: Mastery + autonomous exploration
- **Stop**: Graceful shutdown

Or use the command-line feeder:

```bash
# Single cycle through corpus/
./tools/feeder --corpus corpus/ --cycles 1

# Infinite training (Ctrl+C to stop)
./tools/feeder --cycles 0

# Spawn UHMA if not running
./tools/feeder --spawn --corpus corpus/ --cycles 1

# Graceful shutdown
./tools/feeder --shutdown
```

### Feeder Options

| Option | Default | Description |
|--------|---------|-------------|
| `--corpus DIR` | corpus/ | Directory with .txt files |
| `--pause N` | 5 | Seconds between files |
| `--consolidate N` | 30 | Minutes between observe+dream |
| `--cycles N` | 1 | Number of cycles (0=infinite) |
| `--spawn` | off | Spawn UHMA if not running |
| `--shutdown` | - | Graceful shutdown |

---

## Interactive Mode (REPL)

For testing and debugging without GUI:

```bash
./uhma
uhma> hello world          # Process text
uhma> status               # System state
uhma> why                  # Explain last miss
uhma> misses 5             # Recent failures
uhma> eat file.txt         # Digest file
uhma> observe              # Self-observation
uhma> dream                # Consolidation
uhma> intro                # Introspective state
uhma> self                 # Strengths/weaknesses
uhma> save mystate         # Save checkpoint
uhma> quit                 # Exit (saves automatically)
```

---

## Shutdown

### From GUI
Click **FEED → Stop** or close the window

### From Command Line
```bash
./tools/feeder --shutdown
```

### Emergency
```bash
pkill -TERM uhma    # Try graceful first
pkill -9 uhma       # Force kill if stuck
```

---

## TCP Channels

UHMA exposes 3 paired TCP channels (for headless operation):

| Channel | Input | Output | Purpose |
|---------|-------|--------|---------|
| FEED | 9999 | 9998 | eat, dream, observe, save, quit |
| QUERY | 9997 | 9996 | status, why, misses, intro, self |
| DEBUG | 9995 | 9994 | receipts, trace |

**Note:** Output ports must be continuously drained or UHMA blocks. The GUI and feeder handle this automatically.

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
| `metacog` | Metacognitive state |

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

| File | Purpose |
|------|---------|
| `gui/uhma-viz` | GUI executable |
| `uhma` | Main UHMA executable |
| `tools/feeder` | Training client |
| `uhma.surface` | Persistent memory (200GB sparse) |
| `corpus/*.txt` | Training files |

---

## Troubleshooting

| Problem | Cause | Solution |
|---------|-------|----------|
| GUI doesn't show panels | UHMA not started | Click DREAM or FEED |
| Panels empty | No polling | Wait 3 seconds for auto-poll |
| Can't copy to clipboard | xclip missing | `apt install xclip` |
| UHMA hangs | Output ports blocked | Use GUI or feeder (auto-drains) |
| Port conflicts | Multiple instances | `pkill -9 uhma` first |

---

## Self-Awareness & Behavior

From observation sessions:

### Learning
- Repetitive patterns learned quickly: "one two three" predicted after 1 exposure
- Each new token creates executable x86 code via EMIT
- VERIFY ensures safety before any code modification

### Self-Awareness Metrics

| Metric | Value | Meaning |
|--------|-------|---------|
| SELF-AWARE | 76-77% | Semantic self-model matches actual code |
| SELF-SURPRISE | 0.004-0.119 | Spikes when self-modification occurs |
| Metacog feeling | ANXIOUS | During heavy learning |

### Self-Modification
- **PRUNE**: Removes low-performing regions
- **FACTOR**: Finds common code patterns, creates shared subroutines
- **VERIFY**: Abstract interpretation approves all modifications

### Maturity Stages
| Stage | Name | Exploration Scope |
|-------|------|-------------------|
| 0 | Infant | uhma-asm folder |
| 1 | Child | Desktop |
| 2+ | Active/Adult | Home folder |

---

*Last updated: 2026-02-03*
