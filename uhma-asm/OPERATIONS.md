# UHMA Operations Manual

Complete guide for operating UHMA (Unified Holographic Memory Architecture).

**Architecture**: Pure x86-64 assembly. No runtime dependencies.

---

## Quick Start

```bash
cd /home/peter/Desktop/STARWARS/uhma-asm

# Build everything
make clean && make
cd pet/x86 && make && cd ../..

# Option 1: GUI (primary interface)
./gui/uhma-viz

# Option 2: Interactive REPL
./uhma

# Option 3: Headless training
./uhma < /dev/null &
./tools/feeder --corpus corpus/ --cycles 1
```

---

## Three Ways to Run

### 1. GUI (Command & Control Center)

```bash
cd gui && make && ./uhma-viz
```

The GUI provides visual monitoring and full control. Output is mirrored via the gateway stream and routed by the originating command's subnet; FEED is the live run-log for processing/autonomy, while QUERY/DEBUG are polled.

**Startup**: GUI starts without UHMA. Click **DREAM** for autonomous mode or **FEED** for training.

| Feature | Description |
|---------|-------------|
| Mind Map | Central node with subsystems (BRAIN, REGIONS, TOKENS, CREATURE...) |
| Carousel | Click node to expand, click outside to collapse |
| Side Panels | FEED/QUERY/DEBUG panels (gateway subnets); FEED is live run-log |
| Auto-Polling | Sends status/receipts every ~3 seconds (QUERY/DEBUG only) |
| Clipboard | Click panel or collapse node to copy (requires `xclip`) |
| Creature | Live creature panel showing UHMA's cognitive state |

**Layout**:
```
┌──────────────────────────────────────────────────────────────┐
│ [DREAM] [OBSERVE] [EVOLVE] [STEP] [RUN] [SAVE] [LOAD] [FEED▼] │
├──────────────────────────────────┬───────────────────────────┤
│                                  │ FEED                      │
│     Mind Map / Carousel          │ [live run-log]             │
│                                  ├───────────────────────────┤
│     Click nodes to inspect       │ QUERY                     │
│                                  │ [status/why/misses]       │
│                                  ├───────────────────────────┤
│                                  │ DEBUG                     │
│                                  │ [receipts/trace]          │
├──────────────────────────────────┴───────────────────────────┤
│ INPUT: _______________                      [SEND] [CLEAR]   │
└──────────────────────────────────────────────────────────────┘
```

### 2. Interactive REPL

```bash
./uhma
uhma> hello world          # process text (learn patterns)
uhma> status               # system state
uhma> eat corpus/tiny.txt  # digest a file
uhma> observe              # build self-model
uhma> dream                # consolidate patterns
uhma> autonomy             # see what UHMA is doing
uhma> quit                 # exit (auto-saves)
```

### 3. Headless Training (feeder)

```bash
# Single pass through corpus
./tools/feeder --corpus corpus/ --cycles 1

# Infinite training
./tools/feeder --cycles 0

# Spawn UHMA if not running
./tools/feeder --spawn --corpus corpus/ --cycles 1

# Graceful shutdown
./tools/feeder --shutdown
```

| Option | Default | Description |
|--------|---------|-------------|
| `--corpus DIR` | corpus/ | Directory with training files |
| `--pause N` | 5 | Seconds between files |
| `--consolidate N` | 30 | Minutes between observe+dream |
| `--cycles N` | 1 | Number of cycles (0=infinite) |
| `--spawn` | off | Spawn UHMA if not running |
| `--shutdown` | - | Graceful shutdown |

---

## Autonomous Behavior

UHMA has 10 autonomous actions selected by holographic resonance — the current presence state is compared against learned action traces via dot product. Whatever resonates strongest fires.

### Actions

| # | Action | Gate | Description |
|---|--------|------|-------------|
| 0 | DREAM | 0 | Offline pattern consolidation |
| 1 | OBSERVE | 0 | Self-observation, builds semantic self-model |
| 2 | EVOLVE | 0 | Genetic evolution of code regions |
| 3 | REST | 0 | Energy conservation |
| 4 | EXPLORE | 0 | Seek confused contexts |
| 5 | SEEK | 1 | File exploration (scans directories, digests text files) |
| 6 | SCAN_ENV | 1 | System introspection (/proc/cpuinfo, meminfo, etc.) |
| 7 | COMPOSE | 2 | Generate text from learned patterns |
| 8 | TEACH | 2 | Share knowledge via shared memory (colony mode only) |
| 9 | REFLECT | 2 | Deep self-examination (action patterns → self-model) |

### Maturity Gates

Actions are gated by developmental stage. UHMA earns capabilities by sustaining prediction accuracy, stability, and coherence.

| Stage | Name | Unlocks | Exploration Scope |
|-------|------|---------|-------------------|
| 0 | Infant | Actions 0-4 only | Internal (corpus/) |
| 1 | Aware | + SEEK, SCAN_ENV (file reading) | corpus/ and subdirectories |
| 2 | Active | + COMPOSE, REFLECT, file writing | Broader filesystem |

Advancement requires sustaining 75% accuracy, 80% stability, 70% coherence for 1000 steps.

### Curiosity Pressure

When prediction accuracy drops below 30%, curiosity pressure builds. If SEEK hasn't fired recently and energy is above 50%, curiosity triggers SEEK — driving UHMA to explore files for new patterns. This creates an organic learning drive independent of resonance.

### File Exploration

SEEK probes files before digesting them — reads first 256 bytes, checks if >75% are printable characters. This catches text files regardless of extension, skips binaries. A frontier queue tracks directories breadth-first, expanding outward as maturity increases.

Use `autonomy` to see live resonance scores, fire counts, frontier status, and curiosity pressure.

---

## Command Reference

### Information

| Command | Description |
|---------|-------------|
| `help` | Show all commands |
| `status` | Regions, accuracy, steps, faults, energy |
| `presence` | 30-dim phenomenal state (arousal, valence, continuity...) |
| `drives` | Drive levels (dream, observe, evolve, fatigue) |
| `intro` | Introspective state (confused/confident/learning, SELF-AWARE %) |
| `self` | Strengths/weaknesses by context type |
| `metacog` | Metacognitive confidence for last prediction |
| `autonomy` | Resonance scores for all 10 actions + frontier + curiosity |
| `compose` | Show composition buffer (generated text) |

### Debugging

| Command | Description |
|---------|-------------|
| `why` | Explain last prediction failure |
| `misses [n]` | Last n misses with predicted vs actual |
| `receipts [n]` | Last n trace entries |
| `debugger` | Breakpoints, hits, learning events |
| `causal` | What modifications work in current context |
| `trace` | Toggle journey tracing |
| `regions` | All regions with hit/miss stats |

### Actions

| Command | Description |
|---------|-------------|
| `eat <file>` | Digest file (extract tokens, gain energy) |
| `dream` | Consolidation cycle |
| `observe` | Self-observation (builds self-model, updates presence) |
| `compact` | Garbage collect condemned regions |
| `batch` | Toggle batch mode (disable autonomous workers) |
| `reset` | Reset counters (not knowledge) |

### Memory & Persistence

| Command | Description |
|---------|-------------|
| `save <name>` | Save surface checkpoint |
| `load <name>` | Load surface checkpoint |
| `export <n>` | Export region as .gene file |
| `import <file>` | Import .gene file |

### Collective

| Command | Description |
|---------|-------------|
| `hive` | Show pheromone levels |
| `share` | Enable shared consciousness (Mycorrhiza) |
| `colony` | Colony status |

### Safety

| Command | Description |
|---------|-------------|
| `geom` | Show geometric gate status |
| `geom 0/1/2` | Set verification mode (abstract/geometric/both) |

---

## Pet Creature (x86 Prototype)

The creature lives inside the GUI carousel — a visual translation of UHMA's internal state.

```bash
cd pet/x86 && make
./uhma-pet          # standalone
```

Or view in the GUI as the CREATURE node (gold, 82° position).

### What the Creature Shows

| UHMA State | Creature Expression |
|------------|---------------------|
| High valence | Happy face, tail wag |
| Low valence | Frown, drooping ears |
| High arousal | Fast breathing, alert |
| High fatigue | Sleepy eyes, slow |
| SEEK active | Ears perked, sniffing |
| SCAN_ENV active | Wide eyes, ears rotating |
| COMPOSE active | Slow deep tail wag, warm glow |
| REFLECT active | Half-closed eyes, meditation |

### Species (Future)

Four species crystallize from egg based on interaction patterns:

| Species | Personality | Driven by |
|---------|-------------|-----------|
| Hound | Eager, loyal | High engagement/arousal |
| Feline | Independent, selective | High focus, low arousal |
| Mech | Systematic, precise | High stability/coherence |
| Wisp | Ethereal, abstract | High novelty/entropy |

Currently only hound is rendered.

---

## MCP Integration (Claude Code)

The MCP server lets Claude Code query UHMA and manage holographic memory.

**Setup**: `.mcp.json` in project root configures automatic connection. Restart Claude Code to connect.

### UHMA Commands (via TCP gateway)

All REPL commands available: `status`, `why`, `dream`, `observe`, `eat`, `presence`, etc.

### Holographic Memory (Claude's own)

Separate 6GB surface for cross-session persistence and code RAG.

| Tool | Description |
|------|-------------|
| `mem_add` | Add entry (category + content + optional context) |
| `mem_query` | Semantic similarity search |
| `mem_state` | Cognitive state (entry counts by category) |
| `mem_recent` | Recent entries |
| `mem_summary` | Summary statistics |
| `mem_rag_refresh` | Rebuild code RAG entries |

14 categories: finding, failed, success, insight, warning, session, location, question, todo, context, request, code_high, code_mid, code_low.

---

## TCP Gateway

UHMA exposes a single TCP gateway on port 9999 for external tools.

**Protocol**: framed messages (magic + subnet + seq_id + length). Use the GUI, `tools/feeder`, or `tools/mcp_server` (raw `nc` won't work).
**Note**: response payloads are capped at `GW_MAX_PAYLOAD` (4096). Large outputs are truncated in request/response; use the GUI stream panels for the full run-log.

```bash
# Quick check (spawns UHMA if needed, feeds once)
./tools/feeder --spawn --cycles 1
```

The GUI, feeder, and MCP server all use this gateway.

---

## Shutdown

| Method | Command |
|--------|---------|
| GUI | Close window or FEED → Stop |
| REPL | `quit` |
| Feeder | `./tools/feeder --shutdown` |
| Emergency | `pkill -TERM uhma` (try graceful first) |

---

## Files

| File | Purpose |
|------|---------|
| `uhma` | Main UHMA binary |
| `gui/uhma-viz` | GUI executable |
| `pet/x86/uhma-pet` | Pet creature GUI |
| `tools/feeder` | Training client |
| `tools/mcp_server` | MCP protocol handler (Claude Code integration) |
| `uhma.surface` | Persistent memory (sparse file) |
| `corpus/*.txt` | Training files |
| `HOLO-memory/memory/holo_surface.dat` | Claude's holographic memory (6GB sparse) |

---

## Troubleshooting

| Problem | Cause | Solution |
|---------|-------|----------|
| GUI panels empty | UHMA not started | Click DREAM or FEED |
| UHMA hangs | Output not drained | Use GUI or feeder (auto-drains) |
| Port conflict | Multiple instances | `pkill -9 uhma` first |
| MCP tools missing | Server not connected | Restart Claude Code, check `/mcp` |
| Can't copy to clipboard | xclip missing | `apt install xclip` |
| Stage stuck at 0 | Not enough data | Feed more corpus, run observe |

---

*Last updated: 2026-02-05*
