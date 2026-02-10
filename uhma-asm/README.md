# UHMA — Unified Holographic Memory Architecture

Self-modifying x86-64 assembly that learns patterns through holographic vector encoding.

## Quick Start (GUI-First)

```bash
make clean && make
./gui/uhma-viz
```

GUI-only flow (no CLI required):
1. Click **DREAM** to start autonomy (batch OFF).
2. Click **FEED▼ → Quick** to ingest the corpus.
3. Click **FILE** to ingest one file (type a path in the input bar or use the picker).
4. Click **URL** to ingest web content (type URL or use the dialog).

Requires: `nasm`, `ld` (GNU linker), Linux x86-64. The GUI uses X11 and `xclip`.

## What It Does

UHMA learns to predict token sequences. Feed it text, it builds pattern memory. Predictions that fail become learning events. Everything is stored as 1024-dimensional vectors using VSA (Vector Symbolic Architecture).

```
Input: "the cat sat"
       ↓
Hash context (previous token) → Search for matching pattern → Predict next token
       ↓
HIT (correct) → strengthen pattern
MISS (wrong) → learn correct pattern, emit new code region
```

The system generates x86-64 code at runtime. Faults (SIGSEGV, SIGILL) are caught and converted to learning signals.

## Living Loop Interactions (Order Matters)

UHMA’s “alive” loops are not isolated. They form a closed feedback system where the **order** of operations is fixed and stateful. The diagram below shows the dominant flow and how loops influence each other.

```
INPUT (REPL text or digest_file)
  → dispatch.process_token
     → HIT/MISS bookkeeping
     → learn_pattern on MISS/NEW
     → update_organic_pressure  (fixed order: dream → observe → evolve → introspect)
         → dream_cycle / observe_cycle / evolve_cycle when thresholds fire
            → updates regions + presence + drives
            → changes future dispatch accuracy

IDLE (no input)
  → tick_workers (autonomy loop)
     → resonance_select OR pressure fallback
     → action (dream / observe / evolve / rest / explore / seek / scan / compose / reflect / teach)
     → record_action_outcome → updates action traces
     → seek/scan/eat actions feed back into digest_file → process_token
```

Interaction rules that keep the system coherent:
1. **Live → Organic → Dream/Observe/Evolve** always runs in that exact order after each token.
2. **Observe** updates presence and drives, which selects dispatch mode, which changes future hit/miss rates.
3. **Dream/Evolve** mutate dispatch regions, directly reshaping prediction behavior.
4. **Autonomy actions** are learned from outcomes, which biases future action selection.

## GUI-Only Learning Checks (Examples)

**A/B Pass (same file twice, watch growth):**
1. Type `corpus/causal-structures.txt` in the GUI input bar.
2. Click **FILE → DREAM → INTRO**.
3. Repeat the same three clicks.
4. Look for non-zero resonance and shifting intro metrics on the second pass.

**Mini-Batch (Quick feed + consolidation):**
1. Click **FEED▼ → Quick**.
2. Click **DREAM → OBSERVE → INTRO** after the feed starts.
3. FEED should stream the live run-log; QUERY/DEBUG should keep updating.

## REPL Commands
These are the same commands the GUI sends over the gateway. The GUI is the primary control surface; REPL is optional for headless use.

### Status
| Command | Description |
|---------|-------------|
| `help` | List commands |
| `status` | System stats (energy, accuracy, region count) |
| `regions` | List dispatch regions with hit/miss counts |
| `presence` | 30-dim phenomenal state (arousal, valence, continuity...) |
| `drives` | Drive system levels and thresholds |
| `intro` | Introspective state (SELF-AWARE %, confused/confident/learning) |
| `self` | Strengths and weaknesses by context type |
| `autonomy` | Resonance scores for 10 actions, frontier, curiosity |
| `compose` | Show composition buffer (generated text) |

### Actions
| Command | Description |
|---------|-------------|
| `dream` | Run consolidation (replay misses, schemas, prune/promote) |
| `observe` | Trigger self-observation cycle (builds self-model) |
| `eat <file>` | Digest a text file (learn from it) |
| `batch` | Toggle batch mode (disable autonomous workers) |
| `compact` | Garbage collect condemned regions |
| `trace` | Toggle execution tracing |

### Diagnostics
| Command | Description |
|---------|-------------|
| `why` | Explain last MISS - context, actual vs predicted, confidence |
| `misses [n]` | Show last N misses (default 5) |
| `receipts [n]` | Show last N event receipts |
| `metacog` | Metacognitive state for last prediction |
| `causal` | What modifications work in current context |

### Persistence & Collective
| Command | Description |
|---------|-------------|
| `save <name>` | Save surface checkpoint |
| `load <name>` | Load surface checkpoint |
| `share` | Connect to shared hive mind |
| `colony` | Show connected instances |
| `quit` | Clean exit (syncs surface to disk) |

## Text Input

Any non-command input is processed as tokens:
```
> hello world hello
[TOKEN] hello (ctx=0) → NEW pattern
[TOKEN] world (ctx=hello) → NEW pattern
[TOKEN] hello (ctx=world) → NEW pattern
> hello world hello
[TOKEN] hello (ctx=0) → HIT (predicted!)
[TOKEN] world (ctx=hello) → HIT
[TOKEN] hello (ctx=world) → HIT
```

## Token Abstraction

Numbers and hex literals are abstracted to reduce vocabulary:
- `123`, `456`, `99` → all become TOKEN_NUM (0x4e554d21)
- `0xDEAD`, `0x1234` → all become TOKEN_HEX (0x48455821)

This lets the system learn "a number follows X" rather than memorizing every specific number.

## Persistence

Learning survives restarts via `uhma.surface` (sparse 200GB file, only touched pages use disk):

```bash
# Session 1
./uhma
> the quick brown fox
> quit

# Session 2
./uhma
Recovering 4 regions...
> the quick brown
[HIT] predicted: fox
```

## Memory Zones

| Zone | Range | Purpose |
|------|-------|---------|
| HOT | 0-2GB | State, dispatch regions, scratch (always RAM) |
| WARM | 2-16GB | Embeddings, traces, vocabulary |
| COLD | 16-200GB | Archives, episodic memory (paged on demand) |

## Unified Trace System

All events (HIT, MISS, LEARN, EMIT, etc.) are recorded in a single holographic trace with 8 dimensions:

| Dimension | What it captures |
|-----------|------------------|
| event | HIT, MISS, LEARN, EMIT, NEW |
| ctx | Context hash (previous token) |
| actual | Token that actually occurred |
| predicted | Token the system predicted |
| region | Which dispatch region was used |
| aux | Runner-up prediction, extra data |
| tracer | Debug correlation ID |
| time | Temporal bucket |

Query any dimension via unbind. `why` and `misses` use this to show exactly what went wrong.

## File Layout

```
boot.asm        Entry point, surface init
repl.asm        Interactive loop, command dispatch
dispatch.asm    Prediction engine, hit/miss logic
learn.asm       Pattern learning via superposition
emit.asm        x86-64 code generation
verify.asm      Safety verification (abstract interpretation)
vsa.asm         Vector operations (bind, bundle, similarity)
receipt.asm     Unified trace system
signal.asm      Fault handling (SIGSEGV → learning)
dreams.asm      Consolidation, schema extraction
observe.asm     Self-observation
presence.asm    Arousal, valence, fatigue modulators
genes.asm       Gene pool (patterns from condemned regions)
surface.asm     Memory management, persistence
io.asm          File I/O, digest_file
introspect.asm  Autonomy loop, self-repair, action dispatch
maturity.asm    Developmental stage gating
gateway.asm     Single-port TCP gateway (9999)

include/
  constants.inc   All constants, surface layout
  syscalls.inc    Linux syscall numbers

tools/
  mcp_server.asm  MCP protocol handler for Claude Code
  feeder.asm      Automated training client

pet/x86/
  creature.asm    Creature state & behavior simulation
  render.asm      Procedural creature rendering
```

## Testing Tips

```bash
# GUI sanity check
# - FEED panel is live run-log (streamed, not polled)
# - QUERY panel updates via status
# - DEBUG panel shows receipts/trace
#
# Optional headless checks (advanced):
# echo "a b a b a" | ./uhma 2>&1 | rg -e "HIT|NEW"
```

## Debugging

When something goes wrong:

1. **`why`** - Shows the last miss with full context
2. **`misses 10`** - Shows pattern of recent failures
3. **`trace on`** - Verbose execution trace
4. **`regions`** - Check if regions exist for context

Common issues:
- First token always NEW (no context yet)
- Different contexts = different patterns ("hello" after "the" ≠ "hello" after "world")
- Abstraction bugs: check if numbers/hex are being collapsed properly

## Claude Code Integration (MCP)

UHMA exposes tools via MCP (Model Context Protocol) for control from Claude Code. The MCP server is pure x86-64 assembly (`tools/mcp_server`).

### Setup

`.mcp.json` in project root (already configured):
```json
{
  "mcpServers": {
    "uhma": {
      "command": "/path/to/uhma-asm/tools/mcp_server",
      "cwd": "/path/to/uhma-asm"
    }
  }
}
```

**Important**: UHMA must be running (gateway port 9999, framed protocol) for UHMA commands. Holographic memory (`mem_*`) works standalone. Use GUI/feeder/MCP server (raw `nc` won't work). Restart Claude Code after changes. Verify with `/mcp`.

### Available MCP Tools

| Category | Tools |
|----------|-------|
| Input | `input`, `raw` |
| Status | `status`, `self`, `intro`, `presence`, `drives`, `metacog`, `genes`, `regions`, `hive`, `colony` |
| Debug | `why`, `misses` |
| Actions | `dream`, `observe`, `compact`, `reset` |
| Memory | `mem_add`, `mem_query`, `mem_state`, `mem_recent`, `mem_summary`, `mem_rag_refresh`, `mem_rag_update`, `mem_rag_rebuild` |

Use `raw` for any other REPL command (`eat`, `save`, `load`, `trace`, `receipts`, `step`, `run`, etc.).
Note: Gateway responses are capped to `GW_MAX_PAYLOAD` (4096). Large outputs are truncated in MCP responses; the GUI stream panels carry the full run-log.

### Holographic Memory (Claude's)

Separate 6GB surface for cross-session persistence and 3-layer code RAG. 14 categories (finding, warning, insight, code_high, code_mid, code_low, etc.). Queried by semantic similarity via MPNet embeddings (768→8192 dim projection) implemented in assembly. `mem_rag_refresh` rebuilds traces from existing entries (fast). `mem_rag_update` and `mem_rag_rebuild` scan `.` `include/` `tools/` `gui/` `embed/` and `pet/x86/`.

## GUI (Command & Control Center)

The GUI provides full visual control and monitoring:

```bash
cd gui && make
./uhma-viz
```

### Features

- **Mind Map View**: Central UHMA node with connected subsystems (BRAIN, REGIONS, TOKENS, STATE, etc.)
- **Carousel Nodes**: Click to expand, click outside to collapse (auto-copies to clipboard)
- **Side Panels**: FEED, QUERY, DEBUG panels are logical subnets on the single gateway (port 9999) - click to pause/copy
- **Live Stream Routing**: Output is mirrored via the gateway stream and routed by the originating command's subnet
- **Live FEED**: FEED panel shows processing/run-log output (feed + autonomous work)
- **Auto-Polling**: QUERY and DEBUG panels refresh every ~3 seconds (FEED is stream-only)
- **Dual Spawn Modes**:
  - **DREAM button**: Spawns UHMA in live/autonomous mode (batch OFF)
  - **FEED menu**: Spawns UHMA in feed mode (batch ON)
- **URL button**: Fetches web content (curl/wget) and feeds it to UHMA

### Clipboard Copy

- Click any side panel → toggles pause + copies to clipboard
- Collapse any expanded carousel node → copies content to clipboard
- Uses `xclip` (install with `apt install xclip`)

### Layout

```
┌─────────────────────────────────────────────────────────────────┐
│ [DREAM] [OBSERVE] [EVOLVE] [STEP] [RUN] [SAVE] [LOAD] ...      │
├───────────────────────────────────────────┬─────────────────────┤
│                                           │ FEED                │
│     Mind Map / Carousel View              │ [live run-log]      │
│                                           ├─────────────────────┤
│     Click nodes to expand/inspect         │ QUERY               │
│                                           │ [status/why/misses] │
│                                           ├─────────────────────┤
│                                           │ DEBUG               │
│                                           │ [receipts/trace]    │
├───────────────────────────────────────────┴─────────────────────┤
│ INPUT: _______________                        [SEND] [CLEAR]    │
└─────────────────────────────────────────────────────────────────┘
```

## Building from Source

```bash
make clean && make    # Full rebuild
make                  # Incremental
./uhma                # Run
cd gui && make        # Build GUI
./gui/uhma-viz        # Run GUI
```

Debug build:
```bash
make clean && make DEBUG=1
gdb ./uhma
```
