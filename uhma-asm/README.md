# UHMA — Unified Holographic Memory Architecture

Self-modifying x86-64 assembly that learns patterns through holographic vector encoding.

## Quick Start

```bash
make clean && make
./uhma
```

Requires: `nasm`, `ld` (GNU linker), Linux x86-64

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

## REPL Commands

### Status
| Command | Description |
|---------|-------------|
| `help` | List commands |
| `state` | System stats (energy, accuracy, region count) |
| `regions` | List dispatch regions with hit/miss counts |
| `presence` | Hormonal state (arousal, valence, fatigue) |
| `drives` | Drive system (accuracy, efficiency, novelty) |

### Actions
| Command | Description |
|---------|-------------|
| `dream` | Run consolidation (replay misses, extract schemas, prune weak regions) |
| `observe` | Trigger self-observation cycle |
| `eat <file>` | Digest a text file (learn from it) |
| `trace on/off` | Toggle execution tracing |

### Diagnostics
| Command | Description |
|---------|-------------|
| `why` | Explain last MISS - shows context, actual vs predicted, confidence |
| `misses [n]` | Show last N misses (default 5) with full context |
| `receipts [n]` | Show last N event receipts |
| `listen` | Enable verbose receipt logging |

### Hive
| Command | Description |
|---------|-------------|
| `share` | Connect to shared hive mind (other UHMA instances) |
| `colony` | Show connected instances |

### Exit
| Command | Description |
|---------|-------------|
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
hive.asm        Multi-instance shared memory

include/
  constants.inc   All constants, surface layout
  syscalls.inc    Linux syscall numbers
```

## Testing Tips

```bash
# Quick test (tiny input)
echo "a b a b a" | ./uhma 2>&1 | grep -E "HIT|NEW"

# Stream output (never run blind on large files)
./uhma < commands.txt 2>&1 | tee /tmp/test.log

# Check hit ratio
grep -c "HIT" /tmp/test.log && grep -c "NEW" /tmp/test.log

# Debug a miss
./uhma
> some input that fails
> why
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

UHMA exposes 27 tools via MCP (Model Context Protocol) for full control from Claude Code.

### Setup

Create `.mcp.json` in project root:
```json
{
  "mcpServers": {
    "uhma": {
      "command": "python3",
      "env": {"PYTHONUNBUFFERED": "1"},
      "args": ["/path/to/uhma-asm/tools/rag/server.py"],
      "cwd": "/path/to/uhma-asm"
    }
  }
}
```

**Important**: Restart Claude Code after creating/modifying `.mcp.json`. Verify with `/mcp` command.

### Available MCP Tools

| Category | Tools |
|----------|-------|
| Input | `input`, `raw` |
| Status | `help`, `status`, `self`, `metacog`, `debugger`, `genes`, `subroutines`, `regions`, `presence`, `drives` |
| Debug | `why`, `misses`, `receipts`, `listen`, `trace` |
| Actions | `dream`, `observe`, `compact`, `reset` |
| I/O | `save`, `load`, `eat` |
| Hive | `hive`, `share`, `colony`, `export`, `import_gene` |
| Other | `geom`, `web_fetch`, `quit` |

UHMA auto-spawns when any tool is called.

## Building from Source

```bash
make clean && make    # Full rebuild
make                  # Incremental
./uhma                # Run
```

Debug build:
```bash
make clean && make DEBUG=1
gdb ./uhma
```
