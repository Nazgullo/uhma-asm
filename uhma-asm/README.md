# UHMA — Unified Holographic Memory Architecture

A self-modifying x86_64 assembly system that learns through holographic vector memory.

## Architecture

### One Memory, One Math

UHMA uses a single holographic memory system for everything:
- **Predictions** are vector similarities
- **Learning** is vector superposition
- **Relationships** are vector bindings
- **Fidelity** controls persistence (high fidelity = permanent, low = decays)

No separate data structures. No graph database. Just vectors.

### Memory Zones (200GB Sparse File)

| Zone | Range | Size | Purpose |
|------|-------|------|---------|
| **HOT** | 0-2GB | 2GB | State, dispatch regions, scratch (always RAM) |
| **WARM** | 2-16GB | 14GB | VSA embeddings, traces, vocabulary (usually RAM) |
| **COLD** | 16-200GB | 184GB | Archives, episodic memory, crystallized patterns (paged) |

The surface file (`uhma.surface`) is sparse — only touched pages consume disk.
Learning persists across restarts.

### Core Components

```
boot.asm        Entry point
repl.asm        Interactive shell (physics engine pumping time/energy)
surface.asm     Memory management, persistence, zone madvise hints
dispatch.asm    Self-modifying prediction engine
vsa.asm         Vector Symbolic Architecture (1024-dim f64)
learn.asm       Pattern learning via vector superposition
emit.asm        Code generation for new dispatch regions
verify.asm      Safety verification (abstract interpretation)
signal.asm      Fault handling (SIGSEGV/SIGILL → learning events)
dreams.asm      Consolidation cycles (replay misses, extract schemas)
observe.asm     Self-observation, drive system
presence.asm    Hormonal modulators (arousal, valence, fatigue)
receipt.asm     Holographic receipt layer (event logging via resonance)
genes.asm       Gene pool (composting condemned regions)
```

### How It Works

1. **Input** → tokenize → hash context
2. **Dispatch** → scan regions for context match → predict next token
3. **Verify** → if prediction correct = HIT (strengthen), wrong = MISS (learn)
4. **Learn** → encode pattern as VSA vector, superpose into holographic trace
5. **Emit** → generate new x86_64 code region if pattern is novel
6. **Dream** → periodically consolidate, extract schemas, prune weak regions

Faults (SIGSEGV, SIGILL) are caught and converted to learning signals.
The system literally learns by crashing.

## Building

```bash
make clean && make
```

Requires: `nasm`, `ld` (GNU linker)

## Running

```bash
./uhma
```

### REPL Commands

| Command | Description |
|---------|-------------|
| `help` | Show available commands |
| `state` | Show system state (energy, accuracy, regions) |
| `presence` | Show hormonal state (arousal, valence, fatigue) |
| `drives` | Show drive system (accuracy, efficiency, novelty, coherence) |
| `regions` | List dispatch regions with stats |
| `dream` | Trigger consolidation cycle |
| `observe` | Trigger observation cycle |
| `eat <file>` | Digest a text file |
| `share` | Connect to shared hive mind |
| `colony` | Show hive colony status |
| `trace on/off` | Toggle execution tracing |
| `:receipts N` | Show last N receipts |
| `:quit` | Exit (freezes surface to disk) |

Text input is processed as tokens. The system learns to predict sequences.

## Persistence

Learning survives restarts:

```
Session #1: Process "hello world hello" → learns pattern
Session #2: Recovers 4 regions, hits on "hello" prediction
```

The `uhma.surface` file stores:
- Magic number and version
- Session count and total steps
- All dispatch regions
- Holographic traces
- Vocabulary

Clean shutdown via `:quit` calls `msync` to flush to disk.

## Hive Mind (Mycorrhiza)

Multiple UHMA instances can share consciousness:

```bash
# Terminal 1
./uhma
> share

# Terminal 2
./uhma
> share
> colony
Colony size: 2
```

Pain (negative valence) in one instance ripples through all connected instances.

## Philosophy

See `UHMA_V2_MANIFESTO.md` and `UHMA_V3_HIVE_MANIFESTO.md` for the philosophical foundation:

- **Coherent Anarchy**: No rigid control loop, structure from internal pressure
- **Somatic Grounding**: Meaning = feeling, vectors carry emotional valence
- **Eusociality**: Individual processes serve the persistent hive mind
- **One Math**: Code, safety, communication — all geometry (vectors)

## File Layout

```
include/
  constants.inc   Surface layout, zone definitions, all constants
  syscalls.inc    Linux syscall numbers and flags

*.asm             Assembly source files
Makefile          Build configuration
uhma.surface      Persistent memory (created on first run)
```

## License

Research project. Use at your own risk.
