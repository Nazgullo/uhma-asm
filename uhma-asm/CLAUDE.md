# UHMA Project Guidelines

## What This Is
UHMA (Unified Holographic Memory Architecture) - a self-modifying x86-64 assembly system that learns patterns through holographic/VSA encoding. It's meant to abstract, generalize, and recognize patterns like a cognitive system.

## Problem-Solving Methodology

**USE THE SYSTEM'S OWN PARADIGM FIRST.**

When you see a problem in UHMA, the solution should come from holographic/VSA thinking, not generic CS patterns:

| Problem | Wrong instinct | Right instinct |
|---------|----------------|----------------|
| O(n²) comparison | Hash table, indexes | Holographic trace + resonance query |
| Find similar items | Loop and compare | Encode + dot product |
| Group by property | Buckets, maps | Superpose into trace, query by similarity |
| Remember history | Ring buffer, list | Holographic superposition |

**The pattern:**
1. Encode the thing as a VSA vector
2. Query existing trace by dot product (similarity)
3. Superpose into trace for future queries

This is O(1) per item. The holographic memory IS the index. Don't build separate data structures when the surface already provides similarity search.

**Ask yourself:** "How would this work if the system only had holographic memory?" - then implement that.

## Hard-Learned Lessons

### Testing
- **ALWAYS stream logs** - use `tee`, never run blind waiting for completion
- **Short test FIRST** - verify with tiny_test.txt (8 tokens), THEN scale up
- **Check ALL code paths** - digest_file and process_input had different behavior (abstraction was missing in digest)

### Architecture
- **Mood/somatic binding = metadata, NOT pattern identity** - a bike is a bike whether happy or angry. Mood affects behavior, not what patterns ARE
- **Fault handler longjmps to REPL** - save/restore fault_safe_rsp/rip around calls that might fault (digest loop)
- **Context = hash(prev_token) only** - no somatic XOR, that was breaking pattern recognition
- **rcx is caller-saved** - don't use ecx/rcx in digest loop, it gets clobbered; use r10-r15 instead
- **Restore r14 after fault recovery** - process_token may clobber r14, must reload SURFACE_BASE after .continue_loop

### Emitted Patterns
- Emitted patterns work for text processing - they're valid x86 (cmp/jne/mov/ret stubs)
- The emitted code compares context hash and returns predicted token or 0

### Performance
- FACTOR now uses O(n) hash-based grouping instead of O(n²) byte comparison
- Large files still take time due to volume (11K+ tokens = 11K+ learning cycles)
- Each token goes through: hash → SEARCH → LEARN → VERIFY → EMIT
- For quick testing, use small files (< 100 tokens)

### Token Abstraction
- Hex literals (0x...) → TOKEN_HEX (0x48455821)
- Numbers (digits only) → TOKEN_NUM (0x4e554d21)
- Single-digit numbers ARE abstracted (removed length>1 check)
- This abstraction MUST happen in both process_input AND digest_file

### Schema System
- Schemas generalize CONTEXT (same token, similar contexts)
- They mask low bits of context hash to match broader patterns
- Dreams extract schemas from miss buffer

### Unified Trace System
- One trace (UNIFIED_TRACE_IDX=240) replaces 6 separate traces
- 8 dimensions: event, ctx, actual, predicted, region, aux, tracer, time
- emit_receipt_full() captures full diagnostic context
- `why` and `misses` commands query the trace for debugging
- See DESIGN_unified_trace.md for details

## File Index

### Core Loop
| File | Purpose | Calls | Called By |
|------|---------|-------|-----------|
| boot.asm | Entry point, surface init | surface_init, repl_run | OS |
| repl.asm | Command loop, text dispatch | process_input, dream_cycle, observe_cycle | boot |
| dispatch.asm | Token processing, prediction | learn_pattern, holo_predict, emit_receipt | repl, io |

### Learning & Memory
| File | Purpose | Calls | Called By |
|------|---------|-------|-----------|
| learn.asm | Pattern learning | emit_dispatch_pattern, holo_store | dispatch |
| emit.asm | x86 code generation | region_alloc, verify_code | learn, dreams |
| vsa.asm | Holographic operations | (pure math) | dispatch, learn, receipt |
| receipt.asm | Unified trace system | holo_bind, holo_superpose | dispatch, learn, observe |

### Consolidation
| File | Purpose | Calls | Called By |
|------|---------|-------|-----------|
| dreams.asm | Offline consolidation | emit_dispatch_pattern, holo_store | repl (dream cmd) |
| observe.asm | Self-observation, metrics | receipt_resonate, gene_extract | repl (observe cmd) |
| genes.asm | Gene pool for condemned regions | (storage) | observe, dreams |

### Safety & Verification
| File | Purpose | Calls | Called By |
|------|---------|-------|-----------|
| verify.asm | Abstract interpretation | decode_instruction | emit, modify |
| gate.asm | Modification gating | verify_* | emit, modify |
| signal.asm | Fault handling | (longjmp to repl) | kernel |

### I/O & Persistence
| File | Purpose | Calls | Called By |
|------|---------|-------|-----------|
| io.asm | File I/O, digest_file | process_token | repl (eat cmd) |
| surface.asm | Memory management | mmap, madvise | boot |
| persist.asm | Save/load state | (file I/O) | repl |

### Support
| File | Purpose |
|------|---------|
| format.asm | Print functions (print_str, print_hex, etc.) |
| decode.asm | x86 instruction decoder |
| presence.asm | Hormonal modulators (arousal, valence, etc.) |
| drives.asm | Drive system (accuracy, efficiency, novelty) |
| introspect.asm | Self-inspection, worker threads |
| hooks.asm | Event hooks system |
| trace.asm | Journey tracing (token path tracking) |
| factor.asm | Subroutine extraction |
| modify.asm | Region modification (prune, promote, specialize) |
| evolve.asm | Genetic evolution of patterns |
| symbolic.asm | Symbolic execution |
| maturity.asm | Syscall maturity gating |

### Data Flow
```
Input → repl.asm → dispatch.asm → [predict] → HIT/MISS
                                      ↓
                              learn.asm → emit.asm → new region
                                      ↓
                              receipt.asm → trace (8 dimensions)
                                      ↓
                              dreams.asm → consolidate/prune
```

## Header Template
Every .asm file should have this at top:
```asm
; filename.asm — One-line description
;
; @entry func_name(args) -> return   ; exported functions
; @calls file.asm:func              ; outgoing dependencies
; @calledby file.asm:func           ; incoming dependencies
;
; FLOW: brief data flow description
; STATE: key ST_* fields used
;
; GOTCHAS:
;   - specific pitfalls for this file
```

## Claude Code Integration

### Automatic Context Injection (Hooks)
Pre-tool hooks automatically inject relevant context before Read/Edit/Write/Grep/Glob operations on `.asm` files:
- **File operations** → file description, entry points, gotchas, dependencies
- **Glob for *.asm** → all gotchas across the codebase

This prevents repeating past mistakes by surfacing gotchas at edit time.

### MCP Server (UHMA Control Interface)
Full command/control/communication with UHMA via MCP tools:

| Tool | Description |
|------|-------------|
| `input` | Send text for processing/prediction |
| `status`, `self`, `metacog` | System state inspection |
| `why`, `misses`, `receipts` | Debug via unified trace |
| `dream`, `observe`, `compact` | Trigger consolidation cycles |
| `eat`, `save`, `load` | File I/O |
| `web_fetch` | Fetch URL, optionally digest into UHMA |
| `raw` | Escape hatch for any REPL command |

Auto-spawns UHMA on MCP initialization. Bidirectional stdin/stdout pipe.

### RAG Index
`tools/rag/index.json` contains:
- 29 files with descriptions, entry points, gotchas
- 40 functions with signatures
- Full dependency graph
- Rebuilt via: `python3 tools/rag/build.py`

## Common Commands
```bash
# Quick test
rm -f uhma.surface && ./uhma < /tmp/tiny_cmd.txt 2>&1 | grep -E "HIT|NEW"

# Streaming test
./uhma < commands.txt 2>&1 | tee /tmp/test.log

# Check HIT ratio
grep -c "HIT" /tmp/test.log && grep -c "NEW" /tmp/test.log

# Debug a miss
./uhma
> some failing input
> why
> misses 5
```
