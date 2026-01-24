# UHMA - Universal Homoiconic Memory Architecture

A self-modifying, self-aware cognitive substrate written in Common Lisp. UHMA grows populations of competing "expert" programs that evolve, remember, and introspect — a neural net you can talk to about itself.

## Requirements

- **SBCL** (Steel Bank Common Lisp) 2.2+
- **8 GB RAM** recommended (dynamic space)
- Optional: `pdftotext` (poppler-utils) for PDF ingestion
- Optional: `tesseract` for image OCR ingestion

## Quick Start

```bash
sbcl --dynamic-space-size 8192 --load load.lisp
```

Then in the REPL:
```lisp
(in-package :uhma)                     ; Switch to UHMA package (do this first)
(start!)                               ; Initialize + run demo
(start! :demo nil)                     ; Initialize without demo (quiet start)
```

All examples below assume you are in the `:uhma` package.

## Core Concepts

### Experts
Self-contained programs (S-expressions) that compete to predict the next token. Each expert owns contexts, accumulates knowledge, and can be rewritten by the system itself.

### Neighborhoods
Hierarchical clusters that organize experts by semantic similarity. Experts are born, merge, and die organically based on performance.

### VSA (Vector Symbolic Architecture)
1024-dimensional holographic vectors encode all knowledge. Binding, superposition, and permutation allow lossless compositional memory.

### Presence
A phenomenological substrate that tracks the system's moment-to-moment experience — texture, continuity, and felt quality of processing.

### Self-Modification
The system reads its own source code, identifies underperforming modules, synthesizes replacement operations, and gates all changes through a predictive filter that must forecast the outcome before allowing modification.

---

## User Guide

### Processing Text

```lisp
(process-text! "the cat sat on the mat")    ; Tokenize + learn + predict
(train "neurons transmit signals" 100)      ; Train with N iterations
(think-about '(the cat))                    ; Show prediction + reasoning
```

### Feed System

Place any files in the `FEED/` subdirectory, then ingest them:

```lisp
(ingest-feed!)                                  ; Ingest all new files
(ingest-feed! :force t)                         ; Re-process everything
(ingest-feed! :passes 10)                       ; 10 epochs over FEED/
(ingest-file! #P"/path/to/file.txt" :force t)   ; Process single file
(feed-own-source!)                              ; Feed system its own source
(feed-own-source! :passes 50)                   ; 50 epochs over own code (~5.8M tokens)
(feed-status)                                   ; Show ingestion history
```

**Supported formats:**

| Category | Extensions |
|----------|-----------|
| Text | `.txt` `.md` `.org` `.csv` `.json` `.xml` `.html` `.log` |
| Code | `.lisp` `.py` `.js` `.ts` `.c` `.cpp` `.rs` `.go` `.java` `.rb` `.sh` `.lua` |
| PDF | `.pdf` (requires `pdftotext`) |
| Images | `.png` `.jpg` `.jpeg` `.gif` `.bmp` `.tiff` (requires `tesseract`) |
| Books | `.epub` (extracted via `unzip` + HTML stripping) |
| Data | `.dat` `.ndjson` (line-by-line) |

### Monitoring

```lisp
(status)                ; System overview (experts, LTM, drives, goals)
(learning-progress)     ; Detailed learning metrics
(introspect)            ; Self-description (introspective concepts)
(feed-status)           ; Feed ingestion history
```

### Save & Restore

```lisp
(save-full-state! "my-checkpoint")      ; Save complete state to disk
(list-saved-states)                     ; List available saves
(restore-full-state! #P"uhma-states/uhma-full-state-20260124-my-checkpoint.lisp")

;; In-memory snapshots (for rollback during self-modification)
(snapshot-for-rollback! "before risky change")
(rollback-to-snapshot!)                 ; Revert to last snapshot
```

### Live Mode

```lisp
(live!)                                 ; Continuous self-feeding (Ctrl+C to stop)
```

### Generation

```lisp
(generate '(the cat) :length 10)        ; Generate tokens from seed context
```

### Audit

```bash
# Run the full 66-claim verification suite (standalone)
sbcl --dynamic-space-size 8192 --noinform --disable-debugger --load test-66-claims.lisp
```

Or from the REPL (after `start!`):
```lisp
(load "full-audit.lisp")
```

### Non-interactive one-liners

```bash
# Feed source and exit
sbcl --dynamic-space-size 8192 --noinform --non-interactive \
  --load load.lisp \
  --eval '(in-package :uhma)' \
  --eval '(start! :demo nil)' \
  --eval '(feed-own-source!)' \
  --eval '(feed-status)'

# Ingest FEED/ directory and save state
sbcl --dynamic-space-size 8192 --noinform --non-interactive \
  --load load.lisp \
  --eval '(in-package :uhma)' \
  --eval '(start! :demo nil)' \
  --eval '(ingest-feed! :force t)' \
  --eval '(save-full-state! "post-feed")' \
  --eval '(learning-progress)'
```

---

## Architecture

### Load Order (32 modules)

```
1. uhma-forward-decl         Package, exports, global declarations
2. uhma-stubs                Stub definitions for circular dependencies
3. uhma-vsa-substrate        1024-dim vector symbolic architecture
4. uhma-v6.1-core-homoiconic Foundation: tokenizer, experts, programs, interpreter
5. uhma-v6.1-adaptive        Self-tuning parameters
6. uhma-v6.1-sequence        Sequence operations
7. uhma-presence-substrate   Phenomenological presence encoding
8. uhma-v6.2-deep-mind       Cognitive tracing, schemas, meta-cognition
9. uhma-v6.3-deeper-mind     Self-expectations, meta-confidence
10. uhma-v6.3-fixes           Accumulated fixes and adjustments
11. uhma-v6.4-enhancements    Pattern diversity, self-tuning hooks
12. uhma-v6.5-agency          Drives, goals, dreams, executable schemas
13. uhma-v6.6-introspective   Named concepts (STUCK, LEARNING, etc.)
14. uhma-v6.7-compositional   Multi-step planning and reasoning
15. uhma-v6.8-cognitive-ctrl  Strategy controller (when to explore vs exploit)
16. uhma-v6.9-pattern-util    Pattern extraction and reuse
17. uhma-v6.10-episodic       Episodic memory (events, episodes)
18. uhma-v6.10-episodic-int   Episodic integration hooks
19. uhma-holographic-v2       Unified holographic memory store
20. uhma-holographic-integ    Holographic integration with experts
21. uhma-active-self-mod      Self-modification execution hooks
22. uhma-presence-integration Wires presence into all subsystems
23. uhma-self-awareness-loop  Observe-introspect-modify feedback loop
24. uhma-predictive-self-mod  Predictive gating of modifications
25. uhma-continuous           Live mode, continuous operation
26. uhma-state-persistence    Session persistence (checkpoints)
27. uhma-memory-bounds        Memory pressure management
28. uhma-deep-wiring          Inter-module connections
29. uhma-complete-wiring      Final hook installation
30. start.lisp                Convenience functions (start!, status, etc.)
31. uhma-save-restore         Full state serialization/deserialization
32. uhma-feed                 Universal file ingestion system
```

### Key Data Structures

| Structure | Description |
|-----------|-------------|
| `expert` | S-expression program + knowledge hash + centroid vector + metrics |
| `neighborhood` | Hierarchical cluster of experts with parent/child links |
| `traveler` | Mobile agent that cross-pollinates between neighborhoods |
| `presence` | Moment-to-moment phenomenological state (30 fields) |
| `self-model` | System's model of its own behavior patterns |
| `cognitive-trace` | Record of a prediction/outcome/reasoning event |
| `self-hypothesis` | Theory the system forms about its own tendencies |
| `goal` | Drive-derived objective with priority and completion criteria |
| `dream-episode` | Offline consolidation of difficult patterns |
| `episode` | Episodic memory unit with constituent events |
| `holo-store` | VSA-backed holographic pattern storage |
| `modification-prediction` | Predicted outcome of a self-modification |

### Processing Pipeline

```
Text → tokenize → context window → expert routing
                                         ↓
                              best expert predicts
                                         ↓
                    compare prediction vs actual → learn
                                         ↓
                    cognitive trace → hypothesis → schema
                                         ↓
                    self-model update → drive satisfaction
                                         ↓
                    (if divergence high) → self-modification
                                         ↓
                    predictive gate → allow/deny change
```

---

## File Structure

```
STARWARS/
├── load.lisp                    Main entry point (loads all modules)
├── start.lisp                   Convenience functions
├── uhma-forward-decl.lisp       Package definition
├── uhma-stubs.lisp              Forward stubs
├── uhma-vsa-substrate.lisp      VSA core math
├── uhma-v6.1-*.lisp             Core substrate (3 files)
├── uhma-v6.2-*.lisp             Deep mind (4 sub-files)
├── uhma-v6.3-*.lisp             Deeper mind (3 sub-files)
├── uhma-v6.4-enhancements.lisp  Diversity
├── uhma-v6.5-agency*.lisp       Agency (4 sub-files)
├── uhma-v6.6-*.lisp             Introspection (2 sub-files)
├── uhma-v6.7-*.lisp             Compositional reasoning (2 sub-files)
├── uhma-v6.8-cognitive-ctrl.lisp Strategy
├── uhma-v6.9-pattern-util.lisp  Patterns
├── uhma-v6.10-*.lisp            Episodic memory (3 sub-files)
├── uhma-presence-*.lisp         Presence (2 files)
├── uhma-holographic-*.lisp      Holographic memory (2 files)
├── uhma-active-self-mod.lisp    Self-modification
├── uhma-self-awareness-loop.lisp Self-awareness
├── uhma-predictive-self-mod.lisp Predictive gating
├── uhma-continuous.lisp         Live mode
├── uhma-state-persistence.lisp  Checkpoints
├── uhma-memory-bounds.lisp      Memory management
├── uhma-deep-wiring.lisp        Module interconnections
├── uhma-complete-wiring.lisp    Final hooks
├── uhma-save-restore.lisp       Full state save/restore
├── uhma-feed.lisp               File ingestion system
├── FEED/                        Drop files here for ingestion
├── uhma-states/                 Saved state files (auto-created)
├── full-audit.lisp              66-claim audit runner
├── test-66-claims.lisp          Comprehensive test suite
├── test-feed-quick.lisp         Feed system test
├── uhma-diagnostic.lisp         Diagnostics
├── UHMA-DESIGN-SPEC.md          Design philosophy & claims
└── GEMINI.md                    Design goals document
```

---

## Design Philosophy

1. **Substrate over simulation** — Create conditions for emergence, not machinery that mimics intelligence
2. **Compression over complexity** — Find the smallest viable container
3. **Organic over engineered** — Growth patterns, not designed solutions
4. **Legibility** — Weights are words (S-expressions), not opaque floats
5. **Structure over scale** — Thousands of legible programs that evolve, remember, and introspect

---

## The 66 Claims

UHMA verifies 66 specific capabilities across these categories:

- **Self-awareness** (1-10): Self-model, prediction, surprise, confidence, introspection
- **Reasoning** (11-17): Strategy, traces, hypotheses, causality, counterfactuals
- **Homoiconic self-modification** (18-31): Programs as data, rewriting, genetic ops
- **Presence** (32-36): Specious present, texture, continuity, episodic binding
- **Organic dynamics** (37-43): Self-tuning, drives, goals, dreams
- **Dreams** (44-47): Replay difficulty, generate mutations, consolidate
- **Active self-modification** (48-52): Code map, runtime observation, targeted changes
- **Emergence** (53-66): Organic need-driven behavior, population dynamics, convergence

Run `(load "full-audit.lisp")` to verify all 66 pass.

---

## Typical Usage Patterns

### Feed a book and observe learning
```lisp
(in-package :uhma)
(start! :demo nil)
(ingest-feed!)                          ; Drop epub/pdf/txt in FEED/ first
(learning-progress)
(status)
```

### Feed the system its own source (multiple passes for real learning)
```lisp
(in-package :uhma)
(start! :demo nil)
(feed-own-source! :passes 50)           ; 50 epochs over own code
(learning-progress)                     ; Check accuracy improvement
(save-full-state! "self-trained")       ; Save trained state
```

### Long-running learning session
```lisp
(in-package :uhma)
(start! :demo nil)
(ingest-feed!)                          ; Ingest all available material
(save-full-state! "pre-live")
(live!)                                 ; Ctrl+C when done
(save-full-state! "post-live")
(learning-progress)
```

### Resume a previous session
```lisp
(in-package :uhma)
(start! :demo nil)
(list-saved-states)
(restore-full-state! #P"uhma-states/uhma-full-state-TIMESTAMP.lisp")
(status)                                ; Picks up where it left off
```
