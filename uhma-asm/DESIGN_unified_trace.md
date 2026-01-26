# Unified Holographic Trace - Implementation

## Architecture

One trace, 8 dimensions, full diagnostic capture.

Replaced 6 separate traces (HIT, MISS, NEW, LEARN, EMIT, COMBINED) with a single unified trace at `UNIFIED_TRACE_IDX` (zone 240).

## 8 Dimensions

| Dimension | Seed | Source | Purpose |
|-----------|------|--------|---------|
| event | TRACE_EVENT_SEED | EVENT_HIT, EVENT_MISS, etc | What happened |
| ctx | (raw hash) | Previous token hash | Where in sequence |
| actual | TRACE_ACTUAL_SEED | Token that occurred | Ground truth |
| predicted | TRACE_PREDICTED_SEED | Expected token | What system thought |
| region | TRACE_REGION_SEED | Region pointer >> 4 | Which code region |
| aux | TRACE_AUX_SEED | Runner-up, extra data | Secondary info |
| tracer | TRACE_TRACER_SEED | Debug correlation ID | Journey tracking |
| time | TRACE_TIME_SEED | global_step / bucket | Temporal locality |

## Encoding

```asm
; Inner to outer binding:
; time → tracer → aux → region → predicted → actual → ctx → event
bind(event_vec, bind(ctx_vec, bind(actual_vec, bind(predicted_vec,
     bind(region_vec, bind(aux_vec, bind(tracer_vec, time_vec)))))))

; Scale by fidelity, superpose into trace
unified_trace += fidelity * receipt_vec
```

## API

### emit_receipt_full (new - full context)
```asm
; edi = event_type
; esi = ctx_hash
; edx = actual_token
; ecx = predicted_token    ; KEY: what system expected
; r8d = region_hash        ; which region made prediction
; r9d = aux_data           ; runner-up, extra info
; xmm0 = confidence (f32)
; xmm1 = valence (f64)
emit_receipt_full:
```

### emit_receipt_simple (backward compatible)
```asm
; edi = event_type
; esi = ctx_hash
; edx = token
; xmm0 = confidence
emit_receipt_simple:
    ; Calls emit_receipt_full with predicted=0, region=0, aux=0
```

### Query Functions

```asm
receipt_why_miss:     ; Explains last MISS (actual vs predicted, confidence)
receipt_show_misses:  ; Shows last N misses from working buffer
```

## Working Buffer

16-entry ring buffer for recent receipts (human-readable debugging):
- `receipt_buffer`: 16 × 64 bytes
- `receipt_buffer_idx`: current write position
- Stores: event, ctx, actual, predicted, region, aux, confidence, valence

## REPL Commands

| Command | Function | Output |
|---------|----------|--------|
| `:why` | `receipt_why_miss` | Last MISS: ctx, actual, predicted, confidence |
| `:misses [n]` | `receipt_show_misses` | Last N misses with full context |

## Memory

- Old: 6 traces × 8KB = 48KB
- New: 1 trace × 8KB = 8KB
- Savings: 40KB

Legacy constants (RCPT_TRACE_HIT, etc.) aliased to UNIFIED_TRACE_IDX for compatibility.

## Call Sites Updated

| File | Event | Now captures |
|------|-------|--------------|
| dispatch.asm | MISS | predicted_token, region_hash, runner_up |
| dispatch.asm | HIT | region_hash |
| learn.asm | LEARN | region_hash |
| emit.asm | EMIT | region_hash |

## Query by Unbind

HRR property: unbind = bind (self-inverse)

```asm
; "Show me all MISS events"
holo_gen_vec(EVENT_MISS + TRACE_EVENT_SEED, probe)
result = unbind(probe, unified_trace)  ; filters to MISS
similarity = dot(result, unified_trace)

; "Show me MISSes in context X"
event_probe = gen(EVENT_MISS + TRACE_EVENT_SEED)
ctx_probe = gen(ctx_hash)
combined = bind(event_probe, ctx_probe)
result = unbind(combined, unified_trace)
```

## Cognitive Access Functions (Implemented)

The system queries its own trace for self-improvement:

### trace_region_performance(region_hash) → f64
Query HIT/MISS ratio for a specific region.
```
performance = hit_resonance / (hit_resonance + miss_resonance + epsilon)
```
Use: Pruning identifies regions with performance < 0.3 as candidates for removal.

### trace_context_confidence(ctx_hash) → f64
Query historical HIT rate in given context.
```
confidence = hit_resonance / (hit_resonance + miss_resonance + epsilon)
```
Use: Adjust prediction confidence based on past accuracy in this context.

### trace_token_learnability(token) → f64
Query how often token appears in LEARN events.
Use: Identify problematic patterns that keep getting re-learned.

### trace_event_count(event_type) → f64
Get resonance magnitude for event type (proxy for count).
Use: Monitor system activity levels.

### trace_hit_miss_ratio() → f64
System-wide HIT vs MISS ratio.
Use: Self-model - "how well am I doing overall?"

## Existing Integration (via receipt_resonate)

The subsystems already use trace queries for decision-making:

| Subsystem | Queries | Purpose |
|-----------|---------|---------|
| dispatch.asm:1018-1035 | HIT, MISS by ctx | Modulate prediction confidence |
| learn.asm:120-131 | LEARN by ctx+token | Skip redundant learning |
| observe.asm:161-178 | PRUNE, MISS by ctx | Avoid regretted prunes |
| observe.asm:243-263 | PROMOTE, HIT by ctx | Boost promising regions |

## Cognitive Access Functions (additional helpers)

These functions provide pre-packaged ratio queries:

| Function | Returns | Use case |
|----------|---------|----------|
| trace_region_performance | HIT/(HIT+MISS) for region | Evaluate specific region |
| trace_context_confidence | HIT/(HIT+MISS) for context | Context reliability |
| trace_token_learnability | LEARN resonance for token | Problem token detection |
| trace_event_count | Event magnitude | Activity monitoring |
| trace_hit_miss_ratio | System-wide ratio | Overall performance |

Available for REPL queries and future cognitive enhancements.
