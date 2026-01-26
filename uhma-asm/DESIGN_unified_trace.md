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

## Future: Cognitive Access

The trace can feed back into learning:

1. **Region performance**: Query trace for MISS rate per region → prune bad regions
2. **Learning effectiveness**: Compare pre/post MISS rates → tune learning rate
3. **Pattern analysis**: Find contexts with high MISS → candidates for schema extraction
4. **Self-model**: System queries its own history to understand behavior
