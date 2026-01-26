# Unified Holographic Trace Design

## Current State (To Be Replaced)

6 separate traces storing receipts redundantly:
- RCPT_TRACE_HIT, RCPT_TRACE_MISS, RCPT_TRACE_NEW, RCPT_TRACE_LEARN, RCPT_TRACE_EMIT
- RCPT_TRACE_COMBINED (duplicate of all above)

This is bucket-thinking, not holographic thinking.

## New Design: One Trace, N Dimensions

### Core Principle

One holographic trace. Every receipt is a bound composition of ALL relevant dimensions:

```
receipt_vec = bind(event, bind(ctx, bind(token, bind(tracer, bind(timestamp_bucket, ...)))))
unified_trace += fidelity * receipt_vec
```

Query any dimension via unbind. Correlation is intrinsic.

### Dimensions

| Dimension | Source | Purpose |
|-----------|--------|---------|
| event_type | EVENT_HIT, EVENT_MISS, etc. | What happened |
| ctx_hash | Hash of previous token | Where in sequence |
| token_id | Current token hash | What was processed |
| tracer_id | 0 if none, else unique ID | Journey correlation |
| time_bucket | global_step / BUCKET_SIZE | Temporal locality |
| outcome | HIT=+1, MISS=-1, NEW=0 | Success/failure signal |

### Encoding

```asm
; Generate base vectors from hashes (deterministic, reproducible)
holo_gen_vec(event_type + EVENT_SEED, event_vec)
holo_gen_vec(ctx_hash, ctx_vec)
holo_gen_vec(token_id, token_vec)
holo_gen_vec(tracer_id + TRACER_SEED, tracer_vec)  ; 0 → neutral vec
holo_gen_vec(time_bucket + TIME_SEED, time_vec)

; Bind all dimensions (order matters for unbind)
; Inner to outer: token → ctx → event → tracer → time
temp = bind(token_vec, ctx_vec)
temp = bind(temp, event_vec)
temp = bind(temp, tracer_vec)
receipt_vec = bind(temp, time_vec)

; Superpose with fidelity as learning rate
unified_trace += fidelity * receipt_vec
```

### Query Interface

**Query by single dimension:**
```asm
; "Show me all HIT events"
holo_gen_vec(EVENT_HIT + EVENT_SEED, probe)
similarity = dot(unbind(probe, unified_trace), unified_trace)
```

**Query by multiple dimensions (intersection):**
```asm
; "Show me HITs in context X"
holo_gen_vec(EVENT_HIT + EVENT_SEED, event_probe)
holo_gen_vec(ctx_hash, ctx_probe)
combined_probe = bind(event_probe, ctx_probe)
; Unbind and dot
result = unbind(combined_probe, unified_trace)
similarity = dot(result, unified_trace)
```

**Query tracer journey:**
```asm
; "What happened to tracer #42?"
holo_gen_vec(42 + TRACER_SEED, tracer_probe)
journey = unbind(tracer_probe, unified_trace)
; journey now resonates with all events that had tracer 42 bound
; dot(journey, event_vec) tells you which events occurred
; dot(journey, ctx_vec) tells you which contexts were visited
```

### Tracer Activation

Tracers are activated by condition, not special tokens:

| Trigger | Tracer ID Generation | Purpose |
|---------|---------------------|---------|
| MISS event | hash(ctx, token, step) | Autopsy bad predictions |
| confidence < 0.5 | hash(ctx, step) | Study uncertain moments |
| 1% random sample | step % 100 == 0 → step | Statistical monitoring |
| Manual `:trace N` | N | Explicit debugging |
| Novelty signal | hash(token, step) | Study new patterns |

When tracer_id = 0, the tracer dimension still gets bound (neutral vector), but queries for specific tracers won't resonate with it.

### API Changes

**Old (to remove):**
```asm
emit_receipt(event_type, ctx, token, confidence, valence)
receipt_resonate(event_type, ctx, token) → similarity
```

**New:**
```asm
; Record event with optional tracer
trace_event(event_type, ctx, token, fidelity, tracer_id)

; Query by any dimension combination
trace_query_event(event_type) → similarity
trace_query_ctx(ctx_hash) → similarity
trace_query_token(token_id) → similarity
trace_query_tracer(tracer_id) → journey_vec
trace_query_multi(event, ctx, token, tracer) → similarity  ; 0 = wildcard

; Extract journey details from tracer
trace_journey_events(tracer_id) → resonance with each event type
trace_journey_contexts(tracer_id) → resonance with contexts visited
```

### Memory Layout

**Old:** 6 traces × 8KB = 48KB
**New:** 1 trace × 8KB = 8KB (6x reduction)

The unified trace is denser but holographic superposition handles it - that's the whole point.

### Constants Changes

```asm
; Remove
; %define RCPT_TRACE_HIT      240
; %define RCPT_TRACE_MISS     241
; ... etc

; Add
%define UNIFIED_TRACE_IDX   240   ; single trace index
%define EVENT_SEED          0x45564E54  ; "EVNT"
%define TRACER_SEED         0x54524143  ; "TRAC"
%define TIME_SEED           0x54494D45  ; "TIME"
%define TIME_BUCKET_SIZE    1000        ; steps per bucket
```

### Backward Compatibility (Critical)

**The external API does NOT change.** All 20+ files calling these functions work unchanged:

```asm
; These signatures stay identical:
emit_receipt_simple(event_type, ctx, token, confidence)
emit_receipt(event_type, ctx, token, confidence, valence)
receipt_resonate(event_type, ctx, token) → similarity
```

**Only the internal implementation changes:**

| Old (bucket-based) | New (unified + unbind) |
|--------------------|------------------------|
| Write to RCPT_TRACE_HIT | bind(HIT_vec, ...) into unified_trace |
| Read from RCPT_TRACE_HIT | unbind(HIT_vec, unified_trace) |
| 6 physical traces | 1 trace, 6 virtual views |

The "buckets" become **virtual views** - the unbind operation filters the unified trace to show only events of that type. Mathematically equivalent, architecturally unified.

**New tracer API is additive:**
```asm
; New functions (optional, for tracer support):
trace_set_active(tracer_id)       ; set current tracer (0 = none)
trace_get_active() → tracer_id    ; get current tracer
trace_query_journey(tracer_id) → journey_vec
```

Existing code ignores tracers (tracer_id=0 default). New code can opt-in.

### Migration Path

1. Rewrite receipt.asm internals (emit/resonate) to use unified trace
2. Keep same function signatures - no callers change
3. Add new tracer functions as separate entry points
4. Remove old RCPT_TRACE_* constants from constants.inc
5. Reclaim the 40KB of freed trace memory (optional)

### Performance Notes

- **Encoding cost:** ~5 binds per receipt vs ~2 currently (+3 binds)
- **Query cost:** 1-2 unbinds + dot vs current 1 dot (similar)
- **Memory:** 6x reduction
- **Tracer overhead:** Zero when tracer_id=0 (neutral binding is still O(1))

The extra binds are worth it for:
- Unified architecture (one trace, not six)
- Natural tracer support (no special infrastructure)
- Richer queries (any dimension combination)
- Paradigm consistency (holographic all the way down)

### Usage Examples

**Miss autopsy:**
```
Token "foo" causes MISS
→ tracer_id = hash(ctx, "foo", step) = 0xABCD1234
→ trace_event(EVENT_MISS, ctx, foo, 0.8, 0xABCD1234)
→ trace_event(EVENT_LEARN, ctx, foo, 0.8, 0xABCD1234)
→ Later: trace_query_tracer(0xABCD1234) shows MISS then LEARN
```

**Confidence calibration:**
```
About to predict in ctx X
→ trace_query_multi(EVENT_HIT, X, 0, 0) → hit_similarity
→ trace_query_multi(EVENT_MISS, X, 0, 0) → miss_similarity
→ confidence_mod = hit_similarity - miss_similarity
```

**Schema detection:**
```
Run 100 tracers through similar patterns
→ trace_journey_events(tracer_1) ≈ trace_journey_events(tracer_2) ≈ ...
→ Similar journeys = schema candidate
```
