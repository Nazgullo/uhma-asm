; receipt.asm — Unified Holographic Receipt Layer + Cognitive Self-Model
;
; RECEIPT EMISSION:
; @entry emit_receipt_full(edi=event,esi=ctx,edx=actual,ecx=pred,r8d=region,r9d=aux,xmm0=conf,xmm1=val)
; @entry emit_receipt_simple(edi=event,esi=ctx,edx=token,xmm0=conf) -> void
;
; TRACE QUERIES:
; @entry receipt_resonate(edi=event,esi=ctx,edx=token) -> xmm0=similarity
; @entry trace_region_performance(edi=region) -> xmm0=ratio
; @entry trace_context_confidence(edi=ctx) -> xmm0=ratio
; @entry trace_hit_miss_ratio() -> xmm0=ratio
;
; INTROSPECTIVE STATE (REPL "intro"):
; @entry intro_query_confusion(edi=ctx) -> xmm0=resonance ; MISS resonance
; @entry intro_query_confidence(edi=ctx) -> xmm0=resonance ; HIT resonance
; @entry intro_query_learning(edi=ctx) -> xmm0=resonance ; LEARN resonance
; @entry intro_query_self_surprise(edi=ctx) -> xmm0=resonance ; EVENT_SELF resonance
; @entry intro_get_state(edi=ctx) -> eax=state, xmm0=strength ; dominant state
; @entry intro_get_self_awareness() -> xmm0=ratio ; self-surprise / total-miss
; @entry intro_report(edi=ctx) -> void ; REPL "intro" command
;
; SEMANTIC SELF-KNOWLEDGE (REPL "self"):
; @entry self_show_context_types() -> eax=strengths, edx=weaknesses
;
; CAUSAL MODEL (REPL "causal"):
; @entry causal_query_modification(edi=event,esi=ctx) -> xmm0=resonance
; @entry causal_report(edi=ctx) -> void ; REPL "causal" command
;
; META-STRATEGY:
; @entry meta_recommend_strategy(edi=ctx) -> eax=recommended_event_type
;
; DEBUG COMMANDS:
; @entry receipt_why_miss() -> void ; REPL "why" command
; @entry receipt_show_misses(edi=n) -> void ; REPL "misses N"
;
; @calls vsa.asm:holo_gen_vec, holo_bind_f64, holo_superpose_f64
; @calledby dispatch.asm, learn.asm, emit.asm, observe.asm, dreams.asm, repl.asm, introspect.asm
;
; STORAGE: UNIFIED_TRACE_IDX=240, 8KB | ST_LAST_MISS_* for "why" command
; 8 DIMENSIONS: time→tracer→aux→region→predicted→actual→ctx→event
;
; SELF-AWARENESS SYSTEM:
;   EVENT_SELF (type 15) emitted on self-model violations
;   intro_query_self_surprise() queries EVENT_SELF resonance
;   intro_get_self_awareness() = self_surprise / (total_miss + epsilon)
;   Enables system to distinguish "I was wrong about myself" vs "world surprised me"
;
; GOTCHAS:
;   - emit_receipt_full for MISS must include predicted token (diagnostic key)
;   - receipt_resonate returns f64 similarity, not bool
;   - No working buffer - all history is holographic, only last miss stored for "why"
;   - Causal queries need receipts with aux=accuracy*1000 (emitted by modify.asm)

%include "syscalls.inc"
%include "constants.inc"

section .data
    ; Debug output messages
    rcpt_emit_msg:      db "  #", 0
    rcpt_event_lbl:     db " ", 0
    rcpt_ctx_lbl:       db " ctx=0x", 0
    rcpt_tok_lbl:       db " tok=0x", 0
    rcpt_pred_lbl:      db " pred=0x", 0
    rcpt_fid_lbl:       db " fid=", 0
    rcpt_nl:            db 10, 0

    rcpt_dump_hdr:      db "[RECEIPTS] Holographic trace query:", 10, 0
    rcpt_dump_mid:      db "):", 10, 0
    rcpt_dump_trace_hdr: db "[RECEIPTS] Holographic trace resonance by event:", 10, 0
    rcpt_dump_evt_lbl:  db "  ", 0
    rcpt_dump_res_lbl:  db " = ", 0
    rcpt_dump_ratio:    db "  HIT/MISS ratio: ", 0
    rcpt_dump_trace_mag: db "  Trace magnitude: ", 0
    rcpt_dump_last_miss: db "  Last miss: ctx=0x", 0
    rcpt_resonate_hdr:  db "[RESONATE] ", 0
    rcpt_resonate_sim:  db " similarity=", 0

    listen_msg:         db "[RECEIPT] Unified holographic trace enabled", 10, 0

    ; Event type names (9 chars each, padded)
    evt_hit:            db "HIT      ", 0
    evt_miss:           db "MISS     ", 0
    evt_new:            db "NEW      ", 0
    evt_learn:          db "LEARN    ", 0
    evt_emit:           db "EMIT     ", 0
    evt_prune:          db "PRUNE    ", 0
    evt_promote:        db "PROMOTE  ", 0
    evt_dream:          db "DREAM    ", 0
    evt_observe:        db "OBSERVE  ", 0
    evt_evolve:         db "EVOLVE   ", 0
    evt_holo_pred:      db "HOLOPRED ", 0
    evt_graph_pred:     db "GRAPHPRED", 0
    evt_journey:        db "JOURNEY  ", 0
    evt_generalize:     db "GENERALIZE", 0
    evt_specialize:     db "SPECIALIZE", 0
    evt_self:           db "SELF     ", 0
    evt_unknown:        db "UNKNOWN  ", 0

    ; Base fidelity table (f64) - indexed by event type
    align 8
    fidelity_table:
        dq 0x3FD3333333333333  ; EVENT_HIT      = 0.30
        dq 0x3FE999999999999A  ; EVENT_MISS     = 0.80
        dq 0x3FE6666666666666  ; EVENT_NEW      = 0.70
        dq 0x3FE999999999999A  ; EVENT_LEARN    = 0.80
        dq 0x3FE6666666666666  ; EVENT_EMIT     = 0.70
        dq 0x3FE0000000000000  ; EVENT_PRUNE    = 0.50
        dq 0x3FE0000000000000  ; EVENT_PROMOTE  = 0.50
        dq 0x3FD999999999999A  ; EVENT_DREAM    = 0.40
        dq 0x3FD999999999999A  ; EVENT_OBSERVE  = 0.40
        dq 0x3FE0000000000000  ; EVENT_EVOLVE   = 0.50
        dq 0x3FD3333333333333  ; EVENT_HOLO_PRED= 0.30
        dq 0x3FD3333333333333  ; EVENT_GRAPH_PRED=0.30
        dq 0x3FD3333333333333  ; EVENT_JOURNEY  = 0.30
        dq 0x3FE6666666666666  ; EVENT_GENERALIZE=0.70 - important for causal
        dq 0x3FE6666666666666  ; EVENT_SPECIALIZE=0.70 - important for causal
        dq 0x3FE999999999999A  ; EVENT_SELF     = 0.80 - self-model violation, high importance

    ; Constants
    align 8
    one_f64:            dq 1.0
    valence_boost:      dq 0.3    ; how much |valence| boosts fidelity
    learning_rate:      dq 0.1    ; base superposition rate

section .bss
    ; Scratch vectors for receipt encoding (f64[1024] = 8KB each)
    ; Need vectors for all 8 dimensions + 1 for result
    align 64
    scratch_event_vec:    resb HOLO_VEC_BYTES
    scratch_ctx_vec:      resb HOLO_VEC_BYTES
    scratch_actual_vec:   resb HOLO_VEC_BYTES   ; actual token
    scratch_predicted_vec: resb HOLO_VEC_BYTES  ; predicted token (key for MISS debug)
    scratch_region_vec:   resb HOLO_VEC_BYTES   ; region/pattern that fired
    scratch_aux_vec:      resb HOLO_VEC_BYTES   ; auxiliary data
    scratch_tracer_vec:   resb HOLO_VEC_BYTES
    scratch_time_vec:     resb HOLO_VEC_BYTES
    scratch_result_vec:   resb HOLO_VEC_BYTES

section .text

extern print_cstr
extern print_hex32
extern print_u64
extern print_f32
extern print_f64
extern print_newline
extern holo_gen_vec
extern holo_bind_f64
extern holo_unbind_f64
extern holo_superpose_f64
extern holo_dot_f64
extern holo_magnitude_f64
extern holo_scale_f64
extern vsa_normalize

;; ============================================================
;; emit_receipt_full(event, ctx, actual, predicted, region, aux, confidence, valence)
;; edi = event_type (u16)
;; esi = ctx_hash (u32)
;; edx = actual_token (u32)
;; ecx = predicted_token (u32) - CRITICAL for MISS debugging
;; r8d = region_hash (u32) - which pattern/region fired
;; r9d = aux_data (u32) - hits, misses, schema level, etc.
;; xmm0 = confidence (f32)
;; xmm1 = valence (f64)
;;
;; Full 8-dimension encoding:
;; bind(event, bind(ctx, bind(actual, bind(predicted, bind(region, bind(aux, bind(tracer, time)))))))
;; ============================================================
global emit_receipt_full
emit_receipt_full:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 72             ; locals: [0]=fidelity, [8]=confidence, [16]=valence
                            ; [24]=event, [28]=ctx, [32]=actual, [36]=predicted
                            ; [40]=region, [44]=aux, [48]=time_bucket, [52-71]=padding

    mov rbx, SURFACE_BASE

    ; === FAST PATH: check listener mask ===
    mov eax, [rbx + STATE_OFFSET + ST_RECEIPT_LISTENER]
    test eax, eax
    jz .fast_return

    ; Save ALL parameters immediately (before any clobber)
    mov [rsp + 24], edi     ; event_type
    mov [rsp + 28], esi     ; ctx_hash
    mov [rsp + 32], edx     ; actual_token
    mov [rsp + 36], ecx     ; predicted_token (THE KEY ONE!)
    mov [rsp + 40], r8d     ; region_hash
    mov [rsp + 44], r9d     ; aux_data
    movss [rsp + 8], xmm0   ; confidence
    movsd [rsp + 16], xmm1  ; valence

    ; Keep frequently used in registers
    mov r12d, edi           ; event_type
    mov r13d, esi           ; ctx_hash
    mov r14d, edx           ; actual_token

    ; Compute time bucket: global_step / TRACE_TIME_BUCKET
    mov rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    xor edx, edx
    push rcx                ; save predicted across div
    mov ecx, TRACE_TIME_BUCKET
    div rcx
    pop rcx
    mov [rsp + 48], eax     ; time_bucket

    ; Increment total count
    inc qword [rbx + STATE_OFFSET + ST_RECEIPT_TOTAL]
    mov r15, [rbx + STATE_OFFSET + ST_RECEIPT_TOTAL]

    ; === COMPUTE FIDELITY ===
    movzx eax, r12w
    cmp eax, EVENT_TYPE_COUNT
    jge .use_default_fidelity
    lea rcx, [rel fidelity_table]
    movsd xmm2, [rcx + rax * 8]
    jmp .have_base_fidelity
.use_default_fidelity:
    mov rax, 0x3FE0000000000000   ; 0.5 default
    movq xmm2, rax
.have_base_fidelity:
    movsd xmm3, [rsp + 16]        ; valence
    mov rax, 0x7FFFFFFFFFFFFFFF
    movq xmm4, rax
    andpd xmm3, xmm4              ; |valence|
    mulsd xmm3, [rel valence_boost]
    addsd xmm2, xmm3
    xorpd xmm3, xmm3
    maxsd xmm2, xmm3
    movsd xmm3, [rel one_f64]
    minsd xmm2, xmm3
    movsd [rsp], xmm2             ; store fidelity

    ; === CHECK LISTENER_PRINT ===
    mov eax, [rbx + STATE_OFFSET + ST_RECEIPT_LISTENER]
    test eax, LISTENER_PRINT
    jz .skip_print

    ; Print receipt info (extended format for debugging)
    lea rdi, [rel rcpt_emit_msg]
    call print_cstr
    mov rdi, r15
    call print_u64
    lea rdi, [rel rcpt_event_lbl]
    call print_cstr
    mov edi, r12d
    call get_event_name
    mov rdi, rax
    call print_cstr
    lea rdi, [rel rcpt_ctx_lbl]
    call print_cstr
    mov edi, r13d
    call print_hex32
    lea rdi, [rel rcpt_tok_lbl]
    call print_cstr
    mov edi, r14d
    call print_hex32
    ; Print predicted if non-zero (key for MISS debugging)
    mov edi, [rsp + 36]
    test edi, edi
    jz .skip_predicted_print
    lea rdi, [rel rcpt_pred_lbl]
    call print_cstr
    mov edi, [rsp + 36]
    call print_hex32
.skip_predicted_print:
    lea rdi, [rel rcpt_fid_lbl]
    call print_cstr
    movsd xmm0, [rsp]
    cvtsd2ss xmm0, xmm0
    call print_f32
    call print_newline

.skip_print:
    ; === UPDATE LAST-MISS STATE (for "why" command) ===
    cmp r12w, EVENT_MISS
    jne .skip_last_miss

    ; Store last miss info
    mov [rbx + STATE_OFFSET + ST_LAST_MISS_CTX], r13d      ; ctx_hash
    mov [rbx + STATE_OFFSET + ST_LAST_MISS_ACTUAL], r14d   ; actual_token
    mov eax, [rsp + 36]
    mov [rbx + STATE_OFFSET + ST_LAST_MISS_PRED], eax      ; predicted_token
    mov eax, [rsp + 40]
    mov [rbx + STATE_OFFSET + ST_LAST_MISS_REGION], eax    ; region_hash
    mov eax, [rsp + 44]
    mov [rbx + STATE_OFFSET + ST_LAST_MISS_AUX], eax       ; aux_data
    movss xmm0, [rsp + 8]
    movss [rbx + STATE_OFFSET + ST_LAST_MISS_CONF], xmm0   ; confidence
    mov rax, [rbx + STATE_OFFSET + ST_RECEIPT_TOTAL]       ; use receipt count (guaranteed > 0)
    mov [rbx + STATE_OFFSET + ST_LAST_MISS_STEP], rax      ; step

.skip_last_miss:
    ; === CHECK LISTENER_HOLO (main holographic storage) ===
    mov eax, [rbx + STATE_OFFSET + ST_RECEIPT_LISTENER]
    test eax, LISTENER_HOLO
    jz .fast_return

    ; === ENCODE RECEIPT AS 8-DIMENSIONAL VSA VECTOR ===
    ; Inner to outer: time → tracer → aux → region → predicted → actual → ctx → event

    ; Generate time vector
    mov edi, [rsp + 48]           ; time_bucket
    add edi, TRACE_TIME_SEED
    lea rsi, [rel scratch_time_vec]
    call holo_gen_vec

    ; Generate tracer vector
    mov edi, [rbx + STATE_OFFSET + ST_ACTIVE_TRACER]
    add edi, TRACE_TRACER_SEED
    lea rsi, [rel scratch_tracer_vec]
    call holo_gen_vec

    ; Bind tracer ⊗ time → result
    lea rdi, [rel scratch_tracer_vec]
    lea rsi, [rel scratch_time_vec]
    lea rdx, [rel scratch_result_vec]
    call holo_bind_f64

    ; Generate aux vector
    mov edi, [rsp + 44]           ; aux_data
    add edi, TRACE_AUX_SEED
    lea rsi, [rel scratch_aux_vec]
    call holo_gen_vec

    ; Bind aux ⊗ (tracer⊗time) → result
    lea rdi, [rel scratch_aux_vec]
    lea rsi, [rel scratch_result_vec]
    lea rdx, [rel scratch_result_vec]
    call holo_bind_f64

    ; Generate region vector
    mov edi, [rsp + 40]           ; region_hash
    add edi, TRACE_REGION_SEED
    lea rsi, [rel scratch_region_vec]
    call holo_gen_vec

    ; Bind region ⊗ (aux⊗tracer⊗time) → result
    lea rdi, [rel scratch_region_vec]
    lea rsi, [rel scratch_result_vec]
    lea rdx, [rel scratch_result_vec]
    call holo_bind_f64

    ; Generate predicted vector (THE KEY DIMENSION FOR MISS!)
    mov edi, [rsp + 36]           ; predicted_token
    add edi, TRACE_PREDICTED_SEED
    lea rsi, [rel scratch_predicted_vec]
    call holo_gen_vec

    ; Bind predicted ⊗ (region⊗aux⊗tracer⊗time) → result
    lea rdi, [rel scratch_predicted_vec]
    lea rsi, [rel scratch_result_vec]
    lea rdx, [rel scratch_result_vec]
    call holo_bind_f64

    ; Generate actual vector
    mov edi, r14d                 ; actual_token
    lea rsi, [rel scratch_actual_vec]
    call holo_gen_vec

    ; Bind actual ⊗ (predicted⊗region⊗aux⊗tracer⊗time) → result
    lea rdi, [rel scratch_actual_vec]
    lea rsi, [rel scratch_result_vec]
    lea rdx, [rel scratch_result_vec]
    call holo_bind_f64

    ; Generate context vector
    mov edi, r13d                 ; ctx_hash
    lea rsi, [rel scratch_ctx_vec]
    call holo_gen_vec

    ; Bind ctx ⊗ (actual⊗predicted⊗region⊗aux⊗tracer⊗time) → result
    lea rdi, [rel scratch_ctx_vec]
    lea rsi, [rel scratch_result_vec]
    lea rdx, [rel scratch_result_vec]
    call holo_bind_f64

    ; Generate event vector
    movzx edi, r12w
    add edi, TRACE_EVENT_SEED
    lea rsi, [rel scratch_event_vec]
    call holo_gen_vec

    ; Bind event ⊗ (ctx⊗actual⊗predicted⊗region⊗aux⊗tracer⊗time) → result
    lea rdi, [rel scratch_event_vec]
    lea rsi, [rel scratch_result_vec]
    lea rdx, [rel scratch_result_vec]
    call holo_bind_f64

    ; NORMALIZE: Multiple bindings cause magnitude decay - restore to unit length
    lea rdi, [rel scratch_result_vec]
    call vsa_normalize

    ; === SCALE BY FIDELITY * LEARNING_RATE ===
    movsd xmm0, [rsp]
    mulsd xmm0, [rel learning_rate]
    lea rdi, [rel scratch_result_vec]
    call holo_scale_f64

    ; === SUPERPOSE TO UNIFIED TRACE ===
    mov eax, UNIFIED_TRACE_IDX
    imul rax, rax, HOLO_VEC_BYTES
    mov rdi, rbx
    mov rcx, HOLO_OFFSET              ; use register to avoid sign-extend of 0xC0000000
    add rdi, rcx
    add rdi, rax
    lea rsi, [rel scratch_result_vec]
    call holo_superpose_f64

.fast_return:
    add rsp, 72
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; emit_receipt(event_type, ctx, token, confidence, valence)
;; BACKWARD COMPATIBLE wrapper - calls emit_receipt_full with 0s
;; edi = event_type, esi = ctx, edx = token, xmm0 = confidence, xmm1 = valence
;; ============================================================
global emit_receipt
emit_receipt:
    ; Pass 0 for predicted, region, aux
    xor ecx, ecx              ; predicted = 0
    xor r8d, r8d              ; region = 0
    xor r9d, r9d              ; aux = 0
    jmp emit_receipt_full

;; ============================================================
;; emit_receipt_simple(event_type, ctx, token, confidence)
;; Simplified wrapper - uses current valence from presence
;; edi = event_type, esi = ctx, edx = token, xmm0 = confidence
;; ============================================================
global emit_receipt_simple
emit_receipt_simple:
    push rbx
    mov rbx, SURFACE_BASE
    ; Get current valence from presence field
    movss xmm1, [rbx + STATE_OFFSET + ST_PRESENCE + PRES_VALENCE * 4]
    cvtss2sd xmm1, xmm1           ; convert to f64
    pop rbx
    ; Pass 0 for predicted, region, aux
    xor ecx, ecx              ; predicted = 0
    xor r8d, r8d              ; region = 0
    xor r9d, r9d              ; aux = 0
    jmp emit_receipt_full

;; ============================================================
;; receipt_resonate(event_type, ctx, token) → xmm0 (similarity f64)
;; Query unified trace for similar past receipts.
;; Uses unbind to filter by event type, then dots with ctx/token probe.
;; edi = event_type (or -1 for any event), esi = ctx, edx = token
;; Returns similarity score in xmm0
;; ============================================================
global receipt_resonate
receipt_resonate:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 8

    mov rbx, SURFACE_BASE
    mov r12d, edi             ; event_type
    mov r13d, esi             ; ctx_hash
    mov r14d, edx             ; token_id

    ; === BUILD PROBE VECTOR ===
    ; If event_type specified: probe = bind(event, bind(ctx, token))
    ; If event_type = -1: probe = bind(ctx, token) (any event)

    ; Generate context vector
    mov edi, r13d
    lea rsi, [rel scratch_ctx_vec]
    call holo_gen_vec

    ; Generate token vector (or neutral if token=0)
    mov edi, r14d
    test edi, edi
    jz .token_neutral
    lea rsi, [rel scratch_actual_vec]
    call holo_gen_vec
    jmp .have_token
.token_neutral:
    ; Token=0 means "any token" - use ctx only as probe
    ; Copy ctx_vec to result (no token binding)
    lea rsi, [rel scratch_ctx_vec]
    lea rdi, [rel scratch_result_vec]
    mov ecx, HOLO_VEC_BYTES
    rep movsb
    jmp .check_event
.have_token:
    ; Bind ctx with token → scratch_result_vec
    lea rdi, [rel scratch_ctx_vec]
    lea rsi, [rel scratch_actual_vec]
    lea rdx, [rel scratch_result_vec]
    call holo_bind_f64

.check_event:
    ; Check if event_type specified
    cmp r12d, -1
    je .no_event_filter

    ; Generate event vector and bind with probe
    movzx edi, r12w
    add edi, TRACE_EVENT_SEED
    lea rsi, [rel scratch_event_vec]
    call holo_gen_vec

    ; Bind event with (ctx⊗token) probe
    lea rdi, [rel scratch_event_vec]
    lea rsi, [rel scratch_result_vec]
    lea rdx, [rel scratch_result_vec]
    call holo_bind_f64

.no_event_filter:
    ; === DOT PRODUCT WITH UNIFIED TRACE ===
    mov eax, UNIFIED_TRACE_IDX
    imul rax, rax, HOLO_VEC_BYTES
    mov rdi, rbx
    mov rcx, HOLO_OFFSET
    add rdi, rcx
    add rdi, rax                  ; unified trace ptr
    lea rsi, [rel scratch_result_vec]
    call holo_dot_f64             ; → xmm0 = similarity

    add rsp, 8
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; trace_set_active(tracer_id)
;; Set the active tracer ID. All subsequent receipts will include this.
;; edi = tracer_id (0 = no tracer)
;; ============================================================
global trace_set_active
trace_set_active:
    mov rax, SURFACE_BASE
    mov [rax + STATE_OFFSET + ST_ACTIVE_TRACER], edi
    ret

;; ============================================================
;; trace_get_active() → eax (tracer_id)
;; Get the currently active tracer ID.
;; ============================================================
global trace_get_active
trace_get_active:
    mov rax, SURFACE_BASE
    mov eax, [rax + STATE_OFFSET + ST_ACTIVE_TRACER]
    ret

;; ============================================================
;; trace_query_journey(tracer_id) → journey in scratch_result_vec
;; Query unified trace for all events with given tracer ID.
;; Returns the "journey vector" - unbind result that resonates
;; with all events from that tracer.
;; edi = tracer_id
;; Returns: xmm0 = magnitude of journey (0 = no events found)
;; ============================================================
global trace_query_journey
trace_query_journey:
    push rbx
    sub rsp, 8

    mov rbx, SURFACE_BASE

    ; Generate tracer vector
    add edi, TRACE_TRACER_SEED
    lea rsi, [rel scratch_tracer_vec]
    call holo_gen_vec

    ; Get unified trace pointer
    mov eax, UNIFIED_TRACE_IDX
    imul rax, rax, HOLO_VEC_BYTES
    mov rdi, rbx
    mov rcx, HOLO_OFFSET
    add rdi, rcx
    add rdi, rax                  ; unified trace ptr

    ; Unbind tracer from unified trace → journey vector
    ; journey = unbind(tracer, trace) = trace ⊗ tracer*
    lea rsi, [rel scratch_tracer_vec]
    lea rdx, [rel scratch_result_vec]
    call holo_unbind_f64

    ; Return magnitude of journey (indicates how much content)
    lea rdi, [rel scratch_result_vec]
    call holo_magnitude_f64       ; → xmm0 = magnitude

    add rsp, 8
    pop rbx
    ret

;; ============================================================
;; trace_journey_has_event(tracer_id, event_type) → xmm0 (similarity)
;; Check if a tracer's journey contains a specific event type.
;; edi = tracer_id, esi = event_type
;; Returns similarity score (high = event occurred in journey)
;; ============================================================
global trace_journey_has_event
trace_journey_has_event:
    push rbx
    push r12
    sub rsp, 8

    mov rbx, SURFACE_BASE
    mov r12d, esi             ; event_type

    ; Get journey vector for this tracer
    call trace_query_journey  ; leaves journey in scratch_result_vec

    ; Generate event vector
    mov edi, r12d
    add edi, TRACE_EVENT_SEED
    lea rsi, [rel scratch_event_vec]
    call holo_gen_vec

    ; Dot product: how much does journey resonate with this event?
    lea rdi, [rel scratch_result_vec]
    lea rsi, [rel scratch_event_vec]
    call holo_dot_f64         ; → xmm0 = similarity

    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; receipt_listen(mask)
;; edi = listener bitmask to enable
;; ============================================================
global receipt_listen
receipt_listen:
    mov rax, SURFACE_BASE
    or [rax + STATE_OFFSET + ST_RECEIPT_LISTENER], edi
    ret

;; ============================================================
;; receipt_mute(mask)
;; edi = listener bitmask to disable
;; ============================================================
global receipt_mute
receipt_mute:
    mov rax, SURFACE_BASE
    not edi
    and [rax + STATE_OFFSET + ST_RECEIPT_LISTENER], edi
    ret

;; ============================================================
;; receipt_dump(count)
;; Query holographic trace and show event resonance levels.
;; edi = ignored (kept for API compat)
;; Shows: event type resonance, hit/miss ratio, last miss
;; ============================================================
global receipt_dump
receipt_dump:
    push rbx
    push r12
    sub rsp, 8                ; 2 pushes (even) + 8 = aligned

    mov rbx, SURFACE_BASE

    ; Print header
    lea rdi, [rel rcpt_dump_trace_hdr]
    call print_cstr

    ; Query and print resonance for each event type
    xor r12d, r12d            ; event counter

.event_loop:
    cmp r12d, EVENT_TYPE_COUNT
    jge .event_done

    ; Print event name
    lea rdi, [rel rcpt_dump_evt_lbl]
    call print_cstr
    mov edi, r12d
    call get_event_name
    mov rdi, rax
    call print_cstr

    ; Query trace_event_count for this event type
    mov edi, r12d
    call trace_event_count    ; xmm0 = resonance magnitude
    lea rdi, [rel rcpt_dump_res_lbl]
    call print_cstr
    cvtsd2ss xmm0, xmm0
    call print_f32
    call print_newline

    inc r12d
    jmp .event_loop

.event_done:
    ; Print separator
    call print_newline

    ; Show raw trace magnitude (diagnostic)
    lea rdi, [rel rcpt_dump_trace_mag]
    call print_cstr
    mov eax, UNIFIED_TRACE_IDX
    imul rax, rax, HOLO_VEC_BYTES
    mov rdi, rbx
    mov rcx, HOLO_OFFSET              ; use register to avoid sign-extend
    add rdi, rcx
    add rdi, rax
    call holo_magnitude_f64
    cvtsd2ss xmm0, xmm0
    call print_f32
    call print_newline

    ; Show system hit/miss ratio
    lea rdi, [rel rcpt_dump_ratio]
    call print_cstr
    call trace_hit_miss_ratio
    cvtsd2ss xmm0, xmm0
    call print_f32
    call print_newline

    ; Show last miss if any
    mov rax, [rbx + STATE_OFFSET + ST_LAST_MISS_STEP]
    test rax, rax
    jz .no_last_miss

    lea rdi, [rel rcpt_dump_last_miss]
    call print_cstr
    mov edi, [rbx + STATE_OFFSET + ST_LAST_MISS_CTX]
    call print_hex32
    lea rdi, [rel arrow_str]
    call print_cstr
    mov edi, [rbx + STATE_OFFSET + ST_LAST_MISS_PRED]
    call print_hex32
    lea rdi, [rel actual_str]
    call print_cstr
    mov edi, [rbx + STATE_OFFSET + ST_LAST_MISS_ACTUAL]
    call print_hex32
    lea rdi, [rel close_paren]
    call print_cstr

.no_last_miss:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; receipt_init
;; Initialize receipt system (holographic trace + last-miss state)
;; ============================================================
global receipt_init
receipt_init:
    mov rax, SURFACE_BASE
    ; Clear last-miss state
    mov qword [rax + STATE_OFFSET + ST_LAST_MISS], 0
    mov qword [rax + STATE_OFFSET + ST_LAST_MISS + 8], 0
    mov qword [rax + STATE_OFFSET + ST_LAST_MISS + 16], 0
    mov qword [rax + STATE_OFFSET + ST_LAST_MISS + 24], 0
    ; Initialize counters
    mov qword [rax + STATE_OFFSET + ST_RECEIPT_TOTAL], 0
    mov dword [rax + STATE_OFFSET + ST_ACTIVE_TRACER], 0
    ; Enable holographic storage and print (no working buffer)
    mov dword [rax + STATE_OFFSET + ST_RECEIPT_LISTENER], LISTENER_HOLO | LISTENER_PRINT
    ret

;; ============================================================
;; get_event_name(event_type) → rax (string ptr)
;; ============================================================
get_event_name:
    cmp edi, EVENT_HIT
    je .e_hit
    cmp edi, EVENT_MISS
    je .e_miss
    cmp edi, EVENT_NEW
    je .e_new
    cmp edi, EVENT_LEARN
    je .e_learn
    cmp edi, EVENT_EMIT
    je .e_emit
    cmp edi, EVENT_PRUNE
    je .e_prune
    cmp edi, EVENT_PROMOTE
    je .e_promote
    cmp edi, EVENT_DREAM
    je .e_dream
    cmp edi, EVENT_OBSERVE
    je .e_observe
    cmp edi, EVENT_EVOLVE
    je .e_evolve
    cmp edi, EVENT_HOLO_PRED
    je .e_holo_pred
    cmp edi, EVENT_GRAPH_PRED
    je .e_graph_pred
    cmp edi, EVENT_JOURNEY
    je .e_journey
    lea rax, [rel evt_unknown]
    ret
.e_hit:
    lea rax, [rel evt_hit]
    ret
.e_miss:
    lea rax, [rel evt_miss]
    ret
.e_new:
    lea rax, [rel evt_new]
    ret
.e_learn:
    lea rax, [rel evt_learn]
    ret
.e_emit:
    lea rax, [rel evt_emit]
    ret
.e_prune:
    lea rax, [rel evt_prune]
    ret
.e_promote:
    lea rax, [rel evt_promote]
    ret
.e_dream:
    lea rax, [rel evt_dream]
    ret
.e_observe:
    lea rax, [rel evt_observe]
    ret
.e_evolve:
    lea rax, [rel evt_evolve]
    ret
.e_holo_pred:
    lea rax, [rel evt_holo_pred]
    ret
.e_graph_pred:
    lea rax, [rel evt_graph_pred]
    ret
.e_journey:
    lea rax, [rel evt_journey]
    ret

;; ============================================================
;; QUERY COMMANDS - "No more guessing" interface
;; ============================================================

section .data
    why_hdr:        db "[WHY-MISS] Last prediction failure:", 10, 0
    why_ctx:        db "  Context:   0x", 0
    why_actual:     db "  Actual:    0x", 0
    why_predicted:  db "  Predicted: 0x", 0
    why_region:     db "  Region:    0x", 0
    why_runner:     db "  Runner-up: 0x", 0
    why_conf:       db "  Confidence: ", 0
    why_none:       db "[WHY-MISS] No misses in recent history.", 10, 0
    misses_hdr:     db "[MISSES] Last ", 0
    misses_mid:     db " prediction failures:", 10, 0
    misses_none:    db "[MISSES] No misses found.", 10, 0
    misses_hdr_new: db "[MISSES] Last miss + trace resonance:", 10, 0
    misses_trace_ctx: db "  Context confusion: ", 0
    misses_trace_sys: db "  System confusion:  ", 0
    miss_line:      db "  ", 0
    arrow_str:      db " -> predicted ", 0
    actual_str:     db " (actual: ", 0
    close_paren:    db ")", 10, 0

section .text

;; ============================================================
;; receipt_why_miss()
;; Explain the most recent MISS - what was predicted vs actual
;; The answer to "why did it fail?"
;; Reads from ST_LAST_MISS_* (single record, no buffer scanning)
;; ============================================================
global receipt_why_miss
receipt_why_miss:
    push rbx
    sub rsp, 8                    ; 1 push (odd) + 8 = aligned

    mov rbx, SURFACE_BASE

    ; Check if there's been a miss (step > 0)
    mov rax, [rbx + STATE_OFFSET + ST_LAST_MISS_STEP]
    test rax, rax
    jz .why_not_found

    ; Print header
    lea rdi, [rel why_hdr]
    call print_cstr

    ; Context
    lea rdi, [rel why_ctx]
    call print_cstr
    mov edi, [rbx + STATE_OFFSET + ST_LAST_MISS_CTX]
    call print_hex32
    call print_newline

    ; Actual token
    lea rdi, [rel why_actual]
    call print_cstr
    mov edi, [rbx + STATE_OFFSET + ST_LAST_MISS_ACTUAL]
    call print_hex32
    call print_newline

    ; Predicted token (THE KEY!)
    lea rdi, [rel why_predicted]
    call print_cstr
    mov edi, [rbx + STATE_OFFSET + ST_LAST_MISS_PRED]
    call print_hex32
    call print_newline

    ; Region hash
    lea rdi, [rel why_region]
    call print_cstr
    mov edi, [rbx + STATE_OFFSET + ST_LAST_MISS_REGION]
    call print_hex32
    call print_newline

    ; Runner-up (aux)
    lea rdi, [rel why_runner]
    call print_cstr
    mov edi, [rbx + STATE_OFFSET + ST_LAST_MISS_AUX]
    call print_hex32
    call print_newline

    ; Confidence
    lea rdi, [rel why_conf]
    call print_cstr
    movss xmm0, [rbx + STATE_OFFSET + ST_LAST_MISS_CONF]
    call print_f32
    call print_newline

    jmp .why_done

.why_not_found:
    lea rdi, [rel why_none]
    call print_cstr

.why_done:
    add rsp, 8
    pop rbx
    ret

;; ============================================================
;; receipt_show_misses(count)
;; Show last miss + holographic trace resonance for context
;; edi = count (ignored - only last miss tracked, trace has history)
;; ============================================================
global receipt_show_misses
receipt_show_misses:
    push rbx
    push r12
    sub rsp, 8                    ; 2 pushes (even) + 8 = aligned

    mov rbx, SURFACE_BASE

    ; Print header
    lea rdi, [rel misses_hdr_new]
    call print_cstr

    ; Check if we have a last miss
    mov rax, [rbx + STATE_OFFSET + ST_LAST_MISS_STEP]
    test rax, rax
    jz .misses_none_found

    ; Save last miss ctx for trace query
    mov r12d, [rbx + STATE_OFFSET + ST_LAST_MISS_CTX]

    ; Print last miss: "  ctx -> predicted 0x... (actual: 0x...)"
    lea rdi, [rel miss_line]
    call print_cstr
    mov edi, r12d
    call print_hex32
    lea rdi, [rel arrow_str]
    call print_cstr
    mov edi, [rbx + STATE_OFFSET + ST_LAST_MISS_PRED]
    call print_hex32
    lea rdi, [rel actual_str]
    call print_cstr
    mov edi, [rbx + STATE_OFFSET + ST_LAST_MISS_ACTUAL]
    call print_hex32
    lea rdi, [rel close_paren]
    call print_cstr

    ; Query trace for MISS resonance at this context
    lea rdi, [rel misses_trace_ctx]
    call print_cstr
    mov edi, r12d                 ; last miss ctx
    call trace_context_confidence ; xmm0 = HIT/(HIT+MISS) ratio
    ; Invert to get confusion: 1 - confidence
    mov rax, 0x3FF0000000000000   ; 1.0 f64
    movq xmm1, rax
    subsd xmm1, xmm0              ; confusion = 1 - confidence
    cvtsd2ss xmm0, xmm1
    call print_f32
    call print_newline

    ; Query system-wide MISS resonance
    lea rdi, [rel misses_trace_sys]
    call print_cstr
    call trace_hit_miss_ratio     ; xmm0 = system HIT/(HIT+MISS)
    ; Invert to get confusion
    mov rax, 0x3FF0000000000000
    movq xmm1, rax
    subsd xmm1, xmm0
    cvtsd2ss xmm0, xmm1
    call print_f32
    call print_newline

    jmp .misses_done

.misses_none_found:
    lea rdi, [rel misses_none]
    call print_cstr

.misses_done:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; INTROSPECTIVE STATE - Query self-model via unified trace
;; "How am I feeling in this context?"
;; ============================================================

section .data
    intro_hdr:          db "[INTRO] Current context introspection:", 10, 0
    intro_confused:     db "  CONFUSED:   ", 0
    intro_confident:    db "  CONFIDENT:  ", 0
    intro_learning:     db "  LEARNING:   ", 0
    intro_self_surprise: db "  SELF-SURPRISE: ", 0
    intro_self_aware:   db "  SELF-AWARE: ", 0
    intro_state:        db "  STATE:      ", 0
    intro_state_confused:   db "CONFUSED (high miss resonance)", 10, 0
    intro_state_confident:  db "CONFIDENT (high hit resonance)", 10, 0
    intro_state_learning:   db "LEARNING (high learn resonance)", 10, 0
    intro_state_neutral:    db "NEUTRAL (no dominant pattern)", 10, 0

section .text

;; ============================================================
;; intro_query_confusion(ctx) -> xmm0 (f64)
;; How confused am I in this context? (MISS resonance)
;; edi = ctx_hash
;; ============================================================
global intro_query_confusion
intro_query_confusion:
    mov esi, edi              ; ctx
    mov edi, EVENT_MISS
    xor edx, edx              ; any token
    jmp receipt_resonate

;; ============================================================
;; intro_query_confidence(ctx) -> xmm0 (f64)
;; How confident am I in this context? (HIT resonance)
;; edi = ctx_hash
;; ============================================================
global intro_query_confidence
intro_query_confidence:
    mov esi, edi              ; ctx
    mov edi, EVENT_HIT
    xor edx, edx              ; any token
    jmp receipt_resonate

;; ============================================================
;; intro_query_learning(ctx) -> xmm0 (f64)
;; Am I actively learning in this context? (LEARN resonance)
;; edi = ctx_hash
;; ============================================================
global intro_query_learning
intro_query_learning:
    mov esi, edi              ; ctx
    mov edi, EVENT_LEARN
    xor edx, edx              ; any token
    jmp receipt_resonate

;; ============================================================
;; intro_query_self_surprise(ctx) -> xmm0 (f64)
;; Was I surprised about my own self-model? (EVENT_SELF resonance)
;; This is the core self-awareness query: self-error vs world-error.
;; edi = ctx_hash
;; ============================================================
global intro_query_self_surprise
intro_query_self_surprise:
    mov esi, edi              ; ctx
    mov edi, EVENT_SELF
    xor edx, edx              ; any token
    jmp receipt_resonate

;; ============================================================
;; intro_get_self_awareness() -> xmm0 (f64)
;; Compute self-awareness ratio: self_surprise / total_miss
;; High ratio = more errors are about self-model, not world
;; Returns: 0.0-1.0 normalized self-awareness score
;; ============================================================
global intro_get_self_awareness
intro_get_self_awareness:
    push rbx
    push r12
    sub rsp, 24               ; [0]=self_surprise, [8]=total_miss, [16]=pad

    mov rbx, SURFACE_BASE

    ; Query self-surprise resonance (EVENT_SELF)
    mov edi, EVENT_SELF
    xor esi, esi              ; any context
    xor edx, edx              ; any token
    call receipt_resonate
    movsd [rsp], xmm0         ; save self_surprise

    ; Query total miss resonance (EVENT_MISS)
    mov edi, EVENT_MISS
    xor esi, esi
    xor edx, edx
    call receipt_resonate
    movsd [rsp + 8], xmm0     ; save total_miss

    ; Self-awareness = self_surprise / (total_miss + epsilon)
    movsd xmm0, [rsp]         ; self_surprise
    movsd xmm1, [rsp + 8]     ; total_miss
    mov rax, 0x3F1A36E2EB1C432D  ; epsilon = 0.0001
    movq xmm2, rax
    addsd xmm1, xmm2          ; avoid div by zero
    divsd xmm0, xmm1          ; ratio

    ; Clamp to [0.0, 1.0]
    xorpd xmm2, xmm2          ; 0.0
    maxsd xmm0, xmm2
    mov rax, 0x3FF0000000000000  ; 1.0
    movq xmm2, rax
    minsd xmm0, xmm2

    add rsp, 24
    pop r12
    pop rbx
    ret

;; ============================================================
;; intro_get_state(ctx) -> eax=state_id, xmm0=strength
;; Which introspective state dominates current context?
;; edi = ctx_hash (or 0 to use ST_CTX_HASH)
;; Returns: eax = INTRO_* enum, xmm0 = dominant resonance strength
;; ============================================================
global intro_get_state
intro_get_state:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 40               ; [0]=ctx, [8]=confused, [16]=confident, [24]=learning, [32]=padding

    mov rbx, SURFACE_BASE

    ; Get context hash (use current if not provided)
    test edi, edi
    jnz .have_ctx
    mov edi, [rbx + STATE_OFFSET + ST_CTX_HASH]
.have_ctx:
    mov [rsp], edi            ; save ctx

    ; Query confusion (MISS resonance)
    call intro_query_confusion
    movsd [rsp + 8], xmm0

    ; Query confidence (HIT resonance)
    mov edi, [rsp]
    call intro_query_confidence
    movsd [rsp + 16], xmm0

    ; Query learning (LEARN resonance)
    mov edi, [rsp]
    call intro_query_learning
    movsd [rsp + 24], xmm0

    ; Find dominant state
    movsd xmm0, [rsp + 8]     ; confused
    movsd xmm1, [rsp + 16]    ; confident
    movsd xmm2, [rsp + 24]    ; learning
    mov eax, INTRO_CONFUSED
    movsd xmm3, xmm0          ; max = confused

    ucomisd xmm1, xmm3
    jbe .check_learning
    mov eax, INTRO_CONFIDENT
    movsd xmm3, xmm1

.check_learning:
    ucomisd xmm2, xmm3
    jbe .have_state
    mov eax, INTRO_LEARNING
    movsd xmm3, xmm2

.have_state:
    ; Check if any resonance is significant (> 0.01)
    mov rcx, 0x3F847AE147AE147B  ; 0.01 f64
    movq xmm4, rcx
    ucomisd xmm3, xmm4
    ja .state_done
    mov eax, INTRO_IDLE       ; no significant pattern

.state_done:
    movsd xmm0, xmm3          ; return strength in xmm0
    add rsp, 40
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; intro_report(ctx) -> void
;; Print introspective state report for context.
;; edi = ctx_hash (or 0 to use current)
;; REPL "intro" command implementation.
;; ============================================================
global intro_report
intro_report:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 40

    mov rbx, SURFACE_BASE

    ; Get context hash
    test edi, edi
    jnz .have_ctx
    mov edi, [rbx + STATE_OFFSET + ST_CTX_HASH]
.have_ctx:
    mov r12d, edi             ; save ctx
    mov [rsp], edi

    ; Print header
    lea rdi, [rel intro_hdr]
    call print_cstr

    ; Query and print confusion
    lea rdi, [rel intro_confused]
    call print_cstr
    mov edi, r12d
    call intro_query_confusion
    movsd [rsp + 8], xmm0
    cvtsd2ss xmm0, xmm0
    call print_f32
    call print_newline

    ; Query and print confidence
    lea rdi, [rel intro_confident]
    call print_cstr
    mov edi, r12d
    call intro_query_confidence
    movsd [rsp + 16], xmm0
    cvtsd2ss xmm0, xmm0
    call print_f32
    call print_newline

    ; Query and print learning
    lea rdi, [rel intro_learning]
    call print_cstr
    mov edi, r12d
    call intro_query_learning
    movsd [rsp + 24], xmm0
    cvtsd2ss xmm0, xmm0
    call print_f32
    call print_newline

    ; Query and print self-surprise (self-awareness metric)
    lea rdi, [rel intro_self_surprise]
    call print_cstr
    mov edi, r12d
    call intro_query_self_surprise
    movsd [rsp + 32], xmm0
    cvtsd2ss xmm0, xmm0
    call print_f32
    call print_newline

    ; Print overall self-awareness ratio
    lea rdi, [rel intro_self_aware]
    call print_cstr
    call intro_get_self_awareness
    cvtsd2ss xmm0, xmm0
    call print_f32
    call print_newline

    ; Get dominant state and print
    lea rdi, [rel intro_state]
    call print_cstr

    mov edi, r12d
    call intro_get_state      ; eax = state, xmm0 = strength

    cmp eax, INTRO_CONFUSED
    je .print_confused
    cmp eax, INTRO_CONFIDENT
    je .print_confident
    cmp eax, INTRO_LEARNING
    je .print_learning
    lea rdi, [rel intro_state_neutral]
    jmp .print_state

.print_confused:
    lea rdi, [rel intro_state_confused]
    jmp .print_state
.print_confident:
    lea rdi, [rel intro_state_confident]
    jmp .print_state
.print_learning:
    lea rdi, [rel intro_state_learning]
.print_state:
    call print_cstr

    add rsp, 40
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; SEMANTIC SELF-KNOWLEDGE - Query trace for context-type strengths/weaknesses
;; Context types = high 4 bits of ctx_hash (16 types)
;; ============================================================

section .data
    self_ctx_hdr:       db "[SELF] Context-type confidence (from trace):", 10, 0
    self_strength_lbl:  db "  STRENGTH: type 0x", 0
    self_weakness_lbl:  db "  WEAKNESS: type 0x", 0
    self_conf_lbl:      db " = ", 0
    strength_thresh:    dq 0.7    ; above = strength
    weakness_thresh:    dq 0.3    ; below = weakness

section .text

;; ============================================================
;; self_show_context_types() -> void
;; Scan 16 context types via trace_context_confidence, show strengths/weaknesses.
;; Called by REPL "self" command.
;; ============================================================
global self_show_context_types
self_show_context_types:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8                ; 5 pushes (odd) + 8 = 16-aligned

    xor r12d, r12d            ; ctx_type counter 0..15
    xor r14d, r14d            ; strengths found
    xor r15d, r15d            ; weaknesses found

    ; Print header
    lea rdi, [rel self_ctx_hdr]
    call print_cstr

.type_loop:
    cmp r12d, 16
    jge .done

    ; Generate ctx_hash = (type << 28) | 0x0FFFFFFF (representative hash)
    mov edi, r12d
    shl edi, 28
    or edi, 0x0FFFFFFF
    mov r13d, edi             ; save ctx_hash

    ; Query trace_context_confidence
    call trace_context_confidence  ; xmm0 = confidence (f64)

    ; Check if strength (> 0.7)
    movsd xmm1, [rel strength_thresh]
    ucomisd xmm0, xmm1
    jbe .check_weakness

    ; Print strength
    push r12
    movsd xmm2, xmm0          ; save confidence
    lea rdi, [rel self_strength_lbl]
    call print_cstr
    mov edi, r12d             ; type
    call print_hex32
    lea rdi, [rel self_conf_lbl]
    call print_cstr
    movsd xmm0, xmm2
    cvtsd2ss xmm0, xmm0
    call print_f32
    call print_newline
    pop r12
    inc r14d
    jmp .next_type

.check_weakness:
    ; Check if weakness (< 0.3)
    movsd xmm1, [rel weakness_thresh]
    ucomisd xmm0, xmm1
    jae .next_type

    ; But only if there's actually signal (confidence > 0.01)
    mov rax, 0x3F847AE147AE147B  ; 0.01 f64
    movq xmm1, rax
    ucomisd xmm0, xmm1
    jbe .next_type            ; no signal = ignore

    ; Print weakness
    push r12
    movsd xmm2, xmm0
    lea rdi, [rel self_weakness_lbl]
    call print_cstr
    mov edi, r12d
    call print_hex32
    lea rdi, [rel self_conf_lbl]
    call print_cstr
    movsd xmm0, xmm2
    cvtsd2ss xmm0, xmm0
    call print_f32
    call print_newline
    pop r12
    inc r15d

.next_type:
    inc r12d
    jmp .type_loop

.done:
    ; Return counts: eax = strengths, edx = weaknesses
    mov eax, r14d
    mov edx, r15d

    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; CAUSAL MODEL - Query what modifications work in which contexts
;; Based on historical PRUNE/PROMOTE/GENERALIZE/SPECIALIZE receipts
;; ============================================================

section .data
    causal_hdr:         db "[CAUSAL] Modification history from trace:", 10, 0
    causal_prune_lbl:   db "  PRUNE:      ", 0
    causal_promote_lbl: db "  PROMOTE:    ", 0
    causal_general_lbl: db "  GENERALIZE: ", 0
    causal_special_lbl: db "  SPECIALIZE: ", 0
    causal_resonance:   db " resonance=", 0
    causal_no_data:     db "(no history)", 10, 0
    causal_helps:       db " (helps)", 10, 0
    causal_hurts:       db " (hurts)", 10, 0
    causal_neutral:     db " (neutral)", 10, 0
    causal_signal_thresh: dq 0.01  ; below = no signal

section .text

;; ============================================================
;; causal_query_modification(mod_type, ctx) -> xmm0=resonance
;; Query trace for historical effect of modification type in context.
;; edi = modification event type (EVENT_PRUNE, EVENT_PROMOTE, etc.)
;; esi = ctx_hash (0 = use current)
;; Returns: xmm0 = resonance strength (how much history for this mod+ctx)
;; ============================================================
global causal_query_modification
causal_query_modification:
    push rbx
    sub rsp, 8

    mov rbx, SURFACE_BASE

    ; Use current context if not specified
    test esi, esi
    jnz .have_ctx
    mov esi, [rbx + STATE_OFFSET + ST_CTX_HASH]
.have_ctx:
    xor edx, edx              ; token = 0 (any)
    call receipt_resonate     ; xmm0 = resonance

    add rsp, 8
    pop rbx
    ret

;; ============================================================
;; causal_report(ctx) -> void
;; Print causal model report for context - what modifications have history.
;; edi = ctx_hash (0 = use current)
;; REPL "causal" command implementation.
;; ============================================================
global causal_report
causal_report:
    push rbx
    push r12
    push r13
    sub rsp, 8                ; 3 pushes (odd) + 8 = 16-aligned

    mov rbx, SURFACE_BASE

    ; Get context hash
    test edi, edi
    jnz .have_ctx
    mov edi, [rbx + STATE_OFFSET + ST_CTX_HASH]
.have_ctx:
    mov r12d, edi             ; save ctx

    ; Print header
    lea rdi, [rel causal_hdr]
    call print_cstr

    ; Query PRUNE
    lea rdi, [rel causal_prune_lbl]
    call print_cstr
    mov edi, EVENT_PRUNE
    mov esi, r12d
    call causal_query_modification
    movsd xmm1, xmm0          ; save resonance
    call .print_resonance_result

    ; Query PROMOTE
    lea rdi, [rel causal_promote_lbl]
    call print_cstr
    mov edi, EVENT_PROMOTE
    mov esi, r12d
    call causal_query_modification
    movsd xmm1, xmm0
    call .print_resonance_result

    ; Query GENERALIZE
    lea rdi, [rel causal_general_lbl]
    call print_cstr
    mov edi, EVENT_GENERALIZE
    mov esi, r12d
    call causal_query_modification
    movsd xmm1, xmm0
    call .print_resonance_result

    ; Query SPECIALIZE
    lea rdi, [rel causal_special_lbl]
    call print_cstr
    mov edi, EVENT_SPECIALIZE
    mov esi, r12d
    call causal_query_modification
    movsd xmm1, xmm0
    call .print_resonance_result

    add rsp, 8
    pop r13
    pop r12
    pop rbx
    ret

; Helper: print resonance result with interpretation
; xmm1 = resonance value
.print_resonance_result:
    push rbx
    sub rsp, 16
    movsd [rsp], xmm1

    ; Check if significant signal
    movsd xmm0, [rel causal_signal_thresh]
    ucomisd xmm1, xmm0
    ja .has_signal

    ; No data
    lea rdi, [rel causal_no_data]
    call print_cstr
    jmp .result_done

.has_signal:
    lea rdi, [rel causal_resonance]
    call print_cstr
    movsd xmm0, [rsp]
    cvtsd2ss xmm0, xmm0
    call print_f32

    ; Interpret: high resonance = more history, interpret based on modification type
    ; For now, just indicate presence of history
    movsd xmm0, [rsp]
    mov rax, 0x3FD0000000000000  ; 0.25 threshold
    movq xmm1, rax
    ucomisd xmm0, xmm1
    jbe .neutral_result
    lea rdi, [rel causal_helps]
    jmp .print_interp
.neutral_result:
    lea rdi, [rel causal_neutral]
.print_interp:
    call print_cstr

.result_done:
    add rsp, 16
    pop rbx
    ret

;; ============================================================
;; META-STRATEGY - Use causal history to guide repair decisions
;; ============================================================

section .data
    meta_strat_thresh:  dq 0.05   ; minimum resonance to consider

section .text

;; ============================================================
;; meta_recommend_strategy(ctx) -> eax=strategy
;; Based on causal history, what modification should we do here?
;; edi = ctx_hash (0 = use current)
;; Returns: eax = recommended event type (EVENT_GENERALIZE, EVENT_SPECIALIZE, or 0 if no data)
;; ============================================================
global meta_recommend_strategy
meta_recommend_strategy:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 40               ; [0]=ctx, [8]=gen_res, [16]=spec_res, [24]=prune_res, [32]=promote_res

    mov rbx, SURFACE_BASE

    ; Get context hash
    test edi, edi
    jnz .have_ctx
    mov edi, [rbx + STATE_OFFSET + ST_CTX_HASH]
.have_ctx:
    mov [rsp], edi            ; save ctx
    mov r12d, edi

    ; Query GENERALIZE resonance
    mov edi, EVENT_GENERALIZE
    mov esi, r12d
    call causal_query_modification
    movsd [rsp + 8], xmm0

    ; Query SPECIALIZE resonance
    mov edi, EVENT_SPECIALIZE
    mov esi, r12d
    call causal_query_modification
    movsd [rsp + 16], xmm0

    ; Query PRUNE resonance
    mov edi, EVENT_PRUNE
    mov esi, r12d
    call causal_query_modification
    movsd [rsp + 24], xmm0

    ; Query PROMOTE resonance
    mov edi, EVENT_PROMOTE
    mov esi, r12d
    call causal_query_modification
    movsd [rsp + 32], xmm0

    ; Find highest resonance (most historical evidence)
    movsd xmm0, [rsp + 8]     ; generalize
    movsd xmm1, [rsp + 16]    ; specialize
    movsd xmm2, [rsp + 24]    ; prune
    movsd xmm3, [rsp + 32]    ; promote

    mov eax, EVENT_GENERALIZE
    movsd xmm4, xmm0          ; max = generalize

    ucomisd xmm1, xmm4
    jbe .check_prune
    mov eax, EVENT_SPECIALIZE
    movsd xmm4, xmm1

.check_prune:
    ucomisd xmm2, xmm4
    jbe .check_promote
    mov eax, EVENT_PRUNE
    movsd xmm4, xmm2

.check_promote:
    ucomisd xmm3, xmm4
    jbe .check_thresh
    mov eax, EVENT_PROMOTE
    movsd xmm4, xmm3

.check_thresh:
    ; Check if any resonance is significant
    movsd xmm5, [rel meta_strat_thresh]
    ucomisd xmm4, xmm5
    ja .have_strategy
    xor eax, eax              ; no significant history, return 0

.have_strategy:
    add rsp, 40
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; COGNITIVE ACCESS - System queries its own trace for self-improvement
;; ============================================================

section .data
    perf_low_thresh: dq 0.3   ; below this = bad region

section .text

;; ============================================================
;; trace_region_performance(region_hash) → xmm0 (performance 0.0-1.0)
;; Query unified trace for HIT vs MISS ratio for given region.
;; Returns hit_resonance / (hit_resonance + miss_resonance + epsilon)
;; Used by pruning logic to identify underperforming regions.
;; edi = region_hash
;; ============================================================
global trace_region_performance
trace_region_performance:
    push rbx
    push r12
    sub rsp, 24           ; [0]=region_hash, [8]=hit_score, [16]=miss_score

    mov rbx, SURFACE_BASE
    mov [rsp], edi        ; save region_hash

    ; === Query HIT + region ===
    ; Generate event vector for HIT
    mov edi, EVENT_HIT
    add edi, TRACE_EVENT_SEED
    lea rsi, [rel scratch_event_vec]
    call holo_gen_vec

    ; Generate region vector
    mov edi, [rsp]
    add edi, TRACE_REGION_SEED
    lea rsi, [rel scratch_region_vec]
    call holo_gen_vec

    ; Bind event ⊗ region → probe
    lea rdi, [rel scratch_event_vec]
    lea rsi, [rel scratch_region_vec]
    lea rdx, [rel scratch_result_vec]
    call holo_bind_f64

    ; Dot with unified trace
    mov eax, UNIFIED_TRACE_IDX
    imul rax, rax, HOLO_VEC_BYTES
    mov rdi, rbx
    mov rcx, HOLO_OFFSET
    add rdi, rcx
    add rdi, rax
    lea rsi, [rel scratch_result_vec]
    call holo_dot_f64
    ; Clamp negative to zero
    xorpd xmm1, xmm1
    maxsd xmm0, xmm1
    movsd [rsp + 8], xmm0     ; hit_score

    ; === Query MISS + region ===
    mov edi, EVENT_MISS
    add edi, TRACE_EVENT_SEED
    lea rsi, [rel scratch_event_vec]
    call holo_gen_vec

    ; Region already generated, bind again
    lea rdi, [rel scratch_event_vec]
    lea rsi, [rel scratch_region_vec]
    lea rdx, [rel scratch_result_vec]
    call holo_bind_f64

    ; Dot with unified trace
    mov eax, UNIFIED_TRACE_IDX
    imul rax, rax, HOLO_VEC_BYTES
    mov rdi, rbx
    mov rcx, HOLO_OFFSET
    add rdi, rcx
    add rdi, rax
    lea rsi, [rel scratch_result_vec]
    call holo_dot_f64
    xorpd xmm1, xmm1
    maxsd xmm0, xmm1
    movsd [rsp + 16], xmm0    ; miss_score

    ; === Compute performance = hit / (hit + miss + epsilon) ===
    movsd xmm0, [rsp + 8]     ; hit
    movsd xmm1, [rsp + 16]    ; miss
    addsd xmm1, xmm0          ; hit + miss
    mov rax, 0x3EB0C6F7A0B5ED8D  ; epsilon = 1e-6
    movq xmm2, rax
    addsd xmm1, xmm2          ; hit + miss + epsilon
    divsd xmm0, xmm1          ; performance = hit / total

    add rsp, 24
    pop r12
    pop rbx
    ret

;; ============================================================
;; trace_context_confidence(ctx_hash) → xmm0 (confidence 0.0-1.0)
;; Query trace for historical HIT rate in given context.
;; High = system has been accurate here, low = unreliable context.
;; edi = ctx_hash
;; ============================================================
global trace_context_confidence
trace_context_confidence:
    push rbx
    sub rsp, 24           ; [0]=ctx_hash, [8]=hit_score, [16]=miss_score

    mov rbx, SURFACE_BASE
    mov [rsp], edi

    ; === Query HIT + ctx ===
    mov edi, EVENT_HIT
    add edi, TRACE_EVENT_SEED
    lea rsi, [rel scratch_event_vec]
    call holo_gen_vec

    mov edi, [rsp]        ; ctx_hash (raw, no seed for ctx)
    lea rsi, [rel scratch_ctx_vec]
    call holo_gen_vec

    lea rdi, [rel scratch_event_vec]
    lea rsi, [rel scratch_ctx_vec]
    lea rdx, [rel scratch_result_vec]
    call holo_bind_f64

    mov eax, UNIFIED_TRACE_IDX
    imul rax, rax, HOLO_VEC_BYTES
    mov rdi, rbx
    mov rcx, HOLO_OFFSET
    add rdi, rcx
    add rdi, rax
    lea rsi, [rel scratch_result_vec]
    call holo_dot_f64
    xorpd xmm1, xmm1
    maxsd xmm0, xmm1
    movsd [rsp + 8], xmm0

    ; === Query MISS + ctx ===
    mov edi, EVENT_MISS
    add edi, TRACE_EVENT_SEED
    lea rsi, [rel scratch_event_vec]
    call holo_gen_vec

    lea rdi, [rel scratch_event_vec]
    lea rsi, [rel scratch_ctx_vec]
    lea rdx, [rel scratch_result_vec]
    call holo_bind_f64

    mov eax, UNIFIED_TRACE_IDX
    imul rax, rax, HOLO_VEC_BYTES
    mov rdi, rbx
    mov rcx, HOLO_OFFSET
    add rdi, rcx
    add rdi, rax
    lea rsi, [rel scratch_result_vec]
    call holo_dot_f64
    xorpd xmm1, xmm1
    maxsd xmm0, xmm1
    movsd [rsp + 16], xmm0

    ; performance = hit / (hit + miss + epsilon)
    movsd xmm0, [rsp + 8]
    movsd xmm1, [rsp + 16]
    addsd xmm1, xmm0
    mov rax, 0x3EB0C6F7A0B5ED8D
    movq xmm2, rax
    addsd xmm1, xmm2
    divsd xmm0, xmm1

    add rsp, 24
    pop rbx
    ret

;; ============================================================
;; trace_token_learnability(token) → xmm0 (learnability score)
;; Query trace for how often this token appears in LEARN events.
;; High = token gets learned frequently (maybe problematic pattern).
;; edi = token_hash
;; ============================================================
global trace_token_learnability
trace_token_learnability:
    push rbx
    push r12
    sub rsp, 8

    mov rbx, SURFACE_BASE
    mov r12d, edi         ; save token

    ; Generate LEARN event probe
    mov edi, EVENT_LEARN
    add edi, TRACE_EVENT_SEED
    lea rsi, [rel scratch_event_vec]
    call holo_gen_vec

    ; Generate token probe
    mov edi, r12d
    lea rsi, [rel scratch_actual_vec]
    call holo_gen_vec

    ; Bind event ⊗ token → probe
    lea rdi, [rel scratch_event_vec]
    lea rsi, [rel scratch_actual_vec]
    lea rdx, [rel scratch_result_vec]
    call holo_bind_f64

    ; Dot with unified trace
    mov eax, UNIFIED_TRACE_IDX
    imul rax, rax, HOLO_VEC_BYTES
    mov rdi, rbx
    mov rcx, HOLO_OFFSET
    add rdi, rcx
    add rdi, rax
    lea rsi, [rel scratch_result_vec]
    call holo_dot_f64
    ; Clamp negative
    xorpd xmm1, xmm1
    maxsd xmm0, xmm1

    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; trace_event_count(event_type) → xmm0 (resonance magnitude)
;; Query trace for total resonance with given event type.
;; Proxy for "how many of this event have occurred".
;; edi = event_type
;; ============================================================
global trace_event_count
trace_event_count:
    push rbx
    sub rsp, 8

    mov rbx, SURFACE_BASE

    ; Generate event probe
    add edi, TRACE_EVENT_SEED
    lea rsi, [rel scratch_event_vec]
    call holo_gen_vec

    ; Unbind from trace to isolate this event type
    mov eax, UNIFIED_TRACE_IDX
    imul rax, rax, HOLO_VEC_BYTES
    mov rdi, rbx
    mov rcx, HOLO_OFFSET
    add rdi, rcx
    add rdi, rax
    lea rsi, [rel scratch_event_vec]
    lea rdx, [rel scratch_result_vec]
    call holo_unbind_f64

    ; Magnitude = proxy for count
    lea rdi, [rel scratch_result_vec]
    call holo_magnitude_f64

    add rsp, 8
    pop rbx
    ret

;; ============================================================
;; trace_hit_miss_ratio() → xmm0 (overall hit ratio 0.0-1.0)
;; Query trace for system-wide HIT vs MISS ratio.
;; Used for self-model: "how well am I doing overall?"
;; ============================================================
global trace_hit_miss_ratio
trace_hit_miss_ratio:
    push rbx
    sub rsp, 16           ; [0]=hit_mag, [8]=miss_mag

    mov rbx, SURFACE_BASE

    ; Get HIT magnitude
    mov edi, EVENT_HIT
    call trace_event_count
    movsd [rsp], xmm0

    ; Get MISS magnitude
    mov edi, EVENT_MISS
    call trace_event_count
    movsd [rsp + 8], xmm0

    ; ratio = hit / (hit + miss + epsilon)
    movsd xmm0, [rsp]
    movsd xmm1, [rsp + 8]
    addsd xmm1, xmm0
    mov rax, 0x3EB0C6F7A0B5ED8D
    movq xmm2, rax
    addsd xmm1, xmm2
    divsd xmm0, xmm1

    add rsp, 16
    pop rbx
    ret
