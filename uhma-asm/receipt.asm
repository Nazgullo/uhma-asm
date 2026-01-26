; receipt.asm — Unified Holographic Receipt Layer
;
; One trace. N dimensions. Query by unbind.
;
; Full encoding (8 dimensions):
;   bind(event, bind(ctx, bind(actual, bind(predicted, bind(region, bind(aux, bind(tracer, time)))))))
;
; Dimensions captured:
;   - event:     EVENT_HIT, EVENT_MISS, EVENT_LEARN, etc.
;   - ctx:       Context hash (previous token)
;   - actual:    Actual token that occurred
;   - predicted: What was predicted (0 if none) - CRITICAL for MISS debugging
;   - region:    Pattern/region pointer hash (which code fired)
;   - aux:       Auxiliary data (hits, misses, schema level, etc.)
;   - tracer:    Tracer ID for journey correlation
;   - time:      Time bucket for temporal queries
;
; Query any dimension via unbind. The answer is IN the trace.
;
; Backward compatible: emit_receipt_simple passes 0 for extended dimensions.

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

    rcpt_dump_hdr:      db "[RECEIPTS] Working buffer (last ", 0
    rcpt_dump_mid:      db "):", 10, 0
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
    ; === CHECK LISTENER_WORKING (tiny debug buffer) ===
    mov eax, [rbx + STATE_OFFSET + ST_RECEIPT_LISTENER]
    test eax, LISTENER_WORKING
    jz .skip_working

    ; Write to working buffer (extended 64-byte record)
    mov ecx, [rbx + STATE_OFFSET + ST_RECEIPT_WORK_POS]
    imul eax, ecx, 64
    lea rdi, [rbx + STATE_OFFSET + ST_RECEIPT_WORKING]
    add rdi, rax
    ; Store all fields
    mov rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov [rdi + 0], rax            ; timestamp
    mov [rdi + 8], r12w           ; event_type
    mov word [rdi + 10], 0        ; padding
    mov [rdi + 12], r13d          ; ctx_hash
    mov [rdi + 16], r14d          ; actual_token
    mov eax, [rsp + 36]
    mov [rdi + 20], eax           ; predicted_token (NEW!)
    movss xmm0, [rsp + 8]
    movss [rdi + 24], xmm0        ; confidence
    movsd xmm0, [rsp]
    movsd [rdi + 28], xmm0        ; fidelity
    mov eax, [rsp + 40]
    mov [rdi + 36], eax           ; region_hash (NEW!)
    mov eax, [rsp + 44]
    mov [rdi + 40], eax           ; aux_data (NEW!)
    mov eax, [rbx + STATE_OFFSET + ST_ACTIVE_TRACER]
    mov [rdi + 44], eax           ; tracer_id
    ; Advance position
    inc ecx
    and ecx, (ST_RECEIPT_WORK_CAP - 1)
    mov [rbx + STATE_OFFSET + ST_RECEIPT_WORK_POS], ecx

.skip_working:
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

    ; === SCALE BY FIDELITY * LEARNING_RATE ===
    movsd xmm0, [rsp]
    mulsd xmm0, [rel learning_rate]
    lea rdi, [rel scratch_result_vec]
    call holo_scale_f64

    ; === SUPERPOSE TO UNIFIED TRACE ===
    mov eax, UNIFIED_TRACE_IDX
    imul rax, rax, HOLO_VEC_BYTES
    mov rdi, HOLO_OFFSET
    add rdi, rbx
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
;; edi = number of recent entries from working buffer to show
;; ============================================================
global receipt_dump
receipt_dump:
    push rbx
    push r12
    push r13
    push r14

    mov r12d, edi             ; count
    mov rbx, SURFACE_BASE

    ; Print header
    lea rdi, [rel rcpt_dump_hdr]
    call print_cstr
    mov edi, r12d
    call print_u64
    lea rdi, [rel rcpt_dump_mid]
    call print_cstr

    ; Clamp count to buffer size
    cmp r12d, ST_RECEIPT_WORK_CAP
    jle .count_ok
    mov r12d, ST_RECEIPT_WORK_CAP
.count_ok:

    ; Get current position
    mov r13d, [rbx + STATE_OFFSET + ST_RECEIPT_WORK_POS]

    ; Calculate start position (pos - count, with wrap)
    mov ecx, r13d
    sub ecx, r12d
    jns .no_wrap
    add ecx, ST_RECEIPT_WORK_CAP
.no_wrap:

.dump_loop:
    test r12d, r12d
    jz .dump_done

    push rcx
    push r12

    ; Calculate entry address
    imul eax, ecx, 64
    lea rdi, [rbx + STATE_OFFSET + ST_RECEIPT_WORKING]
    add rdi, rax
    push rdi

    ; Print: #timestamp event ctx=X tok=Y fid=Z
    lea rdi, [rel rcpt_emit_msg]
    call print_cstr
    pop rdi
    push rdi
    mov rdi, [rdi + 0]            ; timestamp
    call print_u64
    lea rdi, [rel rcpt_event_lbl]
    call print_cstr
    pop rdi
    push rdi
    movzx edi, word [rdi + 8]     ; event_type
    call get_event_name
    mov rdi, rax
    call print_cstr
    lea rdi, [rel rcpt_ctx_lbl]
    call print_cstr
    pop rdi
    push rdi
    mov edi, [rdi + 12]           ; ctx_hash
    call print_hex32
    lea rdi, [rel rcpt_tok_lbl]
    call print_cstr
    pop rdi
    push rdi
    mov edi, [rdi + 16]           ; token_id
    call print_hex32
    lea rdi, [rel rcpt_fid_lbl]
    call print_cstr
    pop rdi
    movsd xmm0, [rdi + 24]        ; fidelity
    cvtsd2ss xmm0, xmm0
    call print_f32
    call print_newline

    pop r12
    pop rcx

    ; Next
    inc ecx
    and ecx, (ST_RECEIPT_WORK_CAP - 1)
    dec r12d
    jmp .dump_loop

.dump_done:
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; receipt_init
;; Initialize receipt system
;; ============================================================
global receipt_init
receipt_init:
    mov rax, SURFACE_BASE
    mov dword [rax + STATE_OFFSET + ST_RECEIPT_WORK_POS], 0
    mov qword [rax + STATE_OFFSET + ST_RECEIPT_TOTAL], 0
    mov dword [rax + STATE_OFFSET + ST_ACTIVE_TRACER], 0
    ; Enable holographic storage, working buffer, AND print for observability
    mov dword [rax + STATE_OFFSET + ST_RECEIPT_LISTENER], LISTENER_HOLO | LISTENER_WORKING | LISTENER_PRINT
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
    miss_line:      db "  ", 0
    arrow_str:      db " -> predicted ", 0
    actual_str:     db " (actual: ", 0
    close_paren:    db ")", 10, 0

section .text

;; ============================================================
;; receipt_why_miss()
;; Explain the most recent MISS - what was predicted vs actual
;; The answer to "why did it fail?"
;; ============================================================
global receipt_why_miss
receipt_why_miss:
    push rbx
    push r12
    push r13

    mov rbx, SURFACE_BASE

    ; Scan working buffer backwards for most recent MISS
    mov r12d, [rbx + STATE_OFFSET + ST_RECEIPT_WORK_POS]
    mov r13d, ST_RECEIPT_WORK_CAP   ; max iterations

.why_scan:
    test r13d, r13d
    jz .why_not_found

    ; Move backwards (wrap)
    dec r12d
    and r12d, (ST_RECEIPT_WORK_CAP - 1)

    ; Get entry
    imul eax, r12d, 64
    lea rdi, [rbx + STATE_OFFSET + ST_RECEIPT_WORKING]
    add rdi, rax

    ; Check if MISS event
    movzx eax, word [rdi + 8]
    cmp eax, EVENT_MISS
    je .why_found

    dec r13d
    jmp .why_scan

.why_found:
    ; rdi points to the MISS entry
    push rdi

    ; Print header
    lea rdi, [rel why_hdr]
    call print_cstr

    ; Context
    lea rdi, [rel why_ctx]
    call print_cstr
    pop rdi
    push rdi
    mov edi, [rdi + 12]           ; ctx_hash
    call print_hex32
    call print_newline

    ; Actual token
    lea rdi, [rel why_actual]
    call print_cstr
    pop rdi
    push rdi
    mov edi, [rdi + 16]           ; actual_token
    call print_hex32
    call print_newline

    ; Predicted token (THE KEY!)
    lea rdi, [rel why_predicted]
    call print_cstr
    pop rdi
    push rdi
    mov edi, [rdi + 20]           ; predicted_token
    call print_hex32
    call print_newline

    ; Region hash
    lea rdi, [rel why_region]
    call print_cstr
    pop rdi
    push rdi
    mov edi, [rdi + 36]           ; region_hash
    call print_hex32
    call print_newline

    ; Runner-up (aux)
    lea rdi, [rel why_runner]
    call print_cstr
    pop rdi
    push rdi
    mov edi, [rdi + 40]           ; aux (runner-up)
    call print_hex32
    call print_newline

    ; Confidence
    lea rdi, [rel why_conf]
    call print_cstr
    pop rdi
    movss xmm0, [rdi + 24]        ; confidence
    call print_f32
    call print_newline

    jmp .why_done

.why_not_found:
    lea rdi, [rel why_none]
    call print_cstr

.why_done:
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; receipt_show_misses(count)
;; Show last N misses with predicted vs actual
;; edi = count
;; ============================================================
global receipt_show_misses
receipt_show_misses:
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, SURFACE_BASE
    mov r14d, edi                 ; requested count
    xor r15d, r15d                ; found count

    ; Print header
    lea rdi, [rel misses_hdr]
    call print_cstr
    mov edi, r14d
    call print_u64
    lea rdi, [rel misses_mid]
    call print_cstr

    ; Scan working buffer backwards
    mov r12d, [rbx + STATE_OFFSET + ST_RECEIPT_WORK_POS]
    mov r13d, ST_RECEIPT_WORK_CAP   ; max iterations

.misses_scan:
    test r13d, r13d
    jz .misses_done_scan
    cmp r15d, r14d                ; found enough?
    jge .misses_done_scan

    ; Move backwards (wrap)
    dec r12d
    and r12d, (ST_RECEIPT_WORK_CAP - 1)

    ; Get entry
    imul eax, r12d, 64
    lea rdi, [rbx + STATE_OFFSET + ST_RECEIPT_WORKING]
    add rdi, rax

    ; Check if MISS event
    movzx eax, word [rdi + 8]
    cmp eax, EVENT_MISS
    jne .misses_next

    ; Found a MISS - print it
    push rdi
    push r12
    push r13

    ; Print: "  ctx -> predicted 0x... (actual: 0x...)"
    lea rdi, [rel miss_line]
    call print_cstr

    ; Context
    pop r13
    pop r12
    pop rdi
    push rdi
    push r12
    push r13
    mov edi, [rdi + 12]           ; ctx_hash
    call print_hex32

    lea rdi, [rel arrow_str]
    call print_cstr

    ; Predicted
    pop r13
    pop r12
    pop rdi
    push rdi
    push r12
    push r13
    mov edi, [rdi + 20]           ; predicted
    call print_hex32

    lea rdi, [rel actual_str]
    call print_cstr

    ; Actual
    pop r13
    pop r12
    pop rdi
    push rdi
    push r12
    push r13
    mov edi, [rdi + 16]           ; actual
    call print_hex32

    lea rdi, [rel close_paren]
    call print_cstr

    pop r13
    pop r12
    pop rdi

    inc r15d                      ; count found

.misses_next:
    dec r13d
    jmp .misses_scan

.misses_done_scan:
    test r15d, r15d
    jnz .misses_done
    lea rdi, [rel misses_none]
    call print_cstr

.misses_done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret
