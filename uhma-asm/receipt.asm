; receipt.asm — Unified Holographic Receipt Layer
;
; One memory. One architecture. One math. Fidelity as the dial.
;
; Receipts are stored in holographic memory with fidelity determining persistence:
;   fidelity=1.0 → lossless, permanent (crystallized)
;   fidelity=0.5 → decays slowly over time
;   fidelity=0.1 → blends into statistical background
;
; Storage uses the SAME VSA operations as everything else:
;   receipt_vec = bind(event_vec, bind(ctx_vec, token_vec))
;   trace[i] = trace[i] * (1 - fidelity * lr) + fidelity * lr * receipt_vec
;
; Query via resonance: dot(probe_vec, trace) → similarity
%include "syscalls.inc"
%include "constants.inc"

section .data
    ; Debug output messages
    rcpt_emit_msg:      db "  #", 0
    rcpt_event_lbl:     db " ", 0
    rcpt_ctx_lbl:       db " ctx=0x", 0
    rcpt_tok_lbl:       db " tok=0x", 0
    rcpt_fid_lbl:       db " fid=", 0
    rcpt_nl:            db 10, 0

    rcpt_dump_hdr:      db "[RECEIPTS] Working buffer (last ", 0
    rcpt_dump_mid:      db "):", 10, 0
    rcpt_resonate_hdr:  db "[RESONATE] ", 0
    rcpt_resonate_sim:  db " similarity=", 0

    listen_msg:         db "[RECEIPT] Holographic storage enabled", 10, 0

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

    ; Trace index table - maps event type to trace slot
    trace_index_table:
        dd RCPT_TRACE_HIT       ; EVENT_HIT
        dd RCPT_TRACE_MISS      ; EVENT_MISS
        dd RCPT_TRACE_NEW       ; EVENT_NEW
        dd RCPT_TRACE_LEARN     ; EVENT_LEARN
        dd RCPT_TRACE_EMIT      ; EVENT_EMIT
        dd RCPT_TRACE_COMBINED  ; EVENT_PRUNE (uses combined)
        dd RCPT_TRACE_COMBINED  ; EVENT_PROMOTE
        dd RCPT_TRACE_COMBINED  ; EVENT_DREAM
        dd RCPT_TRACE_COMBINED  ; EVENT_OBSERVE
        dd RCPT_TRACE_COMBINED  ; EVENT_EVOLVE
        dd RCPT_TRACE_COMBINED  ; EVENT_HOLO_PRED
        dd RCPT_TRACE_COMBINED  ; EVENT_GRAPH_PRED
        dd RCPT_TRACE_COMBINED  ; EVENT_JOURNEY

    ; Constants
    align 8
    one_f64:            dq 1.0
    valence_boost:      dq 0.3    ; how much |valence| boosts fidelity

section .bss
    ; Scratch vectors for receipt encoding (f64[1024] = 8KB each)
    align 64
    scratch_event_vec:  resb HOLO_VEC_BYTES
    scratch_ctx_vec:    resb HOLO_VEC_BYTES
    scratch_bound_vec:  resb HOLO_VEC_BYTES

section .text

extern print_cstr
extern print_hex32
extern print_u64
extern print_f32
extern print_f64
extern print_newline
extern holo_gen_vec
extern holo_bind_f64
extern holo_superpose_f64
extern holo_dot_f64
extern holo_magnitude_f64

;; ============================================================
;; emit_receipt(event_type, ctx, token, confidence, valence)
;; edi = event_type (u16)
;; esi = ctx_hash (u32)
;; edx = token_id (u32)
;; xmm0 = confidence (f32)
;; xmm1 = valence (f64)
;;
;; Computes fidelity, encodes receipt as VSA vector, superposes
;; to holographic trace with fidelity as learning rate.
;; ============================================================
global emit_receipt
emit_receipt:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 40             ; locals: [0]=fidelity(f64), [8]=confidence(f32)
                            ; [16]=valence(f64), [24]=event_type, [28]=ctx, [32]=token

    mov rbx, SURFACE_BASE

    ; === FAST PATH: check listener mask ===
    mov eax, [rbx + STATE_OFFSET + ST_RECEIPT_LISTENER]
    test eax, eax
    jz .fast_return

    ; Save parameters
    mov [rsp + 24], edi     ; event_type
    mov [rsp + 28], esi     ; ctx_hash
    mov [rsp + 32], edx     ; token_id
    movss [rsp + 8], xmm0   ; confidence
    movsd [rsp + 16], xmm1  ; valence

    mov r12d, edi           ; event_type
    mov r13d, esi           ; ctx_hash
    mov r14d, edx           ; token_id

    ; Increment total count
    inc qword [rbx + STATE_OFFSET + ST_RECEIPT_TOTAL]
    mov r15, [rbx + STATE_OFFSET + ST_RECEIPT_TOTAL]

    ; === COMPUTE FIDELITY ===
    ; fidelity = base_fidelity[event_type] + |valence| * valence_boost
    ; clamped to [0.0, 1.0]
    movzx eax, r12w
    cmp eax, EVENT_TYPE_COUNT
    jge .use_default_fidelity
    lea rcx, [rel fidelity_table]
    movsd xmm2, [rcx + rax * 8]   ; base fidelity
    jmp .have_base_fidelity
.use_default_fidelity:
    mov rax, 0x3FE0000000000000   ; 0.5 default
    movq xmm2, rax
.have_base_fidelity:
    ; Add |valence| * boost
    movsd xmm3, [rsp + 16]        ; valence
    ; Compute absolute value: and with sign mask
    mov rax, 0x7FFFFFFFFFFFFFFF
    movq xmm4, rax
    andpd xmm3, xmm4              ; |valence|
    mulsd xmm3, [rel valence_boost]
    addsd xmm2, xmm3              ; fidelity = base + |valence| * boost
    ; Clamp to [0, 1]
    xorpd xmm3, xmm3
    maxsd xmm2, xmm3              ; max(fidelity, 0)
    movsd xmm3, [rel one_f64]
    minsd xmm2, xmm3              ; min(fidelity, 1)
    movsd [rsp], xmm2             ; store fidelity

    ; === CHECK LISTENER_PRINT ===
    mov eax, [rbx + STATE_OFFSET + ST_RECEIPT_LISTENER]
    test eax, LISTENER_PRINT
    jz .skip_print

    ; Print receipt info
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

    ; Write to working buffer (simple 64-byte record)
    mov ecx, [rbx + STATE_OFFSET + ST_RECEIPT_WORK_POS]
    imul eax, ecx, 64
    lea rdi, [rbx + STATE_OFFSET + ST_RECEIPT_WORKING]
    add rdi, rax
    ; Store: timestamp(8), event(2), pad(2), ctx(4), token(4), conf(4), fidelity(8)...
    mov rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov [rdi + 0], rax            ; timestamp
    mov [rdi + 8], r12w           ; event_type
    mov word [rdi + 10], 0        ; padding
    mov [rdi + 12], r13d          ; ctx_hash
    mov [rdi + 16], r14d          ; token_id
    movss xmm0, [rsp + 8]
    movss [rdi + 20], xmm0        ; confidence
    movsd xmm0, [rsp]
    movsd [rdi + 24], xmm0        ; fidelity
    ; Advance position
    inc ecx
    and ecx, (ST_RECEIPT_WORK_CAP - 1)
    mov [rbx + STATE_OFFSET + ST_RECEIPT_WORK_POS], ecx

.skip_working:
    ; === CHECK LISTENER_HOLO (main holographic storage) ===
    mov eax, [rbx + STATE_OFFSET + ST_RECEIPT_LISTENER]
    test eax, LISTENER_HOLO
    jz .fast_return

    ; === ENCODE RECEIPT AS VSA VECTOR ===
    ; receipt_vec = bind(event_vec, bind(ctx_vec, token_vec))
    ; Note: holo_gen_vec(hash, out_ptr) takes edi=hash, rsi=output

    ; Generate event vector (seed from event_type)
    movzx edi, r12w
    add edi, 0x45564E54           ; "EVNT" + event_type as seed
    lea rsi, [rel scratch_event_vec]
    call holo_gen_vec

    ; Generate context vector
    mov edi, r13d                 ; ctx_hash as seed
    lea rsi, [rel scratch_ctx_vec]
    call holo_gen_vec

    ; Generate token vector into scratch_bound_vec (will be overwritten by bind)
    mov edi, r14d                 ; token_id as seed
    lea rsi, [rel scratch_bound_vec]
    call holo_gen_vec

    ; Bind ctx_vec with token_vec → scratch_bound_vec
    ; holo_bind_f64(a, b, out) - rdi=a, rsi=b, rdx=out
    lea rdi, [rel scratch_ctx_vec]
    lea rsi, [rel scratch_bound_vec]
    lea rdx, [rel scratch_bound_vec]  ; output overwrites token vec
    call holo_bind_f64

    ; Bind event_vec with (ctx⊗token) → scratch_event_vec
    lea rdi, [rel scratch_event_vec]
    lea rsi, [rel scratch_bound_vec]
    lea rdx, [rel scratch_event_vec]  ; output overwrites event vec
    call holo_bind_f64

    ; === SCALE BY FIDELITY ===
    ; Scale receipt_vec by fidelity * 0.1 before superposing
    movsd xmm0, [rsp]                 ; fidelity
    mov rax, 0x3FB999999999999A       ; 0.1
    movq xmm1, rax
    mulsd xmm0, xmm1                  ; learning_rate = fidelity * 0.1
    ; Scale each element: vec[i] *= learning_rate
    lea rdi, [rel scratch_event_vec]
    mov ecx, HOLO_DIM
.scale_loop:
    movsd xmm1, [rdi]
    mulsd xmm1, xmm0
    movsd [rdi], xmm1
    add rdi, 8
    dec ecx
    jnz .scale_loop

    ; === SUPERPOSE TO TRACE ===
    ; holo_superpose_f64(a, b) - a += b

    ; Get trace index for this event type
    movzx eax, r12w
    cmp eax, EVENT_TYPE_COUNT
    jge .use_combined_trace
    lea rcx, [rel trace_index_table]
    mov eax, [rcx + rax * 4]
    jmp .have_trace_idx
.use_combined_trace:
    mov eax, RCPT_TRACE_COMBINED
.have_trace_idx:
    ; Calculate trace address
    imul rax, rax, HOLO_VEC_BYTES
    lea rdi, [rbx + HOLO_OFFSET]
    add rdi, rax                  ; trace ptr
    lea rsi, [rel scratch_event_vec]
    call holo_superpose_f64

    ; Also superpose to combined trace (for general queries)
    movzx eax, r12w
    cmp eax, EVENT_TYPE_COUNT
    jge .fast_return              ; already wrote to combined for unknown types
    lea rcx, [rel trace_index_table]
    mov eax, [rcx + rax * 4]
    cmp eax, RCPT_TRACE_COMBINED
    je .fast_return               ; already wrote to combined

    mov eax, RCPT_TRACE_COMBINED
    imul rax, rax, HOLO_VEC_BYTES
    lea rdi, [rbx + HOLO_OFFSET]
    add rdi, rax
    lea rsi, [rel scratch_event_vec]
    call holo_superpose_f64

.fast_return:
    add rsp, 40
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

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
    jmp emit_receipt

;; ============================================================
;; receipt_resonate(event_type, ctx, token) → xmm0 (similarity f64)
;; Query holographic trace for similar past receipts
;; edi = event_type (or -1 for combined), esi = ctx, edx = token
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

    ; Generate probe vector same way as receipt encoding
    ; probe = bind(event_vec, bind(ctx_vec, token_vec))
    ; Note: holo_gen_vec(hash, out_ptr) takes edi=hash, rsi=output

    ; Event vector
    cmp r12d, -1
    je .probe_combined
    movzx edi, r12w
    add edi, 0x45564E54
    lea rsi, [rel scratch_event_vec]
    call holo_gen_vec
    jmp .probe_ctx
.probe_combined:
    ; For combined query, use neutral event vector
    mov edi, 0x434F4D42           ; "COMB"
    lea rsi, [rel scratch_event_vec]
    call holo_gen_vec

.probe_ctx:
    ; Context vector
    mov edi, r13d
    lea rsi, [rel scratch_ctx_vec]
    call holo_gen_vec

    ; Token vector
    mov edi, r14d
    lea rsi, [rel scratch_bound_vec]
    call holo_gen_vec

    ; Bind ctx ⊗ token
    lea rdi, [rel scratch_ctx_vec]
    lea rsi, [rel scratch_bound_vec]
    lea rdx, [rel scratch_bound_vec]
    call holo_bind_f64

    ; Bind event ⊗ (ctx ⊗ token)
    lea rdi, [rel scratch_event_vec]
    lea rsi, [rel scratch_bound_vec]
    lea rdx, [rel scratch_event_vec]
    call holo_bind_f64

    ; Get trace to query
    cmp r12d, -1
    je .query_combined
    movzx eax, r12w
    cmp eax, EVENT_TYPE_COUNT
    jge .query_combined
    lea rcx, [rel trace_index_table]
    mov eax, [rcx + rax * 4]
    jmp .have_query_trace
.query_combined:
    mov eax, RCPT_TRACE_COMBINED
.have_query_trace:
    imul rax, rax, HOLO_VEC_BYTES
    lea rdi, [rbx + HOLO_OFFSET]
    add rdi, rax                  ; trace ptr

    ; Compute dot product (similarity)
    lea rsi, [rel scratch_event_vec]
    call holo_dot_f64             ; → xmm0 = similarity

    add rsp, 8
    pop r14
    pop r13
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
    ; Enable holographic storage by default (the whole point!)
    mov dword [rax + STATE_OFFSET + ST_RECEIPT_LISTENER], LISTENER_HOLO
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
