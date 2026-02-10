; qthm.asm — Quantum Triplet Holographic Memory
;
; @entry qthm_store(rdi=ctx_vec_ptr, esi=token_id, xmm0=strength) -> void
; @entry qthm_predict(rdi=ctx_vec_ptr) -> eax=token, xmm0=confidence, xmm1=entropy
; @entry qthm_decay() -> void
; @calls vsa.asm:holo_gen_vec, holo_bind_f64, holo_unbind_f64
; @calls vsa.asm:holo_normalize_f64, holo_scale_f64, holo_superpose_f64
; @calls vsa.asm:holo_dot_f64
; @calls quantum.asm:qdec_set_amps, qdec_measure, qdec_entropy
; @calledby dispatch.asm:dispatch_predict, dispatch.asm:process_token
; @calledby learn.asm:learn_pattern
; @calledby dreams.asm:dream_cycle
;
; FLOW: ctx_vec → unbind(trace, ctx) → top-K vocab scan → Born rule → prediction
;
; ARCHITECTURE:
;   Multi-order holographic context encodes structural similarity.
;   Triplet binding (ctx⊗token) stored via superposition into single trace.
;   Query via unbind extracts candidate pattern, vocab scan finds matches.
;   Born rule measurement over top-K candidates selects prediction.
;
; GOTCHAS:
;   - MUST normalize after bind (exponential magnitude decay otherwise)
;   - ctx_vec is f64[HOLO_DIM], NOT a hash — it's a rich positional encoding
;   - Top-K maintained on stack (128 bytes for K=8, each 16 bytes)
;   - Born rule: high confidence → deterministic; low → stochastic exploration
;   - QTHM_TRACE_IDX=242, lives in warm zone holographic traces
;
%include "syscalls.inc"
%include "constants.inc"

section .data
    align 8
    qthm_min_conf:      dq 0.05         ; minimum confidence to return prediction
    qthm_conf_thresh:   dq 0.1          ; threshold for dispatch to use prediction
    qthm_learn_rate:    dq 0.15         ; store strength on miss
    qthm_hit_reinforce: dq 0.3          ; reinforce strength on hit
    qthm_decay_factor:  dq 0.998        ; per-dream decay
    qthm_zero:          dq 0.0
    qthm_neg_one:       dq -1.0

section .text

extern holo_gen_vec
extern holo_bind_f64
extern holo_unbind_f64
extern holo_superpose_f64
extern holo_normalize_f64
extern holo_scale_f64
extern holo_dot_f64
extern qdec_set_amps
extern qdec_measure
extern qdec_entropy

;; ============================================================
;; qthm_store(ctx_vec_ptr, token_id, strength)
;; rdi=ctx_vec_ptr (f64[HOLO_DIM]), esi=token_id (u32), xmm0=strength (f64)
;;
;; Encode and store a prediction triplet into the QTHM trace:
;;   1. tok_vec = holo_gen_vec(token_id)
;;   2. bound = holo_bind_f64(ctx_vec, tok_vec)
;;   3. normalize(bound) — prevent magnitude decay
;;   4. scale(bound, strength)
;;   5. superpose(QTHM_TRACE, bound)
;; ============================================================
global qthm_store
qthm_store:
    push rbx
    push r12
    push r13
    push r14
    push r15
    ; 5 pushes (odd) → aligned. Need 2*HOLO_VEC_BYTES + 8 for strength
    ; 2*65536 + 8 = 131080, round up to multiple of 16 = 131088
    sub rsp, (HOLO_VEC_BYTES * 2 + 16)
    ; [rsp]                    = tok_vec buffer (HOLO_VEC_BYTES)
    ; [rsp + HOLO_VEC_BYTES]   = bound buffer (HOLO_VEC_BYTES)
    ; [rsp + 2*HOLO_VEC_BYTES] = strength (f64)

    mov r12, rdi              ; ctx_vec_ptr
    mov r13d, esi             ; token_id
    movsd [rsp + HOLO_VEC_BYTES * 2], xmm0  ; save strength

    ; 1. Generate token vector
    mov edi, r13d             ; token_id as seed
    mov rsi, rsp              ; output = tok_vec buffer
    call holo_gen_vec

    ; 2. Bind ctx_vec ⊗ tok_vec → bound
    mov rdi, r12              ; ctx_vec
    mov rsi, rsp              ; tok_vec
    lea rdx, [rsp + HOLO_VEC_BYTES]  ; bound output
    call holo_bind_f64

    ; 3. Normalize bound (prevent exponential magnitude decay)
    lea rdi, [rsp + HOLO_VEC_BYTES]
    call holo_normalize_f64

    ; 4. Scale by strength
    lea rdi, [rsp + HOLO_VEC_BYTES]
    movsd xmm0, [rsp + HOLO_VEC_BYTES * 2]  ; reload strength
    call holo_scale_f64

    ; 5. Superpose into QTHM trace
    mov rdi, SURFACE_BASE
    mov rax, HOLO_OFFSET
    add rdi, rax              ; SURFACE_BASE + HOLO_OFFSET
    mov rax, QTHM_TRACE_IDX
    imul rax, HOLO_VEC_BYTES
    add rdi, rax              ; trace ptr = HOLO_OFFSET + 242 * VEC_BYTES
    lea rsi, [rsp + HOLO_VEC_BYTES]  ; bound
    call holo_superpose_f64

    add rsp, (HOLO_VEC_BYTES * 2 + 16)
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; qthm_predict(ctx_vec_ptr) → eax=token, xmm0=confidence, xmm1=entropy
;; rdi=ctx_vec_ptr (f64[HOLO_DIM])
;;
;; Query the QTHM trace and select via Born rule:
;;   1. candidate = unbind(QTHM_TRACE, ctx_vec) — extract predicted pattern
;;   2. normalize(candidate)
;;   3. Scan vocab top entries: similarity = dot(candidate, tok_vec)
;;   4. Track top-K by similarity
;;   5. If top-1 < MIN_CONF: return 0 (no prediction)
;;   6. Convert top-K to Born rule amplitudes
;;   7. Measure → winner
;;   8. Return winner token, confidence, entropy
;; ============================================================
global qthm_predict
qthm_predict:
    push rbx
    push r12
    push r13
    push r14
    push r15
    ; 5 pushes (odd) → aligned
    ; Need: candidate vec + tok_vec + top-K array + amps array
    ; HOLO_VEC_BYTES + HOLO_VEC_BYTES + K*16 + K*8 = 131072 + 128 + 64 = 131264
    ; Round to multiple of 16 = 131264 (already aligned)
    %define PRED_CANDIDATE  0
    %define PRED_TOK_VEC    HOLO_VEC_BYTES
    %define PRED_TOPK       (HOLO_VEC_BYTES * 2)
    ; Top-K entry: [+0] u32 token_id, [+4] u32 pad, [+8] f64 similarity = 16 bytes
    %define PRED_TOPK_SIZE  (QTHM_TOP_K * 16)
    %define PRED_AMPS       (PRED_TOPK + PRED_TOPK_SIZE)
    %define PRED_AMPS_SIZE  (QTHM_TOP_K * 8)
    %define PRED_FRAME_SIZE (PRED_AMPS + PRED_AMPS_SIZE + 8)  ; +8 for alignment padding

    sub rsp, PRED_FRAME_SIZE

    mov r12, rdi              ; ctx_vec_ptr saved

    ; 1. Unbind: candidate = unbind(QTHM_TRACE, ctx_vec)
    mov rdi, SURFACE_BASE
    mov rax, HOLO_OFFSET
    add rdi, rax
    mov rax, QTHM_TRACE_IDX
    imul rax, HOLO_VEC_BYTES
    add rdi, rax              ; trace ptr
    mov rsi, r12              ; ctx_vec
    lea rdx, [rsp + PRED_CANDIDATE]
    call holo_unbind_f64

    ; 2. Normalize candidate
    lea rdi, [rsp + PRED_CANDIDATE]
    call holo_normalize_f64

    ; 3. Initialize top-K to empty (similarity = -1.0)
    lea rdi, [rsp + PRED_TOPK]
    xor ecx, ecx
.init_topk:
    cmp ecx, QTHM_TOP_K
    jge .topk_init_done
    imul eax, ecx, 16
    mov dword [rdi + rax], 0          ; token_id = 0
    mov dword [rdi + rax + 4], 0      ; pad
    movsd xmm0, [rel qthm_neg_one]
    movsd [rdi + rax + 8], xmm0       ; similarity = -1.0
    inc ecx
    jmp .init_topk
.topk_init_done:

    ; 4. Scan vocabulary for top-K matches
    ; Use vocab table: entries are (token_id:u32, count:u32) at VOCAB_OFFSET
    mov rbx, SURFACE_BASE
    xor r13d, r13d            ; vocab scan index
    mov r14d, VOCAB_MAX_SCAN  ; max entries to scan

.vocab_scan:
    cmp r13d, r14d
    jge .scan_done

    ; Get vocab entry: token_id at VOCAB_OFFSET + index * VOCAB_ENTRY_SIZE
    ; GOTCHA: VOCAB_OFFSET > 0x7FFFFFFF, use register to avoid sign-extend
    mov rax, rbx
    mov rcx, VOCAB_OFFSET
    add rax, rcx
    imul rcx, r13, VOCAB_ENTRY_SIZE
    mov edi, [rax + rcx]      ; token_id
    test edi, edi
    jz .scan_next             ; empty slot

    ; Check count > 0
    mov eax, [rax + rcx + 4]  ; count
    test eax, eax
    jz .scan_next

    ; Generate token vector
    push r13
    push r14
    mov r15d, edi             ; save token_id
    lea rsi, [rsp + 16 + PRED_TOK_VEC]  ; +16 for 2 pushes
    call holo_gen_vec

    ; Compute similarity: dot(candidate, tok_vec)
    lea rdi, [rsp + 16 + PRED_CANDIDATE]
    lea rsi, [rsp + 16 + PRED_TOK_VEC]
    call holo_dot_f64         ; xmm0 = similarity

    ; Check if this beats the worst in top-K (last entry, index K-1)
    lea rdi, [rsp + 16 + PRED_TOPK]
    movsd xmm1, [rdi + (QTHM_TOP_K - 1) * 16 + 8]  ; worst similarity
    ucomisd xmm0, xmm1
    jbe .scan_next_pop        ; not better than worst → skip

    ; Insert into top-K (sorted insertion)
    ; Find insertion point: first entry with lower similarity
    xor ecx, ecx
.find_insert:
    cmp ecx, QTHM_TOP_K
    jge .insert_at_end
    imul eax, ecx, 16
    ucomisd xmm0, qword [rdi + rax + 8]
    ja .found_insert          ; our sim > this entry → insert here
    inc ecx
    jmp .find_insert

.found_insert:
    ; Shift entries down from position ecx to K-2
    mov edx, QTHM_TOP_K - 1
.shift_down:
    cmp edx, ecx
    jle .do_insert
    imul eax, edx, 16
    lea r8d, [edx - 1]
    imul r8d, r8d, 16
    ; Copy entry[edx-1] → entry[edx]
    mov r9, [rdi + r8]        ; token_id + pad
    mov [rdi + rax], r9
    movsd xmm1, [rdi + r8 + 8]
    movsd [rdi + rax + 8], xmm1
    dec edx
    jmp .shift_down

.do_insert:
    ; Insert at position ecx
    imul eax, ecx, 16
    mov [rdi + rax], r15d     ; token_id
    mov dword [rdi + rax + 4], 0  ; pad
    movsd [rdi + rax + 8], xmm0   ; similarity
    jmp .scan_next_pop

.insert_at_end:
    ; Shouldn't reach here, but safety
.scan_next_pop:
    pop r14
    pop r13
.scan_next:
    inc r13d
    jmp .vocab_scan

.scan_done:
    ; 5. Check if top-1 similarity > MIN_CONF
    lea rdi, [rsp + PRED_TOPK]
    movsd xmm0, [rdi + 8]    ; top-1 similarity
    ucomisd xmm0, [rel qthm_min_conf]
    jbe .no_prediction

    ; Count valid candidates (similarity > 0)
    xor r13d, r13d            ; valid count
    xor ecx, ecx
.count_valid:
    cmp ecx, QTHM_TOP_K
    jge .count_done
    imul eax, ecx, 16
    movsd xmm1, [rdi + rax + 8]
    ucomisd xmm1, [rel qthm_zero]
    jbe .count_skip
    inc r13d
.count_skip:
    inc ecx
    jmp .count_valid
.count_done:
    cmp r13d, 1
    jl .no_prediction         ; no valid candidates

    ; 6. Convert top-K similarities to Born rule amplitudes
    ; amp[i] = max(sim[i], 0) — negative similarities become 0
    lea rdi, [rsp + PRED_TOPK]
    lea rsi, [rsp + PRED_AMPS]
    xor ecx, ecx
.build_amps:
    cmp ecx, r13d
    jge .amps_pad
    imul eax, ecx, 16
    movsd xmm0, [rdi + rax + 8]   ; similarity
    xorpd xmm1, xmm1
    maxsd xmm0, xmm1              ; max(sim, 0)
    ; sqrt for amplitude (Born rule: probability = |amplitude|^2)
    sqrtsd xmm0, xmm0
    movsd [rsi + rcx * 8], xmm0
    inc ecx
    jmp .build_amps
.amps_pad:
    ; Pad remaining slots with 0
    cmp ecx, QTHM_TOP_K
    jge .amps_done
    xorpd xmm0, xmm0
    movsd [rsi + rcx * 8], xmm0
    inc ecx
    jmp .amps_pad
.amps_done:

    ; 7. Normalize amplitudes to unit vector
    lea rdi, [rsp + PRED_AMPS]
    mov esi, r13d             ; valid count
    call qdec_set_amps

    ; Compute entropy
    lea rdi, [rsp + PRED_AMPS]
    mov esi, r13d
    call qdec_entropy         ; xmm0 = entropy
    movsd [rsp + PRED_AMPS + PRED_AMPS_SIZE], xmm0  ; save entropy in padding

    ; 8. Born rule measurement → winner index
    lea rdi, [rsp + PRED_AMPS]
    mov esi, r13d
    call qdec_measure         ; eax = winner index

    ; Look up winner token and confidence
    lea rdi, [rsp + PRED_TOPK]
    imul ecx, eax, 16
    mov eax, [rdi + rcx]      ; winner token_id
    movsd xmm0, [rdi + rcx + 8]  ; winner similarity (= confidence)
    movsd xmm1, [rsp + PRED_AMPS + PRED_AMPS_SIZE]  ; entropy

    ; Increment QTHM prediction counter
    mov rbx, SURFACE_BASE
    inc dword [rbx + STATE_OFFSET + ST_QTHM_PREDICTIONS]

    jmp .predict_exit

.no_prediction:
    xor eax, eax              ; no prediction
    xorpd xmm0, xmm0         ; confidence = 0
    xorpd xmm1, xmm1         ; entropy = 0

.predict_exit:
    add rsp, PRED_FRAME_SIZE
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; qthm_decay()
;; Called during dream consolidation. Scales QTHM trace by decay factor.
;; ============================================================
global qthm_decay
qthm_decay:
    push rbx

    ; Get QTHM trace pointer
    mov rdi, SURFACE_BASE
    mov rax, HOLO_OFFSET
    add rdi, rax
    mov rax, QTHM_TRACE_IDX
    imul rax, HOLO_VEC_BYTES
    add rdi, rax              ; trace ptr

    ; Scale by decay factor
    movsd xmm0, [rel qthm_decay_factor]
    call holo_scale_f64

    pop rbx
    ret
