; vsa.asm — Pure AVX2 SIMD operations on flat float64[1024] arrays
%include "syscalls.inc"
%include "constants.inc"

section .text

extern sys_getrandom
extern get_vsa_base

;; ============================================================
;; vsa_init_random
;; Initialize VSA arena with random vectors for common tokens
;; Uses getrandom syscall, then normalizes each vector
;; ============================================================
global vsa_init_random
vsa_init_random:
    push rbx
    push r12
    push r13

    mov rbx, SURFACE_BASE + VSA_OFFSET

    ; Initialize first 256 token vectors with random data
    xor r12d, r12d            ; token index

.init_loop:
    cmp r12d, 256
    jge .init_done

    ; Calculate vector address
    mov rdi, r12
    shl rdi, 13               ; * 8192 (VSA_VEC_BYTES for f64)
    add rdi, rbx              ; vsa_base + token_id * VSA_VEC_BYTES

    ; Fill with random bytes
    mov r13, rdi              ; save vec ptr
    mov rsi, VSA_VEC_BYTES    ; 8192 bytes
    xor edx, edx             ; flags = 0
    mov rax, SYS_GETRANDOM
    syscall

    ; Convert random u64 bits to valid f64 in [-1.0, 1.0):
    ;   1. Extract sign bit from random data
    ;   2. Force exponent to 0x3FF → value in [1.0, 2.0)
    ;   3. Subtract 1.0 → [0.0, 1.0)
    ;   4. Apply sign → [-1.0, 1.0)
    mov rdi, r13
    mov ecx, VSA_DIM
.conv_loop:
    mov rax, [rdi]            ; random u64
    mov rdx, rax
    shr rdx, 63              ; sign bit → rdx[0]
    shl rdx, 63              ; sign bit back to bit 63
    ; Force exponent = 0x3FF, keep mantissa (bits 51:0)
    mov r8, 0x000FFFFFFFFFFFFF
    and rax, r8              ; keep only mantissa
    mov r8, 0x3FF0000000000000
    or rax, r8               ; set exponent to 0x3FF → [1.0, 2.0)
    mov [rdi], rax
    movsd xmm0, [rdi]
    mov r8, 0x3FF0000000000000
    movq xmm1, r8            ; 1.0
    subsd xmm0, xmm1         ; [0.0, 1.0)
    movq xmm1, rdx           ; sign bit
    orpd xmm0, xmm1          ; apply sign → [-1.0, 1.0)
    movsd [rdi], xmm0
    add rdi, 8
    dec ecx
    jnz .conv_loop

    ; Normalize to unit length
    mov rdi, r13
    call vsa_normalize

    inc r12d
    jmp .init_loop

.init_done:
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; vsa_dot(a_ptr, b_ptr) → xmm0 (f64)
;; rdi=a, rsi=b
;; Returns dot product of two 1024-element f64 vectors
;; ============================================================
global vsa_dot
vsa_dot:
    vxorpd ymm0, ymm0, ymm0  ; accumulator (4 doubles)
    mov ecx, VSA_DIM / 4     ; 256 iterations of 4 doubles

.dot_loop:
    vmovupd ymm1, [rdi]
    vmovupd ymm2, [rsi]
    vfmadd231pd ymm0, ymm1, ymm2  ; acc += a * b
    add rdi, 32
    add rsi, 32
    dec ecx
    jnz .dot_loop

    ; Horizontal sum: 4 doubles → 1 double
    vextractf128 xmm1, ymm0, 1
    vaddpd xmm0, xmm0, xmm1  ; 2 doubles
    vhaddpd xmm0, xmm0, xmm0 ; 1 double in xmm0[0]

    vzeroupper
    ret

;; ============================================================
;; vsa_magnitude(ptr) → xmm0
;; rdi=vector ptr
;; Returns sqrt(dot(v, v))
;; ============================================================
global vsa_magnitude
vsa_magnitude:
    mov rsi, rdi              ; b = a
    call vsa_dot              ; dot(a, a) in xmm0 (f64)
    sqrtsd xmm0, xmm0        ; sqrt
    ret

;; ============================================================
;; vsa_cosim(a_ptr, b_ptr) → xmm0
;; rdi=a, rsi=b
;; Returns cosine similarity: dot(a,b) / (|a| * |b|)
;; ============================================================
global vsa_cosim
vsa_cosim:
    push rbx
    push r12
    push r13
    sub rsp, 32               ; space for intermediate results (f64)

    mov r12, rdi              ; save a
    mov r13, rsi              ; save b

    ; dot(a, b)
    call vsa_dot
    movsd [rsp], xmm0        ; save dot product (f64)

    ; |a|
    mov rdi, r12
    mov rsi, r12
    call vsa_dot
    sqrtsd xmm0, xmm0
    movsd [rsp + 8], xmm0    ; save |a| (f64)

    ; |b|
    mov rdi, r13
    mov rsi, r13
    call vsa_dot
    sqrtsd xmm0, xmm0        ; |b| in xmm0 (f64)

    ; result = dot / (|a| * |b|)
    movsd xmm1, [rsp + 8]    ; |a|
    mulsd xmm0, xmm1         ; |a| * |b|

    ; Check for zero magnitude
    xorpd xmm2, xmm2
    ucomisd xmm0, xmm2
    je .zero_mag

    movsd xmm1, [rsp]        ; dot product
    divsd xmm1, xmm0
    movapd xmm0, xmm1
    jmp .cosim_done

.zero_mag:
    xorpd xmm0, xmm0         ; return 0 if zero magnitude

.cosim_done:
    add rsp, 32
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; vsa_bind(a_ptr, b_ptr, out_ptr)
;; rdi=a, rsi=b, rdx=out
;; Circular convolution (simplified: element-wise XOR of bit patterns)
;; For HRR, this would be circular convolution via FFT.
;; We approximate with element-wise multiply for speed.
;; ============================================================
global vsa_bind
vsa_bind:
    mov ecx, VSA_DIM / 4     ; 256 iterations of 4 doubles

.bind_loop:
    vmovupd ymm0, [rdi]
    vmovupd ymm1, [rsi]
    vmulpd ymm2, ymm0, ymm1  ; element-wise multiply
    vmovupd [rdx], ymm2
    add rdi, 32
    add rsi, 32
    add rdx, 32
    dec ecx
    jnz .bind_loop

    vzeroupper
    ret

;; ============================================================
;; vsa_superpose(a_ptr, b_ptr)
;; rdi=a (modified in place: a += b), rsi=b
;; Bundling operation: vector addition
;; ============================================================
global vsa_superpose
vsa_superpose:
    mov ecx, VSA_DIM / 4     ; 256 iterations of 4 doubles

.super_loop:
    vmovupd ymm0, [rdi]
    vmovupd ymm1, [rsi]
    vaddpd ymm0, ymm0, ymm1
    vmovupd [rdi], ymm0
    add rdi, 32
    add rsi, 32
    dec ecx
    jnz .super_loop

    vzeroupper
    ret

;; ============================================================
;; vsa_normalize(ptr)
;; rdi=vector (modified in place to unit length)
;; ============================================================
global vsa_normalize
vsa_normalize:
    push rbx
    mov rbx, rdi

    ; First compute magnitude
    mov rsi, rdi
    call vsa_dot              ; dot(v,v) in xmm0 (f64)
    sqrtsd xmm0, xmm0        ; magnitude

    ; Check for zero
    xorpd xmm1, xmm1
    ucomisd xmm0, xmm1
    je .norm_zero

    ; Compute 1/magnitude and broadcast
    mov rax, 0x3FF0000000000000  ; 1.0 f64
    movq xmm1, rax
    divsd xmm1, xmm0         ; 1/mag
    vbroadcastsd ymm2, xmm1  ; broadcast scalar

    ; Scale all elements
    mov rdi, rbx
    mov ecx, VSA_DIM / 4     ; 256 iterations of 4 doubles
.norm_loop:
    vmovupd ymm0, [rdi]
    vmulpd ymm0, ymm0, ymm2
    vmovupd [rdi], ymm0
    add rdi, 32
    dec ecx
    jnz .norm_loop

.norm_zero:
    vzeroupper
    pop rbx
    ret

;; ============================================================
;; vsa_permute(src_ptr, dst_ptr, shift)
;; rdi=src, rsi=dst, edx=shift amount (elements)
;; Circular shift of vector elements
;; ============================================================
global vsa_permute
vsa_permute:
    push rbx
    push r12
    push r13

    mov rbx, rdi              ; src
    mov r12, rsi              ; dst
    mov r13d, edx             ; shift

    ; Normalize shift to [0, VSA_DIM)
    mov eax, r13d
    cdq
    mov ecx, VSA_DIM
    idiv ecx
    mov r13d, edx             ; shift mod DIM
    test r13d, r13d
    jns .pos_shift
    add r13d, VSA_DIM
.pos_shift:

    ; Copy with circular shift
    ; dst[i] = src[(i + shift) % DIM]
    xor ecx, ecx             ; i = 0
.perm_loop:
    cmp ecx, VSA_DIM
    jge .perm_done

    ; src_idx = (i + shift) % DIM
    mov eax, ecx
    add eax, r13d
    cmp eax, VSA_DIM
    jl .no_wrap
    sub eax, VSA_DIM
.no_wrap:
    ; dst[i] = src[src_idx] (8 bytes per f64 element)
    mov rdx, [rbx + rax * 8]
    mov [r12 + rcx * 8], rdx

    inc ecx
    jmp .perm_loop

.perm_done:
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; vsa_zero(ptr)
;; rdi=vector to zero out
;; ============================================================
global vsa_zero
vsa_zero:
    vxorpd ymm0, ymm0, ymm0
    mov ecx, VSA_DIM / 4     ; 256 iterations of 4 doubles
.zero_loop:
    vmovupd [rdi], ymm0
    add rdi, 32
    dec ecx
    jnz .zero_loop
    vzeroupper
    ret

;; ============================================================
;; vsa_scale(ptr, scalar)
;; rdi=vector (modified in place), xmm0=scalar
;; ============================================================
global vsa_scale
vsa_scale:
    vbroadcastsd ymm1, xmm0  ; broadcast f64 scalar
    mov ecx, VSA_DIM / 4     ; 256 iterations of 4 doubles
.scale_loop:
    vmovupd ymm0, [rdi]
    vmulpd ymm0, ymm0, ymm1
    vmovupd [rdi], ymm0
    add rdi, 32
    dec ecx
    jnz .scale_loop
    vzeroupper
    ret

;; ============================================================
;; vsa_get_token_vec(token_id) → rax (ptr to vector)
;; edi=token_id
;; Returns pointer to the token's vector in VSA arena
;; ============================================================
global vsa_get_token_vec
vsa_get_token_vec:
    mov eax, edi
    shl rax, 13               ; * 8192 (VSA_VEC_BYTES for f64)
    mov rcx, SURFACE_BASE + VSA_OFFSET
    add rax, rcx
    ret

;; ============================================================
;; vsa_encode_context(token_ids_ptr, count, out_ptr)
;; rdi=array of u32 token IDs, esi=count, rdx=output vector
;; Encodes a sequence as superposition of permuted token vectors
;; context = sum(permute(vec[token[i]], i))
;; ============================================================
extern print_cstr

;; ============================================================
;; Holographic Memory Section — ALL f64 (double precision)
;; Vectors are f64[1024] = 8192 bytes each.
;; Traces, binding, superposition, decay — all f64.
;; ============================================================

section .data
    align 8
    holo_learn_rate:  dq 0.1
    holo_decay_val:   dq 0.9995
    holo_threshold:   dq 0.15
    holo_one:         dq 1.0

section .text

;; ============================================================
;; holo_dot_f64(a, b) → xmm0 (f64 scalar)
;; rdi=a (f64[1024]), rsi=b (f64[1024])
;; AVX2 fused multiply-add dot product in double precision.
;; ============================================================
global holo_dot_f64
holo_dot_f64:
    vxorpd ymm0, ymm0, ymm0  ; accumulator (4 doubles)
    mov ecx, HOLO_DIM / 4    ; 256 iterations of 4 doubles

.hdot_loop:
    vmovupd ymm1, [rdi]
    vmovupd ymm2, [rsi]
    vfmadd231pd ymm0, ymm1, ymm2  ; acc += a * b
    add rdi, 32
    add rsi, 32
    dec ecx
    jnz .hdot_loop

    ; Horizontal sum: 4 doubles → 1 double
    vextractf128 xmm1, ymm0, 1
    vaddpd xmm0, xmm0, xmm1  ; 2 doubles
    vhaddpd xmm0, xmm0, xmm0 ; 1 double in xmm0[0]

    vzeroupper
    ret

;; ============================================================
;; holo_bind_f64(a, b, out)
;; rdi=a, rsi=b, rdx=out — all f64[1024]
;; Element-wise multiply (binding operation) in f64.
;; ============================================================
global holo_bind_f64
holo_bind_f64:
    mov ecx, HOLO_DIM / 4    ; 256 iterations

.hbind_loop:
    vmovupd ymm0, [rdi]
    vmovupd ymm1, [rsi]
    vmulpd ymm2, ymm0, ymm1
    vmovupd [rdx], ymm2
    add rdi, 32
    add rsi, 32
    add rdx, 32
    dec ecx
    jnz .hbind_loop

    vzeroupper
    ret

;; ============================================================
;; holo_superpose_f64(a, b)
;; rdi=a (modified: a += b), rsi=b — f64[1024]
;; Vector addition (bundling) in f64.
;; ============================================================
global holo_superpose_f64
holo_superpose_f64:
    mov ecx, HOLO_DIM / 4

.hsuper_loop:
    vmovupd ymm0, [rdi]
    vmovupd ymm1, [rsi]
    vaddpd ymm0, ymm0, ymm1
    vmovupd [rdi], ymm0
    add rdi, 32
    add rsi, 32
    dec ecx
    jnz .hsuper_loop

    vzeroupper
    ret

;; ============================================================
;; holo_scale_f64(vec, scalar)
;; rdi=vec (f64[1024], modified in place), xmm0=scalar (f64)
;; ============================================================
global holo_scale_f64
holo_scale_f64:
    vbroadcastsd ymm1, xmm0
    mov ecx, HOLO_DIM / 4

.hscale_loop:
    vmovupd ymm0, [rdi]
    vmulpd ymm0, ymm0, ymm1
    vmovupd [rdi], ymm0
    add rdi, 32
    dec ecx
    jnz .hscale_loop

    vzeroupper
    ret

;; ============================================================
;; holo_normalize_f64(vec)
;; rdi=vec (f64[1024], modified to unit length)
;; ============================================================
global holo_normalize_f64
holo_normalize_f64:
    push rbx
    mov rbx, rdi

    ; dot(v, v)
    mov rsi, rdi
    call holo_dot_f64         ; xmm0 = dot product (f64)
    sqrtsd xmm0, xmm0        ; magnitude

    ; Check for zero
    xorpd xmm1, xmm1
    ucomisd xmm0, xmm1
    je .hnorm_zero

    ; 1.0 / magnitude
    movsd xmm1, [rel holo_one]
    divsd xmm1, xmm0
    movapd xmm0, xmm1        ; scalar = 1/mag

    mov rdi, rbx
    call holo_scale_f64

.hnorm_zero:
    vzeroupper
    pop rbx
    ret

;; ============================================================
;; holo_magnitude_f64(vec) → xmm0 (f64)
;; rdi=vec (f64[1024])
;; Returns sqrt(dot(v, v)) without modifying the vector.
;; ============================================================
global holo_magnitude_f64
holo_magnitude_f64:
    mov rsi, rdi
    call holo_dot_f64
    sqrtsd xmm0, xmm0
    ret

;; ============================================================
;; holo_gen_vec(hash, out_ptr)
;; edi=hash (u32), rsi=output vector ptr (f64[1024])
;; Generates a unique 1024-dim f64 vector from a 32-bit hash:
;;   basis_idx = hash & 0xFF → one of 256 orthogonal basis vectors
;;   shift = (hash >> 8) & 0x3FF → circular rotation
;;   Reads f64 basis directly and permutes into output.
;; ============================================================
global holo_gen_vec
holo_gen_vec:
    push rbx
    push r12
    push r13
    push r14

    mov r12d, edi             ; hash
    mov r13, rsi              ; output ptr (f64[1024])

    ; basis_idx = hash & 0xFF
    movzx eax, r12b
    shl rax, 13               ; * 8192 (VSA_VEC_BYTES for f64 basis)
    mov rbx, SURFACE_BASE + VSA_OFFSET
    add rbx, rax              ; rbx = f64 basis vector ptr

    ; shift = (hash >> 8) & 0x3FF
    mov r14d, r12d
    shr r14d, 8
    and r14d, 0x3FF           ; shift amount (0-1023)

    ; Permute: out[i] = basis[(i + shift) % 1024] (all f64)
    xor ecx, ecx             ; i = 0
.gen_loop:
    cmp ecx, HOLO_DIM
    jge .gen_done

    ; src_idx = (i + shift) % 1024
    mov eax, ecx
    add eax, r14d
    cmp eax, HOLO_DIM
    jl .gen_no_wrap
    sub eax, HOLO_DIM
.gen_no_wrap:
    ; Load f64 basis[src_idx], store to out[i]
    mov rdx, [rbx + rax * 8]         ; f64 load (as u64)
    mov [r13 + rcx * 8], rdx         ; f64 store

    inc ecx
    jmp .gen_loop

.gen_done:
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; holo_store(ctx_hash, token_id, strength)
;; edi=ctx_hash, esi=token_id, xmm0=strength (f64)
;; Binds context and token f64 vectors, scales by strength,
;; and superposes into the appropriate f64 trace.
;; ============================================================
global holo_store
holo_store:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, HOLO_VEC_BYTES * 3 + 16  ; 3 temp f64 vectors + strength storage

    mov r12d, edi             ; ctx_hash
    mov r13d, esi             ; token_id
    ; Save strength (f64) in the padding area
    movsd [rsp + HOLO_VEC_BYTES * 3], xmm0

    ; 1. ctx_vec = holo_gen_vec(ctx_hash) → f64[1024]
    mov edi, r12d
    lea rsi, [rsp]            ; ctx_vec at [rsp]
    call holo_gen_vec

    ; 2. tok_vec = holo_gen_vec(token_id) → f64[1024]
    mov edi, r13d
    lea rsi, [rsp + HOLO_VEC_BYTES]
    call holo_gen_vec

    ; 3. bound = holo_bind_f64(ctx_vec, tok_vec)
    lea rdi, [rsp]
    lea rsi, [rsp + HOLO_VEC_BYTES]
    lea rdx, [rsp + HOLO_VEC_BYTES * 2]
    call holo_bind_f64

    ; 4. holo_scale_f64(bound, strength)
    lea rdi, [rsp + HOLO_VEC_BYTES * 2]
    movsd xmm0, [rsp + HOLO_VEC_BYTES * 3]  ; reload strength
    call holo_scale_f64

    ; 5. trace_idx = ctx_hash & 0xFFF (4096 buckets for better fidelity)
    mov eax, r12d
    and eax, 0xFFF
    ; trace_ptr = SURFACE_BASE + HOLO_OFFSET + trace_idx * HOLO_VEC_BYTES
    imul rax, rax, HOLO_VEC_BYTES
    mov rbx, SURFACE_BASE + HOLO_OFFSET
    add rbx, rax

    ; 6. holo_superpose_f64(trace, bound)
    mov rdi, rbx
    lea rsi, [rsp + HOLO_VEC_BYTES * 2]
    call holo_superpose_f64

    add rsp, HOLO_VEC_BYTES * 3 + 16
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; holo_predict(ctx_hash) → eax=best_token_id, xmm0=confidence (f64)
;; edi=ctx_hash
;; Unbinds context from trace, normalizes, scans vocabulary.
;; All operations in f64 for maximum fidelity.
;; ============================================================
global holo_predict
holo_predict:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, HOLO_VEC_BYTES * 2 + 16  ; ctx_vec, candidate, + locals
    ; locals at [rsp + HOLO_VEC_BYTES*2]:
    ;   [+0] = best_token (u32)
    ;   [+4] = pad
    ;   [+8] = best_sim (f64)

    mov r12d, edi             ; ctx_hash

    ; 1. ctx_vec = holo_gen_vec(ctx_hash) → f64[1024]
    mov edi, r12d
    lea rsi, [rsp]
    call holo_gen_vec

    ; 2. trace_idx = ctx_hash & 0xFFF (4096 buckets for better fidelity)
    mov eax, r12d
    and eax, 0xFFF
    imul rax, rax, HOLO_VEC_BYTES
    mov r14, SURFACE_BASE + HOLO_OFFSET
    add r14, rax              ; r14 = trace ptr (f64[1024])

    ; 3. candidate = holo_bind_f64(trace, ctx_vec) — unbind
    mov rdi, r14
    lea rsi, [rsp]
    lea rdx, [rsp + HOLO_VEC_BYTES]
    call holo_bind_f64

    ; Normalize candidate for clean cosine matching
    lea rdi, [rsp + HOLO_VEC_BYTES]
    call holo_normalize_f64

    ; Initialize best tracking
    lea rbx, [rsp + HOLO_VEC_BYTES * 2]
    mov dword [rbx], 0               ; best_token = 0
    xorpd xmm0, xmm0
    movsd [rbx + 8], xmm0            ; best_sim = 0.0 (f64)

    ; 4. Scan vocabulary
    mov r15, SURFACE_BASE
    mov r13d, [r15 + STATE_OFFSET + ST_VOCAB_COUNT]
    test r13d, r13d
    jz .holo_pred_done

    ; Cap scan at VOCAB_MAX_SCAN (256)
    cmp r13d, VOCAB_MAX_SCAN
    jle .scan_count_ok
    mov r13d, VOCAB_MAX_SCAN
.scan_count_ok:

    ; r15 = vocab base ptr
    mov r15, SURFACE_BASE + VOCAB_OFFSET
    xor ecx, ecx             ; vocab scan index

.holo_scan_loop:
    cmp ecx, r13d
    jge .holo_pred_done
    push rcx

    ; Load token_id from vocab entry
    imul rax, rcx, VOCAB_ENTRY_SIZE
    add rax, r15
    mov edi, [rax]            ; token_id

    ; Generate token vector — reuse ctx_vec space (candidate already computed)
    lea rsi, [rsp + 8]        ; +8 for pushed rcx
    call holo_gen_vec

    ; dot_f64(candidate, tok_vec)
    lea rdi, [rsp + HOLO_VEC_BYTES + 8]   ; candidate (+8 for pushed rcx)
    lea rsi, [rsp + 8]                     ; tok_vec (+8 for pushed rcx)
    call holo_dot_f64
    ; xmm0 = similarity (f64)

    ; Compare with best (f64)
    lea rbx, [rsp + HOLO_VEC_BYTES * 2 + 8]  ; +8 for pushed rcx
    ucomisd xmm0, [rbx + 8]
    jbe .holo_not_better

    ; New best
    movsd [rbx + 8], xmm0            ; best_sim = sim (f64)
    pop rcx
    push rcx
    ; Reload token_id
    imul rax, rcx, VOCAB_ENTRY_SIZE
    add rax, r15
    mov eax, [rax]
    mov [rbx], eax                    ; best_token = token_id

.holo_not_better:
    pop rcx
    inc ecx
    jmp .holo_scan_loop

.holo_pred_done:
    ; Return best_token in eax, confidence in xmm0 (f64)
    lea rbx, [rsp + HOLO_VEC_BYTES * 2]
    mov eax, [rbx]            ; best_token
    movsd xmm0, [rbx + 8]    ; best_sim (f64 confidence)

    add rsp, HOLO_VEC_BYTES * 2 + 16
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; holo_decay_all()
;; Scales all f64 trace vectors by HOLO_DECAY (0.9995)
;; Inlined loop: broadcasts decay, walks all traces.
;; ============================================================
global holo_decay_all
holo_decay_all:
    push rbx

    mov rbx, SURFACE_BASE + HOLO_OFFSET

    ; Broadcast decay constant to all 4 lanes
    movsd xmm0, [rel holo_decay_val]
    vbroadcastsd ymm2, xmm0  ; ymm2 = [0.9995, 0.9995, 0.9995, 0.9995]

    ; Total elements = HOLO_TRACES * HOLO_DIM = 256 * 1024 = 262144 doubles
    ; Process 4 at a time = 65536 iterations
    mov ecx, (HOLO_TRACES * HOLO_DIM) / 4

.decay_all_loop:
    vmovupd ymm0, [rbx]
    vmulpd ymm0, ymm0, ymm2
    vmovupd [rbx], ymm0
    add rbx, 32
    dec ecx
    jnz .decay_all_loop

    vzeroupper
    pop rbx
    ret

;; ============================================================
;; vocab_register(token_id)
;; edi=token_id
;; Appends token to vocabulary or increments its count.
;; ============================================================
global vocab_register
vocab_register:
    push rbx
    push r12
    push r13

    mov r12d, edi             ; token_id
    mov rbx, SURFACE_BASE
    mov r13, SURFACE_BASE + VOCAB_OFFSET

    ; Get current vocab count
    mov ecx, [rbx + STATE_OFFSET + ST_VOCAB_COUNT]

    ; Search for existing entry
    xor edx, edx
.vocab_search:
    cmp edx, ecx
    jge .vocab_append

    imul rax, rdx, VOCAB_ENTRY_SIZE
    add rax, r13
    cmp [rax], r12d           ; compare token_id
    je .vocab_found

    inc edx
    jmp .vocab_search

.vocab_found:
    ; Increment count
    inc dword [rax + 4]
    jmp .vocab_done

.vocab_append:
    ; Append new entry
    imul rax, rcx, VOCAB_ENTRY_SIZE
    add rax, r13
    mov [rax], r12d           ; token_id
    mov dword [rax + 4], 1    ; count = 1

    ; Increment vocab count
    inc dword [rbx + STATE_OFFSET + ST_VOCAB_COUNT]

.vocab_done:
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; vocab_count() → eax
;; Returns current vocabulary size.
;; ============================================================
global vocab_count
vocab_count:
    mov rax, SURFACE_BASE
    mov eax, [rax + STATE_OFFSET + ST_VOCAB_COUNT]
    ret

;; ============================================================
;; End Holographic Memory Section
;; ============================================================

;; ============================================================
;; Topological Metacognition: Confidence Vector Operations
;; The system "feels" differently about different contexts.
;; ============================================================

section .data
    align 8
    conf_hit_weight:  dq 0.3      ; positive weight on hit
    conf_miss_weight: dq -0.3     ; negative weight on miss
    conf_decay:       dq 0.998    ; slow decay per step
    conf_anxious:     dq -0.2     ; below this = anxious about context
    conf_confident:   dq 0.2      ; above this = confident about context
    conf_update_count: dq 0       ; update counter for observability

section .text

;; ============================================================
;; confidence_update(ctx_hash, is_hit)
;; edi=ctx_hash, esi=is_hit (1=hit, 0=miss)
;; Superposes context vector into confidence vector with +/- weight.
;; Hit → system becomes more confident about this context type.
;; Miss → system becomes more anxious about this context type.
;; ============================================================
global confidence_update
confidence_update:
    push rbx
    push r12
    push r13
    sub rsp, HOLO_VEC_BYTES + 16  ; temp ctx_vec + weight storage

    ; Debug: increment update counter
    inc qword [rel conf_update_count]

    mov r12d, edi             ; ctx_hash
    mov r13d, esi             ; is_hit

    ; 1. Generate context vector
    mov edi, r12d
    lea rsi, [rsp]            ; ctx_vec at [rsp]
    call holo_gen_vec

    ; 2. Select weight based on hit/miss
    test r13d, r13d
    jz .use_miss_weight
    movsd xmm0, [rel conf_hit_weight]
    jmp .scale_vec
.use_miss_weight:
    movsd xmm0, [rel conf_miss_weight]

.scale_vec:
    ; 3. Scale the context vector by weight
    lea rdi, [rsp]
    call holo_scale_f64

    ; 4. Superpose into confidence vector
    mov rdi, SURFACE_BASE + CONFIDENCE_VEC_OFFSET
    lea rsi, [rsp]            ; scaled ctx_vec
    call holo_superpose_f64

    add rsp, HOLO_VEC_BYTES + 16
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; confidence_query(ctx_hash) → xmm0 (f64 confidence score)
;; edi=ctx_hash
;; Returns dot product of context vector and confidence vector.
;; Positive = confident about this context type.
;; Negative = anxious about this context type.
;; ============================================================
global confidence_query
confidence_query:
    push rbx
    push r12
    sub rsp, HOLO_VEC_BYTES   ; temp ctx_vec

    mov r12d, edi             ; ctx_hash

    ; 1. Generate context vector
    mov edi, r12d
    lea rsi, [rsp]
    call holo_gen_vec

    ; 2. Normalize context vector for clean comparison
    lea rdi, [rsp]
    call holo_normalize_f64

    ; 3. Dot product with confidence vector
    lea rdi, [rsp]
    mov rsi, SURFACE_BASE + CONFIDENCE_VEC_OFFSET
    call holo_dot_f64
    ; xmm0 = confidence score

    add rsp, HOLO_VEC_BYTES
    pop r12
    pop rbx
    ret

;; ============================================================
;; confidence_decay_all()
;; Slowly decays the confidence vector toward neutral (0).
;; Called periodically to prevent runaway confidence/anxiety.
;; ============================================================
global confidence_decay_all
confidence_decay_all:
    push rbx

    mov rdi, SURFACE_BASE + CONFIDENCE_VEC_OFFSET
    movsd xmm0, [rel conf_decay]
    call holo_scale_f64

    pop rbx
    ret

;; ============================================================
;; confidence_get_update_count() → rax (u64)
;; Returns the number of times confidence_update was called.
;; ============================================================
global confidence_get_update_count
confidence_get_update_count:
    mov rax, [rel conf_update_count]
    ret

;; ============================================================
;; confidence_get_feeling(ctx_hash) → eax (0=neutral, 1=confident, 2=anxious)
;; edi=ctx_hash
;; Returns the system's "feeling" about this context type.
;; Used to select dispatch mode: anxious → deliberate, confident → fast.
;; ============================================================
global confidence_get_feeling
confidence_get_feeling:
    push rbx

    call confidence_query     ; xmm0 = confidence score

    ; Check if confident (> +0.25)
    movsd xmm1, [rel conf_confident]
    ucomisd xmm0, xmm1
    ja .feeling_confident

    ; Check if anxious (< -0.25)
    movsd xmm1, [rel conf_anxious]
    ucomisd xmm0, xmm1
    jb .feeling_anxious

    ; Neutral
    xor eax, eax
    jmp .feeling_done

.feeling_confident:
    mov eax, 1
    jmp .feeling_done

.feeling_anxious:
    mov eax, 2

.feeling_done:
    pop rbx
    ret

;; ============================================================
;; Resonant Dispatch: VSA-based fuzzy matching
;; ============================================================

section .data
    align 8
    resonant_threshold: dq 0.7    ; similarity required for fuzzy match

section .text

;; ============================================================
;; resonant_match(region_ptr, ctx_hash) -> xmm0 (similarity 0.0-1.0)
;; rdi=region header ptr (must be RTYPE_RESONANT)
;; esi=current context hash (32-bit)
;;
;; Generates context vectors from both hashes and computes
;; cosine similarity. Returns 0.0 for non-resonant regions.
;;
;; Resonant region code layout:
;;   [RHDR_SIZE+0]:  B8 xx xx xx xx  (mov eax, expected_ctx_hash)
;;   [RHDR_SIZE+5]:  B8 xx xx xx xx  (mov eax, predicted_token)
;;   [RHDR_SIZE+10]: C3              (ret)
;; The expected_ctx_hash is extracted from byte offset 1-4.
;; ============================================================
global resonant_match
resonant_match:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, HOLO_VEC_BYTES * 2   ; space for two context vectors

    mov r12, rdi              ; region_ptr
    mov r13d, esi             ; incoming ctx_hash

    ; Extract expected_ctx_hash from region code
    ; Resonant regions have: MOV EAX, imm32 at RHDR_SIZE
    ; Check opcode is 0xB8 (mov eax, imm32)
    cmp byte [r12 + RHDR_SIZE], 0xB8
    jne .not_resonant

    ; Extract the expected context hash (imm32 at offset 1)
    mov r14d, [r12 + RHDR_SIZE + 1]

    ; Generate vector for expected context -> stack[0]
    mov edi, r14d
    lea rsi, [rsp]
    call holo_gen_vec

    ; Generate vector for incoming context -> stack[HOLO_VEC_BYTES]
    mov edi, r13d
    lea rsi, [rsp + HOLO_VEC_BYTES]
    call holo_gen_vec

    ; Normalize both vectors for proper cosine similarity
    lea rdi, [rsp]
    call holo_normalize_f64

    lea rdi, [rsp + HOLO_VEC_BYTES]
    call holo_normalize_f64

    ; Compute dot product (cosine similarity of unit vectors)
    lea rdi, [rsp]
    lea rsi, [rsp + HOLO_VEC_BYTES]
    call holo_dot_f64
    ; xmm0 = similarity score [-1.0, 1.0]

    ; Clamp to [0.0, 1.0] (negative similarity = no match)
    xorpd xmm1, xmm1
    maxsd xmm0, xmm1          ; max(sim, 0.0)

    jmp .resonant_done

.not_resonant:
    ; Return 0.0 for non-resonant regions
    xorpd xmm0, xmm0

.resonant_done:
    add rsp, HOLO_VEC_BYTES * 2
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; resonant_get_threshold() -> xmm0 (f64 threshold)
;; Returns the resonant matching threshold (0.7)
;; ============================================================
global resonant_get_threshold
resonant_get_threshold:
    movsd xmm0, [rel resonant_threshold]
    ret

;; ============================================================
;; holo_query_valence(ctx_hash) -> xmm0 (valence f64)
;; edi=ctx_hash
;; Queries the valence channel of the holographic trace
;; associated with this context. Returns the "felt sense"
;; of memories stored under this context — positive if
;; learned during reward, negative if learned during cost.
;; ============================================================
global holo_query_valence
holo_query_valence:
    push rbx
    sub rsp, HOLO_VEC_BYTES   ; temp ctx_vec

    mov ebx, edi              ; save ctx_hash

    ; Generate context vector
    mov edi, ebx
    lea rsi, [rsp]
    call holo_gen_vec

    ; Get trace pointer
    mov eax, ebx
    and eax, 0xFFF            ; trace_idx = ctx_hash & 0xFFF (4096 buckets)
    imul rax, rax, HOLO_VEC_BYTES
    mov rdi, SURFACE_BASE + HOLO_OFFSET
    add rdi, rax              ; trace ptr

    ; Unbind: trace * ctx_vec (element-wise multiply)
    ; But for valence, we just need the valence channel dot product
    ; Simpler: extract valence from trace directly (unbinding doesn't affect it much)
    movsd xmm0, [rdi + VSA_VALENCE_OFFSET]

    add rsp, HOLO_VEC_BYTES
    pop rbx
    ret

;; ============================================================
;; resonant_extract_token(region_ptr) -> eax (predicted token)
;; rdi=region header ptr (must be RTYPE_RESONANT)
;; Extracts the predicted token from a resonant region.
;; Returns 0 if not a valid resonant region.
;;
;; Layout:
;;   [RHDR_SIZE+0]:  B8 ctx_hash      (mov eax, expected_ctx)
;;   [RHDR_SIZE+5]:  B8 token_id      (mov eax, predicted_token)
;;   [RHDR_SIZE+10]: C3               (ret)
;; ============================================================
global resonant_extract_token
resonant_extract_token:
    ; Check first instruction is MOV EAX, imm32
    cmp byte [rdi + RHDR_SIZE], 0xB8
    jne .invalid
    ; Check second instruction is also MOV EAX, imm32
    cmp byte [rdi + RHDR_SIZE + 5], 0xB8
    jne .invalid

    ; Extract token from second MOV instruction
    mov eax, [rdi + RHDR_SIZE + 6]
    ret

.invalid:
    xor eax, eax
    ret

;; ============================================================
;; Somatic Grounding: Valence Channel Operations
;; The last element (index 1023) of each vector encodes valence.
;; Patterns learned with energy gain get positive valence;
;; patterns learned with energy loss get negative valence.
;; This gives memories a "felt sense" — emotional color.
;; ============================================================

;; ============================================================
;; vsa_extract_valence(vec) → xmm0 (f64 valence)
;; rdi=vector ptr (f64[1024])
;; Returns the valence channel value (last element)
;; ============================================================
global vsa_extract_valence
vsa_extract_valence:
    movsd xmm0, [rdi + VSA_VALENCE_OFFSET]
    ret

;; ============================================================
;; vsa_set_valence(vec, value)
;; rdi=vector ptr (f64[1024]), xmm0=valence value (f64)
;; Sets the valence channel (last element) to the given value
;; Clamps to [-1.0, +1.0] range
;; ============================================================
global vsa_set_valence
vsa_set_valence:
    ; Clamp valence to [-1.0, +1.0]
    mov rax, 0x3FF0000000000000       ; 1.0
    movq xmm1, rax
    minsd xmm0, xmm1                  ; min(val, 1.0)
    mov rax, 0xBFF0000000000000       ; -1.0
    movq xmm1, rax
    maxsd xmm0, xmm1                  ; max(val, -1.0)
    ; Store
    movsd [rdi + VSA_VALENCE_OFFSET], xmm0
    ret

;; ============================================================
;; vsa_gen_valence_vec(valence, out_ptr)
;; xmm0=valence value (f64), rdi=output vector ptr (f64[1024])
;; Generates a "valence vector" — near-zero in all dimensions
;; except the valence channel, which holds the given value.
;; This encodes emotional charge independently of content.
;; ============================================================
global vsa_gen_valence_vec
vsa_gen_valence_vec:
    push rbx
    push r12

    mov r12, rdi                      ; save output ptr
    movsd xmm1, xmm0                  ; save valence in xmm1

    ; Zero the entire vector first
    mov rdi, r12
    call vsa_zero

    ; Set the valence channel (last element)
    ; Clamp to [-1.0, +1.0]
    mov rax, 0x3FF0000000000000       ; 1.0
    movq xmm0, rax
    minsd xmm1, xmm0                  ; min(val, 1.0)
    mov rax, 0xBFF0000000000000       ; -1.0
    movq xmm0, rax
    maxsd xmm1, xmm0                  ; max(val, -1.0)
    movsd [r12 + VSA_VALENCE_OFFSET], xmm1

    pop r12
    pop rbx
    ret

;; ============================================================
;; vsa_superpose_valence(vec, valence)
;; rdi=vector ptr (f64[1024]), xmm0=valence delta (f64)
;; Adds valence delta to the existing valence channel,
;; clamping the result to [-1.0, +1.0].
;; Used during learning to accumulate emotional charge.
;; ============================================================
global vsa_superpose_valence
vsa_superpose_valence:
    ; Load existing valence
    movsd xmm1, [rdi + VSA_VALENCE_OFFSET]
    ; Add delta
    addsd xmm1, xmm0
    ; Clamp to [-1.0, +1.0]
    mov rax, 0x3FF0000000000000       ; 1.0
    movq xmm2, rax
    minsd xmm1, xmm2
    mov rax, 0xBFF0000000000000       ; -1.0
    movq xmm2, rax
    maxsd xmm1, xmm2
    ; Store
    movsd [rdi + VSA_VALENCE_OFFSET], xmm1
    ret

;; ============================================================
;; vsa_energy_to_valence(energy_delta) → xmm0 (valence)
;; xmm0=energy_delta (f64)
;; Converts energy change to valence value:
;;   positive energy → positive valence (reward)
;;   negative energy → negative valence (cost)
;; Uses tanh-like squashing to stay in [-1, +1]
;; ============================================================
global vsa_energy_to_valence
vsa_energy_to_valence:
    ; Simple linear scaling with clamp: valence = energy_delta * 0.01
    ; (More sophisticated would use tanh, but this is fast)
    mov rax, 0x3F847AE147AE147B       ; 0.01
    movq xmm1, rax
    mulsd xmm0, xmm1
    ; Clamp to [-1.0, +1.0]
    mov rax, 0x3FF0000000000000       ; 1.0
    movq xmm1, rax
    minsd xmm0, xmm1
    mov rax, 0xBFF0000000000000       ; -1.0
    movq xmm1, rax
    maxsd xmm0, xmm1
    ret

global vsa_encode_context
vsa_encode_context:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, VSA_VEC_BYTES    ; temp vector on stack

    mov r12, rdi              ; token_ids
    mov r13d, esi             ; count
    mov r14, rdx              ; output ptr

    ; Zero the output vector
    mov rdi, r14
    call vsa_zero

    ; For each token in sequence
    xor r15d, r15d            ; index
.enc_loop:
    cmp r15d, r13d
    jge .enc_done

    ; Get token vector
    mov edi, [r12 + r15 * 4]
    call vsa_get_token_vec
    mov rbx, rax              ; token vec ptr

    ; Permute by position index into temp
    mov rdi, rbx              ; src
    lea rsi, [rsp]            ; dst (temp on stack)
    mov edx, r15d             ; shift = position
    call vsa_permute

    ; Superpose into output
    mov rdi, r14              ; output (a += b)
    lea rsi, [rsp]            ; temp
    call vsa_superpose

    inc r15d
    jmp .enc_loop

.enc_done:
    ; Normalize result
    mov rdi, r14
    call vsa_normalize

    add rsp, VSA_VEC_BYTES
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret
