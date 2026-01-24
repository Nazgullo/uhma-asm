; vsa.asm — Pure AVX2 SIMD operations on flat float32[1024] arrays
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
    shl rdi, 12               ; * 4096
    add rdi, rbx              ; vsa_base + token_id * VSA_VEC_BYTES

    ; Fill with random bytes
    mov r13, rdi              ; save vec ptr
    mov rsi, VSA_VEC_BYTES    ; 4096 bytes
    xor edx, edx             ; flags = 0
    mov rax, SYS_GETRANDOM
    syscall

    ; Convert random u32s to floats in [-1, 1] range
    ; For each element: float = (int / 2^31) - 1.0... simplified:
    ; Just treat as random bits, then normalize the whole vector
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
;; vsa_dot(a_ptr, b_ptr) → xmm0
;; rdi=a, rsi=b
;; Returns dot product of two 1024-element f32 vectors
;; ============================================================
global vsa_dot
vsa_dot:
    vxorps ymm0, ymm0, ymm0  ; accumulator
    mov ecx, VSA_DIM / 8     ; 128 iterations of 8 floats

.dot_loop:
    vmovups ymm1, [rdi]
    vmovups ymm2, [rsi]
    vfmadd231ps ymm0, ymm1, ymm2  ; acc += a * b
    add rdi, 32
    add rsi, 32
    dec ecx
    jnz .dot_loop

    ; Horizontal sum of ymm0 (8 floats → 1)
    vextractf128 xmm1, ymm0, 1
    vaddps xmm0, xmm0, xmm1  ; 4 floats
    vhaddps xmm0, xmm0, xmm0 ; 2 floats
    vhaddps xmm0, xmm0, xmm0 ; 1 float in xmm0[0]

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
    call vsa_dot              ; dot(a, a) in xmm0
    sqrtss xmm0, xmm0        ; sqrt
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
    sub rsp, 16               ; space for intermediate results

    mov r12, rdi              ; save a
    mov r13, rsi              ; save b

    ; dot(a, b)
    call vsa_dot
    movss [rsp], xmm0        ; save dot product

    ; |a|
    mov rdi, r12
    mov rsi, r12
    call vsa_dot
    sqrtss xmm0, xmm0
    movss [rsp + 4], xmm0    ; save |a|

    ; |b|
    mov rdi, r13
    mov rsi, r13
    call vsa_dot
    sqrtss xmm0, xmm0        ; |b| in xmm0

    ; result = dot / (|a| * |b|)
    movss xmm1, [rsp + 4]    ; |a|
    mulss xmm0, xmm1         ; |a| * |b|

    ; Check for zero magnitude
    xorps xmm2, xmm2
    comiss xmm0, xmm2
    je .zero_mag

    movss xmm1, [rsp]        ; dot product
    divss xmm1, xmm0
    movaps xmm0, xmm1
    jmp .cosim_done

.zero_mag:
    xorps xmm0, xmm0         ; return 0 if zero magnitude

.cosim_done:
    add rsp, 16
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
    mov ecx, VSA_DIM / 8     ; 128 iterations

.bind_loop:
    vmovups ymm0, [rdi]
    vmovups ymm1, [rsi]
    vmulps ymm2, ymm0, ymm1  ; element-wise multiply
    vmovups [rdx], ymm2
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
    mov ecx, VSA_DIM / 8

.super_loop:
    vmovups ymm0, [rdi]
    vmovups ymm1, [rsi]
    vaddps ymm0, ymm0, ymm1
    vmovups [rdi], ymm0
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
    call vsa_dot              ; dot(v,v) in xmm0
    sqrtss xmm0, xmm0        ; magnitude

    ; Check for zero
    xorps xmm1, xmm1
    comiss xmm0, xmm1
    je .norm_zero

    ; Compute 1/magnitude and broadcast
    mov eax, 0x3F800000       ; 1.0f
    movd xmm1, eax
    divss xmm1, xmm0         ; 1/mag
    vbroadcastss ymm2, xmm1  ; broadcast scalar

    ; Scale all elements
    mov rdi, rbx
    mov ecx, VSA_DIM / 8
.norm_loop:
    vmovups ymm0, [rdi]
    vmulps ymm0, ymm0, ymm2
    vmovups [rdi], ymm0
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
    ; dst[i] = src[src_idx]
    mov edx, [rbx + rax * 4]
    mov [r12 + rcx * 4], edx

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
    vxorps ymm0, ymm0, ymm0
    mov ecx, VSA_DIM / 8
.zero_loop:
    vmovups [rdi], ymm0
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
    vbroadcastss ymm1, xmm0
    mov ecx, VSA_DIM / 8
.scale_loop:
    vmovups ymm0, [rdi]
    vmulps ymm0, ymm0, ymm1
    vmovups [rdi], ymm0
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
    shl rax, 12               ; * 4096 (VSA_VEC_BYTES)
    mov rcx, SURFACE_BASE + VSA_OFFSET
    add rax, rcx
    ret

;; ============================================================
;; vsa_encode_context(token_ids_ptr, count, out_ptr)
;; rdi=array of u32 token IDs, esi=count, rdx=output vector
;; Encodes a sequence as superposition of permuted token vectors
;; context = sum(permute(vec[token[i]], i))
;; ============================================================
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
