; matmul.asm — AVX2 matrix multiplication for transformer inference
;
; @entry matmul_f32(A, B, C, M, K, N) -> void
;        C[M×N] = A[M×K] × B[K×N], row-major
; @entry matmul_f32_transB(A, BT, C, M, K, N) -> void
;        C[M×N] = A[M×K] × B^T[N×K], B stored transposed
; @entry matmul_f32_add_bias(C, bias, M, N) -> void
;        C[i,j] += bias[j] for all i
; @entry vec_add_f32(dst, src, n) -> void
; @entry vec_mul_f32(dst, src, n) -> void
;
; SIMD: Uses AVX2 (ymm, 8 f32/op)
;
; GOTCHAS:
;   - Assumes 32-byte alignment for best performance
;   - K dimension should be multiple of 8 for full SIMD utilization
;   - Uses ymm0-ymm15

section .data
    align 32
    ; Constants for GELU approximation
    gelu_const_a:   times 8 dd 0.044715      ; 0.044715
    gelu_const_b:   times 8 dd 0.7978845608  ; sqrt(2/pi)
    gelu_const_half: times 8 dd 0.5
    gelu_const_one: times 8 dd 1.0

section .text
global matmul_f32
global matmul_f32_transB
global matmul_f32_add_bias
global vec_add_f32
global vec_mul_f32
global vec_gelu_f32
global layernorm_f32

;; ============================================================================
;; matmul_f32 - General matrix multiplication
;; C[M×N] = A[M×K] × B[K×N]
;;
;; Args:
;;   rdi = A pointer (M × K, row-major, f32)
;;   rsi = B pointer (K × N, row-major, f32)
;;   rdx = C pointer (M × N, row-major, f32)
;;   ecx = M (rows of A and C)
;;   r8d = K (cols of A, rows of B)
;;   r9d = N (cols of B and C)
;; ============================================================================
matmul_f32:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15

    ; Save arguments
    mov r10, rdi            ; A
    mov r11, rsi            ; B
    mov r12, rdx            ; C
    mov r13d, ecx           ; M
    mov r14d, r8d           ; K
    mov r15d, r9d           ; N

    ; Clear C to zero first
    mov rdi, r12
    xor eax, eax
    mov ecx, r13d
    imul ecx, r15d          ; M * N
    shl ecx, 2              ; * 4 bytes
    rep stosb

    ; Outer loop: for each row i of A
    xor ebx, ebx            ; i = 0
.row_loop:
    cmp ebx, r13d
    jge .done

    ; Middle loop: for each column j of C (in blocks of 8)
    xor ecx, ecx            ; j = 0
.col_loop:
    cmp ecx, r15d
    jge .row_next

    ; Accumulator for C[i, j:j+8]
    vxorps ymm0, ymm0, ymm0

    ; Inner loop: dot product over K
    xor edx, edx            ; k = 0
.k_loop:
    cmp edx, r14d
    jge .k_done

    ; Load A[i, k] and broadcast
    mov eax, ebx
    imul eax, r14d          ; i * K
    add eax, edx            ; + k
    vbroadcastss ymm1, [r10 + rax*4]

    ; Load B[k, j:j+8]
    mov eax, edx
    imul eax, r15d          ; k * N
    add eax, ecx            ; + j
    vmovups ymm2, [r11 + rax*4]

    ; Accumulate
    vfmadd231ps ymm0, ymm1, ymm2

    inc edx
    jmp .k_loop

.k_done:
    ; Store C[i, j:j+8]
    mov eax, ebx
    imul eax, r15d          ; i * N
    add eax, ecx            ; + j
    vmovups [r12 + rax*4], ymm0

    add ecx, 8
    jmp .col_loop

.row_next:
    inc ebx
    jmp .row_loop

.done:
    vzeroupper
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; matmul_f32_transB - Matrix multiply with B transposed
;; C[M×N] = A[M×K] × B^T[N×K]
;;
;; Args:
;;   rdi = A pointer (M × K)
;;   rsi = BT pointer (N × K, B stored transposed)
;;   rdx = C pointer (M × N)
;;   ecx = M
;;   r8d = K
;;   r9d = N
;; ============================================================================
matmul_f32_transB:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov r10, rdi            ; A
    mov r11, rsi            ; BT
    mov r12, rdx            ; C
    mov r13d, ecx           ; M
    mov r14d, r8d           ; K
    mov r15d, r9d           ; N

    ; For each row i of A
    xor ebx, ebx
.mtb_row:
    cmp ebx, r13d
    jge .mtb_done

    ; For each row j of BT (column of B)
    xor ecx, ecx
.mtb_col:
    cmp ecx, r15d
    jge .mtb_row_next

    ; Dot product A[i,:] · BT[j,:]
    vxorps ymm0, ymm0, ymm0

    ; A row offset
    mov eax, ebx
    imul eax, r14d
    shl eax, 2
    lea rdi, [r10 + rax]

    ; BT row offset
    mov eax, ecx
    imul eax, r14d
    shl eax, 2
    lea rsi, [r11 + rax]

    ; Vectorized dot product
    mov edx, r14d
    shr edx, 3              ; K / 8
    jz .mtb_dot_tail

.mtb_dot_loop:
    vmovups ymm1, [rdi]
    vmovups ymm2, [rsi]
    vfmadd231ps ymm0, ymm1, ymm2
    add rdi, 32
    add rsi, 32
    dec edx
    jnz .mtb_dot_loop

.mtb_dot_tail:
    ; Horizontal sum
    vextractf128 xmm1, ymm0, 1
    vaddps xmm0, xmm0, xmm1
    vhaddps xmm0, xmm0, xmm0
    vhaddps xmm0, xmm0, xmm0

    ; Handle remaining K elements
    mov edx, r14d
    and edx, 7
    jz .mtb_dot_done

.mtb_dot_scalar:
    vmovss xmm1, [rdi]
    vmovss xmm2, [rsi]
    vfmadd231ss xmm0, xmm1, xmm2
    add rdi, 4
    add rsi, 4
    dec edx
    jnz .mtb_dot_scalar

.mtb_dot_done:
    ; Store C[i,j]
    mov eax, ebx
    imul eax, r15d
    add eax, ecx
    vmovss [r12 + rax*4], xmm0

    inc ecx
    jmp .mtb_col

.mtb_row_next:
    inc ebx
    jmp .mtb_row

.mtb_done:
    vzeroupper
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================================
;; matmul_f32_add_bias - Add bias to each row
;; C[i,j] += bias[j]
;;
;; Args:
;;   rdi = C pointer (M × N)
;;   rsi = bias pointer (N)
;;   edx = M
;;   ecx = N
;; ============================================================================
matmul_f32_add_bias:
    push rbx
    push r12

    mov r10, rdi            ; C
    mov r11, rsi            ; bias
    mov r12d, edx           ; M
    mov ebx, ecx            ; N

    xor ecx, ecx            ; i = 0
.bias_row_loop:
    cmp ecx, r12d
    jge .bias_done

    ; C[i,:] += bias[:]
    mov eax, ecx
    imul eax, ebx           ; i * N
    lea rdi, [r10 + rax*4]  ; &C[i,0]
    mov rsi, r11            ; bias

    mov edx, ebx            ; n = N
    shr edx, 3              ; n / 8
    jz .bias_tail

.bias_vec_loop:
    vmovups ymm0, [rdi]
    vmovups ymm1, [rsi]
    vaddps ymm0, ymm0, ymm1
    vmovups [rdi], ymm0
    add rdi, 32
    add rsi, 32
    dec edx
    jnz .bias_vec_loop

.bias_tail:
    mov edx, ebx
    and edx, 7
    jz .bias_next_row

.bias_scalar_loop:
    vmovss xmm0, [rdi]
    vaddss xmm0, xmm0, [rsi]
    vmovss [rdi], xmm0
    add rdi, 4
    add rsi, 4
    dec edx
    jnz .bias_scalar_loop

.bias_next_row:
    inc ecx
    jmp .bias_row_loop

.bias_done:
    vzeroupper
    pop r12
    pop rbx
    ret

;; ============================================================================
;; vec_add_f32 - Element-wise vector addition
;; dst[i] += src[i]
;;
;; Args:
;;   rdi = dst pointer
;;   rsi = src pointer
;;   edx = n (number of elements)
;; ============================================================================
vec_add_f32:
    mov ecx, edx
    shr ecx, 3              ; n / 8
    jz .va_tail

.va_loop:
    vmovups ymm0, [rdi]
    vaddps ymm0, ymm0, [rsi]
    vmovups [rdi], ymm0
    add rdi, 32
    add rsi, 32
    dec ecx
    jnz .va_loop

.va_tail:
    and edx, 7
    jz .va_done

.va_scalar:
    vmovss xmm0, [rdi]
    vaddss xmm0, xmm0, [rsi]
    vmovss [rdi], xmm0
    add rdi, 4
    add rsi, 4
    dec edx
    jnz .va_scalar

.va_done:
    vzeroupper
    ret

;; ============================================================================
;; vec_mul_f32 - Element-wise vector multiplication
;; dst[i] *= src[i]
;;
;; Args:
;;   rdi = dst pointer
;;   rsi = src pointer
;;   edx = n (number of elements)
;; ============================================================================
vec_mul_f32:
    mov ecx, edx
    shr ecx, 3              ; n / 8
    jz .vm_tail

.vm_loop:
    vmovups ymm0, [rdi]
    vmulps ymm0, ymm0, [rsi]
    vmovups [rdi], ymm0
    add rdi, 32
    add rsi, 32
    dec ecx
    jnz .vm_loop

.vm_tail:
    and edx, 7
    jz .vm_done

.vm_scalar:
    vmovss xmm0, [rdi]
    vmulss xmm0, xmm0, [rsi]
    vmovss [rdi], xmm0
    add rdi, 4
    add rsi, 4
    dec edx
    jnz .vm_scalar

.vm_done:
    vzeroupper
    ret

;; ============================================================================
;; vec_gelu_f32 - GELU activation (Gaussian Error Linear Unit)
;; x[i] = 0.5 * x * (1 + tanh(sqrt(2/pi) * (x + 0.044715 * x^3)))
;;
;; Approximation: 0.5 * x * (1 + tanh(0.7978845608 * x * (1 + 0.044715 * x^2)))
;;
;; Args:
;;   rdi = x pointer (in-place)
;;   edx = n
;; ============================================================================
vec_gelu_f32:
    push rbx

    mov rbx, rdi
    mov ecx, edx
    shr ecx, 3
    jz .gelu_tail

    vmovaps ymm4, [rel gelu_const_a]     ; 0.044715
    vmovaps ymm5, [rel gelu_const_b]     ; sqrt(2/pi)
    vmovaps ymm6, [rel gelu_const_half]  ; 0.5
    vmovaps ymm7, [rel gelu_const_one]   ; 1.0

.gelu_loop:
    vmovups ymm0, [rbx]                   ; x

    ; x^2
    vmulps ymm1, ymm0, ymm0

    ; 0.044715 * x^2
    vmulps ymm1, ymm1, ymm4

    ; 1 + 0.044715 * x^2
    vaddps ymm1, ymm1, ymm7

    ; x * (1 + 0.044715 * x^2)
    vmulps ymm1, ymm1, ymm0

    ; sqrt(2/pi) * x * (1 + 0.044715 * x^2)
    vmulps ymm1, ymm1, ymm5

    ; tanh approximation: tanh(x) ≈ x for small x, clamp to [-1,1]
    ; Simple approximation: tanh(x) ≈ x / (1 + |x|)
    vxorps ymm2, ymm2, ymm2
    vsubps ymm2, ymm2, ymm1       ; -x
    vmaxps ymm2, ymm1, ymm2       ; |x|
    vaddps ymm2, ymm2, ymm7       ; 1 + |x|
    vdivps ymm1, ymm1, ymm2       ; x / (1 + |x|) ≈ tanh(x)

    ; 1 + tanh(...)
    vaddps ymm1, ymm1, ymm7

    ; 0.5 * x * (1 + tanh(...))
    vmulps ymm0, ymm0, ymm6
    vmulps ymm0, ymm0, ymm1

    vmovups [rbx], ymm0
    add rbx, 32
    dec ecx
    jnz .gelu_loop

.gelu_tail:
    and edx, 7
    jz .gelu_done

    ; Scalar tail
    vmovss xmm4, [rel gelu_const_a]
    vmovss xmm5, [rel gelu_const_b]
    vmovss xmm6, [rel gelu_const_half]
    vmovss xmm7, [rel gelu_const_one]

.gelu_scalar:
    vmovss xmm0, [rbx]
    vmulss xmm1, xmm0, xmm0       ; x^2
    vmulss xmm1, xmm1, xmm4       ; 0.044715 * x^2
    vaddss xmm1, xmm1, xmm7       ; 1 + 0.044715 * x^2
    vmulss xmm1, xmm1, xmm0       ; x * (...)
    vmulss xmm1, xmm1, xmm5       ; sqrt(2/pi) * x * (...)

    ; tanh approximation
    vxorps xmm2, xmm2, xmm2
    vsubss xmm2, xmm2, xmm1
    vmaxss xmm2, xmm1, xmm2
    vaddss xmm2, xmm2, xmm7
    vdivss xmm1, xmm1, xmm2

    vaddss xmm1, xmm1, xmm7       ; 1 + tanh
    vmulss xmm0, xmm0, xmm6       ; 0.5 * x
    vmulss xmm0, xmm0, xmm1       ; 0.5 * x * (1 + tanh)
    vmovss [rbx], xmm0

    add rbx, 4
    dec edx
    jnz .gelu_scalar

.gelu_done:
    vzeroupper
    pop rbx
    ret

;; ============================================================================
;; layernorm_f32 - Layer normalization
;; out[i] = (x[i] - mean) / sqrt(var + eps) * gamma[i] + beta[i]
;;
;; Args:
;;   rdi = x pointer (in-place output)
;;   rsi = gamma pointer (scale)
;;   rdx = beta pointer (bias)
;;   ecx = n (hidden dimension)
;;   xmm0 = eps (e.g., 1e-5)
;; ============================================================================
layernorm_f32:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 32             ; local storage

    mov r10, rdi            ; x
    mov r11, rsi            ; gamma
    mov r12, rdx            ; beta
    mov r13d, ecx           ; n
    vmovss [rsp], xmm0      ; eps

    ; Step 1: Compute mean
    vxorps ymm0, ymm0, ymm0     ; sum = 0
    mov rdi, r10
    mov ecx, r13d
    shr ecx, 3
    jz .ln_mean_tail

.ln_mean_loop:
    vaddps ymm0, ymm0, [rdi]
    add rdi, 32
    dec ecx
    jnz .ln_mean_loop

.ln_mean_tail:
    ; Horizontal sum
    vextractf128 xmm1, ymm0, 1
    vaddps xmm0, xmm0, xmm1
    vhaddps xmm0, xmm0, xmm0
    vhaddps xmm0, xmm0, xmm0   ; xmm0 = sum

    ; Handle tail
    mov ecx, r13d
    and ecx, 7
    jz .ln_mean_done

.ln_mean_scalar:
    vaddss xmm0, xmm0, [rdi]
    add rdi, 4
    dec ecx
    jnz .ln_mean_scalar

.ln_mean_done:
    ; mean = sum / n
    vcvtsi2ss xmm1, xmm1, r13d
    vdivss xmm0, xmm0, xmm1     ; mean
    vmovss [rsp+4], xmm0

    ; Step 2: Compute variance
    vbroadcastss ymm2, xmm0     ; mean broadcast
    vxorps ymm0, ymm0, ymm0     ; var_sum = 0
    mov rdi, r10
    mov ecx, r13d
    shr ecx, 3
    jz .ln_var_tail

.ln_var_loop:
    vmovups ymm1, [rdi]
    vsubps ymm1, ymm1, ymm2     ; x - mean
    vmulps ymm1, ymm1, ymm1     ; (x - mean)^2
    vaddps ymm0, ymm0, ymm1
    add rdi, 32
    dec ecx
    jnz .ln_var_loop

.ln_var_tail:
    ; Horizontal sum
    vextractf128 xmm1, ymm0, 1
    vaddps xmm0, xmm0, xmm1
    vhaddps xmm0, xmm0, xmm0
    vhaddps xmm0, xmm0, xmm0

    ; Tail
    mov ecx, r13d
    and ecx, 7
    jz .ln_var_done
    vmovss xmm2, [rsp+4]        ; mean

.ln_var_scalar:
    vmovss xmm1, [rdi]
    vsubss xmm1, xmm1, xmm2
    vmulss xmm1, xmm1, xmm1
    vaddss xmm0, xmm0, xmm1
    add rdi, 4
    dec ecx
    jnz .ln_var_scalar

.ln_var_done:
    ; var = sum / n
    vcvtsi2ss xmm1, xmm1, r13d
    vdivss xmm0, xmm0, xmm1     ; variance
    ; sqrt(var + eps)
    vaddss xmm0, xmm0, [rsp]    ; + eps
    vsqrtss xmm0, xmm0, xmm0    ; sqrt
    vmovss [rsp+8], xmm0        ; save std

    ; Step 3: Normalize and apply gamma/beta
    vmovss xmm2, [rsp+4]        ; mean
    vmovss xmm3, [rsp+8]        ; std
    vbroadcastss ymm2, xmm2
    vbroadcastss ymm3, xmm3

    mov rdi, r10
    mov rsi, r11                ; gamma
    mov rdx, r12                ; beta
    mov ecx, r13d
    shr ecx, 3
    jz .ln_norm_tail

.ln_norm_loop:
    vmovups ymm0, [rdi]
    vsubps ymm0, ymm0, ymm2     ; x - mean
    vdivps ymm0, ymm0, ymm3     ; / std
    vmulps ymm0, ymm0, [rsi]    ; * gamma
    vaddps ymm0, ymm0, [rdx]    ; + beta
    vmovups [rdi], ymm0
    add rdi, 32
    add rsi, 32
    add rdx, 32
    dec ecx
    jnz .ln_norm_loop

.ln_norm_tail:
    mov ecx, r13d
    and ecx, 7
    jz .ln_done
    vmovss xmm2, [rsp+4]
    vmovss xmm3, [rsp+8]

.ln_norm_scalar:
    vmovss xmm0, [rdi]
    vsubss xmm0, xmm0, xmm2
    vdivss xmm0, xmm0, xmm3
    vmulss xmm0, xmm0, [rsi]
    vaddss xmm0, xmm0, [rdx]
    vmovss [rdi], xmm0
    add rdi, 4
    add rsi, 4
    add rdx, 4
    dec ecx
    jnz .ln_norm_scalar

.ln_done:
    vzeroupper
    add rsp, 32
    pop r14
    pop r13
    pop r12
    pop rbx
    ret
