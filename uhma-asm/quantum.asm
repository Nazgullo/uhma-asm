; quantum.asm — Quantum decision circuits (Born rule measurement)
;
; @entry qdec_init(edi=n_states) -> rax=0 (initializes stack-local QDecision)
; @entry qdec_set_amps(rdi=amp_array, esi=count) -> void (normalize to unit vector)
; @entry qdec_measure(rdi=amp_array, esi=count) -> eax=outcome
; @entry qdec_entropy(rdi=amp_array, esi=count) -> xmm0=entropy_f64
;
; @calledby drives.asm:drives_check (Born rule drive selection)
; @calledby dispatch.asm:process_token (quantum surprise boundary)
;
; FLOW: pressures → sqrt → normalize → Born rule sample → collapse
;
; GOTCHAS:
;   - Amplitudes are f64 arrays, NOT f32
;   - rdtsc-based PRNG is weak but fast; sufficient for stochastic decisions
;   - Always normalize before measuring (sum of squares must = 1.0)
;   - Floor amplitudes at 1e-6 before sqrt to avoid denormals
;
%include "syscalls.inc"
%include "constants.inc"

section .data
    align 8
    qdec_one:       dq 1.0
    qdec_epsilon:   dq 1.0e-6
    qdec_log2_e:    dq 1.4426950408889634     ; 1/ln(2) for log2 via ln
    qdec_half_decay: dq 0.5
    qdec_extra_decay: dq 0.5
    qdec_floor:     dq 0.1                     ; pressure floor

section .text

;; ============================================================
;; qdec_set_amps — Convert pressure values to normalized amplitudes
;;
;; rdi = f64 array (in: raw pressures, out: normalized amplitudes)
;; esi = count (max QDEC_MAX_STATES)
;;
;; For each value: amp[i] = sqrt(max(val[i], epsilon))
;; Then normalize: amp[i] /= sqrt(sum(amp[i]^2))
;; ============================================================
global qdec_set_amps
qdec_set_amps:
    push rbx
    push r12
    push r13
    sub rsp, 8                  ; align: 3 pushes + sub 8 = 32 bytes

    mov rbx, rdi                ; array pointer
    mov r12d, esi               ; count
    xorpd xmm3, xmm3           ; sum of squares = 0

    ; Pass 1: sqrt(max(val, epsilon))
    xor r13d, r13d
.sqrt_loop:
    cmp r13d, r12d
    jge .normalize
    movsd xmm0, [rbx + r13*8]
    movsd xmm1, [rel qdec_epsilon]
    maxsd xmm0, xmm1           ; max(val, 1e-6)
    sqrtsd xmm0, xmm0          ; sqrt
    movsd [rbx + r13*8], xmm0
    mulsd xmm0, xmm0           ; amp^2
    addsd xmm3, xmm0           ; accumulate
    inc r13d
    jmp .sqrt_loop

.normalize:
    ; norm = sqrt(sum_of_squares)
    sqrtsd xmm3, xmm3
    movsd xmm1, [rel qdec_one]
    divsd xmm1, xmm3           ; 1/norm

    ; Pass 2: divide each by norm
    xor r13d, r13d
.norm_loop:
    cmp r13d, r12d
    jge .done
    movsd xmm0, [rbx + r13*8]
    mulsd xmm0, xmm1
    movsd [rbx + r13*8], xmm0
    inc r13d
    jmp .norm_loop

.done:
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; qdec_measure — Born rule measurement (probabilistic selection)
;;
;; rdi = f64 amplitude array (normalized, unit vector)
;; esi = count
;; Returns: eax = selected outcome index
;;
;; Generate random r in [0,1]. Walk cumulative probability
;; P(i) = |amp[i]|^2. Return first i where cumsum >= r.
;; ============================================================
global qdec_measure
qdec_measure:
    push rbx
    push r12
    push r13
    sub rsp, 8                  ; align

    mov rbx, rdi                ; array
    mov r12d, esi               ; count

    ; Generate pseudo-random [0,1] via rdtsc
    rdtsc                       ; eax = low 32 bits of timestamp
    ; Simple LCG: x = (x * 1103515245 + 12345) & 0x7FFFFFFF
    imul eax, eax, 1103515245
    add eax, 12345
    and eax, 0x7FFFFFFF
    ; Convert to f64 in [0,1]
    cvtsi2sd xmm0, eax
    mov eax, 0x7FFFFFFF
    cvtsi2sd xmm1, eax
    divsd xmm0, xmm1           ; xmm0 = r in [0,1]

    ; Walk cumulative probability
    xorpd xmm2, xmm2           ; cumsum = 0
    xor r13d, r13d
.measure_loop:
    cmp r13d, r12d
    jge .last                   ; shouldn't happen if normalized, but safety
    movsd xmm3, [rbx + r13*8]
    mulsd xmm3, xmm3           ; P(i) = amp[i]^2
    addsd xmm2, xmm3           ; cumsum += P(i)
    ucomisd xmm2, xmm0
    jae .selected               ; cumsum >= r → select this one
    inc r13d
    jmp .measure_loop

.last:
    ; Edge case: return last index
    lea r13d, [r12d - 1]
.selected:
    mov eax, r13d

    add rsp, 8
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; qdec_entropy — Shannon entropy of probability distribution
;;
;; rdi = f64 amplitude array (normalized)
;; esi = count
;; Returns: xmm0 = entropy in bits (f64)
;;
;; H = -Σ p_i * log2(p_i) where p_i = |amp[i]|^2
;; Uses log2(x) = ln(x) * log2(e), and ln via x87 fyl2x
;; ============================================================
global qdec_entropy
qdec_entropy:
    push rbx
    push r12
    push r13
    sub rsp, 24                 ; align + local storage

    mov rbx, rdi
    mov r12d, esi
    xorpd xmm4, xmm4           ; entropy accumulator = 0.0

    xor r13d, r13d
.entropy_loop:
    cmp r13d, r12d
    jge .entropy_done
    movsd xmm0, [rbx + r13*8]
    mulsd xmm0, xmm0           ; p_i = amp[i]^2
    ; Skip if p_i ≈ 0
    movsd xmm1, [rel qdec_epsilon]
    ucomisd xmm0, xmm1
    jb .entropy_next

    ; Compute log2(p_i) via x87
    movsd [rsp], xmm0          ; save p_i
    movsd [rsp+8], xmm4        ; save accumulator

    ; x87: fyl2x computes y * log2(x), set y=1
    fld1                        ; ST(0) = 1.0
    fld qword [rsp]             ; ST(0) = p_i, ST(1) = 1.0
    fyl2x                       ; ST(0) = 1.0 * log2(p_i) = log2(p_i)
    fstp qword [rsp+16]         ; store log2(p_i)

    movsd xmm4, [rsp+8]        ; restore accumulator
    movsd xmm0, [rsp]          ; restore p_i
    movsd xmm1, [rsp+16]       ; log2(p_i)
    mulsd xmm0, xmm1           ; p_i * log2(p_i) (negative since p<1)
    subsd xmm4, xmm0           ; H -= p_i * log2(p_i) → adding positive value

.entropy_next:
    inc r13d
    jmp .entropy_loop

.entropy_done:
    movapd xmm0, xmm4          ; return entropy

    add rsp, 24
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; qdec_collapse — Apply post-measurement decay to pressures
;;
;; rdi = f64 pressure array (raw pressures, not amplitudes)
;; esi = count
;; edx = winner index
;;
;; All pressures *= 0.5
;; Winner *= additional 0.5 (total 0.25)
;; Floor all at 0.1
;; ============================================================
global qdec_collapse
qdec_collapse:
    push rbx
    push r12
    push r13
    sub rsp, 8

    mov rbx, rdi
    mov r12d, esi
    mov r13d, edx               ; winner

    xor ecx, ecx
.collapse_loop:
    cmp ecx, r12d
    jge .collapse_done
    movsd xmm0, [rbx + rcx*8]
    mulsd xmm0, [rel qdec_half_decay]   ; *= 0.5
    cmp ecx, r13d
    jne .no_extra
    mulsd xmm0, [rel qdec_extra_decay]  ; winner *= additional 0.5
.no_extra:
    ; Floor at 0.1
    maxsd xmm0, [rel qdec_floor]
    movsd [rbx + rcx*8], xmm0
    inc ecx
    jmp .collapse_loop

.collapse_done:
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    ret
