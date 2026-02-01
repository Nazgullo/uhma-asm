; maturity.asm — Developmental stage gating: earn capabilities through stability
;
; @entry maturity_init() -> void                    ; set Stage 0
; @entry maturity_update(xmm0=acc, xmm1=stab, xmm2=coh) -> void ; update EMA
; @entry maturity_check_advance() -> void           ; check stage advance
; @entry gate_syscall(edi=syscall_num) -> eax       ; 1=allowed, 0=blocked
; @entry gate_fd_write(edi=fd) -> eax               ; 1=allowed
; @entry gate_fd_read(edi=fd) -> eax                ; 1=allowed
; @entry get_maturity_level() -> eax                ; current stage (0-2)
; @entry get_mastery_metrics() -> rax               ; ptr to metrics struct
;
; @calls fire_hook
; @calledby io.asm:gate_fd_*, repl.asm:status, dispatch.asm:periodic
;
; GOTCHAS:
;   - Stage 0 (Infant): internal only - surface, timestamps, REPL I/O
;   - Stage 1 (Aware): + read external files
;   - Stage 2 (Active): + write files, spawn processes
;   - Advancement: sustain 75%acc/80%stab/70%coh for thresh_window steps
;   - Uses EMA (alpha=0.01) for rolling metric averages
;
%include "syscalls.inc"
%include "constants.inc"

section .data
    mat_init_msg:       db "[MATURITY] Initialized at Stage 0 (Infant)", 10, 0
    mat_advance_msg:    db "[MATURITY] Advanced to Stage ", 0
    mat_blocked_msg:    db "[MATURITY] BLOCKED syscall ", 0
    mat_stage_msg:      db " (Stage ", 0
    mat_required_msg:   db " required)", 10, 0
    mat_nl:             db 10, 0

    ; Mastery thresholds for advancement
    align 8
    thresh_accuracy:    dq 0.75     ; 75% prediction accuracy
    thresh_stability:   dq 0.80     ; 80% metabolic stability
    thresh_coherence:   dq 0.70     ; 70% graph-holo agreement
    thresh_window:      dd 1000     ; must sustain for 1000 steps

    ; EMA decay for rolling averages
    ema_alpha:          dq 0.01     ; slow adaptation
    ema_one_minus:      dq 0.99

    ; Stage names
    stage_names:        dq stage_0, stage_1, stage_2
    stage_0:            db "0 (Infant)", 0
    stage_1:            db "1 (Aware)", 0
    stage_2:            db "2 (Active)", 0

section .bss
    ; Counters for stability tracking
    mastery_window_count:   resd 1  ; steps in current window
    mastery_above_thresh:   resd 1  ; steps all metrics above threshold

section .text

extern print_cstr
extern print_u64
extern print_hex32
extern print_newline
extern fire_hook

;; ============================================================
;; maturity_init()
;; Initialize maturity tracking. Called once at startup.
;; ============================================================
global maturity_init
maturity_init:
    push rbx

    mov rbx, SURFACE_BASE

    ; Set Stage 0
    mov dword [rbx + STATE_OFFSET + ST_MATURITY_LEVEL], 0

    ; Initialize mastery metrics to zero (must be earned)
    xor rax, rax
    mov [rbx + STATE_OFFSET + ST_MASTERY_ACC], rax
    mov [rbx + STATE_OFFSET + ST_MASTERY_STABILITY], rax
    mov [rbx + STATE_OFFSET + ST_MASTERY_GENE_RATE], rax
    mov [rbx + STATE_OFFSET + ST_MASTERY_COHERENCE], rax

    ; Initialize maturity score
    mov [rbx + STATE_OFFSET + ST_MATURITY_SCORE], rax

    ; External ops disabled
    mov dword [rbx + STATE_OFFSET + ST_EXTERN_ENABLED], 0

    ; Initialize counters
    mov dword [rel mastery_window_count], 0
    mov dword [rel mastery_above_thresh], 0

    ; Print init message
    lea rdi, [rel mat_init_msg]
    call print_cstr

    pop rbx
    ret

;; ============================================================
;; maturity_update(hit, total_hits, total_misses)
;; edi = 1 if this step was a hit, 0 if miss
;; esi = total hits so far
;; edx = total misses so far
;; Called each prediction step to update mastery metrics.
;; ============================================================
global maturity_update
maturity_update:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 8

    mov r12d, edi           ; hit flag
    mov r13d, esi           ; total hits
    mov r14d, edx           ; total misses

    mov rbx, SURFACE_BASE

    ; --- Update ST_MASTERY_ACC (rolling accuracy) ---
    ; EMA: acc = alpha * current + (1-alpha) * acc
    ; current = hit ? 1.0 : 0.0
    movsd xmm0, [rbx + STATE_OFFSET + ST_MASTERY_ACC]
    mulsd xmm0, [rel ema_one_minus]

    test r12d, r12d
    jz .miss_update
    movsd xmm1, [rel ema_alpha]     ; hit: add alpha * 1.0
    addsd xmm0, xmm1
.miss_update:
    movsd [rbx + STATE_OFFSET + ST_MASTERY_ACC], xmm0

    ; --- Update ST_MASTERY_STABILITY ---
    ; Based on recent energy variance (lower variance = more stable)
    ; For now: stability = 1.0 - |energy_delta| / max_delta
    ; Simplified: increment toward 1.0 slowly
    movsd xmm0, [rbx + STATE_OFFSET + ST_MASTERY_STABILITY]
    mulsd xmm0, [rel ema_one_minus]
    ; Add small increment if no condemned regions recently
    mov eax, [rbx + STATE_OFFSET + ST_METABOLIZED_COUNT]
    test eax, eax
    jnz .unstable
    movsd xmm1, [rel ema_alpha]
    addsd xmm0, xmm1
.unstable:
    movsd [rbx + STATE_OFFSET + ST_MASTERY_STABILITY], xmm0

    ; --- Update ST_MASTERY_COHERENCE ---
    ; Graph-holo agreement: how often dispatch matches holographic lookup
    ; Use the global hit rate as proxy
    mov eax, r13d
    add eax, r14d           ; total
    test eax, eax
    jz .skip_coherence

    cvtsi2sd xmm0, r13d     ; hits
    cvtsi2sd xmm1, eax      ; total
    divsd xmm0, xmm1        ; hit rate

    ; EMA update
    movsd xmm1, [rbx + STATE_OFFSET + ST_MASTERY_COHERENCE]
    mulsd xmm1, [rel ema_one_minus]
    movsd xmm2, [rel ema_alpha]
    mulsd xmm2, xmm0
    addsd xmm1, xmm2
    movsd [rbx + STATE_OFFSET + ST_MASTERY_COHERENCE], xmm1

.skip_coherence:
    ; --- Check for stage advancement ---
    call maturity_check_advance

    add rsp, 8
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; maturity_check_advance()
;; Check if all mastery metrics are above threshold.
;; If sustained for window steps, advance stage.
;; ============================================================
global maturity_check_advance
maturity_check_advance:
    push rbx
    push r12

    mov rbx, SURFACE_BASE

    ; Already at max stage?
    mov eax, [rbx + STATE_OFFSET + ST_MATURITY_LEVEL]
    cmp eax, 2
    jge .done

    ; Check ST_MASTERY_ACC >= thresh_accuracy
    movsd xmm0, [rbx + STATE_OFFSET + ST_MASTERY_ACC]
    ucomisd xmm0, [rel thresh_accuracy]
    jb .reset_window

    ; Check ST_MASTERY_STABILITY >= thresh_stability
    movsd xmm0, [rbx + STATE_OFFSET + ST_MASTERY_STABILITY]
    ucomisd xmm0, [rel thresh_stability]
    jb .reset_window

    ; Check ST_MASTERY_COHERENCE >= thresh_coherence
    movsd xmm0, [rbx + STATE_OFFSET + ST_MASTERY_COHERENCE]
    ucomisd xmm0, [rel thresh_coherence]
    jb .reset_window

    ; All above threshold - increment counter
    inc dword [rel mastery_above_thresh]
    mov eax, [rel mastery_above_thresh]
    cmp eax, [rel thresh_window]
    jl .done

    ; === ADVANCE STAGE ===
    mov eax, [rbx + STATE_OFFSET + ST_MATURITY_LEVEL]
    inc eax
    mov [rbx + STATE_OFFSET + ST_MATURITY_LEVEL], eax
    mov r12d, eax

    ; Reset counter
    mov dword [rel mastery_above_thresh], 0

    ; Enable external perception at Stage 1
    cmp r12d, 1
    jne .not_stage1
    mov dword [rbx + STATE_OFFSET + ST_EXTERN_ENABLED], 1
.not_stage1:

    ; Print advancement message
    lea rdi, [rel mat_advance_msg]
    call print_cstr

    ; Print stage name
    lea rax, [rel stage_names]
    mov rdi, [rax + r12 * 8]
    call print_cstr
    call print_newline

    ; Fire advancement hook (reuse promote hook for stage advancement)
    mov edi, HOOK_ON_PROMOTE
    mov esi, r12d
    call fire_hook

    jmp .done

.reset_window:
    ; Metrics dropped below threshold - reset counter
    mov dword [rel mastery_above_thresh], 0

.done:
    pop r12
    pop rbx
    ret

;; ============================================================
;; gate_syscall(syscall_nr) → eax (1=allowed, 0=blocked)
;; edi = syscall number
;; Checks if the syscall is allowed at current maturity level.
;; ============================================================
global gate_syscall
gate_syscall:
    push rbx
    push r12

    mov r12d, edi           ; syscall number
    mov rbx, SURFACE_BASE
    mov eax, [rbx + STATE_OFFSET + ST_MATURITY_LEVEL]

    ; --- ALWAYS ALLOWED (internal operations) ---
    ; These are the "body" - surface memory, timing, entropy

    cmp r12d, SYS_MMAP
    je .allow
    cmp r12d, SYS_MUNMAP
    je .allow
    cmp r12d, SYS_MPROTECT
    je .allow
    cmp r12d, SYS_MADVISE
    je .allow
    cmp r12d, SYS_BRK
    je .allow
    cmp r12d, SYS_GETTIMEOFDAY
    je .allow
    cmp r12d, SYS_CLOCK_GETTIME
    je .allow
    cmp r12d, SYS_GETRANDOM
    je .allow
    cmp r12d, SYS_FTRUNCATE     ; for surface file
    je .allow

    ; --- UMBILICAL CORD (REPL supervised I/O) ---
    ; stdin/stdout/stderr are always allowed (supervised)
    ; But we can't check fd here - that's checked in gated wrappers

    ; --- STAGE 0: Very limited ---
    test eax, eax
    jz .stage0_check

    ; --- STAGE 1+: Read operations ---
    cmp r12d, SYS_OPEN
    je .allow
    cmp r12d, SYS_OPENAT
    je .allow
    cmp r12d, SYS_READ
    je .allow
    cmp r12d, SYS_CLOSE
    je .allow
    cmp r12d, SYS_FSTAT
    je .allow
    cmp r12d, SYS_STAT
    je .allow
    cmp r12d, SYS_LSTAT
    je .allow
    cmp r12d, SYS_GETDENTS
    je .allow
    cmp r12d, SYS_LSEEK
    je .allow

    ; --- STAGE 2: Write/exec operations ---
    cmp eax, 2
    jl .block

    cmp r12d, SYS_WRITE
    je .allow
    cmp r12d, SYS_PWRITE
    je .allow
    cmp r12d, SYS_FSYNC
    je .allow
    cmp r12d, SYS_RENAME
    je .allow
    cmp r12d, SYS_UNLINK
    je .allow
    cmp r12d, SYS_MKDIR
    je .allow
    ; execve, fork, clone still blocked - need Stage 3?

    jmp .block

.stage0_check:
    ; Stage 0: only surface file ops allowed
    ; For open/read/write, need to check if it's the surface file
    ; But we can't check path here - use wrapper
    ; For now, allow close (might be surface fd)
    cmp r12d, SYS_CLOSE
    je .allow

    ; Block everything else at Stage 0
    jmp .block

.allow:
    mov eax, 1
    jmp .done

.block:
    ; Log blocked syscall
    push r12
    lea rdi, [rel mat_blocked_msg]
    call print_cstr
    pop r12
    mov edi, r12d
    call print_u64
    lea rdi, [rel mat_stage_msg]
    call print_cstr
    mov rbx, SURFACE_BASE
    mov edi, [rbx + STATE_OFFSET + ST_MATURITY_LEVEL]
    inc edi                 ; required stage
    call print_u64
    lea rdi, [rel mat_required_msg]
    call print_cstr

    xor eax, eax

.done:
    pop r12
    pop rbx
    ret

;; ============================================================
;; gate_fd_write(fd) → eax (1=allowed, 0=blocked)
;; edi = file descriptor
;; Checks if write to this fd is allowed.
;; stdout/stderr always allowed (umbilical).
;; ============================================================
global gate_fd_write
gate_fd_write:
    push rbx

    ; stdout (1) and stderr (2) always allowed - umbilical cord
    cmp edi, 0
    je .block               ; stdin not writable
    cmp edi, 1
    je .allow               ; stdout - umbilical
    cmp edi, 2
    je .allow               ; stderr - umbilical

    ; Other fds: check maturity
    mov rbx, SURFACE_BASE
    mov eax, [rbx + STATE_OFFSET + ST_MATURITY_LEVEL]
    cmp eax, 2
    jge .allow              ; Stage 2+ can write anywhere

    ; Stage 0-1: check if it's the surface fd
    ; We don't track surface fd, so block non-stdio writes
    jmp .block

.allow:
    mov eax, 1
    jmp .done

.block:
    xor eax, eax

.done:
    pop rbx
    ret

;; ============================================================
;; gate_fd_read(fd) → eax (1=allowed, 0=blocked)
;; edi = file descriptor
;; stdin always allowed (umbilical input).
;; ============================================================
global gate_fd_read
gate_fd_read:
    push rbx

    ; stdin always allowed - umbilical input
    cmp edi, 0
    je .allow

    ; Other fds: check maturity
    mov rbx, SURFACE_BASE
    mov eax, [rbx + STATE_OFFSET + ST_MATURITY_LEVEL]
    cmp eax, 1
    jge .allow              ; Stage 1+ can read files

.block:
    xor eax, eax
    jmp .done

.allow:
    mov eax, 1

.done:
    pop rbx
    ret

;; ============================================================
;; get_maturity_level() → eax
;; ============================================================
global get_maturity_level
get_maturity_level:
    mov rax, SURFACE_BASE
    mov eax, [rax + STATE_OFFSET + ST_MATURITY_LEVEL]
    ret

;; ============================================================
;; get_mastery_metrics(acc_ptr, stab_ptr, coh_ptr)
;; rdi, rsi, rdx = pointers to f64 outputs
;; ============================================================
global get_mastery_metrics
get_mastery_metrics:
    push rbx
    mov rbx, SURFACE_BASE

    movsd xmm0, [rbx + STATE_OFFSET + ST_MASTERY_ACC]
    movsd [rdi], xmm0

    movsd xmm0, [rbx + STATE_OFFSET + ST_MASTERY_STABILITY]
    movsd [rsi], xmm0

    movsd xmm0, [rbx + STATE_OFFSET + ST_MASTERY_COHERENCE]
    movsd [rdx], xmm0

    pop rbx
    ret
