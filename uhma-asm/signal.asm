; signal.asm — SIGSEGV/SIGFPE/SIGBUS fault handler for isolation
%include "syscalls.inc"
%include "constants.inc"

section .data
    fault_msg:      db "[FAULT] Signal ", 0
    fault_at_msg:   db " at RIP=0x", 0
    fault_nl:       db 10, 0
    fault_count:    dq 0              ; total faults caught

section .text

extern print_cstr
extern print_u64
extern print_hex64
extern print_newline
extern sys_sigaction

;; ============================================================
;; install_fault_handlers
;; Sets up SIGSEGV, SIGFPE, SIGBUS handlers
;; ============================================================
global install_fault_handlers
install_fault_handlers:
    push rbx

    ; Set up sigaction struct on stack
    ; struct sigaction { handler, flags, restorer, sa_mask }
    sub rsp, 152              ; sizeof(struct sigaction) = 152 on x86_64

    ; handler = fault_handler
    lea rax, [rel fault_handler]
    mov [rsp], rax            ; sa_handler
    mov qword [rsp + 8], SA_SIGINFO | SA_RESTORER  ; sa_flags
    lea rax, [rel sig_restorer]
    mov [rsp + 16], rax       ; sa_restorer
    ; zero sa_mask
    xor eax, eax
    mov [rsp + 24], rax
    mov [rsp + 32], rax
    mov [rsp + 40], rax
    mov [rsp + 48], rax

    ; Install for SIGSEGV
    mov edi, SIGSEGV
    lea rsi, [rsp]            ; new action
    xor edx, edx              ; no old action
    mov r10, 8                ; sigsetsize
    mov rax, SYS_RT_SIGACTION
    syscall

    ; Install for SIGFPE
    mov edi, SIGFPE
    lea rsi, [rsp]
    xor edx, edx
    mov r10, 8
    mov rax, SYS_RT_SIGACTION
    syscall

    ; Install for SIGBUS
    mov edi, SIGBUS
    lea rsi, [rsp]
    xor edx, edx
    mov r10, 8
    mov rax, SYS_RT_SIGACTION
    syscall

    add rsp, 152
    pop rbx
    ret

;; ============================================================
;; fault_handler(signum, siginfo, ucontext)
;; SA_SIGINFO handler: rdi=signum, rsi=siginfo*, rdx=ucontext*
;; ============================================================
fault_handler:
    ; Save all registers
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    push rdi
    push rbp
    push r8
    push r9
    push r10
    push r11
    push r12
    push r13
    push r14
    push r15

    ; Increment fault counter
    inc qword [rel fault_count]

    ; Print fault message
    mov rbx, rdi              ; save signum
    mov r12, rdx              ; save ucontext

    lea rdi, [rel fault_msg]
    call print_cstr

    ; Print signal number
    mov rdi, rbx
    call print_u64

    lea rdi, [rel fault_at_msg]
    call print_cstr

    ; Get RIP from ucontext (offset 168 in ucontext_t.uc_mcontext.gregs[REG_RIP])
    mov rdi, [r12 + 168]
    call print_hex64
    call print_newline

    ; Recovery: advance RIP past the faulting instruction
    ; Simple heuristic: skip 1-4 bytes (look at instruction prefix)
    mov rax, [r12 + 168]      ; faulting RIP
    ; Check if the faulting address is in the surface
    mov rcx, SURFACE_BASE
    cmp rax, rcx
    jb .skip_small
    add rcx, SURFACE_SIZE
    cmp rax, rcx
    ja .skip_small

    ; In surface: try to skip the faulting instruction
    ; Read first byte to estimate instruction length
    movzx ecx, byte [rax]
    ; REX prefix?
    cmp cl, 0x40
    jl .no_rex
    cmp cl, 0x4F
    jg .no_rex
    add rax, 1                ; skip REX
    movzx ecx, byte [rax]
.no_rex:
    ; Simple length estimation
    cmp cl, 0x90              ; NOP
    je .skip1
    cmp cl, 0xC3              ; RET
    je .skip1
    ; Default: skip 2 bytes
    add rax, 2
    jmp .set_rip
.skip1:
    inc rax
    jmp .set_rip
.skip_small:
    ; Outside surface: skip 1 byte
    inc rax
.set_rip:
    mov [r12 + 168], rax      ; update RIP in ucontext

    ; If this region is in the dispatch tree, mark it CONDEMNED
    ; (simplified: just set xor eax,eax as return value)
    ; The region will be cleaned up by the observation loop

    ; Restore registers
    pop r15
    pop r14
    pop r13
    pop r12
    pop r11
    pop r10
    pop r9
    pop r8
    pop rbp
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    ret

;; ============================================================
;; sig_restorer — required for SA_RESTORER
;; ============================================================
sig_restorer:
    mov rax, SYS_RT_SIGRETURN
    syscall

;; ============================================================
;; get_fault_count → rax
;; ============================================================
global get_fault_count
get_fault_count:
    mov rax, [rel fault_count]
    ret
