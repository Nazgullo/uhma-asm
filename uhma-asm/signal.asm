; signal.asm — SIGSEGV/SIGFPE/SIGBUS fault handler for isolation
%include "syscalls.inc"
%include "constants.inc"

section .data
    fault_msg:      db "[FAULT] Signal ", 0
    fault_at_msg:   db " at RIP=0x", 0
    fault_nl:       db 10, 0
    fault_count:    dq 0              ; total faults caught
    consec_faults:  dq 0              ; consecutive fault counter
    max_consec:     equ 3             ; max consecutive faults before forced return
    fault_recover_msg: db "[FAULT] Recovery — returning to REPL", 10, 0

section .bss
    global fault_safe_rsp
    fault_safe_rsp: resq 1            ; saved RSP for crash recovery
    global fault_safe_rip
    fault_safe_rip: resq 1            ; saved RIP for crash recovery (repl loop head)

section .data

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

    ; Install for SIGILL
    mov edi, SIGILL
    lea rsi, [rsp]
    xor edx, edx
    mov r10, 8
    mov rax, SYS_RT_SIGACTION
    syscall

    ; Install for SIGPIPE (broken pipe — stdout closed)
    mov edi, SIGPIPE
    lea rsi, [rsp]
    xor edx, edx
    mov r10, 8
    mov rax, SYS_RT_SIGACTION
    syscall

    add rsp, 152
    pop rbx
    ret

;; ============================================================
;; fault_return_stub — Safe return point for faulting regions
;; Returns 0 (miss) to the caller that dispatched into surface code
;; ============================================================
global fault_return_stub
fault_return_stub:
    xor eax, eax
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

    ; Special case: SIGPIPE = stdout closed, exit cleanly
    cmp ebx, SIGPIPE
    je .exit_clean

    ; Recovery strategy based on fault location
    mov rax, [r12 + 168]      ; faulting RIP

    ; Check if the faulting address is in the surface
    mov rcx, SURFACE_BASE
    cmp rax, rcx
    jb .outside_surface
    add rcx, SURFACE_SIZE
    cmp rax, rcx
    ja .outside_surface

    ; --- SURFACE FAULT: redirect to safe return stub ---
    ; Instead of stepping through bad code byte by byte, redirect
    ; RIP to fault_return_stub which does "xor eax,eax; ret"
    ; This cleanly returns a "miss" to whatever called into the surface
    lea rax, [rel fault_return_stub]
    mov [r12 + 168], rax
    ; Clear RAX in ucontext so the stub returns 0
    ; RAX is at offset 144 in uc_mcontext.gregs (REG_RAX = index 13, offset 13*8=104...
    ; actually on x86_64 Linux: gregs[REG_RAX] is at offset 144 from uc_mcontext)
    ; Reset consecutive counter
    mov qword [rel consec_faults], 0
    jmp .handler_done

.outside_surface:
    ; --- APPLICATION CODE FAULT ---
    ; This shouldn't happen normally. Use consecutive counter to prevent infinite loops.
    inc qword [rel consec_faults]
    cmp qword [rel consec_faults], max_consec
    jg .force_return

    ; Try to advance past the faulting instruction with proper length decoding
    movzx ecx, byte [rax]

    ; Check for common multi-byte prefixes
    ; REX prefix (0x40-0x4F)
    cmp cl, 0x40
    jl .no_rex
    cmp cl, 0x4F
    jg .no_rex
    add rax, 1
    movzx ecx, byte [rax]
.no_rex:
    ; F2/F3 prefix (SSE)
    cmp cl, 0xF2
    je .sse_prefix
    cmp cl, 0xF3
    je .sse_prefix
    jmp .check_opcode
.sse_prefix:
    add rax, 1
    movzx ecx, byte [rax]
    ; After F2/F3, check for 0F escape
    cmp cl, 0x0F
    jne .check_opcode
    ; F2 0F xx or F3 0F xx = 3+ byte opcode, skip at least 4 more
    add rax, 5               ; skip 0F + opcode + modrm + at least 1
    jmp .set_rip
.check_opcode:
    ; 0F escape (two-byte opcode)
    cmp cl, 0x0F
    jne .one_byte
    add rax, 4               ; 0F + opcode + modrm + sib/disp (estimate)
    jmp .set_rip
.one_byte:
    cmp cl, 0x90              ; NOP
    je .skip1
    cmp cl, 0xC3              ; RET
    je .skip1
    cmp cl, 0xFF              ; inc/call/jmp indirect (modrm + sib + disp32)
    je .skip7
    ; Default: skip 3 bytes (opcode + modrm + sib is common)
    add rax, 3
    jmp .set_rip
.skip1:
    inc rax
    jmp .set_rip
.skip7:
    add rax, 7
    jmp .set_rip

.force_return:
    ; Too many consecutive faults — longjmp back to REPL loop
    mov qword [rel consec_faults], 0

    ; Check if we have a saved recovery point
    mov rax, [rel fault_safe_rsp]
    test rax, rax
    jz .force_stub            ; no recovery point saved, use stub

    ; Restore RSP and RIP from saved recovery point
    ; RSP is at offset 160 in ucontext (gregs[15])
    ; RIP is at offset 168 in ucontext (gregs[16])
    mov [r12 + 160], rax      ; restore RSP
    mov rax, [rel fault_safe_rip]
    mov [r12 + 168], rax      ; restore RIP
    jmp .handler_done

.force_stub:
    lea rax, [rel fault_return_stub]

.set_rip:
    mov [r12 + 168], rax      ; update RIP in ucontext

.exit_clean:
    ; SIGPIPE or fatal — exit process cleanly
    mov edi, 0
    mov rax, SYS_EXIT
    syscall

.handler_done:
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
