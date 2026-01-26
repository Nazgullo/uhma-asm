; signal.asm — SIGSEGV/SIGFPE/SIGBUS fault handler for isolation
%include "syscalls.inc"
%include "constants.inc"

section .data
    fault_msg:      db "[FAULT] Signal ", 0
    fault_at_msg:   db " at RIP=0x", 0
    fault_addr_msg: db " ADDR=0x", 0
    fault_nl:       db 10, 0
    fault_count:    dq 0              ; total faults caught
    consec_faults:  dq 0              ; consecutive fault counter
    max_consec:     equ 3             ; max consecutive faults before forced return
    fault_recover_msg: db "[FAULT] Recovery — returning to REPL", 10, 0

    ; Self-debugger messages
    trap_msg:       db "[TRAP] Breakpoint at RIP=0x", 0
    trap_rax_msg:   db " RAX=0x", 0
    trap_ctx_msg:   db " CTX=0x", 0
    trap_learn_msg: db " → Learning event", 10, 0
    trap_mismatch:  db " MISMATCH expected=0x", 0

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
extern journey_dump

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

    ; Install for SIGTRAP (INT 3 breakpoints — self-debugger)
    mov edi, SIGTRAP
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
    mov r13, rsi              ; save siginfo_t pointer
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

    ; Print faulting address (si_addr at offset 16 in siginfo_t)
    lea rdi, [rel fault_addr_msg]
    call print_cstr
    mov rdi, [r13 + 16]       ; si_addr = memory address that caused fault
    call print_hex64
    call print_newline

    ; Dump trace buffer (shows execution path leading to fault)
    call journey_dump

    ; Special case: SIGPIPE = stdout closed, exit cleanly
    cmp ebx, SIGPIPE
    je .exit_clean

    ; Special case: SIGTRAP = INT 3 breakpoint (self-debugger)
    cmp ebx, SIGTRAP
    je .handle_trap

    ; Recovery strategy based on fault location
    mov rax, [r12 + 168]      ; faulting RIP

    ; Check if the faulting address is in the surface
    mov rcx, SURFACE_BASE
    cmp rax, rcx
    jb .outside_surface
    mov rcx, SURFACE_BASE + SURFACE_SIZE  ; single 64-bit immediate (avoids sign-extension)
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

.handle_trap:
    ; --- SIGTRAP: INT 3 breakpoint hit (Self-Debugger) ---
    ; This is a LEARNING EVENT, not a crash!
    ; RIP points to instruction AFTER INT 3 (which was 1 byte)
    mov r13, [r12 + 168]      ; RIP after INT 3
    dec r13                   ; back up to INT 3 location
    mov r14, [r12 + 144]      ; RAX value at breakpoint

    ; Print trap message
    lea rdi, [rel trap_msg]
    call print_cstr
    mov rdi, r13
    call print_hex64
    lea rdi, [rel trap_rax_msg]
    call print_cstr
    mov rdi, r14
    call print_hex64

    ; Increment trap hit counter
    mov rax, SURFACE_BASE
    inc dword [rax + STATE_OFFSET + ST_BP_TOTAL_HITS]

    ; Look up breakpoint in table
    mov rdi, r13              ; breakpoint address
    call bp_find_by_addr
    test rax, rax
    jz .trap_unknown          ; not in our table

    ; Found breakpoint entry in rax
    mov r15, rax              ; save entry ptr

    ; Increment trigger count
    inc word [r15 + BPE_TRIGGER_COUNT]

    ; Store actual RAX value
    mov [r15 + BPE_LAST_RAX], r14d

    ; Check for mismatch with expected value
    mov ecx, [r15 + BPE_EXPECTED_RAX]
    cmp ecx, r14d
    je .trap_match

    ; MISMATCH: This is where we learn!
    lea rdi, [rel trap_mismatch]
    call print_cstr
    mov edi, ecx
    call print_hex64

    ; Generate learning event
    mov rax, SURFACE_BASE
    inc dword [rax + STATE_OFFSET + ST_BP_LEARNING_EVENTS]

    ; Mark breakpoint as learning
    or word [r15 + BPE_FLAGS], BPFLAG_LEARNING

.trap_match:
    lea rdi, [rel trap_learn_msg]
    call print_cstr

    ; Restore original byte and continue execution
    movzx eax, byte [r15 + BPE_ORIG_BYTE]
    mov [r13], al             ; restore original instruction byte

    ; RIP already points past INT 3, but we restored the original byte
    ; at the INT 3 location, so we need to re-execute that instruction
    mov [r12 + 168], r13      ; set RIP back to the restored instruction

    jmp .handler_done

.trap_unknown:
    ; Unknown breakpoint — just skip past it
    lea rdi, [rel trap_learn_msg]
    call print_cstr
    ; RIP already points past INT 3, so we can just continue
    jmp .handler_done

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

;; ============================================================
;; Self-Debugger: Breakpoint Management Functions
;; ============================================================

;; ============================================================
;; bp_init()
;; Initialize breakpoint table to zero.
;; ============================================================
global bp_init
bp_init:
    mov rdi, SURFACE_BASE + BREAKPOINT_TABLE_OFFSET
    xor eax, eax
    mov ecx, BREAKPOINT_TABLE_SIZE / 8
.bp_init_loop:
    mov [rdi], rax
    add rdi, 8
    dec ecx
    jnz .bp_init_loop
    ; Clear state counters
    mov rax, SURFACE_BASE
    mov dword [rax + STATE_OFFSET + ST_BREAKPOINT_COUNT], 0
    mov dword [rax + STATE_OFFSET + ST_BP_TOTAL_HITS], 0
    mov dword [rax + STATE_OFFSET + ST_BP_LEARNING_EVENTS], 0
    ret

;; ============================================================
;; bp_find_by_addr(addr) → rax (entry ptr or 0)
;; rdi = code address where INT 3 was hit
;; Returns pointer to breakpoint entry, or 0 if not found.
;; ============================================================
global bp_find_by_addr
bp_find_by_addr:
    mov rsi, SURFACE_BASE + BREAKPOINT_TABLE_OFFSET
    xor ecx, ecx             ; index

.bp_find_loop:
    cmp ecx, BREAKPOINT_MAX
    jge .bp_find_not_found

    ; Check if entry is active
    mov eax, [rsi + BPE_FLAGS]
    test eax, BPFLAG_ACTIVE
    jz .bp_find_next

    ; Calculate breakpoint address: region_ptr + RHDR_SIZE + code_offset
    mov rax, [rsi + BPE_REGION_PTR]
    add rax, RHDR_SIZE
    mov edx, [rsi + BPE_CODE_OFFSET]
    add rax, rdx

    ; Compare with target address
    cmp rax, rdi
    je .bp_find_found

.bp_find_next:
    add rsi, BREAKPOINT_ENTRY_SIZE
    inc ecx
    jmp .bp_find_loop

.bp_find_found:
    mov rax, rsi
    ret

.bp_find_not_found:
    xor eax, eax
    ret

;; ============================================================
;; bp_inject(region_ptr, code_offset, bp_type, expected_rax) → eax (1=success, 0=fail)
;; rdi = region header pointer
;; esi = offset into region code (after RHDR_SIZE)
;; edx = breakpoint type (BPTYPE_*)
;; ecx = expected RAX value at this point
;; Injects INT 3 (0xCC) at the specified location.
;; ============================================================
global bp_inject
bp_inject:
    push rbx
    push r12
    push r13
    push r14

    mov r12, rdi              ; region_ptr
    mov r13d, esi             ; code_offset
    mov r14d, edx             ; bp_type
    mov ebx, ecx              ; expected_rax

    ; Find empty slot in breakpoint table
    mov rdi, SURFACE_BASE + BREAKPOINT_TABLE_OFFSET
    xor ecx, ecx

.bp_inject_find:
    cmp ecx, BREAKPOINT_MAX
    jge .bp_inject_full

    mov eax, [rdi + BPE_FLAGS]
    test eax, BPFLAG_ACTIVE
    jz .bp_inject_slot_found

    add rdi, BREAKPOINT_ENTRY_SIZE
    inc ecx
    jmp .bp_inject_find

.bp_inject_slot_found:
    ; rdi = empty slot
    ; Calculate actual code address
    lea rax, [r12 + RHDR_SIZE]
    add rax, r13              ; rax = code address

    ; Save original byte
    movzx edx, byte [rax]
    mov [rdi + BPE_ORIG_BYTE], dl

    ; Fill in breakpoint entry
    mov [rdi + BPE_REGION_PTR], r12
    mov [rdi + BPE_CODE_OFFSET], r13d
    mov [rdi + BPE_TYPE], r14b
    mov word [rdi + BPE_TRIGGER_COUNT], 0
    mov [rdi + BPE_EXPECTED_RAX], ebx
    ; Get current context hash for expected_ctx
    mov rax, SURFACE_BASE
    mov edx, [rax + STATE_OFFSET + ST_CTX_HASH]
    mov [rdi + BPE_EXPECTED_CTX], edx
    mov dword [rdi + BPE_LAST_RAX], 0
    mov dword [rdi + BPE_FLAGS], BPFLAG_ACTIVE

    ; Inject INT 3 (0xCC)
    lea rax, [r12 + RHDR_SIZE]
    add rax, r13
    mov byte [rax], 0xCC

    ; Increment breakpoint count
    mov rax, SURFACE_BASE
    inc dword [rax + STATE_OFFSET + ST_BREAKPOINT_COUNT]

    mov eax, 1                ; success
    jmp .bp_inject_done

.bp_inject_full:
    xor eax, eax              ; failure

.bp_inject_done:
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; bp_remove(region_ptr) → eax (1=removed, 0=not found)
;; rdi = region header pointer
;; Removes all breakpoints for this region and restores original bytes.
;; ============================================================
global bp_remove
bp_remove:
    push rbx
    push r12

    mov r12, rdi              ; region_ptr
    xor ebx, ebx              ; removed count

    mov rsi, SURFACE_BASE + BREAKPOINT_TABLE_OFFSET
    xor ecx, ecx

.bp_remove_loop:
    cmp ecx, BREAKPOINT_MAX
    jge .bp_remove_done

    ; Check if entry is active and matches region
    mov eax, [rsi + BPE_FLAGS]
    test eax, BPFLAG_ACTIVE
    jz .bp_remove_next

    cmp [rsi + BPE_REGION_PTR], r12
    jne .bp_remove_next

    ; Found matching entry — restore original byte
    lea rax, [r12 + RHDR_SIZE]
    mov edx, [rsi + BPE_CODE_OFFSET]
    add rax, rdx
    movzx edx, byte [rsi + BPE_ORIG_BYTE]
    mov [rax], dl

    ; Clear entry
    mov dword [rsi + BPE_FLAGS], 0
    mov qword [rsi + BPE_REGION_PTR], 0

    ; Decrement breakpoint count
    mov rax, SURFACE_BASE
    dec dword [rax + STATE_OFFSET + ST_BREAKPOINT_COUNT]

    inc ebx

.bp_remove_next:
    add rsi, BREAKPOINT_ENTRY_SIZE
    inc ecx
    jmp .bp_remove_loop

.bp_remove_done:
    mov eax, ebx
    pop r12
    pop rbx
    ret

;; ============================================================
;; bp_inject_struggling(region_ptr) → eax (1=injected, 0=already has bp or fail)
;; rdi = region header pointer
;; Injects breakpoints at key decision points in a "struggling" region.
;; A struggling region has high miss rate but has received significant traffic.
;; ============================================================
global bp_inject_struggling
bp_inject_struggling:
    push rbx
    push r12
    push r13

    mov r12, rdi              ; region_ptr

    ; Check if region already has breakpoints
    mov rdi, r12
    call bp_has_breakpoints
    test eax, eax
    jnz .bp_struggling_skip   ; already has breakpoints

    ; Get region code length
    movzx eax, word [r12 + RHDR_CODE_LEN]
    cmp eax, 5                ; need at least 5 bytes for meaningful bp
    jl .bp_struggling_skip

    ; Inject at entry (offset 0) — see context hash in RAX before CMP
    mov rdi, r12
    xor esi, esi              ; offset 0
    mov edx, BPTYPE_STRUGGLING
    ; Expected RAX is the context we're checking for
    ; Read it from the CMP instruction if present
    lea rax, [r12 + RHDR_SIZE]
    cmp byte [rax], 0x3D      ; CMP EAX, imm32?
    jne .bp_struggling_no_cmp
    mov ecx, [rax + 1]        ; the immediate value
    jmp .bp_struggling_inject

.bp_struggling_no_cmp:
    xor ecx, ecx              ; unknown expected value

.bp_struggling_inject:
    call bp_inject
    mov r13d, eax             ; save result

    ; If first bp succeeded, also inject at decision point (after CMP, offset 5)
    test r13d, r13d
    jz .bp_struggling_done

    movzx eax, word [r12 + RHDR_CODE_LEN]
    cmp eax, 7                ; need space for second bp
    jl .bp_struggling_done

    mov rdi, r12
    mov esi, 5                ; after CMP EAX, imm32
    mov edx, BPTYPE_DECISION
    xor ecx, ecx              ; no specific expected value
    call bp_inject

    mov eax, 1
    jmp .bp_struggling_done

.bp_struggling_skip:
    xor eax, eax

.bp_struggling_done:
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; bp_has_breakpoints(region_ptr) → eax (1=has bp, 0=no bp)
;; rdi = region header pointer
;; ============================================================
global bp_has_breakpoints
bp_has_breakpoints:
    mov rsi, SURFACE_BASE + BREAKPOINT_TABLE_OFFSET
    xor ecx, ecx

.bp_has_loop:
    cmp ecx, BREAKPOINT_MAX
    jge .bp_has_no

    mov eax, [rsi + BPE_FLAGS]
    test eax, BPFLAG_ACTIVE
    jz .bp_has_next

    cmp [rsi + BPE_REGION_PTR], rdi
    je .bp_has_yes

.bp_has_next:
    add rsi, BREAKPOINT_ENTRY_SIZE
    inc ecx
    jmp .bp_has_loop

.bp_has_yes:
    mov eax, 1
    ret

.bp_has_no:
    xor eax, eax
    ret
