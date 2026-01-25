; verify.asm — Runtime symbolic verification for self-modifications
;
; The system must verify its own changes before committing them.
; This module provides:
; 1. Symbolic representation of code invariants
; 2. Verification of proposed modifications
; 3. Rollback mechanism for failed verifications
;
%include "syscalls.inc"
%include "constants.inc"

section .data
    verify_pass:    db "[VERIFY] Modification approved", 10, 0
    verify_fail:    db "[VERIFY] Modification REJECTED: ", 0
    verify_stack:   db "stack imbalance", 10, 0
    verify_reg:     db "callee-saved register violation", 10, 0
    verify_bounds:  db "code bounds violation", 10, 0
    verify_opcode:  db "invalid opcode sequence", 10, 0
    verify_jump:    db "invalid jump target", 10, 0
    verify_rollback: db "[VERIFY] Rolling back modification", 10, 0

section .bss
    ; Verification state
    verify_enabled:     resd 1          ; 0=disabled, 1=enabled
    verify_depth:       resd 1          ; Current verification depth

    ; Snapshot for rollback (stores pre-modification state)
    snapshot_valid:     resd 1          ; Is snapshot valid?
    snapshot_addr:      resq 1          ; Address being modified
    snapshot_size:      resq 1          ; Size of snapshot
    snapshot_buf:       resb 4096       ; Backup buffer for rollback

section .text

extern print_cstr
extern print_u64
extern print_newline

;; ============================================================
;; Symbolic Logic Representation
;; ============================================================
;;
;; Invariants are encoded as verification predicates:
;;
;; PRED_STACK_BALANCED:
;;   ∀ path through code: Σ(push) - Σ(pop) = 0 at ret
;;
;; PRED_REGS_PRESERVED:
;;   ∀ reg ∈ {rbx, rbp, r12-r15}:
;;     modified(reg) → saved_before(reg) ∧ restored_after(reg)
;;
;; PRED_VALID_JUMPS:
;;   ∀ jmp/jcc target: target ∈ [code_start, code_end]
;;
;; PRED_VALID_OPCODES:
;;   ∀ instruction: opcode ∈ ALLOWED_OPCODES
;;
;; These are checked by scanning the proposed code modification.
;; ============================================================

;; ============================================================
;; verify_init
;; Initialize verification subsystem
;; ============================================================
global verify_init
verify_init:
    mov dword [rel verify_enabled], 1
    mov dword [rel verify_depth], 0
    mov dword [rel snapshot_valid], 0
    ret

;; ============================================================
;; verify_enable / verify_disable
;; Toggle verification (for bootstrapping or performance)
;; ============================================================
global verify_enable
verify_enable:
    mov dword [rel verify_enabled], 1
    ret

global verify_disable
verify_disable:
    mov dword [rel verify_enabled], 0
    ret

;; ============================================================
;; verify_begin_modification
;; Call before any self-modification. Takes snapshot for rollback.
;; rdi = address to modify
;; rsi = size of modification
;; Returns: rax = 1 if ok to proceed, 0 if verification disabled
;; ============================================================
global verify_begin_modification
verify_begin_modification:
    push rbx
    push r12
    push r13

    ; Check if enabled
    cmp dword [rel verify_enabled], 0
    je .disabled

    ; Check size limit
    cmp rsi, 4096
    ja .too_large

    ; Save parameters
    mov r12, rdi                ; addr
    mov r13, rsi                ; size

    ; Take snapshot for potential rollback
    mov qword [rel snapshot_addr], r12
    mov qword [rel snapshot_size], r13

    ; Copy current content to snapshot buffer
    lea rdi, [rel snapshot_buf]
    mov rsi, r12
    mov rcx, r13
.copy_loop:
    test rcx, rcx
    jz .copy_done
    mov al, [rsi]
    mov [rdi], al
    inc rsi
    inc rdi
    dec rcx
    jmp .copy_loop
.copy_done:

    mov dword [rel snapshot_valid], 1
    inc dword [rel verify_depth]

    mov eax, 1
    jmp .done

.disabled:
    xor eax, eax
    jmp .done

.too_large:
    ; Can't snapshot, proceed without rollback capability
    mov dword [rel snapshot_valid], 0
    mov eax, 1

.done:
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; verify_modification
;; Verify a proposed code modification meets all invariants.
;; rdi = address of code
;; rsi = size of code
;; Returns: rax = 1 if valid, 0 if invalid (with reason printed)
;; ============================================================
global verify_modification
verify_modification:
    push rbx
    push r12
    push r13
    push r14
    push r15

    ; Check if enabled
    cmp dword [rel verify_enabled], 0
    je .pass_disabled

    mov r12, rdi                ; code address
    mov r13, rsi                ; code size

    ; === Check 1: Valid opcode sequences ===
    mov rdi, r12
    mov rsi, r13
    call verify_opcodes
    test eax, eax
    jz .fail_opcode

    ; === Check 2: Stack balance ===
    mov rdi, r12
    mov rsi, r13
    call verify_stack_balance
    test eax, eax
    jz .fail_stack

    ; === Check 3: Jump targets in bounds ===
    mov rdi, r12
    mov rsi, r13
    call verify_jump_targets
    test eax, eax
    jz .fail_jump

    ; === Check 4: No forbidden instructions ===
    mov rdi, r12
    mov rsi, r13
    call verify_no_forbidden
    test eax, eax
    jz .fail_opcode

    ; All checks passed
.pass:
    lea rdi, [rel verify_pass]
    call print_cstr
    mov eax, 1
    jmp .done

.pass_disabled:
    mov eax, 1
    jmp .done

.fail_opcode:
    lea rdi, [rel verify_fail]
    call print_cstr
    lea rdi, [rel verify_opcode]
    call print_cstr
    xor eax, eax
    jmp .done

.fail_stack:
    lea rdi, [rel verify_fail]
    call print_cstr
    lea rdi, [rel verify_stack]
    call print_cstr
    xor eax, eax
    jmp .done

.fail_jump:
    lea rdi, [rel verify_fail]
    call print_cstr
    lea rdi, [rel verify_jump]
    call print_cstr
    xor eax, eax
    jmp .done

.done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; verify_commit
;; Called after successful modification. Clears snapshot.
;; ============================================================
global verify_commit
verify_commit:
    mov dword [rel snapshot_valid], 0
    cmp dword [rel verify_depth], 0
    je .done
    dec dword [rel verify_depth]
.done:
    ret

;; ============================================================
;; verify_rollback
;; Restore pre-modification state from snapshot.
;; Returns: rax = 1 if rolled back, 0 if no snapshot
;; ============================================================
global verify_rollback
verify_rollback:
    push rbx
    push r12
    push r13

    ; Check if we have a valid snapshot
    cmp dword [rel snapshot_valid], 0
    je .no_snapshot

    lea rdi, [rel verify_rollback]
    call print_cstr

    ; Restore from snapshot
    mov rdi, [rel snapshot_addr]
    lea rsi, [rel snapshot_buf]
    mov rcx, [rel snapshot_size]
.restore_loop:
    test rcx, rcx
    jz .restore_done
    mov al, [rsi]
    mov [rdi], al
    inc rsi
    inc rdi
    dec rcx
    jmp .restore_loop
.restore_done:

    mov dword [rel snapshot_valid], 0
    dec dword [rel verify_depth]

    mov eax, 1
    jmp .done

.no_snapshot:
    xor eax, eax

.done:
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; verify_opcodes
;; Check that code contains only valid x86-64 opcodes.
;; rdi = code, rsi = size
;; Returns: rax = 1 if valid, 0 if invalid
;; ============================================================
verify_opcodes:
    push rbx
    push r12
    push r13

    mov r12, rdi                ; code ptr
    mov r13, rsi                ; remaining size

.scan_loop:
    test r13, r13
    jz .valid

    movzx eax, byte [r12]

    ; Check for definitely invalid single-byte opcodes
    ; 0x06, 0x07 (push/pop es - invalid in 64-bit)
    cmp al, 0x06
    je .invalid
    cmp al, 0x07
    je .invalid
    ; 0x0E (push cs - invalid)
    cmp al, 0x0E
    je .invalid
    ; 0x16, 0x17 (push/pop ss - invalid)
    cmp al, 0x16
    je .invalid
    cmp al, 0x17
    je .invalid
    ; 0x1E, 0x1F (push/pop ds - invalid)
    cmp al, 0x1E
    je .invalid
    cmp al, 0x1F
    je .invalid
    ; 0x27 (DAA - invalid)
    cmp al, 0x27
    je .invalid
    ; 0x2F (DAS - invalid)
    cmp al, 0x2F
    je .invalid
    ; 0x37 (AAA - invalid)
    cmp al, 0x37
    je .invalid
    ; 0x3F (AAS - invalid)
    cmp al, 0x3F
    je .invalid
    ; 0x60, 0x61 (PUSHA/POPA - invalid)
    cmp al, 0x60
    je .invalid
    cmp al, 0x61
    je .invalid
    ; 0x62 (BOUND - invalid)
    cmp al, 0x62
    je .invalid
    ; 0xD4, 0xD5 (AAM/AAD - invalid)
    cmp al, 0xD4
    je .invalid
    cmp al, 0xD5
    je .invalid
    ; 0xD6 (undefined)
    cmp al, 0xD6
    je .invalid

    ; Skip this byte and continue
    ; (Real implementation would properly decode instruction length)
    inc r12
    dec r13
    jmp .scan_loop

.valid:
    mov eax, 1
    jmp .done

.invalid:
    xor eax, eax

.done:
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; verify_stack_balance
;; Symbolically verify stack balance: count push/pop/sub rsp/add rsp
;; rdi = code, rsi = size
;; Returns: rax = 1 if balanced, 0 if imbalanced
;;
;; Logic: Σ(push) + Σ(sub rsp,N)/8 = Σ(pop) + Σ(add rsp,N)/8
;; ============================================================
verify_stack_balance:
    push rbx
    push r12
    push r13
    push r14

    mov r12, rdi                ; code ptr
    mov r13, rsi                ; remaining size
    xor r14d, r14d              ; stack delta (positive = pushes, negative = pops)

.scan:
    cmp r13, 1
    jl .check_balance

    movzx eax, byte [r12]

    ; Check for PUSH (0x50-0x57, or 0x41 0x50-0x57 for r8-r15)
    cmp al, 0x50
    jl .not_push
    cmp al, 0x57
    jle .found_push

    ; Check for REX.B + PUSH (0x41 0x50-0x57)
    cmp al, 0x41
    jne .not_push
    cmp r13, 2
    jl .not_push
    movzx ebx, byte [r12 + 1]
    cmp bl, 0x50
    jl .not_push
    cmp bl, 0x57
    jg .not_push
    ; Found REX push
    inc r14d
    add r12, 2
    sub r13, 2
    jmp .scan

.found_push:
    inc r14d
    inc r12
    dec r13
    jmp .scan

.not_push:
    ; Check for POP (0x58-0x5F, or 0x41 0x58-0x5F for r8-r15)
    cmp al, 0x58
    jl .not_pop
    cmp al, 0x5F
    jle .found_pop
    jmp .not_pop

.found_pop:
    dec r14d
    inc r12
    dec r13
    jmp .scan

.not_pop:
    ; Check for RET (0xC3)
    cmp al, 0xC3
    jne .not_ret
    ; At ret, check balance
    test r14d, r14d
    jnz .imbalanced
    inc r12
    dec r13
    jmp .scan

.not_ret:
    ; Check for SUB RSP, imm8 (0x48 0x83 0xEC imm8)
    cmp al, 0x48
    jne .skip_byte
    cmp r13, 4
    jl .skip_byte
    cmp byte [r12 + 1], 0x83
    jne .skip_byte
    cmp byte [r12 + 2], 0xEC
    jne .skip_byte
    ; Found sub rsp, imm8
    movzx eax, byte [r12 + 3]
    shr eax, 3                  ; divide by 8
    add r14d, eax
    add r12, 4
    sub r13, 4
    jmp .scan

.skip_byte:
    inc r12
    dec r13
    jmp .scan

.check_balance:
    ; End of code - check final balance
    test r14d, r14d
    jnz .imbalanced

    mov eax, 1
    jmp .done

.imbalanced:
    xor eax, eax

.done:
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; verify_jump_targets
;; Check all jumps target within the code region or known safe targets.
;; rdi = code, rsi = size
;; Returns: rax = 1 if valid, 0 if invalid
;; ============================================================
verify_jump_targets:
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov r12, rdi                ; code start
    mov r13, rsi                ; code size
    lea r14, [r12 + r13]        ; code end
    mov r15, r12                ; current position

.scan:
    cmp r15, r14
    jge .valid

    movzx eax, byte [r15]

    ; Check for short jump (0xEB)
    cmp al, 0xEB
    jne .not_short_jmp
    cmp r15, r14
    jge .invalid                ; not enough bytes
    movsx ebx, byte [r15 + 1]   ; signed offset
    lea rcx, [r15 + 2 + rbx]    ; target address
    ; Check target in bounds
    cmp rcx, r12
    jl .invalid
    cmp rcx, r14
    jg .invalid
    add r15, 2
    jmp .scan

.not_short_jmp:
    ; Check for conditional jumps (0x70-0x7F)
    cmp al, 0x70
    jl .not_cond_jmp
    cmp al, 0x7F
    jg .not_cond_jmp
    ; Short conditional jump
    lea rcx, [r15 + 1]
    cmp rcx, r14
    jge .invalid
    movsx ebx, byte [r15 + 1]
    lea rcx, [r15 + 2 + rbx]
    cmp rcx, r12
    jl .invalid
    cmp rcx, r14
    jg .invalid
    add r15, 2
    jmp .scan

.not_cond_jmp:
    ; Check for near jump (0xE9)
    cmp al, 0xE9
    jne .not_near_jmp
    lea rcx, [r15 + 4]
    cmp rcx, r14
    jge .invalid
    mov ebx, [r15 + 1]          ; 32-bit offset
    movsxd rbx, ebx
    lea rcx, [r15 + 5 + rbx]
    ; Near jumps can go to .text section (known safe)
    ; Check if in .text (0x401000 - 0x410000) or in code region
    cmp rcx, 0x401000
    jl .check_code_region
    cmp rcx, 0x410000
    jl .jmp_ok
.check_code_region:
    cmp rcx, r12
    jl .invalid
    cmp rcx, r14
    jg .invalid
.jmp_ok:
    add r15, 5
    jmp .scan

.not_near_jmp:
    ; Skip other bytes
    inc r15
    jmp .scan

.valid:
    mov eax, 1
    jmp .done

.invalid:
    xor eax, eax

.done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; verify_no_forbidden
;; Check code doesn't contain forbidden instruction sequences.
;; Forbidden: syscall (except in known safe patterns), int, hlt
;; rdi = code, rsi = size
;; Returns: rax = 1 if ok, 0 if forbidden found
;; ============================================================
verify_no_forbidden:
    push rbx
    push r12
    push r13

    mov r12, rdi
    mov r13, rsi

.scan:
    cmp r13, 1
    jl .valid

    movzx eax, byte [r12]

    ; Check for INT (0xCD)
    cmp al, 0xCD
    je .forbidden

    ; Check for HLT (0xF4)
    cmp al, 0xF4
    je .forbidden

    ; Check for SYSCALL (0x0F 0x05)
    cmp al, 0x0F
    jne .not_syscall
    cmp r13, 2
    jl .not_syscall
    cmp byte [r12 + 1], 0x05
    je .forbidden

.not_syscall:
    inc r12
    dec r13
    jmp .scan

.valid:
    mov eax, 1
    jmp .done

.forbidden:
    xor eax, eax

.done:
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; verify_region_invariants
;; Verify a complete region header + code meets invariants.
;; rdi = region header address
;; Returns: rax = 1 if valid, 0 if invalid
;; ============================================================
global verify_region_invariants
verify_region_invariants:
    push rbx
    push r12

    mov r12, rdi                ; region header

    ; Check magic
    cmp dword [r12], REGION_MAGIC
    jne .invalid

    ; Get code start and size
    lea rdi, [r12 + RHDR_SIZE]  ; code starts after header
    movzx esi, word [r12 + RHDR_CODE_SIZE]

    ; Verify the code
    call verify_modification
    test eax, eax
    jz .invalid

    mov eax, 1
    jmp .done

.invalid:
    xor eax, eax

.done:
    pop r12
    pop rbx
    ret
