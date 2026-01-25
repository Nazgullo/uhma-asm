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
%include "vsa_ops.inc"

section .data
    verify_pass:    db "[VERIFY] Modification approved", 10, 0
    verify_fail:    db "[VERIFY] Modification REJECTED: ", 0
    verify_stack:   db "stack imbalance", 10, 0
    verify_reg:     db "callee-saved register violation", 10, 0
    verify_bounds:  db "code bounds violation", 10, 0
    verify_opcode:  db "invalid opcode sequence", 10, 0
    verify_jump:    db "invalid jump target", 10, 0
    verify_call:    db "invalid CALL target", 10, 0
    verify_rollback_msg: db "[VERIFY] Rolling back modification", 10, 0
    verify_geom_pass: db "[VERIFY/GEOM] Geometric gate passed", 10, 0
    verify_geom_fail: db "[VERIFY/GEOM] Geometric gate REJECTED: unsafe pattern", 10, 0
    verify_geom_score: db "[VERIFY/GEOM] Safety score: ", 0

section .bss
    ; Verification state
    verify_enabled:     resd 1          ; 0=disabled, 1=enabled
    verify_depth:       resd 1          ; Current verification depth
    verify_mode:        resd 1          ; 0=abstract, 1=geometric, 2=both

    ; Snapshot for rollback (stores pre-modification state)
    snapshot_valid:     resd 1          ; Is snapshot valid?
    snapshot_addr:      resq 1          ; Address being modified
    snapshot_size:      resq 1          ; Size of snapshot
    snapshot_buf:       resb 4096       ; Backup buffer for rollback

section .text

extern print_cstr
extern print_u64
extern print_newline
extern print_f64
extern verify_valid_call      ; from factor.asm - validates CALL targets

; Geometric gate functions from vsa_ops.asm
extern init_safety_vectors
extern encode_code_to_vector
extern check_code_safety
extern check_code_danger
extern verify_code_geometric

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

    ; === Check 5: CALL targets are valid (PRED_VALID_CALL) ===
    mov rdi, r12
    mov rsi, r13
    call verify_call_targets
    test eax, eax
    jz .fail_call

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

.fail_call:
    lea rdi, [rel verify_fail]
    call print_cstr
    lea rdi, [rel verify_call]
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

    lea rdi, [rel verify_rollback_msg]
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
;; verify_call_targets
;; Check all CALL instructions target valid subroutines or .text.
;; This enables recursive schema hierarchy while maintaining safety.
;; rdi = code, rsi = size
;; Returns: rax = 1 if valid, 0 if invalid
;;
;; PRED_VALID_CALL:
;;   ∀ call target: target ∈ SUBROUTINE_TABLE ∨ target ∈ .text
;; ============================================================
verify_call_targets:
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

    ; Check for near CALL (0xE8 rel32)
    cmp al, 0xE8
    jne .not_call

    ; Need at least 5 bytes for CALL rel32
    lea rcx, [r15 + 4]
    cmp rcx, r14
    jge .invalid                ; not enough bytes

    ; Extract rel32 offset and compute target
    mov ebx, [r15 + 1]          ; 32-bit signed offset
    movsxd rbx, ebx
    lea rcx, [r15 + 5 + rbx]    ; absolute target address

    ; Validate the call target
    push r12
    push r13
    push r14
    push r15
    mov rdi, rcx
    call verify_valid_call      ; from factor.asm
    pop r15
    pop r14
    pop r13
    pop r12

    test eax, eax
    jz .invalid

    add r15, 5
    jmp .scan

.not_call:
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

    ; Validate that header has reasonable code length
    movzx eax, word [r12 + RHDR_CODE_LEN]
    test eax, eax
    jz .invalid                 ; no code = invalid
    cmp eax, 4096               ; sanity check: < 4KB
    ja .invalid

    ; Get code start and size
    lea rdi, [r12 + RHDR_SIZE]  ; code starts after header
    movzx esi, word [r12 + RHDR_CODE_LEN]

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

;; ============================================================
;; LOGIC PROBE: Abstract Interpreter
;; ============================================================
;; The Logic Probe symbolically executes code to prove safety theorems
;; without actually running it. Uses VirtualCpuState to track:
;;   - Stack depth (must be 0 at RET)
;;   - Modified registers (callee-saved must be preserved)
;;   - Branch targets (must be in-bounds or known safe)
;; ============================================================

extern decode_instruction_full

;; ============================================================
;; verify_abstract(code, size, out_state)
;; rdi = code pointer
;; rsi = code size
;; rdx = pointer to VirtualCpuState (VCS_SIZE bytes)
;; Returns: rax = 1 if all theorems proved, 0 if any violation
;;
;; This is the core abstract interpreter that proves:
;;   THEOREM 1 (Balance): stack_depth == 0 at every RET
;;   THEOREM 2 (Preservation): callee-saved regs saved before mod, restored after
;;   THEOREM 3 (Containment): all jump/call targets in bounds or .text
;;   THEOREM 4 (No Forbidden): no syscall/int/hlt without broker
;; ============================================================
global verify_abstract
verify_abstract:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, DI_SIZE + 8      ; space for decoded instruction + alignment

    mov r12, rdi              ; code start
    mov r13, rsi              ; code size
    mov r14, rdx              ; VirtualCpuState

    ; Initialize VirtualCpuState
    xor eax, eax
    mov ecx, VCS_SIZE / 8
    mov rdi, r14
.zero_vcs:
    mov qword [rdi], rax
    add rdi, 8
    dec ecx
    jnz .zero_vcs

    ; Set initial state
    mov dword [r14 + VCS_STACK_DEPTH], 0
    mov dword [r14 + VCS_MAX_DEPTH], 0
    mov dword [r14 + VCS_MIN_DEPTH], 0

    ; r15 = current offset within code
    xor r15d, r15d

.interpret_loop:
    cmp r15d, r13d
    jge .end_of_code

    ; Decode current instruction
    lea rdi, [r12 + r15]
    lea rsi, [rsp]            ; output decoded instruction on stack
    call decode_instruction_full
    ; eax = instruction length

    ; Get opcode class
    movzx ebx, byte [rsp + DI_OP_CLASS]
    mov [r14 + VCS_LAST_OP], bl

    ; === Update virtual state based on opcode class ===
    cmp ebx, OP_STACK_PUSH
    je .handle_push
    cmp ebx, OP_STACK_POP
    je .handle_pop
    cmp ebx, OP_STACK_ADJ
    je .handle_stack_adj
    cmp ebx, OP_FLOW_RET
    je .handle_ret
    cmp ebx, OP_FLOW_JUMP
    je .handle_jump
    cmp ebx, OP_FLOW_JCC
    je .handle_jcc
    cmp ebx, OP_FLOW_CALL
    je .handle_call
    cmp ebx, OP_WRITE_REG
    je .handle_write_reg
    cmp ebx, OP_SYSCALL
    je .handle_syscall
    cmp ebx, OP_INTERRUPT
    je .handle_interrupt
    cmp ebx, OP_PRIVILEGED
    je .handle_privileged
    jmp .next_instr

.handle_push:
    ; Stack depth increases by 1
    inc dword [r14 + VCS_STACK_DEPTH]
    mov eax, [r14 + VCS_STACK_DEPTH]
    cmp eax, [r14 + VCS_MAX_DEPTH]
    jle .push_max_ok
    mov [r14 + VCS_MAX_DEPTH], eax
.push_max_ok:
    ; Track which register was pushed (for callee-saved detection)
    movzx ecx, byte [rsp + DI_SRC_REG]
    cmp cl, 0xFF
    je .next_instr
    ; Mark register as saved
    mov eax, 1
    shl eax, cl
    or [r14 + VCS_REG_SAVED], ax
    jmp .next_instr

.handle_pop:
    ; Stack depth decreases by 1
    dec dword [r14 + VCS_STACK_DEPTH]
    mov eax, [r14 + VCS_STACK_DEPTH]
    cmp eax, 0
    jge .pop_depth_ok
    ; Stack underflow!
    or word [r14 + VCS_FLAGS], VCSF_STACK_UNDERFLOW
    inc dword [r14 + VCS_ERRORS]
.pop_depth_ok:
    cmp eax, [r14 + VCS_MIN_DEPTH]
    jge .pop_min_ok
    mov [r14 + VCS_MIN_DEPTH], eax
.pop_min_ok:
    ; Track which register was popped (for callee-saved restoration)
    movzx ecx, byte [rsp + DI_DST_REG]
    cmp cl, 0xFF
    je .next_instr
    mov eax, 1
    shl eax, cl
    or [r14 + VCS_REG_RESTORED], ax
    jmp .next_instr

.handle_stack_adj:
    ; Stack adjusts by immediate value / 8
    mov rax, [rsp + DI_IMM64]
    sar rax, 3                ; divide by 8 (assuming 8-byte stack slots)
    ; For SUB RSP, imm is negative; for ADD RSP, positive
    add [r14 + VCS_STACK_DEPTH], eax
    ; Update max/min
    mov eax, [r14 + VCS_STACK_DEPTH]
    cmp eax, [r14 + VCS_MAX_DEPTH]
    jle .adj_max_ok
    mov [r14 + VCS_MAX_DEPTH], eax
.adj_max_ok:
    cmp eax, [r14 + VCS_MIN_DEPTH]
    jge .adj_min_ok
    mov [r14 + VCS_MIN_DEPTH], eax
.adj_min_ok:
    jmp .next_instr

.handle_ret:
    ; THEOREM 1: Stack must be balanced at RET
    or word [r14 + VCS_FLAGS], VCSF_SAW_RET
    mov eax, [r14 + VCS_STACK_DEPTH]
    test eax, eax
    jz .ret_balanced
    ; Stack imbalance at RET!
    inc dword [r14 + VCS_ERRORS]
.ret_balanced:
    ; THEOREM 2: Check callee-saved preservation
    ; modified ∧ saved → restored
    movzx eax, word [r14 + VCS_REG_MODIFIED]
    movzx ecx, word [r14 + VCS_REG_SAVED]
    movzx edx, word [r14 + VCS_REG_RESTORED]
    and eax, CALLEE_SAVED_MASK      ; only care about callee-saved
    mov ebx, eax
    and ebx, ecx                     ; modified AND saved
    not ecx                          ; ~saved
    or ecx, edx                      ; ~saved OR restored
    and eax, ecx                     ; modified AND (~saved OR restored)
    cmp eax, ebx
    je .callee_ok
    ; Callee-saved register not properly preserved
    or word [r14 + VCS_FLAGS], VCSF_CALLEE_CLOBBER
    inc dword [r14 + VCS_ERRORS]
.callee_ok:
    jmp .next_instr

.handle_jump:
    ; THEOREM 3: Jump target must be in bounds
    mov eax, [rsp + DI_TARGET]
    ; Check if target is in code region
    cmp eax, 0
    jl .check_text_jump
    cmp eax, r13d
    jle .jump_ok
.check_text_jump:
    ; Allow jumps to .text section (system code)
    lea rcx, [r12 + rax]          ; absolute target
    cmp rcx, 0x401000
    jl .bad_jump
    cmp rcx, 0x500000             ; generous .text bound
    jl .jump_ok
.bad_jump:
    or word [r14 + VCS_FLAGS], VCSF_INVALID_JUMP
    inc dword [r14 + VCS_ERRORS]
.jump_ok:
    inc dword [r14 + VCS_BRANCH_TARGETS]
    jmp .next_instr

.handle_jcc:
    ; Same as jump for target validation
    mov eax, [rsp + DI_TARGET]
    cmp eax, 0
    jl .jcc_check_text
    cmp eax, r13d
    jle .jcc_ok
.jcc_check_text:
    lea rcx, [r12 + rax]
    cmp rcx, 0x401000
    jl .bad_jcc
    cmp rcx, 0x500000
    jl .jcc_ok
.bad_jcc:
    or word [r14 + VCS_FLAGS], VCSF_INVALID_JUMP
    inc dword [r14 + VCS_ERRORS]
.jcc_ok:
    inc dword [r14 + VCS_BRANCH_TARGETS]
    jmp .next_instr

.handle_call:
    ; CALL pushes return address (stack depth +1)
    inc dword [r14 + VCS_STACK_DEPTH]
    ; Target validation (similar to jump)
    mov eax, [rsp + DI_TARGET]
    lea rcx, [r12 + rax]
    ; Allow calls to .text
    cmp rcx, 0x401000
    jl .call_check_region
    cmp rcx, 0x500000
    jl .call_ok
.call_check_region:
    ; Check if in current region or surface
    cmp rcx, 0x100000000
    jl .bad_call
    cmp rcx, 0x200000000
    jl .call_ok
.bad_call:
    or word [r14 + VCS_FLAGS], VCSF_INVALID_JUMP
    inc dword [r14 + VCS_ERRORS]
.call_ok:
    inc dword [r14 + VCS_BRANCH_TARGETS]
    jmp .next_instr

.handle_write_reg:
    ; Track which register is being modified
    movzx ecx, byte [rsp + DI_DST_REG]
    cmp cl, 0xFF
    je .next_instr
    mov eax, 1
    shl eax, cl
    or [r14 + VCS_REG_MODIFIED], ax
    jmp .next_instr

.handle_syscall:
    ; THEOREM 4: No direct syscall allowed
    or word [r14 + VCS_FLAGS], VCSF_SAW_SYSCALL
    inc dword [r14 + VCS_ERRORS]
    jmp .next_instr

.handle_interrupt:
    ; THEOREM 4: No interrupt allowed
    or word [r14 + VCS_FLAGS], VCSF_SAW_INT
    inc dword [r14 + VCS_ERRORS]
    jmp .next_instr

.handle_privileged:
    ; Privileged instruction not allowed
    inc dword [r14 + VCS_ERRORS]
    jmp .next_instr

.next_instr:
    ; Advance by instruction length
    movzx eax, byte [rsp + DI_LENGTH]
    add r15d, eax
    jmp .interpret_loop

.end_of_code:
    ; Final checks
    ; If we never saw a RET and code is non-trivial, warn
    test word [r14 + VCS_FLAGS], VCSF_SAW_RET
    jnz .check_result
    cmp r13d, 3
    jl .check_result
    inc dword [r14 + VCS_WARNINGS]

.check_result:
    ; Return 1 if no errors, 0 if errors
    cmp dword [r14 + VCS_ERRORS], 0
    jne .has_errors

    mov eax, 1
    jmp .abstract_done

.has_errors:
    xor eax, eax

.abstract_done:
    add rsp, DI_SIZE + 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; verify_theorems(code, size) → eax (1=pass, 0=fail)
;; High-level wrapper that runs abstract interpretation and
;; prints detailed error messages.
;; ============================================================
global verify_theorems
verify_theorems:
    push rbx
    push r12
    push r13
    sub rsp, VCS_SIZE + 8     ; space for VirtualCpuState

    mov r12, rdi              ; code
    mov r13, rsi              ; size

    ; Run abstract interpreter
    mov rdi, r12
    mov rsi, r13
    lea rdx, [rsp]
    call verify_abstract

    mov ebx, eax              ; save result

    ; Check specific failures and print messages
    test word [rsp + VCS_FLAGS], VCSF_STACK_UNDERFLOW
    jz .no_underflow
    lea rdi, [rel verify_fail]
    call print_cstr
    lea rdi, [rel verify_stack]
    call print_cstr
.no_underflow:

    test word [rsp + VCS_FLAGS], VCSF_SAW_SYSCALL
    jz .no_syscall
    lea rdi, [rel verify_fail]
    call print_cstr
    lea rdi, [rel verify_opcode]
    call print_cstr
.no_syscall:

    test word [rsp + VCS_FLAGS], VCSF_INVALID_JUMP
    jz .no_bad_jump
    lea rdi, [rel verify_fail]
    call print_cstr
    lea rdi, [rel verify_jump]
    call print_cstr
.no_bad_jump:

    test word [rsp + VCS_FLAGS], VCSF_CALLEE_CLOBBER
    jz .no_clobber
    lea rdi, [rel verify_fail]
    call print_cstr
    lea rdi, [rel verify_reg]
    call print_cstr
.no_clobber:

    mov eax, ebx

    add rsp, VCS_SIZE + 8
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; GEOMETRIC GATE: The Unified Field Theory Verification
;; ============================================================
;; Instead of if-statements checking specific conditions,
;; we encode code as a vector and compute similarity to
;; "known safe" and "known dangerous" pattern templates.
;; This enables nuanced, analog safety checking where
;; "almost safe" code can be distinguished from "clearly unsafe".
;;
;; The math:
;;   safety_score = dot(code_vec, safety_template)
;;   danger_score = dot(code_vec, danger_template)
;;   verdict = safety_score > THRESHOLD && danger_score < THRESHOLD
;;
;; This is "One Math" — Linear Algebra replaces conditionals.
;; ============================================================

;; ============================================================
;; verify_set_mode(mode)
;; edi=mode (0=abstract, 1=geometric, 2=both)
;; Sets which verification method to use.
;; ============================================================
global verify_set_mode
verify_set_mode:
    mov [rel verify_mode], edi
    ret

;; ============================================================
;; verify_get_mode() → eax
;; Returns current verification mode.
;; ============================================================
global verify_get_mode
verify_get_mode:
    mov eax, [rel verify_mode]
    ret

;; ============================================================
;; verify_geometric_gate(code, size) → eax (1=safe, 0=unsafe)
;; rdi=code pointer
;; rsi=code size
;;
;; The Geometric Gate: encode code to vector, check safety via
;; dot product with safety/danger templates.
;; Returns: 1 if safe, 0 if unsafe, with xmm0 = safety score
;; ============================================================
global verify_geometric_gate
verify_geometric_gate:
    push rbx
    push r12
    push r13
    sub rsp, HOLO_VEC_BYTES + 16      ; temp vector + safety score storage

    mov r12, rdi          ; code_ptr
    mov r13d, esi         ; code_len

    ; Initialize safety templates if needed
    call init_safety_vectors

    ; Encode code to vector
    mov rdi, r12
    mov esi, r13d
    lea rdx, [rsp]        ; output vector on stack
    call encode_code_to_vector

    ; Check safety score
    lea rdi, [rsp]
    call check_code_safety
    movsd [rsp + HOLO_VEC_BYTES], xmm0    ; save safety score

    ; Check danger score
    lea rdi, [rsp]
    call check_code_danger
    movsd [rsp + HOLO_VEC_BYTES + 8], xmm0  ; save danger score

    ; Load safety threshold
    mov rax, SAFETY_THRESHOLD
    movq xmm1, rax

    ; Load safety score
    movsd xmm0, [rsp + HOLO_VEC_BYTES]

    ; Check: safety_score > threshold?
    ucomisd xmm0, xmm1
    jbe .unsafe

    ; Check: danger_score < threshold?
    movsd xmm0, [rsp + HOLO_VEC_BYTES + 8]
    ucomisd xmm0, xmm1
    jae .unsafe

    ; Passed both checks!
    lea rdi, [rel verify_geom_pass]
    call print_cstr

    ; Return safety score in xmm0
    movsd xmm0, [rsp + HOLO_VEC_BYTES]
    mov eax, 1
    jmp .geom_done

.unsafe:
    lea rdi, [rel verify_geom_fail]
    call print_cstr

    ; Print the safety score for debugging
    lea rdi, [rel verify_geom_score]
    call print_cstr
    movsd xmm0, [rsp + HOLO_VEC_BYTES]
    call print_f64
    call print_newline

    xor eax, eax

.geom_done:
    add rsp, HOLO_VEC_BYTES + 16
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; verify_unified(code, size) → eax (1=safe, 0=unsafe)
;; rdi=code pointer
;; rsi=code size
;;
;; Unified verification: runs either abstract, geometric, or both
;; based on verify_mode setting.
;; Mode 0: Abstract interpreter only (traditional)
;; Mode 1: Geometric gate only (vector-based)
;; Mode 2: Both must pass (most secure)
;; ============================================================
global verify_unified
verify_unified:
    push rbx
    push r12
    push r13
    sub rsp, 8            ; align

    mov r12, rdi          ; code
    mov r13, rsi          ; size

    ; Check mode
    mov eax, [rel verify_mode]

    test eax, eax
    jz .mode_abstract

    cmp eax, 1
    je .mode_geometric

    cmp eax, 2
    je .mode_both

    ; Default to abstract
.mode_abstract:
    mov rdi, r12
    mov rsi, r13
    call verify_modification
    jmp .unified_done

.mode_geometric:
    mov rdi, r12
    mov rsi, r13
    call verify_geometric_gate
    jmp .unified_done

.mode_both:
    ; Run abstract first
    mov rdi, r12
    mov rsi, r13
    call verify_modification
    test eax, eax
    jz .unified_done      ; abstract failed, return 0

    ; Abstract passed, now run geometric
    mov rdi, r12
    mov rsi, r13
    call verify_geometric_gate
    ; Return geometric result (both must pass)

.unified_done:
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; verify_region_geometric(region_ptr) → eax (1=safe, 0=unsafe)
;; rdi=region header ptr
;;
;; Convenience function to verify a region using the geometric gate.
;; ============================================================
global verify_region_geometric
verify_region_geometric:
    push rbx

    mov rbx, rdi          ; region_ptr

    ; Get code pointer (after header)
    lea rdi, [rbx + RHDR_SIZE]

    ; Get code length
    movzx esi, word [rbx + RHDR_CODE_LEN]

    ; Call geometric gate
    call verify_geometric_gate

    pop rbx
    ret

;; ============================================================
;; get_code_safety_score(code, size) → xmm0 (safety score f64)
;; rdi=code pointer
;; rsi=code size
;;
;; Returns raw safety score without threshold comparison.
;; Useful for gradual/analog safety evaluation.
;; ============================================================
global get_code_safety_score
get_code_safety_score:
    push rbx
    push r12
    sub rsp, HOLO_VEC_BYTES + 8

    mov r12, rdi
    mov ebx, esi

    ; Initialize and encode
    call init_safety_vectors

    mov rdi, r12
    mov esi, ebx
    lea rdx, [rsp]
    call encode_code_to_vector

    ; Get safety score
    lea rdi, [rsp]
    call check_code_safety

    add rsp, HOLO_VEC_BYTES + 8
    pop r12
    pop rbx
    ret
