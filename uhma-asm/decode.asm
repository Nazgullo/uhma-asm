; decode.asm — x86 instruction decoder for self-reading and verification
;
; @entry decode_instruction_length(rdi=ptr) -> eax=length in bytes
; @entry decode_instruction_full(rdi=ptr, rsi=out_buf) -> fills DecodedInstr struct
; @entry decode_region_instructions(rdi=hdr, rsi=out) -> decode all instrs in region
; @entry count_region_instructions(rdi=hdr) -> eax=instruction count
; @entry classify_opcode(edi=opcode) -> eax=OpClass enum
;
; @calledby verify.asm:verify_modification (abstract interpretation)
; @calledby introspect.asm:introspect_region (semantic analysis)
; @calledby factor.asm:factor_suffix (suffix length detection)
;
; FLOW: Raw bytes → REX detection → opcode dispatch → ModR/M → SIB → displ → imm
;
; OPCODE CLASSES: OP_CMP, OP_JCC, OP_MOV, OP_CALL, OP_RET, OP_SYSCALL, OP_PUSH, OP_POP
;
; GOTCHAS:
;   - REX prefix (0x40-0x4F) extends register set to r8-r15
;   - ModR/M byte required for most instructions
;   - SIB byte only when ModR/M.rm == 4
;
%include "syscalls.inc"
%include "constants.inc"

section .data
    ; Register mapping for push/pop (0x50-0x5F -> REG_*)
    push_reg_map: db REG_RAX, REG_RCX, REG_RDX, REG_RBX, REG_RSP, REG_RBP, REG_RSI, REG_RDI
    pop_reg_map:  db REG_RAX, REG_RCX, REG_RDX, REG_RBX, REG_RSP, REG_RBP, REG_RSI, REG_RDI

section .text

;; ============================================================
;; decode_instruction_length(ptr) → eax (length in bytes)
;; rdi=pointer to instruction
;; Returns the length of the x86_64 instruction at ptr
;; Handles common instructions used in dispatch patterns
;; ============================================================
global decode_instruction_length
decode_instruction_length:
    movzx eax, byte [rdi]     ; first byte (opcode or prefix)

    ; Check for REX prefix (0x40-0x4F)
    cmp al, 0x40
    jl .no_rex
    cmp al, 0x4F
    jg .no_rex
    ; REX prefix: read next byte as actual opcode
    inc rdi
    movzx eax, byte [rdi]
    ; Add 1 for REX prefix to final length
    push 1                    ; prefix_len on stack
    jmp .decode_opcode

.no_rex:
    push 0                    ; no prefix

.decode_opcode:
    ; --- Single byte instructions ---
    cmp al, 0x90              ; NOP
    je .len1
    cmp al, 0xC3              ; RET
    je .len1
    cmp al, 0xCC              ; INT3
    je .len1
    cmp al, 0xF4              ; HLT
    je .len1
    cmp al, 0xCB              ; RETF
    je .len1

    ; --- Two byte instructions ---
    cmp al, 0x31              ; XOR r/m32, r32 (XOR eax, eax = 31 C0)
    je .len2_modrm
    cmp al, 0x29              ; SUB r/m, r
    je .len2_modrm
    cmp al, 0x01              ; ADD r/m, r
    je .len2_modrm
    cmp al, 0x09              ; OR r/m, r
    je .len2_modrm
    cmp al, 0x21              ; AND r/m, r
    je .len2_modrm
    cmp al, 0x39              ; CMP r/m, r
    je .len2_modrm
    cmp al, 0x89              ; MOV r/m, r
    je .len2_modrm
    cmp al, 0x8B              ; MOV r, r/m
    je .len2_modrm

    ; --- Relative jump short (2 bytes) ---
    cmp al, 0x70
    jl .not_jcc_short
    cmp al, 0x7F
    jg .not_jcc_short
    jmp .len2               ; Jcc rel8
.not_jcc_short:
    cmp al, 0xEB              ; JMP rel8
    je .len2

    ; --- 5-byte instructions ---
    cmp al, 0x3D              ; CMP eax, imm32
    je .len5
    cmp al, 0x05              ; ADD eax, imm32
    je .len5
    cmp al, 0x2D              ; SUB eax, imm32
    je .len5
    cmp al, 0x25              ; AND eax, imm32
    je .len5
    cmp al, 0x0D              ; OR eax, imm32
    je .len5
    cmp al, 0x35              ; XOR eax, imm32
    je .len5
    cmp al, 0xB8              ; MOV eax, imm32
    je .len5
    cmp al, 0xB9              ; MOV ecx, imm32
    je .len5
    cmp al, 0xBA              ; MOV edx, imm32
    je .len5
    cmp al, 0xBB              ; MOV ebx, imm32
    je .len5

    ; --- 5-byte: JMP/CALL rel32 ---
    cmp al, 0xE8              ; CALL rel32
    je .len5
    cmp al, 0xE9              ; JMP rel32
    je .len5

    ; --- PUSH/POP (1 byte) ---
    cmp al, 0x50
    jl .not_push
    cmp al, 0x5F
    jle .len1
.not_push:

    ; --- INC/DEC r32 (1 byte, legacy encoding) ---
    cmp al, 0x40
    jl .not_incdec
    cmp al, 0x4F
    jle .len1                ; (but these are REX in 64-bit... already handled)
.not_incdec:

    ; --- MOV r8, imm8 (2 bytes) ---
    cmp al, 0xB0
    jl .not_mov8
    cmp al, 0xB7
    jle .len2
.not_mov8:

    ; --- Two-byte opcode escape (0x0F) ---
    cmp al, 0x0F
    jne .not_twobyte
    movzx ecx, byte [rdi + 1]
    ; 0F 80-8F: Jcc rel32 (6 bytes total)
    cmp cl, 0x80
    jl .twobyte_other
    cmp cl, 0x8F
    jle .len6
.twobyte_other:
    ; Most 0F xx instructions are 3 bytes (0F xx modrm)
    jmp .len3
.not_twobyte:

    ; --- FF group (INC/DEC/CALL/JMP r/m) ---
    cmp al, 0xFF
    jne .not_ff
    ; Need to check ModR/M for addressing mode
    movzx ecx, byte [rdi + 1]
    ; Simple case: ModR/M with register operand (mod=11)
    mov edx, ecx
    shr edx, 6
    cmp dl, 3                 ; mod=11 → register direct
    je .len2_modrm
    ; Memory operand — complex, assume 6 bytes (common case with disp32)
    pop rcx                   ; prefix len
    lea eax, [ecx + 6]
    ret
.not_ff:

    ; --- 0x83: arithmetic r/m, imm8 ---
    cmp al, 0x83
    je .len3                  ; opcode + modrm + imm8

    ; --- 0x81: arithmetic r/m, imm32 ---
    cmp al, 0x81
    je .len6                  ; opcode + modrm + imm32

    ; --- Default: assume 3 bytes (safe-ish) ---
    jmp .len3

.len1:
    pop rcx
    lea eax, [ecx + 1]
    ret
.len2:
.len2_modrm:
    pop rcx
    lea eax, [ecx + 2]
    ret
.len3:
    pop rcx
    lea eax, [ecx + 3]
    ret
.len5:
    pop rcx
    lea eax, [ecx + 5]
    ret
.len6:
    pop rcx
    lea eax, [ecx + 6]
    ret

;; ============================================================
;; decode_region_instructions(header_ptr, callback)
;; rdi=region header, rsi=callback(instr_ptr, instr_len)
;; Walks instructions in a region, calling callback for each
;; ============================================================
global decode_region_instructions
decode_region_instructions:
    push rbx
    push r12
    push r13
    push r14

    mov r12, rdi              ; header ptr
    mov r13, rsi              ; callback

    ; Get code start and length
    movzx r14d, word [r12 + RHDR_CODE_LEN]
    lea rbx, [r12 + RHDR_SIZE]  ; code start

    xor ecx, ecx             ; offset from code start
.walk:
    cmp ecx, r14d
    jge .walk_done
    push rcx

    ; Decode instruction at current position
    lea rdi, [rbx + rcx]
    call decode_instruction_length
    ; eax = instruction length

    ; Call callback(instr_ptr, instr_len)
    pop rcx
    push rcx
    push rax
    lea rdi, [rbx + rcx]      ; instruction ptr
    mov esi, eax              ; length
    call r13

    pop rax
    pop rcx
    add ecx, eax              ; advance by instruction length
    jmp .walk

.walk_done:
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; count_region_instructions(header_ptr) → eax
;; rdi=region header
;; Returns number of instructions in the region
;; ============================================================
global count_region_instructions
count_region_instructions:
    push rbx
    push r12

    movzx r12d, word [rdi + RHDR_CODE_LEN]
    lea rbx, [rdi + RHDR_SIZE]

    xor ecx, ecx             ; offset
    xor edx, edx             ; count
.count:
    cmp ecx, r12d
    jge .count_done
    push rcx
    push rdx
    lea rdi, [rbx + rcx]
    call decode_instruction_length
    pop rdx
    pop rcx
    add ecx, eax
    inc edx
    jmp .count

.count_done:
    mov eax, edx
    pop r12
    pop rbx
    ret

;; ============================================================
;; decode_instruction_full(instr_ptr, out_decoded)
;; rdi = pointer to instruction bytes
;; rsi = pointer to DecodedInstruction structure (DI_SIZE bytes)
;; Returns: eax = instruction length (also stored in DI_LENGTH)
;;
;; Fills the DecodedInstruction structure with:
;;   - Length, opcode class, registers, immediate, etc.
;; Used by the Logic Probe for abstract interpretation.
;; ============================================================
global decode_instruction_full
decode_instruction_full:
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov r12, rdi              ; instruction pointer
    mov r13, rsi              ; output structure

    ; Zero the output structure
    xor eax, eax
    mov ecx, DI_SIZE / 8
    mov rdi, r13
.zero_loop:
    mov qword [rdi], rax
    add rdi, 8
    dec ecx
    jnz .zero_loop

    ; Default values
    mov byte [r13 + DI_DST_REG], 0xFF   ; no destination
    mov byte [r13 + DI_SRC_REG], 0xFF   ; no source
    mov byte [r13 + DI_OP_CLASS], OP_UNKNOWN

    ; Read first byte
    movzx eax, byte [r12]
    mov r14d, eax             ; save opcode
    xor r15d, r15d            ; prefix length

    ; Check for REX prefix (0x40-0x4F)
    cmp al, 0x40
    jl .no_rex
    cmp al, 0x4F
    jg .no_rex
    ; REX prefix present
    or byte [r13 + DI_FLAGS], DIF_IS_REX
    mov r15d, 1               ; prefix = 1 byte
    movzx eax, byte [r12 + 1]
    mov r14d, eax             ; actual opcode
    ; Check REX.W for 64-bit
    movzx ecx, byte [r12]
    test cl, 0x08
    jz .no_rex
    or byte [r13 + DI_FLAGS], DIF_IS_64BIT
.no_rex:

    ; === Classify the opcode ===
    mov eax, r14d

    ; --- NOP (0x90) ---
    cmp al, 0x90
    jne .not_nop
    mov byte [r13 + DI_OP_CLASS], OP_NOP
    mov byte [r13 + DI_LENGTH], 1
    jmp .add_prefix

.not_nop:
    ; --- RET (0xC3) ---
    cmp al, 0xC3
    jne .not_ret
    mov byte [r13 + DI_OP_CLASS], OP_FLOW_RET
    mov byte [r13 + DI_LENGTH], 1
    jmp .add_prefix

.not_ret:
    ; --- INT3 (0xCC) ---
    cmp al, 0xCC
    jne .not_int3
    mov byte [r13 + DI_OP_CLASS], OP_INTERRUPT
    mov byte [r13 + DI_LENGTH], 1
    jmp .add_prefix

.not_int3:
    ; --- HLT (0xF4) ---
    cmp al, 0xF4
    jne .not_hlt
    mov byte [r13 + DI_OP_CLASS], OP_PRIVILEGED
    mov byte [r13 + DI_LENGTH], 1
    jmp .add_prefix

.not_hlt:
    ; --- SYSCALL (0x0F 0x05) ---
    cmp al, 0x0F
    jne .not_syscall
    cmp byte [r12 + r15 + 1], 0x05
    jne .not_syscall
    mov byte [r13 + DI_OP_CLASS], OP_SYSCALL
    mov byte [r13 + DI_LENGTH], 2
    jmp .add_prefix

.not_syscall:
    ; --- PUSH reg (0x50-0x57) ---
    cmp al, 0x50
    jl .not_push_reg
    cmp al, 0x57
    jg .not_push_reg
    mov byte [r13 + DI_OP_CLASS], OP_STACK_PUSH
    mov byte [r13 + DI_LENGTH], 1
    ; Extract register (al - 0x50)
    sub al, 0x50
    mov [r13 + DI_SRC_REG], al
    jmp .add_prefix

.not_push_reg:
    ; --- POP reg (0x58-0x5F) ---
    cmp al, 0x58
    jl .not_pop_reg
    cmp al, 0x5F
    jg .not_pop_reg
    mov byte [r13 + DI_OP_CLASS], OP_STACK_POP
    mov byte [r13 + DI_LENGTH], 1
    ; Extract register (al - 0x58)
    sub al, 0x58
    mov [r13 + DI_DST_REG], al
    jmp .add_prefix

.not_pop_reg:
    ; --- JMP rel8 (0xEB) ---
    cmp al, 0xEB
    jne .not_jmp_rel8
    mov byte [r13 + DI_OP_CLASS], OP_FLOW_JUMP
    mov byte [r13 + DI_LENGTH], 2
    or byte [r13 + DI_FLAGS], DIF_HAS_IMM
    ; Compute target: current + 2 + rel8
    movsx ecx, byte [r12 + r15 + 1]
    add ecx, 2
    add ecx, r15d
    mov [r13 + DI_TARGET], ecx
    movsxd rax, ecx
    mov [r13 + DI_IMM64], rax
    jmp .add_prefix

.not_jmp_rel8:
    ; --- Jcc rel8 (0x70-0x7F) ---
    cmp al, 0x70
    jl .not_jcc_rel8
    cmp al, 0x7F
    jg .not_jcc_rel8
    mov byte [r13 + DI_OP_CLASS], OP_FLOW_JCC
    mov byte [r13 + DI_LENGTH], 2
    or byte [r13 + DI_FLAGS], DIF_HAS_IMM
    movsx ecx, byte [r12 + r15 + 1]
    add ecx, 2
    add ecx, r15d
    mov [r13 + DI_TARGET], ecx
    jmp .add_prefix

.not_jcc_rel8:
    ; --- JMP rel32 (0xE9) ---
    cmp al, 0xE9
    jne .not_jmp_rel32
    mov byte [r13 + DI_OP_CLASS], OP_FLOW_JUMP
    mov byte [r13 + DI_LENGTH], 5
    or byte [r13 + DI_FLAGS], DIF_HAS_IMM
    mov ecx, [r12 + r15 + 1]
    add ecx, 5
    add ecx, r15d
    mov [r13 + DI_TARGET], ecx
    mov [r13 + DI_IMM64], rcx
    jmp .add_prefix

.not_jmp_rel32:
    ; --- CALL rel32 (0xE8) ---
    cmp al, 0xE8
    jne .not_call_rel32
    mov byte [r13 + DI_OP_CLASS], OP_FLOW_CALL
    mov byte [r13 + DI_LENGTH], 5
    or byte [r13 + DI_FLAGS], DIF_HAS_IMM
    mov ecx, [r12 + r15 + 1]
    add ecx, 5
    add ecx, r15d
    mov [r13 + DI_TARGET], ecx
    mov [r13 + DI_IMM64], rcx
    jmp .add_prefix

.not_call_rel32:
    ; --- MOV reg, imm32 (0xB8-0xBF) ---
    cmp al, 0xB8
    jl .not_mov_imm32
    cmp al, 0xBF
    jg .not_mov_imm32
    mov byte [r13 + DI_OP_CLASS], OP_WRITE_REG
    mov byte [r13 + DI_LENGTH], 5
    or byte [r13 + DI_FLAGS], DIF_HAS_IMM
    sub al, 0xB8
    mov [r13 + DI_DST_REG], al
    mov ecx, [r12 + r15 + 1]
    mov [r13 + DI_IMM64], rcx
    jmp .add_prefix

.not_mov_imm32:
    ; --- CMP EAX, imm32 (0x3D) ---
    cmp al, 0x3D
    jne .not_cmp_eax
    mov byte [r13 + DI_OP_CLASS], OP_ARITH
    mov byte [r13 + DI_LENGTH], 5
    or byte [r13 + DI_FLAGS], DIF_HAS_IMM | DIF_WRITES_FLAGS
    mov byte [r13 + DI_DST_REG], REG_RAX
    mov ecx, [r12 + r15 + 1]
    mov [r13 + DI_IMM64], rcx
    jmp .add_prefix

.not_cmp_eax:
    ; --- XOR r/m, r (0x31) or r, r/m (0x33) ---
    cmp al, 0x31
    je .arith_modrm
    cmp al, 0x33
    je .arith_modrm
    ; --- ADD, SUB, AND, OR, CMP r/m, r ---
    cmp al, 0x01
    je .arith_modrm
    cmp al, 0x29
    je .arith_modrm
    cmp al, 0x21
    je .arith_modrm
    cmp al, 0x09
    je .arith_modrm
    cmp al, 0x39
    je .arith_modrm
    jmp .not_arith_modrm

.arith_modrm:
    mov byte [r13 + DI_OP_CLASS], OP_ARITH
    mov byte [r13 + DI_LENGTH], 2
    or byte [r13 + DI_FLAGS], DIF_HAS_MODRM | DIF_WRITES_FLAGS
    ; Extract ModR/M
    movzx ecx, byte [r12 + r15 + 1]
    mov [r13 + DI_MODRM], cl
    ; Extract registers from ModR/M
    mov eax, ecx
    shr eax, 3
    and eax, 7
    mov [r13 + DI_SRC_REG], al    ; reg field
    mov eax, ecx
    and eax, 7
    mov [r13 + DI_DST_REG], al    ; r/m field (if mod=11)
    jmp .add_prefix

.not_arith_modrm:
    ; --- MOV r/m, r (0x89) or r, r/m (0x8B) ---
    cmp al, 0x89
    je .mov_modrm
    cmp al, 0x8B
    je .mov_modrm
    jmp .not_mov_modrm

.mov_modrm:
    mov byte [r13 + DI_LENGTH], 2
    or byte [r13 + DI_FLAGS], DIF_HAS_MODRM
    movzx ecx, byte [r12 + r15 + 1]
    mov [r13 + DI_MODRM], cl
    ; Determine if write to reg or mem
    mov edx, ecx
    shr edx, 6
    cmp dl, 3               ; mod=11 means register
    je .mov_reg_reg
    ; Memory operand
    cmp r14d, 0x89
    je .mov_to_mem
    mov byte [r13 + DI_OP_CLASS], OP_READ_MEM
    jmp .mov_extract_regs
.mov_to_mem:
    mov byte [r13 + DI_OP_CLASS], OP_WRITE_MEM
    jmp .mov_extract_regs
.mov_reg_reg:
    mov byte [r13 + DI_OP_CLASS], OP_WRITE_REG
.mov_extract_regs:
    mov eax, ecx
    shr eax, 3
    and eax, 7
    mov [r13 + DI_SRC_REG], al
    mov eax, ecx
    and eax, 7
    mov [r13 + DI_DST_REG], al
    jmp .add_prefix

.not_mov_modrm:
    ; --- SUB RSP, imm8 (0x48 0x83 0xEC imm8) ---
    cmp al, 0x83
    jne .not_sub_rsp
    ; Check if r15 > 0 (REX present) and next is EC (sub rsp)
    cmp r15d, 1
    jl .not_sub_rsp
    movzx ecx, byte [r12 + r15 + 1]
    cmp cl, 0xEC
    jne .check_add_rsp
    mov byte [r13 + DI_OP_CLASS], OP_STACK_ADJ
    mov byte [r13 + DI_LENGTH], 3
    or byte [r13 + DI_FLAGS], DIF_HAS_IMM
    movzx eax, byte [r12 + r15 + 2]
    neg eax                   ; negative for sub
    movsxd rax, eax
    mov [r13 + DI_IMM64], rax
    jmp .add_prefix

.check_add_rsp:
    ; --- ADD RSP, imm8 (0x48 0x83 0xC4 imm8) ---
    cmp cl, 0xC4
    jne .not_sub_rsp
    mov byte [r13 + DI_OP_CLASS], OP_STACK_ADJ
    mov byte [r13 + DI_LENGTH], 3
    or byte [r13 + DI_FLAGS], DIF_HAS_IMM
    movzx eax, byte [r12 + r15 + 2]
    movsxd rax, eax
    mov [r13 + DI_IMM64], rax   ; positive for add
    jmp .add_prefix

.not_sub_rsp:
    ; --- 0x0F xx: Two-byte opcodes ---
    cmp r14d, 0x0F
    jne .not_twobyte
    movzx ecx, byte [r12 + r15 + 1]
    ; 0F 80-8F: Jcc rel32
    cmp cl, 0x80
    jl .twobyte_other
    cmp cl, 0x8F
    jg .twobyte_other
    mov byte [r13 + DI_OP_CLASS], OP_FLOW_JCC
    mov byte [r13 + DI_LENGTH], 6
    or byte [r13 + DI_FLAGS], DIF_HAS_IMM
    mov edx, [r12 + r15 + 2]
    add edx, 6
    add edx, r15d
    mov [r13 + DI_TARGET], edx
    jmp .add_prefix

.twobyte_other:
    mov byte [r13 + DI_LENGTH], 3
    jmp .add_prefix

.not_twobyte:
    ; Default: unknown, assume 1 byte (conservative)
    mov byte [r13 + DI_OP_CLASS], OP_UNKNOWN
    mov byte [r13 + DI_LENGTH], 1

.add_prefix:
    ; Add prefix length to total length
    movzx eax, byte [r13 + DI_LENGTH]
    add eax, r15d
    mov [r13 + DI_LENGTH], al

    ; Return total length
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; classify_opcode(opcode_byte) → eax (OP_* class)
;; Quick opcode classification without full decode.
;; rdi = opcode byte
;; ============================================================
global classify_opcode
classify_opcode:
    movzx eax, dil

    ; NOP
    cmp al, 0x90
    jne .not_nop
    mov eax, OP_NOP
    ret
.not_nop:
    ; RET
    cmp al, 0xC3
    jne .not_ret
    mov eax, OP_FLOW_RET
    ret
.not_ret:
    ; PUSH 0x50-0x57
    cmp al, 0x50
    jl .not_push
    cmp al, 0x57
    jle .is_push
    jmp .not_push
.is_push:
    mov eax, OP_STACK_PUSH
    ret
.not_push:
    ; POP 0x58-0x5F
    cmp al, 0x58
    jl .not_pop
    cmp al, 0x5F
    jle .is_pop
    jmp .not_pop
.is_pop:
    mov eax, OP_STACK_POP
    ret
.not_pop:
    ; CALL 0xE8
    cmp al, 0xE8
    jne .not_call
    mov eax, OP_FLOW_CALL
    ret
.not_call:
    ; JMP 0xE9, 0xEB
    cmp al, 0xE9
    je .is_jmp
    cmp al, 0xEB
    je .is_jmp
    jmp .not_jmp
.is_jmp:
    mov eax, OP_FLOW_JUMP
    ret
.not_jmp:
    ; Jcc 0x70-0x7F
    cmp al, 0x70
    jl .not_jcc
    cmp al, 0x7F
    jle .is_jcc
    jmp .not_jcc
.is_jcc:
    mov eax, OP_FLOW_JCC
    ret
.not_jcc:
    ; INT3 0xCC
    cmp al, 0xCC
    jne .not_int
    mov eax, OP_INTERRUPT
    ret
.not_int:
    ; Default
    mov eax, OP_UNKNOWN
    ret
