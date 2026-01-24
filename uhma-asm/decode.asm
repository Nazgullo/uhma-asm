; decode.asm — x86 instruction length decoder for self-reading
%include "syscalls.inc"
%include "constants.inc"

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
