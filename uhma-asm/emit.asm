; emit.asm — Code emission primitives: write x86 instructions to surface
%include "syscalls.inc"
%include "constants.inc"

section .data
    emit_msg:       db "[EMIT] New pattern at 0x", 0
    emit_ctx_msg:   db " ctx=0x", 0
    emit_tok_msg:   db " tok=0x", 0
    emit_nl:        db 10, 0

section .text

extern print_cstr
extern print_hex32
extern print_hex64
extern print_newline
extern region_alloc
extern fire_hook
extern wire_new_region

;; ============================================================
;; emit_dispatch_pattern(ctx_hash_32, token_id, birth_step)
;; edi=context hash (32-bit), esi=predicted token_id, edx=birth_step
;; Emits a new dispatch region:
;;   cmp eax, <ctx_hash>       ; 5 bytes (3D imm32)
;;   jne .skip                  ; 2 bytes (75 xx)
;;   inc dword [header+0]      ; 7 bytes (FF 05 rel32)... simplified
;;   mov eax, <token_id>       ; 5 bytes (B8 imm32)
;;   ret                        ; 1 byte  (C3)
;;   .skip:
;;   xor eax, eax              ; 2 bytes (31 C0)
;;   ret                        ; 1 byte  (C3)
;; Total: 23 bytes
;; Returns: rax = ptr to new region header
;; ============================================================
global emit_dispatch_pattern
emit_dispatch_pattern:
    push rbx
    push r12
    push r13
    push r14

    mov r12d, edi             ; ctx_hash
    mov r13d, esi             ; token_id
    mov r14d, edx             ; birth_step

    ; Allocate region: 17 bytes of code
    mov rdi, 17               ; code size
    mov rsi, RTYPE_DISPATCH   ; type
    mov edx, r14d             ; birth step
    call region_alloc
    mov rbx, rax              ; header ptr

    ; Now write the code at rbx + RHDR_SIZE
    lea rdi, [rbx + RHDR_SIZE]

    ; Byte 0-4: cmp eax, imm32 (opcode 3D)
    mov byte [rdi + 0], 0x3D
    mov [rdi + 1], r12d       ; ctx_hash immediate

    ; Byte 5-6: jne .skip (+15 bytes forward: skip over inc+mov+ret = 13 bytes)
    mov byte [rdi + 5], 0x75
    mov byte [rdi + 6], 13    ; jump offset to .skip

    ; Byte 7-13: inc dword [rbx + RHDR_HITS] — use absolute addressing
    ; Actually, use a simpler approach: inc dword [rip + offset]
    ; For simplicity, use: push rcx; mov rcx, header_addr; inc dword [rcx]; pop rcx
    ; That's too long. Instead, just skip the inc for now and rely on dispatch_predict
    ; to do the counting. Use the space for a NOP sled + mov + ret.
    ;
    ; Revised layout (23 bytes):
    ;   0-4:   cmp eax, imm32      (5)
    ;   5-6:   jne +11              (2)  → jumps to byte 18
    ;   7-11:  mov eax, imm32      (5)  predicted token
    ;   12:    nop                   (1)  padding
    ;   13-17: nop x5               (5)  padding for future inc
    ;   18-19: xor eax, eax        (2)  miss path
    ;   20:    ret                   (1)
    ;   Total = 21... let me redo this properly.
    ;
    ; Clean layout (20 bytes):
    ;   0-4:   cmp eax, <ctx>       (5)  3D xx xx xx xx
    ;   5-6:   jne +8               (2)  75 08
    ;   7-11:  mov eax, <tok>       (5)  B8 xx xx xx xx
    ;   12:    ret                   (1)  C3
    ;   13:    nop                   (1)  90 (alignment)
    ;   14-15: xor eax, eax         (2)  31 C0
    ;   16:    ret                   (1)  C3
    ; Total: 17 bytes. Let's use 20 with padding.

    ; Actually let me just emit a clean 17-byte pattern:
    ; Reset the region header code_len
    mov word [rbx + RHDR_CODE_LEN], 17

    ; cmp eax, ctx_hash
    mov byte [rdi + 0], 0x3D
    mov [rdi + 1], r12d

    ; jne +8 (skip mov+ret, land on xor)
    mov byte [rdi + 5], 0x75
    mov byte [rdi + 6], 7     ; +7 lands at offset 13

    ; mov eax, token_id (hit path)
    mov byte [rdi + 7], 0xB8
    mov [rdi + 8], r13d

    ; ret (hit path returns)
    mov byte [rdi + 12], 0xC3

    ; xor eax, eax (miss path: no prediction)
    mov byte [rdi + 13], 0x31
    mov byte [rdi + 14], 0xC0

    ; ret (miss path)
    mov byte [rdi + 15], 0xC3

    ; NOP padding to 17
    mov byte [rdi + 16], 0x90

    ; Print emission info
    push rbx
    lea rdi, [rel emit_msg]
    call print_cstr
    lea rdi, [rbx + RHDR_SIZE]
    call print_hex64

    lea rdi, [rel emit_ctx_msg]
    call print_cstr
    mov edi, r12d
    call print_hex32

    lea rdi, [rel emit_tok_msg]
    call print_cstr
    mov edi, r13d
    call print_hex32
    call print_newline
    pop rbx

    ; Fire emit hook
    mov edi, HOOK_ON_EMIT
    mov esi, r12d
    call fire_hook

    ; Wire new region into connectivity graph
    mov rdi, rbx              ; header ptr
    mov esi, r13d             ; token_id (for finding same-token regions)
    call wire_new_region

    mov rax, rbx              ; return header ptr
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; emit_nop_sled(ptr, count)
;; rdi=destination, esi=byte count
;; Fills with NOP (0x90)
;; ============================================================
global emit_nop_sled
emit_nop_sled:
    movzx ecx, si
.nop_loop:
    test ecx, ecx
    jz .nop_done
    mov byte [rdi], 0x90
    inc rdi
    dec ecx
    jmp .nop_loop
.nop_done:
    ret

;; ============================================================
;; emit_ret(ptr)
;; Writes a RET instruction at ptr
;; ============================================================
global emit_ret
emit_ret:
    mov byte [rdi], 0xC3
    ret

;; ============================================================
;; emit_int3(ptr)
;; Writes INT3 (breakpoint/trap) at ptr
;; ============================================================
global emit_int3
emit_int3:
    mov byte [rdi], 0xCC
    ret

;; ============================================================
;; emit_cmp_eax_imm32(ptr, imm32) → bytes written (5)
;; rdi=dest, esi=immediate
;; ============================================================
global emit_cmp_eax_imm32
emit_cmp_eax_imm32:
    mov byte [rdi], 0x3D
    mov [rdi + 1], esi
    mov eax, 5
    ret

;; ============================================================
;; emit_jne_rel8(ptr, offset) → bytes written (2)
;; rdi=dest, sil=relative offset
;; ============================================================
global emit_jne_rel8
emit_jne_rel8:
    mov byte [rdi], 0x75
    mov [rdi + 1], sil
    mov eax, 2
    ret

;; ============================================================
;; emit_mov_eax_imm32(ptr, imm32) → bytes written (5)
;; rdi=dest, esi=immediate
;; ============================================================
global emit_mov_eax_imm32
emit_mov_eax_imm32:
    mov byte [rdi], 0xB8
    mov [rdi + 1], esi
    mov eax, 5
    ret

;; ============================================================
;; emit_xor_eax_eax(ptr) → bytes written (2)
;; ============================================================
global emit_xor_eax_eax
emit_xor_eax_eax:
    mov byte [rdi], 0x31
    mov byte [rdi + 1], 0xC0
    mov eax, 2
    ret
