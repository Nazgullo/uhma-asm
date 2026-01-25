; hooks.asm — Hook table management, fire_hook
%include "syscalls.inc"
%include "constants.inc"

section .text

extern journey_step

;; ============================================================
;; fire_hook(hook_id, arg)
;; edi=hook_id (0-21), esi=argument (context-dependent)
;; Iterates the hook's handler array, calls each registered handler
;; Each handler is called with: rdi=hook_id, rsi=arg
;; ============================================================
global fire_hook
fire_hook:
    push rbx
    push r12
    push r13
    push r14

    mov r12d, edi             ; hook_id
    mov r13d, esi             ; arg

    ; JOURNEY: record fire_hook
    push r12
    push r13
    mov edi, TRACE_FIRE_HOOK
    call journey_step
    pop r13
    pop r12

    ; Validate hook_id
    cmp r12d, NUM_HOOKS
    jge .done

    ; Calculate hook entry address in state block
    mov rbx, SURFACE_BASE
    lea rax, [rbx + STATE_OFFSET + ST_HOOKS]

    ; Offset: hook_id * HOOK_ENTRY_SIZE
    imul rcx, r12, HOOK_ENTRY_SIZE
    add rax, rcx              ; rax = hook entry base

    ; Read handler count
    movzx r14d, word [rax]    ; count
    test r14d, r14d
    jz .done                  ; no handlers

    ; Skip header (8 bytes: count:u16 + pad:u16 + pad:u32)
    add rax, 8                ; rax = start of fn_ptr array

    ; Iterate handlers
    xor ecx, ecx             ; index
.fire_loop:
    cmp ecx, r14d
    jge .done
    push rcx
    push rax

    ; Load function pointer
    mov rdx, [rax + rcx * 8]
    test rdx, rdx
    jz .skip_handler

    ; Safety: validate handler is in valid .text section
    ; Valid code range: 0x401000 - 0x408000 (program's .text)
    ; Reject: NULL, data sections (.bss/.data), surface region
    mov r8, 0x401000
    cmp rdx, r8
    jb .skip_handler                ; below .text = invalid (NULL, etc)
    mov r8, 0x408000
    cmp rdx, r8
    jae .skip_handler               ; above .text = invalid (data section, surface, etc)

.call_safe:
    ; Call handler(hook_id, arg)
    mov edi, r12d
    mov esi, r13d
    call rdx

.skip_handler:
    pop rax
    pop rcx
    inc ecx
    jmp .fire_loop

.done:
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; hook_register(hook_id, handler_ptr)
;; edi=hook_id, rsi=function pointer
;; Adds a handler to the hook's array
;; Returns: 0=success, -1=full or invalid
;; ============================================================
global hook_register
hook_register:
    push rbx

    ; Validate
    cmp edi, NUM_HOOKS
    jge .fail

    mov rbx, SURFACE_BASE
    lea rax, [rbx + STATE_OFFSET + ST_HOOKS]

    ; Find hook entry
    imul rcx, rdi, HOOK_ENTRY_SIZE
    add rax, rcx

    ; Check if full
    movzx ecx, word [rax]     ; current count
    cmp ecx, HOOK_MAX_HANDLERS
    jge .fail

    ; Add handler at position [count]
    mov [rax + 8 + rcx * 8], rsi

    ; Increment count
    inc word [rax]

    xor eax, eax              ; success
    pop rbx
    ret

.fail:
    mov eax, -1
    pop rbx
    ret

;; ============================================================
;; hook_unregister(hook_id, handler_ptr)
;; edi=hook_id, rsi=function pointer to remove
;; Removes first matching handler from the hook
;; Returns: 0=success, -1=not found
;; ============================================================
global hook_unregister
hook_unregister:
    push rbx
    push r12

    cmp edi, NUM_HOOKS
    jge .fail

    mov rbx, SURFACE_BASE
    lea rax, [rbx + STATE_OFFSET + ST_HOOKS]
    imul rcx, rdi, HOOK_ENTRY_SIZE
    add rax, rcx

    movzx ecx, word [rax]     ; count
    test ecx, ecx
    jz .fail

    ; Search for the handler
    xor edx, edx
.search:
    cmp edx, ecx
    jge .fail
    cmp [rax + 8 + rdx * 8], rsi
    je .found
    inc edx
    jmp .search

.found:
    ; Shift remaining handlers down
    mov r12d, edx             ; found index
.shift:
    inc edx
    cmp edx, ecx
    jge .shift_done
    mov rbx, [rax + 8 + rdx * 8]
    mov [rax + 8 + rdx * 8 - 8], rbx
    jmp .shift
.shift_done:
    ; Clear last slot
    dec ecx
    mov qword [rax + 8 + rcx * 8], 0
    ; Decrement count
    dec word [rax]

    xor eax, eax
    pop r12
    pop rbx
    ret

.fail:
    mov eax, -1
    pop r12
    pop rbx
    ret

;; ============================================================
;; hook_clear(hook_id)
;; edi=hook_id
;; Removes all handlers from a hook
;; ============================================================
global hook_clear
hook_clear:
    cmp edi, NUM_HOOKS
    jge .done

    mov rax, SURFACE_BASE
    lea rax, [rax + STATE_OFFSET + ST_HOOKS]
    imul rcx, rdi, HOOK_ENTRY_SIZE
    add rax, rcx

    ; Zero count
    mov word [rax], 0

    ; Zero all pointers
    xor ecx, ecx
.clear_loop:
    cmp ecx, HOOK_MAX_HANDLERS
    jge .done
    mov qword [rax + 8 + rcx * 8], 0
    inc ecx
    jmp .clear_loop

.done:
    ret

;; ============================================================
;; hook_get_count(hook_id) → eax
;; edi=hook_id
;; ============================================================
global hook_get_count
hook_get_count:
    cmp edi, NUM_HOOKS
    jge .zero

    mov rax, SURFACE_BASE
    lea rax, [rax + STATE_OFFSET + ST_HOOKS]
    imul rcx, rdi, HOOK_ENTRY_SIZE
    add rax, rcx
    movzx eax, word [rax]
    ret
.zero:
    xor eax, eax
    ret
