; evolve.asm — Evolution: reproduce, mutate, crossover regions
%include "syscalls.inc"
%include "constants.inc"

section .data
    evolve_msg:     db "[EVOLVE] ", 0
    reproduce_msg:  db "Reproducing region ", 0
    mutate_msg:     db "Mutating region ", 0
    crossover_msg:  db "Crossover regions ", 0
    evolve_and:     db " × ", 0
    evolve_nl:      db 10, 0
    evolve_done:    db "[EVOLVE] Cycle complete", 10, 0

section .text

extern print_cstr
extern print_u64
extern print_newline
extern region_alloc
extern emit_dispatch_pattern
extern fire_hook
extern sys_getrandom
extern gate_test_modification

;; ============================================================
;; evolve_cycle
;; One evolution cycle:
;; 1. Select top-N regions by fitness (hits / (hits + misses))
;; 2. Reproduce (copy with small mutation)
;; 3. Optionally crossover two parents
;; ============================================================
global evolve_cycle
evolve_cycle:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, EVOLVE_POOL_SIZE * 8  ; pool of candidate indices

    mov rbx, SURFACE_BASE

    ; --- Select candidates: top regions by hit count ---
    lea r12, [rbx + REGION_TABLE_OFFSET]
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov r13d, [rax]

    ; Find top-performing dispatch regions
    ; Simple approach: iterate and keep top EVOLVE_POOL_SIZE by hits
    xor r14d, r14d            ; pool count
    xor ecx, ecx             ; scan index

.select_loop:
    cmp ecx, r13d
    jge .select_done
    cmp r14d, EVOLVE_POOL_SIZE
    jge .select_done

    push rcx
    imul rdi, rcx, RTE_SIZE
    add rdi, r12

    ; Only DISPATCH, ACTIVE, not CONDEMNED
    movzx eax, word [rdi + RTE_TYPE]
    cmp eax, RTYPE_DISPATCH
    jne .skip_select
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_CONDEMNED
    jnz .skip_select
    test eax, RFLAG_ACTIVE
    jz .skip_select

    ; Must have some hits
    mov eax, [rdi + RTE_HITS]
    test eax, eax
    jz .skip_select

    ; Add to pool
    pop rcx
    push rcx
    mov [rsp + 8 + r14 * 8], ecx  ; store index (offset for pushed rcx)
    inc r14d

.skip_select:
    pop rcx
    inc ecx
    jmp .select_loop

.select_done:
    ; Need at least 1 candidate
    test r14d, r14d
    jz .evolve_exit

    ; --- Reproduce top candidate ---
    mov ecx, [rsp]            ; first candidate index (best by scan order)
    mov edi, ecx
    call evolve_reproduce

    ; --- Mutate second candidate if available ---
    cmp r14d, 2
    jl .no_mutate
    mov ecx, [rsp + 8]        ; second candidate
    mov edi, ecx
    call evolve_mutate
.no_mutate:

    ; --- Crossover if 2+ candidates ---
    cmp r14d, 2
    jl .no_crossover
    mov edi, [rsp]            ; parent A
    mov esi, [rsp + 8]        ; parent B
    call evolve_crossover
.no_crossover:

    lea rdi, [rel evolve_done]
    call print_cstr

    ; Fire evolve hook
    mov edi, HOOK_ON_EVOLVE
    xor esi, esi
    call fire_hook

.evolve_exit:
    add rsp, EVOLVE_POOL_SIZE * 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; evolve_reproduce(region_index)
;; edi=index of region to reproduce
;; Creates a copy with same ctx/token (exact duplicate)
;; ============================================================
global evolve_reproduce
evolve_reproduce:
    push rbx
    push r12

    mov r12d, edi
    mov rbx, SURFACE_BASE

    ; Print
    push r12
    lea rdi, [rel evolve_msg]
    call print_cstr
    lea rdi, [rel reproduce_msg]
    call print_cstr
    movzx rdi, r12w
    call print_u64
    call print_newline
    pop r12

    ; Get source region's ctx and token
    lea rax, [rbx + REGION_TABLE_OFFSET]
    imul rcx, r12, RTE_SIZE
    add rax, rcx
    mov rsi, [rax + RTE_ADDR]  ; header ptr

    ; Read ctx from cmp instruction
    cmp byte [rsi + RHDR_SIZE], 0x3D
    jne .done
    mov edi, [rsi + RHDR_SIZE + 1]   ; ctx_hash
    mov esi, [rsi + RHDR_SIZE + 8]   ; token_id (from mov eax, imm32)

    ; Emit as new region (duplicate)
    lea rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov edx, [rax]
    call emit_dispatch_pattern

.done:
    pop r12
    pop rbx
    ret

;; ============================================================
;; evolve_mutate(region_index)
;; edi=index
;; Creates a copy with slightly modified context hash
;; ============================================================
global evolve_mutate
evolve_mutate:
    push rbx
    push r12
    sub rsp, 8

    mov r12d, edi
    mov rbx, SURFACE_BASE

    ; Print
    push r12
    lea rdi, [rel evolve_msg]
    call print_cstr
    lea rdi, [rel mutate_msg]
    call print_cstr
    movzx rdi, r12w
    call print_u64
    call print_newline
    pop r12

    ; Get source region
    lea rax, [rbx + REGION_TABLE_OFFSET]
    imul rcx, r12, RTE_SIZE
    add rax, rcx
    mov rsi, [rax + RTE_ADDR]

    cmp byte [rsi + RHDR_SIZE], 0x3D
    jne .done

    mov edi, [rsi + RHDR_SIZE + 1]   ; ctx_hash
    mov r12d, [rsi + RHDR_SIZE + 8]  ; token_id

    ; Mutate: flip a random bit in the context hash
    lea rdi, [rsp]
    mov rsi, 4
    xor edx, edx
    mov rax, SYS_GETRANDOM
    syscall
    mov eax, [rsp]
    and eax, 0x1F             ; bit position 0-31
    mov ecx, eax
    mov eax, 1
    shl eax, cl               ; bit mask
    xor edi, eax              ; flip one bit in ctx_hash

    ; Keep same token prediction
    mov esi, r12d
    lea rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov edx, [rax]
    call emit_dispatch_pattern

.done:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; evolve_crossover(index_a, index_b)
;; edi=parent A index, esi=parent B index
;; Creates offspring with A's context and B's token prediction
;; ============================================================
global evolve_crossover
evolve_crossover:
    push rbx
    push r12
    push r13

    mov r12d, edi             ; parent A
    mov r13d, esi             ; parent B

    mov rbx, SURFACE_BASE

    ; Print
    push r12
    push r13
    lea rdi, [rel evolve_msg]
    call print_cstr
    lea rdi, [rel crossover_msg]
    call print_cstr
    movzx rdi, r12w
    call print_u64
    lea rdi, [rel evolve_and]
    call print_cstr
    movzx rdi, r13w
    call print_u64
    call print_newline
    pop r13
    pop r12

    ; Get parent A's context
    lea rax, [rbx + REGION_TABLE_OFFSET]
    imul rcx, r12, RTE_SIZE
    add rax, rcx
    mov rsi, [rax + RTE_ADDR]
    cmp byte [rsi + RHDR_SIZE], 0x3D
    jne .done
    mov edi, [rsi + RHDR_SIZE + 1]   ; A's ctx_hash

    ; Get parent B's token
    lea rax, [rbx + REGION_TABLE_OFFSET]
    imul rcx, r13, RTE_SIZE
    add rax, rcx
    mov rsi, [rax + RTE_ADDR]
    cmp byte [rsi + RHDR_SIZE + 7], 0xB8
    jne .done
    mov esi, [rsi + RHDR_SIZE + 8]   ; B's token_id

    ; Emit offspring: A's context → B's prediction
    push rdi
    push rsi
    lea rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov edx, [rax]
    pop rsi
    pop rdi
    call emit_dispatch_pattern

.done:
    pop r13
    pop r12
    pop rbx
    ret
