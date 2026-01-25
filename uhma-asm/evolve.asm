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
extern sym_observe_mod
extern sym_record_anomaly

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
    sub rsp, EVOLVE_POOL_SIZE * 16  ; pool: (index:u32, fitness:f32) × N

    mov rbx, SURFACE_BASE

    ; --- Metabolic cost: evolution is expensive ---
    movsd xmm0, [rbx + STATE_OFFSET + ST_ENERGY]
    mov rax, ENERGY_STARVATION
    movq xmm1, rax
    ucomisd xmm0, xmm1
    jbe .evolve_exit           ; too hungry to evolve
    mov rax, ENERGY_EVOLVE_COST
    movq xmm1, rax
    subsd xmm0, xmm1
    xorpd xmm2, xmm2
    maxsd xmm0, xmm2
    movsd [rbx + STATE_OFFSET + ST_ENERGY], xmm0
    addsd xmm1, [rbx + STATE_OFFSET + ST_ENERGY_SPENT]
    movsd [rbx + STATE_OFFSET + ST_ENERGY_SPENT], xmm1

    ; --- Select candidates by FITNESS (accuracy × diversity) ---
    lea r12, [rbx + REGION_TABLE_OFFSET]
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov r13d, [rax]

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

    ; Must have data (hits + misses > 2)
    mov eax, [rdi + RTE_HITS]
    mov edx, [rdi + RTE_MISSES]
    add edx, eax
    cmp edx, 2
    jl .skip_select

    ; Compute fitness = accuracy * (1 + log2(hits))
    ; accuracy = hits / (hits + misses)
    cvtsi2ss xmm0, eax       ; hits
    cvtsi2ss xmm1, edx       ; total
    divss xmm0, xmm1         ; accuracy

    ; Diversity bonus: regions with unique contexts score higher
    ; Simple: use resonance field (co-fire correlation) as diversity indicator
    mov rsi, [rdi + RTE_ADDR]
    movsd xmm2, [rsi + RHDR_RESONANCE]
    cvtsd2ss xmm2, xmm2
    mov edx, 0x3F800000       ; 1.0f
    movd xmm3, edx
    addss xmm2, xmm3         ; 1.0 + resonance
    mulss xmm0, xmm2         ; fitness = accuracy * (1 + resonance)

    ; Add to pool with fitness score
    pop rcx
    push rcx
    imul eax, r14d, 16
    mov [rsp + 8 + rax], ecx           ; index
    movss [rsp + 8 + rax + 4], xmm0   ; fitness
    inc r14d

.skip_select:
    pop rcx
    inc ecx
    jmp .select_loop

.select_done:
    ; Need at least 1 candidate
    test r14d, r14d
    jz .evolve_exit

    ; --- Sort pool by fitness (simple insertion for small N) ---
    ; Find best fitness entry, use as first candidate
    xor ecx, ecx
    xor edx, edx              ; best_idx
    movss xmm0, [rsp + 4]    ; best_fitness = pool[0].fitness
    mov esi, 1
.sort_loop:
    cmp esi, r14d
    jge .sort_done
    imul eax, esi, 16
    movss xmm1, [rsp + rax + 4]
    comiss xmm1, xmm0
    jbe .sort_next
    movss xmm0, xmm1
    mov edx, esi
.sort_next:
    inc esi
    jmp .sort_loop
.sort_done:
    ; edx = index of best candidate in pool
    mov r15d, edx              ; save best_idx in callee-saved register

    ; --- Reproduce best candidate ---
    imul eax, r15d, 16
    mov ecx, [rsp + rax]      ; best candidate's region index
    mov edi, ecx
    call evolve_reproduce

    ; --- Mutate second-best if available ---
    cmp r14d, 2
    jl .no_mutate
    ; Find second-best (first entry that's not the best)
    xor esi, esi
    cmp esi, r15d
    jne .got_second
    mov esi, 1
.got_second:
    imul eax, esi, 16
    mov ecx, [rsp + rax]
    mov edi, ecx
    call evolve_mutate
.no_mutate:

    ; --- Crossover if 2+ candidates ---
    cmp r14d, 2
    jl .no_crossover
    imul eax, r15d, 16
    mov edi, [rsp + rax]      ; parent A (best)
    xor esi, esi
    cmp esi, r15d
    jne .cross_b
    mov esi, 1
.cross_b:
    imul eax, esi, 16
    mov esi, [rsp + rax]      ; parent B (second)
    call evolve_crossover
.no_crossover:

    lea rdi, [rel evolve_done]
    call print_cstr

    ; Fire evolve hook
    mov edi, HOOK_ON_EVOLVE
    xor esi, esi
    call fire_hook

.evolve_exit:
    add rsp, EVOLVE_POOL_SIZE * 16
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
