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
    evolve_gene_msg: db "Resurrecting from gene pool: ctx=0x", 0
    evolve_tok_msg:  db " token=0x", 0

section .text

extern print_cstr
extern print_u64
extern print_hex32
extern print_newline
extern region_alloc
extern emit_dispatch_pattern
extern fire_hook
extern sys_getrandom
extern gate_test_modification
extern sym_observe_mod
extern sym_record_anomaly
extern gene_pool_sample
extern receipt_resonate
extern emit_receipt_simple

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

    ; --- RESONANCE QUERY: Boost fitness if similar evolved patterns succeeded ---
    ; Query past EVOLVE events for similar contexts
    push rdi
    push rsi
    push rcx
    sub rsp, 8
    movss [rsp], xmm0        ; save fitness
    cmp byte [rsi + RHDR_SIZE], 0x3D
    jne .evolve_no_resonate
    mov edi, EVENT_EVOLVE
    mov esi, [rsi + RHDR_SIZE + 1]  ; ctx_hash from region
    xor edx, edx
    call receipt_resonate    ; → xmm0 = similarity to past EVOLVEs
    ; If high similarity (>0.5), check if those led to HITs
    mov rax, 0x3FE0000000000000     ; 0.5 f64
    movq xmm1, rax
    ucomisd xmm0, xmm1
    jbe .evolve_no_resonate
    ; Query for subsequent HITs
    mov edi, EVENT_HIT
    add rsp, 8
    pop rcx
    pop rsi
    push rsi
    push rcx
    sub rsp, 8
    mov esi, [rsi + RHDR_SIZE + 1]
    xor edx, edx
    call receipt_resonate    ; → xmm0 = HIT similarity
    ; Boost fitness by (1 + 0.3 * hit_similarity)
    cvtsd2ss xmm0, xmm0      ; convert to f32
    mov eax, 0x3E99999A      ; 0.3f
    movd xmm1, eax
    mulss xmm0, xmm1         ; 0.3 * hit_sim
    mov eax, 0x3F800000      ; 1.0f
    movd xmm1, eax
    addss xmm0, xmm1         ; 1 + 0.3 * hit_sim
    movss xmm2, [rsp]        ; restore fitness
    mulss xmm0, xmm2         ; fitness *= (1 + 0.3 * hit_sim)
    movss [rsp], xmm0        ; save boosted fitness
.evolve_no_resonate:
    movss xmm0, [rsp]        ; restore fitness
    add rsp, 8
    pop rcx
    pop rsi
    pop rdi

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

    ; --- Resurrect from gene pool (composted knowledge) ---
    ; Sample a proven pattern from the gene pool and recreate it
    call evolve_from_gene_pool

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

    ; Save ctx and token for receipt
    push rdi
    push rsi

    ; Emit as new region (duplicate)
    lea rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov edx, [rax]
    call emit_dispatch_pattern

    ; === EMIT RECEIPT: EVENT_EVOLVE ===
    pop rsi                   ; token_id
    pop rdi                   ; ctx_hash
    push rdi
    push rsi
    mov edx, esi              ; token_id
    mov esi, edi              ; ctx_hash
    mov edi, EVENT_EVOLVE     ; event_type
    mov eax, 0x3F800000       ; 1.0f confidence
    movd xmm0, eax
    call emit_receipt_simple
    pop rsi
    pop rdi

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

;; ============================================================
;; evolve_from_gene_pool
;; Sample from the gene pool and resurrect a pattern
;; This recycles composted knowledge from condemned regions
;; ============================================================
global evolve_from_gene_pool
evolve_from_gene_pool:
    push rbx
    push r12
    push r13
    sub rsp, 8

    mov rbx, SURFACE_BASE

    ; Sample a gene from the pool
    call gene_pool_sample             ; → eax = ctx_hash, edx = token_id, xmm0 = fitness

    ; Check if we got a valid gene (ctx_hash != 0)
    test eax, eax
    jz .no_gene

    ; Save the gene data
    mov r12d, eax                     ; ctx_hash
    mov r13d, edx                     ; token_id

    ; Print resurrection message
    push r12
    push r13
    lea rdi, [rel evolve_msg]
    call print_cstr
    lea rdi, [rel evolve_gene_msg]
    call print_cstr
    mov edi, r12d
    call print_hex32
    lea rdi, [rel evolve_tok_msg]
    call print_cstr
    mov edi, r13d
    call print_hex32
    call print_newline
    pop r13
    pop r12

    ; Emit a new region from the gene
    mov edi, r12d                     ; ctx_hash
    mov esi, r13d                     ; token_id
    mov edx, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    call emit_dispatch_pattern

.no_gene:
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    ret
