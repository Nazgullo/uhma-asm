; genes.asm — Gene Pool: metabolic recycling of condemned regions
;
; ENTRY POINTS:
;   gene_pool_init()                  - zero out gene pool
;   gene_extract(region_ptr)          - extract ctx/token/fitness from dying region
;   gene_pool_add(ctx, token, fitness)- add gene, replace lowest if full
;   gene_pool_sample()                → ctx in eax, token in edx (or 0 if empty)
;   gene_pool_show()                  - display pool status and top genes
;
; GENE STRUCTURE (16 bytes each):
;   [0-3]  ctx_hash   - context hash pattern matched
;   [4-7]  token_id   - predicted token
;   [8-15] fitness    - f64 fitness at time of death (hits/(hits+miss))
;
; POOL POLICY:
;   Fixed size: GENE_POOL_SIZE entries (default 64)
;   When full: find lowest-fitness gene, replace if new gene is fitter
;   Minimum fitness threshold: gene_min_fitness (0.05) to avoid junk
;
; CALLED BY:
;   surface.asm: region_condemn() extracts gene before death
;   evolve.asm:  evolve_from_gene_pool() resurrects genes
;
%include "syscalls.inc"
%include "constants.inc"

section .data
    gene_msg:           db "[GENES] ", 0
    gene_extract_msg:   db "Extracted: ctx=0x", 0
    gene_token_msg:     db " token=0x", 0
    gene_fitness_msg:   db " fitness=", 0
    gene_replace_msg:   db " (replaced idx ", 0
    gene_close_msg:     db ")", 0
    gene_sample_msg:    db "Sampled: idx=", 0
    gene_reuse_msg:     db "Reusing gene for evolution", 10, 0
    gene_nl:            db 10, 0
    gene_status_hdr:    db "--- Gene Pool Status ---", 10, 0
    gene_count_lbl:     db "  Active genes: ", 0
    gene_extracted_lbl: db "  Total extracted: ", 0
    gene_reused_lbl:    db "  Total reused: ", 0
    gene_lowest_lbl:    db "  Lowest fitness: ", 0
    gene_at_idx_lbl:    db " at idx ", 0
    gene_list_hdr:      db "  Top genes:", 10, 0
    gene_entry_lbl:     db "    [", 0
    gene_ctx_lbl:       db "] ctx=0x", 0
    gene_tok_lbl:       db " tok=0x", 0
    gene_fit_lbl:       db " fit=", 0
    gene_hits_lbl:      db " h/m=", 0
    gene_slash:         db "/", 0

    align 8
    gene_min_fitness:   dq 0.05         ; minimum fitness to extract a gene

section .text

extern print_cstr
extern print_hex32
extern print_u64
extern print_f32
extern print_newline
extern introspect_region
extern sys_getrandom

;; ============================================================
;; gene_pool_init
;; Initialize the gene pool (zero all entries)
;; Called from surface_init
;; ============================================================
global gene_pool_init
gene_pool_init:
    push rbx
    mov rbx, SURFACE_BASE

    ; Zero the gene pool area
    ; NOTE: GENE_POOL_OFFSET > 32-bit, lea would sign-extend. Use single 64-bit immediate.
    mov rdi, SURFACE_BASE + GENE_POOL_OFFSET
    xor eax, eax
    mov ecx, GENE_POOL_SIZE / 8
.zero_loop:
    mov qword [rdi], rax
    add rdi, 8
    dec ecx
    jnz .zero_loop

    ; Initialize state fields
    mov dword [rbx + STATE_OFFSET + ST_GENE_COUNT], 0
    mov dword [rbx + STATE_OFFSET + ST_GENES_EXTRACTED], 0
    mov dword [rbx + STATE_OFFSET + ST_GENES_REUSED], 0
    mov dword [rbx + STATE_OFFSET + ST_GENE_LOWEST_IDX], 0
    ; Set lowest fitness to max (1.0) initially
    mov eax, 0x3F800000               ; 1.0f
    mov [rbx + STATE_OFFSET + ST_GENE_LOWEST_FIT], eax

    pop rbx
    ret

;; ============================================================
;; gene_extract(region_header_ptr)
;; rdi = pointer to region header (being condemned)
;; Extract the region's (ctx_hash, token_id) pair and fitness.
;; Add to gene pool if the region had any successful predictions.
;; Returns: 1 if extracted, 0 if not worth extracting
;; ============================================================
global gene_extract
gene_extract:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 24                       ; local storage

    mov r12, rdi                      ; save region header ptr
    mov rbx, SURFACE_BASE

    ; Check if region has any hits (worth extracting)
    mov eax, [r12 + RHDR_HITS]
    test eax, eax
    jz .not_worth                     ; no hits = no useful knowledge

    ; Compute fitness = hits / (hits + misses)
    mov r13d, eax                     ; r13d = hits
    mov edx, [r12 + RHDR_MISSES]
    mov r14d, edx                     ; r14d = misses
    add edx, eax                      ; edx = total
    test edx, edx
    jz .not_worth

    cvtsi2ss xmm0, eax                ; hits
    cvtsi2ss xmm1, edx                ; total
    divss xmm0, xmm1                  ; fitness
    movss [rsp], xmm0                 ; save fitness

    ; Check minimum fitness threshold
    cvtss2sd xmm1, xmm0
    movsd xmm2, [rel gene_min_fitness]
    ucomisd xmm1, xmm2
    jb .not_worth                     ; below minimum, don't extract

    ; Use introspect_region to decode ctx_hash and pred_token
    mov rdi, r12
    call introspect_region            ; returns semantic type in eax
    ; After call: we need to re-decode to get ctx_hash and token
    ; introspect_region leaves r12d = ctx_hash, r13d = pred_token in its impl
    ; But those are callee-saved, so they're clobbered. Let's decode directly.

    ; Decode ctx_hash from region code (cmp eax, imm32 at offset 0)
    lea rsi, [r12 + RHDR_SIZE]        ; code body
    cmp byte [rsi], 0x3D              ; cmp eax, imm32?
    jne .not_worth                    ; can't decode, skip
    mov r15d, [rsi + 1]               ; ctx_hash

    ; Decode token_id (mov eax, imm32 - scan for 0xB8)
    movzx ecx, word [r12 + RHDR_CODE_LEN]
    lea rdi, [rsi + 5]                ; skip past cmp
    lea rdx, [rsi + rcx]              ; end of code
    xor eax, eax                      ; token_id (0 = not found)
.scan_token:
    cmp rdi, rdx
    jge .got_token
    cmp byte [rdi], 0xB8
    jne .scan_next_token
    mov eax, [rdi + 1]                ; found token_id
    jmp .got_token
.scan_next_token:
    inc rdi
    jmp .scan_token

.got_token:
    test eax, eax
    jz .not_worth                     ; couldn't find token prediction
    mov [rsp + 4], eax                ; save token_id
    mov [rsp + 8], r15d               ; save ctx_hash

    ; Now add to gene pool
    ; Args: ctx_hash=r15d, token_id=[rsp+4], fitness=[rsp], hits=r13d, misses=r14d
    mov edi, r15d                     ; ctx_hash
    mov esi, [rsp + 4]                ; token_id
    movss xmm0, [rsp]                 ; fitness
    mov edx, r13d                     ; hits
    mov ecx, r14d                     ; misses
    call gene_pool_add

    ; Print extraction message
    push rax
    lea rdi, [rel gene_msg]
    call print_cstr
    lea rdi, [rel gene_extract_msg]
    call print_cstr
    mov edi, [rsp + 16]               ; ctx_hash (offset shifted by push)
    call print_hex32
    lea rdi, [rel gene_token_msg]
    call print_cstr
    mov edi, [rsp + 12]               ; token_id
    call print_hex32
    lea rdi, [rel gene_fitness_msg]
    call print_cstr
    movss xmm0, [rsp + 8]             ; fitness
    call print_f32
    call print_newline
    pop rax

    ; Increment extraction counter
    inc dword [rbx + STATE_OFFSET + ST_GENES_EXTRACTED]

    mov eax, 1                        ; success
    jmp .done

.not_worth:
    xor eax, eax                      ; not extracted

.done:
    add rsp, 24
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; gene_pool_add(ctx_hash, token_id, fitness, hits, misses)
;; edi = ctx_hash, esi = token_id, xmm0 = fitness (f32)
;; edx = hits, ecx = misses
;; Add a gene to the pool. If full, replace lowest-fitness gene.
;; ============================================================
global gene_pool_add
gene_pool_add:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 24

    mov r12d, edi                     ; ctx_hash
    mov r13d, esi                     ; token_id
    movss [rsp], xmm0                 ; fitness
    mov r14d, edx                     ; hits
    mov r15d, ecx                     ; misses
    mov rbx, SURFACE_BASE

    ; Check if pool has space
    mov eax, [rbx + STATE_OFFSET + ST_GENE_COUNT]
    cmp eax, GENE_MAX
    jge .pool_full

    ; Pool has space - add at next available slot
    ; Find first empty slot (flags == 0)
    mov rsi, SURFACE_BASE + GENE_POOL_OFFSET
    xor ecx, ecx
.find_empty:
    cmp ecx, GENE_MAX
    jge .pool_full                    ; shouldn't happen, but safety
    imul edi, ecx, GENE_ENTRY_SIZE
    lea rdi, [rsi + rdi]
    cmp dword [rdi + GENE_FLAGS], 0
    je .found_slot
    inc ecx
    jmp .find_empty

.found_slot:
    ; ecx = slot index, rdi = slot ptr
    mov [rsp + 8], ecx                ; save slot index
    jmp .write_gene

.pool_full:
    ; Replace lowest-fitness gene
    mov ecx, [rbx + STATE_OFFSET + ST_GENE_LOWEST_IDX]
    mov [rsp + 8], ecx                ; slot to replace
    mov rsi, SURFACE_BASE + GENE_POOL_OFFSET
    imul edi, ecx, GENE_ENTRY_SIZE
    lea rdi, [rsi + rdi]
    ; Don't increment gene count (replacing, not adding)
    jmp .write_gene_no_inc

.write_gene:
    ; Increment gene count
    inc dword [rbx + STATE_OFFSET + ST_GENE_COUNT]

.write_gene_no_inc:
    ; rdi = slot ptr, write the gene entry
    mov [rdi + GENE_CTX_HASH], r12d
    mov [rdi + GENE_TOKEN_ID], r13d
    movss xmm0, [rsp]
    movss [rdi + GENE_FITNESS], xmm0
    mov [rdi + GENE_HITS], r14d
    mov [rdi + GENE_MISSES], r15d
    mov eax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov [rdi + GENE_BIRTH_STEP], eax
    mov dword [rdi + GENE_FLAGS], GFLAG_ACTIVE
    ; Set HIGH_FITNESS flag if fitness > 0.5
    movss xmm0, [rsp]
    mov eax, 0x3F000000               ; 0.5f
    movd xmm1, eax
    comiss xmm0, xmm1
    jbe .no_high_fit
    or dword [rdi + GENE_FLAGS], GFLAG_HIGH_FITNESS
.no_high_fit:

    ; Update lowest fitness tracking
    call gene_pool_update_lowest

    add rsp, 24
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; gene_pool_update_lowest
;; Scan pool to find lowest fitness gene, update ST_GENE_LOWEST_*
;; ============================================================
gene_pool_update_lowest:
    push rbx
    push r12
    mov rbx, SURFACE_BASE

    ; Initialize: lowest_fit = 2.0 (above max), lowest_idx = 0
    mov eax, 0x40000000               ; 2.0f
    movd xmm0, eax                    ; lowest fitness seen
    xor r12d, r12d                    ; lowest index

    mov rsi, SURFACE_BASE + GENE_POOL_OFFSET
    xor ecx, ecx
.scan:
    cmp ecx, GENE_MAX
    jge .scan_done

    imul edi, ecx, GENE_ENTRY_SIZE
    lea rdi, [rsi + rdi]

    ; Skip inactive slots
    test dword [rdi + GENE_FLAGS], GFLAG_ACTIVE
    jz .scan_next

    ; Compare fitness
    movss xmm1, [rdi + GENE_FITNESS]
    comiss xmm1, xmm0
    jae .scan_next                    ; not lower

    ; New lowest found
    movss xmm0, xmm1
    mov r12d, ecx

.scan_next:
    inc ecx
    jmp .scan

.scan_done:
    ; Store results
    mov [rbx + STATE_OFFSET + ST_GENE_LOWEST_IDX], r12d
    movss [rbx + STATE_OFFSET + ST_GENE_LOWEST_FIT], xmm0

    pop r12
    pop rbx
    ret

;; ============================================================
;; gene_pool_sample() → ctx_hash in eax, token_id in edx
;; Sample a random gene from the pool, weighted by fitness.
;; Returns: eax = ctx_hash, edx = token_id, xmm0 = fitness
;; Returns ctx_hash=0 if pool is empty
;; ============================================================
global gene_pool_sample
gene_pool_sample:
    push rbx
    push r12
    push r13
    sub rsp, 16

    mov rbx, SURFACE_BASE

    ; Check if pool is empty
    mov eax, [rbx + STATE_OFFSET + ST_GENE_COUNT]
    test eax, eax
    jz .empty

    ; Get random number
    lea rdi, [rsp]
    mov rsi, 4
    xor edx, edx
    mov rax, SYS_GETRANDOM
    syscall

    ; Compute total fitness for weighted sampling
    mov rsi, SURFACE_BASE + GENE_POOL_OFFSET
    xorps xmm2, xmm2                  ; total fitness
    xor ecx, ecx
.sum_fitness:
    cmp ecx, GENE_MAX
    jge .sum_done
    imul edi, ecx, GENE_ENTRY_SIZE
    lea rdi, [rsi + rdi]
    test dword [rdi + GENE_FLAGS], GFLAG_ACTIVE
    jz .sum_next
    addss xmm2, [rdi + GENE_FITNESS]
.sum_next:
    inc ecx
    jmp .sum_fitness
.sum_done:

    ; xmm2 = total fitness. Generate random threshold [0, total)
    mov eax, [rsp]
    and eax, 0x7FFFFFFF               ; positive
    cvtsi2ss xmm0, eax
    mov eax, 0x7FFFFFFF
    cvtsi2ss xmm1, eax
    divss xmm0, xmm1                  ; random [0, 1)
    mulss xmm0, xmm2                  ; random [0, total_fitness)
    ; xmm0 = threshold

    ; Scan and accumulate until we cross threshold
    xorps xmm1, xmm1                  ; accumulated
    xor ecx, ecx
    mov r12d, -1                      ; selected index
.select:
    cmp ecx, GENE_MAX
    jge .select_done
    imul edi, ecx, GENE_ENTRY_SIZE
    lea rdi, [rsi + rdi]
    test dword [rdi + GENE_FLAGS], GFLAG_ACTIVE
    jz .select_next
    addss xmm1, [rdi + GENE_FITNESS]
    comiss xmm1, xmm0
    jb .select_next
    mov r12d, ecx                     ; selected!
    jmp .select_done
.select_next:
    inc ecx
    jmp .select

.select_done:
    ; If no selection (shouldn't happen), pick first active
    cmp r12d, -1
    jne .got_selection
    xor ecx, ecx
.find_any:
    cmp ecx, GENE_MAX
    jge .empty
    imul edi, ecx, GENE_ENTRY_SIZE
    lea rdi, [rsi + rdi]
    test dword [rdi + GENE_FLAGS], GFLAG_ACTIVE
    jnz .found_any
    inc ecx
    jmp .find_any
.found_any:
    mov r12d, ecx

.got_selection:
    ; r12d = selected index
    imul eax, r12d, GENE_ENTRY_SIZE
    lea rdi, [rsi + rax]

    ; Return values
    mov eax, [rdi + GENE_CTX_HASH]
    mov edx, [rdi + GENE_TOKEN_ID]
    movss xmm0, [rdi + GENE_FITNESS]

    ; Mark as proven (used during evolution)
    or dword [rdi + GENE_FLAGS], GFLAG_PROVEN

    ; Increment reuse counter
    inc dword [rbx + STATE_OFFSET + ST_GENES_REUSED]

    jmp .done

.empty:
    xor eax, eax                      ; ctx_hash = 0 means no gene
    xor edx, edx
    xorps xmm0, xmm0

.done:
    add rsp, 16
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; gene_pool_show
;; Print gene pool status (for REPL "genes" command)
;; ============================================================
global gene_pool_show
gene_pool_show:
    push rbx
    push r12
    push r13
    mov rbx, SURFACE_BASE

    ; Header
    lea rdi, [rel gene_status_hdr]
    call print_cstr

    ; Active genes
    lea rdi, [rel gene_count_lbl]
    call print_cstr
    mov edi, [rbx + STATE_OFFSET + ST_GENE_COUNT]
    mov r12d, edi                     ; save count
    call print_u64
    call print_newline

    ; Total extracted
    lea rdi, [rel gene_extracted_lbl]
    call print_cstr
    mov edi, [rbx + STATE_OFFSET + ST_GENES_EXTRACTED]
    call print_u64
    call print_newline

    ; Total reused
    lea rdi, [rel gene_reused_lbl]
    call print_cstr
    mov edi, [rbx + STATE_OFFSET + ST_GENES_REUSED]
    call print_u64
    call print_newline

    ; Lowest fitness
    lea rdi, [rel gene_lowest_lbl]
    call print_cstr
    movss xmm0, [rbx + STATE_OFFSET + ST_GENE_LOWEST_FIT]
    call print_f32
    lea rdi, [rel gene_at_idx_lbl]
    call print_cstr
    mov edi, [rbx + STATE_OFFSET + ST_GENE_LOWEST_IDX]
    call print_u64
    call print_newline

    ; List top genes (first 10 active)
    test r12d, r12d
    jz .no_genes
    lea rdi, [rel gene_list_hdr]
    call print_cstr

    mov rsi, SURFACE_BASE + GENE_POOL_OFFSET
    xor ecx, ecx                      ; index
    xor r13d, r13d                    ; printed count
.list_loop:
    cmp ecx, GENE_MAX
    jge .list_done
    cmp r13d, 10                      ; max 10 entries
    jge .list_done

    push rcx
    imul edi, ecx, GENE_ENTRY_SIZE
    lea rdi, [rsi + rdi]

    test dword [rdi + GENE_FLAGS], GFLAG_ACTIVE
    jz .list_next

    ; Print entry
    push rdi
    push rsi
    lea rdi, [rel gene_entry_lbl]
    call print_cstr
    mov rdi, [rsp + 16]               ; index from stack
    call print_u64
    lea rdi, [rel gene_ctx_lbl]
    call print_cstr
    mov rdi, [rsp]                    ; entry ptr
    mov edi, [rdi + GENE_CTX_HASH]
    call print_hex32
    lea rdi, [rel gene_tok_lbl]
    call print_cstr
    mov rdi, [rsp]
    mov edi, [rdi + GENE_TOKEN_ID]
    call print_hex32
    lea rdi, [rel gene_fit_lbl]
    call print_cstr
    mov rdi, [rsp]
    movss xmm0, [rdi + GENE_FITNESS]
    call print_f32
    lea rdi, [rel gene_hits_lbl]
    call print_cstr
    mov rdi, [rsp]
    mov edi, [rdi + GENE_HITS]
    call print_u64
    lea rdi, [rel gene_slash]
    call print_cstr
    mov rdi, [rsp]
    mov edi, [rdi + GENE_MISSES]
    call print_u64
    call print_newline
    pop rsi
    pop rdi

    inc r13d

.list_next:
    pop rcx
    inc ecx
    jmp .list_loop

.list_done:
.no_genes:

    pop r13
    pop r12
    pop rbx
    ret
