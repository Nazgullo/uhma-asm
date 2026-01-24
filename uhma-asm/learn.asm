; learn.asm — Learning: emit new patterns, strengthen/weaken existing
%include "syscalls.inc"
%include "constants.inc"

section .data
    learn_msg:      db "[LEARN] ctx=0x", 0
    learn_tok_msg:  db " → tok=0x", 0
    learn_dup_msg:  db " (exists, skip)", 10, 0
    learn_new_msg:  db " (new pattern)", 10, 0
    strengthen_msg: db "[STRENGTHEN] region at 0x", 0
    weaken_msg:     db "[WEAKEN] region at 0x", 0

section .text

extern print_cstr
extern print_hex32
extern print_hex64
extern print_newline
extern emit_dispatch_pattern
extern fire_hook

;; ============================================================
;; learn_pattern(ctx_hash, token_id)
;; rdi=context_hash (u64, lower 32 used), esi=token_id
;; Learns a new ctx→token association by emitting code
;; First checks if the pattern already exists (avoid duplicates)
;; ============================================================
global learn_pattern
learn_pattern:
    push rbx
    push r12
    push r13

    mov r12d, edi             ; ctx_hash (lower 32)
    mov r13d, esi             ; token_id

    mov rbx, SURFACE_BASE

    ; Print learn info
    push r12
    push r13
    lea rdi, [rel learn_msg]
    call print_cstr
    mov edi, r12d
    call print_hex32
    lea rdi, [rel learn_tok_msg]
    call print_cstr
    mov edi, r13d
    call print_hex32
    pop r13
    pop r12

    ; Check if this exact pattern already exists
    mov edi, r12d
    mov esi, r13d
    call find_existing_pattern
    test rax, rax
    jnz .already_exists

    ; Check region count limit
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov ecx, [rax]
    cmp ecx, REGION_TABLE_MAX - 4    ; leave room
    jge .table_full

    ; Emit new dispatch pattern
    lea rdi, [rel learn_new_msg]
    call print_cstr

    mov edi, r12d             ; ctx_hash
    mov esi, r13d             ; token_id
    ; Get current step for birth
    lea rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov edx, [rax]            ; birth_step (lower 32 of global step)
    call emit_dispatch_pattern

    ; Track recent emission count (for introspective state)
    lea rax, [rbx + STATE_OFFSET + ST_RECENT_EMITS]
    inc dword [rax]

    ; Check for auto-generalization opportunity
    mov edi, r12d             ; ctx_hash
    mov esi, r13d             ; token_id
    call check_auto_generalize

    ; Fire learn hook
    mov edi, HOOK_ON_LEARN
    mov esi, r13d
    call fire_hook

    jmp .done

.already_exists:
    ; Pattern exists — strengthen it (boost hit counter)
    push rax                  ; save region header ptr across print call
    lea rdi, [rel learn_dup_msg]
    call print_cstr
    pop rax

    ; Strengthen: increment hits on the existing region
    mov rdi, rax
    call strengthen_region

.table_full:
.done:
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; find_existing_pattern(ctx_hash_32, token_id) → rax
;; edi=ctx_hash, esi=token_id
;; Searches dispatch regions for matching ctx→token
;; Returns: header ptr if found, 0 if not
;; ============================================================
global find_existing_pattern
find_existing_pattern:
    push rbx
    push r12
    push r13

    mov r12d, edi             ; ctx_hash
    mov r13d, esi             ; token_id

    mov rbx, SURFACE_BASE
    lea rax, [rbx + REGION_TABLE_OFFSET]
    push rax                  ; save table base
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov ecx, [rax]
    pop rsi                   ; table base

    xor edx, edx             ; index
.search:
    cmp edx, ecx
    jge .not_found
    push rcx
    push rdx

    imul rdi, rdx, RTE_SIZE
    add rdi, rsi

    ; Check type
    movzx eax, word [rdi + RTE_TYPE]
    cmp eax, RTYPE_DISPATCH
    jne .skip

    ; Check not condemned
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_CONDEMNED
    jnz .skip

    ; Get header address
    mov rdi, [rdi + RTE_ADDR]

    ; Check the CMP immediate (ctx_hash)
    cmp byte [rdi + RHDR_SIZE], 0x3D
    jne .skip
    cmp [rdi + RHDR_SIZE + 1], r12d
    jne .skip

    ; Check the MOV immediate (token_id)
    ; At offset RHDR_SIZE + 7: mov eax, imm32 (B8 xx xx xx xx)
    cmp byte [rdi + RHDR_SIZE + 7], 0xB8
    jne .skip
    cmp [rdi + RHDR_SIZE + 8], r13d
    jne .skip

    ; Found match
    mov rax, rdi
    pop rdx
    pop rcx
    jmp .found

.skip:
    pop rdx
    pop rcx
    inc edx
    jmp .search

.not_found:
    xor eax, eax
.found:
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; strengthen_region(header_ptr)
;; rdi=region header ptr
;; Adds bonus hits to boost the region's fitness
;; ============================================================
global strengthen_region
strengthen_region:
    ; Add 2 bonus hits (reinforcement)
    add dword [rdi + RHDR_HITS], 2
    ret

;; ============================================================
;; weaken_region(header_ptr)
;; rdi=region header ptr
;; Adds penalty misses
;; ============================================================
global weaken_region
weaken_region:
    ; Add 1 penalty miss
    inc dword [rdi + RHDR_MISSES]
    ret

;; ============================================================
;; check_auto_generalize(ctx_hash, token_id)
;; edi=ctx_hash (just emitted), esi=token_id
;; Scans existing dispatch patterns for the same token with
;; a similar context. If found, emits a generalized pattern
;; (masked lower bits) — this is how schemas emerge from
;; repeated observation of the same token in similar contexts.
;; ============================================================
check_auto_generalize:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8                ; [rsp] = existing_ctx local var

    mov r12d, edi             ; new ctx_hash
    mov r13d, esi             ; token_id

    mov rbx, SURFACE_BASE
    lea r14, [rbx + REGION_TABLE_OFFSET]
    mov r15d, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    xor ecx, ecx
.ag_loop:
    cmp ecx, r15d
    jge .ag_done
    push rcx

    imul rdi, rcx, RTE_SIZE
    add rdi, r14
    movzx eax, word [rdi + RTE_TYPE]
    cmp eax, RTYPE_DISPATCH
    jne .ag_skip
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_CONDEMNED
    jnz .ag_skip

    mov rsi, [rdi + RTE_ADDR]
    cmp byte [rsi + RHDR_SIZE], 0x3D
    jne .ag_skip

    ; Check same token (mov eax, imm32 at offset +7)
    cmp byte [rsi + RHDR_SIZE + 7], 0xB8
    jne .ag_skip
    cmp [rsi + RHDR_SIZE + 8], r13d
    jne .ag_skip

    ; Same token! Check different context
    mov eax, [rsi + RHDR_SIZE + 1]
    cmp eax, r12d
    je .ag_skip               ; same context = same pattern

    ; Found overlap — save existing context to local var
    mov [rsp + 8], eax        ; +8 because rcx is pushed

    ; --- Emit schema for NEW context ---
    mov edi, r12d
    and edi, 0xFFFFFFF0
    mov esi, r13d
    call find_existing_pattern
    test rax, rax
    jnz .ag_new_exists
    cmp dword [rbx + STATE_OFFSET + ST_REGION_COUNT], REGION_TABLE_MAX - 4
    jge .ag_done_pop
    mov edi, r12d
    and edi, 0xFFFFFFF0
    mov esi, r13d
    mov edx, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    call emit_dispatch_pattern
    inc dword [rbx + STATE_OFFSET + ST_RECENT_EMITS]
.ag_new_exists:

    ; --- Emit schema for EXISTING context ---
    mov eax, [rsp + 8]       ; reload existing_ctx
    mov edi, eax
    and edi, 0xFFFFFFF0
    mov esi, r13d
    call find_existing_pattern
    test rax, rax
    jnz .ag_done_pop          ; already exists
    cmp dword [rbx + STATE_OFFSET + ST_REGION_COUNT], REGION_TABLE_MAX - 4
    jge .ag_done_pop
    mov eax, [rsp + 8]       ; reload existing_ctx
    mov edi, eax
    and edi, 0xFFFFFFF0
    mov esi, r13d
    mov edx, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    call emit_dispatch_pattern
    inc dword [rbx + STATE_OFFSET + ST_RECENT_EMITS]

    ; One generalization per learn call — done
    pop rcx
    jmp .ag_done

.ag_done_pop:
    pop rcx
    jmp .ag_done
.ag_skip:
    pop rcx
    inc ecx
    jmp .ag_loop
.ag_done:
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret
