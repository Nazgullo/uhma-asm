; dreams.asm — Offline consolidation: replay misses, speculate, commit
%include "syscalls.inc"
%include "constants.inc"

section .data
    dream_start:    db "[DREAM] Beginning consolidation cycle...", 10, 0
    dream_end:      db "[DREAM] Cycle complete. Replayed: ", 0
    dream_emit:     db "[DREAM] Speculative pattern emitted", 10, 0
    dream_skip:     db "[DREAM] Pattern rejected (exists or weak)", 10, 0
    dream_count:    db " entries, emitted: ", 0
    dream_nl:       db 10, 0
    schema_msg:     db "[DREAM] Schema extracted (generalized pattern)", 10, 0

section .text

extern print_cstr
extern print_u64
extern print_newline
extern find_existing_pattern
extern emit_dispatch_pattern
extern fire_hook
extern gate_test_modification
extern journey_step
extern sym_scan_for_discoveries

;; ============================================================
;; dream_cycle
;; Offline replay of the miss buffer:
;; 1. Iterate miss buffer entries
;; 2. For each (context, token) pair:
;;    a. Check if a pattern already exists
;;    b. If not, emit a speculative pattern (NURSERY flag)
;; 3. Consolidation: patterns survive if they help
;; ============================================================
global dream_cycle
dream_cycle:
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, SURFACE_BASE

    ; JOURNEY: record dream_cycle
    mov edi, TRACE_DREAM_CYCLE
    call journey_step

    ; Fire dream start hook
    mov edi, HOOK_ON_DREAM_START
    xor esi, esi
    call fire_hook

    lea rdi, [rel dream_start]
    call print_cstr

    ; Get miss buffer state
    lea rax, [rbx + STATE_OFFSET + ST_MISS_POS]
    mov r12d, [rax]           ; current miss position (write cursor)

    ; Determine how many entries to replay
    ; Replay up to DREAM_REPLAY_COUNT entries, starting from oldest
    mov r13d, DREAM_REPLAY_COUNT
    cmp r13d, r12d
    jle .cap_ok
    mov r13d, r12d            ; don't replay more than available
.cap_ok:
    test r13d, r13d
    jz .dream_done

    ; Calculate start position (oldest entries)
    mov eax, r12d
    sub eax, r13d
    test eax, eax
    jns .start_ok
    ; Wrapped around — start from 0
    xor eax, eax
.start_ok:
    mov r14d, eax             ; start index

    xor r15d, r15d            ; emitted count

    ; --- Replay loop ---
.replay_loop:
    cmp r14d, r12d
    jge .dream_done
    push r14

    ; Get miss entry
    lea rsi, [rbx + STATE_OFFSET + ST_MISS_BUF]
    imul eax, r14d, ST_MISS_ENTRY_SIZE
    add rsi, rax

    ; Read context hash and token
    mov rdi, [rsi]            ; ctx_hash (u64)
    mov ecx, [rsi + 8]       ; token_id

    ; Use lower 32 bits of context hash for pattern matching
    mov edi, edi              ; zero-extend lower 32
    push rcx                  ; save token_id

    ; Check if pattern already exists
    mov esi, ecx
    call find_existing_pattern
    pop rcx

    test rax, rax
    jnz .skip_entry           ; already exists

    ; Check region table capacity
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov eax, [rax]
    cmp eax, REGION_TABLE_MAX - 8  ; leave room
    jge .table_full            ; table full, stop dreaming

    ; Emit speculative pattern
    ; Recover context and token from miss entry
    lea rsi, [rbx + STATE_OFFSET + ST_MISS_BUF]
    pop r14
    push r14
    imul eax, r14d, ST_MISS_ENTRY_SIZE
    add rsi, rax
    mov edi, [rsi]            ; ctx_hash (lower 32)
    mov esi, [rsi + 8]       ; token_id

    ; Get birth step
    lea rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov edx, [rax]

    ; Emit
    call emit_dispatch_pattern

    ; Mark as NURSERY (speculative)
    test rax, rax
    jz .skip_entry
    or word [rax + RHDR_FLAGS], RFLAG_NURSERY

    lea rdi, [rel dream_emit]
    call print_cstr

    inc r15d
    jmp .next_entry

.skip_entry:
    ; Pattern exists or was rejected

.next_entry:
    pop r14
    inc r14d
    cmp r15d, DREAM_REPLAY_COUNT / 2   ; limit emissions per cycle
    jl .replay_loop
    jmp .dream_done

.table_full:
    ; Stack has loop r14 on top - pop it before exiting
    pop r14

.dream_done:
    ; --- Schema extraction pass ---
    ; Look for pairs of miss entries with same token but similar contexts
    ; If found, emit a generalized pattern (masked context)
    call dream_extract_schemas

    ; Print summary
    lea rdi, [rel dream_end]
    call print_cstr
    movzx rdi, r13w
    call print_u64
    lea rdi, [rel dream_count]
    call print_cstr
    movzx rdi, r15w
    call print_u64
    call print_newline

    ; Scan for emergent patterns (things that work despite looking wrong)
    call sym_scan_for_discoveries

    ; Fire dream end hook
    mov edi, HOOK_ON_DREAM_END
    mov esi, r15d
    call fire_hook

    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; dream_extract_schemas
;; Scan miss buffer for entries with same token but different contexts.
;; When found, compute a generalized context (mask out differing bits)
;; and emit a schema pattern. This is how the system discovers
;; "token X follows ANY context that looks like Y".
;; ============================================================
dream_extract_schemas:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 32               ; locals: [0]=outer_idx, [4]=inner_idx,
                              ;         [8]=token_i, [12]=ctx_i, [16]=limit

    mov rbx, SURFACE_BASE
    lea r12, [rbx + STATE_OFFSET + ST_MISS_BUF]
    lea rax, [rbx + STATE_OFFSET + ST_MISS_POS]
    mov r13d, [rax]           ; entries written
    cmp r13d, 4               ; need at least 4 entries
    jl .schema_done

    ; Limit search window
    mov eax, r13d
    cmp eax, 32
    jle .schema_lim_ok
    mov eax, 32
.schema_lim_ok:
    mov r14d, r13d
    sub r14d, eax             ; start index
    test r14d, r14d
    jns .schema_start_ok
    xor r14d, r14d
.schema_start_ok:
    mov [rsp + 16], r13d      ; limit = miss_pos
    xor r15d, r15d            ; schemas emitted

.schema_outer:
    cmp r14d, [rsp + 16]
    jge .schema_done
    ; Load entry i
    imul eax, r14d, ST_MISS_ENTRY_SIZE
    mov ecx, [r12 + rax + 8]  ; token_id_i
    mov edx, [r12 + rax]      ; ctx_hash_i (lower 32)
    mov [rsp + 8], ecx         ; save token_i
    mov [rsp + 12], edx        ; save ctx_i

    ; Inner loop
    lea eax, [r14d + 1]
    mov [rsp + 4], eax         ; inner_idx

.schema_inner:
    mov eax, [rsp + 4]
    cmp eax, [rsp + 16]
    jge .schema_next_outer

    ; Load entry j
    imul ecx, eax, ST_MISS_ENTRY_SIZE
    mov edi, [r12 + rcx + 8]  ; token_id_j
    mov esi, [r12 + rcx]      ; ctx_hash_j

    ; Same token?
    cmp edi, [rsp + 8]
    jne .schema_adv_inner

    ; Different context?
    cmp esi, [rsp + 12]
    je .schema_adv_inner

    ; Same token in different contexts — generalize
    mov eax, [rsp + 12]        ; ctx_i

    ; Generalize: mask out low 4 bits of ctx_i
    and eax, 0xFFFFFFF0

    ; Check if schema already exists
    mov edi, eax              ; masked context
    mov esi, [rsp + 8]        ; token_id
    push rax                  ; save masked context
    call find_existing_pattern
    pop rcx                   ; restore masked context → rcx
    test rax, rax
    jnz .schema_adv_inner     ; already exists

    ; Check capacity
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    cmp dword [rax], REGION_TABLE_MAX - 8
    jge .schema_done

    ; Emit generalized pattern
    mov edi, ecx              ; masked context
    mov esi, [rsp + 8]        ; token_id
    lea rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov edx, [rax]
    call emit_dispatch_pattern

    inc r15d
    lea rdi, [rel schema_msg]
    call print_cstr

    ; Limit: max 4 schemas per dream
    cmp r15d, 4
    jge .schema_done

.schema_adv_inner:
    inc dword [rsp + 4]
    jmp .schema_inner

.schema_next_outer:
    inc r14d
    jmp .schema_outer

.schema_done:
    add rsp, 32
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; dream_consolidate
;; Review NURSERY regions: promote survivors, condemn failures
;; Called after some steps have passed since dreaming
;; ============================================================
global dream_consolidate
dream_consolidate:
    push rbx
    push r12
    push r13

    mov rbx, SURFACE_BASE
    lea r12, [rbx + REGION_TABLE_OFFSET]
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov r13d, [rax]

    xor ecx, ecx
.consol_loop:
    cmp ecx, r13d
    jge .consol_done
    push rcx

    imul rdi, rcx, RTE_SIZE
    add rdi, r12

    ; Only check NURSERY regions
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_NURSERY
    jz .consol_next

    ; Get header
    mov rsi, [rdi + RTE_ADDR]

    ; Check age: must have had time to be tested
    lea rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov eax, [rax]
    sub eax, [rsi + RHDR_BIRTH]
    cmp eax, 50               ; minimum age before judgment
    jl .consol_next

    ; Check performance
    mov eax, [rsi + RHDR_HITS]
    mov edx, [rsi + RHDR_MISSES]
    add edx, eax              ; total
    test edx, edx
    jz .condemn_nursery        ; no activity = condemn

    ; If any hits, promote to ACTIVE
    test eax, eax
    jnz .promote_nursery

.condemn_nursery:
    ; No hits after min age — condemn
    or word [rsi + RHDR_FLAGS], RFLAG_CONDEMNED
    and word [rsi + RHDR_FLAGS], ~RFLAG_NURSERY
    ; Update table flags
    or word [rdi + RTE_FLAGS], RFLAG_CONDEMNED
    jmp .consol_next

.promote_nursery:
    ; Has hits — graduate to ACTIVE
    and word [rsi + RHDR_FLAGS], ~RFLAG_NURSERY
    or word [rsi + RHDR_FLAGS], RFLAG_ACTIVE
    ; Update table
    and word [rdi + RTE_FLAGS], ~RFLAG_NURSERY

.consol_next:
    pop rcx
    inc ecx
    jmp .consol_loop

.consol_done:
    pop r13
    pop r12
    pop rbx
    ret
