; dreams.asm — Offline consolidation: replay misses, speculate, commit
;
; @entry dream_cycle() -> void ; replay miss buffer, emit speculative
; @entry dream_consolidate() -> void ; review NURSERY, promote/condemn
; @entry dream_extract_schemas() -> void ; holographic schema extraction
; @calls emit.asm:emit_dispatch_pattern
; @calls vsa.asm:holo_store, vsa.asm:holo_dot_f64, vsa.asm:holo_scale_f64
; @calls receipt.asm:receipt_resonate, emit_receipt_simple
; @calls dispatch.asm:schema_learn_from_context
; @calledby repl.asm:cmd_dream
;
; FLOW (dream_cycle):
;   miss_buffer → check exists → emit NURSERY pattern → schema extract
; FLOW (consolidate):
;   NURSERY aged>50 → no hits=condemn, hits=promote+reinforce
;
; SCHEMA EXTRACTION (holographic approach):
;   1. Query: holo_dot_f64(struct_ctx, schema_trace) → resonance
;   2. If resonance > 0.6: call schema_learn_from_context
;   3. Decay trace by 0.5 to prevent saturation
;   The schema trace accumulates struct_ctx on every MISS (in dispatch.asm)
;   High resonance = recurring structural pattern worth generalizing
;
; GOTCHAS:
;   - NURSERY patterns need 50+ steps before judgment
;   - Schema trace decays each dream (0.5x) to allow new patterns
;   - Promoted patterns must be reinforced via holo_store
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
    reinforce_msg:  db "[LTM] Reinforcing proven pattern ctx=0x", 0

    align 8
    ; Reinforcement strength for proven patterns (counteracts decay)
    reinforce_strength: dq 2.0
    ; Schema resonance threshold - structural pattern must resonate above this
    schema_resonate_thresh: dq 0.6
    ; Schema trace decay factor - prevents saturation
    schema_trace_decay: dq 0.5

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
extern holo_store
extern holo_dot_f64
extern holo_scale_f64
extern receipt_resonate
extern emit_receipt_simple
extern schema_learn_from_context

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

    ; --- RESONANCE QUERY: Have we dreamed about this pattern recently? ---
    ; Avoid redundant speculation by checking past DREAM events
    lea rsi, [rbx + STATE_OFFSET + ST_MISS_BUF]
    pop r14
    push r14
    imul eax, r14d, ST_MISS_ENTRY_SIZE
    add rsi, rax
    ; Read ctx_hash and token_id from miss entry
    mov eax, [rsi]            ; ctx_hash (save in eax)
    mov edx, [rsi + 8]        ; token_id
    mov edi, EVENT_DREAM
    mov esi, eax              ; ctx_hash as second param
    call receipt_resonate     ; → xmm0 = similarity to past DREAMs
    ; If very high similarity (>0.8), skip - we've dreamed this recently
    mov rax, 0x3FE999999999999A  ; 0.8 f64
    movq xmm1, rax
    ucomisd xmm0, xmm1
    ja .skip_entry            ; skip if too similar to recent dream

    ; Check region table capacity
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov eax, [rax]
    cmp eax, REGION_TABLE_MAX - 8  ; leave room
    jge .table_full            ; table full, stop dreaming

    ; Emit speculative pattern
    ; Recover context and token from miss entry
    lea rsi, [rbx + STATE_OFFSET + ST_MISS_BUF]
    imul eax, r14d, ST_MISS_ENTRY_SIZE
    add rsi, rax
    mov edi, [rsi]            ; ctx_hash (lower 32)
    mov esi, [rsi + 8]       ; token_id

    ; Save ctx and token for receipt emission
    push rdi
    push rsi

    ; Get birth step
    lea rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov edx, [rax]

    ; Emit
    call emit_dispatch_pattern

    ; Mark as NURSERY (speculative)
    test rax, rax
    jz .dream_skip_receipt
    or word [rax + RHDR_FLAGS], RFLAG_NURSERY

    ; === EMIT RECEIPT: EVENT_DREAM ===
    pop rsi                   ; token_id
    pop rdi                   ; ctx_hash (but we need edi, esi, edx)
    push rdi
    push rsi
    mov edx, esi              ; token_id
    mov esi, edi              ; ctx_hash
    mov edi, EVENT_DREAM      ; event_type
    xorps xmm0, xmm0          ; confidence = 0 (speculative)
    call emit_receipt_simple

.dream_skip_receipt:
    pop rsi
    pop rdi

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
;; Uses holographic resonance to detect recurring structural patterns.
;;
;; The schema trace (ST_SCHEMA_TRACE_VEC) accumulates structural contexts
;; from every miss. When we dream, we query: does the current struct_ctx
;; resonate with this accumulated trace? High resonance = recurring pattern
;; that should become a schema.
;;
;; This replaces the O(n²) miss buffer scan with O(1) resonance query.
;; The holographic memory IS the index.
;; ============================================================
dream_extract_schemas:
    push rbx
    push r12
    sub rsp, 8                ; align stack (2 pushes = even, need 8 mod 16)

    mov rbx, SURFACE_BASE

    ; Check if structural context is valid
    cmp dword [rbx + STATE_OFFSET + ST_STRUCT_CTX_VALID], 0
    je .no_schema

    ; Query: does current struct_ctx resonate with accumulated schema trace?
    ; High similarity = we've seen this structural pattern many times on misses
    lea rdi, [rbx + STATE_OFFSET + ST_STRUCT_CTX_VEC]
    lea rsi, [rbx + STATE_OFFSET + ST_SCHEMA_TRACE_VEC]
    call holo_dot_f64           ; xmm0 = similarity

    ; If resonance > threshold, create schema from current struct_ctx
    movsd xmm1, [rel schema_resonate_thresh]  ; 0.6
    ucomisd xmm0, xmm1
    jbe .decay_trace            ; below threshold, just decay

    ; High resonance detected - create schema using existing function
    ; schema_learn_from_context uses current struct_ctx as template
    call schema_learn_from_context

    lea rdi, [rel schema_msg]
    call print_cstr

.decay_trace:
    ; Decay trace to prevent saturation (even if no schema created)
    ; This ensures old patterns fade and new ones can emerge
    lea rdi, [rbx + STATE_OFFSET + ST_SCHEMA_TRACE_VEC]
    movsd xmm0, [rel schema_trace_decay]  ; 0.5 decay factor
    call holo_scale_f64

.no_schema:
    add rsp, 8
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
    ; Has hits — graduate to ACTIVE (this is now Long-Term Memory)
    and word [rsi + RHDR_FLAGS], ~RFLAG_NURSERY
    or word [rsi + RHDR_FLAGS], RFLAG_ACTIVE
    ; Update table
    and word [rdi + RTE_FLAGS], ~RFLAG_NURSERY

    ; === REINFORCE HOLOGRAPHIC TRACE ===
    ; Proven patterns must be reinforced to counteract decay (LTM)
    ; rsi = region header, code starts at rsi + RHDR_SIZE
    push rdi
    push rsi
    push rcx

    lea rax, [rsi + RHDR_SIZE]     ; code body

    ; Check for cmp eax, imm32 (0x3D) to extract ctx_hash
    cmp byte [rax], 0x3D
    jne .no_reinforce             ; skip if not standard pattern

    mov edi, [rax + 1]            ; ctx_hash = bytes 1-4
    push rdi                      ; save ctx_hash

    ; Scan for mov eax, imm32 (0xB8) to extract pred_token
    movzx ecx, word [rsi + RHDR_CODE_LEN]
    lea rax, [rsi + RHDR_SIZE + 5] ; skip cmp instruction
    sub ecx, 5
.scan_token:
    cmp ecx, 5
    jl .no_token
    cmp byte [rax], 0xB8
    je .found_token
    inc rax
    dec ecx
    jmp .scan_token

.found_token:
    mov esi, [rax + 1]            ; pred_token = bytes after 0xB8
    pop rdi                       ; restore ctx_hash

    ; Save for receipt emission
    push rdi
    push rsi

    ; Call holo_store(ctx_hash, pred_token, reinforce_strength)
    movsd xmm0, [rel reinforce_strength]
    call holo_store

    ; === EMIT RECEIPT: EVENT_PROMOTE ===
    pop rsi                       ; token_id
    pop rdi                       ; ctx_hash
    mov edx, esi                  ; token_id
    mov esi, edi                  ; ctx_hash
    mov edi, EVENT_PROMOTE        ; event_type
    mov eax, 0x3F800000           ; 1.0f confidence (promoted = proven)
    movd xmm0, eax
    call emit_receipt_simple

    jmp .reinforce_done

.no_token:
    pop rdi                       ; clean up saved ctx_hash
.no_reinforce:
.reinforce_done:
    pop rcx
    pop rsi
    pop rdi

.consol_next:
    pop rcx
    inc ecx
    jmp .consol_loop

.consol_done:
    pop r13
    pop r12
    pop rbx
    ret
