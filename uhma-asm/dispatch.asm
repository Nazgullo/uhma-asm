; dispatch.asm — Dispatch tree: tokenize, route through branches, predict
%include "syscalls.inc"
%include "constants.inc"

section .data
    token_msg:      db "  token=0x", 0
    predict_msg:    db "  predict=0x", 0
    hit_msg:        db " [HIT]", 10, 0
    miss_msg:       db " [MISS]", 10, 0
    no_predict_msg: db " [NEW]", 10, 0
    process_hdr:    db "Processing: ", 0

section .bss
    word_buf:       resb MAX_WORD_LEN

section .text

extern print_cstr
extern print_str
extern print_hex32
extern print_u64
extern print_newline
extern learn_pattern
extern fire_hook
extern vsa_get_token_vec
extern vsa_superpose
extern region_alloc
extern find_existing_pattern

;; ============================================================
;; dispatch_init
;; Create the initial (empty) dispatch region
;; ============================================================
global dispatch_init
dispatch_init:
    push rbx

    ; Allocate a dispatch region with just a "return 0" stub
    ; Code: xor eax, eax; ret = 31 C0 C3 (3 bytes)
    mov rdi, 3                ; code size
    mov rsi, RTYPE_DISPATCH   ; type
    xor edx, edx             ; birth step = 0
    call region_alloc
    ; rax = header ptr, code at rax+16

    ; Write: xor eax, eax; ret
    mov byte [rax + RHDR_SIZE + 0], 0x31  ; xor
    mov byte [rax + RHDR_SIZE + 1], 0xC0  ; eax, eax
    mov byte [rax + RHDR_SIZE + 2], 0xC3  ; ret

    ; Store as the dispatch entry point
    mov rbx, SURFACE_BASE
    lea rcx, [rbx + STATE_OFFSET + ST_DISPATCH_PTR]
    ; The dispatch_ptr is the allocator; we need a separate dispatch_entry
    ; Store the entry point in a known location (beginning of dispatch region)
    ; Actually the first region IS the dispatch entry
    ; We'll call it by looking up region 0

    pop rbx
    ret

;; ============================================================
;; process_input(text_ptr, text_len)
;; rdi=text, rsi=len
;; Tokenizes text into words, processes each token through dispatch
;; ============================================================
global process_input
process_input:
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov r12, rdi              ; text ptr
    mov r13, rsi              ; text len
    xor r14d, r14d            ; current position
    mov r15, SURFACE_BASE

    ; Print header
    push r12
    push r13
    lea rdi, [rel process_hdr]
    call print_cstr
    mov rdi, r12
    mov rsi, r13
    call print_str
    call print_newline
    pop r13
    pop r12

    ; Fire pre-step hook
    mov edi, HOOK_PRE_STEP
    xor esi, esi
    call fire_hook

.next_word:
    ; Skip whitespace
    cmp r14, r13
    jge .done
    movzx eax, byte [r12 + r14]
    cmp al, ' '
    je .skip_ws
    cmp al, 9                 ; tab
    je .skip_ws
    cmp al, 10                ; newline
    je .skip_ws
    cmp al, 13                ; CR
    je .skip_ws
    jmp .word_start

.skip_ws:
    inc r14
    jmp .next_word

.word_start:
    ; Collect word into word_buf
    lea rbx, [rel word_buf]
    xor ecx, ecx             ; word length

.collect:
    cmp r14, r13
    jge .word_done
    movzx eax, byte [r12 + r14]
    cmp al, ' '
    je .word_done
    cmp al, 9
    je .word_done
    cmp al, 10
    je .word_done
    cmp al, 13
    je .word_done
    cmp ecx, MAX_WORD_LEN - 1
    jge .word_done

    ; Store (uppercase)
    cmp al, 'a'
    jl .no_upper
    cmp al, 'z'
    jg .no_upper
    sub al, 32
.no_upper:
    mov [rbx + rcx], al
    inc ecx
    inc r14
    jmp .collect

.word_done:
    test ecx, ecx
    jz .next_word             ; empty word, skip

    ; Hash the word → token ID
    mov rdi, rbx              ; word_buf
    mov esi, ecx              ; word_len
    call tokenize_word        ; → eax = token_id

    ; Process this token through the dispatch system
    mov edi, eax
    call process_token

    jmp .next_word

.done:
    ; Fire post-step hook
    mov edi, HOOK_POST_STEP
    xor esi, esi
    call fire_hook

    ; Increment global step
    lea rax, [r15 + STATE_OFFSET + ST_GLOBAL_STEP]
    inc qword [rax]

    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; tokenize_word(buf, len) → eax (token_id)
;; rdi=buf, esi=len
;; FNV-1a hash, uppercase already applied
;; ============================================================
global tokenize_word
tokenize_word:
    mov ecx, FNV32_INIT
    xor edx, edx             ; index

.hash_loop:
    cmp edx, esi
    jge .hash_done
    movzx eax, byte [rdi + rdx]
    xor ecx, eax
    imul ecx, FNV32_PRIME
    inc edx
    jmp .hash_loop

.hash_done:
    mov eax, ecx              ; return hash as token ID
    ret

;; ============================================================
;; process_token(token_id)
;; edi=token_id
;; Updates context, runs dispatch, handles hit/miss, triggers learning
;; ============================================================
global process_token
process_token:
    push rbx
    push r12
    push r13
    push r14

    mov r12d, edi             ; save token_id
    mov rbx, SURFACE_BASE

    ; --- Update token ring buffer ---
    lea rax, [rbx + STATE_OFFSET + ST_TOKEN_POS]
    mov ecx, [rax]            ; current pos
    lea rdx, [rbx + STATE_OFFSET + ST_TOKEN_BUF]
    mov [rdx + rcx * 4], r12d ; store token
    inc ecx
    and ecx, (ST_TOKEN_BUF_CAP - 1)  ; wrap
    mov [rax], ecx            ; update pos

    ; Increment token count
    lea rax, [rbx + STATE_OFFSET + ST_TOKEN_COUNT]
    inc dword [rax]

    ; --- Update rolling context hash ---
    ; ctx_hash = ctx_hash XOR (token_id * FNV64_PRIME)
    lea rax, [rbx + STATE_OFFSET + ST_CTX_HASH]
    mov rcx, [rax]            ; current hash
    mov rdx, r12              ; token_id (zero extended)
    mov r8, FNV64_PRIME       ; use r8 so we don't clobber rcx
    imul rdx, r8
    xor rcx, rdx
    mov [rax], rcx            ; store updated hash
    mov r13, rcx              ; r13 = context hash

    ; --- Check last prediction ---
    lea rax, [rbx + STATE_OFFSET + ST_LAST_PREDICT]
    mov r14d, [rax]           ; last predicted token

    test r14d, r14d
    jz .no_prediction         ; no prediction was made

    ; Compare prediction with actual
    cmp r14d, r12d
    je .hit
    jmp .miss

.hit:
    ; Clear surprise type (no surprise on hit)
    lea rax, [rbx + STATE_OFFSET + ST_SURPRISE_TYPE]
    mov dword [rax], SURPRISE_NONE

    ; Increment hit counter on the predicting region
    lea rax, [rbx + STATE_OFFSET + ST_PREDICT_REGION]
    mov rcx, [rax]
    test rcx, rcx
    jz .after_counter
    inc dword [rcx + RHDR_HITS]

    ; Schema coverage: did a generalized pattern match this context?
    ; (flag set during dispatch_predict scan)
    cmp dword [rbx + STATE_OFFSET + ST_EXPECT_IS_SCHEMA], 0
    je .no_schema_check
    ; Schema was available for this context — count as schema hit
    lea rax, [rbx + STATE_OFFSET + ST_SCHEMA_HITS]
    inc dword [rax]
.no_schema_check:
    ; Always count total for schema coverage
    lea rax, [rbx + STATE_OFFSET + ST_SCHEMA_TOTAL]
    inc dword [rax]

    ; Also update region table entry
    push rcx
    mov rdi, rcx
    call update_region_table_hits
    pop rcx

    ; Self-prediction tracking via successor table
    lea rax, [rbx + STATE_OFFSET + ST_SELF_PRED_REGION]
    mov rdx, [rax]
    test rdx, rdx
    jz .skip_self_pred_hit
    cmp rdx, rcx
    jne .self_pred_miss_h
    ; Correct self-prediction
    lea rax, [rbx + STATE_OFFSET + ST_SELF_PRED_HITS]
    inc dword [rax]
    jmp .skip_self_pred_hit
.self_pred_miss_h:
    lea rax, [rbx + STATE_OFFSET + ST_SELF_PRED_MISSES]
    inc dword [rax]
.skip_self_pred_hit:
    ; Find current region's index in the table
    push rcx                   ; save region ptr
    mov rdi, rcx
    call find_region_index     ; → eax = index (or -1)
    pop rcx
    cmp eax, -1
    je .skip_successor

    ; Update successor_tbl[last_fired_idx] = current_idx
    movzx edx, word [rbx + STATE_OFFSET + ST_LAST_FIRED_IDX]
    lea rsi, [rbx + STATE_OFFSET + ST_SUCCESSOR_TBL]
    mov [rsi + rdx * 2], ax   ; current idx is successor of last fired

    ; Predict: successor_tbl[current_idx] = next region idx
    movzx edx, word [rsi + rax * 2]
    test dx, dx
    jz .successor_fallback
    ; Convert successor idx to region ptr
    push rax
    imul rdi, rdx, RTE_SIZE
    lea rsi, [rbx + REGION_TABLE_OFFSET]
    add rdi, rsi
    mov rdx, [rdi + RTE_ADDR]
    mov [rbx + STATE_OFFSET + ST_SELF_PRED_REGION], rdx
    pop rax
    jmp .successor_done
.successor_fallback:
    ; No successor known — predict same region
    mov [rbx + STATE_OFFSET + ST_SELF_PRED_REGION], rcx
.successor_done:
    ; Update last_fired_idx
    mov [rbx + STATE_OFFSET + ST_LAST_FIRED_IDX], ax
.skip_successor:

    ; Fire hit hook
    mov edi, HOOK_ON_HIT
    mov esi, r12d
    call fire_hook

    ; Print hit feedback
    lea rdi, [rel token_msg]
    call print_cstr
    mov edi, r12d
    call print_hex32
    lea rdi, [rel hit_msg]
    call print_cstr
    jmp .after_counter

.miss:
    ; Classify surprise type based on predicting region's confidence
    lea rax, [rbx + STATE_OFFSET + ST_PREDICT_REGION]
    mov rcx, [rax]
    test rcx, rcx
    jz .surprise_outcome       ; no region = expected miss

    ; Compute predicting region's accuracy
    mov eax, [rcx + RHDR_HITS]
    mov edx, [rcx + RHDR_MISSES]
    add edx, eax
    test edx, edx
    jz .surprise_outcome
    cvtsi2ss xmm0, eax
    cvtsi2ss xmm1, edx
    divss xmm0, xmm1          ; accuracy of predicting region
    ; If accuracy > 0.7 → self-model violated (high confidence was wrong)
    mov eax, 0x3F333333        ; 0.7f
    movd xmm1, eax
    comiss xmm0, xmm1
    jbe .surprise_outcome
    ; SURPRISE_SELF: high-confidence region missed
    lea rax, [rbx + STATE_OFFSET + ST_SURPRISE_TYPE]
    mov dword [rax], SURPRISE_SELF
    jmp .do_miss_counter
.surprise_outcome:
    ; SURPRISE_OUTCOME: low-confidence miss (expected uncertainty)
    lea rax, [rbx + STATE_OFFSET + ST_SURPRISE_TYPE]
    mov dword [rax], SURPRISE_OUTCOME

.do_miss_counter:
    ; Increment miss counter on the predicting region
    lea rax, [rbx + STATE_OFFSET + ST_PREDICT_REGION]
    mov rcx, [rax]
    test rcx, rcx
    jz .record_miss
    inc dword [rcx + RHDR_MISSES]

    ; Self-prediction: this region was predicted but missed
    lea rax, [rbx + STATE_OFFSET + ST_SELF_PRED_REGION]
    mov rdx, [rax]
    test rdx, rdx
    jz .skip_self_pred_miss
    ; Any miss means self-prediction was wrong about outcome
    lea rax, [rbx + STATE_OFFSET + ST_SELF_PRED_MISSES]
    inc dword [rax]
.skip_self_pred_miss:
    ; Clear self-prediction (context shifted)
    lea rax, [rbx + STATE_OFFSET + ST_SELF_PRED_REGION]
    mov qword [rax], 0

    push rcx
    mov rdi, rcx
    call update_region_table_misses
    pop rcx

.record_miss:
    ; Record in miss buffer for dream replay
    lea rax, [rbx + STATE_OFFSET + ST_MISS_POS]
    mov ecx, [rax]
    lea rdx, [rbx + STATE_OFFSET + ST_MISS_BUF]
    imul edi, ecx, ST_MISS_ENTRY_SIZE
    add rdi, rdx
    ; Store: ctx_hash (the context BEFORE this token)
    lea rax, [rbx + STATE_OFFSET + ST_LAST_CTX]
    mov rsi, [rax]
    mov [rdi], rsi            ; context hash
    mov [rdi + 8], r12d       ; actual token
    ; Advance miss pos
    lea rax, [rbx + STATE_OFFSET + ST_MISS_POS]
    mov ecx, [rax]
    inc ecx
    cmp ecx, ST_MISS_BUF_CAP
    jl .no_miss_wrap
    xor ecx, ecx
.no_miss_wrap:
    mov [rax], ecx

    ; Fire miss hook
    mov edi, HOOK_ON_MISS
    mov esi, r12d
    call fire_hook

    ; Print miss feedback
    lea rdi, [rel token_msg]
    call print_cstr
    mov edi, r12d
    call print_hex32
    lea rdi, [rel miss_msg]
    call print_cstr

    ; --- Learn from the miss ---
    ; Context that preceded this token → should predict this token
    lea rax, [rbx + STATE_OFFSET + ST_LAST_CTX]
    mov rdi, [rax]            ; context hash
    mov esi, r12d             ; token to predict
    call learn_pattern
    jmp .after_counter

.no_prediction:
    ; No prediction existed — this is a new context
    lea rdi, [rel token_msg]
    call print_cstr
    mov edi, r12d
    call print_hex32
    lea rdi, [rel no_predict_msg]
    call print_cstr

    ; Learn: previous context → this token
    lea rax, [rbx + STATE_OFFSET + ST_LAST_CTX]
    mov rdi, [rax]            ; previous context
    test rdi, rdi
    jz .after_counter         ; no previous context yet
    mov esi, r12d
    call learn_pattern

.after_counter:
    ; --- Make next prediction ---
    ; Dispatch: use current context hash to predict next token
    mov rdi, r13              ; current context hash
    call dispatch_predict     ; → eax = predicted token (0 if none)

    ; Store prediction for next step
    lea rcx, [rbx + STATE_OFFSET + ST_LAST_PREDICT]
    mov [rcx], eax
    ; Store current context for miss recording
    lea rcx, [rbx + STATE_OFFSET + ST_LAST_CTX]
    mov [rcx], r13

    ; Fire input hook
    mov edi, HOOK_ON_INPUT
    mov esi, r12d
    call fire_hook

    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; dispatch_predict(ctx_hash) → eax (predicted token, 0=none)
;; rdi=context hash (u64, we use lower 32 bits for comparison)
;; Walks the dispatch tree (region table) looking for matching context
;; ============================================================
global dispatch_predict
dispatch_predict:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 16                ; [rsp+0]=best_token, [rsp+4]=best_hits, [rsp+8]=best_region_ptr

    mov r12d, edi             ; context hash (lower 32 bits)
    mov rbx, SURFACE_BASE

    ; Read dispatch mode
    lea rax, [rbx + STATE_OFFSET + ST_DISPATCH_MODE]
    mov r14d, [rax]           ; dispatch mode

    ; Walk all DISPATCH regions looking for a match
    lea r13, [rbx + REGION_TABLE_OFFSET]
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov ecx, [rax]            ; region count

    ; Initialize best-match tracking (for DMODE_BEST/EXPLORE)
    mov dword [rsp + 0], 0    ; best_token
    mov dword [rsp + 4], 0    ; best_hits (or inverse for EXPLORE)
    mov qword [rsp + 8], 0    ; best_region_ptr

    ; Zero trace counters and schema flag for this dispatch cycle
    mov dword [rbx + STATE_OFFSET + ST_TRACE_CANDIDATES], 0
    mov dword [rbx + STATE_OFFSET + ST_TRACE_MATCHED], 0
    mov dword [rbx + STATE_OFFSET + ST_EXPECT_IS_SCHEMA], 0

    ; Start from index 1 (index 0 is the empty bootstrap dispatch)
    mov edx, 1

.search:
    cmp edx, ecx
    jge .search_done
    push rcx
    push rdx

    ; Get region table entry
    imul rdi, rdx, RTE_SIZE
    add rdi, r13
    movzx eax, word [rdi + RTE_TYPE]
    cmp eax, RTYPE_DISPATCH
    jne .skip_region

    ; Check flags — skip condemned
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_CONDEMNED
    jnz .skip_region

    ; Get the region header address
    mov rsi, [rdi + RTE_ADDR]

    ; Trace: candidate considered
    inc dword [rbx + STATE_OFFSET + ST_TRACE_CANDIDATES]

    ; Check if this region's stored context matches ours
    cmp byte [rsi + RHDR_SIZE], 0x3D   ; is it cmp eax, imm32?
    jne .skip_region

    mov eax, [rsi + RHDR_SIZE + 1]     ; the imm32 (stored context)
    ; Schema handling: if stored context has low 4 bits = 0, do masked compare
    test eax, 0x0F
    jnz .exact_cmp
    ; Schema pattern — mask incoming context too
    mov edi, r12d
    and edi, 0xFFFFFFF0
    cmp eax, edi
    jne .skip_region
    ; Schema matched this context
    mov dword [rbx + STATE_OFFSET + ST_EXPECT_IS_SCHEMA], 1
    jmp .ctx_matched
.exact_cmp:
    cmp eax, r12d                       ; exact match
    jne .skip_region
.ctx_matched:

    ; Trace: matched
    inc dword [rbx + STATE_OFFSET + ST_TRACE_MATCHED]

    ; MATCH found. Behavior depends on dispatch mode.
    ; Read the predicted token (B8 opcode at +7, imm32 at +8)
    mov r15d, [rsi + RHDR_SIZE + 8] ; the predicted token

    ; DMODE_FAST: first match wins (return immediately)
    cmp r14d, DMODE_FAST
    je .fast_match

    ; DMODE_BEST: pick highest-accuracy match
    cmp r14d, DMODE_BEST
    je .best_match

    ; DMODE_EXPLORE: prefer low-hit (novel) regions
    cmp r14d, DMODE_EXPLORE
    je .explore_match

    ; DMODE_DELIBERATE or unknown: treat as BEST
    jmp .best_match

.fast_match:
    ; Return immediately with this match
    mov eax, r15d
    lea rcx, [rbx + STATE_OFFSET + ST_PREDICT_REGION]
    mov [rcx], rsi
    pop rdx
    pop rcx
    jmp .fill_expect

.best_match:
    ; Compare this region's hits with current best
    mov eax, [rsi + RHDR_HITS]
    cmp eax, [rsp + 16 + 4]   ; best_hits (adjusted for two pushes)
    jle .skip_region
    ; New best
    mov [rsp + 16 + 0], r15d   ; best_token
    mov [rsp + 16 + 4], eax    ; best_hits
    mov [rsp + 16 + 8], rsi    ; best_region_ptr
    jmp .skip_region

.explore_match:
    ; Prefer regions with FEWER hits (more novel)
    mov eax, [rsi + RHDR_HITS]
    mov ecx, [rsp + 16 + 4]    ; current "best" (lowest hits so far)
    test ecx, ecx
    jz .explore_first           ; first match always wins
    cmp eax, ecx
    jge .skip_region            ; this one has more hits, skip
.explore_first:
    mov [rsp + 16 + 0], r15d
    inc eax                     ; store hits+1 so 0-hit regions still register
    mov [rsp + 16 + 4], eax
    mov [rsp + 16 + 8], rsi
    jmp .skip_region

.skip_region:
    pop rdx
    pop rcx
    inc edx
    jmp .search

.search_done:
    ; Check if we found anything (for non-FAST modes)
    cmp qword [rsp + 8], 0
    je .no_match

    ; Use the best match
    mov eax, [rsp + 0]        ; best_token
    mov rsi, [rsp + 8]        ; best_region_ptr
    lea rcx, [rbx + STATE_OFFSET + ST_PREDICT_REGION]
    mov [rcx], rsi
    jmp .fill_expect

.no_match:
    xor eax, eax             ; no prediction
    lea rcx, [rbx + STATE_OFFSET + ST_PREDICT_REGION]
    mov qword [rcx], 0
    ; Clear expectation bundle
    mov qword [rbx + STATE_OFFSET + ST_EXPECT_REGION], 0
    jmp .found

.fill_expect:
    ; Fill self-expectation bundle
    ; eax = predicted token, rsi = region ptr
    push rax
    mov [rbx + STATE_OFFSET + ST_EXPECT_REGION], rsi
    ; Confidence = hits/(hits+misses)
    mov ecx, [rsi + RHDR_HITS]
    mov edx, [rsi + RHDR_MISSES]
    add edx, ecx
    test edx, edx
    jz .expect_zero
    cvtsi2ss xmm0, ecx
    cvtsi2ss xmm1, edx
    divss xmm0, xmm1
    jmp .expect_store
.expect_zero:
    xorps xmm0, xmm0
.expect_store:
    movss [rbx + STATE_OFFSET + ST_EXPECT_CONF], xmm0
    pop rax
    mov [rbx + STATE_OFFSET + ST_EXPECT_TOKEN], eax
    ; Is schema? Lower 4 bits of stored context = 0
    mov edx, [rsi + RHDR_SIZE + 1]
    test edx, 0x0F
    jnz .not_schema_e
    mov dword [rbx + STATE_OFFSET + ST_EXPECT_IS_SCHEMA], 1
    jmp .found
.not_schema_e:
    ; Check if a schema pattern covers this exact context
    ; (in FAST mode, scan exits before reaching schemas)
    push rax                  ; save predicted token (our return value)
    mov edi, edx              ; edx = stored context from test above
    and edi, 0xFFFFFFF0       ; mask to schema form
    mov esi, eax              ; predicted token
    call find_existing_pattern
    test rax, rax
    pop rax                   ; restore predicted token
    jz .found                 ; no schema exists for this context
    mov dword [rbx + STATE_OFFSET + ST_EXPECT_IS_SCHEMA], 1

.found:
    add rsp, 16
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; update_region_table_hits(header_ptr)
;; rdi=region header address
;; Syncs the hit counter to the region table entry
;; ============================================================
update_region_table_hits:
    push rbx
    mov rbx, SURFACE_BASE
    lea rsi, [rbx + REGION_TABLE_OFFSET]
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov ecx, [rax]
    xor edx, edx
.loop:
    cmp edx, ecx
    jge .done
    imul rax, rdx, RTE_SIZE
    add rax, rsi
    cmp [rax + RTE_ADDR], rdi
    jne .next
    ; Found — copy hits from header
    mov ebx, [rdi + RHDR_HITS]
    mov [rax + RTE_HITS], ebx
    jmp .done
.next:
    inc edx
    jmp .loop
.done:
    pop rbx
    ret

;; ============================================================
;; update_region_table_misses(header_ptr)
;; ============================================================
update_region_table_misses:
    push rbx
    mov rbx, SURFACE_BASE
    lea rsi, [rbx + REGION_TABLE_OFFSET]
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov ecx, [rax]
    xor edx, edx
.loop:
    cmp edx, ecx
    jge .done
    imul rax, rdx, RTE_SIZE
    add rax, rsi
    cmp [rax + RTE_ADDR], rdi
    jne .next
    mov ebx, [rdi + RHDR_MISSES]
    mov [rax + RTE_MISSES], ebx
    jmp .done
.next:
    inc edx
    jmp .loop
.done:
    pop rbx
    ret

;; ============================================================
;; find_region_index(header_ptr) → eax (index, or -1 if not found)
;; rdi=region header address
;; ============================================================
global find_region_index
find_region_index:
    push rbx
    push r12
    mov rbx, SURFACE_BASE
    lea r12, [rbx + REGION_TABLE_OFFSET]
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov ecx, [rax]
    xor edx, edx
.fri_loop:
    cmp edx, ecx
    jge .fri_not_found
    imul rax, rdx, RTE_SIZE
    add rax, r12
    cmp [rax + RTE_ADDR], rdi
    je .fri_found
    inc edx
    jmp .fri_loop
.fri_found:
    mov eax, edx
    pop r12
    pop rbx
    ret
.fri_not_found:
    mov eax, -1
    pop r12
    pop rbx
    ret
