; dispatch.asm — Token processing, prediction, hit/miss handling
;
; @entry process_input(rdi=buf, rsi=len) -> void
; @entry process_token(edi=token_id) -> void
; @entry dispatch_predict(edi=ctx_hash) -> eax=token, xmm0=conf
; @entry schema_dispatch() -> eax=token ; match schemas against struct_ctx
; @entry schema_learn_from_context() -> void ; create schema from struct_ctx
; @entry compute_struct_ctx() -> void ; build 8-position f64 structural context
; @calls learn.asm:learn_pattern, emit.asm:emit_dispatch_pattern
; @calls receipt.asm:emit_receipt_full, receipt.asm:receipt_resonate
; @calls vsa.asm:holo_predict, vsa.asm:holo_superpose_f64, vsa.asm:holo_cosim_f64
; @calls vsa.asm:holo_gen_vec, vsa.asm:holo_bind_f64, vsa.asm:holo_unbind_f64
; @calledby repl.asm:repl_run, io.asm:digest_file, dreams.asm:dream_extract_schemas
;
; FLOW: token → ctx=hash(prev) → predict → HIT/MISS → learn on miss
; STATE: ST_CTX_HASH, ST_EXPECT_TOKEN, ST_EXPECT_CONF, ST_PREDICT_REGION
;
; TOKEN ABSTRACTION (~line 240):
;   digits → TOKEN_NUM (0x4e554d21), 0x... → TOKEN_HEX (0x48455821)
;   MUST match io.asm:digest_file abstraction
;
; STRUCTURAL CONTEXT (compute_struct_ctx):
;   struct_ctx = Σ bind(ROLE_i, token_history[i]) for positions 0-7
;   Uses f64 vectors: holo_gen_vec for roles/tokens, holo_bind_f64, holo_superpose_f64
;
; SCHEMA TRACE (~line 825):
;   On MISS: superpose ST_STRUCT_CTX_VEC into ST_SCHEMA_TRACE_VEC
;   Accumulates structural patterns for holographic schema learning
;
; GOTCHAS:
;   - ctx = hash(prev_token) ONLY, no somatic XOR (breaks pattern identity)
;   - rcx is caller-saved, use r10-r15 in loops that call functions
;   - Token abstraction must match in BOTH process_input AND digest_file
;   - All struct_ctx operations use f64 (holo_*), not f32 (vsa_*)
;
%include "syscalls.inc"
%include "constants.inc"

section .data
    token_msg:      db "  token=0x", 0
    predict_msg:    db "  predict=0x", 0
    hit_msg:        db " [HIT]", 10, 0
    miss_msg:       db " [MISS]", 10, 0
    no_predict_msg: db " [NEW]", 10, 0
    process_hdr:    db "Processing: ", 0
    dbg_credit_msg: db "[CREDIT]", 10, 0
    dbg_nocredit_msg: db "[NO_CREDIT]", 10, 0
    dbg_search_msg: db "[SEARCH] ctx=0x", 0
    dbg_tok_msg:    db " tok=0x", 0
    dbg_region_msg: db "[REGION] ", 0

    ; f64 constants for graph dynamics
    align 8
    prime_decay:    dq 0.9
    activ_decay:    dq 0.85
    one_point_o:    dq 1.0
    zero_point_o:   dq 0.0
    half_point_o:   dq 0.5
    activ_thresh:   dq 0.1

    ; Holographic threshold (f64) - confidence required to use holographic prediction
    ; VSA element-wise binding produces ~0.7-0.8 confidence for exact matches
    ; Lowered to 0.3 to test resonance accumulation
    align 8
    holo_thresh:    dq 0.3

    ; Resonant dispatch threshold (f64) - similarity required for fuzzy match
    ; 0.7 = strong similarity required (can adjust based on noise tolerance)
    align 8
    resonant_thresh: dq 0.7

section .bss
    word_buf:       resb MAX_WORD_LEN

section .text

extern print_cstr
extern print_str
extern print_hex32
extern print_hex64
extern print_u64
extern print_newline
extern learn_pattern
extern fire_hook
extern vsa_get_token_vec
extern vsa_superpose
extern holo_superpose_f64
extern holo_gen_vec
extern holo_bind_f64
extern holo_cosim_f64
extern holo_unbind_f64
extern vsa_bind
extern vsa_unbind
extern vsa_gen_role_pos
extern vsa_normalize
extern vsa_dot
extern vsa_cosim
extern region_alloc
extern find_existing_pattern
extern learn_connections
extern observe_cycle
extern tick_regulators
extern accrue_pressure
extern holo_predict
extern holo_query_valence
extern holo_decay_all
extern update_organic_pressure
extern maturity_update
extern update_anticipatory
extern decay_anticipatory
extern update_oscillation
extern update_presence_dispatch
extern introspect_scan_regions
extern journey_step
extern confidence_update
extern confidence_query
extern confidence_get_feeling
extern confidence_decay_all
extern resonant_match
extern resonant_get_threshold
extern resonant_extract_token
extern emit_receipt_simple
extern emit_receipt_full
extern receipt_resonate

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

    ; Reset context hash for this line — makes context line-local
    ; so patterns are recognized by local structure, not absolute position
    mov qword [r15 + STATE_OFFSET + ST_CTX_HASH], 0
    mov qword [r15 + STATE_OFFSET + ST_LAST_CTX], 0

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

    ; --- Categorical abstraction ---
    ; Check if word contains hex literal (0X...) → abstract to class token
    ; This is chunking: "0x0000000100010110" and "0xDEADBEEF" are both "HEX"
    push rcx                  ; save word_len
    cmp ecx, 3
    jl .not_hex
    xor edx, edx
.hex_scan:
    cmp edx, ecx
    jge .not_hex
    cmp byte [rbx + rdx], '0'
    jne .hex_scan_next
    cmp edx, ecx
    jge .not_hex
    lea eax, [edx + 1]
    cmp eax, ecx
    jge .hex_scan_next
    cmp byte [rbx + rax], 'X'
    je .is_hex
.hex_scan_next:
    inc edx
    jmp .hex_scan
.is_hex:
    pop rcx
    mov eax, 0x48455821       ; TOKEN_HEX class ("HEX!" as u32)
    jmp .token_ready

.not_hex:
    ; Check if word is all digits → abstract to class token
    ; (zero-length already rejected at .word_done, so ecx >= 1 here)
    pop rcx
    xor edx, edx
.num_scan:
    cmp edx, ecx
    jge .is_num
    movzx eax, byte [rbx + rdx]
    cmp al, '0'
    jl .no_abstract
    cmp al, '9'
    jg .no_abstract
    inc edx
    jmp .num_scan
.is_num:
    mov eax, 0x4e554d21       ; TOKEN_NUM class ("NUM!" as u32)
    jmp .token_ready

.no_abstract:
    ; Normal tokenization — hash the word
    mov rdi, rbx              ; word_buf
    mov esi, ecx              ; word_len
    call tokenize_word        ; → eax = token_id

.token_ready:
    ; Process this token through the dispatch system
    mov edi, eax
    call process_token

    jmp .next_word

.done:
    ; Holographic decay (interference traces)
    call holo_decay_all

    ; Topological metacognition: decay confidence vector toward neutral
    ; Prevents runaway confidence/anxiety over time
    call confidence_decay_all

    ; Decay dynamics once per line (not per token)
    call decay_all_regions

    ; --- Temporal rhythm: update felt tempo from presence ---
    ; tempo = 1.0 + arousal*0.5 - fatigue*0.25, clamped to [0.5, 2.0]
    lea rdi, [r15 + STATE_OFFSET + ST_PRESENCE]
    movss xmm0, [rdi + PRES_AROUSAL * 4]
    cvtss2sd xmm0, xmm0
    mov rax, TEMPO_AROUSAL_SCALE
    movq xmm1, rax
    mulsd xmm0, xmm1             ; arousal * 0.5
    movss xmm2, [rdi + PRES_FATIGUE * 4]
    cvtss2sd xmm2, xmm2
    mov rax, TEMPO_FATIGUE_SCALE
    movq xmm3, rax
    mulsd xmm2, xmm3             ; fatigue * 0.25
    mov rax, 0x3FF0000000000000   ; 1.0
    movq xmm1, rax
    addsd xmm0, xmm1             ; 1.0 + arousal*0.5
    subsd xmm0, xmm2             ; - fatigue*0.25
    ; Clamp [0.5, 2.0]
    mov rax, TEMPO_MIN
    movq xmm1, rax
    maxsd xmm0, xmm1
    mov rax, TEMPO_MAX
    movq xmm1, rax
    minsd xmm0, xmm1
    movsd [r15 + STATE_OFFSET + ST_TEMPO_MULT], xmm0

    ; --- Organic per-line updates ---
    ; Decay anticipatory signals (distant things fade if not reinforced)
    call decay_anticipatory

    ; Track oscillation (flat = dead, oscillating = alive)
    call update_oscillation

    ; Let presence field influence dispatch mode (felt experience → behavior)
    call update_presence_dispatch

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

    ; Store current token for journey tracing
    mov [rbx + STATE_OFFSET + ST_CURRENT_TOKEN], r12d

    ; Check if we should start tracing this token (0xFFFFFFFF = trace next)
    cmp dword [rbx + STATE_OFFSET + ST_JOURNEY_TOKEN], 0xFFFFFFFF
    jne .no_start_journey
    ; Start tracing this token
    mov [rbx + STATE_OFFSET + ST_JOURNEY_TOKEN], r12d
    mov dword [rbx + STATE_OFFSET + ST_JOURNEY_POS], 0
.no_start_journey:

    ; JOURNEY: record this token passing through process_token
    mov edi, TRACE_PROCESS_TOKEN
    call journey_step

    ; --- Novelty tracking: bloom filter for unique token detection ---
    ; Hash token to 3 bloom positions, check if ALL set (seen before)
    mov eax, r12d
    mov edx, eax
    shr edx, 5                ; edx = token >> 5 = word index
    and edx, 63              ; edx mod 64 = word in 256-byte bloom
    mov ecx, eax
    and ecx, 31              ; bit position within word
    lea rsi, [rbx + STATE_OFFSET + ST_TOKEN_BLOOM]
    bt dword [rsi + rdx*4], ecx
    jc .token_seen
    ; NEW token — set bloom bit and increment novelty
    bts dword [rsi + rdx*4], ecx
    inc dword [rbx + STATE_OFFSET + ST_UNIQUE_TOKENS]
    inc dword [rbx + STATE_OFFSET + ST_NOVELTY_RECENT]
.token_seen:

    ; --- Context hash = predecessor only ---
    ; Pattern identity is independent of mood. A bike is a bike whether happy or angry.
    ; Mood affects BEHAVIOR (what to do with patterns), not IDENTITY (what patterns are).
    lea rsi, [rbx + STATE_OFFSET + ST_TOKEN_BUF]
    mov ecx, [rbx + STATE_OFFSET + ST_TOKEN_POS]
    mov r9, 0x9E3779B97F4A7C15  ; golden ratio prime

    ; Get previous token (position - 1)
    dec ecx
    and ecx, (ST_TOKEN_BUF_CAP - 1)
    mov r10d, [rsi + rcx * 4]  ; previous token

    ; Context = prev_token * prime (pure sequential context)
    mov rax, r10
    imul rax, r9
    mov r13, rax
    mov [rbx + STATE_OFFSET + ST_CTX_HASH], r13

    ; --- Update token ring buffer ---
    lea rax, [rbx + STATE_OFFSET + ST_TOKEN_POS]
    mov ecx, [rax]
    lea rdx, [rbx + STATE_OFFSET + ST_TOKEN_BUF]
    mov [rdx + rcx * 4], r12d
    inc ecx
    and ecx, (ST_TOKEN_BUF_CAP - 1)
    mov [rax], ecx

    ; Increment token count
    inc dword [rbx + STATE_OFFSET + ST_TOKEN_COUNT]

    ; --- Working memory: semantic slots (noun/verb/modifier) ---
    ; Classify token by low 2 bits: 0=noun, 1=verb, 2=modifier, 3=other
    ; This creates implicit binding structure from token statistics
    mov eax, r12d
    and eax, 0x3
    cmp eax, 0
    jne .slot_check_verb
    ; Noun slot
    mov [rbx + STATE_OFFSET + ST_CTX_SLOT_NOUN], r12d
    mov byte [rbx + STATE_OFFSET + ST_SLOT_RECENCY], 0      ; noun recency = 0
    jmp .slot_done
.slot_check_verb:
    cmp eax, 1
    jne .slot_check_mod
    ; Verb slot
    mov [rbx + STATE_OFFSET + ST_CTX_SLOT_VERB], r12d
    mov byte [rbx + STATE_OFFSET + ST_SLOT_RECENCY + 1], 0  ; verb recency = 0
    jmp .slot_done
.slot_check_mod:
    cmp eax, 2
    jne .slot_done
    ; Modifier slot
    mov [rbx + STATE_OFFSET + ST_CTX_SLOT_MOD], r12d
    mov byte [rbx + STATE_OFFSET + ST_SLOT_RECENCY + 2], 0  ; mod recency = 0
.slot_done:
    ; Age all recencies (saturate at 255)
    lea rax, [rbx + STATE_OFFSET + ST_SLOT_RECENCY]
    movzx ecx, byte [rax]
    cmp ecx, 255
    jge .slot_age1_done
    inc byte [rax]
.slot_age1_done:
    movzx ecx, byte [rax + 1]
    cmp ecx, 255
    jge .slot_age2_done
    inc byte [rax + 1]
.slot_age2_done:
    movzx ecx, byte [rax + 2]
    cmp ecx, 255
    jge .slot_age3_done
    inc byte [rax + 2]
.slot_age3_done:

    ; --- Phase 2: Compute structural context (dual-track) ---
    ; struct_ctx = Σ bind(ROLE_POS_i, token_vec[history[i]])
    ; This runs in parallel with the flat context hash
    push r12
    push r13
    call compute_struct_ctx
    pop r13
    pop r12

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

    ; --- Metabolic income: hits generate energy (correct predictions are valuable) ---
    mov rax, ENERGY_HIT_INCOME
    movq xmm5, rax
    movsd xmm6, [rbx + STATE_OFFSET + ST_ENERGY]
    addsd xmm6, xmm5
    mov rax, ENERGY_MAX
    movq xmm5, rax
    minsd xmm6, xmm5            ; cap at max
    movsd [rbx + STATE_OFFSET + ST_ENERGY], xmm6
    ; Track income
    mov rax, ENERGY_HIT_INCOME
    movq xmm5, rax
    addsd xmm5, [rbx + STATE_OFFSET + ST_ENERGY_INCOME]
    movsd [rbx + STATE_OFFSET + ST_ENERGY_INCOME], xmm5

    ; --- Self-knowledge: track context-type accuracy ---
    ; Extract ctx_type from hash (top 4 bits → 16 types)
    mov rax, [rbx + STATE_OFFSET + ST_CTX_HASH]
    shr rax, 60               ; top 4 bits → 0-15
    and eax, 0xF              ; mask to 4 bits
    ; Increment both hits and total for this context type
    lea rcx, [rbx + STATE_OFFSET + ST_CTX_TYPE_HITS]
    inc dword [rcx + rax * 4]       ; hits[ctx_type]++
    lea rcx, [rbx + STATE_OFFSET + ST_CTX_TYPE_TOTAL]
    inc dword [rcx + rax * 4]       ; total[ctx_type]++

    ; --- Topological metacognition: update confidence vector on HIT ---
    ; The system becomes more confident about this context type
    mov edi, r13d             ; ctx_hash (lower 32 bits from r13)
    mov esi, 1                ; is_hit = true
    call confidence_update

    ; Increment hit counter on the predicting region
    lea rax, [rbx + STATE_OFFSET + ST_PREDICT_REGION]
    mov rcx, [rax]
    test rcx, rcx
    jz .holo_hit              ; holo prediction — still count as hit
    inc dword [rcx + RHDR_HITS]

    ; === EMIT RECEIPT: EVENT_HIT (full context) ===
    push rcx                  ; save region ptr
    mov edi, EVENT_HIT                ; event_type
    mov esi, r13d                     ; ctx_hash
    mov edx, r12d                     ; actual_token (same as predicted for HIT)
    mov ecx, r12d                     ; predicted_token (same as actual)
    ; Hash the region pointer
    mov rax, [rsp]                    ; get region ptr from stack
    shr rax, 4
    mov r8d, eax                      ; region_hash
    mov r9d, [rbx + STATE_OFFSET + ST_RUNNER_UP_TOKEN] ; aux = runner-up
    movss xmm0, [rbx + STATE_OFFSET + ST_EXPECT_CONF]
    movss xmm1, [rbx + STATE_OFFSET + ST_PRESENCE + PRES_VALENCE * 4]
    cvtss2sd xmm1, xmm1
    call emit_receipt_full
    pop rcx

    ; STDP connection learning — strengthen temporal links
    push rcx
    mov rdi, rcx              ; firing region ptr
    call learn_connections
    pop rcx

    ; OCTOPUS: Hits slightly reduce dream pressure (performing well)
    mov edi, RPRES_DREAM
    mov rax, 0xBF847AE147AE147B  ; -0.01 f64 (small decay on success)
    movq xmm0, rax
    call accrue_pressure

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
    ; Bounds check: successor table is 256 entries
    cmp eax, 256
    jge .skip_successor

    ; Update successor_tbl[last_fired_idx] = current_idx
    movzx edx, word [rbx + STATE_OFFSET + ST_LAST_FIRED_IDX]
    cmp edx, 256
    jge .succ_update_idx      ; skip write if last_fired out of bounds
    lea rsi, [rbx + STATE_OFFSET + ST_SUCCESSOR_TBL]
    mov [rsi + rdx * 2], ax   ; current idx is successor of last fired

    ; Predict: successor_tbl[current_idx] = next region idx
    movzx edx, word [rsi + rax * 2]
    test dx, dx
    jz .successor_fallback
    cmp edx, REGION_TABLE_MAX
    jge .successor_fallback   ; invalid successor index
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
.succ_update_idx:
    ; Update last_fired_idx (bounded to u8 range for table safety)
    and eax, 0xFF
    mov [rbx + STATE_OFFSET + ST_LAST_FIRED_IDX], ax
.skip_successor:

    ; Fire hit hook
    mov edi, HOOK_ON_HIT
    mov esi, r12d
    call fire_hook

.holo_hit:
    ; Print hit feedback (graph or holographic)
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
    ; --- Self-knowledge: track context-type (miss → total only) ---
    mov rax, [rbx + STATE_OFFSET + ST_CTX_HASH]
    shr rax, 60               ; top 4 bits → 0-15
    and eax, 0xF
    lea rcx, [rbx + STATE_OFFSET + ST_CTX_TYPE_TOTAL]
    inc dword [rcx + rax * 4]       ; total[ctx_type]++ (no hit increment)

    ; --- Topological metacognition: update confidence vector on MISS ---
    ; The system becomes more anxious about this context type
    mov edi, r13d             ; ctx_hash (lower 32 bits from r13)
    xor esi, esi              ; is_hit = false (miss)
    call confidence_update

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

    ; --- Hypothesis confidence: how sure were we when wrong? ---
    ; Store prediction confidence at same index as miss buffer entry
    ; This turns misses into testable hypotheses
    lea rdi, [rbx + STATE_OFFSET + ST_HYPOTHESIS_CONF]
    movss xmm0, [rbx + STATE_OFFSET + ST_EXPECT_CONF]
    movss [rdi + rcx * 4], xmm0      ; hypothesis_conf[miss_pos] = expect_conf
    inc dword [rbx + STATE_OFFSET + ST_HYPOTHESIS_COUNT]

    ; Advance miss pos
    lea rax, [rbx + STATE_OFFSET + ST_MISS_POS]
    mov ecx, [rax]
    inc ecx
    cmp ecx, ST_MISS_BUF_CAP
    jl .no_miss_wrap
    xor ecx, ecx
.no_miss_wrap:
    mov [rax], ecx

    ; --- Counterfactual check: would runner-up have been right? ---
    mov eax, [rbx + STATE_OFFSET + ST_RUNNER_UP_TOKEN]
    test eax, eax
    jz .counterfact_done               ; no runner-up
    inc dword [rbx + STATE_OFFSET + ST_COUNTERFACT_TOTAL]
    cmp eax, r12d                      ; runner-up == actual token?
    jne .counterfact_done
    ; Runner-up would have been correct (counterfactual win)
    inc dword [rbx + STATE_OFFSET + ST_COUNTERFACT_WINS]
.counterfact_done:

    ; === EMIT RECEIPT: EVENT_MISS (full context for debugging) ===
    mov edi, EVENT_MISS               ; event_type
    mov esi, r13d                     ; ctx_hash
    mov edx, r12d                     ; actual_token
    mov ecx, [rbx + STATE_OFFSET + ST_EXPECT_TOKEN]   ; predicted_token (THE KEY!)
    ; Hash the region pointer for region dimension
    mov rax, [rbx + STATE_OFFSET + ST_PREDICT_REGION]
    shr rax, 4                        ; simple hash: shift off alignment bits
    mov r8d, eax                      ; region_hash
    mov r9d, [rbx + STATE_OFFSET + ST_RUNNER_UP_TOKEN] ; aux = runner-up token
    movss xmm0, [rbx + STATE_OFFSET + ST_EXPECT_CONF] ; confidence (was wrong)
    ; Get valence from presence
    movss xmm1, [rbx + STATE_OFFSET + ST_PRESENCE + PRES_VALENCE * 4]
    cvtss2sd xmm1, xmm1
    call emit_receipt_full

    ; --- Superpose structural context into schema trace ---
    ; This accumulates structural patterns from misses for schema learning
    ; Schema trace = Σ struct_ctx on each miss
    lea rdi, [rbx + STATE_OFFSET + ST_SCHEMA_TRACE_VEC]  ; dst = schema trace
    lea rsi, [rbx + STATE_OFFSET + ST_STRUCT_CTX_VEC]    ; src = current struct_ctx
    call holo_superpose_f64

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
    ; Current context (hash of prev token) → should predict this token
    mov rdi, r13              ; current context = hash(prev_token)
    mov esi, r12d             ; token to predict
    ; SOMATIC GROUNDING: negative energy_delta for miss (mistakes are costly)
    mov rax, 0xC024000000000000  ; -10.0 f64 (negative valence for errors)
    movq xmm0, rax
    call learn_pattern

    ; OCTOPUS: Misses accrue dream pressure (urge to consolidate)
    mov edi, RPRES_DREAM
    mov rax, 0x3FB999999999999A  ; 0.1 f64 pressure per miss
    movq xmm0, rax
    call accrue_pressure

    ; --- Inhibitory competition: wrong predictor should be suppressed ---
    ; If a confident region predicted wrong, the correct region should inhibit it.
    ; This creates lateral inhibition between competing predictions.
    lea rax, [rbx + STATE_OFFSET + ST_PREDICT_REGION]
    mov rcx, [rax]
    test rcx, rcx
    jz .after_counter          ; no predicting region to inhibit
    ; Find the region that correctly predicts this token in this context
    mov rdi, r13              ; current context
    mov esi, r12d
    push rcx                   ; save wrong-predictor
    call find_existing_pattern ; → rax = correct region (or 0)
    pop rcx                    ; rcx = wrong predictor
    test rax, rax
    jz .after_counter          ; no correct pattern found yet
    cmp rax, rcx
    je .after_counter          ; same region (shouldn't happen, but guard)
    ; Wire inhibition: correct → inhibits wrong (lateral suppression)
    cmp qword [rax + RHDR_INHIBIT_A], 0
    jne .try_inhib_b
    mov [rax + RHDR_INHIBIT_A], rcx
    mov rdx, INITIAL_WEIGHT
    movq xmm0, rdx
    movsd [rax + RHDR_W_INHIBIT_A], xmm0
    inc dword [rbx + STATE_OFFSET + ST_INHIBIT_LEARNED]
    jmp .after_counter
.try_inhib_b:
    cmp qword [rax + RHDR_INHIBIT_B], 0
    jne .after_counter         ; both slots full
    mov [rax + RHDR_INHIBIT_B], rcx
    mov rdx, INITIAL_WEIGHT
    movq xmm0, rdx
    movsd [rax + RHDR_W_INHIBIT_B], xmm0
    inc dword [rbx + STATE_OFFSET + ST_INHIBIT_LEARNED]
    jmp .after_counter

.no_prediction:
    ; No prediction existed — this is a new context
    ; === EMIT RECEIPT: EVENT_NEW ===
    mov edi, EVENT_NEW        ; event_type
    mov esi, r13d             ; ctx_hash (lower 32 bits of context)
    mov edx, r12d             ; token_id
    xorps xmm0, xmm0          ; confidence = 0 (no prediction)
    call emit_receipt_simple

    lea rdi, [rel token_msg]
    call print_cstr
    mov edi, r12d
    call print_hex32
    lea rdi, [rel no_predict_msg]
    call print_cstr

    ; Learn: current context (hash of prev token) → this token
    mov rdi, r13              ; current context = hash(prev_token)
    test rdi, rdi
    jz .after_counter         ; no context yet
    mov esi, r12d
    ; SOMATIC GROUNDING: neutral energy_delta for new context (just learning)
    xorpd xmm0, xmm0          ; 0.0 f64 (neutral valence for new info)
    call learn_pattern

    ; OCTOPUS: New contexts accrue observe pressure (need to monitor growth)
    mov edi, RPRES_OBSERVE
    mov rax, 0x3FA999999999999A  ; 0.05 f64 pressure per new token
    movq xmm0, rax
    call accrue_pressure

.after_counter:
    ; --- Organic dynamics: let internal pressure drive actions ---
    call update_organic_pressure

    ; --- Make next prediction ---
    ; Dispatch: predict what comes AFTER current token
    ; Context for prediction = hash(current_token) so we can find patterns that predict successors
    mov rax, r12              ; current token
    mov rcx, 0x9E3779B97F4A7C15  ; golden ratio prime (64-bit - can't use imul imm64)
    imul rax, rcx             ; hash(current_token)
    mov rdi, rax              ; context = hash(current_token)
    push rax                  ; save for ST_LAST_CTX
    call dispatch_predict     ; → eax = predicted token (0 if none)

    ; Store prediction for next step
    lea rcx, [rbx + STATE_OFFSET + ST_LAST_PREDICT]
    mov [rcx], eax
    ; Store prediction context (hash of current token, for matching on next step)
    pop rax
    lea rcx, [rbx + STATE_OFFSET + ST_LAST_CTX]
    mov [rcx], rax

    ; SOMATIC GROUNDING: Query and store valence for this context
    ; This gives us the "felt sense" of past experiences with this context
    push rax                  ; save ctx for valence query
    mov edi, eax              ; ctx_hash (lower 32)
    call holo_query_valence   ; → xmm0 = valence f64
    movsd [rbx + STATE_OFFSET + ST_LAST_VALENCE], xmm0
    pop rax

    ; OCTOPUS NERVOUS SYSTEM: Let distributed ganglia react
    ; This replaces centralized REPL control with local pressure-based triggering
    call tick_regulators

    ; DEVELOPMENTAL TRACKING: Update mastery metrics
    ; edi = 1 if last prediction was hit, 0 if miss (check ST_LAST_PREDICT vs actual)
    ; For simplicity, use self-prediction stats as proxy
    mov eax, [rbx + STATE_OFFSET + ST_SELF_PRED_HITS]
    mov edx, eax
    mov ecx, [rbx + STATE_OFFSET + ST_SELF_PRED_MISSES]
    add ecx, eax                ; total = hits + misses
    ; edi = 1 if we just had a hit (check if hits increased)
    ; Simplified: pass 1 if hits > misses trend
    cmp eax, [rbx + STATE_OFFSET + ST_SELF_PRED_MISSES]
    setg dil
    movzx edi, dil
    mov esi, edx                ; total hits
    ; edx already has total from earlier calculation
    push r12
    call maturity_update
    pop r12

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
;; Graph-based dispatch: entry table → traverse links → fallback
;; Spreads activation, decays dynamics, records fires.
;; ============================================================
global dispatch_predict
dispatch_predict:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 80               ; locals:
                              ; [rsp+0]  = best_token (u32)
                              ; [rsp+8]  = best_weight (f64)
                              ; [rsp+16] = best_region_ptr (u64)
                              ; [rsp+24] = depth (u32)
                              ; [rsp+28] = visited (u32)
                              ; [rsp+32] = entry_slot (u32)
                              ; [rsp+36] = ctx_hash_copy (u32)
                              ; --- Counterfactual: runner-up tracking ---
                              ; [rsp+40] = runner_up_token (u32)
                              ; [rsp+48] = runner_up_weight (f64)
                              ; [rsp+56] = runner_up_region (u64)
                              ; [rsp+64] = runner_up_conf (f32)
                              ; [rsp+68] = ctx_type (u32) for self-knowledge

    mov r12d, edi             ; context hash (lower 32 bits)
    mov [rsp + 36], edi       ; save copy
    mov rbx, SURFACE_BASE

    ; JOURNEY: record dispatch_predict
    mov edi, TRACE_DISPATCH_PREDICT
    call journey_step

    ; --- Metabolic cost: prediction attempt costs energy ---
    mov rax, ENERGY_PREDICT_COST
    movq xmm0, rax
    movsd xmm1, [rbx + STATE_OFFSET + ST_ENERGY]
    subsd xmm1, xmm0
    xorpd xmm2, xmm2
    maxsd xmm1, xmm2            ; floor at 0
    movsd [rbx + STATE_OFFSET + ST_ENERGY], xmm1
    addsd xmm0, [rbx + STATE_OFFSET + ST_ENERGY_SPENT]
    movsd [rbx + STATE_OFFSET + ST_ENERGY_SPENT], xmm0

    ; Zero trace counters
    mov dword [rbx + STATE_OFFSET + ST_TRACE_CANDIDATES], 0
    mov dword [rbx + STATE_OFFSET + ST_TRACE_MATCHED], 0
    mov dword [rbx + STATE_OFFSET + ST_EXPECT_IS_SCHEMA], 0

    ; Initialize best-match tracking
    mov dword [rsp + 0], 0    ; best_token
    mov rax, 0xBFF0000000000000  ; -1.0 f64 (any valid weight beats this)
    mov [rsp + 8], rax        ; best_weight = -1.0
    mov qword [rsp + 16], 0   ; best_region_ptr
    mov dword [rsp + 24], 0   ; depth
    mov dword [rsp + 28], 0   ; visited
    ; Initialize runner-up (counterfactual)
    mov dword [rsp + 40], 0   ; runner_up_token
    mov [rsp + 48], rax       ; runner_up_weight = -1.0
    mov qword [rsp + 56], 0   ; runner_up_region
    mov dword [rsp + 64], 0   ; runner_up_conf
    ; Extract context type (top 4 bits → 16 types for self-knowledge)
    mov eax, r12d
    shr eax, 28               ; top 4 bits
    mov [rsp + 68], eax       ; ctx_type

    ; --- RESONANCE QUERY: Check past outcomes for this context ---
    ; Query receipt trace for past HIT events with similar context
    ; High similarity → boost confidence; low → be cautious
    mov edi, EVENT_HIT        ; query for past HITs
    mov esi, r12d             ; ctx_hash
    xor edx, edx              ; token=0 (context-only query)
    call receipt_resonate     ; → xmm0 = similarity to past HITs (f64)
    movsd [rsp + 72], xmm0    ; save hit_similarity

    ; Query for past MISS events
    mov edi, EVENT_MISS
    mov esi, r12d
    xor edx, edx
    call receipt_resonate     ; → xmm0 = similarity to past MISSes
    ; Compute confidence_modulator = hit_sim - miss_sim (range [-1, +1])
    movsd xmm1, [rsp + 72]    ; hit_similarity
    subsd xmm1, xmm0          ; hit_sim - miss_sim
    movsd [rsp + 72], xmm1    ; save confidence_modulator

    ; --- TRY HOLOGRAPHIC PREDICTION FIRST ---
    mov edi, r12d             ; ctx_hash
    call holo_predict
    ; eax = best_token, xmm0 = confidence (f64)

    ; Save predicted token BEFORE modulation (uses rax for constants!)
    mov r15d, eax             ; r15d = best_token (preserved)

    ; --- APPLY RESONANCE MODULATION ---
    ; Adjust confidence based on past HIT/MISS patterns for this context
    ; modulated_conf = conf * (1 + 0.2 * confidence_modulator)
    ; This makes the system trust contexts with good history more
    movsd xmm2, [rsp + 72]    ; confidence_modulator (hit_sim - miss_sim)
    mov rax, 0x3FC999999999999A  ; 0.2 f64
    movq xmm3, rax
    mulsd xmm2, xmm3          ; 0.2 * modulator
    mov rax, 0x3FF0000000000000  ; 1.0 f64
    movq xmm3, rax
    addsd xmm2, xmm3          ; 1 + 0.2 * modulator
    mulsd xmm0, xmm2          ; conf * (1 + 0.2 * modulator)
    ; Clamp to [0, 1]
    xorpd xmm2, xmm2
    maxsd xmm0, xmm2          ; max(0, conf)
    movq xmm2, rax            ; 1.0
    minsd xmm0, xmm2          ; min(1, conf)

    ; Restore predicted token
    mov eax, r15d             ; eax = best_token

    ; Check if confidence > threshold (f64 comparison)
    ucomisd xmm0, [rel holo_thresh]
    jbe .holo_miss

    ; Holographic hit — record and return
    mov [rsp + 0], eax        ; best_token
    ; Update holo prediction stats (f64 accumulator)
    movsd xmm1, [rbx + STATE_OFFSET + ST_HOLO_PREDICT_SUM]
    addsd xmm1, xmm0
    movsd [rbx + STATE_OFFSET + ST_HOLO_PREDICT_SUM], xmm1
    inc dword [rbx + STATE_OFFSET + ST_HOLO_PREDICT_N]
    ; Store as expectation (convert f64 confidence to f32 for compat)
    mov [rbx + STATE_OFFSET + ST_EXPECT_TOKEN], eax
    cvtsd2ss xmm1, xmm0
    movss [rbx + STATE_OFFSET + ST_EXPECT_CONF], xmm1

    ; --- MEMBRANE CREDIT SHARING ---
    ; Holo predicted; check if a DISPATCH region agrees and credit it
    ; This allows NURSERY regions to accumulate hits for promotion
    push rax                    ; save predicted token
    ; DEBUG: print ctx and token being searched
    push rax
    lea rdi, [rel dbg_search_msg]
    call print_cstr
    mov edi, r12d
    call print_hex32
    lea rdi, [rel dbg_tok_msg]
    call print_cstr
    pop rax
    push rax
    mov edi, eax
    call print_hex32
    call print_newline
    pop rax
    ; END DEBUG
    mov edi, r12d               ; context hash
    mov esi, eax                ; predicted token
    call find_existing_pattern  ; → rax = matching region ptr or 0
    mov rcx, rax
    pop rax                     ; restore predicted token
    ; DEBUG: print if match found
    push rax
    push rcx
    test rcx, rcx
    jz .dbg_no_match
    lea rdi, [rel dbg_credit_msg]
    jmp .dbg_print
.dbg_no_match:
    lea rdi, [rel dbg_nocredit_msg]
.dbg_print:
    call print_cstr
    pop rcx
    pop rax
    ; END DEBUG
    test rcx, rcx
    jz .holo_no_dispatch
    ; Found matching DISPATCH region — credit it
    ; DEBUG: print region pointer
    push rax
    push rcx
    lea rdi, [rel dbg_region_msg]
    call print_cstr
    mov rdi, rcx
    call print_hex64
    call print_newline
    pop rcx
    pop rax
    ; END DEBUG
    inc dword [rcx + RHDR_HITS]
    ; Sync to region table entry
    push rax
    push rcx
    mov rdi, rcx
    call update_region_table_hits
    pop rcx
    pop rax
    mov qword [rbx + STATE_OFFSET + ST_PREDICT_REGION], rcx
    mov qword [rbx + STATE_OFFSET + ST_EXPECT_REGION], rcx
    jmp .predict_return
.holo_no_dispatch:
    mov qword [rbx + STATE_OFFSET + ST_EXPECT_REGION], 0
    mov qword [rbx + STATE_OFFSET + ST_PREDICT_REGION], 0
    jmp .predict_return

.holo_miss:
    ; Store sub-threshold holo token for coherence comparison with graph
    mov [rbx + STATE_OFFSET + ST_HOLO_LAST_TOKEN], eax

    ; Sub-threshold holographic signal → anticipatory buffer
    ; "Something forming in the distance" — not strong enough to predict,
    ; but present enough to notice. Accumulates until it materializes.
    test eax, eax
    jz .no_antic_signal         ; no token at all
    ; xmm0 still has confidence from holo_predict
    mov rax, ANTIC_SIGNAL_FLOOR
    movq xmm1, rax
    ucomisd xmm0, xmm1
    jbe .no_antic_signal        ; below noise floor
    ; Feed into anticipatory buffer
    mov edi, eax                ; token_id
    ; xmm0 = confidence (already set)
    call update_anticipatory
.no_antic_signal:

    ; --- Phase 3: Try Schema Dispatch ---
    ; Schemas use structural context with role bindings for prediction.
    ; This is faster than graph traversal and preserves positional info.
    push r12
    call schema_dispatch
    pop r12
    ; eax = predicted token, 0 if no match

    test eax, eax
    jz .no_schema_hit

    ; Schema hit! Return this prediction
    mov [rsp + 0], eax            ; best_token
    mov qword [rsp + 16], 0       ; no region for schema predictions
    mov dword [rbx + STATE_OFFSET + ST_EXPECT_IS_SCHEMA], 1
    jmp .predict_return

.no_schema_hit:

    ; --- Topological Metacognition: Feel the context before choosing strategy ---
    ; Query how we "feel" about this context type: anxious, neutral, or confident
    ; This creates per-topic dispatch mode selection (e.g., "I'm bad at math")

    ; First, get raw confidence score for observability
    mov edi, r12d             ; ctx_hash
    push r12                  ; save ctx_hash across call
    call confidence_query     ; → xmm0 = f64 confidence score
    movsd [rbx + STATE_OFFSET + ST_META_CONFIDENCE], xmm0
    pop r12

    ; Now get the feeling enum
    mov edi, r12d             ; ctx_hash
    call confidence_get_feeling
    ; eax = 0 (neutral), 1 (confident), 2 (anxious)

    ; Store for observability
    mov [rbx + STATE_OFFSET + ST_META_FEELING], eax

    cmp eax, FEELING_ANXIOUS
    je .feeling_anxious
    cmp eax, FEELING_CONFIDENT
    je .feeling_confident
    jmp .entry_point_select    ; neutral → use current dispatch mode

.feeling_anxious:
    ; "I'm anxious about this context" → be careful, deliberate
    ; Override dispatch mode to DELIBERATE for this prediction only
    mov dword [rbx + STATE_OFFSET + ST_DISPATCH_MODE], DMODE_DELIBERATE
    jmp .entry_point_select

.feeling_confident:
    ; "I'm confident about this context" → be quick, intuitive
    ; Override dispatch mode to FAST for this prediction only
    mov dword [rbx + STATE_OFFSET + ST_DISPATCH_MODE], DMODE_FAST

    ; --- ANTICIPATORY INTERFERENCE: Let the ghosts haunt the machine ---
    ; Scan anticipatory buffer for strong (but not yet fired) signals.
    ; Pre-bias RHDR_PRIME of regions that predict those tokens.
    ; This creates "priming": pathways lower their resistance before evidence arrives.
.anticipatory_prime:
    push r12                    ; save ctx_hash
    lea r14, [rbx + STATE_OFFSET + ST_ANTIC_BUF]
    xor ecx, ecx                ; antic buffer index

.antic_prime_loop:
    cmp ecx, ST_ANTIC_CAP
    jge .antic_prime_done

    push rcx
    imul edx, ecx, ST_ANTIC_ENTRY_SIZE
    lea rsi, [r14 + rdx]

    ; Check if this slot is active (token_id != 0)
    mov eax, [rsi + ABE_TOKEN_ID]
    test eax, eax
    jz .antic_prime_next

    ; Check if accumulated confidence is above priming threshold (0.3)
    movsd xmm0, [rsi + ABE_ACCUM_CONF]
    mov rax, 0x3FD3333333333333      ; 0.3 f64
    movq xmm1, rax
    ucomisd xmm0, xmm1
    jbe .antic_prime_next           ; not strong enough to prime

    ; This is a GHOST - strong anticipation for this token
    ; Find regions that predict this token and boost their activation
    mov r15d, eax                   ; anticipated token_id
    movsd xmm7, xmm0                ; save confidence

    ; Scan region table for regions predicting this token
    lea rdi, [rbx + REGION_TABLE_OFFSET]
    xor r8d, r8d                    ; region index

.antic_region_scan:
    cmp r8d, 32                     ; scan up to 32 regions (limit work)
    jge .antic_prime_next

    push r8
    imul eax, r8d, RTE_SIZE
    lea r9, [rdi + rax]

    ; Get region address
    mov r10, [r9 + RTE_ADDR]
    test r10, r10
    jz .antic_region_next

    ; Check if active dispatch region
    movzx eax, word [r9 + RTE_TYPE]
    cmp eax, RTYPE_DISPATCH
    jne .antic_check_resonant
    jmp .antic_check_prediction

.antic_check_resonant:
    cmp eax, RTYPE_RESONANT
    jne .antic_region_next

.antic_check_prediction:
    ; Decode what token this region predicts
    ; Dispatch: CMP EAX, ctx (3D xx xx xx xx) then MOV EAX, token (B8 xx xx xx xx) after JE
    ; Resonant: MOV EAX, ctx (B8 xx xx xx xx) then MOV EAX, token (B8 xx xx xx xx)
    lea rsi, [r10 + RHDR_SIZE]      ; code start
    movzx eax, byte [rsi]

    cmp al, 0x3D                    ; CMP EAX, imm32 (dispatch)
    jne .antic_try_resonant
    ; Skip past CMP (5) + JE (2) = 7, then MOV EAX at offset 7
    cmp byte [rsi + 7], 0xB8
    jne .antic_region_next
    mov eax, [rsi + 8]              ; predicted token
    jmp .antic_compare_token

.antic_try_resonant:
    cmp al, 0xB8                    ; MOV EAX, imm32 (resonant)
    jne .antic_region_next
    ; Second MOV EAX at offset 5
    cmp byte [rsi + 5], 0xB8
    jne .antic_region_next
    mov eax, [rsi + 6]              ; predicted token

.antic_compare_token:
    cmp eax, r15d                   ; does this region predict our ghost token?
    jne .antic_region_next

    ; MATCH: This region predicts the anticipated token
    ; Boost its PRIME by confidence * 0.5 (ghosts partially activate pathways)
    movsd xmm0, xmm7                ; anticipated confidence
    mov rax, 0x3FE0000000000000     ; 0.5 f64
    movq xmm1, rax
    mulsd xmm0, xmm1                ; prime_boost = conf * 0.5
    addsd xmm0, [r10 + RHDR_PRIME]
    movsd [r10 + RHDR_PRIME], xmm0  ; GHOSTLY PRIMING APPLIED

.antic_region_next:
    pop r8
    inc r8d
    jmp .antic_region_scan

.antic_prime_next:
    pop rcx
    inc ecx
    jmp .antic_prime_loop

.antic_prime_done:
    pop r12                         ; restore ctx_hash

.entry_point_select:
    ; --- Entry point selection ---
    ; entry_slot = (ctx_hash >> 4) & 0xF
    mov eax, r12d
    shr eax, 4
    and eax, 0xF
    mov [rsp + 32], eax       ; save slot
    mov r14d, eax             ; r14d = slot

    ; Lookup entry table
    lea rax, [rbx + STATE_OFFSET + ST_ENTRY_TABLE]
    mov r13, [rax + r14 * 8]  ; r13 = entry_ptr (region header ptr)

    ; If entry_ptr is 0, skip to linear fallback
    test r13, r13
    jz .graph_fallback

    ; --- Check dispatch mode for strategy selection ---
    mov eax, [rbx + STATE_OFFSET + ST_DISPATCH_MODE]
    cmp eax, DMODE_CAUSAL
    je .mode_causal
    cmp eax, DMODE_STRUCTURAL
    je .mode_structural
    jmp .graph_traverse           ; default: use standard graph traversal

.mode_causal:
    ; CAUSAL mode: consult causal log for recently-fired connections
    ; Look for chains that led to success
    lea rdi, [rbx + STATE_OFFSET + ST_CAUSAL_LOG]
    mov ecx, [rbx + STATE_OFFSET + ST_CAUSAL_LOG_POS]
    test ecx, ecx
    jz .graph_traverse            ; no causal log, fall back
    dec ecx
    imul edx, ecx, ST_CAUSAL_LOG_ENTRY
    ; Get most recent dst from causal log
    movzx eax, word [rdi + rdx + 2]
    ; Try to find a region with matching low-16 bits
    ; This biases toward recently-causally-connected regions
    ; Just boost prime on regions with matching low bits
    lea rsi, [rbx + REGION_TABLE_OFFSET]
    xor ecx, ecx
.causal_boost_loop:
    cmp ecx, 16                   ; check first 16 regions
    jge .graph_traverse
    push rcx
    imul rdi, rcx, RTE_SIZE
    add rdi, rsi
    mov r8, [rdi + RTE_ADDR]
    test r8, r8
    jz .causal_boost_next
    mov edx, r8d
    and edx, 0xFFFF
    cmp edx, eax
    jne .causal_boost_next
    ; Boost this region's prime (causal connection recently fired)
    mov rax, 0x3FB999999999999A   ; 0.1 f64
    movq xmm0, rax
    addsd xmm0, [r8 + RHDR_PRIME]
    movsd [r8 + RHDR_PRIME], xmm0
.causal_boost_next:
    pop rcx
    inc ecx
    jmp .causal_boost_loop

.mode_structural:
    ; STRUCTURAL mode: weight by working memory slot binding
    ; If token matches a bound slot, boost matching schema regions
    mov eax, [rbx + STATE_OFFSET + ST_CTX_SLOT_NOUN]
    xor eax, r12d                 ; XOR with current context hash
    and eax, 0xF0                 ; mask to top nibble pattern
    ; Just sets a structural bias flag — traversal will use this
    mov [rsp + 72], eax           ; store structural bias pattern
    jmp .graph_traverse

    ; --- Graph Traversal Loop ---
.graph_traverse:
    ; Check depth limit
    cmp dword [rsp + 24], MAX_TRAVERSE_DEPTH
    jge .graph_done

    ; Validate current region pointer (basic sanity)
    ; Must be within dispatch area
    mov rax, SURFACE_BASE + DISPATCH_OFFSET
    cmp r13, rax
    jb .graph_done
    lea rax, [rax + DISPATCH_MAX_SIZE]
    cmp r13, rax
    jae .graph_done

    ; Check flags — skip condemned
    movzx eax, word [r13 + RHDR_FLAGS]
    test eax, RFLAG_ACTIVE
    jz .graph_follow_next
    test eax, RFLAG_CONDEMNED
    jnz .graph_follow_next

    ; Trace: candidate considered
    inc dword [rbx + STATE_OFFSET + ST_TRACE_CANDIDATES]
    inc dword [rsp + 28]      ; visited++

    ; Detect region type by first opcode
    ; 0x3D = CMP EAX, imm32 (exact dispatch)
    ; 0xB8 = MOV EAX, imm32 (resonant dispatch)
    cmp byte [r13 + RHDR_SIZE], 0x3D
    je .graph_exact_dispatch
    cmp byte [r13 + RHDR_SIZE], 0xB8
    je .graph_resonant_dispatch
    jmp .graph_follow_next

.graph_exact_dispatch:
    ; Standard exact-match context check
    ; Compare BASE context only (bits 0-23), ignore mood (bits 24-31)
    ; Mood orients self-regulation, but skills work regardless of mood
    mov eax, [r13 + RHDR_SIZE + 1]   ; stored context (imm32)
    and eax, 0x00FFFFFF              ; strip mood from stored
    mov edi, r12d
    and edi, 0x00FFFFFF              ; strip mood from current

    ; Schema handling
    test eax, 0x0F
    jnz .graph_exact_cmp
    ; Schema pattern — mask to schema granularity
    and edi, 0x00FFFFF0
    and eax, 0x00FFFFF0
    cmp eax, edi
    jne .graph_no_match
    mov dword [rbx + STATE_OFFSET + ST_EXPECT_IS_SCHEMA], 1
    jmp .graph_ctx_matched
.graph_exact_cmp:
    cmp eax, edi
    jne .graph_no_match
    jmp .graph_ctx_matched

.graph_resonant_dispatch:
    ; --- RESONANT GRAPH: Fuzzy VSA matching ---
    ; Call resonant_match(region_ptr, ctx_hash) → xmm0 = similarity
    push r13                  ; save current region ptr
    mov rdi, r13
    mov esi, r12d
    call resonant_match
    pop r13                   ; restore region ptr

    ; Check if similarity exceeds threshold
    ucomisd xmm0, [rel resonant_thresh]
    jbe .graph_no_match       ; below threshold, try other paths

    ; Store similarity for weight calculation
    sub rsp, 8
    movsd [rsp], xmm0         ; save similarity on stack

    ; Fall through to matched path - we have a resonant match
    jmp .graph_ctx_matched_resonant

.graph_ctx_matched:
    ; --- EXACT MATCH: compute effective_weight ---
    inc dword [rbx + STATE_OFFSET + ST_TRACE_MATCHED]

    ; effective_weight = hits/(hits+misses) + activation + prime_level
    mov eax, [r13 + RHDR_HITS]
    mov edx, [r13 + RHDR_MISSES]
    add edx, eax
    test edx, edx
    jz .graph_ew_zero
    cvtsi2sd xmm0, eax
    cvtsi2sd xmm1, edx
    divsd xmm0, xmm1          ; accuracy ratio (f64)
    jmp .graph_ew_add
.graph_ew_zero:
    xorpd xmm0, xmm0
.graph_ew_add:
    addsd xmm0, [r13 + RHDR_ACTIVATION]
    addsd xmm0, [r13 + RHDR_PRIME]
    ; xmm0 = effective_weight
    jmp .graph_compare_best

.graph_ctx_matched_resonant:
    ; --- RESONANT MATCH: compute effective_weight with similarity weighting ---
    inc dword [rbx + STATE_OFFSET + ST_TRACE_MATCHED]

    ; Retrieve saved similarity from stack
    movsd xmm2, [rsp]         ; xmm2 = similarity
    add rsp, 8                ; restore stack

    ; effective_weight = (hits/(hits+misses) + activation + prime) * similarity
    mov eax, [r13 + RHDR_HITS]
    mov edx, [r13 + RHDR_MISSES]
    add edx, eax
    test edx, edx
    jz .graph_res_ew_zero
    cvtsi2sd xmm0, eax
    cvtsi2sd xmm1, edx
    divsd xmm0, xmm1          ; accuracy ratio
    jmp .graph_res_ew_add
.graph_res_ew_zero:
    xorpd xmm0, xmm0
.graph_res_ew_add:
    addsd xmm0, [r13 + RHDR_ACTIVATION]
    addsd xmm0, [r13 + RHDR_PRIME]
    mulsd xmm0, xmm2          ; weight by similarity
    ; xmm0 = effective_weight (similarity-weighted)

.graph_compare_best:
    ; Compare with best
    ucomisd xmm0, [rsp + 8]
    jbe .graph_check_runner_up

    ; New best match — demote old best to runner-up (counterfactual)
    mov eax, [rsp + 0]
    mov [rsp + 40], eax               ; runner_up_token = old best_token
    movsd xmm1, [rsp + 8]
    movsd [rsp + 48], xmm1            ; runner_up_weight = old best_weight
    mov rax, [rsp + 16]
    mov [rsp + 56], rax               ; runner_up_region = old best_region

    ; Store new best - token location differs for exact vs resonant
    ; Exact: [RHDR_SIZE+8] after JNE offset
    ; Resonant: [RHDR_SIZE+6] after first MOV EAX
    cmp byte [r13 + RHDR_SIZE], 0xB8
    je .graph_store_resonant_token
    mov eax, [r13 + RHDR_SIZE + 8]    ; exact: predicted token after JNE
    jmp .graph_store_best
.graph_store_resonant_token:
    mov eax, [r13 + RHDR_SIZE + 6]    ; resonant: token from second MOV
.graph_store_best:
    mov [rsp + 0], eax                ; best_token
    movsd [rsp + 8], xmm0             ; best_weight
    mov [rsp + 16], r13               ; best_region_ptr
    jmp .graph_not_best

.graph_check_runner_up:
    ; Not best, but better than runner-up?
    ucomisd xmm0, [rsp + 48]
    jbe .graph_not_best
    ; New runner-up (counterfactual alternative)
    cmp byte [r13 + RHDR_SIZE], 0xB8
    je .graph_runner_resonant
    mov eax, [r13 + RHDR_SIZE + 8]    ; exact token
    jmp .graph_store_runner
.graph_runner_resonant:
    mov eax, [r13 + RHDR_SIZE + 6]    ; resonant token
.graph_store_runner:
    mov [rsp + 40], eax               ; runner_up_token
    movsd [rsp + 48], xmm0            ; runner_up_weight
    mov [rsp + 56], r13               ; runner_up_region

.graph_not_best:
    ; --- Spread activation to excite targets ---
    mov rdi, r13
    call spread_activation

    ; Follow excite_a for depth search (refinement)
    mov r13, [r13 + RHDR_EXCITE_A]
    inc dword [rsp + 24]      ; depth++
    test r13, r13
    jnz .graph_traverse
    jmp .graph_done

.graph_no_match:
    ; No context match — fall through to follow next
.graph_follow_next:
    inc dword [rsp + 24]      ; depth++ (any link traversal counts)
    mov rax, [r13 + RHDR_NEXT_A]
    test rax, rax
    jnz .graph_follow_a
    ; Try next_b
    mov rax, [r13 + RHDR_NEXT_B]
    test rax, rax
    jnz .graph_follow_b
    ; Both dead — done with graph traversal
    jmp .graph_done
.graph_follow_a:
    mov r13, rax
    jmp .graph_traverse
.graph_follow_b:
    mov r13, rax
    jmp .graph_traverse

.graph_done:
    ; Check if graph traversal found anything
    cmp qword [rsp + 16], 0
    jne .dispatch_found

.graph_fallback:
    ; --- Fallback: scan primed regions (activation > threshold) ---
    lea r13, [rbx + REGION_TABLE_OFFSET]
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov ecx, [rax]            ; region count
    xor edx, edx              ; start from index 0

.fallback_scan:
    cmp edx, ecx
    jge .fallback_done
    push rcx
    push rdx

    imul rdi, rdx, RTE_SIZE
    add rdi, r13
    movzx eax, word [rdi + RTE_TYPE]

    ; Check for exact dispatch regions (RTYPE_DISPATCH)
    cmp eax, RTYPE_DISPATCH
    je .fallback_check_dispatch

    ; Check for resonant regions (RTYPE_RESONANT) - fuzzy matching
    cmp eax, RTYPE_RESONANT
    je .fallback_check_resonant

    jmp .fallback_skip

.fallback_check_dispatch:
    ; Standard exact-match dispatch region
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_CONDEMNED
    jnz .fallback_skip

    mov rsi, [rdi + RTE_ADDR]

    ; Check context match FIRST (even if not primed, context match matters)
    cmp byte [rsi + RHDR_SIZE], 0x3D
    jne .fallback_skip

    ; Trace: candidate
    inc dword [rbx + STATE_OFFSET + ST_TRACE_CANDIDATES]
    mov eax, [rsi + RHDR_SIZE + 1]
    test eax, 0x0F
    jnz .fallback_exact
    mov edi, r12d
    and edi, 0xFFFFFFF0
    cmp eax, edi
    jne .fallback_skip
    mov dword [rbx + STATE_OFFSET + ST_EXPECT_IS_SCHEMA], 1
    jmp .fallback_matched
.fallback_exact:
    cmp eax, r12d
    jne .fallback_skip
.fallback_matched:
    inc dword [rbx + STATE_OFFSET + ST_TRACE_MATCHED]

    ; Compute effective_weight
    mov eax, [rsi + RHDR_HITS]
    mov edx, [rsi + RHDR_MISSES]
    add edx, eax
    test edx, edx
    jz .fb_ew_zero
    cvtsi2sd xmm0, eax
    cvtsi2sd xmm1, edx
    divsd xmm0, xmm1
    jmp .fb_ew_add
.fb_ew_zero:
    xorpd xmm0, xmm0
.fb_ew_add:
    addsd xmm0, [rsi + RHDR_ACTIVATION]
    addsd xmm0, [rsi + RHDR_PRIME]

    ; Compare with best
    ucomisd xmm0, [rsp + 16 + 8]   ; adjust for 2 pushes
    jbe .fallback_skip
    ; New best
    mov eax, [rsi + RHDR_SIZE + 8]
    mov [rsp + 16 + 0], eax
    movsd [rsp + 16 + 8], xmm0
    mov [rsp + 16 + 16], rsi
    jmp .fallback_skip

.fallback_check_resonant:
    ; --- RESONANT FALLBACK: Fuzzy matching via VSA similarity ---
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_CONDEMNED
    jnz .fallback_skip

    mov rsi, [rdi + RTE_ADDR]
    inc dword [rbx + STATE_OFFSET + ST_TRACE_CANDIDATES]

    ; Save rsi across call
    push rsi

    ; Call resonant_match(region_ptr, ctx_hash) → xmm0 = similarity
    mov rdi, rsi
    mov esi, r12d
    call resonant_match

    ; Restore rsi
    pop rsi

    ; Check if similarity exceeds threshold
    ucomisd xmm0, [rel resonant_thresh]
    jbe .fallback_skip

    ; RESONANT MATCH
    inc dword [rbx + STATE_OFFSET + ST_TRACE_MATCHED]

    ; Save similarity in xmm2
    movsd xmm2, xmm0

    ; Compute effective_weight with similarity weighting
    mov eax, [rsi + RHDR_HITS]
    mov edx, [rsi + RHDR_MISSES]
    add edx, eax
    test edx, edx
    jz .fb_res_ew_zero
    cvtsi2sd xmm0, eax
    cvtsi2sd xmm1, edx
    divsd xmm0, xmm1
    jmp .fb_res_ew_add
.fb_res_ew_zero:
    xorpd xmm0, xmm0
.fb_res_ew_add:
    addsd xmm0, [rsi + RHDR_ACTIVATION]
    addsd xmm0, [rsi + RHDR_PRIME]
    mulsd xmm0, xmm2          ; weight by similarity

    ; Compare with best
    ucomisd xmm0, [rsp + 16 + 8]
    jbe .fallback_skip
    ; New best resonant match
    mov eax, [rsi + RHDR_SIZE + 6]    ; token from resonant region
    mov [rsp + 16 + 0], eax
    movsd [rsp + 16 + 8], xmm0
    mov [rsp + 16 + 16], rsi

.fallback_skip:
    pop rdx
    pop rcx
    inc edx
    jmp .fallback_scan

.fallback_done:
    ; Check if fallback found anything
    cmp qword [rsp + 16], 0
    jne .dispatch_found

    ; --- Last resort: full linear scan (no priming filter) ---
    lea r13, [rbx + REGION_TABLE_OFFSET]
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov ecx, [rax]
    xor edx, edx              ; start from index 0

.linear_scan:
    cmp edx, ecx
    jge .linear_done
    push rcx
    push rdx

    imul rdi, rdx, RTE_SIZE
    add rdi, r13
    movzx eax, word [rdi + RTE_TYPE]

    ; Check for exact dispatch regions (RTYPE_DISPATCH)
    cmp eax, RTYPE_DISPATCH
    je .linear_check_dispatch

    ; Check for resonant regions (RTYPE_RESONANT) - fuzzy matching
    cmp eax, RTYPE_RESONANT
    je .linear_check_resonant

    jmp .linear_skip

.linear_check_dispatch:
    ; Standard exact-match dispatch region
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_CONDEMNED
    jnz .linear_skip

    mov rsi, [rdi + RTE_ADDR]
    inc dword [rbx + STATE_OFFSET + ST_TRACE_CANDIDATES]

    cmp byte [rsi + RHDR_SIZE], 0x3D
    jne .linear_skip
    mov eax, [rsi + RHDR_SIZE + 1]
    test eax, 0x0F
    jnz .linear_exact
    mov edi, r12d
    and edi, 0xFFFFFFF0
    cmp eax, edi
    jne .linear_skip
    mov dword [rbx + STATE_OFFSET + ST_EXPECT_IS_SCHEMA], 1
    jmp .linear_matched
.linear_exact:
    cmp eax, r12d
    jne .linear_skip
.linear_matched:
    inc dword [rbx + STATE_OFFSET + ST_TRACE_MATCHED]

    ; Compute effective_weight
    mov eax, [rsi + RHDR_HITS]
    mov edx, [rsi + RHDR_MISSES]
    add edx, eax
    test edx, edx
    jz .lin_ew_zero
    cvtsi2sd xmm0, eax
    cvtsi2sd xmm1, edx
    divsd xmm0, xmm1
    jmp .lin_ew_add
.lin_ew_zero:
    xorpd xmm0, xmm0
.lin_ew_add:
    addsd xmm0, [rsi + RHDR_ACTIVATION]
    addsd xmm0, [rsi + RHDR_PRIME]

    ucomisd xmm0, [rsp + 16 + 8]
    jbe .linear_skip
    mov eax, [rsi + RHDR_SIZE + 8]
    mov [rsp + 16 + 0], eax
    movsd [rsp + 16 + 8], xmm0
    mov [rsp + 16 + 16], rsi
    jmp .linear_skip

.linear_check_resonant:
    ; --- RESONANT DISPATCH: Fuzzy matching via VSA similarity ---
    ; RTYPE_RESONANT regions use vector similarity instead of exact CMP
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_CONDEMNED
    jnz .linear_skip

    mov rsi, [rdi + RTE_ADDR]
    inc dword [rbx + STATE_OFFSET + ST_TRACE_CANDIDATES]

    ; Save rsi (region ptr) across call - use stack
    push rsi

    ; Call resonant_match(region_ptr, ctx_hash) → xmm0 = similarity
    mov rdi, rsi              ; region_ptr
    mov esi, r12d             ; ctx_hash
    call resonant_match
    ; xmm0 = similarity score [0.0, 1.0]

    ; Restore rsi
    pop rsi

    ; Check if similarity exceeds resonant threshold (0.7)
    ucomisd xmm0, [rel resonant_thresh]
    jbe .linear_skip          ; below threshold, no match

    ; RESONANT MATCH: similarity > threshold
    inc dword [rbx + STATE_OFFSET + ST_TRACE_MATCHED]

    ; Compute effective_weight = similarity * (accuracy + activation + prime)
    ; First, save similarity in xmm2
    movsd xmm2, xmm0

    ; Compute accuracy
    mov eax, [rsi + RHDR_HITS]
    mov edx, [rsi + RHDR_MISSES]
    add edx, eax
    test edx, edx
    jz .res_ew_zero
    cvtsi2sd xmm0, eax
    cvtsi2sd xmm1, edx
    divsd xmm0, xmm1          ; accuracy
    jmp .res_ew_add
.res_ew_zero:
    xorpd xmm0, xmm0
.res_ew_add:
    addsd xmm0, [rsi + RHDR_ACTIVATION]
    addsd xmm0, [rsi + RHDR_PRIME]
    ; Multiply by similarity score (weigh by how fuzzy the match is)
    mulsd xmm0, xmm2          ; effective_weight = (acc + act + prime) * similarity

    ; Compare with current best
    ucomisd xmm0, [rsp + 16 + 8]
    jbe .linear_skip

    ; New best - extract predicted token from resonant region
    ; Resonant layout: [+0] MOV EAX, ctx_hash; [+5] MOV EAX, token_id; [+10] RET
    mov eax, [rsi + RHDR_SIZE + 6]    ; token_id (from second MOV instruction)
    mov [rsp + 16 + 0], eax           ; best_token
    movsd [rsp + 16 + 8], xmm0        ; best_weight
    mov [rsp + 16 + 16], rsi          ; best_region_ptr

.linear_skip:
    pop rdx
    pop rcx
    inc edx
    jmp .linear_scan

.linear_done:
    ; Check if linear scan found anything
    cmp qword [rsp + 16], 0
    jne .dispatch_found

.no_match:
    xor eax, eax             ; no prediction
    lea rcx, [rbx + STATE_OFFSET + ST_PREDICT_REGION]
    mov qword [rcx], 0
    mov qword [rbx + STATE_OFFSET + ST_EXPECT_REGION], 0

    jmp .predict_return

.dispatch_found:
    ; --- We have a match ---
    mov eax, [rsp + 0]        ; best_token
    mov rsi, [rsp + 16]       ; best_region_ptr

    ; --- Coherence tracking: does graph agree with holo? ---
    mov [rbx + STATE_OFFSET + ST_GRAPH_LAST_TOKEN], eax
    mov ecx, [rbx + STATE_OFFSET + ST_HOLO_LAST_TOKEN]
    test ecx, ecx
    jz .skip_coherence         ; no holo prediction to compare
    cmp ecx, eax
    jne .coherence_disagree
    ; AGREE: holographic and graph both predict same thing → internal consistency
    inc dword [rbx + STATE_OFFSET + ST_COHERENCE_AGREE]
    jmp .skip_coherence
.coherence_disagree:
    ; DISAGREE: tension between memory systems → needs resolution
    inc dword [rbx + STATE_OFFSET + ST_COHERENCE_DISAGREE]
.skip_coherence:

    ; Store as predicting region
    lea rcx, [rbx + STATE_OFFSET + ST_PREDICT_REGION]
    mov [rcx], rsi

    ; --- Record firing in fire ring ---
    push rax
    push rsi
    mov rdi, rsi
    call record_fire
    pop rsi
    pop rax

    ; --- Update entry table for this context slot ---
    mov ecx, [rsp + 32]       ; entry_slot
    lea rdx, [rbx + STATE_OFFSET + ST_ENTRY_TABLE]
    mov [rdx + rcx * 8], rsi  ; entry_table[slot] = winning region

    ; --- Store graph trace metrics ---
    mov ecx, [rsp + 24]
    mov [rbx + STATE_OFFSET + ST_GRAPH_DEPTH], ecx
    mov ecx, [rsp + 28]
    mov [rbx + STATE_OFFSET + ST_GRAPH_VISITED], ecx

    ; --- Fill expectation bundle ---
    jmp .fill_expect

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

.predict_return:
    ; Save return value (predicted token)
    push rax

    ; Store runner-up for counterfactual analysis
    mov eax, [rsp + 8 + 40]           ; adjust for push
    mov [rbx + STATE_OFFSET + ST_RUNNER_UP_TOKEN], eax
    cvtsd2ss xmm0, qword [rsp + 8 + 48]
    movss [rbx + STATE_OFFSET + ST_RUNNER_UP_CONF], xmm0
    mov rax, [rsp + 8 + 56]
    mov [rbx + STATE_OFFSET + ST_RUNNER_UP_REGION], rax

    ; Restore return value
    pop rax

    add rsp, 80
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; spread_activation(region_ptr)
;; rdi = firing region header ptr
;; Updates excite/inhibit targets' prime and activation levels.
;; ============================================================
global spread_activation
spread_activation:
    push rbx
    push r12
    mov r12, rdi              ; firing region

    ; Load source activation
    movsd xmm0, [r12 + RHDR_ACTIVATION]   ; source.activation

    ; --- Excite target A ---
    mov rax, [r12 + RHDR_EXCITE_A]
    test rax, rax
    jz .spread_excite_b
    ; target_a.prime += source.activation * w_excite_a
    movsd xmm1, [r12 + RHDR_W_EXCITE_A]
    movsd xmm2, xmm0
    mulsd xmm2, xmm1          ; source.activation * w_excite_a
    addsd xmm2, [rax + RHDR_PRIME]
    movsd [rax + RHDR_PRIME], xmm2

    ; --- Causal chain logging: record this connection fired ---
    push rax
    push rcx
    mov rbx, SURFACE_BASE
    mov ecx, [rbx + STATE_OFFSET + ST_CAUSAL_LOG_POS]
    lea rdi, [rbx + STATE_OFFSET + ST_CAUSAL_LOG]
    imul edx, ecx, ST_CAUSAL_LOG_ENTRY
    ; Entry: (src:u16, dst:u16, step:u32)
    mov eax, r12d
    mov [rdi + rdx], ax           ; src (low 16 bits of ptr)
    pop rcx
    pop rax
    push rax
    push rcx
    mov [rdi + rdx + 2], ax       ; dst (low 16 bits of target ptr)
    mov eax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov [rdi + rdx + 4], eax      ; step
    ; Advance log position (wrap at cap)
    inc ecx
    cmp ecx, ST_CAUSAL_LOG_CAP
    jl .causal_log_no_wrap_a
    xor ecx, ecx
.causal_log_no_wrap_a:
    mov [rbx + STATE_OFFSET + ST_CAUSAL_LOG_POS], ecx
    pop rcx
    pop rax

    ; target_a.activation += source.prime * w_excite_a * 0.5
    movsd xmm2, [r12 + RHDR_PRIME]
    mulsd xmm2, xmm1
    mulsd xmm2, [rel half_point_o]
    addsd xmm2, [rax + RHDR_ACTIVATION]
    ; Clamp to [0.0, 1.0]
    xorpd xmm3, xmm3
    maxsd xmm2, xmm3
    movsd xmm3, [rel one_point_o]
    minsd xmm2, xmm3
    movsd [rax + RHDR_ACTIVATION], xmm2

.spread_excite_b:
    ; --- Excite target B ---
    mov rax, [r12 + RHDR_EXCITE_B]
    test rax, rax
    jz .spread_inhibit_a
    movsd xmm1, [r12 + RHDR_W_EXCITE_B]
    movsd xmm2, xmm0
    mulsd xmm2, xmm1
    addsd xmm2, [rax + RHDR_PRIME]
    movsd [rax + RHDR_PRIME], xmm2

    ; --- Causal chain logging for excite_b ---
    push rax
    push rcx
    mov rbx, SURFACE_BASE
    mov ecx, [rbx + STATE_OFFSET + ST_CAUSAL_LOG_POS]
    lea rdi, [rbx + STATE_OFFSET + ST_CAUSAL_LOG]
    imul edx, ecx, ST_CAUSAL_LOG_ENTRY
    mov eax, r12d
    mov [rdi + rdx], ax           ; src
    pop rcx
    pop rax
    push rax
    push rcx
    mov [rdi + rdx + 2], ax       ; dst
    mov eax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov [rdi + rdx + 4], eax      ; step
    inc ecx
    cmp ecx, ST_CAUSAL_LOG_CAP
    jl .causal_log_no_wrap_b
    xor ecx, ecx
.causal_log_no_wrap_b:
    mov [rbx + STATE_OFFSET + ST_CAUSAL_LOG_POS], ecx
    pop rcx
    pop rax

    ; target_b.activation += source.prime * w_excite_b * 0.5
    movsd xmm2, [r12 + RHDR_PRIME]
    mulsd xmm2, xmm1
    mulsd xmm2, [rel half_point_o]
    addsd xmm2, [rax + RHDR_ACTIVATION]
    xorpd xmm3, xmm3
    maxsd xmm2, xmm3
    movsd xmm3, [rel one_point_o]
    minsd xmm2, xmm3
    movsd [rax + RHDR_ACTIVATION], xmm2

.spread_inhibit_a:
    ; --- Inhibit target A ---
    mov rax, [r12 + RHDR_INHIBIT_A]
    test rax, rax
    jz .spread_inhibit_b
    ; target.activation -= source.activation * w_inhibit_a
    movsd xmm1, [r12 + RHDR_W_INHIBIT_A]
    movsd xmm2, xmm0
    mulsd xmm2, xmm1
    movsd xmm3, [rax + RHDR_ACTIVATION]
    subsd xmm3, xmm2
    ; Clamp to >= 0
    xorpd xmm4, xmm4
    maxsd xmm3, xmm4
    movsd [rax + RHDR_ACTIVATION], xmm3

.spread_inhibit_b:
    ; --- Inhibit target B ---
    mov rax, [r12 + RHDR_INHIBIT_B]
    test rax, rax
    jz .spread_done
    movsd xmm1, [r12 + RHDR_W_INHIBIT_B]
    movsd xmm2, xmm0
    mulsd xmm2, xmm1
    movsd xmm3, [rax + RHDR_ACTIVATION]
    subsd xmm3, xmm2
    xorpd xmm4, xmm4
    maxsd xmm3, xmm4
    movsd [rax + RHDR_ACTIVATION], xmm3

.spread_done:
    ; Set firing region's activation to 1.0 (it just fired)
    movsd xmm0, [rel one_point_o]
    movsd [r12 + RHDR_ACTIVATION], xmm0

    pop r12
    pop rbx
    ret

;; ============================================================
;; decay_all_regions
;; Walk all regions: prime *= PRIME_DECAY, activation *= ACTIVATION_DECAY
;; ============================================================
global decay_all_regions
decay_all_regions:
    push rbx
    push r12
    push r13

    mov rbx, SURFACE_BASE
    lea r12, [rbx + REGION_TABLE_OFFSET]
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov r13d, [rax]

    ; Preload decay constants
    movsd xmm4, [rel prime_decay]     ; 0.9
    movsd xmm5, [rel activ_decay]     ; 0.85

    xor ecx, ecx
.decay_loop:
    cmp ecx, r13d
    jge .decay_done
    push rcx

    imul rdi, rcx, RTE_SIZE
    add rdi, r12
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_CONDEMNED
    jnz .decay_next

    mov rsi, [rdi + RTE_ADDR]
    test rsi, rsi              ; null check - skip if RTE_ADDR is 0
    jz .decay_next

    ; prime *= PRIME_DECAY
    movsd xmm0, [rsi + RHDR_PRIME]
    mulsd xmm0, xmm4
    movsd [rsi + RHDR_PRIME], xmm0

    ; activation *= ACTIVATION_DECAY
    movsd xmm0, [rsi + RHDR_ACTIVATION]
    mulsd xmm0, xmm5
    movsd [rsi + RHDR_ACTIVATION], xmm0

.decay_next:
    pop rcx
    inc ecx
    jmp .decay_loop

.decay_done:
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; record_fire(region_ptr)
;; rdi = region header ptr that just fired
;; Writes (ptr, timestamp_f64) to ST_FIRE_RING
;; ============================================================
global record_fire
record_fire:
    push rbx
    mov rbx, SURFACE_BASE

    ; Get fire ring position
    lea rax, [rbx + STATE_OFFSET + ST_FIRE_POS]
    mov ecx, [rax]

    ; Calculate ring entry address
    lea rdx, [rbx + STATE_OFFSET + ST_FIRE_RING]
    imul r8d, ecx, ST_FIRE_RING_ENTRY  ; 16 bytes per entry
    add rdx, r8

    ; Store region ptr
    mov [rdx], rdi

    ; Store timestamp as f64 (convert global_step to f64)
    mov rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    cvtsi2sd xmm0, rax
    movsd [rdx + 8], xmm0

    ; Update fire_recency on the region
    movsd [rdi + RHDR_FIRE_RECENCY], xmm0

    ; Advance ring position
    lea rax, [rbx + STATE_OFFSET + ST_FIRE_POS]
    inc ecx
    cmp ecx, ST_FIRE_RING_CAP
    jl .fire_no_wrap
    xor ecx, ecx
.fire_no_wrap:
    mov [rax], ecx

    ; Increment total fire count
    lea rax, [rbx + STATE_OFFSET + ST_FIRE_COUNT]
    inc qword [rax]

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
    ; Reset consecutive errors on hit (region is healthy)
    mov word [rax + RTE_CONSEC_ERRORS], 0
    jmp .done
.next:
    inc edx
    jmp .loop
.done:
    pop rbx
    ret

;; ============================================================
;; update_region_table_misses(header_ptr)
;; Increments consecutive error count; condemns at 9 errors (broken region death)
;; ============================================================
update_region_table_misses:
    push rbx
    push r12
    mov rbx, SURFACE_BASE
    mov r12, rdi                    ; save header ptr
    lea rsi, [rbx + REGION_TABLE_OFFSET]
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov ecx, [rax]
    xor edx, edx
.loop:
    cmp edx, ecx
    jge .done
    imul rax, rdx, RTE_SIZE
    add rax, rsi
    cmp [rax + RTE_ADDR], r12
    jne .next
    ; Found — copy misses from header
    mov ebx, [r12 + RHDR_MISSES]
    mov [rax + RTE_MISSES], ebx
    ; Increment consecutive errors (9-error kill rule)
    movzx ebx, word [rax + RTE_CONSEC_ERRORS]
    inc ebx
    mov [rax + RTE_CONSEC_ERRORS], bx
    ; Check if region should die (9 consecutive errors = broken)
    cmp ebx, CONSEC_ERROR_KILL
    jl .done
    ; 9-error death: condemn immediately (code is genuinely broken)
    movzx ebx, word [rax + RTE_FLAGS]
    or ebx, RFLAG_CONDEMNED
    mov [rax + RTE_FLAGS], bx
    ; Also mark header
    movzx ebx, word [r12 + RHDR_FLAGS]
    or ebx, RFLAG_CONDEMNED
    mov [r12 + RHDR_FLAGS], bx
    jmp .done
.next:
    inc edx
    jmp .loop
.done:
    pop r12
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

;; ============================================================
;; compute_struct_ctx()
;; Computes structural context: Σ bind(ROLE_POS_i, token_vec[history[i]])
;; Uses last 8 tokens from token ring buffer.
;; Result stored in ST_STRUCT_CTX_VEC.
;;
;; This is Phase 2 of structural learning:
;; - Flat context (ST_CTX_HASH): Rolling hash, loses position info
;; - Structural context (ST_STRUCT_CTX_VEC): Preserves position via role binding
;;
;; Uses SCRATCH_OFFSET for temp vectors to avoid stack overflow.
;; ============================================================
global compute_struct_ctx
compute_struct_ctx:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8                            ; align stack

    mov rbx, SURFACE_BASE

    ; Use scratch area for temp vectors (safer than stack)
    ; temp_role = SCRATCH_OFFSET + 0
    ; temp_bound = SCRATCH_OFFSET + HOLO_VEC_BYTES
    lea r14, [rbx + SCRATCH_OFFSET]                   ; temp_role ptr
    lea r15, [rbx + SCRATCH_OFFSET + HOLO_VEC_BYTES]  ; temp_bound ptr

    ; --- Zero the structural context vector ---
    lea rdi, [rbx + STATE_OFFSET + ST_STRUCT_CTX_VEC]
    mov ecx, HOLO_VEC_BYTES / 8
    xor eax, eax
.zero_loop:
    mov [rdi], rax
    add rdi, 8
    dec ecx
    jnz .zero_loop

    ; --- Get token count and ring position ---
    mov eax, [rbx + STATE_OFFSET + ST_TOKEN_COUNT]
    test eax, eax
    jz .ctx_done                          ; no tokens yet

    ; Compute how many tokens to use: min(8, token_count)
    cmp eax, 8
    jle .use_count
    mov eax, 8
.use_count:
    mov r12d, eax                         ; r12 = num_tokens to process

    ; r13 = current position index (0 = most recent)
    xor r13d, r13d

.bind_loop:
    cmp r13d, r12d
    jge .ctx_normalize

    ; --- Generate f64 role vector for position r13 ---
    ; Use (position | 0x524F4C00) as seed for unique role vectors
    mov edi, r13d
    or edi, 0x524F4C00                    ; "ROL\0" + position
    mov rsi, r14                          ; output to temp_role (f64[1024])
    push r12
    push r13
    call holo_gen_vec
    pop r13
    pop r12

    ; --- Get token from history ---
    ; ring_idx = (pos - 1 - r13) & (CAP - 1)
    mov eax, [rbx + STATE_OFFSET + ST_TOKEN_POS]
    sub eax, 1
    sub eax, r13d
    and eax, (ST_TOKEN_BUF_CAP - 1)

    ; token_id = token_buf[ring_idx]
    lea rcx, [rbx + STATE_OFFSET + ST_TOKEN_BUF]
    mov r10d, [rcx + rax * 4]             ; token_id (use r10, rcx is clobbered)

    ; --- Generate f64 token vector ---
    ; Use SCRATCH_OFFSET + 2*HOLO_VEC_BYTES for temp_token
    mov edi, r10d                         ; token_id as seed
    lea rsi, [rbx + SCRATCH_OFFSET + HOLO_VEC_BYTES * 2]  ; temp_token (f64[1024])
    push r12
    push r13
    call holo_gen_vec
    pop r13
    pop r12

    ; --- Bind role with token: temp_bound = bind(temp_role, temp_token) ---
    mov rdi, r14                          ; temp_role (f64)
    lea rsi, [rbx + SCRATCH_OFFSET + HOLO_VEC_BYTES * 2]  ; temp_token (f64)
    mov rdx, r15                          ; output to temp_bound (f64)
    push r12
    push r13
    call holo_bind_f64
    pop r13
    pop r12

    ; --- Superpose into structural context (f64) ---
    ; struct_ctx += temp_bound
    lea rdi, [rbx + STATE_OFFSET + ST_STRUCT_CTX_VEC]
    mov rsi, r15                          ; temp_bound (f64)
    push r12
    push r13
    call holo_superpose_f64
    pop r13
    pop r12

    inc r13d
    jmp .bind_loop

.ctx_normalize:
    ; Optional: normalize the structural context
    ; For now, skip normalization to preserve magnitude information

    ; Mark as valid
    mov dword [rbx + STATE_OFFSET + ST_STRUCT_CTX_VALID], 1
    mov rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov [rbx + STATE_OFFSET + ST_STRUCT_CTX_STEP], rax

.ctx_done:
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; PHASE 3: SCHEMA DISPATCH FUNCTIONS
;; Structural pattern matching with variable extraction
;; ============================================================

section .data
    align 8
    ; NOTE: Schema matching threshold is low (0.01) because current implementation
    ; stores full struct_ctx but doesn't mask variable positions during comparison.
    ; Full variable masking would allow higher thresholds.
    schema_match_thresh: dq 0.01

section .text

;; ============================================================
;; schema_match(schema_ptr) → xmm0 (similarity score)
;; rdi = schema entry pointer
;; Returns cosine similarity between schema template and ST_STRUCT_CTX_VEC
;; ============================================================
global schema_match
schema_match:
    push rbx
    push r12
    sub rsp, 8

    mov r12, rdi                          ; schema_ptr
    mov rbx, SURFACE_BASE

    ; Check if structural context is valid
    cmp dword [rbx + STATE_OFFSET + ST_STRUCT_CTX_VALID], 0
    je .no_match

    ; Compute cosine similarity: cos(template, struct_ctx)
    mov rdi, r12                          ; template is at start of schema entry
    lea rsi, [rbx + STATE_OFFSET + ST_STRUCT_CTX_VEC]
    call holo_cosim_f64
    ; xmm0 = similarity

    jmp .match_done

.no_match:
    xorpd xmm0, xmm0                      ; return 0.0

.match_done:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; schema_extract_var(role_idx, out_ptr)
;; edi = role position index (0-7)
;; rsi = output vector pointer
;; Extracts filler from structural context by unbinding with role vector.
;; filler ≈ unbind(struct_ctx, ROLE_POS_idx)
;; ============================================================
global schema_extract_var
schema_extract_var:
    push rbx
    push r12
    push r13
    sub rsp, 8

    mov r12d, edi                         ; role_idx
    mov r13, rsi                          ; out_ptr
    mov rbx, SURFACE_BASE

    ; Generate f64 role vector in scratch area
    mov edi, r12d
    or edi, 0x524F4C00                    ; "ROL\0" + position (same as compute_struct_ctx)
    lea rsi, [rbx + SCRATCH_OFFSET + HOLO_VEC_BYTES * 2]  ; temp for role
    call holo_gen_vec

    ; Unbind: filler = unbind(struct_ctx, role) using f64
    lea rdi, [rbx + STATE_OFFSET + ST_STRUCT_CTX_VEC]
    lea rsi, [rbx + SCRATCH_OFFSET + HOLO_VEC_BYTES * 2]  ; role vector
    mov rdx, r13                          ; output
    call holo_unbind_f64

    add rsp, 8
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; schema_query_filler(filler_ptr) → eax (best matching token)
;; rdi = filler vector pointer
;; Finds the token whose vector is most similar to the filler.
;; Returns token_id of best match, 0 if no good match.
;; ============================================================
global schema_query_filler
schema_query_filler:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8

    mov r12, rdi                          ; filler_ptr
    mov rbx, SURFACE_BASE
    xor r13d, r13d                        ; best_token = 0
    mov rax, 0xBFF0000000000000           ; -1.0 (any positive beats this)
    movq xmm7, rax                        ; best_sim in xmm7

    ; Scan first 256 token vectors for best match
    xor r14d, r14d                        ; token_idx

.scan_loop:
    cmp r14d, 256
    jge .scan_done

    ; Generate f64 token vector in scratch area
    mov edi, r14d                         ; token_id as seed
    lea rsi, [rbx + SCRATCH_OFFSET + HOLO_VEC_BYTES * 4]  ; temp for token vec
    push r12
    push r13
    push r14
    call holo_gen_vec
    pop r14
    pop r13
    pop r12

    ; Compute similarity (f64)
    mov rdi, r12                          ; filler (f64)
    lea rsi, [rbx + SCRATCH_OFFSET + HOLO_VEC_BYTES * 4]  ; token_vec (f64)
    push r12
    push r13
    push r14
    call holo_cosim_f64
    pop r14
    pop r13
    pop r12
    ; xmm0 = similarity

    ; Check if better than best
    ucomisd xmm0, xmm7
    jbe .next_token

    ; New best
    movapd xmm7, xmm0
    mov r13d, r14d                        ; best_token = current

.next_token:
    inc r14d
    jmp .scan_loop

.scan_done:
    ; Check if best_sim > 0.1 (lowered for noisy unbinding)
    mov rax, 0x3FB999999999999A           ; 0.1
    movq xmm0, rax
    ucomisd xmm7, xmm0
    jbe .no_good_match

    mov eax, r13d                         ; return best_token
    jmp .query_done

.no_good_match:
    xor eax, eax                          ; return 0

.query_done:
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; schema_dispatch() → eax (predicted token, 0=none)
;; Tries to match structural context against all active schemas.
;; If match found, extracts variables and makes prediction.
;; ============================================================
global schema_dispatch
schema_dispatch:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 24                           ; locals

    mov rbx, SURFACE_BASE

    ; Check if structural context is valid
    cmp dword [rbx + STATE_OFFSET + ST_STRUCT_CTX_VALID], 0
    je .no_schema_match

    ; Get schema count
    mov eax, [rbx + STATE_OFFSET + ST_SCHEMA_COUNT]
    test eax, eax
    jz .no_schema_match

    mov r12d, eax                         ; num_schemas
    xor r13d, r13d                        ; schema_idx
    xor r14d, r14d                        ; best_token = 0
    mov rax, 0xBFF0000000000000           ; -1.0
    mov [rsp], rax                        ; best_score

    ; Schema table base
    lea r15, [rbx + SCHEMA_TABLE_OFFSET]

.schema_loop:
    cmp r13d, r12d
    jge .schema_done

    ; Compute schema entry address
    mov eax, r13d
    imul eax, SCHEMA_ENTRY_SIZE
    lea rdi, [r15 + rax]                  ; schema_ptr

    ; Check if schema is active
    movzx eax, byte [rdi + SCHE_FLAGS]
    test al, SCHEF_ACTIVE
    jz .next_schema

    ; Save schema_ptr
    mov [rsp + 8], rdi

    ; Match schema
    push r12
    push r13
    push r14
    push r15
    call schema_match
    pop r15
    pop r14
    pop r13
    pop r12
    ; xmm0 = match score

    ; Check against threshold
    ucomisd xmm0, [rel schema_match_thresh]
    jbe .next_schema

    ; Check if better than current best
    ucomisd xmm0, [rsp]
    jbe .next_schema

    ; New best match - extract variable and predict
    movsd [rsp], xmm0                     ; update best_score

    ; Get predict_role from schema
    mov rdi, [rsp + 8]                    ; schema_ptr
    movzx edi, byte [rdi + SCHE_PREDICT_ROLE]
    and edi, 7                            ; clamp to 0-7

    ; Output to scratch area
    lea rsi, [rbx + SCRATCH_OFFSET + HOLO_VEC_BYTES * 3]
    push r12
    push r13
    push r14
    push r15
    call schema_extract_var
    pop r15
    pop r14
    pop r13
    pop r12

    ; Query extracted filler to find predicted token
    lea rdi, [rbx + SCRATCH_OFFSET + HOLO_VEC_BYTES * 3]
    push r12
    push r13
    push r14
    push r15
    call schema_query_filler
    pop r15
    pop r14
    pop r13
    pop r12
    ; eax = predicted token

    test eax, eax
    jz .next_schema

    mov r14d, eax                         ; best_token

    ; Update schema stats
    mov rdi, [rsp + 8]                    ; schema_ptr
    inc dword [rdi + SCHE_HITS]

    ; Track match
    inc dword [rbx + STATE_OFFSET + ST_SCHEMA_MATCHES]

.next_schema:
    inc r13d
    jmp .schema_loop

.schema_done:
    mov eax, r14d                         ; return best_token
    test eax, eax
    jz .no_schema_match

    ; Increment schema extraction counter
    inc dword [rbx + STATE_OFFSET + ST_SCHEMA_EXTRACTS]

    jmp .schema_exit

.no_schema_match:
    xor eax, eax

.schema_exit:
    add rsp, 24
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; schema_create(template_ptr, var_mask, predict_role) → rax (schema_ptr)
;; rdi = template vector pointer
;; esi = variable mask (bits for variable positions)
;; edx = predict role (which position to predict)
;; Creates a new schema entry. Returns pointer or 0 if full.
;; ============================================================
global schema_create
schema_create:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 8

    mov r12, rdi                          ; template_ptr
    mov r13d, esi                         ; var_mask
    mov r14d, edx                         ; predict_role
    mov rbx, SURFACE_BASE

    ; Check if we have room
    mov eax, [rbx + STATE_OFFSET + ST_SCHEMA_COUNT]
    cmp eax, SCHEMA_MAX
    jge .schema_full

    ; Compute new schema entry address
    mov ecx, eax                          ; schema_idx
    imul ecx, SCHEMA_ENTRY_SIZE
    lea rax, [rbx + SCHEMA_TABLE_OFFSET + rcx]
    push rax                              ; save schema_ptr

    ; Copy template vector
    mov rdi, rax                          ; dest = schema entry start
    mov rsi, r12                          ; src = template
    mov ecx, HOLO_VEC_BYTES / 8
.copy_template:
    mov rdx, [rsi]
    mov [rdi], rdx
    add rsi, 8
    add rdi, 8
    dec ecx
    jnz .copy_template

    ; Fill metadata
    pop rax                               ; restore schema_ptr
    mov byte [rax + SCHE_VAR_MASK], r13b
    mov byte [rax + SCHE_PREDICT_ROLE], r14b
    mov byte [rax + SCHE_CONDITION_ROLE], 0
    mov byte [rax + SCHE_FLAGS], SCHEF_ACTIVE | SCHEF_LEARNED
    mov dword [rax + SCHE_MATCH_THRESH], 0x3F19999A  ; 0.6 f32
    mov dword [rax + SCHE_PRED_TOKEN], 0
    mov dword [rax + SCHE_HITS], 0
    mov dword [rax + SCHE_MISSES], 0
    mov ecx, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov [rax + SCHE_BIRTH], ecx
    mov [rax + SCHE_LAST_FIRE], ecx

    ; Increment count
    inc dword [rbx + STATE_OFFSET + ST_SCHEMA_COUNT]

    jmp .create_done

.schema_full:
    xor eax, eax                          ; return 0

.create_done:
    add rsp, 8
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; schema_learn_from_context()
;; Creates a schema from the current structural context.
;; Called when a pattern is worth generalizing.
;; Uses current struct_ctx as template, position 0 as condition,
;; position 2 as variable to predict.
;; ============================================================
global schema_learn_from_context
schema_learn_from_context:
    push rbx
    sub rsp, 8

    mov rbx, SURFACE_BASE

    ; Check if structural context is valid
    cmp dword [rbx + STATE_OFFSET + ST_STRUCT_CTX_VALID], 0
    je .learn_done

    ; Use current structural context as template
    lea rdi, [rbx + STATE_OFFSET + ST_STRUCT_CTX_VEC]
    mov esi, 0x04                         ; var_mask = bit 2 (ROLE_POS_2 is variable)
    mov edx, 2                            ; predict_role = 2
    call schema_create

.learn_done:
    add rsp, 8
    pop rbx
    ret
