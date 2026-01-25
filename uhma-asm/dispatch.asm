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
    ; Set at 0.5 to allow holographic predictions while filtering noise
    align 8
    holo_thresh:    dq 0.5

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
extern learn_connections
extern observe_cycle
extern holo_predict
extern holo_decay_all
extern update_organic_pressure
extern update_anticipatory
extern decay_anticipatory
extern update_oscillation
extern update_presence_dispatch
extern introspect_scan_regions

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
    ; Check if word is all digits (len > 1) → abstract to class token
    pop rcx
    cmp ecx, 2
    jl .no_abstract
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

    ; --- Context hash = immediate predecessor only (true bigram) ---
    ; Deterministic: prev_token determines context for predicting current
    ; This ensures "hello → world" matches every time after learning once
    lea rsi, [rbx + STATE_OFFSET + ST_TOKEN_BUF]
    mov ecx, [rbx + STATE_OFFSET + ST_TOKEN_POS]
    mov r9, 0x9E3779B97F4A7C15  ; golden ratio prime

    ; Get previous token (position - 1)
    dec ecx
    and ecx, (ST_TOKEN_BUF_CAP - 1)
    mov r10d, [rsi + rcx * 4]  ; previous token

    ; Context = just prev_token * prime (simple, deterministic)
    mov rax, r10
    imul rax, r9
    mov r13, rax
    ; Store context
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

    ; Increment hit counter on the predicting region
    lea rax, [rbx + STATE_OFFSET + ST_PREDICT_REGION]
    mov rcx, [rax]
    test rcx, rcx
    jz .holo_hit              ; holo prediction — still count as hit
    inc dword [rcx + RHDR_HITS]

    ; STDP connection learning — strengthen temporal links
    push rcx
    mov rdi, rcx              ; firing region ptr
    call learn_connections
    pop rcx

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
    call learn_pattern

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
    call learn_pattern

.after_counter:
    ; --- Organic dynamics: let internal pressure drive actions ---
    call update_organic_pressure

    ; --- Make next prediction ---
    ; Dispatch: predict what comes AFTER current token
    ; Context for prediction = hash(current_token) so we can find patterns that predict successors
    mov rax, r12              ; current token
    imul rax, 0x9E3779B97F4A7C15  ; golden ratio prime (same hash as context computation)
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

    ; --- TRY HOLOGRAPHIC PREDICTION FIRST ---
    mov edi, r12d             ; ctx_hash
    call holo_predict
    ; eax = best_token, xmm0 = confidence (f64)
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

    ; Check context match (same bytecode inspection as before)
    cmp byte [r13 + RHDR_SIZE], 0x3D
    jne .graph_follow_next

    mov eax, [r13 + RHDR_SIZE + 1]   ; stored context (imm32)

    ; Schema handling
    test eax, 0x0F
    jnz .graph_exact_cmp
    ; Schema pattern — mask incoming context
    mov edi, r12d
    and edi, 0xFFFFFFF0
    cmp eax, edi
    jne .graph_no_match
    mov dword [rbx + STATE_OFFSET + ST_EXPECT_IS_SCHEMA], 1
    jmp .graph_ctx_matched
.graph_exact_cmp:
    cmp eax, r12d
    jne .graph_no_match
.graph_ctx_matched:

    ; --- MATCH: compute effective_weight ---
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

    ; Store new best
    mov eax, [r13 + RHDR_SIZE + 8]    ; predicted token (B8 imm32)
    mov [rsp + 0], eax                ; best_token
    movsd [rsp + 8], xmm0             ; best_weight
    mov [rsp + 16], r13               ; best_region_ptr
    jmp .graph_not_best

.graph_check_runner_up:
    ; Not best, but better than runner-up?
    ucomisd xmm0, [rsp + 48]
    jbe .graph_not_best
    ; New runner-up (counterfactual alternative)
    mov eax, [r13 + RHDR_SIZE + 8]
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
    cmp eax, RTYPE_DISPATCH
    jne .fallback_skip
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

    ; Repair routing: if we came from a dead-end graph, wire it
    ; (The last graph node should get next_a → this region)

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
    cmp eax, RTYPE_DISPATCH
    jne .linear_skip
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
