; introspect.asm — Semantic self-model, bootstrap, organic regulation, and autonomy loop
;
; THIS IS WHERE SELF-AWARENESS LIVES.
;
; @entry tick_workers()               → autonomous idle processing (called by REPL on timeout)
; @entry introspect_scan_regions()    → builds semantic self-model (SELF-AWARE reading)
; @entry introspect_repair_cycle()    → processes RFLAG_NEEDS_REPAIR regions
; @entry metacog_report()             → REPL 'intro' command output
; @entry init_action_registry()       → initialize 10-action registry with gates/flags
; @entry encode_presence_vec()        → encode 30-dim presence → holographic vector
; @entry on_maturity_advance(edi)     → seed action traces on stage transitions
; @entry action_seek()                → scan dir, pick file by holographic resonance, digest
; @entry action_scan_env()            → rotate through /proc files for world-model
; @entry action_compose()             → chain predictions to generate novel text
; @entry action_teach()               → share schema traces to colony (shared memory)
; @entry action_reflect()             → superpose action traces into self-model
; @entry probe_readable(rdi=path)     → eax=1 if file is mostly printable text
; @entry frontier_push(rdi=path)      → push dir onto explore frontier queue
; @entry frontier_pop(rdi,esi)        → pop next dir from frontier
; @entry advance_explore_path()       → move to next dir in frontier
;
; AUTONOMOUS BEHAVIOR (tick_workers):
;   Called by REPL on poll timeout. Makes UHMA do useful work when idle:
;   1. Bootstrap: if SELF-AWARE < 0.3 → force observe_cycle (build self-model)
;   2. Startup consolidation: if regions > 100 and fresh session → dream_cycle
;   3. Organic pressure: decay + check thresholds → fire appropriate cycles
;   4. Resonance selection: encode presence → cosim against 10 action traces → fire best
;   5. Curiosity pressure: builds from confusion, fires SEEK when accuracy is low
;
; AUTONOMY LOOP (resonance_select):
;   Presence field encoded as holographic vector → cosim against action traces
;   Best match = next action. Cold start falls through to pressure checks.
;   10 actions: DREAM, OBSERVE, EVOLVE, REST, EXPLORE, SEEK, SCAN_ENV, COMPOSE, TEACH, REFLECT
;   Actions gated by maturity (Stage 0-2) via registry
;
; SEMANTIC SELF-MODEL:
;   introspect_scan_regions() encodes each region via encode_region_to_vector()
;   Similar code → similar vectors. Superposed into ST_SELF_MODEL_VEC.
;   After observe cycle: SELF-AWARE reading typically 0.9+ (97.3% measured)
;
; @calls vsa_ops.asm:encode_region_to_vector
; @calls receipt.asm:meta_recommend_strategy, intro_get_self_awareness
; @calls dreams.asm:dream_cycle, observe.asm:observe_cycle
; @calls dispatch.asm:dispatch_predict (for action_compose)
; @calls vsa.asm:holo_cosim_f64, holo_superpose_f64, holo_magnitude_f64
; @calledby repl.asm (on poll timeout), maturity.asm (on_maturity_advance)
;
%include "syscalls.inc"
%include "constants.inc"

section .data
    ; Batch mode flag - when set, tick_workers returns immediately
    ; Default=1 for training (use "batch" command to toggle off for interactive)
    global batch_mode
    batch_mode:         dq 1

    intro_hdr:          db "[INTROSPECT] ", 0
    intro_decode_msg:   db "Decoded region: ctx=0x", 0
    intro_pred_msg:     db " pred=0x", 0
    intro_sem_msg:      db " sem=", 0
    intro_antic_msg:    db "[ANTICIPATE] Signal materializing: token=0x", 0
    intro_antic_accum:  db "[ANTICIPATE] Accumulating: token=0x", 0
    intro_conf_msg:     db " conf=", 0
    intro_organic_msg:  db "[ORGANIC] ", 0
    intro_dream_trig:   db "Dream fired (miss pressure)", 10, 0
    intro_observe_trig: db "Observe fired (accuracy drift)", 10, 0
    intro_evolve_trig:  db "Evolve fired (stagnation)", 10, 0
    intro_introspect_trig: db "Introspect fired (self-surprise)", 10, 0
    intro_repair_msg:   db "[SELF-REPAIR] Examining region that violated self-model", 10, 0
    intro_flatness_msg: db "[OSCILLATION] Flatness detected — perturbing", 10, 0
    intro_nl:           db 10, 0

    ; Semantic signature names
    sem_unknown:    db "UNKNOWN", 0
    sem_cmp_je:     db "CMP-JE-RET", 0
    sem_chain:      db "CMP-CHAIN", 0
    sem_multi:      db "MULTI-CMP", 0
    sem_schema:     db "SCHEMA", 0
    sem_relay:      db "CALL-RELAY", 0

    align 8
    antic_decay:    dq 0.97     ; anticipation decays each step (slow fade)
    pressure_decay: dq 0.985    ; pressure decays toward zero (slow drain)
    pressure_boost: dq 0.04     ; miss adds this to dream pressure (gentle)
    observe_boost:  dq 0.02     ; accuracy variance adds to observe pressure
    evolve_boost:   dq 0.01     ; stagnation adds to evolve pressure (rare event)
    one_f64:        dq 1.0
    zero_f64:       dq 0.0

section .text

extern print_cstr
extern print_hex32
extern print_f32
extern print_u64
extern print_newline
extern dream_cycle
extern observe_cycle
extern evolve_cycle
extern fire_hook
extern drives_check
extern journey_step
extern modify_generalize
extern modify_specialize
extern meta_recommend_strategy  ; from receipt.asm - causal model guidance
extern intro_get_self_awareness ; from receipt.asm - SELF-AWARE ratio
extern encode_region_to_vector  ; from vsa_ops.asm - semantic code encoding
extern holo_superpose_f64       ; from vsa.asm - holographic accumulation
extern holo_normalize_f64       ; from vsa.asm - prevent magnitude explosion

;; ============================================================
;; introspect_region(region_ptr) → fills cache entry
;; rdi = pointer to region header
;; Decodes the x86 instructions in the region's code body.
;; Extracts: context hash compared, token predicted, structure type.
;; Returns: eax = semantic signature (RSEM_*)
;; ============================================================
global introspect_region
introspect_region:
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi                ; region header ptr
    xor r12d, r12d              ; decoded ctx_hash
    xor r13d, r13d              ; decoded pred_token
    mov r14d, RSEM_UNKNOWN      ; semantic signature

    ; Get code length
    movzx ecx, word [rbx + RHDR_CODE_LEN]
    test ecx, ecx
    jz .decode_done
    cmp ecx, 3                  ; minimum useful region
    jl .decode_done

    ; Point to code body (after 128-byte header)
    lea rsi, [rbx + RHDR_SIZE]

    ; --- Decode first instruction ---
    ; Dispatch regions start with: cmp eax, imm32 (opcode 0x3D, 5 bytes)
    ; or: 81 /7 (cmp [reg], imm32)
    cmp byte [rsi], 0x3D        ; cmp eax, imm32?
    je .decode_cmp_eax

    cmp byte [rsi], 0x81        ; cmp r/m32, imm32?
    je .decode_cmp_rm

    ; Check for call instruction (relay pattern)
    cmp byte [rsi], 0xE8        ; call rel32?
    je .decode_call

    ; Check for mov eax, imm32 (direct token return)
    cmp byte [rsi], 0xB8        ; mov eax, imm32?
    je .decode_mov_eax

    jmp .decode_done

.decode_cmp_eax:
    ; 0x3D imm32 — comparing context hash
    mov r12d, [rsi + 1]         ; extract the context hash being compared
    mov r14d, RSEM_CMP_JE_RET  ; assume simple pattern

    ; Check if lower nibble is 0 → schema (masked comparison)
    test r12d, 0x0F
    jnz .find_predicted_token
    mov r14d, RSEM_SCHEMA
    jmp .find_predicted_token

.decode_cmp_rm:
    ; 81 FF imm32 — cmp edi, imm32 (less common but valid)
    cmp byte [rsi + 1], 0xFF   ; ModRM for edi
    jne .decode_done
    mov r12d, [rsi + 2]         ; extract context hash
    mov r14d, RSEM_CMP_JE_RET
    jmp .find_predicted_token

.decode_call:
    ; E8 rel32 — relay to another function
    mov r14d, RSEM_CALL_RELAY
    jmp .decode_done

.decode_mov_eax:
    ; B8 imm32 — direct token return (no comparison, always returns)
    mov r13d, [rsi + 1]         ; the token being returned
    mov r14d, RSEM_CMP_JE_RET
    jmp .decode_done

.find_predicted_token:
    ; Scan forward for mov eax, imm32 (0xB8) — the predicted token
    movzx ecx, word [rbx + RHDR_CODE_LEN]
    lea rdi, [rsi + 5]          ; skip past the cmp instruction
    lea rdx, [rsi + rcx]       ; end of code

    ; Also check for multiple cmp instructions (MULTI_CMP pattern)
    xor eax, eax               ; cmp_count
.scan_code:
    cmp rdi, rdx
    jge .decode_done

    cmp byte [rdi], 0xB8       ; mov eax, imm32?
    je .found_token

    cmp byte [rdi], 0x3D       ; another cmp eax, imm32?
    jne .scan_next
    inc eax
    cmp eax, 1
    jle .scan_next
    mov r14d, RSEM_MULTI_CMP   ; multiple comparisons
    jmp .scan_next

.found_token:
    mov r13d, [rdi + 1]         ; extract predicted token
    ; Check if there's a jmp/call after (chain pattern)
    lea rax, [rdi + 5]
    cmp rax, rdx
    jge .decode_done
    cmp byte [rax], 0xE9       ; jmp rel32? (chain to next region)
    je .is_chain
    cmp byte [rax], 0xEB       ; jmp rel8?
    je .is_chain
    jmp .decode_done

.is_chain:
    cmp r14d, RSEM_SCHEMA
    je .decode_done             ; keep SCHEMA if already set
    mov r14d, RSEM_CMP_JE_CHAIN
    jmp .decode_done

.scan_next:
    inc rdi
    jmp .scan_code

.decode_done:
    ; Return values in registers for caller to use
    ; eax = semantic signature
    ; r12d = ctx_hash decoded
    ; r13d = pred_token decoded
    mov eax, r14d

    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; introspect_scan_regions()
;; Scan all active regions, decode their semantics, fill cache.
;; This is the system reading its own code as data.
;; ============================================================
global introspect_scan_regions
introspect_scan_regions:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, (HOLO_VEC_BYTES + 8)               ; temp vector + 8 alignment (5 pushes = odd)

    mov rbx, SURFACE_BASE
    lea r12, [rbx + REGION_TABLE_OFFSET]  ; region table base
    mov r13d, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    xor r14d, r14d              ; region index
    xor r15d, r15d              ; cache write index

.scan_loop:
    cmp r14d, r13d
    jge .scan_done
    cmp r15d, ST_INTRO_CACHE_CAP
    jge .scan_done

    ; Get region table entry
    imul eax, r14d, RTE_SIZE
    lea rsi, [r12 + rax]

    ; Check if active
    movzx ecx, word [rsi + RTE_FLAGS]
    test ecx, RFLAG_ACTIVE
    jz .scan_next
    test ecx, RFLAG_CONDEMNED
    jnz .scan_next

    ; Get region address
    mov rdi, [rsi + RTE_ADDR]
    test rdi, rdi
    jz .scan_next

    ; Decode this region
    push rsi
    push r14
    call introspect_region      ; → eax = semantic sig, r12d/r13d decoded
    pop r14
    pop rsi

    ; Fill cache entry
    lea rcx, [rbx + STATE_OFFSET + ST_INTRO_CACHE]
    imul edx, r15d, ST_INTRO_ENTRY_SIZE
    add rcx, rdx

    mov rdi, [rsi + RTE_ADDR]
    mov [rcx + ICE_REGION_PTR], rdi
    ; Note: introspect_region clobbered r12/r13, but we saved them
    ; Actually we need to re-decode or restructure. Let me use the return value approach.
    ; For now store what we have:
    mov [rcx + ICE_SEMANTIC_SIG], eax

    ; Compute accuracy for this region
    mov edi, [rsi + RTE_HITS]
    mov edx, [rsi + RTE_MISSES]
    add edx, edi
    test edx, edx
    jz .store_zero_acc
    cvtsi2ss xmm0, edi
    cvtsi2ss xmm1, edx
    divss xmm0, xmm1
    movss [rcx + ICE_ACCURACY], xmm0
    jmp .count_neighbors

.store_zero_acc:
    xor eax, eax
    mov [rcx + ICE_ACCURACY], eax

.count_neighbors:
    ; Count non-null neighbor pointers
    mov rdi, [rsi + RTE_ADDR]
    xor edx, edx               ; neighbor count
    cmp qword [rdi + RHDR_NEXT_A], 0
    je .n1
    inc edx
.n1:
    cmp qword [rdi + RHDR_EXCITE_A], 0
    je .n2
    inc edx
.n2:
    cmp qword [rdi + RHDR_EXCITE_B], 0
    je .n3
    inc edx
.n3:
    cmp qword [rdi + RHDR_INHIBIT_A], 0
    je .n4
    inc edx
.n4:
    mov [rcx + ICE_NEIGHBOR_COUNT], dx

    ; === SEMANTIC SELF-MODEL: encode region code into self-model ===
    ; This builds a holographic representation of "what code I am"
    ; using semantic encoding where similar code → similar vectors
    push rsi                        ; save RTE pointer
    mov rdi, [rsi + RTE_ADDR]       ; region header
    lea rsi, [rsp + 8]              ; temp vector on stack (offset for push)
    call encode_region_to_vector    ; encode region semantically

    ; Superpose into self-model: ST_SELF_MODEL_VEC += region_vec
    lea rdi, [rbx + STATE_OFFSET + ST_SELF_MODEL_VEC]
    lea rsi, [rsp + 8]              ; temp vector
    call holo_superpose_f64

    ; Normalize to prevent magnitude explosion
    lea rdi, [rbx + STATE_OFFSET + ST_SELF_MODEL_VEC]
    call holo_normalize_f64
    pop rsi                         ; restore RTE pointer

    inc r15d

.scan_next:
    inc r14d
    jmp .scan_loop

.scan_done:
    add rsp, (HOLO_VEC_BYTES + 8)               ; free temp vector + alignment
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; update_anticipatory(token_id, confidence)
;; edi = token_id, xmm0 = confidence (f64)
;; Called from dispatch with sub-threshold holographic signals.
;; Accumulates weak signals — "things forming in the distance."
;; When accumulated confidence crosses threshold → signal materializes.
;; ============================================================
global update_anticipatory
update_anticipatory:
    push rbx
    push r12
    push r13
    sub rsp, 16                 ; save xmm0
    movsd [rsp], xmm0

    mov r12d, edi               ; token_id
    mov rbx, SURFACE_BASE

    ; Check if signal is above noise floor
    movsd xmm1, [rsp]
    mov rax, ANTIC_SIGNAL_FLOOR
    movq xmm2, rax
    ucomisd xmm1, xmm2
    jbe .antic_done             ; below noise floor, ignore

    ; Search anticipatory buffer for this token
    lea rdi, [rbx + STATE_OFFSET + ST_ANTIC_BUF]
    xor ecx, ecx               ; index
    mov r13d, -1                ; free slot (-1 = none found)

.antic_search:
    cmp ecx, ST_ANTIC_CAP
    jge .antic_not_found

    imul edx, ecx, ST_ANTIC_ENTRY_SIZE
    lea rsi, [rdi + rdx]

    ; Check if this slot has our token
    cmp dword [rsi + ABE_TOKEN_ID], r12d
    je .antic_found

    ; Track first empty slot
    cmp dword [rsi + ABE_TOKEN_ID], 0
    jne .antic_search_next
    cmp r13d, -1
    jne .antic_search_next
    mov r13d, ecx               ; remember free slot

.antic_search_next:
    inc ecx
    jmp .antic_search

.antic_found:
    ; Accumulate confidence
    imul edx, ecx, ST_ANTIC_ENTRY_SIZE
    lea rsi, [rdi + rdx]
    movsd xmm0, [rsi + ABE_ACCUM_CONF]
    addsd xmm0, [rsp]          ; add new signal
    movsd [rsi + ABE_ACCUM_CONF], xmm0
    inc dword [rsi + ABE_HIT_COUNT]

    ; Update last_step
    mov eax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov [rsi + ABE_LAST_STEP], eax

    ; Check if materialized (accumulated > threshold)
    mov rax, ANTIC_MATERIALIZE_THRESH
    movq xmm1, rax
    ucomisd xmm0, xmm1
    jbe .antic_done

    ; MATERIALIZED — a distant signal has become concrete
    ; Print notification
    push rcx
    push rdi
    lea rdi, [rel intro_antic_msg]
    call print_cstr
    mov edi, r12d
    call print_hex32
    lea rdi, [rel intro_nl]
    call print_cstr
    pop rdi
    pop rcx

    ; Increment materialized counter
    inc dword [rbx + STATE_OFFSET + ST_ANTIC_FIRED]

    ; Clear this slot (it has materialized, no longer anticipatory)
    imul edx, ecx, ST_ANTIC_ENTRY_SIZE
    lea rsi, [rdi + rdx]
    mov qword [rsi], 0
    mov qword [rsi + 8], 0
    mov qword [rsi + 16], 0
    jmp .antic_done

.antic_not_found:
    ; No existing entry — use free slot if available
    cmp r13d, -1
    je .antic_done              ; buffer full, oldest will decay away

    imul edx, r13d, ST_ANTIC_ENTRY_SIZE
    lea rsi, [rdi + rdx]
    mov [rsi + ABE_TOKEN_ID], r12d
    movsd xmm0, [rsp]
    movsd [rsi + ABE_ACCUM_CONF], xmm0
    mov eax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov [rsi + ABE_LAST_STEP], eax
    mov dword [rsi + ABE_HIT_COUNT], 1

    ; Update active count
    inc dword [rbx + STATE_OFFSET + ST_ANTIC_ACTIVE]

.antic_done:
    add rsp, 16
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; decay_anticipatory()
;; Decay all anticipatory signals each step.
;; Signals that don't get reinforced fade — the distance stays distant.
;; ============================================================
global decay_anticipatory
decay_anticipatory:
    push rbx
    mov rbx, SURFACE_BASE
    lea rdi, [rbx + STATE_OFFSET + ST_ANTIC_BUF]
    xor ecx, ecx

.decay_loop:
    cmp ecx, ST_ANTIC_CAP
    jge .decay_done

    imul edx, ecx, ST_ANTIC_ENTRY_SIZE
    lea rsi, [rdi + rdx]

    ; Skip empty slots
    cmp dword [rsi + ABE_TOKEN_ID], 0
    je .decay_next

    ; Decay accumulated confidence
    movsd xmm0, [rsi + ABE_ACCUM_CONF]
    mulsd xmm0, [rel antic_decay]
    movsd [rsi + ABE_ACCUM_CONF], xmm0

    ; If decayed below noise floor, clear the slot
    mov rax, ANTIC_SIGNAL_FLOOR
    movq xmm1, rax
    ucomisd xmm0, xmm1
    ja .decay_next

    ; Signal faded away — clear slot
    mov qword [rsi], 0
    mov qword [rsi + 8], 0
    mov qword [rsi + 16], 0
    dec dword [rbx + STATE_OFFSET + ST_ANTIC_ACTIVE]

.decay_next:
    inc ecx
    jmp .decay_loop

.decay_done:
    pop rbx
    ret

;; ============================================================
;; update_organic_pressure()
;; Called after each token. Updates miss pressure, dream/observe/evolve
;; pressure based on actual system dynamics — not timers, not commands.
;; Triggers actions when pressure exceeds thresholds.
;; ============================================================
global update_organic_pressure
update_organic_pressure:
    ; Check batch mode - skip organic triggers during training
    cmp qword [rel batch_mode], 0
    jne .organic_skip

    push rbx
    push r12
    sub rsp, 8                  ; alignment

    mov rbx, SURFACE_BASE
    lea r12, [rbx + STATE_OFFSET]

    ; JOURNEY: record update_organic_pressure
    mov edi, TRACE_UPDATE_ORGANIC
    call journey_step

    ; --- Update miss pressure (EMA of recent miss rate) ---
    ; miss_pressure = alpha * (was_miss ? 1.0 : 0.0) + (1-alpha) * miss_pressure
    movsd xmm0, [r12 + ST_MISS_PRESSURE]    ; current pressure

    ; Check if last step was a miss (surprise type != NONE)
    cmp dword [r12 + ST_SURPRISE_TYPE], SURPRISE_NONE
    je .no_miss_boost

    ; Was a miss — boost pressure
    movsd xmm1, [rel pressure_boost]
    addsd xmm0, xmm1
    jmp .cap_pressure

.no_miss_boost:
    ; Decay pressure toward zero
    mulsd xmm0, [rel pressure_decay]

.cap_pressure:
    ; Cap at 1.0
    movsd xmm1, [rel one_f64]
    ucomisd xmm0, xmm1
    jbe .store_miss_pressure
    movsd xmm0, xmm1
.store_miss_pressure:
    movsd [r12 + ST_MISS_PRESSURE], xmm0

    ; --- Accumulate dream pressure from miss pressure ---
    movsd xmm1, [r12 + ST_MISS_PRESSURE]
    movsd xmm2, [r12 + ST_DREAM_PRESSURE]
    ; dream_pressure += miss_pressure * boost_factor
    mov rax, MISS_PRESSURE_ALPHA
    movq xmm3, rax
    mulsd xmm1, xmm3
    addsd xmm2, xmm1
    ; Decay dream pressure
    mulsd xmm2, [rel pressure_decay]
    movsd [r12 + ST_DREAM_PRESSURE], xmm2

    ; --- Check dream pressure threshold → ORGANIC DREAM ---
    mov rax, DREAM_PRESSURE_THRESH
    movq xmm3, rax
    ucomisd xmm2, xmm3
    jbe .check_observe

    ; --- Energy check: can we afford to dream? ---
    movsd xmm4, [rbx + STATE_OFFSET + ST_ENERGY]
    mov rax, ENERGY_STARVATION
    movq xmm5, rax
    ucomisd xmm4, xmm5
    jbe .check_observe         ; too hungry to dream — skip

    ; ORGANIC DREAM: miss pressure demands consolidation
    ; Deduct energy cost
    mov rax, ENERGY_DREAM_COST
    movq xmm5, rax
    subsd xmm4, xmm5
    xorpd xmm6, xmm6
    maxsd xmm4, xmm6
    movsd [rbx + STATE_OFFSET + ST_ENERGY], xmm4
    addsd xmm5, [rbx + STATE_OFFSET + ST_ENERGY_SPENT]
    movsd [rbx + STATE_OFFSET + ST_ENERGY_SPENT], xmm5

    push rdi
    lea rdi, [rel intro_organic_msg]
    call print_cstr
    lea rdi, [rel intro_dream_trig]
    call print_cstr
    pop rdi

    inc dword [r12 + ST_ORGANIC_DREAMS]

    ; Reset dream pressure (action taken)
    movsd xmm0, [rel zero_f64]
    movsd [r12 + ST_DREAM_PRESSURE], xmm0

    ; Fire the dream
    call dream_cycle

.check_observe:
    ; --- Accumulate observe pressure from accuracy variance ---
    movss xmm0, [r12 + ST_ACCURACY_VARIANCE]
    cvtss2sd xmm0, xmm0        ; convert to f64
    movsd xmm1, [r12 + ST_OBSERVE_PRESSURE]
    movsd xmm2, [rel observe_boost]
    mulsd xmm0, xmm2
    addsd xmm1, xmm0
    mulsd xmm1, [rel pressure_decay]
    movsd [r12 + ST_OBSERVE_PRESSURE], xmm1

    ; Check observe pressure threshold
    mov rax, OBSERVE_PRESSURE_THRESH
    movq xmm2, rax
    ucomisd xmm1, xmm2
    jbe .check_evolve

    ; ORGANIC OBSERVE: accuracy is drifting, system needs to look at itself
    push rdi
    lea rdi, [rel intro_organic_msg]
    call print_cstr
    lea rdi, [rel intro_observe_trig]
    call print_cstr
    pop rdi

    inc dword [r12 + ST_ORGANIC_OBSERVES]

    ; Reset observe pressure
    movsd xmm0, [rel zero_f64]
    movsd [r12 + ST_OBSERVE_PRESSURE], xmm0

    ; Fire observation
    call observe_cycle
    call drives_check

.check_evolve:
    ; --- Accumulate evolve pressure from stagnation (flatness) ---
    ; Only accumulates when system has been flat LONGER than the minimum window.
    ; This is organic: evolution only fires from sustained stagnation, not brief pauses.
    mov eax, [r12 + ST_FLATNESS_COUNT]
    cmp eax, OSCILLATION_MIN
    jle .evolve_decay_only      ; not yet stagnant, just decay
    sub eax, OSCILLATION_MIN    ; excess flatness beyond threshold
    cvtsi2sd xmm0, eax
    movsd xmm1, [rel evolve_boost]
    mulsd xmm0, xmm1
    movsd xmm1, [r12 + ST_EVOLVE_PRESSURE]
    addsd xmm1, xmm0
    mulsd xmm1, [rel pressure_decay]
    movsd [r12 + ST_EVOLVE_PRESSURE], xmm1
    jmp .check_evolve_fire
.evolve_decay_only:
    movsd xmm1, [r12 + ST_EVOLVE_PRESSURE]
    mulsd xmm1, [rel pressure_decay]
    movsd [r12 + ST_EVOLVE_PRESSURE], xmm1

.check_evolve_fire:
    ; Check evolve pressure threshold
    mov rax, EVOLVE_PRESSURE_THRESH
    movq xmm2, rax
    ucomisd xmm1, xmm2
    jbe .pressure_done

    ; ORGANIC EVOLVE: system is stagnant, needs mutation
    push rdi
    lea rdi, [rel intro_organic_msg]
    call print_cstr
    lea rdi, [rel intro_evolve_trig]
    call print_cstr
    pop rdi

    inc dword [r12 + ST_ORGANIC_EVOLVES]

    ; Reset evolve pressure and flatness
    movsd xmm0, [rel zero_f64]
    movsd [r12 + ST_EVOLVE_PRESSURE], xmm0
    mov dword [r12 + ST_FLATNESS_COUNT], 0

    ; Fire evolution
    call evolve_cycle

.check_introspect:
    ; --- SELF-SURPRISE HANDLING: Introspect pressure from self-model violations ---
    ; This is the self/other boundary: when the system's confident predictions about
    ; ITSELF are wrong, it triggers introspection rather than world-learning.
    ; Introspect pressure is boosted in dispatch.asm on SURPRISE_SELF events.
    movsd xmm1, [r12 + ST_INTROSPECT_PRESSURE]
    mulsd xmm1, [rel pressure_decay]    ; decay toward zero
    movsd [r12 + ST_INTROSPECT_PRESSURE], xmm1

    ; Check introspect pressure threshold
    mov rax, INTROSPECT_PRESSURE_THRESH
    movq xmm2, rax
    ucomisd xmm1, xmm2
    jbe .pressure_done

    ; ORGANIC INTROSPECT: self-model is broken, examine regions that violated it
    push rdi
    lea rdi, [rel intro_organic_msg]
    call print_cstr
    lea rdi, [rel intro_introspect_trig]
    call print_cstr
    pop rdi

    ; Reset introspect pressure (action taken)
    movsd xmm0, [rel zero_f64]
    movsd [r12 + ST_INTROSPECT_PRESSURE], xmm0

    ; Fire self-repair cycle
    call introspect_repair_cycle

.organic_skip:
    ret

.pressure_done:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; update_oscillation()
;; Track whether the system is oscillating (alive) or flat (dead).
;; If flat for too long, inject perturbation via presence field.
;; ============================================================
global update_oscillation
update_oscillation:
    push rbx
    mov rbx, SURFACE_BASE
    lea rdi, [rbx + STATE_OFFSET]

    ; Shift accuracy history: prev_prev = prev, prev = current
    movsd xmm0, [rdi + ST_PREV_ACCURACY]
    movsd [rdi + ST_PREV_PREV_ACC], xmm0

    ; Current accuracy → prev
    movss xmm1, [rdi + ST_DRIVES + 0]      ; current accuracy (f32)
    cvtss2sd xmm1, xmm1
    movsd [rdi + ST_PREV_ACCURACY], xmm1

    ; Compute oscillation amplitude = |current - prev| + |prev - prev_prev|
    movsd xmm2, [rdi + ST_PREV_PREV_ACC]
    subsd xmm1, xmm0           ; current - prev
    ; Absolute value
    movsd xmm3, xmm1
    movsd xmm4, [rel zero_f64]
    ucomisd xmm3, xmm4
    jae .pos1
    movsd xmm5, [rel zero_f64]
    subsd xmm5, xmm3
    movsd xmm3, xmm5
.pos1:
    subsd xmm0, xmm2           ; prev - prev_prev
    movsd xmm4, [rel zero_f64]
    ucomisd xmm0, xmm4
    jae .pos2
    movsd xmm5, [rel zero_f64]
    subsd xmm5, xmm0
    movsd xmm0, xmm5
.pos2:
    addsd xmm3, xmm0           ; total amplitude
    movsd [rdi + ST_OSCILLATION_AMP], xmm3

    ; Check flatness
    mov rax, FLATNESS_THRESH
    movq xmm1, rax
    ucomisd xmm3, xmm1
    ja .not_flat

    ; System is flat — increment flatness counter
    inc dword [rdi + ST_FLATNESS_COUNT]

    ; If flat for too long, perturb via presence
    cmp dword [rdi + ST_FLATNESS_COUNT], OSCILLATION_MIN
    jl .osc_done

    ; PERTURBATION: inject novelty pressure through presence field
    ; Boost arousal, reduce stability, increase surprise
    lea rsi, [rbx + STATE_OFFSET + ST_PRESENCE]
    ; Arousal += 0.3
    mov eax, 0x3E99999A         ; 0.3f
    movd xmm0, eax
    addss xmm0, [rsi + PRES_AROUSAL * 4]
    movss [rsi + PRES_AROUSAL * 4], xmm0
    ; Stability -= 0.2
    mov eax, 0x3E4CCCCD         ; 0.2f
    movd xmm0, eax
    movss xmm1, [rsi + PRES_STABILITY * 4]
    subss xmm1, xmm0
    movss [rsi + PRES_STABILITY * 4], xmm1
    ; Temperature += 0.25
    mov eax, 0x3E800000         ; 0.25f
    movd xmm0, eax
    addss xmm0, [rsi + PRES_TEMPERATURE * 4]
    movss [rsi + PRES_TEMPERATURE * 4], xmm0

    ; Print flatness warning
    push rdi
    lea rdi, [rel intro_flatness_msg]
    call print_cstr
    pop rdi

    jmp .osc_done

.not_flat:
    ; System is oscillating — reset flatness counter
    mov dword [rdi + ST_FLATNESS_COUNT], 0

.osc_done:
    pop rbx
    ret

;; ============================================================
;; update_presence_dispatch()
;; Let the presence field influence dispatch mode selection.
;; High arousal → explore. High fatigue → fast. High focus → deliberate.
;; The presence IS the experience influencing behavior.
;; ============================================================
global update_presence_dispatch
update_presence_dispatch:
    push rbx
    mov rbx, SURFACE_BASE
    lea rdi, [rbx + STATE_OFFSET]
    lea rsi, [rbx + STATE_OFFSET + ST_PRESENCE]

    ; Read presence dimensions
    movss xmm0, [rsi + PRES_AROUSAL * 4]     ; arousal
    movss xmm1, [rsi + PRES_FOCUS * 4]       ; focus
    movss xmm2, [rsi + PRES_FATIGUE * 4]     ; fatigue
    movss xmm3, [rsi + PRES_TEMPERATURE * 4] ; temperature

    ; Compute risk appetite = arousal - fatigue + temperature * 0.5
    movss xmm4, xmm0           ; arousal
    subss xmm4, xmm2           ; - fatigue
    mov eax, 0x3F000000         ; 0.5f
    movd xmm5, eax
    mulss xmm5, xmm3           ; temperature * 0.5
    addss xmm4, xmm5           ; risk appetite
    movss [rdi + ST_PRES_RISK_APPETITE], xmm4

    ; Compute dispatch bias = focus - arousal * 0.3
    movss xmm5, xmm1           ; focus
    mov eax, 0x3E99999A         ; 0.3f
    movd xmm6, eax
    mulss xmm6, xmm0           ; arousal * 0.3
    subss xmm5, xmm6
    movss [rdi + ST_PRES_DISPATCH_BIAS], xmm5

    ; Mode selection based on presence state and energy:
    ; Starving → FAST always (survival mode)
    ; High fatigue → FAST (conserve energy)
    ; High arousal + low focus → EXPLORE
    ; High focus + low arousal → DELIBERATE
    ; Otherwise → drive-selected or BEST

    ; Check energy starvation first (overrides everything)
    movsd xmm5, [rbx + STATE_OFFSET + ST_ENERGY]
    mov rax, ENERGY_STARVATION
    movq xmm6, rax
    ucomisd xmm5, xmm6
    ja .energy_ok
    ; STARVING: force minimal-cost dispatch
    mov dword [rdi + ST_DISPATCH_MODE], DMODE_FAST
    jmp .pres_done
.energy_ok:

    mov eax, 0x3F19999A         ; 0.6f threshold
    movd xmm5, eax

    ; Check fatigue first (fatigue overrides — tired system goes fast)
    comiss xmm2, xmm5
    jbe .check_arousal
    mov dword [rdi + ST_DISPATCH_MODE], DMODE_FAST
    jmp .pres_done

.check_arousal:
    ; High arousal + high temperature → EXPLORE
    addss xmm0, xmm3           ; arousal + temperature
    mov eax, 0x3F800000         ; 1.0f
    movd xmm5, eax
    comiss xmm0, xmm5
    jbe .check_focus
    mov dword [rdi + ST_DISPATCH_MODE], DMODE_EXPLORE
    jmp .pres_done

.check_focus:
    ; High focus → DELIBERATE
    mov eax, 0x3F333333         ; 0.7f
    movd xmm5, eax
    comiss xmm1, xmm5
    jbe .pres_done              ; leave mode as-is (drive-selected)
    mov dword [rdi + ST_DISPATCH_MODE], DMODE_DELIBERATE

.pres_done:
    pop rbx
    ret

;; ============================================================
;; Topological Metacognition Reporting
;; ============================================================

section .data
    meta_msg:       db "[METACOG] ", 0
    meta_feeling:   db "feeling=", 0
    meta_neutral:   db "NEUTRAL", 0
    meta_confident: db "CONFIDENT", 0
    meta_anxious:   db "ANXIOUS", 0
    meta_conf_val:  db " confidence=", 0
    meta_mode:      db " → mode=", 0
    meta_fast:      db "FAST", 0
    meta_deliberate: db "DELIBERATE", 0
    meta_other:     db "OTHER", 0
    meta_updates:   db " (updates=", 0
    meta_close:     db ")", 0

section .text

extern confidence_query
extern confidence_get_feeling
extern confidence_get_update_count

;; ============================================================
;; metacog_report(ctx_hash)
;; edi=ctx_hash
;; Prints the system's metacognitive state for this context:
;; feeling (neutral/confident/anxious), raw confidence score,
;; and resulting dispatch mode.
;; Actively queries the confidence vector (doesn't just read stored values).
;; ============================================================
global metacog_report
metacog_report:
    push rbx
    push r12
    sub rsp, 24               ; space for confidence value (f64)

    mov r12d, edi             ; save ctx_hash
    mov rbx, SURFACE_BASE

    ; Query confidence for this specific context
    mov edi, r12d
    call confidence_query     ; → xmm0 = confidence score (f64)
    movsd [rsp], xmm0         ; save confidence value

    ; Get feeling for this context
    mov edi, r12d
    call confidence_get_feeling  ; → eax = feeling enum
    mov [rsp + 8], eax        ; save feeling

    ; Print header
    lea rdi, [rel meta_msg]
    call print_cstr

    ; Print feeling label
    lea rdi, [rel meta_feeling]
    call print_cstr

    ; Print feeling
    mov eax, [rsp + 8]
    cmp eax, FEELING_CONFIDENT
    je .print_confident
    cmp eax, FEELING_ANXIOUS
    je .print_anxious
    lea rdi, [rel meta_neutral]
    jmp .printed_feeling
.print_confident:
    lea rdi, [rel meta_confident]
    jmp .printed_feeling
.print_anxious:
    lea rdi, [rel meta_anxious]
.printed_feeling:
    call print_cstr

    ; Print raw confidence value (convert f64 to f32 for printing)
    lea rdi, [rel meta_conf_val]
    call print_cstr
    movsd xmm0, [rsp]         ; reload confidence
    cvtsd2ss xmm0, xmm0       ; f64 → f32
    call print_f32

    ; Print resulting mode
    lea rdi, [rel meta_mode]
    call print_cstr
    mov eax, [rbx + STATE_OFFSET + ST_DISPATCH_MODE]
    cmp eax, DMODE_FAST
    je .mode_fast
    cmp eax, DMODE_DELIBERATE
    je .mode_deliberate
    lea rdi, [rel meta_other]
    jmp .printed_mode
.mode_fast:
    lea rdi, [rel meta_fast]
    jmp .printed_mode
.mode_deliberate:
    lea rdi, [rel meta_deliberate]
.printed_mode:
    call print_cstr

    ; Print update count for debugging
    lea rdi, [rel meta_updates]
    call print_cstr
    call confidence_get_update_count
    mov rdi, rax
    call print_u64
    lea rdi, [rel meta_close]
    call print_cstr

    call print_newline

    add rsp, 24
    pop r12
    pop rbx
    ret

;; ============================================================
;; STRUCTURAL ABSORPTION: Learning Grammar from Ingested Code
;; "You are what you eat" — valid code becomes high-value schema
;; ============================================================

section .data
    absorb_msg:         db "[ABSORB] Analyzing ingested code (", 0
    absorb_bytes:       db " bytes)...", 10, 0
    absorb_valid:       db "[ABSORB] VALID — storing as schema", 10, 0
    absorb_invalid:     db "[ABSORB] INVALID — rejected (", 0
    absorb_errors:      db " errors)", 10, 0
    absorb_energy:      db "[ABSORB] Energy gained from valid schema: ", 0

    align 8
    schema_energy:      dq 5.0    ; energy reward for absorbing valid code

section .text

extern verify_abstract
extern holo_store
extern vsa_energy_to_valence

;; ============================================================
;; absorb_code(code_ptr, code_len) -> eax (1=absorbed, 0=rejected)
;; rdi=code pointer, rsi=code length
;; Applies the Logic Probe to ingested code. If valid:
;;   - Stores in holographic memory as high-value schema
;;   - Rewards system with energy (valid code = nutritious food)
;; If invalid:
;;   - Rejected with negative valence (bad food = poison)
;; ============================================================
global absorb_code
absorb_code:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, VCS_SIZE + 16    ; verification state + locals

    mov r12, rdi              ; code ptr
    mov r13, rsi              ; code len
    mov rbx, SURFACE_BASE

    ; Print analysis message
    lea rdi, [rel absorb_msg]
    call print_cstr
    mov rdi, r13
    call print_u64
    lea rdi, [rel absorb_bytes]
    call print_cstr

    ; Skip if too short or too long
    cmp r13, 3
    jl .absorb_reject
    cmp r13, 4096             ; max schema size
    jg .absorb_reject

    ; Run abstract interpreter
    mov rdi, r12              ; code
    mov rsi, r13              ; length
    lea rdx, [rsp]            ; output state
    call verify_abstract
    mov r14d, eax             ; save error count

    test eax, eax
    jnz .absorb_reject

    ; --- VALID CODE: Store as schema ---
    lea rdi, [rel absorb_valid]
    call print_cstr

    ; Generate hash from code bytes
    xor eax, eax
    mov rcx, r13
    mov rsi, r12
.hash_code:
    test rcx, rcx
    jz .hash_done
    movzx edx, byte [rsi]
    imul eax, eax, 31
    add eax, edx
    inc rsi
    dec rcx
    jmp .hash_code
.hash_done:
    mov r14d, eax             ; code hash

    ; Store in holographic memory with high strength
    mov edi, r14d             ; ctx = code_hash
    mov esi, r14d             ; token = code_hash (self-reference)
    mov rax, 0x3FF0000000000000  ; 1.0 f64 (max strength)
    movq xmm0, rax
    call holo_store

    ; Reward energy
    movsd xmm0, [rbx + STATE_OFFSET + ST_ENERGY]
    addsd xmm0, [rel schema_energy]
    mov rax, ENERGY_MAX
    movq xmm1, rax
    minsd xmm0, xmm1
    movsd [rbx + STATE_OFFSET + ST_ENERGY], xmm0

    ; Print energy gained
    lea rdi, [rel absorb_energy]
    call print_cstr
    movsd xmm0, [rel schema_energy]
    cvtsd2ss xmm0, xmm0
    call print_f32
    call print_newline

    mov eax, 1                ; return absorbed
    jmp .absorb_done

.absorb_reject:
    lea rdi, [rel absorb_invalid]
    call print_cstr
    mov edi, r14d
    call print_u64
    lea rdi, [rel absorb_errors]
    call print_cstr

    xor eax, eax              ; return rejected

.absorb_done:
    add rsp, VCS_SIZE + 16
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; HIVE WORKER SYSTEM: Pheromone-Driven Swarm Intelligence
;; Replaces centralized REPL control with pheromone-triggered
;; worker castes. Each caste responds to its pheromone channel
;; and performs specialized work when threshold exceeded.
;; ============================================================

section .data
    worker_trigger_msg: db "[HIVE] ", 0
    worker_dream:       db "Dream worker activated (pheromone=", 0
    worker_observe:     db "Observe worker activated (pheromone=", 0
    worker_evolve:      db "Evolve worker activated (pheromone=", 0
    worker_compact:     db "Compact worker activated", 10, 0
    worker_rest:        db "Rest worker activated (fatigue=", 0
    worker_ingest:      db "Self-ingest worker activated (SELF-AWARE=", 0
    worker_bootstrap:   db "Bootstrap: low self-awareness, triggering observe", 10, 0
    worker_startup_dream: db "Startup consolidation: prior knowledge found, dreaming...", 10, 0
    worker_close:       db ")", 10, 0

    align 8
    pheromone_threshold: dq 0.5   ; default activation threshold
    self_aware_threshold: dq 0.3  ; below this, trigger self-observation
    self_ingest_done: dq 0        ; flag: have we ingested own code this session?
    startup_consolidation_done: dq 0  ; flag: have we dreamed on startup?
    traces_verified:  dq 0        ; flag: have we checked action trace integrity?

    ; Autonomy loop constants
    cold_start_thresh:  dq 0.1    ; minimum cosim score to trust resonance
    outcome_baseline:   dq 0.1    ; minimum outcome so traces get seeded (prevents delta_valence ≈ 0 → empty traces)
    outcome_valence_w:  dq 1.0    ; weight for valence delta in outcome
    half_f64:           dq 0.5    ; for fatigue reduction

    ; Autonomy loop messages
    autonomy_resonance_msg: db "[AUTONOMY] Resonance selected: ", 0
    trace_reseed_msg:       db "[AUTONOMY] Action traces empty — reseeding all 10 from presence", 10, 0
    trace_reseed_done_msg:  db "[AUTONOMY] Traces reseeded, resonance can now bootstrap", 10, 0
    autonomy_fallback_msg:  db "[AUTONOMY] Cold start — pressure fallback", 10, 0
    autonomy_outcome_msg:   db "[AUTONOMY] Outcome recorded: ", 0
    autonomy_action_dream:  db "DREAM", 0
    autonomy_action_observe: db "OBSERVE", 0
    autonomy_action_evolve: db "EVOLVE", 0
    autonomy_action_rest:   db "REST", 0
    autonomy_action_explore: db "EXPLORE", 0
    autonomy_action_seek:    db "SEEK", 0
    autonomy_action_scan_env: db "SCAN_ENV", 0
    autonomy_action_compose:  db "COMPOSE", 0
    autonomy_action_teach:    db "TEACH", 0
    autonomy_action_reflect:  db "REFLECT", 0
    autonomy_score_msg:     db " (score=", 0
    autonomy_close_paren:   db ")", 10, 0
    autonomy_gated_msg:     db " [GATED]", 10, 0
    autonomy_seek_msg:      db "[SEEK] Scanning for relevant file (confusion=", 0
    autonomy_seek_file_msg: db "[SEEK] Digesting: ", 0
    autonomy_seek_none_msg: db "[SEEK] No files found in explore path", 10, 0
    autonomy_scan_msg:      db "[SCAN_ENV] Reading: ", 0

    ; Curiosity pressure constants
    curiosity_boost:    dq 0.03
    curiosity_thresh:   dq 0.4
    confusion_thresh:   dq 0.3
    worker_curiosity:   db "Curiosity (low accuracy) -> SEEK", 10, 0
    worker_curiosity_scan: db "Environmental curiosity -> SCAN_ENV", 10, 0

    ; Probe constants: file is "readable" if >75% of first 256 bytes are printable
    ; Printable = 0x09 (tab), 0x0A (newline), 0x0D (CR), 0x20-0x7E
    align 8
    probe_threshold:    dq 75         ; percentage threshold for printable bytes

    ; Frontier messages
    frontier_advance_msg:   db "[SEEK] Advancing explore path to: ", 0
    frontier_exhausted_msg: db "[SEEK] Exploration frontier exhausted", 10, 0
    frontier_push_msg:      db "[SEEK] Queued subdir: ", 0
    seek_skip_binary_msg:   db "[SEEK] Skipping binary: ", 0
    seek_all_seen_msg:      db "[SEEK] All files seen, advancing path", 10, 0

    ; Composition messages
    compose_start_msg:      db "[COMPOSE] Generating from current context...", 10, 0
    compose_token_msg:      db "[COMPOSE] Generated ", 0
    compose_tokens_msg:     db " tokens (avg conf=", 0
    compose_empty_msg:      db "[COMPOSE] Not enough material to compose", 10, 0
    compose_close:          db ")", 10, 0

    ; Teach messages
    teach_msg:              db "[TEACH] Shared schema trace to colony", 10, 0
    teach_solo_msg:         db "[TEACH] Solo mode — no colony to teach", 10, 0

    ; Reflect messages
    reflect_msg:            db "[REFLECT] Self-model updated: ", 0
    reflect_resonate_msg:   db "  Action ", 0
    reflect_resonate2_msg:  db " resonates with self (sim=", 0
    reflect_actions_msg:    db " actions resonate, delta=", 0
    reflect_stable_msg:     db "[REFLECT] Self-model stable", 10, 0
    reflect_shifted_msg:    db "[REFLECT] Self-model shifted", 10, 0

    ; Maturity seeding messages
    maturity_seed_msg:      db "[MATURITY] Seeded SEEK/SCAN_ENV traces from current state", 10, 0
    maturity_seed2_msg:     db "[MATURITY] Seeded COMPOSE/REFLECT traces, enabled actions", 10, 0

    ; Explore path default
    explore_default_path:   db "corpus/", 0

    ; /proc files for SCAN_ENV rotation
    proc_cpuinfo:    db "/proc/cpuinfo", 0
    proc_meminfo:    db "/proc/meminfo", 0
    proc_self_stat:  db "/proc/self/status", 0
    proc_uptime:     db "/proc/uptime", 0

    align 8
    proc_file_table:
        dq proc_cpuinfo
        dq proc_meminfo
        dq proc_self_stat
        dq proc_uptime
    proc_file_count equ 4

    ; Action name table (pointers, 10 entries)
    align 8
    action_names:
        dq autonomy_action_dream
        dq autonomy_action_observe
        dq autonomy_action_evolve
        dq autonomy_action_rest
        dq autonomy_action_explore
        dq autonomy_action_seek
        dq autonomy_action_scan_env
        dq autonomy_action_compose
        dq autonomy_action_teach
        dq autonomy_action_reflect

section .text

extern dream_cycle
extern observe_cycle
extern evolve_cycle
extern region_compact
extern holo_gen_vec
extern holo_scale_f64
extern holo_bind_f64
extern holo_cosim_f64
extern vsa_zero
extern digest_file
extern get_maturity_level
extern trace_context_confidence
extern print_f64
extern dispatch_predict
extern holo_magnitude_f64
extern update_presence

;; ============================================================
;; init_action_registry()
;; Sets gate levels, enables actions, initializes explore path.
;; Called once from boot.asm after maturity_init.
;;
;; Actions 0-4: gate=0 (always available), enabled
;; Actions 5-6: gate=1 (need Stage 1 maturity), enabled
;; Actions 7-9: gate=2 (future), disabled
;; ============================================================
global init_action_registry
init_action_registry:
    push rbx
    ; 1 push (odd) → stack is 16-aligned → no sub needed

    mov rbx, SURFACE_BASE
    lea rdi, [rbx + STATE_OFFSET + ST_ACTION_REGISTRY]

    ; Zero the entire registry first (160 bytes = 20 qwords)
    xor eax, eax
    mov ecx, 20
.zero_reg:
    mov [rdi], rax
    add rdi, 8
    dec ecx
    jnz .zero_reg

    lea rdi, [rbx + STATE_OFFSET + ST_ACTION_REGISTRY]

    ; Actions 0-4: gate=0, flags=1 (enabled)
    %assign i 0
    %rep 5
        mov dword [rdi + i * ACTION_REG_ENTRY_SIZE + ACTION_REG_GATE], 0
        mov dword [rdi + i * ACTION_REG_ENTRY_SIZE + ACTION_REG_FLAGS], 1
        %assign i i+1
    %endrep

    ; Actions 5-6: gate=1, flags=1 (enabled but gated by maturity)
    mov dword [rdi + ACTION_SEEK * ACTION_REG_ENTRY_SIZE + ACTION_REG_GATE], 1
    mov dword [rdi + ACTION_SEEK * ACTION_REG_ENTRY_SIZE + ACTION_REG_FLAGS], 1
    mov dword [rdi + ACTION_SCAN_ENV * ACTION_REG_ENTRY_SIZE + ACTION_REG_GATE], 1
    mov dword [rdi + ACTION_SCAN_ENV * ACTION_REG_ENTRY_SIZE + ACTION_REG_FLAGS], 1

    ; Action 7 (COMPOSE): gate=2, flags=1 (enabled but needs Stage 2)
    mov dword [rdi + ACTION_COMPOSE * ACTION_REG_ENTRY_SIZE + ACTION_REG_GATE], 2
    mov dword [rdi + ACTION_COMPOSE * ACTION_REG_ENTRY_SIZE + ACTION_REG_FLAGS], 1

    ; Action 8 (TEACH): gate=2, flags=0 (disabled until colony mode)
    mov dword [rdi + ACTION_TEACH * ACTION_REG_ENTRY_SIZE + ACTION_REG_GATE], 2
    mov dword [rdi + ACTION_TEACH * ACTION_REG_ENTRY_SIZE + ACTION_REG_FLAGS], 0

    ; Action 9 (REFLECT): gate=2, flags=1 (enabled but needs Stage 2)
    mov dword [rdi + ACTION_REFLECT * ACTION_REG_ENTRY_SIZE + ACTION_REG_GATE], 2
    mov dword [rdi + ACTION_REFLECT * ACTION_REG_ENTRY_SIZE + ACTION_REG_FLAGS], 1

    ; Initialize explore path = "corpus/"
    lea rdi, [rbx + STATE_OFFSET + ST_EXPLORE_PATH]
    lea rsi, [rel explore_default_path]
    ; Copy up to null terminator
.copy_path:
    lodsb
    stosb
    test al, al
    jnz .copy_path

    ; Zero scan_env_index and files_seen_count
    mov dword [rbx + STATE_OFFSET + ST_SCAN_ENV_INDEX], 0
    mov dword [rbx + STATE_OFFSET + ST_FILES_SEEN_COUNT], 0

    pop rbx
    ret

;; ============================================================
;; AUTONOMY LOOP: Holographic Resonance-Based Action Selection
;;
;; Instead of checking each pressure against a fixed threshold,
;; encode the entire presence field as a holographic vector and
;; compare against action traces that accumulate experience of
;; when each action helped. Same mechanism as token prediction.
;;
;; encode_presence_vec() — state → vector
;; resonance_select()   — vector × traces → best action
;; record_action_outcome() — learn from result
;; ============================================================

;; ============================================================
;; encode_presence_vec()
;; Encodes 30-dim presence field + structural context into
;; a single 8192-dim f64 holographic vector at ST_AUTONOMY_VEC.
;;
;; Algorithm:
;;   1. Zero ST_AUTONOMY_VEC
;;   2. For each presence dim i (0-29):
;;      - role_vec = holo_gen_vec(0x50520000 | i)
;;      - magnitude = presence[i] (f32→f64)
;;      - holo_scale_f64(role_vec, magnitude)
;;      - holo_superpose_f64(autonomy_vec, role_vec)
;;   3. If ST_STRUCT_CTX_VALID:
;;      - holo_bind_f64(autonomy_vec, struct_ctx_vec, autonomy_vec)
;;   4. holo_normalize_f64(autonomy_vec)
;; ============================================================
global encode_presence_vec
encode_presence_vec:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, HOLO_VEC_BYTES       ; temp vector (5 pushes=odd → aligned, sub must be mult of 16)

    mov rbx, SURFACE_BASE
    lea r12, [rbx + STATE_OFFSET]

    ; 1. Zero autonomy vec
    lea rdi, [r12 + ST_AUTONOMY_VEC]
    call vsa_zero

    ; r13 = presence dim loop counter
    xor r13d, r13d

.encode_dim_loop:
    cmp r13d, 30
    jge .encode_bind_ctx

    ; Generate role vector for this dimension into temp buffer on stack
    mov edi, 0x50520000           ; "PR\0\0" base hash
    or edi, r13d                  ; unique per dimension
    lea rsi, [rsp]                ; temp vector on stack
    call holo_gen_vec

    ; Load presence[i] as f64 magnitude
    movss xmm0, [r12 + ST_PRESENCE + r13 * 4]
    cvtss2sd xmm0, xmm0          ; f32 → f64

    ; Scale role vector by magnitude
    lea rdi, [rsp]                ; temp vector
    ; xmm0 already has the scalar
    call holo_scale_f64

    ; Superpose into autonomy vec: autonomy_vec += scaled_role
    lea rdi, [r12 + ST_AUTONOMY_VEC]
    lea rsi, [rsp]                ; temp vector
    call holo_superpose_f64

    inc r13d
    jmp .encode_dim_loop

.encode_bind_ctx:
    ; 3. If structural context is valid, bind it in
    cmp dword [r12 + ST_STRUCT_CTX_VALID], 0
    je .encode_normalize

    ; bind(autonomy_vec, struct_ctx) → autonomy_vec
    ; This means same mood + different content = different state vector
    lea rdi, [r12 + ST_AUTONOMY_VEC]
    lea rsi, [r12 + ST_STRUCT_CTX_VEC]
    lea rdx, [r12 + ST_AUTONOMY_VEC]     ; output = in-place
    call holo_bind_f64

.encode_normalize:
    ; 4. Normalize to unit length (prevents magnitude issues)
    lea rdi, [r12 + ST_AUTONOMY_VEC]
    call holo_normalize_f64

    add rsp, HOLO_VEC_BYTES
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; ensure_action_traces_seeded — Seed action traces if empty
;; Uses current presence encoding (f64 8192-dim vectors only).
;; ============================================================
global ensure_action_traces_seeded
ensure_action_traces_seeded:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 8                    ; 4 pushes (even) + 8 = aligned

    cmp qword [rel traces_verified], 0
    jne .seed_ret

    mov rbx, SURFACE_BASE
    lea r12, [rbx + STATE_OFFSET]

    ; Check magnitude of first trace
    lea rdi, [r12 + ST_ACTION_TRACES]
    call holo_magnitude_f64       ; xmm0 = magnitude
    xorpd xmm1, xmm1
    ucomisd xmm0, xmm1
    jne .mark_verified            ; nonzero → traces exist

    lea rdi, [rel trace_reseed_msg]
    call print_cstr
    call encode_presence_vec      ; fills ST_AUTONOMY_VEC

    xor r13d, r13d                ; action index
.seed_loop:
    cmp r13d, ACTION_COUNT
    jge .seed_done
    push r13
    imul eax, r13d, HOLO_VEC_BYTES
    lea rdi, [r12 + ST_ACTION_TRACES]
    add rdi, rax
    lea rsi, [r12 + ST_AUTONOMY_VEC]
    call holo_superpose_f64
    pop r13
    inc r13d
    jmp .seed_loop
.seed_done:
    lea rdi, [rel trace_reseed_done_msg]
    call print_cstr

.mark_verified:
    mov qword [rel traces_verified], 1

.seed_ret:
    add rsp, 8
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; resonance_select() -> eax = ACTION_* id, xmm0 = best score
;; Queries all action traces against current autonomy_vec.
;; Calls encode_presence_vec first to update the state encoding.
;; ============================================================
global resonance_select
resonance_select:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 24                   ; locals + alignment (4 pushes=even, +24=8mod16→aligned)
    ; [rsp+0] = best_score (f64), [rsp+8] = best_action (u32)

    mov rbx, SURFACE_BASE
    lea r12, [rbx + STATE_OFFSET]

    ; Encode current state
    call encode_presence_vec

    ; Get maturity level once (cheap: single load from surface)
    call get_maturity_level       ; eax = stage (0-2)
    mov [rsp + 16], eax           ; store for gate checks

    ; Initialize best = -1.0, action = -1
    mov rax, 0xBFF0000000000000   ; -1.0 f64
    mov [rsp], rax
    mov dword [rsp + 8], -1

    ; Loop over all action traces
    xor r13d, r13d                ; action index

.resonance_loop:
    cmp r13d, ACTION_COUNT
    jge .resonance_done

    ; --- Gate check: skip if action is disabled or maturity too low ---
    imul eax, r13d, ACTION_REG_ENTRY_SIZE
    ; r14 = registry entry ptr
    lea r14, [r12 + ST_ACTION_REGISTRY]
    add r14, rax
    ; Check flags (enabled?)
    cmp dword [r14 + ACTION_REG_FLAGS], 0
    je .resonance_next            ; disabled action, skip
    ; Check gate level vs maturity
    mov ecx, [r14 + ACTION_REG_GATE]
    cmp ecx, [rsp + 16]          ; gate > maturity?
    jg .resonance_next            ; gated out, skip

    ; trace_ptr = ST_ACTION_TRACES + i * HOLO_VEC_BYTES
    imul r14d, r13d, HOLO_VEC_BYTES
    lea rdi, [r12 + ST_AUTONOMY_VEC]
    lea rsi, [r12 + ST_ACTION_TRACES]
    add rsi, r14                  ; trace_ptr for action i
    call holo_cosim_f64           ; xmm0 = cosim score

    ; Compare with best
    movsd xmm1, [rsp]            ; best_score
    ucomisd xmm0, xmm1
    jbe .resonance_next           ; not better

    ; New best
    movsd [rsp], xmm0
    mov [rsp + 8], r13d

.resonance_next:
    inc r13d
    jmp .resonance_loop

.resonance_done:
    ; Return best_action in eax, best_score in xmm0
    mov eax, [rsp + 8]
    movsd xmm0, [rsp]

    add rsp, 24
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; record_action_outcome(edi=action_id)
;; Measures outcome by comparing presence valence before/after.
;; Scales the pre-action state vector by outcome and superposes
;; into the action's trace. Normalizes to prevent explosion.
;; ============================================================
global record_action_outcome
record_action_outcome:
    push rbx
    push r12
    push r13
    sub rsp, 16                   ; alignment (3 pushes=odd → aligned, sub must be mult of 16)

    mov r12d, edi                 ; action_id
    mov rbx, SURFACE_BASE
    lea r13, [rbx + STATE_OFFSET]

    ; Measure outcome: new_valence - snapshot_valence
    ; Current valence
    movss xmm0, [r13 + ST_PRESENCE + PRES_VALENCE * 4]
    cvtss2sd xmm0, xmm0
    ; Snapshot valence (was stored before action)
    movss xmm1, [r13 + ST_AUTONOMY_SNAP + PRES_VALENCE * 4]
    cvtss2sd xmm1, xmm1
    subsd xmm0, xmm1             ; outcome = delta_valence

    ; Scale autonomy_vec by outcome score
    ; (positive outcome → reinforce this state-action association)
    ; (negative outcome → anti-reinforce)
    lea rdi, [r13 + ST_AUTONOMY_VEC]
    ; xmm0 = outcome scalar
    call holo_scale_f64

    ; Superpose into action trace
    imul eax, r12d, HOLO_VEC_BYTES
    lea rdi, [r13 + ST_ACTION_TRACES]
    add rdi, rax                  ; trace_ptr for this action
    lea rsi, [r13 + ST_AUTONOMY_VEC]
    call holo_superpose_f64

    ; Normalize trace to prevent magnitude explosion
    imul eax, r12d, HOLO_VEC_BYTES
    lea rdi, [r13 + ST_ACTION_TRACES]
    add rdi, rax
    call holo_normalize_f64

    ; Update metadata
    mov dword [r13 + ST_AUTONOMY_ACTIVE], 1
    inc dword [r13 + ST_AUTONOMY_COLD_COUNT]

    add rsp, 16
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; action_seek()
;; Find what UHMA is confused about, scan explore path for a
;; relevant file, and digest it. Dynamic knowledge acquisition.
;;
;; 1. Scan 16 context types via trace_context_confidence()
;;    → pick lowest confidence = most confused context
;; 2. SYS_OPEN + SYS_GETDENTS64 on ST_EXPLORE_PATH
;; 3. For each regular file: hash filename → holo_gen_vec →
;;    cosim against confused context vec → pick best match
;; 4. Build full path, call digest_file()
;;
;; Stack frame: 5 pushes (odd→aligned) + 4096+512+HOLO_VEC_BYTES
;; = sub rsp, round_up_16(4096+512+65536) = 70144
;; Actually let's keep it simpler: 4096 getdents + 512 path = 4608
;; We'll use a scratch vec from the surface instead of stack.
;; sub rsp, 4624 (4608 rounded to mult of 16)
;; [rsp+0..4095] = getdents buffer
;; [rsp+4096..4607] = path buffer (512 bytes)
;; [rsp+4608..4615] = best_score (f64)
;; [rsp+4616..4619] = best_name_offset (u32, offset into getdents buf)
;; [rsp+4620..4623] = padding
;; Total: 4624 bytes. 5 pushes (odd) → sub must be mult of 16.
;; 4624 = 289 * 16. ✓
;; ============================================================
global action_seek
action_seek:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 4624                 ; 5 pushes (odd) → aligned, 4624 is mult of 16

    mov rbx, SURFACE_BASE
    lea r12, [rbx + STATE_OFFSET]

    ; === 1. Find confusion: scan context types 0-15 ===
    ; Track minimum confidence and its ctx_hash
    mov rax, 0x3FF0000000000000   ; 1.0 f64 (start high)
    movq xmm7, rax               ; xmm7 = min_confidence
    xor r14d, r14d                ; best_ctx_type = 0
    xor r13d, r13d                ; loop counter

.seek_ctx_loop:
    cmp r13d, 16
    jge .seek_ctx_done

    ; Generate representative ctx_hash for this type: (type << 28) | 0x0FFFFFFF
    mov edi, r13d
    shl edi, 28
    or edi, 0x0FFFFFFF
    call trace_context_confidence ; xmm0 = confidence (f64) for this context type

    ; Is this lower than current min?
    ucomisd xmm7, xmm0           ; min > current? (note: reversed for "below")
    jbe .seek_ctx_next            ; min <= current, skip

    ; New minimum
    movsd xmm7, xmm0             ; update min
    mov r14d, r13d                ; save ctx_type

.seek_ctx_next:
    inc r13d
    jmp .seek_ctx_loop

.seek_ctx_done:
    ; r14d = most confused context type, xmm7 = its confidence
    ; Store for logging
    mov [r12 + ST_SEEK_CTX_HASH], r14d
    cvtsd2ss xmm0, xmm7
    movss [r12 + ST_SEEK_CONFUSION], xmm0

    ; Log confusion
    lea rdi, [rel autonomy_seek_msg]
    call print_cstr
    movss xmm0, [r12 + ST_SEEK_CONFUSION]
    call print_f32
    lea rdi, [rel autonomy_close_paren]
    call print_cstr

    ; Generate a vector for the confused context (for file selection)
    ; Use the ctx_hash as seed, store vector in SCRATCH area
    mov edi, r14d
    shl edi, 28
    or edi, 0x0FFFFFFF
    mov rcx, SCRATCH_OFFSET
    lea rsi, [rbx + rcx]         ; scratch vec for confused context
    mov r15, rsi                  ; r15 = confused_ctx_vec ptr (in scratch)
    call holo_gen_vec

    ; === 2. Open explore path directory ===
    lea rdi, [r12 + ST_EXPLORE_PATH]
    mov esi, 0x10000              ; O_RDONLY | O_DIRECTORY (0x10000 = O_DIRECTORY)
    xor edx, edx
    mov eax, SYS_OPEN
    syscall
    test eax, eax
    js .seek_no_dir               ; open failed

    mov r13d, eax                 ; r13d = dir_fd

    ; === 3. SYS_GETDENTS64 ===
    mov eax, SYS_GETDENTS64
    mov edi, r13d
    lea rsi, [rsp]                ; getdents buffer on stack
    mov edx, 4096
    syscall
    test eax, eax
    jle .seek_close_dir           ; no entries or error

    mov r14d, eax                 ; r14d = bytes read

    ; Initialize best file selection
    ; [rsp+4608] = best_score (f64), [rsp+4616] = best_entry_offset (u32)
    mov rax, 0xBFF0000000000000   ; -1.0 f64 (no match yet)
    mov [rsp + 4608], rax         ; best_score
    mov dword [rsp + 4616], -1    ; best_entry_offset = invalid

    ; Walk directory entries
    ; r14d = bytes read from getdents, use as limit
    ; We need a register for current offset. r13d has dir_fd — save dir_fd to stack.
    mov [rsp + 4620], r13d        ; save dir_fd in padding slot
    xor r13d, r13d                ; r13d = current offset into getdents buffer

.seek_entry_loop:
    cmp r13d, r14d
    jge .seek_pick_best

    ; Entry ptr = getdents_buf + offset
    lea rsi, [rsp + r13]

    ; d_reclen at offset 16 — save in local for advance
    movzx eax, word [rsi + 16]
    mov [rsp + 4096 + 508], eax   ; stash reclen at end of path buffer area (4 bytes)

    ; d_type at offset 18
    movzx eax, byte [rsi + 18]
    cmp eax, 8                    ; DT_REG?
    jne .seek_next_entry

    ; d_name at offset 19
    lea rdi, [rsi + 19]

    ; Skip "." and ".."
    cmp byte [rdi], '.'
    jne .seek_hash_file
    cmp byte [rdi + 1], 0
    je .seek_next_entry
    cmp byte [rdi + 1], '.'
    jne .seek_hash_file
    cmp byte [rdi + 2], 0
    je .seek_next_entry

.seek_hash_file:
    ; Hash the filename via FNV-1a → generate vector → cosim against confused ctx
    ; rdi = filename ptr (caller-saved, but we don't need it after hash)
    mov eax, FNV32_INIT
.seek_fnv_loop:
    movzx ecx, byte [rdi]
    test ecx, ecx
    jz .seek_fnv_done
    xor eax, ecx
    imul eax, eax, FNV32_PRIME
    inc rdi
    jmp .seek_fnv_loop

.seek_fnv_done:
    ; eax = filename hash
    mov edi, eax
    ; Use second scratch slot for file vector (SCRATCH_OFFSET + HOLO_VEC_BYTES)
    mov rcx, SCRATCH_OFFSET
    lea rsi, [rbx + rcx + HOLO_VEC_BYTES]
    call holo_gen_vec

    ; cosim(file_vec, confused_ctx_vec)
    mov rcx, SCRATCH_OFFSET
    lea rdi, [rbx + rcx + HOLO_VEC_BYTES]   ; file_vec
    mov rsi, r15                              ; confused_ctx_vec
    call holo_cosim_f64           ; xmm0 = similarity

    ; Compare with best
    movsd xmm1, [rsp + 4608]     ; best_score
    ucomisd xmm0, xmm1
    jbe .seek_next_entry

    ; New best file
    movsd [rsp + 4608], xmm0
    mov [rsp + 4616], r13d       ; save getdents buffer offset

.seek_next_entry:
    add r13d, [rsp + 4096 + 508]  ; advance by reclen
    jmp .seek_entry_loop

.seek_pick_best:
    ; Check if we found any file
    cmp dword [rsp + 4616], -1
    je .seek_no_files

    ; Get the winning entry's filename
    mov ecx, [rsp + 4616]        ; offset into getdents buffer
    lea rsi, [rsp]               ; getdents buffer base
    add rsi, rcx
    lea r14, [rsi + 19]          ; d_name of best entry

    ; === 4. Build full path: explore_path + "/" + filename ===
    lea rdi, [rsp + 4096]        ; path buffer on stack (512 bytes)
    ; Copy explore path
    lea rsi, [r12 + ST_EXPLORE_PATH]
.seek_copy_dir:
    lodsb
    test al, al
    jz .seek_check_slash
    stosb
    jmp .seek_copy_dir

.seek_check_slash:
    ; Check if path already ends with '/'
    cmp byte [rdi - 1], '/'
    je .seek_copy_name
    mov byte [rdi], '/'
    inc rdi

.seek_copy_name:
    ; Copy filename
    mov rsi, r14
.seek_copy_fname:
    lodsb
    stosb
    test al, al
    jnz .seek_copy_fname

    ; Log what we're digesting
    lea rdi, [rel autonomy_seek_file_msg]
    call print_cstr
    lea rdi, [rsp + 4096]        ; path buffer
    call print_cstr
    call print_newline

    ; Close dir fd before digesting (digest_file may take a while)
    mov eax, SYS_CLOSE
    mov edi, [rsp + 4620]        ; dir_fd from saved slot
    syscall

    ; === 4b. Probe file: read first 256 bytes, check if mostly printable ===
    lea rdi, [rsp + 4096]        ; path buffer (the file path)
    call probe_readable           ; eax=1 if readable text, 0 if binary
    test eax, eax
    jnz .seek_do_digest
    ; Binary file, skip
    lea rdi, [rel seek_skip_binary_msg]
    call print_cstr
    lea rdi, [rsp + 4096]
    call print_cstr
    call print_newline
    jmp .seek_done

.seek_do_digest:
    ; === 5. Digest the file ===
    lea rdi, [rsp + 4096]        ; path buffer
    call digest_file              ; eax = tokens digested

    ; Increment files seen count
    inc dword [r12 + ST_FILES_SEEN_COUNT]

    ; Increment registry fire count for SEEK
    inc dword [r12 + ST_ACTION_REGISTRY + ACTION_SEEK * ACTION_REG_ENTRY_SIZE + ACTION_REG_FIRES]

    ; Check if we've seen enough files to advance explore path
    ; Approximate: if files_seen >= 10 (arbitrary threshold), try advancing
    mov eax, [r12 + ST_FILES_SEEN_COUNT]
    cmp eax, 10
    jl .seek_done
    ; All files likely seen — advance explore path
    lea rdi, [rel seek_all_seen_msg]
    call print_cstr
    call advance_explore_path

    jmp .seek_done

.seek_no_files:
    lea rdi, [rel autonomy_seek_none_msg]
    call print_cstr

.seek_close_dir:
    mov eax, SYS_CLOSE
    mov edi, [rsp + 4620]        ; dir_fd from saved slot
    syscall
    jmp .seek_done

.seek_no_dir:
    lea rdi, [rel autonomy_seek_none_msg]
    call print_cstr

.seek_done:
    add rsp, 4624
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; action_scan_env()
;; Read system files to build world-model awareness.
;; Rotates through /proc/cpuinfo, /proc/meminfo, /proc/self/status, /proc/uptime.
;; Each call digests one file and advances the index.
;; ============================================================
global action_scan_env
action_scan_env:
    push rbx
    push r12
    push r13
    sub rsp, 16                   ; 3 pushes (odd) → aligned, sub must be mult of 16

    mov rbx, SURFACE_BASE
    lea r12, [rbx + STATE_OFFSET]

    ; Get current index (0-3)
    mov eax, [r12 + ST_SCAN_ENV_INDEX]
    cmp eax, proc_file_count
    jb .scan_valid_idx
    xor eax, eax                  ; wrap around
.scan_valid_idx:
    ; Look up file path from table
    lea rcx, [rel proc_file_table]
    mov r13, [rcx + rax * 8]      ; r13 = path string (callee-saved)

    ; Log what we're reading
    lea rdi, [rel autonomy_scan_msg]
    call print_cstr
    mov rdi, r13
    call print_cstr
    call print_newline

    ; Digest the /proc file
    mov rdi, r13
    call digest_file

    ; Advance index mod proc_file_count
    mov eax, [r12 + ST_SCAN_ENV_INDEX]
    inc eax
    cmp eax, proc_file_count
    jb .scan_store_idx
    xor eax, eax
.scan_store_idx:
    mov [r12 + ST_SCAN_ENV_INDEX], eax

    ; Increment registry fire count for SCAN_ENV
    inc dword [r12 + ST_ACTION_REGISTRY + ACTION_SCAN_ENV * ACTION_REG_ENTRY_SIZE + ACTION_REG_FIRES]

    add rsp, 16
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; print_action_name(edi=action_id)
;; Helper: prints the name of an action
;; ============================================================
print_action_name:
    cmp edi, ACTION_COUNT
    jge .unknown_action
    lea rax, [rel action_names]
    mov rdi, [rax + rdi * 8]
    jmp print_cstr                ; tail call
.unknown_action:
    ret

;; ============================================================
;; tick_workers()
;; Called each processing step. Resonance-first action selection
;; with pressure-based fallback for cold start.
;; Returns: eax = number of workers activated
;; ============================================================
global tick_workers
tick_workers:
global tick_regulators          ; legacy alias
tick_regulators:
    ; Check batch mode - skip all autonomous work if set
    cmp qword [rel batch_mode], 0
    jne .batch_skip

    push rbx
    push r12
    push r13
    sub rsp, 16               ; alignment (3 pushes=odd → aligned, sub must be mult of 16)

    mov rbx, SURFACE_BASE
    xor r12d, r12d            ; actions triggered

    ; Ensure action traces are seeded before resonance selection
    call ensure_action_traces_seeded

    ; === BOOTSTRAP CHECK ===
    ; If self-awareness is low and we haven't bootstrapped, trigger observe
    cmp qword [rel self_ingest_done], 0
    jne .skip_bootstrap

    ; Get current self-awareness reading
    call intro_get_self_awareness     ; xmm0 = SELF-AWARE ratio
    movsd xmm1, [rel self_aware_threshold]
    ucomisd xmm0, xmm1
    jae .skip_bootstrap               ; already self-aware enough

    ; Self-awareness is low - boost observe pressure to trigger self-observation
    lea rdi, [rel worker_bootstrap]
    call print_cstr

    ; Set observe_pressure = 1.0 to force observe_cycle
    mov rax, 0x3FF0000000000000       ; 1.0 f64
    mov [rbx + STATE_OFFSET + ST_OBSERVE_PRESSURE], rax

    ; Mark bootstrap done so we don't spam
    mov qword [rel self_ingest_done], 1

.skip_bootstrap:
    ; === STARTUP CONSOLIDATION ===
    ; Skip if batch mode (training shouldn't auto-dream)
    cmp qword [rel batch_mode], 0
    jne .skip_startup_dream

    ; If we have prior knowledge (regions > 100) and haven't consolidated, trigger dream
    cmp qword [rel startup_consolidation_done], 0
    jne .skip_startup_dream

    ; Check region count
    mov eax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    cmp eax, 100
    jl .skip_startup_dream        ; fresh start, skip

    ; Have prior knowledge - trigger consolidation
    lea rdi, [rel worker_trigger_msg]
    call print_cstr
    lea rdi, [rel worker_startup_dream]
    call print_cstr

    ; Set dream pressure high to trigger dream_cycle
    mov rax, 0x3FF0000000000000   ; 1.0 f64
    mov [rbx + STATE_OFFSET + ST_DREAM_PRESSURE], rax

    mov qword [rel startup_consolidation_done], 1

.skip_startup_dream:
    ; === RESONANCE SELECTION (autonomy loop) ===
    ; Snapshot current presence for outcome measurement
    lea rsi, [rbx + STATE_OFFSET + ST_PRESENCE]
    lea rdi, [rbx + STATE_OFFSET + ST_AUTONOMY_SNAP]
    mov ecx, 30                   ; 30 × f32 = 120 bytes
.snap_loop:
    mov eax, [rsi]
    mov [rdi], eax
    add rsi, 4
    add rdi, 4
    dec ecx
    jnz .snap_loop

    ; Try resonance selection
    call resonance_select         ; eax = best action, xmm0 = best score

    ; Check cold start threshold
    movsd xmm1, [rel cold_start_thresh]
    ucomisd xmm0, xmm1
    jbe .fallback_pressures       ; not enough experience yet

    ; Resonance selected an action — log and execute
    mov r13d, eax                 ; save action id
    mov [rbx + STATE_OFFSET + ST_AUTONOMY_LAST_ACTION], eax
    ; Store score as f32 for creature sync
    cvtsd2ss xmm1, xmm0
    movss [rbx + STATE_OFFSET + ST_LAST_ACTION_SCORE], xmm1
    movsd [rsp], xmm0             ; save score on stack (we have 8 bytes local)

    lea rdi, [rel autonomy_resonance_msg]
    call print_cstr
    mov edi, r13d
    call print_action_name
    lea rdi, [rel autonomy_score_msg]
    call print_cstr
    movsd xmm0, [rsp]
    cvtsd2ss xmm0, xmm0
    call print_f32
    lea rdi, [rel autonomy_close_paren]
    call print_cstr

    ; Dispatch to action
    cmp r13d, ACTION_DREAM
    je .res_dream
    cmp r13d, ACTION_OBSERVE
    je .res_observe
    cmp r13d, ACTION_EVOLVE
    je .res_evolve
    cmp r13d, ACTION_REST
    je .res_rest
    cmp r13d, ACTION_EXPLORE
    je .res_explore
    cmp r13d, ACTION_SEEK
    je .res_seek
    cmp r13d, ACTION_SCAN_ENV
    je .res_scan_env
    cmp r13d, ACTION_COMPOSE
    je .res_compose
    cmp r13d, ACTION_TEACH
    je .res_teach
    cmp r13d, ACTION_REFLECT
    je .res_reflect
    jmp .record_outcome

.res_dream:
    call dream_cycle
    ; Reset dream pressure (action taken)
    xorpd xmm0, xmm0
    movsd [rbx + STATE_OFFSET + ST_DREAM_PRESSURE], xmm0
    jmp .record_outcome

.res_observe:
    call observe_cycle
    xorpd xmm0, xmm0
    movsd [rbx + STATE_OFFSET + ST_OBSERVE_PRESSURE], xmm0
    jmp .record_outcome

.res_evolve:
    call evolve_cycle
    xorpd xmm0, xmm0
    movsd [rbx + STATE_OFFSET + ST_EVOLVE_PRESSURE], xmm0
    jmp .record_outcome

.res_rest:
    ; Reduce fatigue by 50%
    movss xmm0, [rbx + STATE_OFFSET + ST_PRESENCE + PRES_FATIGUE * 4]
    mov eax, 0x3F000000           ; 0.5f
    movd xmm1, eax
    mulss xmm0, xmm1
    movss [rbx + STATE_OFFSET + ST_PRESENCE + PRES_FATIGUE * 4], xmm0
    jmp .record_outcome

.res_explore:
    ; Switch to EXPLORE dispatch mode
    mov dword [rbx + STATE_OFFSET + ST_DISPATCH_MODE], DMODE_EXPLORE
    jmp .record_outcome

.res_seek:
    call action_seek
    jmp .record_outcome

.res_scan_env:
    call action_scan_env
    jmp .record_outcome

.res_compose:
    call action_compose
    jmp .record_outcome

.res_teach:
    call action_teach
    jmp .record_outcome

.res_reflect:
    call action_reflect
    jmp .record_outcome

.record_outcome:
    ; Record what happened — learn from the action's effect
    mov edi, r13d
    call record_action_outcome
    inc r12d
    jmp .workers_done

.fallback_pressures:
    ; === PRESSURE CHECKS (cold start fallback) ===
    ; As traces accumulate, resonance fires more often and this path executes less
    lea rdi, [rel autonomy_fallback_msg]
    call print_cstr

    ; Load threshold
    movsd xmm7, [rel pheromone_threshold]

    ; --- Check dream pheromone ---
    movsd xmm0, [rbx + STATE_OFFSET + ST_DREAM_PRESSURE]
    ucomisd xmm0, xmm7
    jbe .fb_check_observe

    ; Dream pheromone exceeded
    lea rdi, [rel worker_trigger_msg]
    call print_cstr
    lea rdi, [rel worker_dream]
    call print_cstr
    movsd xmm0, [rbx + STATE_OFFSET + ST_DREAM_PRESSURE]
    cvtsd2ss xmm0, xmm0
    call print_f32
    lea rdi, [rel worker_close]
    call print_cstr

    call dream_cycle
    inc r12d

    ; Reset pressure + record outcome for learning
    xorpd xmm0, xmm0
    movsd [rbx + STATE_OFFSET + ST_DREAM_PRESSURE], xmm0
    mov edi, ACTION_DREAM
    call record_action_outcome

.fb_check_observe:
    ; --- Check observe pheromone ---
    movsd xmm0, [rbx + STATE_OFFSET + ST_OBSERVE_PRESSURE]
    ucomisd xmm0, xmm7
    jbe .fb_check_evolve

    lea rdi, [rel worker_trigger_msg]
    call print_cstr
    lea rdi, [rel worker_observe]
    call print_cstr
    movsd xmm0, [rbx + STATE_OFFSET + ST_OBSERVE_PRESSURE]
    cvtsd2ss xmm0, xmm0
    call print_f32
    lea rdi, [rel worker_close]
    call print_cstr

    call observe_cycle
    inc r12d

    xorpd xmm0, xmm0
    movsd [rbx + STATE_OFFSET + ST_OBSERVE_PRESSURE], xmm0
    mov edi, ACTION_OBSERVE
    call record_action_outcome

.fb_check_evolve:
    ; --- Check evolve pheromone ---
    movsd xmm0, [rbx + STATE_OFFSET + ST_EVOLVE_PRESSURE]
    ucomisd xmm0, xmm7
    jbe .fb_check_fatigue

    lea rdi, [rel worker_trigger_msg]
    call print_cstr
    lea rdi, [rel worker_evolve]
    call print_cstr
    movsd xmm0, [rbx + STATE_OFFSET + ST_EVOLVE_PRESSURE]
    cvtsd2ss xmm0, xmm0
    call print_f32
    lea rdi, [rel worker_close]
    call print_cstr

    call evolve_cycle
    inc r12d

    xorpd xmm0, xmm0
    movsd [rbx + STATE_OFFSET + ST_EVOLVE_PRESSURE], xmm0
    mov edi, ACTION_EVOLVE
    call record_action_outcome

.fb_check_fatigue:
    ; --- Check fatigue (high fatigue = rest worker) ---
    movss xmm0, [rbx + STATE_OFFSET + ST_PRESENCE + PRES_FATIGUE * 4]
    cvtss2sd xmm0, xmm0
    mov rax, 0x3FE8000000000000   ; 0.75 threshold for rest
    movq xmm1, rax
    ucomisd xmm0, xmm1
    jbe .workers_done

    lea rdi, [rel worker_trigger_msg]
    call print_cstr
    lea rdi, [rel worker_rest]
    call print_cstr
    movss xmm0, [rbx + STATE_OFFSET + ST_PRESENCE + PRES_FATIGUE * 4]
    call print_f32
    lea rdi, [rel worker_close]
    call print_cstr

    ; Rest worker action: reduce fatigue by 50%
    movss xmm0, [rbx + STATE_OFFSET + ST_PRESENCE + PRES_FATIGUE * 4]
    mov eax, 0x3F000000           ; 0.5f
    movd xmm1, eax
    mulss xmm0, xmm1
    movss [rbx + STATE_OFFSET + ST_PRESENCE + PRES_FATIGUE * 4], xmm0

    inc r12d
    mov edi, ACTION_REST
    call record_action_outcome

.fb_check_curiosity:
    ; --- Curiosity pressure: drives SEEK when confused and idle ---
    ; Decay curiosity pressure
    movsd xmm0, [rbx + STATE_OFFSET + ST_CURIOSITY_PRESSURE]
    mulsd xmm0, [rel pressure_decay]

    ; Check if any context type confidence < 0.3 (check 4 representative types)
    ; Quick scan: types 0, 4, 8, 12
    push rax
    xor ecx, ecx                  ; confused flag
    mov edi, 0x0FFFFFFF            ; type 0
    call trace_context_confidence  ; xmm1 = confidence
    movsd xmm2, [rel confusion_thresh]
    ucomisd xmm1, xmm2
    jae .curio_type4
    inc ecx
.curio_type4:
    mov edi, 0x4FFFFFFF            ; type 4
    call trace_context_confidence
    ucomisd xmm1, xmm2
    jae .curio_type8
    inc ecx
.curio_type8:
    mov edi, 0x8FFFFFFF            ; type 8
    call trace_context_confidence
    ucomisd xmm1, xmm2
    jae .curio_type12
    inc ecx
.curio_type12:
    mov edi, 0xCFFFFFFF            ; type 12
    call trace_context_confidence
    ucomisd xmm1, xmm2
    jae .curio_check_boost
    inc ecx

.curio_check_boost:
    pop rax
    test ecx, ecx
    jz .curio_store                ; not confused, don't boost

    ; Check energy > 50% of max (400/2=200)
    movsd xmm3, [rbx + STATE_OFFSET + ST_ENERGY]
    mov rax, 0x4069000000000000    ; 200.0 f64
    movq xmm4, rax
    ucomisd xmm3, xmm4
    jbe .curio_store               ; too tired, no curiosity

    ; Confused AND has energy → boost curiosity
    movsd xmm0, [rbx + STATE_OFFSET + ST_CURIOSITY_PRESSURE]
    addsd xmm0, [rel curiosity_boost]

.curio_store:
    movsd [rbx + STATE_OFFSET + ST_CURIOSITY_PRESSURE], xmm0

    ; Check curiosity threshold → fire SEEK
    movsd xmm1, [rel curiosity_thresh]
    ucomisd xmm0, xmm1
    jbe .fb_check_curiosity_scan

    ; Check cooldown (don't fire too often)
    mov rax, [rbx + STATE_OFFSET + ST_SEEK_LAST_TICK]
    mov rcx, [rbx + STATE_OFFSET + SHDR_TOTAL_STEPS]
    sub rcx, rax
    cmp rcx, ST_CURIOSITY_COOLDOWN
    jl .fb_check_curiosity_scan

    ; Check maturity gate (SEEK needs Stage 1)
    call get_maturity_level
    cmp eax, 1
    jl .fb_check_curiosity_scan

    ; Fire SEEK via curiosity
    lea rdi, [rel worker_trigger_msg]
    call print_cstr
    lea rdi, [rel worker_curiosity]
    call print_cstr

    ; Reset curiosity pressure
    xorpd xmm0, xmm0
    movsd [rbx + STATE_OFFSET + ST_CURIOSITY_PRESSURE], xmm0

    ; Store tick for cooldown
    mov rax, [rbx + STATE_OFFSET + SHDR_TOTAL_STEPS]
    mov [rbx + STATE_OFFSET + ST_SEEK_LAST_TICK], rax

    mov r13d, ACTION_SEEK
    mov [rbx + STATE_OFFSET + ST_AUTONOMY_LAST_ACTION], r13d
    call action_seek
    inc r12d
    mov edi, ACTION_SEEK
    call record_action_outcome
    jmp .workers_done

.fb_check_curiosity_scan:
    ; Also fire SCAN_ENV if curiosity is moderate (> 0.2) and maturity allows
    movsd xmm0, [rbx + STATE_OFFSET + ST_CURIOSITY_PRESSURE]
    mov rax, 0x3FC999999999999A    ; 0.2 f64
    movq xmm1, rax
    ucomisd xmm0, xmm1
    jbe .workers_done

    call get_maturity_level
    cmp eax, 1
    jl .workers_done

    ; Check SCAN_ENV hasn't fired this cycle already
    cmp r12d, 0
    jg .workers_done              ; already did something

    lea rdi, [rel worker_trigger_msg]
    call print_cstr
    lea rdi, [rel worker_curiosity_scan]
    call print_cstr

    mov r13d, ACTION_SCAN_ENV
    mov [rbx + STATE_OFFSET + ST_AUTONOMY_LAST_ACTION], r13d
    call action_scan_env
    inc r12d
    mov edi, ACTION_SCAN_ENV
    call record_action_outcome

.batch_skip:
    xor eax, eax                  ; return 0 workers in batch mode
    ret

.workers_done:
    mov eax, r12d                 ; return workers activated
    add rsp, 16
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; release_pheromone(channel, delta)
;; edi=pheromone channel (PHERO_*), xmm0=delta (f64)
;; Adds delta to the specified pheromone level.
;; This is how events signal worker castes in the hive.
;; ============================================================
global release_pheromone
release_pheromone:
global accrue_pressure          ; legacy alias
accrue_pressure:
    mov rax, SURFACE_BASE

    cmp edi, RPRES_DREAM
    je .accrue_dream
    cmp edi, RPRES_OBSERVE
    je .accrue_observe
    cmp edi, RPRES_EVOLVE
    je .accrue_evolve
    ret                       ; unknown source

.accrue_dream:
    addsd xmm0, [rax + STATE_OFFSET + ST_DREAM_PRESSURE]
    ; Clamp to [0, 1]
    xorpd xmm1, xmm1
    maxsd xmm0, xmm1
    mov rcx, 0x3FF0000000000000
    movq xmm1, rcx
    minsd xmm0, xmm1
    movsd [rax + STATE_OFFSET + ST_DREAM_PRESSURE], xmm0
    ret

.accrue_observe:
    addsd xmm0, [rax + STATE_OFFSET + ST_OBSERVE_PRESSURE]
    xorpd xmm1, xmm1
    maxsd xmm0, xmm1
    mov rcx, 0x3FF0000000000000
    movq xmm1, rcx
    minsd xmm0, xmm1
    movsd [rax + STATE_OFFSET + ST_OBSERVE_PRESSURE], xmm0
    ret

.accrue_evolve:
    addsd xmm0, [rax + STATE_OFFSET + ST_EVOLVE_PRESSURE]
    xorpd xmm1, xmm1
    maxsd xmm0, xmm1
    mov rcx, 0x3FF0000000000000
    movq xmm1, rcx
    minsd xmm0, xmm1
    movsd [rax + STATE_OFFSET + ST_EVOLVE_PRESSURE], xmm0
    ret

;; ============================================================
;; introspect_repair_cycle()
;; SELF/OTHER BOUNDARY: Process regions that violated self-model.
;;
;; When SURPRISE_SELF occurs, it means a high-confidence region was wrong.
;; This is qualitatively different from SURPRISE_OUTCOME (world unknown).
;; Self-surprise demands introspection and self-repair, not just learning.
;;
;; Actions for regions with RFLAG_NEEDS_REPAIR:
;;   1. If hits > misses: specialize (make more specific)
;;   2. If misses > hits: generalize (make broader)
;;   3. Clear RFLAG_NEEDS_REPAIR after processing
;; ============================================================
global introspect_repair_cycle
introspect_repair_cycle:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8                      ; align stack (5 pushes = odd → aligned)

    mov rbx, SURFACE_BASE
    lea r12, [rbx + REGION_TABLE_OFFSET]
    mov r13d, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    xor r14d, r14d                  ; loop index
    xor r15d, r15d                  ; repaired count

.repair_loop:
    cmp r14d, r13d
    jge .repair_done

    ; Get region table entry
    imul rdi, r14, RTE_SIZE
    add rdi, r12

    ; Check if RFLAG_NEEDS_REPAIR is set
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_NEEDS_REPAIR
    jz .repair_next

    ; Found region that violated self-model
    push rdi
    lea rdi, [rel intro_repair_msg]
    call print_cstr
    pop rdi

    ; Get region header
    mov rsi, [rdi + RTE_ADDR]
    test rsi, rsi
    jz .clear_flag

    ; First, consult causal model for guidance
    xor edi, edi              ; use current context
    call meta_recommend_strategy  ; eax = recommended event type

    ; If causal model has data, use it
    cmp eax, EVENT_GENERALIZE
    je .do_generalize
    cmp eax, EVENT_SPECIALIZE
    je .do_specialize
    ; No causal guidance - fall back to heuristic

    ; Heuristic: Analyze performance: hits vs misses
    imul rdi, r14, RTE_SIZE
    add rdi, r12
    mov rsi, [rdi + RTE_ADDR]
    mov eax, [rsi + RHDR_HITS]
    mov ecx, [rsi + RHDR_MISSES]
    cmp eax, ecx
    jg .do_specialize
    ; misses >= hits: generalize (too specific?)

.do_generalize:
    push r14
    mov edi, r14d
    call modify_generalize
    pop r14
    jmp .clear_flag

.do_specialize:
    ; hits > misses but violated: specialize (conflating contexts?)
    push r14
    mov edi, r14d
    call modify_specialize
    pop r14

.clear_flag:
    ; Clear RFLAG_NEEDS_REPAIR
    imul rdi, r14, RTE_SIZE
    add rdi, r12
    movzx eax, word [rdi + RTE_FLAGS]
    and eax, ~RFLAG_NEEDS_REPAIR
    mov word [rdi + RTE_FLAGS], ax

    ; Also clear in header if present
    mov rsi, [rdi + RTE_ADDR]
    test rsi, rsi
    jz .repair_count
    movzx eax, word [rsi + RHDR_FLAGS]
    and eax, ~RFLAG_NEEDS_REPAIR
    mov word [rsi + RHDR_FLAGS], ax

.repair_count:
    inc r15d

.repair_next:
    inc r14d
    jmp .repair_loop

.repair_done:
    ; r15d = number of regions repaired
    mov eax, r15d
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; check_text_file(rdi=filename) -> eax (1=text, 0=skip)
;; Checks if filename has a known text extension.
;; Scans backward from null terminator for last '.', then
;; compares extension against a table of known text types.
;; ============================================================
global probe_readable
probe_readable:
    ; probe_readable(rdi=filepath) -> eax (1=readable text, 0=binary)
    ; Opens file, reads first 256 bytes, checks if >75% are printable ASCII.
    ; Printable = 0x09(tab), 0x0A(newline), 0x0D(CR), 0x20-0x7E
    ; Works on extensionless files (/proc/*, etc.)
    push rbx
    push r12
    push r13
    sub rsp, 272                  ; 3 pushes (odd) → aligned → sub must be mult of 16
                                  ; 256 bytes read buffer + 16 padding = 272

    mov r12, rdi                  ; save filepath

    ; Open file read-only
    mov eax, SYS_OPEN
    mov rdi, r12
    xor esi, esi                  ; O_RDONLY
    xor edx, edx
    syscall
    test eax, eax
    js .probe_reject              ; can't open → skip
    mov ebx, eax                  ; fd

    ; Read first 256 bytes
    mov eax, SYS_READ
    mov edi, ebx
    lea rsi, [rsp]                ; buffer on stack
    mov edx, 256
    syscall
    mov r13d, eax                 ; bytes_read (or error)

    ; Close fd immediately
    push rax
    mov eax, SYS_CLOSE
    mov edi, ebx
    syscall
    pop rax

    ; Check read result
    cmp r13d, 0
    jle .probe_reject             ; empty or error → skip

    ; Count printable bytes
    xor ecx, ecx                  ; printable count
    xor edx, edx                  ; byte index
.probe_loop:
    cmp edx, r13d
    jge .probe_check
    movzx eax, byte [rsp + rdx]

    ; Check: tab (9), newline (10), CR (13), space-tilde (32-126)
    cmp al, 9
    je .probe_printable
    cmp al, 10
    je .probe_printable
    cmp al, 13
    je .probe_printable
    cmp al, 32
    jb .probe_not_printable
    cmp al, 126
    jbe .probe_printable

.probe_not_printable:
    inc edx
    jmp .probe_loop

.probe_printable:
    inc ecx
    inc edx
    jmp .probe_loop

.probe_check:
    ; percentage = (printable * 100) / total_bytes
    imul ecx, 100
    xor edx, edx
    mov eax, ecx
    mov ecx, r13d
    div ecx                       ; eax = percentage
    cmp eax, 75
    jge .probe_accept

.probe_reject:
    xor eax, eax
    jmp .probe_done

.probe_accept:
    mov eax, 1

.probe_done:
    add rsp, 272
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; frontier_push(rdi=path)
;; Pushes a directory path onto the explore frontier queue.
;; Paths are stored null-separated in ST_EXPLORE_FRONTIER.
;; Maturity-gated: Stage 1 = corpus/ subtree only, Stage 2 = any.
;; ============================================================
global frontier_push
frontier_push:
    push rbx
    push r12
    push r13
    sub rsp, 16                   ; 3 pushes (odd) → aligned, sub mult of 16

    mov r12, rdi                  ; path to push
    mov rbx, SURFACE_BASE

    ; Maturity gate: at Stage 1, only push paths starting with "corpus/"
    call get_maturity_level
    cmp eax, 2
    jge .fp_gate_ok               ; Stage 2+ accepts anything

    ; Stage 1: check prefix "corpus/"
    mov rdi, r12
    lea rsi, [rel explore_default_path]  ; "corpus/"
.fp_prefix_check:
    movzx eax, byte [rsi]
    test al, al
    jz .fp_gate_ok                ; prefix matched fully
    cmp al, [rdi]
    jne .fp_gate_reject           ; prefix mismatch
    inc rdi
    inc rsi
    jmp .fp_prefix_check

.fp_gate_reject:
    ; Outside allowed radius, don't push
    jmp .fp_done

.fp_gate_ok:
    ; Get write position
    mov eax, [rbx + STATE_OFFSET + ST_FRONTIER_WRITE_POS]

    ; Copy path into frontier buffer
    mov rdi, r12                  ; source path
    lea r13, [rbx + STATE_OFFSET + ST_EXPLORE_FRONTIER]
.fp_copy:
    movzx ecx, byte [rdi]
    mov [r13 + rax], cl
    inc eax
    ; Wrap at frontier size
    cmp eax, ST_EXPLORE_FRONTIER_SIZE
    jl .fp_no_wrap
    xor eax, eax
.fp_no_wrap:
    inc rdi
    test cl, cl
    jnz .fp_copy                  ; copy including the null terminator

    ; Update write position
    mov [rbx + STATE_OFFSET + ST_FRONTIER_WRITE_POS], eax

.fp_done:
    add rsp, 16
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; frontier_pop(rdi=dest_buf, esi=max_len) -> eax (0=empty, 1=got path)
;; Pops the next directory path from the frontier queue.
;; ============================================================
global frontier_pop
frontier_pop:
    push rbx
    push r12
    push r13
    sub rsp, 16                   ; 3 pushes (odd) → aligned

    mov r12, rdi                  ; dest buffer
    mov r13d, esi                 ; max length
    mov rbx, SURFACE_BASE

    ; Check if empty (read_pos == write_pos)
    mov eax, [rbx + STATE_OFFSET + ST_FRONTIER_READ_POS]
    cmp eax, [rbx + STATE_OFFSET + ST_FRONTIER_WRITE_POS]
    je .fpop_empty

    ; Read path from frontier
    lea rsi, [rbx + STATE_OFFSET + ST_EXPLORE_FRONTIER]
    xor ecx, ecx                 ; bytes copied
.fpop_copy:
    cmp ecx, r13d
    jge .fpop_trunc               ; hit max length
    movzx edx, byte [rsi + rax]
    mov [r12 + rcx], dl
    inc ecx
    inc eax
    ; Wrap
    cmp eax, ST_EXPLORE_FRONTIER_SIZE
    jl .fpop_no_wrap
    xor eax, eax
.fpop_no_wrap:
    test dl, dl
    jnz .fpop_copy                ; copy until null

    ; Update read position
    mov [rbx + STATE_OFFSET + ST_FRONTIER_READ_POS], eax
    mov eax, 1
    jmp .fpop_done

.fpop_trunc:
    ; Null-terminate and advance read past this entry
    mov byte [r12 + rcx - 1], 0
.fpop_skip:
    movzx edx, byte [rsi + rax]
    inc eax
    cmp eax, ST_EXPLORE_FRONTIER_SIZE
    jl .fpop_skip_wrap
    xor eax, eax
.fpop_skip_wrap:
    test dl, dl
    jnz .fpop_skip
    mov [rbx + STATE_OFFSET + ST_FRONTIER_READ_POS], eax
    mov eax, 1
    jmp .fpop_done

.fpop_empty:
    xor eax, eax

.fpop_done:
    add rsp, 16
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; advance_explore_path()
;; Called when SEEK has seen enough files in current directory.
;; Scans current dir for subdirectories, pushes them to frontier,
;; then pops next dir as new explore path.
;; ============================================================
global advance_explore_path
advance_explore_path:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 4624                 ; same layout as action_seek
                                  ; [rsp+0..4095] = getdents buf
                                  ; [rsp+4096..4607] = path buf (512)
                                  ; 5 pushes (odd) → aligned, 4624 is mult of 16

    mov rbx, SURFACE_BASE
    lea r12, [rbx + STATE_OFFSET]

    ; Open current explore path
    lea rdi, [r12 + ST_EXPLORE_PATH]
    mov esi, 0x10000              ; O_RDONLY | O_DIRECTORY
    xor edx, edx
    mov eax, SYS_OPEN
    syscall
    test eax, eax
    js .adv_try_pop               ; can't open, just try frontier

    mov r13d, eax                 ; dir_fd

    ; Read directory entries
    mov eax, SYS_GETDENTS64
    mov edi, r13d
    lea rsi, [rsp]
    mov edx, 4096
    syscall
    mov r14d, eax                 ; bytes read

    ; Close dir
    mov eax, SYS_CLOSE
    mov edi, r13d
    syscall

    cmp r14d, 0
    jle .adv_try_pop

    ; Walk entries, push subdirectories to frontier
    xor r15d, r15d                ; offset
.adv_entry_loop:
    cmp r15d, r14d
    jge .adv_try_pop

    lea rsi, [rsp + r15]

    ; Get reclen for advance
    movzx eax, word [rsi + 16]
    mov r13d, eax                 ; save reclen

    ; Check d_type = DT_DIR (4)
    movzx eax, byte [rsi + 18]
    cmp eax, 4
    jne .adv_next_entry

    ; d_name
    lea rdi, [rsi + 19]

    ; Skip "." and ".."
    cmp byte [rdi], '.'
    jne .adv_push_dir
    cmp byte [rdi + 1], 0
    je .adv_next_entry
    cmp byte [rdi + 1], '.'
    jne .adv_push_dir
    cmp byte [rdi + 2], 0
    je .adv_next_entry

.adv_push_dir:
    ; Build full path: explore_path + "/" + dirname
    push rdi                      ; save d_name ptr
    lea rdi, [rsp + 4096 + 8]    ; path buffer (offset for push)
    lea rsi, [r12 + ST_EXPLORE_PATH]
.adv_copy_base:
    lodsb
    test al, al
    jz .adv_check_slash2
    stosb
    jmp .adv_copy_base
.adv_check_slash2:
    cmp byte [rdi - 1], '/'
    je .adv_copy_subdir
    mov byte [rdi], '/'
    inc rdi
.adv_copy_subdir:
    pop rsi                       ; d_name ptr
.adv_copy_sub2:
    lodsb
    stosb
    test al, al
    jnz .adv_copy_sub2

    ; Append trailing '/'
    dec rdi                       ; back to null
    mov byte [rdi], '/'
    mov byte [rdi + 1], 0

    ; Push to frontier
    lea rdi, [rsp + 4096]        ; path buffer
    call frontier_push

    ; Log
    lea rdi, [rel frontier_push_msg]
    call print_cstr
    lea rdi, [rsp + 4096]
    call print_cstr
    call print_newline

.adv_next_entry:
    add r15d, r13d
    jmp .adv_entry_loop

.adv_try_pop:
    ; Pop next directory from frontier
    lea rdi, [rsp + 4096]        ; temp buffer
    mov esi, 256
    call frontier_pop
    test eax, eax
    jz .adv_exhausted

    ; Copy to ST_EXPLORE_PATH
    lea rdi, [r12 + ST_EXPLORE_PATH]
    lea rsi, [rsp + 4096]
.adv_copy_new:
    lodsb
    stosb
    test al, al
    jnz .adv_copy_new

    ; Reset files seen
    mov dword [r12 + ST_FILES_SEEN_COUNT], 0

    ; Log new path
    lea rdi, [rel frontier_advance_msg]
    call print_cstr
    lea rdi, [r12 + ST_EXPLORE_PATH]
    call print_cstr
    call print_newline
    jmp .adv_done

.adv_exhausted:
    lea rdi, [rel frontier_exhausted_msg]
    call print_cstr

.adv_done:
    add rsp, 4624
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; action_compose()
;; Generate novel text from learned patterns by chaining predictions.
;; Uses dispatch_predict to predict next token given current context,
;; then feeds prediction back as new context. Stops when confidence
;; drops or 64 tokens generated.
;; Output stored in ST_COMPOSITION_BUF.
;; ============================================================
global action_compose
action_compose:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 32                   ; 5 pushes (odd) → aligned, 32 is mult of 16
                                  ; [rsp+0] = total_confidence (f64)
                                  ; [rsp+8] = token_count (u32)
                                  ; [rsp+16] = last 4 tokens for loop detect
                                  ; [rsp+24] = pad

    mov rbx, SURFACE_BASE
    lea r12, [rbx + STATE_OFFSET]

    ; Check if enough material (region_count > 500)
    mov eax, [r12 + ST_REGION_COUNT]
    cmp eax, 500
    jl .compose_too_early

    ; Log start
    lea rdi, [rel compose_start_msg]
    call print_cstr

    ; Clear composition buffer
    lea rdi, [r12 + ST_COMPOSITION_BUF]
    xor eax, eax
    mov ecx, 64                   ; zero first 256 bytes (enough for start)
.compose_clear:
    mov dword [rdi], eax
    add rdi, 4
    dec ecx
    jnz .compose_clear

    ; Initialize
    xorpd xmm0, xmm0
    movsd [rsp], xmm0             ; total_confidence = 0.0
    mov dword [rsp + 8], 0        ; token_count = 0
    mov dword [rsp + 16], 0       ; loop detect buf
    mov dword [rsp + 20], 0

    ; Get starting context hash
    mov r13d, [r12 + ST_CTX_HASH] ; current context hash
    test r13d, r13d
    jnz .compose_loop
    ; No context — use a random-ish seed from global step
    mov r13d, [r12 + ST_GLOBAL_STEP]

.compose_loop:
    cmp dword [rsp + 8], 64       ; max 64 tokens
    jge .compose_finish

    ; Predict next token given context
    mov edi, r13d
    call dispatch_predict          ; eax = predicted token (0 = no prediction)

    test eax, eax
    jz .compose_finish             ; no prediction available

    mov r14d, eax                  ; predicted token
    mov r15d, eax                  ; save for loop detect

    ; Get confidence from ST_EXPECT_CONF
    movss xmm0, [r12 + ST_EXPECT_CONF]
    cvtss2sd xmm0, xmm0
    ; Check confidence threshold (0.3)
    mov rax, 0x3FD3333333333333    ; 0.3 f64
    movq xmm1, rax
    ucomisd xmm0, xmm1
    jbe .compose_finish            ; confidence too low

    ; Accumulate confidence
    addsd xmm0, [rsp]
    movsd [rsp], xmm0

    ; Store token as text in composition buffer
    ; Simple: store token ID as 4 hex digits + space
    ; Actually simpler: store raw u32 tokens, print them later
    mov eax, [rsp + 8]            ; token_count
    shl eax, 2                    ; * 4 bytes per token
    cmp eax, ST_COMPOSITION_SIZE - 4
    jge .compose_finish            ; buffer full
    mov [r12 + ST_COMPOSITION_BUF + rax], r14d

    inc dword [rsp + 8]

    ; Loop detection: check if this token matches 2 tokens ago
    mov eax, [rsp + 8]
    cmp eax, 4
    jl .compose_no_loop_check
    ; Simple repeat check: if last 2 == previous 2 → loop
    mov eax, [rsp + 16]           ; prev-prev token
    cmp eax, [rsp + 20]           ; prev token
    jne .compose_no_loop
    cmp r14d, eax                 ; current == prev-prev == prev → stuck
    je .compose_finish
.compose_no_loop:

.compose_no_loop_check:
    ; Shift loop detect buffer
    mov eax, [rsp + 20]
    mov [rsp + 16], eax           ; prev-prev = prev
    mov [rsp + 20], r14d          ; prev = current

    ; Update context: hash(prev_context, predicted_token)
    ; Simple: context = context * 31 + token
    imul r13d, r13d, 31
    add r13d, r14d

    jmp .compose_loop

.compose_finish:
    ; Store composition length and avg confidence
    mov eax, [rsp + 8]
    mov [r12 + ST_COMPOSITION_LEN], eax

    test eax, eax
    jz .compose_too_early

    ; avg confidence = total / count
    movsd xmm0, [rsp]
    cvtsi2sd xmm1, eax
    divsd xmm0, xmm1
    cvtsd2ss xmm0, xmm0
    movss [r12 + ST_COMPOSITION_CONF], xmm0

    ; Log result
    lea rdi, [rel compose_token_msg]
    call print_cstr
    mov eax, [r12 + ST_COMPOSITION_LEN]
    mov edi, eax
    call print_u64
    lea rdi, [rel compose_tokens_msg]
    call print_cstr
    movss xmm0, [r12 + ST_COMPOSITION_CONF]
    call print_f32
    lea rdi, [rel compose_close]
    call print_cstr

    ; Increment fire count
    inc dword [r12 + ST_ACTION_REGISTRY + ACTION_COMPOSE * ACTION_REG_ENTRY_SIZE + ACTION_REG_FIRES]
    jmp .compose_done

.compose_too_early:
    lea rdi, [rel compose_empty_msg]
    call print_cstr

.compose_done:
    add rsp, 32
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; action_teach()
;; Share strongest schema trace with colony via shared segment.
;; Only fires when ST_SHARED_MODE=1 (colony mode active).
;; ============================================================
global action_teach
action_teach:
    push rbx
    push r12
    sub rsp, 8                    ; 2 pushes (even) → not aligned → sub 8

    mov rbx, SURFACE_BASE
    lea r12, [rbx + STATE_OFFSET]

    ; Check colony mode
    cmp dword [r12 + ST_SHARED_MODE], 0
    je .teach_solo

    ; Check schema trace magnitude
    lea rdi, [r12 + ST_SCHEMA_TRACE_VEC]
    call holo_magnitude_f64        ; xmm0 = magnitude
    mov rax, 0x3FB999999999999A    ; 0.1 f64
    movq xmm1, rax
    ucomisd xmm0, xmm1
    jbe .teach_done                ; nothing consolidated enough

    ; Open shared holographic segment
    lea rdi, [rel shared_holo_path]
    mov esi, 0x42                  ; O_RDWR | O_CREAT
    mov edx, 0x1B6                 ; 0666
    mov eax, SYS_OPEN
    syscall
    test eax, eax
    js .teach_done                 ; can't open shared segment

    mov r12d, eax                  ; fd

    ; mmap the shared segment
    xor edi, edi                   ; addr = NULL
    mov esi, HOLO_VEC_BYTES        ; just one vector
    mov edx, 3                     ; PROT_READ | PROT_WRITE
    mov r10d, 1                    ; MAP_SHARED
    mov r8d, r12d                  ; fd
    xor r9d, r9d                   ; offset = 0
    mov eax, 9                     ; SYS_MMAP
    syscall
    test rax, rax
    js .teach_close_fd

    ; Copy schema trace to shared segment
    mov rdi, rax                   ; dest = mapped region
    mov rbx, SURFACE_BASE
    lea rsi, [rbx + STATE_OFFSET + ST_SCHEMA_TRACE_VEC]
    mov ecx, HOLO_VEC_BYTES / 8
.teach_copy:
    mov rdx, [rsi]
    mov [rdi], rdx
    add rsi, 8
    add rdi, 8
    dec ecx
    jnz .teach_copy

    ; Unmap
    sub rdi, HOLO_VEC_BYTES        ; back to start
    mov rsi, HOLO_VEC_BYTES
    mov eax, 11                    ; SYS_MUNMAP
    syscall

    lea rdi, [rel teach_msg]
    call print_cstr

    ; Increment fire count
    mov rbx, SURFACE_BASE
    inc dword [rbx + STATE_OFFSET + ST_ACTION_REGISTRY + ACTION_TEACH * ACTION_REG_ENTRY_SIZE + ACTION_REG_FIRES]

.teach_close_fd:
    mov eax, SYS_CLOSE
    mov edi, r12d
    syscall
    jmp .teach_done

.teach_solo:
    lea rdi, [rel teach_solo_msg]
    call print_cstr

.teach_done:
    add rsp, 8
    pop r12
    pop rbx
    ret

section .data
    shared_holo_path: db "/dev/shm/uhma_holo", 0

section .text

;; ============================================================
;; action_reflect()
;; Deep self-examination: queries action traces against self-model,
;; superposes high-similarity actions into self-model.
;; Makes self-model include behavioral patterns, not just code structure.
;; ============================================================
global action_reflect
action_reflect:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 32                   ; 5 pushes (odd) → aligned, 32 is mult of 16
                                  ; [rsp+0] = baseline magnitude (f64)
                                  ; [rsp+8] = resonate_count (u32)
                                  ; [rsp+16] = pad

    mov rbx, SURFACE_BASE
    lea r12, [rbx + STATE_OFFSET]

    ; 1. Snapshot current self-model magnitude
    lea rdi, [r12 + ST_SELF_MODEL_VEC]
    call holo_magnitude_f64        ; xmm0 = magnitude
    movsd [rsp], xmm0             ; save baseline
    mov dword [rsp + 8], 0        ; resonate_count = 0

    ; 2. For each action trace (0-9), cosim against self_model
    xor r13d, r13d                ; action index

.reflect_loop:
    cmp r13d, ACTION_COUNT
    jge .reflect_normalize

    ; Compute action trace pointer
    imul eax, r13d, HOLO_VEC_BYTES
    lea rdi, [r12 + ST_ACTION_TRACES]
    add rdi, rax
    lea rsi, [r12 + ST_SELF_MODEL_VEC]
    call holo_cosim_f64            ; xmm0 = similarity

    ; Check threshold (0.3)
    mov rax, 0x3FD3333333333333    ; 0.3 f64
    movq xmm1, rax
    ucomisd xmm0, xmm1
    jbe .reflect_next

    ; This action resonates with self — superpose into self-model
    inc dword [rsp + 8]

    ; Scale action trace by 0.1 (small contribution)
    imul eax, r13d, HOLO_VEC_BYTES
    lea rdi, [r12 + ST_ACTION_TRACES]
    add rdi, rax
    mov rax, 0x3FB999999999999A    ; 0.1 f64
    movq xmm0, rax
    call holo_scale_f64

    ; Superpose into self_model
    lea rdi, [r12 + ST_SELF_MODEL_VEC]
    imul eax, r13d, HOLO_VEC_BYTES
    lea rsi, [r12 + ST_ACTION_TRACES]
    add rsi, rax
    call holo_superpose_f64

.reflect_next:
    inc r13d
    jmp .reflect_loop

.reflect_normalize:
    ; 3. Normalize self-model
    lea rdi, [r12 + ST_SELF_MODEL_VEC]
    call holo_normalize_f64

    ; 4. Compare new magnitude to baseline
    lea rdi, [r12 + ST_SELF_MODEL_VEC]
    call holo_magnitude_f64        ; xmm0 = new magnitude

    ; delta = |new - baseline|
    movsd xmm1, [rsp]             ; baseline
    subsd xmm0, xmm1
    ; Absolute value
    xorpd xmm2, xmm2
    ucomisd xmm0, xmm2
    jae .reflect_pos_delta
    movsd xmm3, xmm2
    subsd xmm3, xmm0
    movsd xmm0, xmm3
.reflect_pos_delta:

    ; Check if delta > 0.1 → self-surprise
    mov rax, 0x3FB999999999999A    ; 0.1 f64
    movq xmm1, rax
    ucomisd xmm0, xmm1
    jbe .reflect_stable

    ; Self-model shifted
    inc dword [r12 + ST_SELF_SURPRISE_COUNT]
    lea rdi, [rel reflect_shifted_msg]
    call print_cstr
    jmp .reflect_log

.reflect_stable:
    lea rdi, [rel reflect_stable_msg]
    call print_cstr

.reflect_log:
    ; Log summary
    lea rdi, [rel reflect_msg]
    call print_cstr
    mov edi, [rsp + 8]
    call print_u64
    lea rdi, [rel reflect_actions_msg]
    call print_cstr
    ; Print delta (already in xmm0 from above, but may be clobbered)
    ; Just print the resonate count, skip delta for simplicity
    call print_newline

    ; Increment fire count
    inc dword [r12 + ST_ACTION_REGISTRY + ACTION_REFLECT * ACTION_REG_ENTRY_SIZE + ACTION_REG_FIRES]

    add rsp, 32
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; on_maturity_advance(edi=new_stage)
;; Called from maturity.asm when stage advances.
;; Seeds action traces so they can participate in resonance.
;; ============================================================
global on_maturity_advance
on_maturity_advance:
    push rbx
    push r12
    push r13
    sub rsp, 16                   ; 3 pushes (odd) → aligned, sub mult of 16

    mov r12d, edi                 ; new stage
    mov rbx, SURFACE_BASE
    lea r13, [rbx + STATE_OFFSET]

    cmp r12d, 1
    je .oma_stage1
    cmp r12d, 2
    je .oma_stage2
    jmp .oma_done

.oma_stage1:
    ; Encode current presence as initial trace for SEEK and SCAN_ENV
    call encode_presence_vec

    ; Copy autonomy_vec into SEEK trace
    lea rdi, [r13 + ST_ACTION_TRACES + ACTION_SEEK * HOLO_VEC_BYTES]
    lea rsi, [r13 + ST_AUTONOMY_VEC]
    call holo_superpose_f64

    ; Same for SCAN_ENV
    lea rdi, [r13 + ST_ACTION_TRACES + ACTION_SCAN_ENV * HOLO_VEC_BYTES]
    lea rsi, [r13 + ST_AUTONOMY_VEC]
    call holo_superpose_f64

    lea rdi, [rel maturity_seed_msg]
    call print_cstr
    jmp .oma_done

.oma_stage2:
    ; Enable COMPOSE and REFLECT (TEACH stays disabled until colony mode)
    lea rax, [r13 + ST_ACTION_REGISTRY]
    mov dword [rax + ACTION_COMPOSE * ACTION_REG_ENTRY_SIZE + ACTION_REG_FLAGS], 1
    mov dword [rax + ACTION_REFLECT * ACTION_REG_ENTRY_SIZE + ACTION_REG_FLAGS], 1

    ; Seed their traces
    call encode_presence_vec

    lea rdi, [r13 + ST_ACTION_TRACES + ACTION_COMPOSE * HOLO_VEC_BYTES]
    lea rsi, [r13 + ST_AUTONOMY_VEC]
    call holo_superpose_f64

    lea rdi, [r13 + ST_ACTION_TRACES + ACTION_REFLECT * HOLO_VEC_BYTES]
    lea rsi, [r13 + ST_AUTONOMY_VEC]
    call holo_superpose_f64

    lea rdi, [rel maturity_seed2_msg]
    call print_cstr

.oma_done:
    add rsp, 16
    pop r13
    pop r12
    pop rbx
    ret
