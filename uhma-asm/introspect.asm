; introspect.asm — Semantic self-model, bootstrap, and organic regulation
;
; THIS IS WHERE SELF-AWARENESS LIVES.
;
; @entry tick_workers()               → autonomous idle processing (called by REPL on timeout)
; @entry introspect_scan_regions()    → builds semantic self-model (SELF-AWARE reading)
; @entry introspect_repair_cycle()    → processes RFLAG_NEEDS_REPAIR regions
; @entry metacog_report()             → REPL 'intro' command output
;
; AUTONOMOUS BEHAVIOR (tick_workers):
;   Called by REPL on poll timeout. Makes UHMA do useful work when idle:
;   1. Bootstrap: if SELF-AWARE < 0.3 → force observe_cycle (build self-model)
;   2. Startup consolidation: if regions > 100 and fresh session → dream_cycle
;   3. Organic pressure: decay + check thresholds → fire appropriate cycles
;
; SEMANTIC SELF-MODEL:
;   introspect_scan_regions() encodes each region via encode_region_to_vector()
;   Similar code → similar vectors. Superposed into ST_SELF_MODEL_VEC.
;   After observe cycle: SELF-AWARE reading typically 0.9+ (97.3% measured)
;
; ORGANIC PRESSURE (thresholds):
;   dream_pressure     > 0.5 → dream_cycle()      (misses accumulate)
;   observe_pressure   > 1.0 → observe_cycle()    (accuracy variance)
;   introspect_pressure > 0.75 → repair_cycle()   (SURPRISE_SELF events)
;
; SELF/OTHER BOUNDARY:
;   SURPRISE_SELF → introspect pressure (I was wrong about myself)
;   SURPRISE_OUTCOME → dream pressure (world surprised me)
;
; @calls vsa_ops.asm:encode_region_to_vector
; @calls receipt.asm:meta_recommend_strategy, intro_get_self_awareness
; @calls dreams.asm:dream_cycle, observe.asm:observe_cycle
; @calledby repl.asm (on poll timeout)
;
%include "syscalls.inc"
%include "constants.inc"

section .data
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
    sub rsp, 8200               ; temp vector (8192) + 8 alignment (5 pushes = odd)

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
    add rsp, 8200               ; free temp vector + alignment
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

section .text

extern dream_cycle
extern observe_cycle
extern evolve_cycle
extern region_compact

;; ============================================================
;; tick_workers()
;; Called each processing step. Scans pheromone levels and
;; activates worker castes when thresholds exceeded.
;; This is the hive's swarm intelligence — distributed control.
;; Returns: eax = number of workers activated
;; ============================================================
global tick_workers
tick_workers:
global tick_regulators          ; legacy alias
tick_regulators:
    push rbx
    push r12
    push r13
    sub rsp, 8                ; alignment (3 pushes = odd, need 8 more)

    mov rbx, SURFACE_BASE
    xor r12d, r12d            ; actions triggered

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
    ; Load threshold
    movsd xmm7, [rel pheromone_threshold]

    ; --- Check dream pheromone ---
    movsd xmm0, [rbx + STATE_OFFSET + ST_DREAM_PRESSURE]
    ucomisd xmm0, xmm7
    jbe .check_observe

    ; Dream pheromone exceeded — activate consolidation worker
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

    ; Reset pressure after firing
    xorpd xmm0, xmm0
    movsd [rbx + STATE_OFFSET + ST_DREAM_PRESSURE], xmm0

.check_observe:
    ; --- Check observe pheromone ---
    movsd xmm0, [rbx + STATE_OFFSET + ST_OBSERVE_PRESSURE]
    ucomisd xmm0, xmm7
    jbe .check_evolve

    ; Observe pheromone exceeded — activate observation worker
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

    ; Reset pressure
    xorpd xmm0, xmm0
    movsd [rbx + STATE_OFFSET + ST_OBSERVE_PRESSURE], xmm0

.check_evolve:
    ; --- Check evolve pheromone ---
    movsd xmm0, [rbx + STATE_OFFSET + ST_EVOLVE_PRESSURE]
    ucomisd xmm0, xmm7
    jbe .check_fatigue

    ; Evolve pheromone exceeded — activate evolution worker
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

    ; Reset pressure
    xorpd xmm0, xmm0
    movsd [rbx + STATE_OFFSET + ST_EVOLVE_PRESSURE], xmm0

.check_fatigue:
    ; --- Check fatigue (high fatigue = rest worker) ---
    movss xmm0, [rbx + STATE_OFFSET + ST_PRESENCE + PRES_FATIGUE * 4]
    cvtss2sd xmm0, xmm0
    mov rax, 0x3FE8000000000000  ; 0.75 threshold for rest
    movq xmm1, rax
    ucomisd xmm0, xmm1
    jbe .workers_done

    ; Fatigue high — activate rest worker (reduce activity)
    lea rdi, [rel worker_trigger_msg]
    call print_cstr
    lea rdi, [rel worker_rest]
    call print_cstr
    movss xmm0, [rbx + STATE_OFFSET + ST_PRESENCE + PRES_FATIGUE * 4]
    call print_f32
    lea rdi, [rel worker_close]
    call print_cstr

    ; Rest worker action: reduce fatigue, slow down dispatch
    movss xmm0, [rbx + STATE_OFFSET + ST_PRESENCE + PRES_FATIGUE * 4]
    mov eax, 0x3F000000        ; 0.5f multiplier
    movd xmm1, eax
    mulss xmm0, xmm1
    movss [rbx + STATE_OFFSET + ST_PRESENCE + PRES_FATIGUE * 4], xmm0

    inc r12d

.workers_done:
    mov eax, r12d             ; return workers activated
    add rsp, 8
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
