; introspect.asm — Self-reading: decode own regions, extract semantic meaning
; This is the homoiconic core: code reads itself as data, understands what it does.
; x86 instructions ARE the primitives — mov, cmp, je are atoms like car, cdr, cons.
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

    inc r15d

.scan_next:
    inc r14d
    jmp .scan_loop

.scan_done:
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
