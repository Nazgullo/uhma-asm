; presence.asm — Hormonal Modulator System (Presence Hyper-Regions)
; Creates regions that check state configurations and trigger system-wide changes.
; These are "emotional reflexes" that evolve to manage system parameters.
; The Presence system becomes the steering wheel, not just a dashboard.
%include "syscalls.inc"
%include "constants.inc"

section .data
    pres_trig_msg:      db "[PRESENCE] Hormonal trigger: ", 0
    pres_panic_msg:     db "PANIC_MODE", 0
    pres_contemp_msg:   db "CONTEMPLATIVE_MODE", 0
    pres_fatigue_msg:   db "FATIGUE_RESPONSE", 0
    pres_vigor_msg:     db "VIGOR_BOOST", 0
    pres_crisis_msg:    db "CRISIS_INTERVENTION", 0
    pres_stable_msg:    db "STABILITY_RESTORED", 0
    pres_newline:       db 10, 0

    ; Thresholds for built-in presence checks
    align 8
    high_entropy_thresh:    dq 0.8      ; entropy > 0.8 = chaotic
    low_entropy_thresh:     dq 0.3      ; entropy < 0.3 = stable
    low_energy_thresh:      dq 20.0     ; energy < 20 = exhausted
    high_energy_thresh:     dq 80.0     ; energy > 80 = vigorous
    high_arousal_thresh:    dd 0.7      ; arousal > 0.7 = excited/panicked (f32)
    low_arousal_thresh:     dd 0.3      ; arousal < 0.3 = calm
    high_fatigue_thresh:    dd 0.7      ; fatigue > 0.7 = tired
    low_accuracy_thresh:    dd 0.3      ; accuracy < 0.3 = struggling

section .text

extern print_cstr
extern print_newline
extern region_alloc
extern dream_cycle
extern observe_cycle

;; ============================================================
;; presence_init
;; Initialize the presence-as-action system.
;; Creates initial hormonal regions for basic survival reflexes.
;; ============================================================
global presence_init
presence_init:
    push rbx

    ; Create the fundamental presence regions (hardwired reflexes)
    ; These are the system's innate emotional responses

    ; 1. PANIC MODE: High entropy + low energy → force fast/prune
    call emit_panic_region

    ; 2. CONTEMPLATIVE MODE: Low entropy + stable → allow exploration
    call emit_contemplative_region

    ; 3. FATIGUE RESPONSE: High fatigue → trigger dream, slow down
    call emit_fatigue_region

    pop rbx
    ret

;; ============================================================
;; emit_panic_region
;; Creates a presence region for panic response:
;; IF entropy > 0.8 AND energy < 20 THEN set DMODE_FAST, boost prune
;; ============================================================
emit_panic_region:
    push rbx
    push r12

    ; Allocate presence region (32 bytes of code)
    mov rdi, 32               ; code size
    mov rsi, RTYPE_PRESENCE
    xor edx, edx              ; birth = 0
    call region_alloc
    mov r12, rax              ; region header

    ; Write code that checks state and sets parameters
    ; Code layout:
    ;   [0-7]:   Check entropy > 0.8
    ;   [8-15]:  Check energy < 20
    ;   [16-23]: Set dispatch mode = FAST
    ;   [24-31]: Boost prune threshold, RET

    lea rbx, [r12 + RHDR_SIZE]

    ; mov rax, SURFACE_BASE + STATE_OFFSET + ST_PRESENCE + PRES_ENTROPY*4
    ; This would be complex in pure x86... use a simpler sentinel approach:
    ; We'll mark this region with magic bytes that trigger_presence interprets
    ; Format: [condition_type:u8][threshold_high:f32][effect_type:u8][effect_value:u32][RET]

    ; Condition: PCOND_ENTROPY_GT
    mov byte [rbx + 0], PCOND_ENTROPY_GT
    ; Threshold: 0.8 (as f32)
    mov eax, 0x3F4CCCCD          ; 0.8f
    mov [rbx + 1], eax
    ; Second condition: PCOND_ENERGY_LT
    mov byte [rbx + 5], PCOND_ENERGY_LT
    ; Threshold: 20.0 (as f32)
    mov eax, 0x41A00000          ; 20.0f
    mov [rbx + 6], eax
    ; Effect: PEFF_SET_DISPATCH_MODE
    mov byte [rbx + 10], PEFF_SET_DISPATCH_MODE
    ; Value: DMODE_FAST
    mov dword [rbx + 11], DMODE_FAST
    ; End marker
    mov byte [rbx + 15], 0xFF    ; end of effects
    ; Pad rest with RET (0xC3)
    mov ecx, 16
.pad_panic:
    cmp ecx, 32
    jge .panic_done
    mov byte [rbx + rcx], 0xC3
    inc ecx
    jmp .pad_panic

.panic_done:
    mov rax, r12
    pop r12
    pop rbx
    ret

;; ============================================================
;; emit_contemplative_region
;; Creates a presence region for contemplative/exploratory state:
;; IF entropy < 0.3 AND energy > 50 THEN set DMODE_EXPLORE
;; ============================================================
emit_contemplative_region:
    push rbx
    push r12

    mov rdi, 32
    mov rsi, RTYPE_PRESENCE
    xor edx, edx
    call region_alloc
    mov r12, rax

    lea rbx, [r12 + RHDR_SIZE]

    ; Condition: PCOND_ENTROPY_LT with threshold 0.3
    mov byte [rbx + 0], PCOND_ENTROPY_LT
    mov eax, 0x3E99999A          ; 0.3f
    mov [rbx + 1], eax
    ; Second condition: PCOND_ENERGY_GT with threshold 50
    mov byte [rbx + 5], PCOND_ENERGY_GT
    mov eax, 0x42480000          ; 50.0f
    mov [rbx + 6], eax
    ; Effect: PEFF_SET_DISPATCH_MODE = EXPLORE
    mov byte [rbx + 10], PEFF_SET_DISPATCH_MODE
    mov dword [rbx + 11], DMODE_EXPLORE
    mov byte [rbx + 15], 0xFF
    ; Pad with RET
    mov ecx, 16
.pad_contemp:
    cmp ecx, 32
    jge .contemp_done
    mov byte [rbx + rcx], 0xC3
    inc ecx
    jmp .pad_contemp

.contemp_done:
    mov rax, r12
    pop r12
    pop rbx
    ret

;; ============================================================
;; emit_fatigue_region
;; Creates a presence region for fatigue response:
;; IF fatigue > 0.7 THEN trigger dream cycle
;; ============================================================
emit_fatigue_region:
    push rbx
    push r12

    mov rdi, 32
    mov rsi, RTYPE_PRESENCE
    xor edx, edx
    call region_alloc
    mov r12, rax

    lea rbx, [r12 + RHDR_SIZE]

    ; Condition: PCOND_FATIGUE_GT with threshold 0.7
    mov byte [rbx + 0], PCOND_FATIGUE_GT
    mov eax, 0x3F333333          ; 0.7f
    mov [rbx + 1], eax
    ; No second condition (mark as unused)
    mov byte [rbx + 5], 0xFF
    ; Effect: PEFF_TRIGGER_DREAM
    mov byte [rbx + 10], PEFF_TRIGGER_DREAM
    mov dword [rbx + 11], 1      ; value doesn't matter
    mov byte [rbx + 15], 0xFF
    ; Pad with RET
    mov ecx, 16
.pad_fatigue:
    cmp ecx, 32
    jge .fatigue_done
    mov byte [rbx + rcx], 0xC3
    inc ecx
    jmp .pad_fatigue

.fatigue_done:
    mov rax, r12
    pop r12
    pop rbx
    ret

;; ============================================================
;; trigger_presence_regions
;; Scans all RTYPE_PRESENCE regions and evaluates their conditions.
;; If conditions match current state, applies their effects.
;; Called periodically from observe_cycle.
;; ============================================================
global trigger_presence_regions
trigger_presence_regions:
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, SURFACE_BASE
    lea r12, [rbx + REGION_TABLE_OFFSET]
    mov r13d, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    xor r14d, r14d              ; region index
    xor r15d, r15d              ; triggered count

.scan_presence:
    cmp r14d, r13d
    jge .scan_done

    ; Get region entry
    imul eax, r14d, RTE_SIZE
    lea rsi, [r12 + rax]

    ; Check if RTYPE_PRESENCE
    movzx eax, word [rsi + RTE_TYPE]
    cmp eax, RTYPE_PRESENCE
    jne .scan_next

    ; Check if active
    movzx eax, word [rsi + RTE_FLAGS]
    test eax, RFLAG_ACTIVE
    jz .scan_next
    test eax, RFLAG_CONDEMNED
    jnz .scan_next

    ; Get region code
    mov rdi, [rsi + RTE_ADDR]
    test rdi, rdi
    jz .scan_next

    ; Evaluate this presence region
    push rsi
    push r14
    call evaluate_presence_region
    pop r14
    pop rsi

    ; If returned true (eax != 0), increment triggered count
    test eax, eax
    jz .scan_next
    inc r15d
    ; Update hit count
    inc dword [rsi + RTE_HITS]

.scan_next:
    inc r14d
    jmp .scan_presence

.scan_done:
    mov eax, r15d               ; return triggered count
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; evaluate_presence_region(region_header)
;; rdi = region header pointer
;; Returns: eax = 1 if triggered, 0 if conditions not met
;; ============================================================
evaluate_presence_region:
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov r12, rdi                ; region header
    lea r13, [r12 + RHDR_SIZE]  ; code start
    mov rbx, SURFACE_BASE
    xor r15d, r15d              ; conditions_met = 0

    ; Read first condition
    movzx eax, byte [r13 + 0]
    cmp eax, 0xFF               ; no condition = always true
    je .all_conditions_met
    mov r14d, eax               ; condition type

    ; Get threshold (f32 at offset 1)
    movss xmm1, [r13 + 1]

    ; Evaluate condition based on type
    call evaluate_condition
    test eax, eax
    jz .condition_failed

    ; Check second condition (offset 5)
    movzx eax, byte [r13 + 5]
    cmp eax, 0xFF               ; no second condition
    je .all_conditions_met
    mov r14d, eax

    movss xmm1, [r13 + 6]
    call evaluate_condition
    test eax, eax
    jz .condition_failed

.all_conditions_met:
    ; Apply effects
    movzx eax, byte [r13 + 10]
    cmp eax, 0xFF
    je .eval_done_true

    mov r14d, eax               ; effect type
    mov r15d, [r13 + 11]        ; effect value

    call apply_effect

.eval_done_true:
    mov eax, 1
    jmp .eval_done

.condition_failed:
    xor eax, eax

.eval_done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; evaluate_condition(type in r14d, threshold in xmm1)
;; Returns: eax = 1 if condition met, 0 otherwise
;; ============================================================
evaluate_condition:
    push rbx
    mov rbx, SURFACE_BASE

    cmp r14d, PCOND_ENTROPY_GT
    je .check_entropy_gt
    cmp r14d, PCOND_ENTROPY_LT
    je .check_entropy_lt
    cmp r14d, PCOND_ENERGY_GT
    je .check_energy_gt
    cmp r14d, PCOND_ENERGY_LT
    je .check_energy_lt
    cmp r14d, PCOND_AROUSAL_GT
    je .check_arousal_gt
    cmp r14d, PCOND_AROUSAL_LT
    je .check_arousal_lt
    cmp r14d, PCOND_FATIGUE_GT
    je .check_fatigue_gt
    cmp r14d, PCOND_ACCURACY_LT
    je .check_accuracy_lt

    ; Unknown condition type - return false
    xor eax, eax
    jmp .cond_done

.check_entropy_gt:
    movss xmm0, [rbx + STATE_OFFSET + ST_PRESENCE + PRES_ENTROPY * 4]
    ucomiss xmm0, xmm1
    ja .cond_true
    jmp .cond_false

.check_entropy_lt:
    movss xmm0, [rbx + STATE_OFFSET + ST_PRESENCE + PRES_ENTROPY * 4]
    ucomiss xmm0, xmm1
    jb .cond_true
    jmp .cond_false

.check_energy_gt:
    movsd xmm0, [rbx + STATE_OFFSET + ST_ENERGY]
    cvtss2sd xmm2, xmm1         ; threshold to f64
    ucomisd xmm0, xmm2
    ja .cond_true
    jmp .cond_false

.check_energy_lt:
    movsd xmm0, [rbx + STATE_OFFSET + ST_ENERGY]
    cvtss2sd xmm2, xmm1
    ucomisd xmm0, xmm2
    jb .cond_true
    jmp .cond_false

.check_arousal_gt:
    movss xmm0, [rbx + STATE_OFFSET + ST_PRESENCE + PRES_AROUSAL * 4]
    ucomiss xmm0, xmm1
    ja .cond_true
    jmp .cond_false

.check_arousal_lt:
    movss xmm0, [rbx + STATE_OFFSET + ST_PRESENCE + PRES_AROUSAL * 4]
    ucomiss xmm0, xmm1
    jb .cond_true
    jmp .cond_false

.check_fatigue_gt:
    movss xmm0, [rbx + STATE_OFFSET + ST_PRESENCE + PRES_FATIGUE * 4]
    ucomiss xmm0, xmm1
    ja .cond_true
    jmp .cond_false

.check_accuracy_lt:
    ; Use self-prediction accuracy from state
    mov eax, [rbx + STATE_OFFSET + ST_SELF_PRED_HITS]
    mov edx, [rbx + STATE_OFFSET + ST_SELF_PRED_MISSES]
    add edx, eax
    test edx, edx
    jz .cond_false              ; no data
    cvtsi2ss xmm0, eax
    cvtsi2ss xmm2, edx
    divss xmm0, xmm2            ; accuracy
    ucomiss xmm0, xmm1
    jb .cond_true
    jmp .cond_false

.cond_true:
    mov eax, 1
    jmp .cond_done

.cond_false:
    xor eax, eax

.cond_done:
    pop rbx
    ret

;; ============================================================
;; apply_effect(type in r14d, value in r15d)
;; Applies a hormonal effect to system parameters
;; ============================================================
apply_effect:
    push rbx
    mov rbx, SURFACE_BASE

    cmp r14d, PEFF_SET_DISPATCH_MODE
    je .eff_dispatch_mode
    cmp r14d, PEFF_TRIGGER_DREAM
    je .eff_trigger_dream
    cmp r14d, PEFF_TRIGGER_OBSERVE
    je .eff_trigger_observe
    jmp .eff_done

.eff_dispatch_mode:
    mov [rbx + STATE_OFFSET + ST_DISPATCH_MODE], r15d
    jmp .eff_done

.eff_trigger_dream:
    call dream_cycle
    jmp .eff_done

.eff_trigger_observe:
    call observe_cycle
    jmp .eff_done

.eff_done:
    pop rbx
    ret

;; ============================================================
;; presence_regions_show
;; REPL command to display presence region status
;; ============================================================
global presence_regions_show
presence_regions_show:
    push rbx
    push r12
    push r13

    mov rbx, SURFACE_BASE

    ; Print header
    lea rdi, [rel pres_trig_msg]
    call print_cstr

    ; Count presence regions
    lea r12, [rbx + REGION_TABLE_OFFSET]
    mov r13d, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    xor ecx, ecx
    xor edx, edx                ; presence count

.count_loop:
    cmp ecx, r13d
    jge .count_done
    push rcx
    imul eax, ecx, RTE_SIZE
    movzx eax, word [r12 + rax + RTE_TYPE]
    cmp eax, RTYPE_PRESENCE
    jne .count_next
    inc edx
.count_next:
    pop rcx
    inc ecx
    jmp .count_loop

.count_done:
    ; Print count
    push rdx
    lea rdi, [rel pres_newline]
    call print_cstr
    pop rdx

    pop r13
    pop r12
    pop rbx
    ret
