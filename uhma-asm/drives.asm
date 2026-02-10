; drives.asm — Homeostatic drive system: thresholds, goals, actions
;
; @entry drives_check() -> void ; check drives vs thresholds, trigger actions
; @entry drives_show() -> void ; display current drive levels (REPL "drives")
; @entry drives_set_threshold(edi=id, xmm0=val) -> void
;
; @calledby introspect.asm:update_organic_pressure
; @calledby repl.asm:cmd_drives
;
; DRIVES (f32 values in ST_DRIVES[]):
;   [0] Accuracy   - prediction hit rate → GOAL_EXPLORATION when low
;   [1] Efficiency - resource usage → GOAL_EFFICIENCY, region_compact()
;   [2] Novelty    - pattern diversity → GOAL_NOVELTY, rotate context
;   [3] Coherence  - graph/holo agreement → GOAL_COHERENCE, modify_restructure()
;
; GOTCHAS:
;   - Drive values are f32, thresholds in ST_DRIVE_THRESHOLDS[]
;   - Actions only fire when drive < threshold
;   - Goals set in ST_CURRENT_GOAL (single active goal)
;
%include "syscalls.inc"
%include "constants.inc"

section .data
    drive_hdr:      db "--- Drives ---", 10, 0
    drive_acc_lbl:  db "  Accuracy:   ", 0
    drive_eff_lbl:  db "  Efficiency: ", 0
    drive_nov_lbl:  db "  Novelty:    ", 0
    drive_coh_lbl:  db "  Coherence:  ", 0
    drive_thresh:   db " (thresh=", 0
    drive_close:    db ")", 10, 0
    drive_trigger:  db "[DRIVE] ", 0
    drive_acc_trig: db "Accuracy low — increasing exploration", 10, 0
    drive_eff_trig: db "Efficiency low — triggering pruning", 10, 0
    drive_nov_trig: db "Novelty low — forcing context rotation", 10, 0
    drive_coh_trig: db "Coherence low — triggering alignment", 10, 0

    ; Born rule drive labels
    drive_born_msg: db "[DRIVE-Q] Born rule selected: ", 0
    drive_born_acc: db "ACCURACY (explore)", 10, 0
    drive_born_eff: db "EFFICIENCY (prune)", 10, 0
    drive_born_nov: db "NOVELTY (restructure)", 10, 0
    drive_born_coh: db "COHERENCE (align)", 10, 0
    drive_entropy_msg: db "[DRIVE-Q] Entropy: ", 0

    align 8
    drive_pressure_floor: dq 0.1
    drive_one_f64:    dq 1.0

section .text

extern print_cstr
extern print_f32
extern print_f64
extern print_newline
extern fire_hook
extern region_compact
extern modify_restructure
extern qdec_set_amps
extern qdec_measure
extern qdec_entropy
extern qdec_collapse

;; ============================================================
;; drives_check
;; Check all drive levels against thresholds, trigger actions
;; Called periodically from the observation loop or REPL
;; ============================================================
global drives_check
drives_check:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 40                ; 4 pushes = 32 bytes offset, sub 40 → 72 total (16n+8, aligned)
                               ; [rsp+0..31] = 4 × f64 pressure array

    mov rbx, SURFACE_BASE
    lea r12, [rbx + STATE_OFFSET]

    ; Clear goal at start (will be set by winning drive)
    mov dword [r12 + ST_CURRENT_GOAL], GOAL_NONE

    ; === BORN RULE DRIVE MEASUREMENT (quantum circuit) ===
    ; Compute pressure for each drive: how far below threshold = how urgent
    ; pressure[i] = max(threshold[i] - drive[i], floor)
    ; For efficiency (inverted): pressure = max(drive - threshold, floor)

    ; Load persistent pressures from state (accumulate across calls)
    ; If first time (all zero), initialize from drive deltas
    movsd xmm0, [r12 + ST_DRIVE_PRESSURES + 0]     ; pressure[0] (f64)
    movsd xmm1, [r12 + ST_DRIVE_PRESSURES + 8]
    movsd xmm2, [r12 + ST_DRIVE_PRESSURES + 16]
    movsd xmm3, [r12 + ST_DRIVE_PRESSURES + 24]

    ; Accumulate new pressure from current drive deficits
    ; Accuracy pressure: threshold - drive (low accuracy = high pressure)
    movss xmm4, [r12 + ST_DRIVE_THRESH + 0]
    cvtss2sd xmm4, xmm4
    movss xmm5, [r12 + ST_DRIVES + 0]
    cvtss2sd xmm5, xmm5
    subsd xmm4, xmm5           ; deficit
    maxsd xmm4, [rel drive_pressure_floor]
    ; QTHM entropy contribution: high entropy → more explore pressure
    ; entropy * 0.5 adds to accuracy pressure (many candidates = uncertain)
    movsd xmm5, [r12 + ST_QTHM_ENTROPY]
    mov rax, 0x3FE0000000000000  ; 0.5 f64
    movq xmm6, rax
    mulsd xmm5, xmm6            ; entropy * 0.5
    addsd xmm4, xmm5            ; deficit += entropy contribution
    addsd xmm0, xmm4

    ; Efficiency pressure: drive - threshold (high usage = high pressure)
    movss xmm4, [r12 + ST_DRIVES + 4]
    cvtss2sd xmm4, xmm4
    movss xmm5, [r12 + ST_DRIVE_THRESH + 4]
    cvtss2sd xmm5, xmm5
    subsd xmm4, xmm5
    maxsd xmm4, [rel drive_pressure_floor]
    addsd xmm1, xmm4

    ; Novelty pressure: drive - threshold (high repetition = high pressure)
    movss xmm4, [r12 + ST_DRIVES + 8]
    cvtss2sd xmm4, xmm4
    movss xmm5, [r12 + ST_DRIVE_THRESH + 8]
    cvtss2sd xmm5, xmm5
    subsd xmm4, xmm5
    maxsd xmm4, [rel drive_pressure_floor]
    addsd xmm2, xmm4

    ; Coherence pressure: threshold - drive (low coherence = high pressure)
    movss xmm4, [r12 + ST_DRIVE_THRESH + 12]
    cvtss2sd xmm4, xmm4
    movss xmm5, [r12 + ST_DRIVES + 12]
    cvtss2sd xmm5, xmm5
    subsd xmm4, xmm5
    maxsd xmm4, [rel drive_pressure_floor]
    addsd xmm3, xmm4

    ; Store pressures on stack for qdec functions
    movsd [rsp + 0], xmm0
    movsd [rsp + 8], xmm1
    movsd [rsp + 16], xmm2
    movsd [rsp + 24], xmm3

    ; Save pressures back to state
    movsd [r12 + ST_DRIVE_PRESSURES + 0], xmm0
    movsd [r12 + ST_DRIVE_PRESSURES + 8], xmm1
    movsd [r12 + ST_DRIVE_PRESSURES + 16], xmm2
    movsd [r12 + ST_DRIVE_PRESSURES + 24], xmm3

    ; Normalize to amplitudes (sqrt + unit vector)
    lea rdi, [rsp]
    mov esi, DRIVE_COUNT
    call qdec_set_amps

    ; Compute entropy for monitoring
    lea rdi, [rsp]
    mov esi, DRIVE_COUNT
    call qdec_entropy           ; xmm0 = entropy
    cvtsd2ss xmm0, xmm0
    movss [r12 + ST_DRIVE_ENTROPY], xmm0

    ; Print entropy
    lea rdi, [rel drive_entropy_msg]
    call print_cstr
    movss xmm0, [r12 + ST_DRIVE_ENTROPY]
    cvtss2sd xmm0, xmm0
    call print_f64
    call print_newline

    ; Measure (Born rule) → winner index
    lea rdi, [rsp]
    mov esi, DRIVE_COUNT
    call qdec_measure           ; eax = winning drive index
    mov r13d, eax               ; save winner
    mov [r12 + ST_DRIVE_ACTIVE], r13d

    ; Print winning drive
    lea rdi, [rel drive_born_msg]
    call print_cstr
    cmp r13d, 0
    je .born_acc
    cmp r13d, 1
    je .born_eff
    cmp r13d, 2
    je .born_nov
    ; default: coherence
    lea rdi, [rel drive_born_coh]
    call print_cstr
    jmp .born_action
.born_acc:
    lea rdi, [rel drive_born_acc]
    call print_cstr
    jmp .born_action
.born_eff:
    lea rdi, [rel drive_born_eff]
    call print_cstr
    jmp .born_action
.born_nov:
    lea rdi, [rel drive_born_nov]
    call print_cstr

.born_action:
    ; Execute winning drive's action
    mov eax, [r12 + ST_GLOBAL_STEP]
    mov [r12 + ST_GOAL_STEP], eax

    cmp r13d, DRIVE_ACCURACY
    jne .not_acc_action
    mov dword [r12 + ST_CURRENT_GOAL], GOAL_EXPLORE
    mov edi, HOOK_ON_DRIVE
    mov esi, DRIVE_ACCURACY
    call fire_hook
    jmp .born_collapse

.not_acc_action:
    cmp r13d, DRIVE_EFFICIENCY
    jne .not_eff_action
    mov dword [r12 + ST_CURRENT_GOAL], GOAL_PRUNE
    call region_compact
    mov edi, HOOK_ON_DRIVE
    mov esi, DRIVE_EFFICIENCY
    call fire_hook
    jmp .born_collapse

.not_eff_action:
    cmp r13d, DRIVE_NOVELTY
    jne .not_nov_action
    mov dword [r12 + ST_CURRENT_GOAL], GOAL_EXPLORE
    call modify_restructure
    mov edi, HOOK_ON_DRIVE
    mov esi, DRIVE_NOVELTY
    call fire_hook
    jmp .born_collapse

.not_nov_action:
    ; Coherence
    mov dword [r12 + ST_CURRENT_GOAL], GOAL_ALIGN
    mov edi, HOOK_ON_DRIVE
    mov esi, DRIVE_COHERENCE
    call fire_hook

.born_collapse:
    ; Collapse: decay all pressures by 0.5, winner by extra 0.5
    ; Floor all at 0.1
    lea rdi, [r12 + ST_DRIVE_PRESSURES]
    mov esi, DRIVE_COUNT
    mov edx, r13d               ; winner
    call qdec_collapse

    ; --- Select dispatch mode based on winning drive ---
    mov dword [r12 + ST_DISPATCH_MODE], DMODE_FAST    ; default
    cmp r13d, DRIVE_ACCURACY
    jne .mode_not_explore_q
    mov dword [r12 + ST_DISPATCH_MODE], DMODE_EXPLORE
    jmp .mode_set_q
.mode_not_explore_q:
    ; If accuracy > 0.7 → BEST mode
    movss xmm0, [r12 + ST_DRIVES + 0]
    mov eax, 0x3F333333        ; 0.7f
    movd xmm1, eax
    comiss xmm0, xmm1
    jbe .mode_set_q
    mov dword [r12 + ST_DISPATCH_MODE], DMODE_BEST
.mode_set_q:

    add rsp, 40
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; drives_show
;; Print current drive levels and thresholds
;; ============================================================
global drives_show
drives_show:
    push rbx
    mov rbx, SURFACE_BASE

    lea rdi, [rel drive_hdr]
    call print_cstr

    ; Accuracy
    lea rdi, [rel drive_acc_lbl]
    call print_cstr
    movss xmm0, [rbx + STATE_OFFSET + ST_DRIVES + 0]
    call print_f32
    lea rdi, [rel drive_thresh]
    call print_cstr
    movss xmm0, [rbx + STATE_OFFSET + ST_DRIVE_THRESH + 0]
    call print_f32
    lea rdi, [rel drive_close]
    call print_cstr

    ; Efficiency
    lea rdi, [rel drive_eff_lbl]
    call print_cstr
    movss xmm0, [rbx + STATE_OFFSET + ST_DRIVES + 4]
    call print_f32
    lea rdi, [rel drive_thresh]
    call print_cstr
    movss xmm0, [rbx + STATE_OFFSET + ST_DRIVE_THRESH + 4]
    call print_f32
    lea rdi, [rel drive_close]
    call print_cstr

    ; Novelty
    lea rdi, [rel drive_nov_lbl]
    call print_cstr
    movss xmm0, [rbx + STATE_OFFSET + ST_DRIVES + 8]
    call print_f32
    lea rdi, [rel drive_thresh]
    call print_cstr
    movss xmm0, [rbx + STATE_OFFSET + ST_DRIVE_THRESH + 8]
    call print_f32
    lea rdi, [rel drive_close]
    call print_cstr

    ; Coherence
    lea rdi, [rel drive_coh_lbl]
    call print_cstr
    movss xmm0, [rbx + STATE_OFFSET + ST_DRIVES + 12]
    call print_f32
    lea rdi, [rel drive_thresh]
    call print_cstr
    movss xmm0, [rbx + STATE_OFFSET + ST_DRIVE_THRESH + 12]
    call print_f32
    lea rdi, [rel drive_close]
    call print_cstr

    pop rbx
    ret

;; ============================================================
;; drives_set_threshold(drive_id, value)
;; edi=drive index (0-3), xmm0=new threshold (f32)
;; ============================================================
global drives_set_threshold
drives_set_threshold:
    cmp edi, 4
    jge .done
    mov rax, SURFACE_BASE
    lea rax, [rax + STATE_OFFSET + ST_DRIVE_THRESH]
    movss [rax + rdi * 4], xmm0
.done:
    ret
