; drives.asm — Drive system: thresholds, actions, homeostatic loops
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

section .text

extern print_cstr
extern print_f32
extern print_newline
extern fire_hook
extern region_compact
extern modify_restructure

;; ============================================================
;; drives_check
;; Check all drive levels against thresholds, trigger actions
;; Called periodically from the observation loop or REPL
;; ============================================================
global drives_check
drives_check:
    push rbx
    push r12

    mov rbx, SURFACE_BASE
    lea r12, [rbx + STATE_OFFSET]

    ; Clear goal at start (will be set by triggered drive)
    mov dword [r12 + ST_CURRENT_GOAL], GOAL_NONE

    ; --- Drive 0: Accuracy ---
    ; If accuracy < threshold → increase exploration
    movss xmm0, [r12 + ST_DRIVES + 0]        ; current accuracy
    mov eax, [r12 + ST_DRIVE_THRESH + 0]      ; threshold
    movd xmm1, eax
    comiss xmm0, xmm1
    ja .check_efficiency       ; accuracy above threshold, OK

    ; TRIGGER: Accuracy too low
    mov dword [r12 + ST_CURRENT_GOAL], GOAL_EXPLORE
    mov eax, [r12 + ST_GLOBAL_STEP]
    mov [r12 + ST_GOAL_STEP], eax

    lea rdi, [rel drive_trigger]
    call print_cstr
    lea rdi, [rel drive_acc_trig]
    call print_cstr

    ; Action: emit some exploratory patterns from miss buffer
    ; (The observation loop will handle this via learn_pattern calls)
    mov edi, HOOK_ON_DRIVE
    mov esi, DRIVE_ACCURACY
    call fire_hook

.check_efficiency:
    ; --- Drive 1: Efficiency ---
    ; If surface usage > threshold → prune
    movss xmm0, [r12 + ST_DRIVES + 4]
    mov eax, [r12 + ST_DRIVE_THRESH + 4]
    movd xmm1, eax
    comiss xmm0, xmm1
    jb .check_novelty          ; usage below threshold, OK

    ; TRIGGER: Too much surface used
    mov dword [r12 + ST_CURRENT_GOAL], GOAL_PRUNE
    mov eax, [r12 + ST_GLOBAL_STEP]
    mov [r12 + ST_GOAL_STEP], eax

    lea rdi, [rel drive_trigger]
    call print_cstr
    lea rdi, [rel drive_eff_trig]
    call print_cstr

    ; Action: compact condemned regions
    call region_compact

    mov edi, HOOK_ON_DRIVE
    mov esi, DRIVE_EFFICIENCY
    call fire_hook

.check_novelty:
    ; --- Drive 2: Novelty ---
    ; If novelty too low (same regions always firing) → force rotation
    movss xmm0, [r12 + ST_DRIVES + 8]
    mov eax, [r12 + ST_DRIVE_THRESH + 8]
    movd xmm1, eax
    comiss xmm0, xmm1
    jb .check_coherence

    ; TRIGGER: Too repetitive
    mov dword [r12 + ST_CURRENT_GOAL], GOAL_EXPLORE
    mov eax, [r12 + ST_GLOBAL_STEP]
    mov [r12 + ST_GOAL_STEP], eax

    lea rdi, [rel drive_trigger]
    call print_cstr
    lea rdi, [rel drive_nov_trig]
    call print_cstr

    ; Action: restructure dispatch tree (reorder)
    call modify_restructure

    mov edi, HOOK_ON_DRIVE
    mov esi, DRIVE_NOVELTY
    call fire_hook

.check_coherence:
    ; --- Drive 3: Coherence ---
    ; If VSA and dispatch disagree often → alignment pass
    movss xmm0, [r12 + ST_DRIVES + 12]
    mov eax, [r12 + ST_DRIVE_THRESH + 12]
    movd xmm1, eax
    comiss xmm0, xmm1
    ja .drives_done

    ; TRIGGER: Coherence too low
    mov dword [r12 + ST_CURRENT_GOAL], GOAL_ALIGN
    mov eax, [r12 + ST_GLOBAL_STEP]
    mov [r12 + ST_GOAL_STEP], eax

    lea rdi, [rel drive_trigger]
    call print_cstr
    lea rdi, [rel drive_coh_trig]
    call print_cstr

    mov edi, HOOK_ON_DRIVE
    mov esi, DRIVE_COHERENCE
    call fire_hook

.drives_done:
    ; --- Select dispatch mode based on drive state ---
    ; Default: FAST
    mov dword [r12 + ST_DISPATCH_MODE], DMODE_FAST

    ; If accuracy is below threshold → EXPLORE mode
    movss xmm0, [r12 + ST_DRIVES + 0]
    mov eax, [r12 + ST_DRIVE_THRESH + 0]
    movd xmm1, eax
    comiss xmm0, xmm1
    ja .mode_not_explore
    mov dword [r12 + ST_DISPATCH_MODE], DMODE_EXPLORE
    jmp .mode_set
.mode_not_explore:
    ; If accuracy > 0.7 and efficiency OK → BEST mode
    mov eax, 0x3F333333        ; 0.7f
    movd xmm1, eax
    comiss xmm0, xmm1
    jbe .mode_set
    movss xmm0, [r12 + ST_DRIVES + 4]
    mov eax, [r12 + ST_DRIVE_THRESH + 4]
    movd xmm1, eax
    comiss xmm0, xmm1
    ja .mode_set               ; efficiency too high, stay FAST
    mov dword [r12 + ST_DISPATCH_MODE], DMODE_BEST
.mode_set:

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
