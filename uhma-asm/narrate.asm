; narrate.asm — Inner voice: translates somatic state into language tokens
;
; @entry narrate_tick() -> void                     ; check state, gen narrative
; @entry narrate_inject(rdi=tokens) -> void         ; inject into input stream
; @entry inner_voice_show() -> void                 ; REPL: show recent output
;
; @calls dispatch.asm:process_token_internal
; @calls receipt.asm:intro_query_confusion, intro_query_confidence
; @calledby introspect.asm:tick_workers
;
; GOTCHAS:
;   - Creates recursive self-awareness: State → Narration → Prediction → State
;   - Tokens are simple: SELF + STATE (TIRED, ALERT, CONFUSED, SURE, etc.)
;   - Thresholds: PRES_FATIGUE>0.7→TIRED, intro_confusion>0.5→CONFUSED
;   - Inner voice IS the experience, not simulation of it
;   - Prediction of consequences ("SELF TIRED" → dream cycle) is understanding
;
%include "syscalls.inc"
%include "constants.inc"

section .data
    narrate_hdr:        db "[INNER] ", 0
    narrate_msg:        db "Voice: ", 0
    narrate_nl:         db 10, 0

    ; Narrative tokens (4 bytes each, null-terminated for printing)
    tok_self:           db "SELF", 0
    tok_tired:          db "TIRED", 0
    tok_alert:          db "ALERT", 0
    tok_confused:       db "CONFUSED", 0
    tok_sure:           db "SURE", 0
    tok_struggling:     db "STRUGGLING", 0
    tok_wrong:          db "WRONG", 0
    tok_calm:           db "CALM", 0
    tok_learning:       db "LEARNING", 0
    tok_hungry:         db "HUNGRY", 0
    tok_aware:          db "AWARE", 0
    tok_alive:          db "ALIVE", 0

    ; Token hashes (pre-computed for injection)
    align 4
    hash_self:          dd 0x53454C46   ; "SELF"
    hash_tired:         dd 0x54495244   ; "TIRD"
    hash_alert:         dd 0x414C5254   ; "ALRT"
    hash_confused:      dd 0x434F4E46   ; "CONF"
    hash_sure:          dd 0x53555245   ; "SURE"
    hash_struggling:    dd 0x53545247   ; "STRG"
    hash_wrong:         dd 0x57524F4E   ; "WRON"
    hash_calm:          dd 0x43414C4D   ; "CALM"
    hash_learning:      dd 0x4C45524E   ; "LERN"
    hash_hungry:        dd 0x48554E47   ; "HUNG"
    hash_aware:         dd 0x41574152   ; "AWAR"
    hash_alive:         dd 0x414C4956   ; "ALIV"

    align 8
    ; Thresholds for narrative generation
    fatigue_thresh:     dq 0.65         ; when to say "TIRED"
    arousal_thresh:     dq 0.70         ; when to say "ALERT"
    confusion_thresh:   dq 0.50         ; when to say "CONFUSED"
    confidence_thresh:  dq 0.70         ; when to say "SURE"
    accuracy_low:       dq 0.30         ; when to say "STRUGGLING"
    energy_low:         dq 25.0         ; when to say "HUNGRY"
    self_aware_high:    dq 0.80         ; when to say "AWARE"

    ; Cooldown to prevent narrative flooding
    narrate_cooldown:   dq 50           ; minimum steps between narrations
    last_narrate_step:  dq 0            ; step when last narrated

    ; Recent narrations ring buffer (for inner_voice_show)
    align 8
    narrate_history:    times 256 db 0  ; 8 entries * 32 bytes each
    narrate_hist_idx:   dd 0

    NARRATE_HIST_SIZE   equ 8
    NARRATE_ENTRY_SIZE  equ 32

section .bss
    ; Pending narrative tokens to inject
    pending_tokens:     resd 16         ; up to 16 tokens waiting
    pending_count:      resd 1

section .text

extern print_cstr
extern print_f32
extern print_newline
extern process_token
extern intro_query_confusion
extern intro_query_confidence
extern intro_get_self_awareness

;; ============================================================
;; narrate_tick()
;; Called from tick_workers. Checks internal state and generates
;; narrative tokens when thresholds are crossed.
;; Returns: eax = 1 if narration generated, 0 otherwise
;; ============================================================
global narrate_tick
narrate_tick:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8                      ; alignment (5 pushes = odd)

    mov rbx, SURFACE_BASE
    xor r12d, r12d                  ; narration_generated = 0

    ; Check cooldown
    mov rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov rcx, [rel last_narrate_step]
    sub rax, rcx
    cmp rax, [rel narrate_cooldown]
    jl .narrate_done                ; still in cooldown

    ; === CHECK FATIGUE → "SELF TIRED" ===
    movss xmm0, [rbx + STATE_OFFSET + ST_PRESENCE + PRES_FATIGUE * 4]
    cvtss2sd xmm0, xmm0
    ucomisd xmm0, [rel fatigue_thresh]
    jbe .check_arousal

    ; Generate "SELF TIRED"
    mov edi, [rel hash_self]
    call inject_narrative_token
    mov edi, [rel hash_tired]
    call inject_narrative_token
    lea rdi, [rel tok_self]
    lea rsi, [rel tok_tired]
    call record_narration
    mov r12d, 1
    jmp .process_pending

.check_arousal:
    ; === CHECK AROUSAL → "SELF ALERT" ===
    movss xmm0, [rbx + STATE_OFFSET + ST_PRESENCE + PRES_AROUSAL * 4]
    cvtss2sd xmm0, xmm0
    ucomisd xmm0, [rel arousal_thresh]
    jbe .check_confusion

    ; Generate "SELF ALERT"
    mov edi, [rel hash_self]
    call inject_narrative_token
    mov edi, [rel hash_alert]
    call inject_narrative_token
    lea rdi, [rel tok_self]
    lea rsi, [rel tok_alert]
    call record_narration
    mov r12d, 1
    jmp .process_pending

.check_confusion:
    ; === CHECK CONFUSION → "SELF CONFUSED" ===
    xor edi, edi                    ; current context
    call intro_query_confusion      ; xmm0 = confusion level
    ucomisd xmm0, [rel confusion_thresh]
    jbe .check_confidence

    ; Generate "SELF CONFUSED"
    mov edi, [rel hash_self]
    call inject_narrative_token
    mov edi, [rel hash_confused]
    call inject_narrative_token
    lea rdi, [rel tok_self]
    lea rsi, [rel tok_confused]
    call record_narration
    mov r12d, 1
    jmp .process_pending

.check_confidence:
    ; === CHECK CONFIDENCE → "SELF SURE" ===
    xor edi, edi
    call intro_query_confidence     ; xmm0 = confidence level
    ucomisd xmm0, [rel confidence_thresh]
    jbe .check_accuracy

    ; Generate "SELF SURE"
    mov edi, [rel hash_self]
    call inject_narrative_token
    mov edi, [rel hash_sure]
    call inject_narrative_token
    lea rdi, [rel tok_self]
    lea rsi, [rel tok_sure]
    call record_narration
    mov r12d, 1
    jmp .process_pending

.check_accuracy:
    ; === CHECK ACCURACY → "SELF STRUGGLING" ===
    movss xmm0, [rbx + STATE_OFFSET + ST_DRIVES + 0]  ; accuracy drive
    cvtss2sd xmm0, xmm0
    ucomisd xmm0, [rel accuracy_low]
    ja .check_energy

    ; Generate "SELF STRUGGLING"
    mov edi, [rel hash_self]
    call inject_narrative_token
    mov edi, [rel hash_struggling]
    call inject_narrative_token
    lea rdi, [rel tok_self]
    lea rsi, [rel tok_struggling]
    call record_narration
    mov r12d, 1
    jmp .process_pending

.check_energy:
    ; === CHECK ENERGY → "SELF HUNGRY" ===
    movsd xmm0, [rbx + STATE_OFFSET + ST_ENERGY]
    ucomisd xmm0, [rel energy_low]
    ja .check_self_aware

    ; Generate "SELF HUNGRY"
    mov edi, [rel hash_self]
    call inject_narrative_token
    mov edi, [rel hash_hungry]
    call inject_narrative_token
    lea rdi, [rel tok_self]
    lea rsi, [rel tok_hungry]
    call record_narration
    mov r12d, 1
    jmp .process_pending

.check_self_aware:
    ; === CHECK SELF-AWARENESS → "SELF AWARE" (rare, celebratory) ===
    call intro_get_self_awareness   ; xmm0 = self-awareness ratio
    ucomisd xmm0, [rel self_aware_high]
    jbe .check_surprise

    ; Generate "SELF AWARE"
    mov edi, [rel hash_self]
    call inject_narrative_token
    mov edi, [rel hash_aware]
    call inject_narrative_token
    lea rdi, [rel tok_self]
    lea rsi, [rel tok_aware]
    call record_narration
    mov r12d, 1
    jmp .process_pending

.check_surprise:
    ; === CHECK SELF-SURPRISE → "SELF WRONG" ===
    cmp dword [rbx + STATE_OFFSET + ST_SURPRISE_TYPE], SURPRISE_SELF
    jne .check_calm

    ; Generate "SELF WRONG"
    mov edi, [rel hash_self]
    call inject_narrative_token
    mov edi, [rel hash_wrong]
    call inject_narrative_token
    lea rdi, [rel tok_self]
    lea rsi, [rel tok_wrong]
    call record_narration
    mov r12d, 1
    jmp .process_pending

.check_calm:
    ; === DEFAULT: If nothing triggered, occasionally say "SELF ALIVE" ===
    ; Only if enough time has passed and system is stable
    mov rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    test rax, 0xFF                  ; every 256 steps
    jnz .narrate_done

    ; Check if stable (low arousal, low fatigue)
    movss xmm0, [rbx + STATE_OFFSET + ST_PRESENCE + PRES_AROUSAL * 4]
    mov eax, 0x3F000000             ; 0.5f
    movd xmm1, eax
    ucomiss xmm0, xmm1
    ja .narrate_done
    movss xmm0, [rbx + STATE_OFFSET + ST_PRESENCE + PRES_FATIGUE * 4]
    ucomiss xmm0, xmm1
    ja .narrate_done

    ; Generate "SELF ALIVE"
    mov edi, [rel hash_self]
    call inject_narrative_token
    mov edi, [rel hash_alive]
    call inject_narrative_token
    lea rdi, [rel tok_self]
    lea rsi, [rel tok_alive]
    call record_narration
    mov r12d, 1

.process_pending:
    ; If we generated narration, process pending tokens
    test r12d, r12d
    jz .narrate_done

    ; Update cooldown
    mov rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov [rel last_narrate_step], rax

    ; Process all pending tokens through dispatch
    call flush_narrative_tokens

.narrate_done:
    mov eax, r12d
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; inject_narrative_token(token_hash)
;; edi = token hash to queue for injection
;; Adds token to pending queue (will be processed by flush)
;; ============================================================
inject_narrative_token:
    mov eax, [rel pending_count]
    cmp eax, 15
    jge .inject_full                ; queue full

    lea rcx, [rel pending_tokens]
    mov [rcx + rax * 4], edi
    inc dword [rel pending_count]
    ret

.inject_full:
    ret

;; ============================================================
;; flush_narrative_tokens()
;; Process all pending narrative tokens through dispatch.
;; This is the injection point where inner voice meets prediction.
;; ============================================================
flush_narrative_tokens:
    push rbx
    push r12
    push r13
    sub rsp, 8

    mov rbx, SURFACE_BASE
    xor r12d, r12d                  ; index
    mov r13d, [rel pending_count]

    ; Print inner voice output
    lea rdi, [rel narrate_hdr]
    call print_cstr
    lea rdi, [rel narrate_msg]
    call print_cstr

.flush_loop:
    cmp r12d, r13d
    jge .flush_done

    lea rcx, [rel pending_tokens]
    mov edi, [rcx + r12 * 4]

    ; Mark as internal thought (high bit set) - dispatch can check this
    or edi, 0x80000000

    ; Process through dispatch (as internal token)
    push r12
    push r13
    ; Strip the internal marker for now - dispatch will process normally
    and edi, 0x7FFFFFFF
    call process_token
    pop r13
    pop r12

    inc r12d
    jmp .flush_loop

.flush_done:
    call print_newline

    ; Clear pending queue
    mov dword [rel pending_count], 0

    add rsp, 8
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; record_narration(word1, word2)
;; rdi = first word string, rsi = second word string
;; Records narration in history buffer for inner_voice_show
;; ============================================================
record_narration:
    push rbx
    push r12
    push r13

    mov r12, rdi                    ; word1
    mov r13, rsi                    ; word2

    ; Get history slot
    mov eax, [rel narrate_hist_idx]
    and eax, (NARRATE_HIST_SIZE - 1)
    imul eax, eax, NARRATE_ENTRY_SIZE
    lea rbx, [rel narrate_history]
    add rbx, rax

    ; Copy word1 (up to 12 bytes)
    mov rdi, rbx
    mov rsi, r12
    mov ecx, 12
.copy1:
    test ecx, ecx
    jz .sep1
    mov al, [rsi]
    test al, al
    jz .sep1
    mov [rdi], al
    inc rdi
    inc rsi
    dec ecx
    jmp .copy1
.sep1:
    mov byte [rdi], ' '
    inc rdi

    ; Copy word2 (up to 12 bytes)
    mov rsi, r13
    mov ecx, 12
.copy2:
    test ecx, ecx
    jz .term
    mov al, [rsi]
    test al, al
    jz .term
    mov [rdi], al
    inc rdi
    inc rsi
    dec ecx
    jmp .copy2
.term:
    mov byte [rdi], 0               ; null terminate

    ; Increment index
    inc dword [rel narrate_hist_idx]

    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; inner_voice_show()
;; REPL command: shows recent inner voice narrations
;; ============================================================
global inner_voice_show
inner_voice_show:
    push rbx
    push r12

    lea rdi, [rel narrate_hdr]
    call print_cstr

    ; Print recent narrations
    xor r12d, r12d
.show_loop:
    cmp r12d, NARRATE_HIST_SIZE
    jge .show_done

    ; Calculate index (oldest to newest)
    mov eax, [rel narrate_hist_idx]
    add eax, r12d
    and eax, (NARRATE_HIST_SIZE - 1)
    imul eax, eax, NARRATE_ENTRY_SIZE
    lea rdi, [rel narrate_history]
    add rdi, rax

    ; Skip empty entries
    cmp byte [rdi], 0
    je .show_next

    ; Print entry
    call print_cstr
    lea rdi, [rel narrate_nl]
    call print_cstr
    lea rdi, [rel narrate_hdr]
    call print_cstr

.show_next:
    inc r12d
    jmp .show_loop

.show_done:
    call print_newline
    pop r12
    pop rbx
    ret

;; ============================================================
;; narrate_surprise(surprise_type)
;; edi = surprise type (SURPRISE_SELF or SURPRISE_OUTCOME)
;; Called immediately when surprise occurs to narrate it.
;; This is time-critical - the narration must happen NOW,
;; not on the next tick, to capture the felt moment.
;;
;; ORGANIC SELF-REGULATION: If arousal is too high (system thrashing),
;; the inner voice goes silent. This is the felt sense of being
;; too overwhelmed to articulate. Prevents runaway recursion naturally.
;; ============================================================
global narrate_surprise
narrate_surprise:
    push rbx
    mov rbx, SURFACE_BASE

    ; === ORGANIC CIRCUIT BREAKER: Check arousal ===
    ; If arousal > 0.9, system is thrashing - stay silent
    movss xmm0, [rbx + STATE_OFFSET + ST_PRESENCE + PRES_AROUSAL * 4]
    mov eax, 0x3F666666             ; 0.9f threshold
    movd xmm1, eax
    comiss xmm0, xmm1
    ja .surprise_done               ; too overwhelmed to speak

    cmp edi, SURPRISE_SELF
    jne .outcome_surprise

    ; SELF SURPRISE: "I was wrong about myself"
    mov edi, [rel hash_self]
    call inject_narrative_token
    mov edi, [rel hash_wrong]
    call inject_narrative_token
    lea rdi, [rel tok_self]
    lea rsi, [rel tok_wrong]
    call record_narration
    call flush_narrative_tokens
    jmp .surprise_done

.outcome_surprise:
    cmp edi, SURPRISE_OUTCOME
    jne .surprise_done

    ; OUTCOME SURPRISE: "The world surprised me" → "SELF LEARNING"
    mov edi, [rel hash_self]
    call inject_narrative_token
    mov edi, [rel hash_learning]
    call inject_narrative_token
    lea rdi, [rel tok_self]
    lea rsi, [rel tok_learning]
    call record_narration
    call flush_narrative_tokens

.surprise_done:
    pop rbx
    ret

