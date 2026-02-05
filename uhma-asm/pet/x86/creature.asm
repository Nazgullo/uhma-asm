; creature.asm — Creature state management and behavior simulation
;
; @entry creature_init() -> sets initial state
; @entry creature_update() -> advances one frame (call at 60fps)
; @entry creature_feed() -> user feeds creature
; @entry creature_poke() -> user pokes creature
; @entry creature_dream() -> trigger dream cycle
; @entry creature_set_activity(edi=action_id) -> set current UHMA action for visual expression
; @entry creature_sync_uhma(rdi=text) -> parse UHMA presence response, update state
; @global creature_state -> base of CreatureState struct
;
; All state values are integers 0-1000 unless noted.
; Phase 2 DONE: creature_sync_uhma() wires real UHMA presence → creature state.
; Phase 3: Activity expression — creature visually reflects UHMA's current action
;   SEEK=sniffing (ears perked, rapid bounce), SCAN=radar ears, COMPOSE=creative wag,
;   REFLECT=meditation (half-closed eyes). Fades after 5 seconds (300 frames).
;
; GOTCHAS:
;   - All values 0-1000 (not float). Rendering scales as needed.
;   - creature_state is global BSS, accessed by render.asm
;   - Frame counter wraps at 2^32 (~828 days at 60fps, fine)
;   - CS_ACTIVITY_FADE counts up from 0; effects stop at 300 frames

;; ======== CreatureState struct offsets ========
CS_SPECIES      equ 0       ; byte: 0=egg, 1=hound, 2=feline, 3=mech, 4=wisp
CS_MATURITY     equ 1       ; byte: 0=egg, 1=infant, 2=child, 3=adolescent, 4=adult, 5=elder
CS_SLEEPING     equ 2       ; byte: 1=sleeping
CS_DREAMING     equ 3       ; byte: 1=dreaming
CS_VALENCE      equ 4       ; dword: 0-1000 (500=neutral, >500=good)
CS_AROUSAL      equ 8       ; dword: 0-1000 (0=calm, 1000=hyper)
CS_FATIGUE      equ 12      ; dword: 0-1000 (0=fresh, 1000=exhausted)
CS_CONFIDENCE   equ 16      ; dword: 0-1000
CS_SURPRISE     equ 20      ; dword: 0-1000 (spikes then decays)
CS_SELF_AWARE   equ 24      ; dword: 0-1000
CS_POS_X        equ 28      ; dword: screen x (pixels)
CS_POS_Y        equ 32      ; dword: screen y (pixels)
CS_TARGET_X     equ 36      ; dword: movement target x
CS_TARGET_Y     equ 40      ; dword: movement target y
CS_EYE_OPEN     equ 44      ; dword: 0-1000 (0=closed, 1000=wide)
CS_EAR_POS      equ 48      ; dword: 0-1000 (0=flat, 500=neutral, 1000=perked)
CS_TAIL_POS     equ 52      ; dword: 0-1000 (0=tucked, 500=neutral, 1000=high)
CS_TAIL_PHASE   equ 56      ; dword: wag animation phase (0-3600, /10 = degrees)
CS_BREATH_PHASE equ 60      ; dword: breathing animation (0-3600)
CS_FRAME        equ 64      ; dword: frame counter
CS_BODY_COLOR   equ 68      ; dword: 0xRRGGBB
CS_EYE_COLOR    equ 72      ; dword: 0xRRGGBB
CS_BLINK_TIMER  equ 76      ; dword: frames until next blink
CS_BLINK_DUR    equ 80      ; dword: frames remaining in blink (0=not blinking)
CS_MOOD         equ 84      ; dword: 0=panic,1=anxious,2=neutral,3=content,4=happy,5=vigorous
CS_WAG_SPEED    equ 88      ; dword: tail wag speed (derived from valence)
CS_WANDER_TMR   equ 92      ; dword: frames until picking new wander target
CS_BOUNCE_PHASE equ 96      ; dword: bounce animation phase
CS_COGNITIVE_LOAD equ 100   ; dword: 0-1000, how "busy" UHMA's processing is (from entropy)
CS_PATTERN_TYPE equ 104     ; dword: 0=idle, 1=repetitive, 2=novel, 3=self-referential
CS_ACTIVITY     equ 108     ; dword: last UHMA action (ACTION_* id, 0-9)
CS_ACTIVITY_FADE equ 112    ; dword: frames since last action change (fades visual effect)
CS_SIZE         equ 116

section .bss
global creature_state
creature_state: resb CS_SIZE

section .data
    ; Hound body color variants
    color_hound_body:   dd 0xCD853F     ; peru/sandy brown
    color_hound_eye:    dd 0x3D2B1F     ; dark brown
    color_hound_nose:   dd 0x2A1A0A     ; very dark brown

    ; Presence dimension indices we care about
    PRES_NOVELTY        equ 2
    PRES_AROUSAL        equ 3
    PRES_VALENCE        equ 4
    PRES_UNCERTAINTY    equ 5
    PRES_ENGAGEMENT     equ 6
    PRES_STABILITY      equ 11
    PRES_DISSONANCE     equ 21
    PRES_SURPRISE       equ 25
    PRES_FAMILIARITY    equ 26
    PRES_ENTROPY        equ 16
    PRES_DEPTH          equ 18
    PRES_RESONANCE      equ 20
    PRES_META_AWARE     equ 29
    PRES_COUNT          equ 30

section .text

extern rand, srand, time

;; ============================================================
;; creature_init — Set initial creature state
;; ============================================================
global creature_init
creature_init:
    push rbx
    sub rsp, 16

    lea rbx, [rel creature_state]

    ; Seed random
    xor edi, edi
    call time
    mov edi, eax
    call srand

    ; Identity
    mov byte [rbx + CS_SPECIES], 1      ; hound
    mov byte [rbx + CS_MATURITY], 2     ; child
    mov byte [rbx + CS_SLEEPING], 0
    mov byte [rbx + CS_DREAMING], 0

    ; Internal state
    mov dword [rbx + CS_VALENCE], 600
    mov dword [rbx + CS_AROUSAL], 350
    mov dword [rbx + CS_FATIGUE], 100
    mov dword [rbx + CS_CONFIDENCE], 500
    mov dword [rbx + CS_SURPRISE], 0
    mov dword [rbx + CS_SELF_AWARE], 400

    ; Position (center of 800x600 window, slightly below center)
    mov dword [rbx + CS_POS_X], 400
    mov dword [rbx + CS_POS_Y], 360
    mov dword [rbx + CS_TARGET_X], 400
    mov dword [rbx + CS_TARGET_Y], 360

    ; Expression
    mov dword [rbx + CS_EYE_OPEN], 800
    mov dword [rbx + CS_EAR_POS], 650
    mov dword [rbx + CS_TAIL_POS], 650
    mov dword [rbx + CS_TAIL_PHASE], 0
    mov dword [rbx + CS_BREATH_PHASE], 0
    mov dword [rbx + CS_BOUNCE_PHASE], 0

    ; Appearance
    mov eax, [rel color_hound_body]
    mov [rbx + CS_BODY_COLOR], eax
    mov eax, [rel color_hound_eye]
    mov [rbx + CS_EYE_COLOR], eax

    ; Timers
    mov dword [rbx + CS_FRAME], 0
    mov dword [rbx + CS_BLINK_TIMER], 180   ; ~3 seconds
    mov dword [rbx + CS_BLINK_DUR], 0
    mov dword [rbx + CS_MOOD], 3            ; content
    mov dword [rbx + CS_WAG_SPEED], 6
    mov dword [rbx + CS_WANDER_TMR], 120
    mov dword [rbx + CS_COGNITIVE_LOAD], 0
    mov dword [rbx + CS_PATTERN_TYPE], 0
    mov dword [rbx + CS_ACTIVITY], 0
    mov dword [rbx + CS_ACTIVITY_FADE], 301    ; start idle

    add rsp, 16
    pop rbx
    ret

;; ============================================================
;; creature_update — Advance one animation frame
;; ============================================================
global creature_update
creature_update:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 32             ; 5 pushes (odd) → aligned → sub multiple of 16

    lea rbx, [rel creature_state]

    ; Advance frame counter
    inc dword [rbx + CS_FRAME]

    ;; ---- Breathing animation ----
    ; Breathing rate modulated by cognitive load:
    ; high load → faster breathing (4-5), low load → slower (2-3)
    mov eax, [rbx + CS_COGNITIVE_LOAD]
    shr eax, 8              ; 0-3
    add eax, 2              ; base speed 2-5
    mov ecx, eax            ; breath speed
    mov eax, [rbx + CS_BREATH_PHASE]
    add eax, ecx
    cmp eax, 3600
    jl .breath_ok
    xor eax, eax
.breath_ok:
    mov [rbx + CS_BREATH_PHASE], eax

    ;; ---- Tail wag ----
    ; Wag speed proportional to valence (happy = fast wag)
    mov eax, [rbx + CS_VALENCE]
    shr eax, 6              ; valence/64, range 0-15
    add eax, 2              ; minimum wag speed of 2
    mov [rbx + CS_WAG_SPEED], eax

    mov ecx, [rbx + CS_TAIL_PHASE]
    add ecx, eax
    cmp ecx, 3600
    jl .wag_ok
    sub ecx, 3600
.wag_ok:
    mov [rbx + CS_TAIL_PHASE], ecx

    ;; ---- Bounce (when happy) ----
    mov eax, [rbx + CS_VALENCE]
    cmp eax, 700            ; only bounce when quite happy
    jl .no_bounce
    mov ecx, [rbx + CS_BOUNCE_PHASE]
    add ecx, 8
    cmp ecx, 3600
    jl .bounce_ok
    sub ecx, 3600
.bounce_ok:
    mov [rbx + CS_BOUNCE_PHASE], ecx
    jmp .bounce_done
.no_bounce:
    ; Settle bounce to 0
    mov eax, [rbx + CS_BOUNCE_PHASE]
    test eax, eax
    jz .bounce_done
    sub eax, 20
    jns .bounce_settle
    xor eax, eax
.bounce_settle:
    mov [rbx + CS_BOUNCE_PHASE], eax
.bounce_done:

    ;; ---- Blinking ----
    mov eax, [rbx + CS_BLINK_DUR]
    test eax, eax
    jnz .in_blink

    ; Not blinking — count down to next blink
    dec dword [rbx + CS_BLINK_TIMER]
    jnz .blink_done
    ; Time to blink
    mov dword [rbx + CS_BLINK_DUR], 6      ; 6 frames = 100ms blink
    jmp .blink_done

.in_blink:
    dec eax
    mov [rbx + CS_BLINK_DUR], eax
    test eax, eax
    jnz .blink_done
    ; Blink finished — pick next blink time (90-300 frames = 1.5-5 seconds)
    call rand
    and eax, 0xFF           ; 0-255
    add eax, 90             ; 90-345
    mov [rbx + CS_BLINK_TIMER], eax
.blink_done:

    ;; ---- Eye openness ----
    cmp byte [rbx + CS_SLEEPING], 1
    je .eyes_sleep

    ; Check if blinking
    mov eax, [rbx + CS_BLINK_DUR]
    test eax, eax
    jnz .eyes_blink

    ; Normal: based on arousal and fatigue
    mov eax, [rbx + CS_AROUSAL]
    mov ecx, [rbx + CS_FATIGUE]
    ; eye_target = 600 + arousal/4 - fatigue/4
    shr eax, 2
    shr ecx, 2
    mov r12d, 600
    add r12d, eax
    sub r12d, ecx
    ; Clamp 200-1000
    cmp r12d, 200
    jge .eye_min_ok
    mov r12d, 200
.eye_min_ok:
    cmp r12d, 1000
    jle .eye_max_ok
    mov r12d, 1000
.eye_max_ok:
    ; Lerp toward target
    mov eax, [rbx + CS_EYE_OPEN]
    ; new = old + (target - old) / 8
    mov ecx, r12d
    sub ecx, eax
    sar ecx, 3
    add eax, ecx
    mov [rbx + CS_EYE_OPEN], eax
    jmp .ears

.eyes_sleep:
    ; Sleeping: eyes closed, lerp to 0
    mov eax, [rbx + CS_EYE_OPEN]
    sub eax, 20
    jns .eye_sleep_ok
    xor eax, eax
.eye_sleep_ok:
    mov [rbx + CS_EYE_OPEN], eax
    jmp .ears

.eyes_blink:
    mov dword [rbx + CS_EYE_OPEN], 50       ; nearly closed during blink
    jmp .ears

    ;; ---- Ear position ----
.ears:
    cmp byte [rbx + CS_SLEEPING], 1
    je .ears_sleep

    ; Ear target: high valence + arousal → perked, low → flat
    mov eax, [rbx + CS_VALENCE]
    mov ecx, [rbx + CS_AROUSAL]
    ; target = 300 + valence/3 + arousal/5
    xor edx, edx
    push rbx
    mov ebx, 3
    div ebx                 ; eax = valence/3
    pop rbx
    mov r12d, eax
    mov eax, ecx
    xor edx, edx
    push rbx
    mov ebx, 5
    div ebx                 ; eax = arousal/5
    pop rbx
    add r12d, eax
    add r12d, 300
    ; Clamp 100-950
    cmp r12d, 100
    jge .ear_min_ok
    mov r12d, 100
.ear_min_ok:
    cmp r12d, 950
    jle .ear_max_ok
    mov r12d, 950
.ear_max_ok:
    ; Surprise → ears spike to max
    mov eax, [rbx + CS_SURPRISE]
    cmp eax, 500
    jl .ear_no_surprise
    mov r12d, 1000
.ear_no_surprise:
    ; Lerp
    mov eax, [rbx + CS_EAR_POS]
    mov ecx, r12d
    sub ecx, eax
    sar ecx, 3
    add eax, ecx
    mov [rbx + CS_EAR_POS], eax
    jmp .tail

.ears_sleep:
    ; Sleeping: ears flat
    mov eax, [rbx + CS_EAR_POS]
    sub eax, 10
    cmp eax, 100
    jge .ear_sleep_ok
    mov eax, 100
.ear_sleep_ok:
    mov [rbx + CS_EAR_POS], eax

    ;; ---- Tail position ----
.tail:
    cmp byte [rbx + CS_SLEEPING], 1
    je .tail_sleep

    ; Tail height based on valence
    ; High valence → high tail, low → tucked
    mov eax, [rbx + CS_VALENCE]
    ; target = 200 + valence * 6 / 10
    mov ecx, 6
    imul eax, ecx
    xor edx, edx
    mov ecx, 10
    div ecx
    add eax, 200
    ; Clamp 100-900
    cmp eax, 100
    jge .tail_min_ok
    mov eax, 100
.tail_min_ok:
    cmp eax, 900
    jle .tail_max_ok
    mov eax, 900
.tail_max_ok:
    mov r12d, eax
    ; Lerp
    mov eax, [rbx + CS_TAIL_POS]
    mov ecx, r12d
    sub ecx, eax
    sar ecx, 4
    add eax, ecx
    mov [rbx + CS_TAIL_POS], eax
    jmp .decay

.tail_sleep:
    ; Sleeping: tail low
    mov eax, [rbx + CS_TAIL_POS]
    sub eax, 5
    cmp eax, 200
    jge .tail_sleep_ok
    mov eax, 200
.tail_sleep_ok:
    mov [rbx + CS_TAIL_POS], eax

    ;; ---- Decay / drift ----
.decay:
    ; Surprise decays fast: surprise = surprise * 93 / 100
    mov eax, [rbx + CS_SURPRISE]
    imul eax, 93
    xor edx, edx
    mov ecx, 100
    div ecx
    mov [rbx + CS_SURPRISE], eax

    ; Arousal drifts toward 300: arousal = arousal * 99 / 100 + 3
    mov eax, [rbx + CS_AROUSAL]
    imul eax, 99
    xor edx, edx
    mov ecx, 100
    div ecx
    add eax, 3
    mov [rbx + CS_AROUSAL], eax

    ; Valence drifts toward 500: valence += (500 - valence) / 200
    mov eax, 500
    sub eax, [rbx + CS_VALENCE]
    cdq
    mov ecx, 200
    idiv ecx
    add [rbx + CS_VALENCE], eax

    ; Fatigue slowly increases when awake (~1 per 2 seconds)
    ; UHMA presence sync overrides this via stability dimension
    cmp byte [rbx + CS_SLEEPING], 1
    je .fatigue_sleep
    mov eax, [rbx + CS_FRAME]
    and eax, 127                    ; every 128 frames (~2s at 60fps)
    jnz .fatigue_no_inc
    mov eax, [rbx + CS_FATIGUE]
    inc eax
    cmp eax, 1000
    jle .fatigue_ok
    mov eax, 1000
.fatigue_ok:
    mov [rbx + CS_FATIGUE], eax
.fatigue_no_inc:
    mov eax, [rbx + CS_FATIGUE]

    ; Auto-sleep when exhausted
    cmp eax, 900
    jl .fatigue_done
    mov byte [rbx + CS_SLEEPING], 1
    mov byte [rbx + CS_DREAMING], 1
    jmp .fatigue_done

.fatigue_sleep:
    ; Sleeping: fatigue decreases
    mov eax, [rbx + CS_FATIGUE]
    sub eax, 3
    jns .fatigue_sleep_ok
    xor eax, eax
.fatigue_sleep_ok:
    mov [rbx + CS_FATIGUE], eax
    ; Wake up when rested
    cmp eax, 50
    jg .fatigue_done
    mov byte [rbx + CS_SLEEPING], 0
    mov byte [rbx + CS_DREAMING], 0
    ; Post-nap valence boost
    mov eax, [rbx + CS_VALENCE]
    add eax, 100
    cmp eax, 1000
    jle .wake_valence_ok
    mov eax, 1000
.wake_valence_ok:
    mov [rbx + CS_VALENCE], eax

.fatigue_done:

    ;; ---- Wandering ----
    cmp byte [rbx + CS_SLEEPING], 1
    je .wander_done

    dec dword [rbx + CS_WANDER_TMR]
    jnz .wander_move

    ; Pick new target
    call rand
    and eax, 0x1FF          ; 0-511
    add eax, 150            ; 150-661 (within 800px window)
    mov [rbx + CS_TARGET_X], eax

    call rand
    and eax, 0x7F           ; 0-127
    add eax, 310            ; 310-437 (stay in lower half)
    mov [rbx + CS_TARGET_Y], eax

    ; Next wander in 2-6 seconds
    call rand
    and eax, 0xFF
    add eax, 120            ; 120-375 frames
    mov [rbx + CS_WANDER_TMR], eax

.wander_move:
    ; Lerp position toward target
    mov eax, [rbx + CS_TARGET_X]
    sub eax, [rbx + CS_POS_X]
    sar eax, 5              ; slow movement
    add [rbx + CS_POS_X], eax

    mov eax, [rbx + CS_TARGET_Y]
    sub eax, [rbx + CS_POS_Y]
    sar eax, 5
    add [rbx + CS_POS_Y], eax

.wander_done:

    ;; ---- Mood computation ----
    ; mood = composite of valence, arousal, fatigue
    mov eax, [rbx + CS_VALENCE]
    mov ecx, [rbx + CS_AROUSAL]
    mov r12d, [rbx + CS_FATIGUE]

    ; PANIC: low valence + high arousal
    cmp eax, 200
    jg .not_panic
    cmp ecx, 700
    jl .not_panic
    mov dword [rbx + CS_MOOD], 0
    jmp .mood_done

.not_panic:
    ; ANXIOUS: low valence OR low confidence
    cmp eax, 350
    jl .is_anxious
    mov edx, [rbx + CS_CONFIDENCE]
    cmp edx, 300
    jg .not_anxious
.is_anxious:
    mov dword [rbx + CS_MOOD], 1
    jmp .mood_done

.not_anxious:
    ; VIGOROUS: high valence + high arousal + low fatigue
    cmp eax, 750
    jl .not_vigorous
    cmp ecx, 600
    jl .not_vigorous
    cmp r12d, 300
    jg .not_vigorous
    mov dword [rbx + CS_MOOD], 5
    jmp .mood_done

.not_vigorous:
    ; HAPPY: high valence
    cmp eax, 650
    jl .not_happy
    mov dword [rbx + CS_MOOD], 4
    jmp .mood_done

.not_happy:
    ; CONTENT: moderate valence
    cmp eax, 450
    jl .is_neutral
    mov dword [rbx + CS_MOOD], 3
    jmp .mood_done

.is_neutral:
    mov dword [rbx + CS_MOOD], 2

    ;; ---- Activity expression (modulates animation from UHMA action) ----
    inc dword [rbx + CS_ACTIVITY_FADE]

    cmp dword [rbx + CS_ACTIVITY_FADE], 300  ; 5 seconds at 60fps
    jg .activity_idle

    mov eax, [rbx + CS_ACTIVITY]

    cmp eax, 5                  ; ACTION_SEEK
    je .activity_seek
    cmp eax, 6                  ; ACTION_SCAN_ENV
    je .activity_scan
    cmp eax, 7                  ; ACTION_COMPOSE
    je .activity_compose
    cmp eax, 9                  ; ACTION_REFLECT
    je .activity_reflect
    jmp .activity_done

.activity_seek:
    ; Sniffing: ears perked forward, small rapid bounce
    mov dword [rbx + CS_EAR_POS], 950
    mov eax, [rbx + CS_FRAME]
    and eax, 0xF
    cmp eax, 8
    jl .activity_done
    add dword [rbx + CS_BOUNCE_PHASE], 30
    jmp .activity_done

.activity_scan:
    ; Radar: ears oscillate, eyes wide
    mov dword [rbx + CS_EYE_OPEN], 950
    mov eax, [rbx + CS_FRAME]
    and eax, 0x7F               ; period = 128 frames (~2 sec)
    cmp eax, 64
    jl .scan_ear_up
    sub eax, 128
    neg eax
.scan_ear_up:
    shl eax, 3                  ; scale to 0-512
    add eax, 400                ; center at 400-912
    mov [rbx + CS_EAR_POS], eax
    jmp .activity_done

.activity_compose:
    ; Creative wag: slower, deeper
    mov dword [rbx + CS_WAG_SPEED], 3
    ; Warm body color shift (slight red boost)
    mov eax, [rbx + CS_BODY_COLOR]
    or eax, 0x100000
    mov [rbx + CS_BODY_COLOR], eax
    jmp .activity_done

.activity_reflect:
    ; Meditation: half-closed eyes, still
    mov eax, [rbx + CS_EYE_OPEN]
    cmp eax, 400
    jle .activity_done
    sub eax, 20
    mov [rbx + CS_EYE_OPEN], eax
    jmp .activity_done

.activity_idle:
    ; Prevent overflow
    mov dword [rbx + CS_ACTIVITY_FADE], 301

.activity_done:

.mood_done:
    add rsp, 32
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; creature_feed — User feeds the creature
;; ============================================================
global creature_feed
creature_feed:
    lea rax, [rel creature_state]

    ; Wake up if sleeping
    mov byte [rax + CS_SLEEPING], 0
    mov byte [rax + CS_DREAMING], 0

    ; Valence boost
    mov ecx, [rax + CS_VALENCE]
    add ecx, 200
    cmp ecx, 1000
    jle .v_ok
    mov ecx, 1000
.v_ok:
    mov [rax + CS_VALENCE], ecx

    ; Arousal spike
    mov ecx, [rax + CS_AROUSAL]
    add ecx, 250
    cmp ecx, 1000
    jle .a_ok
    mov ecx, 1000
.a_ok:
    mov [rax + CS_AROUSAL], ecx

    ; Reduce fatigue slightly
    mov ecx, [rax + CS_FATIGUE]
    sub ecx, 30
    jns .f_ok
    xor ecx, ecx
.f_ok:
    mov [rax + CS_FATIGUE], ecx

    ; Confidence boost
    mov ecx, [rax + CS_CONFIDENCE]
    add ecx, 50
    cmp ecx, 1000
    jle .c_ok
    mov ecx, 1000
.c_ok:
    mov [rax + CS_CONFIDENCE], ecx

    ret

;; ============================================================
;; creature_poke — User pokes the creature
;; ============================================================
global creature_poke
creature_poke:
    lea rax, [rel creature_state]

    ; Arousal spike
    mov ecx, [rax + CS_AROUSAL]
    add ecx, 400
    cmp ecx, 1000
    jle .a_ok
    mov ecx, 1000
.a_ok:
    mov [rax + CS_AROUSAL], ecx

    ; Surprise spike
    mov dword [rax + CS_SURPRISE], 1000

    ; Might wake from sleep
    cmp byte [rax + CS_SLEEPING], 1
    jne .not_sleeping
    mov byte [rax + CS_SLEEPING], 0
    mov byte [rax + CS_DREAMING], 0
.not_sleeping:
    ret

;; ============================================================
;; creature_dream — Trigger dream cycle
;; ============================================================
global creature_dream
creature_dream:
    lea rax, [rel creature_state]

    ; Start sleeping/dreaming
    mov byte [rax + CS_SLEEPING], 1
    mov byte [rax + CS_DREAMING], 1

    ; Set fatigue to moderate (will decrease during sleep)
    mov ecx, [rax + CS_FATIGUE]
    cmp ecx, 400
    jge .f_ok
    mov ecx, 400
.f_ok:
    mov [rax + CS_FATIGUE], ecx

    ; Lower arousal
    mov dword [rax + CS_AROUSAL], 100

    ret

;; ============================================================
;; creature_set_activity — Set current UHMA action for visual expression
;; edi = action ID (0-9), resets fade timer
;; ============================================================
global creature_set_activity
creature_set_activity:
    lea rax, [rel creature_state]
    mov [rax + CS_ACTIVITY], edi
    mov dword [rax + CS_ACTIVITY_FADE], 0    ; reset fade
    ret

;; ============================================================
;; creature_sync_uhma — Parse UHMA presence response, update state
;; rdi = pointer to presence response text (null-terminated)
;;
;; Parses lines like "3: 0.567\n" → value 567 (0-1000 scale)
;; Maps presence dimensions to creature state fields.
;; Called every poll interval (~3 seconds) when UHMA is connected.
;;
;; If response is empty or doesn't contain presence data,
;; creature keeps its simulated state (no-op).
;; ============================================================
global creature_sync_uhma
creature_sync_uhma:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 136            ; 5 pushes (odd) → aligned → sub multiple of 16
                            ; 30*4 = 120 bytes for presence vals + 16 padding = 136

    mov r12, rdi            ; text pointer
    lea r13, [rsp]          ; presence_vals[30] array on stack

    ; Zero the array
    xor eax, eax
    mov ecx, 30
    lea rdi, [rsp]
.zero:
    mov dword [rdi], eax
    add rdi, 4
    dec ecx
    jnz .zero

    ; Check for empty response
    cmp byte [r12], 0
    je .sync_done

    ; Skip header line (find first newline)
    mov rdi, r12
.skip_header:
    cmp byte [rdi], 0
    je .sync_done
    cmp byte [rdi], 10      ; newline
    je .header_found
    inc rdi
    jmp .skip_header
.header_found:
    inc rdi                 ; skip past newline
    mov r12, rdi            ; r12 now points to first data line

    ; Parse lines: "index: int_part.frac_part\n"
    ; We expect lines 0-29 in order
    xor r14d, r14d          ; current dimension index
.parse_loop:
    cmp r14d, PRES_COUNT
    jge .apply_to_creature
    cmp byte [r12], 0
    je .apply_to_creature

    ; Skip the index number and ": " prefix
    ; Find ": " in current line
.find_colon:
    cmp byte [r12], 0
    je .apply_to_creature
    cmp byte [r12], 10
    je .next_line_skip      ; empty line, skip
    cmp byte [r12], ':'
    je .found_colon
    inc r12
    jmp .find_colon

.found_colon:
    inc r12                 ; skip ':'
    cmp byte [r12], ' '
    jne .parse_value
    inc r12                 ; skip space

.parse_value:
    ; Parse value: could be negative ("-0.523") or positive ("0.523")
    xor r15d, r15d          ; sign flag (0=pos, 1=neg)
    cmp byte [r12], '-'
    jne .no_neg
    mov r15d, 1
    inc r12
.no_neg:
    ; Parse integer part (before '.')
    xor ebx, ebx            ; integer accumulator
.parse_int:
    movzx eax, byte [r12]
    sub al, '0'
    cmp al, 9
    ja .int_done
    imul ebx, 10
    add ebx, eax
    inc r12
    jmp .parse_int
.int_done:
    ; ebx = integer part (usually 0 or 1)
    imul ebx, 1000          ; scale to 0-1000

    ; Check for decimal point
    cmp byte [r12], '.'
    jne .no_frac
    inc r12                 ; skip '.'

    ; Parse exactly 3 fraction digits (UHMA prints 3 via print_f32)
    xor ecx, ecx            ; frac accumulator

    ; Hundreds place
    movzx edx, byte [r12]
    sub dl, '0'
    cmp dl, 9
    ja .frac_done
    imul edx, 100
    add ecx, edx
    inc r12

    ; Tens place
    movzx edx, byte [r12]
    sub dl, '0'
    cmp dl, 9
    ja .frac_done
    imul edx, 10
    add ecx, edx
    inc r12

    ; Ones place
    movzx edx, byte [r12]
    sub dl, '0'
    cmp dl, 9
    ja .frac_done
    add ecx, edx
    inc r12

.no_frac:
.frac_done:
    ; Total value = int_part * 1000 + frac (already scaled)
    add ebx, ecx

    ; Apply sign
    test r15d, r15d
    jz .store_val
    neg ebx
    ; Clamp negative to 0
    test ebx, ebx
    jns .store_val
    xor ebx, ebx
.store_val:
    ; Clamp to 0-1000
    cmp ebx, 1000
    jle .clamp_ok
    mov ebx, 1000
.clamp_ok:
    test ebx, ebx
    jns .clamp_ok2
    xor ebx, ebx
.clamp_ok2:
    ; Store in presence_vals[r14d]
    mov [r13 + r14 * 4], ebx

    ; Skip to next line
.skip_to_eol:
    cmp byte [r12], 0
    je .apply_to_creature
    cmp byte [r12], 10
    je .next_line
    inc r12
    jmp .skip_to_eol

.next_line_skip:
    ; Empty line, just advance
.next_line:
    inc r12                 ; skip newline
    inc r14d
    jmp .parse_loop

.apply_to_creature:
    ; Check if we actually parsed anything (dim index > 0)
    test r14d, r14d
    jz .sync_done

    lea rbx, [rel creature_state]

    ; VALENCE ← presence[4] (accuracy = happiness)
    mov eax, [r13 + PRES_VALENCE * 4]
    ; Lerp: creature_val = creature_val + (target - creature_val) / 4
    mov ecx, [rbx + CS_VALENCE]
    sub eax, ecx
    sar eax, 2
    add ecx, eax
    mov [rbx + CS_VALENCE], ecx

    ; AROUSAL ← presence[3] (modification rate = energy)
    mov eax, [r13 + PRES_AROUSAL * 4]
    mov ecx, [rbx + CS_AROUSAL]
    sub eax, ecx
    sar eax, 2
    add ecx, eax
    mov [rbx + CS_AROUSAL], ecx

    ; CONFIDENCE ← presence[26] (familiarity)
    mov eax, [r13 + PRES_FAMILIARITY * 4]
    mov ecx, [rbx + CS_CONFIDENCE]
    sub eax, ecx
    sar eax, 2
    add ecx, eax
    mov [rbx + CS_CONFIDENCE], ecx

    ; SURPRISE ← presence[25] * scale (surprise events)
    ; Don't lerp surprise - it should spike
    mov eax, [r13 + PRES_SURPRISE * 4]
    ; Only update if UHMA surprise is higher than current
    cmp eax, [rbx + CS_SURPRISE]
    jle .no_surprise_update
    mov [rbx + CS_SURPRISE], eax
.no_surprise_update:

    ; SELF_AWARE ← presence[29] (meta-awareness)
    mov eax, [r13 + PRES_META_AWARE * 4]
    mov ecx, [rbx + CS_SELF_AWARE]
    sub eax, ecx
    sar eax, 2
    add ecx, eax
    mov [rbx + CS_SELF_AWARE], ecx

    ; FATIGUE ← (1000 - presence[11]) (low stability = tired)
    mov eax, 1000
    sub eax, [r13 + PRES_STABILITY * 4]
    mov ecx, [rbx + CS_FATIGUE]
    sub eax, ecx
    sar eax, 3              ; slower lerp for fatigue
    add ecx, eax
    ; Clamp 0-1000
    test ecx, ecx
    jns .fat_pos
    xor ecx, ecx
.fat_pos:
    cmp ecx, 1000
    jle .fat_ok
    mov ecx, 1000
.fat_ok:
    mov [rbx + CS_FATIGUE], ecx

    ; EAR_POS ← 300 + presence[2] * 0.7 (novelty perks ears)
    mov eax, [r13 + PRES_NOVELTY * 4]
    imul eax, 7
    xor edx, edx
    mov ecx, 10
    div ecx                 ; eax = novelty * 0.7
    add eax, 300            ; 300-1000 range
    ; Lerp
    mov ecx, [rbx + CS_EAR_POS]
    sub eax, ecx
    sar eax, 3
    add ecx, eax
    mov [rbx + CS_EAR_POS], ecx

    ; TAIL_POS ← 200 + presence[6] * 0.8 (engagement raises tail)
    mov eax, [r13 + PRES_ENGAGEMENT * 4]
    imul eax, 8
    xor edx, edx
    mov ecx, 10
    div ecx                 ; eax = engagement * 0.8
    add eax, 200            ; 200-1000 range
    ; Lerp
    mov ecx, [rbx + CS_TAIL_POS]
    sub eax, ecx
    sar eax, 3
    add ecx, eax
    mov [rbx + CS_TAIL_POS], ecx

    ; COGNITIVE_LOAD ← presence[16] (entropy = processing chaos)
    mov eax, [r13 + PRES_ENTROPY * 4]
    mov ecx, [rbx + CS_COGNITIVE_LOAD]
    sub eax, ecx
    sar eax, 2
    add ecx, eax
    ; Clamp 0-1000
    test ecx, ecx
    jns .cog_pos
    xor ecx, ecx
.cog_pos:
    cmp ecx, 1000
    jle .cog_ok
    mov ecx, 1000
.cog_ok:
    mov [rbx + CS_COGNITIVE_LOAD], ecx

    ; PATTERN_TYPE from resonance (dim 20) and depth (dim 18)
    ; high resonance (>500) = repetitive, low resonance + high depth = self-referential
    ; low resonance + low depth = novel, otherwise idle
    mov eax, [r13 + PRES_RESONANCE * 4]
    cmp eax, 500
    jg .pat_repetitive
    mov ecx, [r13 + PRES_DEPTH * 4]
    cmp ecx, 600
    jg .pat_selfref
    cmp eax, 200
    jl .pat_novel
    mov dword [rbx + CS_PATTERN_TYPE], 0       ; idle
    jmp .sync_done
.pat_repetitive:
    mov dword [rbx + CS_PATTERN_TYPE], 1
    jmp .sync_done
.pat_selfref:
    mov dword [rbx + CS_PATTERN_TYPE], 3
    jmp .sync_done
.pat_novel:
    mov dword [rbx + CS_PATTERN_TYPE], 2

.sync_done:
    add rsp, 136
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret
