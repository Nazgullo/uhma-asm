; render.asm — Creature renderer for carousel expanded panel
;
; @entry render_creature_panel(edi=x, esi=y, edx=w, ecx=h) -> draws creature in rect
; @calls gfx.asm:gfx_fill_rect, gfx.asm:gfx_rect, gfx.asm:gfx_line, gfx.asm:gfx_text
; @calledby visualizer.asm:focus_creature
;
; Draws a hound creature from creature_state fields.
; All positions are relative to the panel bounds passed in.
;
; GOTCHAS:
;   - Stack alignment: all gfx calls need 16-byte aligned stack
;   - creature_state is extern global from creature.asm
;   - Colors come from creature_state (CS_BODY_COLOR, CS_EYE_COLOR)
;   - Tail wag uses triangle wave from CS_TAIL_PHASE
;   - Breathing uses triangle wave from CS_BREATH_PHASE

section .data
    mood_str_0: db "PANIC!", 0
    mood_str_1: db "anxious", 0
    mood_str_2: db "neutral", 0
    mood_str_3: db "content", 0
    mood_str_4: db "happy!", 0
    mood_str_5: db "VIGOROUS!", 0
global mood_strs
    mood_strs:  dq mood_str_0, mood_str_1, mood_str_2, mood_str_3, mood_str_4, mood_str_5
global mood_lens
    mood_lens:  db 6, 7, 7, 7, 6, 9

    sleep_str:  db "z Z z Z", 0
    dream_str:  db "~ dreaming ~", 0

    ; Status labels
    lbl_val:    db "VAL:", 0
    lbl_aro:    db "ARO:", 0
    lbl_fat:    db "FAT:", 0
    lbl_cnf:    db "CNF:", 0

    ; Creature name
    lbl_name:   db "CREATURE", 0

    ; Bar fill char
    bar_fill:   db 0xDB, 0xDB, 0xDB, 0xDB, 0xDB, 0xDB, 0xDB, 0xDB
                db 0xDB, 0xDB, 0xDB, 0xDB, 0xDB, 0xDB, 0xDB, 0xDB
                db 0xDB, 0xDB, 0xDB, 0xDB, 0

    ; Colors
    col_body_shadow: dd 0x008B6914
    col_nose:        dd 0x002A1A0A
    col_mouth:       dd 0x003D2B1F
    col_pupil:       dd 0x00000000
    col_highlight:   dd 0x00FFE4B5   ; moccasin highlight
    col_sleep_bg:    dd 0x00222244
    col_dream_fg:    dd 0x008888FF
    col_bar_bg:      dd 0x00333333
    col_bar_val:     dd 0x0044CC44   ; green for valence
    col_bar_aro:     dd 0x00FF8844   ; orange for arousal
    col_bar_fat:     dd 0x00CC4444   ; red for fatigue
    col_bar_cnf:     dd 0x004488FF   ; blue for confidence
    col_white:       dd 0x00FFFFFF
    col_text_dim:    dd 0x00888888

section .text

extern gfx_fill_rect, gfx_rect, gfx_line, gfx_text
extern creature_state

;; CreatureState offsets (from creature.asm)
CS_SPECIES      equ 0
CS_MATURITY     equ 1
CS_SLEEPING     equ 2
CS_DREAMING     equ 3
CS_VALENCE      equ 4
CS_AROUSAL      equ 8
CS_FATIGUE      equ 12
CS_CONFIDENCE   equ 16
CS_SURPRISE     equ 20
CS_SELF_AWARE   equ 24
CS_POS_X        equ 28
CS_POS_Y        equ 32
CS_TARGET_X     equ 36
CS_TARGET_Y     equ 40
CS_EYE_OPEN     equ 44
CS_EAR_POS      equ 48
CS_TAIL_POS     equ 52
CS_TAIL_PHASE   equ 56
CS_BREATH_PHASE equ 60
CS_FRAME        equ 64
CS_BODY_COLOR   equ 68
CS_EYE_COLOR    equ 72
CS_BLINK_TIMER  equ 76
CS_BLINK_DUR    equ 80
CS_MOOD         equ 84
CS_WAG_SPEED    equ 88
CS_WANDER_TMR   equ 92
CS_BOUNCE_PHASE equ 96

;; ============================================================
;; render_creature_panel — Draw creature inside panel bounds
;; edi=panel_x, esi=panel_y, edx=panel_w, ecx=panel_h
;; ============================================================
global render_creature_panel
render_creature_panel:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 56             ; 6 pushes (even) → unaligned → sub 8 mod 16 = 56 (48+8)

    ; Save panel bounds
    mov [rbp - 48], edi     ; panel_x
    mov [rbp - 44], esi     ; panel_y
    mov [rbp - 40], edx     ; panel_w
    mov [rbp - 36], ecx     ; panel_h

    lea rbx, [rel creature_state]

    ; Compute creature center in panel
    ; cx = panel_x + panel_w/2
    ; cy = panel_y + panel_h/2 + 20 (lower half for creature, upper for stats)
    mov eax, edx
    shr eax, 1
    add eax, edi
    sub eax, 30             ; shift creature 30px left
    mov r12d, eax           ; cx = center x

    mov eax, ecx
    shr eax, 1
    add eax, esi
    add eax, 30             ; push creature down a bit
    mov r13d, eax           ; cy = center y

    ; === Bounce offset ===
    mov eax, [rbx + CS_BOUNCE_PHASE]
    ; Triangle wave: if < 1800, offset = phase/180, else offset = (3600-phase)/180
    cmp eax, 1800
    jl .bounce_up
    mov ecx, 3600
    sub ecx, eax
    mov eax, ecx
.bounce_up:
    xor edx, edx
    mov ecx, 180
    div ecx                 ; eax = 0-10
    sub r13d, eax           ; bounce up

    ; === Breathing offset ===
    mov eax, [rbx + CS_BREATH_PHASE]
    cmp eax, 1800
    jl .breath_up
    mov ecx, 3600
    sub ecx, eax
    mov eax, ecx
.breath_up:
    xor edx, edx
    mov ecx, 900
    div ecx                 ; 0-2
    mov r14d, eax           ; breath_expand (0-2 pixels)

    ; === Sleep overlay ===
    cmp byte [rbx + CS_SLEEPING], 1
    jne .not_sleeping_bg

    ; Dim background for sleeping
    mov edi, [rbp - 48]
    add edi, 5
    mov esi, [rbp - 44]
    add esi, 5
    mov edx, [rbp - 40]
    sub edx, 10
    mov ecx, [rbp - 36]
    sub ecx, 10
    mov r8d, [rel col_sleep_bg]
    call gfx_fill_rect
.not_sleeping_bg:

    ; === BODY (main oval approximation) ===
    ; Body: 80x44 rect centered at (cx, cy), with breath expand
    mov edi, r12d
    sub edi, 40
    sub edi, r14d           ; wider when breathing in
    mov esi, r13d
    sub esi, 22
    mov edx, 80
    lea edx, [edx + r14d*2]
    mov ecx, 44
    add ecx, r14d
    mov r8d, [rbx + CS_BODY_COLOR]
    call gfx_fill_rect

    ; Body highlight (lighter stripe on top)
    mov edi, r12d
    sub edi, 30
    mov esi, r13d
    sub esi, 18
    mov edx, 60
    mov ecx, 8
    mov r8d, [rel col_highlight]
    call gfx_fill_rect

    ; === HEAD ===
    ; Head: 36x30 rect above body center
    mov edi, r12d
    sub edi, 18
    mov esi, r13d
    sub esi, 52
    mov edx, 36
    mov ecx, 30
    mov r8d, [rbx + CS_BODY_COLOR]
    call gfx_fill_rect

    ; === EARS ===
    ; Ear position: 0=flat, 500=neutral, 1000=perked
    ; Left ear
    mov eax, [rbx + CS_EAR_POS]
    ; ear height = ear_pos / 100 + 3 (range 3-13)
    xor edx, edx
    mov ecx, 100
    div ecx
    add eax, 3
    mov r15d, eax           ; ear_h

    ; Left ear
    mov edi, r12d
    sub edi, 20
    mov esi, r13d
    sub esi, 52
    sub esi, r15d
    mov edx, 8
    mov ecx, r15d
    mov r8d, [rbx + CS_BODY_COLOR]
    call gfx_fill_rect

    ; Right ear
    mov edi, r12d
    add edi, 12
    mov esi, r13d
    sub esi, 52
    sub esi, r15d
    mov edx, 8
    mov ecx, r15d
    mov r8d, [rbx + CS_BODY_COLOR]
    call gfx_fill_rect

    ; Ear tips (darker)
    mov edi, r12d
    sub edi, 19
    mov esi, r13d
    sub esi, 52
    sub esi, r15d
    mov edx, 6
    mov ecx, 3
    mov r8d, [rel col_mouth]
    call gfx_fill_rect

    mov edi, r12d
    add edi, 13
    mov esi, r13d
    sub esi, 52
    sub esi, r15d
    mov edx, 6
    mov ecx, 3
    mov r8d, [rel col_mouth]
    call gfx_fill_rect

    ; === EYES ===
    ; Eye openness: 0-1000 → height 0-8
    mov eax, [rbx + CS_EYE_OPEN]
    xor edx, edx
    mov ecx, 125
    div ecx                 ; 0-8
    mov r15d, eax
    test r15d, r15d
    jz .skip_eyes           ; fully closed

    ; Left eye white
    mov edi, r12d
    sub edi, 12
    mov esi, r13d
    sub esi, 44
    mov eax, 8
    sub eax, r15d
    shr eax, 1
    add esi, eax            ; center vertically
    mov edx, 8
    mov ecx, r15d
    mov r8d, 0x00FFFFFF
    call gfx_fill_rect

    ; Left pupil (centered in eye)
    cmp r15d, 3
    jl .skip_left_pupil
    mov edi, r12d
    sub edi, 10
    mov esi, r13d
    sub esi, 42
    mov edx, 4
    mov ecx, 4
    cmp r15d, 4
    jge .left_pupil_ok
    mov ecx, r15d
.left_pupil_ok:
    mov r8d, [rbx + CS_EYE_COLOR]
    call gfx_fill_rect
.skip_left_pupil:

    ; Right eye white
    mov edi, r12d
    add edi, 4
    mov esi, r13d
    sub esi, 44
    mov eax, 8
    sub eax, r15d
    shr eax, 1
    add esi, eax
    mov edx, 8
    mov ecx, r15d
    mov r8d, 0x00FFFFFF
    call gfx_fill_rect

    ; Right pupil
    cmp r15d, 3
    jl .skip_eyes
    mov edi, r12d
    add edi, 6
    mov esi, r13d
    sub esi, 42
    mov edx, 4
    mov ecx, 4
    cmp r15d, 4
    jge .right_pupil_ok
    mov ecx, r15d
.right_pupil_ok:
    mov r8d, [rbx + CS_EYE_COLOR]
    call gfx_fill_rect

.skip_eyes:

    ; === NOSE ===
    mov edi, r12d
    sub edi, 3
    mov esi, r13d
    sub esi, 30
    mov edx, 6
    mov ecx, 4
    mov r8d, [rel col_nose]
    call gfx_fill_rect

    ; === MOUTH (simple line when happy) ===
    mov eax, [rbx + CS_VALENCE]
    cmp eax, 600
    jl .no_smile
    ; Smile: short line below nose
    mov edi, r12d
    sub edi, 5
    mov esi, r13d
    sub esi, 24
    mov edx, r12d
    add edx, 5
    mov ecx, r13d
    sub ecx, 22
    mov r8d, [rel col_mouth]
    call gfx_line
    jmp .mouth_done
.no_smile:
    cmp eax, 300
    jg .mouth_done
    ; Frown: inverted
    mov edi, r12d
    sub edi, 5
    mov esi, r13d
    sub esi, 22
    mov edx, r12d
    add edx, 5
    mov ecx, r13d
    sub ecx, 24
    mov r8d, [rel col_mouth]
    call gfx_line
.mouth_done:

    ; === LEGS ===
    ; 4 legs below body
    ; Front left
    mov edi, r12d
    sub edi, 28
    mov esi, r13d
    add esi, 22
    mov edx, 10
    mov ecx, 18
    mov r8d, [rbx + CS_BODY_COLOR]
    call gfx_fill_rect

    ; Front right
    mov edi, r12d
    sub edi, 10
    mov esi, r13d
    add esi, 22
    mov edx, 10
    mov ecx, 18
    mov r8d, [rbx + CS_BODY_COLOR]
    call gfx_fill_rect

    ; Back left
    mov edi, r12d
    add edi, 2
    mov esi, r13d
    add esi, 22
    mov edx, 10
    mov ecx, 18
    mov r8d, [rbx + CS_BODY_COLOR]
    call gfx_fill_rect

    ; Back right
    mov edi, r12d
    add edi, 20
    mov esi, r13d
    add esi, 22
    mov edx, 10
    mov ecx, 18
    mov r8d, [rbx + CS_BODY_COLOR]
    call gfx_fill_rect

    ; Paws (darker tips)
    mov edi, r12d
    sub edi, 28
    mov esi, r13d
    add esi, 36
    mov edx, 10
    mov ecx, 4
    mov r8d, [rel col_mouth]
    call gfx_fill_rect

    mov edi, r12d
    sub edi, 10
    mov esi, r13d
    add esi, 36
    mov edx, 10
    mov ecx, 4
    mov r8d, [rel col_mouth]
    call gfx_fill_rect

    mov edi, r12d
    add edi, 2
    mov esi, r13d
    add esi, 36
    mov edx, 10
    mov ecx, 4
    mov r8d, [rel col_mouth]
    call gfx_fill_rect

    mov edi, r12d
    add edi, 20
    mov esi, r13d
    add esi, 36
    mov edx, 10
    mov ecx, 4
    mov r8d, [rel col_mouth]
    call gfx_fill_rect

    ; === TAIL ===
    ; Tail wag: phase 0-3600 → x offset -15 to +15
    mov eax, [rbx + CS_TAIL_PHASE]
    ; Triangle wave centered at 0
    cmp eax, 900
    jl .tail_q1
    cmp eax, 2700
    jl .tail_q23
    ; Q4: 2700-3600 → rising from -15 to 0
    sub eax, 2700
    ; eax = 0-900, want -15 to 0
    xor edx, edx
    mov ecx, 60
    div ecx             ; 0-15
    sub eax, 15
    jmp .tail_offset_done
.tail_q1:
    ; Q1: 0-900 → rising from 0 to +15
    xor edx, edx
    mov ecx, 60
    div ecx             ; 0-15
    jmp .tail_offset_done
.tail_q23:
    ; Q2-Q3: 900-2700 → falling from +15 to -15
    sub eax, 900
    ; eax = 0-1800, want +15 to -15
    xor edx, edx
    mov ecx, 60
    div ecx             ; 0-30
    mov ecx, 15
    sub ecx, eax
    mov eax, ecx       ; +15 to -15
.tail_offset_done:
    mov r15d, eax       ; tail_wag_offset

    ; Tail height based on tail_pos (0-1000 → y offset 20 to -20)
    mov eax, [rbx + CS_TAIL_POS]
    xor edx, edx
    mov ecx, 25
    div ecx             ; 0-40
    sub eax, 20         ; -20 to +20
    neg eax             ; high tail_pos = higher tail
    mov ecx, eax        ; tail_y_offset

    ; Draw tail: line from body rear to tip
    ; ecx = tail_y_offset, r15d = tail_wag_offset
    ; Compute tip_y before setting up args
    mov r14d, r13d
    sub r14d, 15
    add r14d, ecx       ; tip_y = cy - 15 + tail_y_offset

    mov edi, r12d
    add edi, 38         ; x0 = tail base (right side of body)
    mov esi, r13d
    sub esi, 5          ; y0 = slightly above center
    mov edx, r12d
    add edx, 55
    add edx, r15d       ; x1 = tip with wag
    mov ecx, r14d       ; y1 = tip_y
    mov r8d, [rbx + CS_BODY_COLOR]
    call gfx_line

    ; Tail tuft (small rect at tip)
    mov eax, [rbx + CS_TAIL_POS]
    xor edx, edx
    mov ecx, 25
    div ecx
    sub eax, 20
    neg eax

    mov edi, r12d
    add edi, 52
    add edi, r15d
    mov esi, r13d
    sub esi, 18
    add esi, eax
    mov edx, 8
    mov ecx, 6
    mov r8d, [rbx + CS_BODY_COLOR]
    call gfx_fill_rect

    ; === SLEEP INDICATORS ===
    cmp byte [rbx + CS_SLEEPING], 1
    jne .no_sleep_text

    ; "z Z z Z" floating above head
    mov eax, [rbx + CS_FRAME]
    and eax, 0x3F          ; 0-63 frames
    shr eax, 3             ; 0-7 offset
    mov edi, r12d
    add edi, 20
    mov esi, r13d
    sub esi, 70
    sub esi, eax           ; float upward
    lea rdx, [rel sleep_str]
    mov ecx, 7
    mov r8d, [rel col_white]
    call gfx_text

    cmp byte [rbx + CS_DREAMING], 1
    jne .no_sleep_text
    mov edi, r12d
    sub edi, 30
    mov esi, r13d
    add esi, 55
    lea rdx, [rel dream_str]
    mov ecx, 12
    mov r8d, [rel col_dream_fg]
    call gfx_text
.no_sleep_text:

    ; === STATUS BARS (upper portion of panel) ===
    mov r12d, [rbp - 48]   ; panel_x
    add r12d, 15            ; left margin
    mov r13d, [rbp - 44]   ; panel_y
    add r13d, 15            ; top margin

    ; Title: "CREATURE"
    mov edi, [rbp - 48]
    mov eax, [rbp - 40]
    shr eax, 1
    add edi, eax
    sub edi, 24             ; center "CREATURE" (8 chars * ~6px)
    mov esi, r13d
    lea rdx, [rel lbl_name]
    mov ecx, 8
    mov r8d, [rel col_white]
    call gfx_text

    ; Mood text
    mov eax, [rbx + CS_MOOD]
    cmp eax, 5
    jg .mood_clamp
    test eax, eax
    jns .mood_ok
.mood_clamp:
    xor eax, eax
.mood_ok:
    lea rcx, [rel mood_strs]
    mov rdx, [rcx + rax*8]  ; mood string pointer
    lea rcx, [rel mood_lens]
    movzx ecx, byte [rcx + rax]  ; mood string length

    mov edi, [rbp - 48]
    mov eax, [rbp - 40]
    shr eax, 1
    add edi, eax
    sub edi, 20
    lea esi, [r13d + 14]
    ; Color based on mood
    mov r8d, [rel col_text_dim]
    mov eax, [rbx + CS_MOOD]
    cmp eax, 4
    jl .mood_color_ok
    mov r8d, 0x0044FF44     ; green for happy+
.mood_color_ok:
    cmp eax, 1
    jg .mood_color_done
    mov r8d, 0x00FF4444     ; red for panic/anxious
.mood_color_done:
    call gfx_text

    ; === Mini status bars ===
    ; VAL bar
    lea edi, [r12d]
    lea esi, [r13d + 32]
    lea rdx, [rel lbl_val]
    mov ecx, 4
    mov r8d, [rel col_text_dim]
    call gfx_text

    ; Bar background
    lea edi, [r12d + 30]
    lea esi, [r13d + 28]
    mov edx, 80
    mov ecx, 8
    mov r8d, [rel col_bar_bg]
    call gfx_fill_rect

    ; Bar fill (valence/1000 * 80)
    mov eax, [rbx + CS_VALENCE]
    imul eax, 80
    xor edx, edx
    mov ecx, 1000
    div ecx
    test eax, eax
    jz .skip_val_bar
    lea edi, [r12d + 30]
    lea esi, [r13d + 28]
    mov edx, eax
    mov ecx, 8
    mov r8d, [rel col_bar_val]
    call gfx_fill_rect
.skip_val_bar:

    ; ARO bar
    lea edi, [r12d + 120]
    lea esi, [r13d + 32]
    lea rdx, [rel lbl_aro]
    mov ecx, 4
    mov r8d, [rel col_text_dim]
    call gfx_text

    lea edi, [r12d + 150]
    lea esi, [r13d + 28]
    mov edx, 80
    mov ecx, 8
    mov r8d, [rel col_bar_bg]
    call gfx_fill_rect

    mov eax, [rbx + CS_AROUSAL]
    imul eax, 80
    xor edx, edx
    mov ecx, 1000
    div ecx
    test eax, eax
    jz .skip_aro_bar
    lea edi, [r12d + 150]
    lea esi, [r13d + 28]
    mov edx, eax
    mov ecx, 8
    mov r8d, [rel col_bar_aro]
    call gfx_fill_rect
.skip_aro_bar:

    ; FAT bar
    lea edi, [r12d + 240]
    lea esi, [r13d + 32]
    lea rdx, [rel lbl_fat]
    mov ecx, 4
    mov r8d, [rel col_text_dim]
    call gfx_text

    lea edi, [r12d + 270]
    lea esi, [r13d + 28]
    mov edx, 80
    mov ecx, 8
    mov r8d, [rel col_bar_bg]
    call gfx_fill_rect

    mov eax, [rbx + CS_FATIGUE]
    imul eax, 80
    xor edx, edx
    mov ecx, 1000
    div ecx
    test eax, eax
    jz .skip_fat_bar
    lea edi, [r12d + 270]
    lea esi, [r13d + 28]
    mov edx, eax
    mov ecx, 8
    mov r8d, [rel col_bar_fat]
    call gfx_fill_rect
.skip_fat_bar:

    add rsp, 56
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret
