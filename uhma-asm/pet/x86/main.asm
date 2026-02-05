; main.asm — Pet creature prototype entry point
;
; @entry main() -> exit code
; @calls gfx.asm, creature.asm, render.asm
;
; Keyboard:
;   f     = feed (valence boost, arousal spike)
;   d     = dream (trigger sleep cycle)
;   space = poke (arousal spike, surprise)
;   q     = quit
;
; GOTCHAS:
;   - Link with: gcc -lX11 -lc -lm -no-pie
;   - 16ms usleep ≈ 60fps

section .text

extern gfx_init, gfx_shutdown, gfx_clear, gfx_flip
extern gfx_poll_event, gfx_get_last_keycode
extern creature_init, creature_update
extern creature_feed, creature_poke, creature_dream
extern render_creature, render_debug_overlay
extern usleep, exit

global main
main:
    push rbx
    sub rsp, 16             ; 1 push (odd) → aligned → sub multiple of 16

    ; Init graphics 800x600
    mov edi, 800
    mov esi, 600
    call gfx_init
    test eax, eax
    jz .fail

    ; Init creature
    call creature_init

.loop:
    ; Poll all pending events
.event_loop:
    call gfx_poll_event
    test eax, eax
    jz .no_more_events

    cmp eax, 2              ; KeyPress
    jne .event_loop

    call gfx_get_last_keycode

    cmp eax, 24             ; 'q' key
    je .done
    cmp eax, 41             ; 'f' key
    je .do_feed
    cmp eax, 40             ; 'd' key
    je .do_dream
    cmp eax, 65             ; space
    je .do_poke
    jmp .event_loop

.do_feed:
    call creature_feed
    jmp .event_loop
.do_dream:
    call creature_dream
    jmp .event_loop
.do_poke:
    call creature_poke
    jmp .event_loop

.no_more_events:
    ; Update creature state + animation
    call creature_update

    ; Clear screen (dark blue-grey)
    mov edi, 0x1a1a2e
    call gfx_clear

    ; Draw creature
    call render_creature

    ; Draw debug state overlay
    call render_debug_overlay

    ; Flip
    call gfx_flip

    ; Sleep 16ms ≈ 60fps
    mov edi, 16000
    call usleep

    jmp .loop

.done:
    call gfx_shutdown
    xor edi, edi
    call exit

.fail:
    mov edi, 1
    call exit
