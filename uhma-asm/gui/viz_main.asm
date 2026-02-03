; viz_main.asm — Main entry point for UHMA GUI (MCP wrapper)
; Visual interface that communicates with UHMA via MCP server
;
; @entry main() -> exit code
; @calls mcp_client.asm:mcp_init, mcp_try_connect, mcp_spawn_uhma, mcp_shutdown
; @calls visualizer.asm:vis_init, vis_update, vis_shutdown
;
; STARTUP FLOW:
;   1. Try to connect to existing UHMA
;   2. If connected → proceed to main loop
;   3. If not connected → show 10s countdown
;   4. If 'S' pressed → wait for server mode (external UHMA start)
;   5. If countdown expires → spawn UHMA directly in live mode
;
; GOTCHAS:
;   - Link with: gcc -lX11 -lc -no-pie (NOT ld directly)
;   - vis_update returns 0 to quit, 1 to continue
;   - 16ms usleep ≈ 60fps frame rate

section .data
    banner: db "UHMA GUI - MCP Interface", 10, 0
    init_fail: db "Failed to initialize graphics", 10, 0
    mcp_fail: db "Failed to start MCP server", 10, 0
    countdown_msg: db "Starting UHMA in %d seconds... Press 'S' for server mode", 10, 0
    server_wait: db "Waiting for server... Press any key when ready", 10, 0
    spawning_msg: db "Spawning UHMA in live mode...", 10, 0
    countdown_text: db "Starting UHMA in    seconds...", 0
    server_hint:    db "Press 'S' for server mode", 0
    wait_text:      db "Waiting for server connection...", 0

section .bss
    countdown_secs: resd 1
    sec_char:       resb 4          ; for digit display

section .text

; Libc
extern printf
extern usleep
extern exit

; MCP client
extern mcp_init
extern mcp_try_connect
extern mcp_spawn_uhma
extern mcp_shutdown

; Visualizer
extern vis_init
extern vis_shutdown
extern vis_update

; Graphics (for countdown display)
extern gfx_init
extern gfx_shutdown
extern gfx_clear
extern gfx_text
extern gfx_flip
extern gfx_poll_event
extern gfx_get_last_keycode

global main
main:
    push rbx
    push r12
    push r13
    sub rsp, 16                     ; 3 pushes (odd) -> aligned, sub must be multiple of 16

    ; Print banner
    lea rdi, [rel banner]
    xor eax, eax
    call printf

    ; Try to connect to existing UHMA
    call mcp_try_connect
    test eax, eax
    jnz .connected

    ; Not connected - show countdown
    ; Initialize basic graphics for countdown display
    mov edi, 1280
    mov esi, 720
    call gfx_init
    test eax, eax
    jz .gfx_fail_early

    ; Countdown loop (10 seconds)
    mov dword [rel countdown_secs], 10

.countdown_loop:
    ; Clear screen (dark blue)
    mov edi, 0x001a1a2e
    call gfx_clear

    ; Draw countdown text
    mov edi, 400                    ; x
    mov esi, 340                    ; y
    lea rdx, [rel countdown_text]
    mov ecx, 45                     ; length
    mov r8d, 0x00e0e0e0             ; white
    call gfx_text

    ; Draw seconds number
    mov eax, [rel countdown_secs]
    add eax, '0'                    ; convert to ASCII (works for 0-9)
    mov [rel sec_char], al
    mov edi, 570
    mov esi, 340
    lea rdx, [rel sec_char]
    mov ecx, 1
    mov r8d, 0x0000ff00             ; green
    call gfx_text

    ; Draw server mode hint
    mov edi, 420
    mov esi, 380
    lea rdx, [rel server_hint]
    mov ecx, 25
    mov r8d, 0x00808080             ; gray
    call gfx_text

    call gfx_flip

    ; Poll events for 1 second (check for 'S' key)
    mov r12d, 50                    ; 50 * 20ms = 1 second

.poll_second:
    call gfx_poll_event
    cmp eax, 2                      ; KeyPress
    jne .no_key
    call gfx_get_last_keycode
    cmp eax, 39                     ; 'S' key (keycode may vary)
    je .server_mode
    cmp eax, 52                     ; lowercase 's' alternate
    je .server_mode
.no_key:
    mov edi, 20000                  ; 20ms
    call usleep
    dec r12d
    jnz .poll_second

    ; Decrement countdown
    dec dword [rel countdown_secs]
    jnz .countdown_loop

    ; Countdown expired - spawn UHMA
    lea rdi, [rel spawning_msg]
    xor eax, eax
    call printf

    call gfx_shutdown               ; close countdown window
    call mcp_spawn_uhma
    test eax, eax
    jz .mcp_fail
    jmp .connected

.server_mode:
    ; User pressed S - wait for server
    ; Clear and show waiting message
    mov edi, 0x001a1a2e
    call gfx_clear
    mov edi, 400
    mov esi, 360
    lea rdx, [rel wait_text]
    mov ecx, 35
    mov r8d, 0x00ffff00             ; yellow
    call gfx_text
    call gfx_flip

    ; Poll until UHMA connects or user presses key
.wait_loop:
    call mcp_try_connect
    test eax, eax
    jnz .server_connected

    call gfx_poll_event
    cmp eax, 2                      ; KeyPress
    je .try_connect_again

    mov edi, 500000                 ; 500ms
    call usleep
    jmp .wait_loop

.try_connect_again:
    call mcp_try_connect
    test eax, eax
    jz .wait_loop

.server_connected:
    call gfx_shutdown               ; close countdown window

.connected:
    ; Initialize visualizer (includes gfx_init)
    xor edi, edi
    call vis_init
    test eax, eax
    jz .gfx_fail

    ; Main loop
.loop:
    xor edi, edi
    call vis_update
    test eax, eax
    jz .done

    ; Small delay (16ms ~ 60fps)
    mov edi, 16000
    call usleep

    jmp .loop

.done:
    call vis_shutdown
    call mcp_shutdown
    xor edi, edi
    call exit

.gfx_fail:
    lea rdi, [rel init_fail]
    xor eax, eax
    call printf
    call mcp_shutdown
    mov edi, 1
    call exit

.gfx_fail_early:
    lea rdi, [rel init_fail]
    xor eax, eax
    call printf
    mov edi, 1
    call exit

.mcp_fail:
    lea rdi, [rel mcp_fail]
    xor eax, eax
    call printf
    mov edi, 1
    call exit

