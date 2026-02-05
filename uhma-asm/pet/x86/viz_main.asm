; viz_main.asm — Main entry point for UHMA GUI
; Visual interface - UHMA spawned on user action (DREAM or FEED)
;
; @entry main() -> exit code
; @calls visualizer.asm:vis_init, vis_update, vis_shutdown
; @calls mcp_client.asm:mcp_shutdown
;
; STARTUP FLOW:
;   1. Initialize visualizer (shows GUI)
;   2. User clicks DREAM → spawn UHMA with batch OFF (live/autonomous mode)
;   3. User clicks FEED → show config, then spawn UHMA with batch ON (feeding mode)
;   4. Main loop until quit
;
; GOTCHAS:
;   - Link with: gcc -lX11 -lc -no-pie (NOT ld directly)
;   - vis_update returns 0 to quit, 1 to continue
;   - 16ms usleep ≈ 60fps frame rate

section .data
    banner: db "UHMA GUI - Command & Control", 10, 0
    init_fail: db "Failed to initialize graphics", 10, 0

section .text

; Libc
extern printf
extern usleep
extern exit
extern signal

; MCP client
extern mcp_shutdown

; Visualizer
extern vis_init
extern vis_shutdown
extern vis_update

global main
main:
    push rbx
    sub rsp, 16                     ; 1 push (odd) -> aligned, sub must be multiple of 16

    ; Ignore SIGPIPE (broken pipe from TCP send to disconnected UHMA)
    mov edi, 13             ; SIGPIPE = 13
    mov rsi, 1              ; SIG_IGN = 1
    call signal

    ; Print banner
    lea rdi, [rel banner]
    xor eax, eax
    call printf

    ; Initialize visualizer (shows GUI, no UHMA yet)
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
    mov edi, 1
    call exit
