; viz_main.asm — Main entry point for UHMA GUI (MCP wrapper)
; Visual interface that communicates with UHMA via MCP server
;
; @entry main() -> exit code
; @calls mcp_client.asm:mcp_init, mcp_shutdown
; @calls visualizer.asm:vis_init, vis_update, vis_shutdown
;
; FLOW: main → mcp_init → vis_init → loop(vis_update, usleep) → vis_shutdown → mcp_shutdown
;
; GOTCHAS:
;   - Link with: gcc -lX11 -lc -no-pie (NOT ld directly)
;   - MCP server spawned as subprocess (python3 tools/rag/server.py)
;   - vis_update returns 0 to quit, 1 to continue
;   - 16ms usleep ≈ 60fps frame rate
;   - Stack alignment: ODD pushes need sub rsp,16; EVEN pushes need sub rsp,8
;

section .data
    banner: db "UHMA GUI - MCP Interface", 10, 0
    init_fail: db "Failed to initialize graphics", 10, 0
    mcp_fail: db "Failed to start MCP server", 10, 0

section .bss

section .text

; Libc
extern printf
extern usleep
extern exit

; MCP client
extern mcp_init
extern mcp_shutdown

; Visualizer
extern vis_init
extern vis_shutdown
extern vis_update

global main
main:
    push rbx
    push r12
    sub rsp, 8

    ; Print banner
    lea rdi, [rel banner]
    xor eax, eax
    call printf

    ; Initialize MCP connection (spawns server + UHMA)
    call mcp_init
    test eax, eax
    jz .mcp_fail

    ; Initialize visualizer (includes gfx_init)
    xor edi, edi            ; no surface pointer needed
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

.mcp_fail:
    lea rdi, [rel mcp_fail]
    xor eax, eax
    call printf
    mov edi, 1
    call exit
