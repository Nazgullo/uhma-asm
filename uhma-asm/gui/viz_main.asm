; viz_main.asm — Main entry point for UHMA Visualizer
; Links with libc for X11 support
;
%include "constants.inc"

section .data
    banner: db "UHMA Visualizer - Real-time system transparency", 10
            db "Press any key to exit", 10, 0
    init_fail: db "Failed to initialize graphics", 10, 0
    surface_fail: db "Failed to initialize surface", 10, 0

section .bss
    surface_base: resq 1

section .text

; Libc
extern printf
extern usleep
extern exit

; UHMA
extern surface_init
extern dispatch_init

; Visualizer
extern vis_init
extern vis_shutdown
extern vis_update

global main
main:
    push rbx
    push r12
    sub rsp, 8              ; 2 pushes + 8 = 24, rsp % 16 == 0

    ; Print banner
    lea rdi, [rel banner]
    xor eax, eax
    call printf

    ; Initialize surface
    call surface_init
    test rax, rax
    jz .surface_fail
    mov [rel surface_base], rax
    mov r12, rax

    ; Initialize dispatch
    call dispatch_init

    ; Initialize visualizer (includes gfx_init)
    call vis_init
    test eax, eax
    jz .gfx_fail

    ; Main loop
.loop:
    ; Update visualization
    mov rdi, r12
    call vis_update
    test eax, eax
    jz .done

    ; Small delay (16ms ~ 60fps)
    mov edi, 16000
    call usleep

    jmp .loop

.done:
    call vis_shutdown
    xor edi, edi
    call exit

.gfx_fail:
    lea rdi, [rel init_fail]
    xor eax, eax
    call printf
    mov edi, 1
    call exit

.surface_fail:
    lea rdi, [rel surface_fail]
    xor eax, eax
    call printf
    mov edi, 1
    call exit
