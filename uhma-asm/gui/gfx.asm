; gfx.asm — Graphics primitives for X11 visualization
; Pure assembly graphics library using direct X11 drawing
;
; Link with: -lX11 -lc

section .data
    display_name:   db 0

section .bss
    ; X11 state
    display:        resq 1
    screen:         resd 1
    root_window:    resq 1
    window:         resq 1
    gc:             resq 1
    fb_width:       resd 1
    fb_height:      resd 1

    ; Colors
    color_black:    resq 1
    color_white:    resq 1

section .text

extern XOpenDisplay
extern XCloseDisplay
extern XCreateSimpleWindow
extern XMapWindow
extern XSelectInput
extern XNextEvent
extern XPending
extern XCreateGC
extern XFreeGC
extern XFlush
extern XDefaultScreen
extern XDefaultRootWindow
extern XSetForeground
extern XFillRectangle
extern XDrawRectangle
extern XDrawLine
extern XDrawPoint
extern XBlackPixel
extern XWhitePixel
extern XClearWindow

;; ============================================================
;; gfx_init — Initialize graphics system
;; edi = width, esi = height
;; Returns: rax = 1 on success, 0 on failure
;; ============================================================
global gfx_init
gfx_init:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 24             ; Local space + alignment (5 pushes = 40, +24 = 64)

    mov [rel fb_width], edi
    mov [rel fb_height], esi
    mov r14d, edi           ; save width
    mov r15d, esi           ; save height

    ; Open display
    xor edi, edi
    call XOpenDisplay
    test rax, rax
    jz .fail
    mov [rel display], rax
    mov r12, rax            ; display

    ; Get default screen
    mov rdi, r12
    call XDefaultScreen
    mov [rel screen], eax
    mov r13d, eax           ; screen

    ; Get root window
    mov rdi, r12
    call XDefaultRootWindow
    mov [rel root_window], rax

    ; Get black/white pixels
    mov rdi, r12
    mov esi, r13d
    call XBlackPixel
    mov [rel color_black], rax
    mov rbx, rax            ; black

    mov rdi, r12
    mov esi, r13d
    call XWhitePixel
    mov [rel color_white], rax

    ; Create window
    ; XCreateSimpleWindow(display, parent, x, y, width, height, border_width, border, background)
    mov rdi, r12
    mov rsi, [rel root_window]
    mov edx, 100            ; x
    mov ecx, 100            ; y
    mov r8d, r14d           ; width
    mov r9d, r15d           ; height
    ; Stack args
    mov qword [rsp], 2      ; border_width
    mov rax, [rel color_white]
    mov [rsp + 8], rax      ; border (white)
    mov [rsp + 16], rbx     ; background (black)
    call XCreateSimpleWindow
    mov [rel window], rax

    ; Create GC
    mov rdi, r12
    mov rsi, rax
    xor edx, edx
    xor ecx, ecx
    call XCreateGC
    mov [rel gc], rax

    ; Select input events
    mov rdi, r12
    mov rsi, [rel window]
    mov rdx, 0x020005       ; ExposureMask | KeyPressMask | ButtonPressMask
    call XSelectInput

    ; Map window
    mov rdi, r12
    mov rsi, [rel window]
    call XMapWindow

    ; Flush
    mov rdi, r12
    call XFlush

    mov eax, 1
    jmp .done

.fail:
    xor eax, eax

.done:
    add rsp, 24
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================
;; gfx_shutdown — Clean up
;; ============================================================
global gfx_shutdown
gfx_shutdown:
    push rbx
    mov rbx, [rel display]
    test rbx, rbx
    jz .done

    mov rdi, rbx
    mov rsi, [rel gc]
    call XFreeGC

    mov rdi, rbx
    call XCloseDisplay

.done:
    pop rbx
    ret

;; ============================================================
;; gfx_clear — Clear window to color
;; edi = color (0x00RRGGBB)
;; ============================================================
global gfx_clear
gfx_clear:
    push rbx
    push r12
    sub rsp, 8

    mov ebx, edi            ; save color

    ; Set foreground color
    mov rdi, [rel display]
    mov rsi, [rel gc]
    mov edx, ebx
    call XSetForeground

    ; Fill entire window
    mov rdi, [rel display]
    mov rsi, [rel window]
    mov rdx, [rel gc]
    xor ecx, ecx            ; x = 0
    xor r8d, r8d            ; y = 0
    mov r9d, [rel fb_width]
    mov eax, [rel fb_height]
    mov [rsp], rax          ; height on stack
    call XFillRectangle

    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; gfx_pixel — Draw pixel
;; edi = x, esi = y, edx = color
;; ============================================================
global gfx_pixel
gfx_pixel:
    push rbx
    push r12
    push r13
    sub rsp, 8

    mov r12d, edi           ; x
    mov r13d, esi           ; y
    mov ebx, edx            ; color

    ; Set color
    mov rdi, [rel display]
    mov rsi, [rel gc]
    mov edx, ebx
    call XSetForeground

    ; Draw point
    mov rdi, [rel display]
    mov rsi, [rel window]
    mov rdx, [rel gc]
    mov ecx, r12d
    mov r8d, r13d
    call XDrawPoint

    add rsp, 8
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; gfx_line — Draw line
;; edi = x0, esi = y0, edx = x1, ecx = y1, r8d = color
;; ============================================================
global gfx_line
gfx_line:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8

    mov r12d, edi           ; x0
    mov r13d, esi           ; y0
    mov r14d, edx           ; x1
    mov r15d, ecx           ; y1
    mov ebx, r8d            ; color

    ; Set color
    mov rdi, [rel display]
    mov rsi, [rel gc]
    mov edx, ebx
    call XSetForeground

    ; Draw line
    mov rdi, [rel display]
    mov rsi, [rel window]
    mov rdx, [rel gc]
    mov ecx, r12d
    mov r8d, r13d
    mov r9d, r14d
    mov eax, r15d
    mov [rsp], rax
    call XDrawLine

    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; gfx_rect — Rectangle outline
;; edi = x, esi = y, edx = w, ecx = h, r8d = color
;; ============================================================
global gfx_rect
gfx_rect:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8

    mov r12d, edi           ; x
    mov r13d, esi           ; y
    mov r14d, edx           ; w
    mov r15d, ecx           ; h
    mov ebx, r8d            ; color

    ; Set color
    mov rdi, [rel display]
    mov rsi, [rel gc]
    mov edx, ebx
    call XSetForeground

    ; Draw rectangle outline
    mov rdi, [rel display]
    mov rsi, [rel window]
    mov rdx, [rel gc]
    mov ecx, r12d
    mov r8d, r13d
    mov r9d, r14d
    mov eax, r15d
    mov [rsp], rax
    call XDrawRectangle

    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; gfx_fill_rect — Filled rectangle
;; edi = x, esi = y, edx = w, ecx = h, r8d = color
;; ============================================================
global gfx_fill_rect
gfx_fill_rect:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8

    mov r12d, edi           ; x
    mov r13d, esi           ; y
    mov r14d, edx           ; w
    mov r15d, ecx           ; h
    mov ebx, r8d            ; color

    ; Set color
    mov rdi, [rel display]
    mov rsi, [rel gc]
    mov edx, ebx
    call XSetForeground

    ; Fill rectangle
    mov rdi, [rel display]
    mov rsi, [rel window]
    mov rdx, [rel gc]
    mov ecx, r12d
    mov r8d, r13d
    mov r9d, r14d
    mov eax, r15d
    mov [rsp], rax
    call XFillRectangle

    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; gfx_flip — Flush display (no double buffering)
;; ============================================================
global gfx_flip
gfx_flip:
    push rbx
    mov rdi, [rel display]
    call XFlush
    pop rbx
    ret

;; ============================================================
;; gfx_poll_event — Non-blocking event poll
;; Returns: rax = event type or 0
;; ============================================================
global gfx_poll_event
gfx_poll_event:
    push rbp
    mov rbp, rsp
    sub rsp, 208            ; XEvent is 192 bytes, align to 16

    mov rdi, [rel display]
    call XPending
    test eax, eax
    jz .no_event

    mov rdi, [rel display]
    lea rsi, [rbp - 200]
    call XNextEvent

    mov eax, [rbp - 200]    ; event type
    jmp .done

.no_event:
    xor eax, eax

.done:
    add rsp, 208
    pop rbp
    ret
