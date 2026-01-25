; gfx.asm — Graphics primitives for X11 visualization
; Pure assembly graphics library
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
    visual:         resq 1
    depth:          resd 1
    colormap:       resq 1
    ximage:         resq 1
    framebuf:       resq 1
    fb_width:       resd 1
    fb_height:      resd 1
    fb_stride:      resd 1

    ; Colors
    color_black:    resd 1
    color_white:    resd 1

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
extern XCreateImage
extern XPutImage
extern XDestroyImage
extern XDefaultScreen
extern XDefaultRootWindow
extern XDefaultVisual
extern XDefaultDepth
extern XDefaultColormap
extern XBlackPixel
extern XWhitePixel
extern malloc
extern free

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
    sub rsp, 56             ; Local space + alignment

    mov [rbp - 48], edi     ; save width
    mov [rbp - 52], esi     ; save height
    mov [rel fb_width], edi
    mov [rel fb_height], esi

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

    ; Get visual
    mov rdi, r12
    mov esi, r13d
    call XDefaultVisual
    mov [rel visual], rax

    ; Get depth
    mov rdi, r12
    mov esi, r13d
    call XDefaultDepth
    mov [rel depth], eax

    ; Get colormap
    mov rdi, r12
    mov esi, r13d
    call XDefaultColormap
    mov [rel colormap], rax

    ; Get black/white pixels
    mov rdi, r12
    mov esi, r13d
    call XBlackPixel
    mov [rel color_black], eax
    mov r14d, eax           ; black

    mov rdi, r12
    mov esi, r13d
    call XWhitePixel
    mov [rel color_white], eax
    mov r15d, eax           ; white

    ; Create window - need stack args
    ; XCreateSimpleWindow(display, parent, x, y, width, height, border_width, border, background)
    mov rdi, r12
    mov rsi, [rel root_window]
    mov edx, 100            ; x
    mov ecx, 100            ; y
    mov r8d, [rbp - 48]     ; width
    mov r9d, [rbp - 52]     ; height
    ; Stack args (reverse order, aligned)
    mov dword [rsp], 2      ; border_width
    mov eax, r15d
    mov [rsp + 8], rax      ; border (white)
    mov eax, r14d
    mov [rsp + 16], rax     ; background (black)
    call XCreateSimpleWindow
    mov [rel window], rax
    mov rbx, rax            ; window

    ; Create GC
    mov rdi, r12
    mov rsi, rbx
    xor edx, edx
    xor ecx, ecx
    call XCreateGC
    mov [rel gc], rax

    ; Select input events
    mov rdi, r12
    mov rsi, rbx
    mov rdx, 0x020005       ; ExposureMask | KeyPressMask | ButtonPressMask
    call XSelectInput

    ; Map window
    mov rdi, r12
    mov rsi, rbx
    call XMapWindow

    ; Create framebuffer
    mov eax, [rbp - 48]     ; width
    imul eax, [rbp - 52]    ; * height
    shl eax, 2              ; * 4 bytes per pixel
    mov edi, eax
    mov eax, [rbp - 48]
    shl eax, 2
    mov [rel fb_stride], eax
    call malloc
    test rax, rax
    jz .fail
    mov [rel framebuf], rax

    ; Zero the framebuffer
    mov rdi, rax
    xor eax, eax
    mov ecx, [rel fb_width]
    imul ecx, [rel fb_height]
    rep stosd

    ; Create XImage
    ; XCreateImage(display, visual, depth, format, offset, data, width, height, bitmap_pad, bytes_per_line)
    mov rdi, r12
    mov rsi, [rel visual]
    mov edx, [rel depth]
    mov ecx, 2              ; ZPixmap
    xor r8d, r8d            ; offset
    mov r9, [rel framebuf]  ; data
    ; Stack args
    mov eax, [rel fb_width]
    mov [rsp], rax          ; width
    mov eax, [rel fb_height]
    mov [rsp + 8], rax      ; height
    mov qword [rsp + 16], 32 ; bitmap_pad
    mov eax, [rel fb_stride]
    mov [rsp + 24], rax     ; bytes_per_line
    call XCreateImage
    mov [rel ximage], rax

    ; Flush
    mov rdi, r12
    call XFlush

    mov eax, 1
    jmp .done

.fail:
    xor eax, eax

.done:
    add rsp, 56
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
;; gfx_clear — Clear framebuffer
;; edi = color (ARGB)
;; ============================================================
global gfx_clear
gfx_clear:
    mov eax, edi
    mov rdi, [rel framebuf]
    mov ecx, [rel fb_width]
    imul ecx, [rel fb_height]
    rep stosd
    ret

;; ============================================================
;; gfx_pixel — Draw pixel
;; edi = x, esi = y, edx = color
;; ============================================================
global gfx_pixel
gfx_pixel:
    ; Bounds check
    cmp edi, 0
    jl .done
    cmp edi, [rel fb_width]
    jge .done
    cmp esi, 0
    jl .done
    cmp esi, [rel fb_height]
    jge .done

    ; offset = y * stride + x * 4
    imul eax, esi, 4
    imul eax, [rel fb_width]
    lea eax, [eax + edi * 4]

    mov rdi, [rel framebuf]
    mov [rdi + rax], edx
.done:
    ret

;; ============================================================
;; gfx_line — Bresenham line
;; edi = x0, esi = y0, edx = x1, ecx = y1, r8d = color
;; ============================================================
global gfx_line
gfx_line:
    push rbx
    push r12
    push r13
    push r14
    push r15
    push rbp

    mov r12d, edi           ; x0
    mov r13d, esi           ; y0
    mov r14d, edx           ; x1
    mov r15d, ecx           ; y1
    mov ebp, r8d            ; color

    ; dx = abs(x1 - x0)
    mov eax, r14d
    sub eax, r12d
    cdq
    xor eax, edx
    sub eax, edx            ; abs
    mov r8d, eax            ; dx

    ; dy = -abs(y1 - y0)
    mov eax, r15d
    sub eax, r13d
    cdq
    xor eax, edx
    sub eax, edx
    neg eax
    mov r9d, eax            ; dy

    ; sx = x0 < x1 ? 1 : -1
    mov eax, 1
    mov ebx, -1
    cmp r12d, r14d
    cmovge eax, ebx
    mov r10d, eax           ; sx

    ; sy = y0 < y1 ? 1 : -1
    mov eax, 1
    cmp r13d, r15d
    cmovge eax, ebx
    mov r11d, eax           ; sy

    ; err = dx + dy
    mov ebx, r8d
    add ebx, r9d

.loop:
    ; Draw pixel
    mov edi, r12d
    mov esi, r13d
    mov edx, ebp
    call gfx_pixel

    ; if (x0 == x1 && y0 == y1) break
    cmp r12d, r14d
    jne .continue
    cmp r13d, r15d
    je .done

.continue:
    mov eax, ebx
    shl eax, 1              ; e2 = 2*err

    cmp eax, r9d
    jl .skip_x
    add ebx, r9d
    add r12d, r10d
.skip_x:

    cmp eax, r8d
    jg .loop
    add ebx, r8d
    add r13d, r11d
    jmp .loop

.done:
    pop rbp
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

    mov r12d, edi
    mov r13d, esi
    mov r14d, edx
    mov r15d, ecx
    mov ebx, r8d

    ; Top
    mov edi, r12d
    mov esi, r13d
    lea edx, [r12d + r14d - 1]
    mov ecx, r13d
    mov r8d, ebx
    call gfx_line

    ; Bottom
    mov edi, r12d
    lea esi, [r13d + r15d - 1]
    lea edx, [r12d + r14d - 1]
    lea ecx, [r13d + r15d - 1]
    mov r8d, ebx
    call gfx_line

    ; Left
    mov edi, r12d
    mov esi, r13d
    mov edx, r12d
    lea ecx, [r13d + r15d - 1]
    mov r8d, ebx
    call gfx_line

    ; Right
    lea edi, [r12d + r14d - 1]
    mov esi, r13d
    lea edx, [r12d + r14d - 1]
    lea ecx, [r13d + r15d - 1]
    mov r8d, ebx
    call gfx_line

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

    mov r12d, edi           ; x
    mov r13d, esi           ; y
    mov r14d, edx           ; w
    mov r15d, ecx           ; h
    mov ebx, r8d            ; color

    test r14d, r14d
    jle .done
    test r15d, r15d
    jle .done

.row_loop:
    test r15d, r15d
    jz .done

    mov ecx, r14d
    mov edi, r12d
.col_loop:
    test ecx, ecx
    jz .next_row

    push rcx
    mov esi, r13d
    mov edx, ebx
    call gfx_pixel
    pop rcx

    inc edi
    dec ecx
    jmp .col_loop

.next_row:
    inc r13d
    dec r15d
    jmp .row_loop

.done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; gfx_flip — Display framebuffer
;; ============================================================
global gfx_flip
gfx_flip:
    push rbp
    mov rbp, rsp
    sub rsp, 32

    mov rdi, [rel display]
    mov rsi, [rel window]
    mov rdx, [rel gc]
    mov rcx, [rel ximage]
    xor r8d, r8d            ; src_x
    xor r9d, r9d            ; src_y
    mov eax, 0
    mov [rsp], rax          ; dest_x
    mov [rsp + 8], rax      ; dest_y
    mov eax, [rel fb_width]
    mov [rsp + 16], rax     ; width
    mov eax, [rel fb_height]
    mov [rsp + 24], rax     ; height
    call XPutImage

    mov rdi, [rel display]
    call XFlush

    add rsp, 32
    pop rbp
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
