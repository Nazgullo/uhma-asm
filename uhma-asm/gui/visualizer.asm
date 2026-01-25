; visualizer.asm — UHMA System Visualizer
; Real-time visualization of the self-modifying system
;
; Shows:
; - Memory map with region activity
; - Symbolic observation log
; - Discovery timeline
; - Region type distribution
; - Live metrics
;

%include "constants.inc"

section .data
    vis_title:      db "UHMA System Visualizer", 0
    vis_regions:    db "REGIONS", 0
    vis_symbolic:   db "SYMBOLIC LOG", 0
    vis_discovery:  db "DISCOVERIES", 0
    vis_metrics:    db "METRICS", 0
    vis_step:       db "Step: ", 0
    vis_mods:       db "Mods: ", 0
    vis_blocked:    db "Blocked: ", 0
    vis_anomaly:    db "Anomalies: ", 0

    ; Region type colors (RGB for framebuffer)
    color_dispatch: dd 0x00FF6600   ; Orange
    color_frozen:   dd 0x006666FF   ; Blue
    color_nursery:  dd 0x0066FF66   ; Green
    color_condemned:dd 0x00FF0000   ; Red
    color_active:   dd 0x00FFFF00   ; Yellow
    color_bg:       dd 0x00202030   ; Dark blue-gray
    color_grid:     dd 0x00404050   ; Grid lines
    color_text:     dd 0x00FFFFFF   ; White text
    color_highlight:dd 0x00FF00FF   ; Magenta highlight
    color_green:    dd 0x0000FF00   ; Green
    color_red:      dd 0x00FF0000   ; Red
    color_yellow:   dd 0x00FFFF00   ; Yellow
    color_cyan:     dd 0x0000FFFF   ; Cyan
    color_white:    dd 0x00FFFFFF   ; White
    color_darkgray: dd 0x00404040   ; Dark gray

section .bss
    ; Visualizer state
    vis_running:    resd 1
    vis_frame:      resq 1
    vis_last_step:  resq 1

    ; Layout regions (x, y, w, h)
    region_map_x:   resd 1
    region_map_y:   resd 1
    region_map_w:   resd 1
    region_map_h:   resd 1

    log_area_x:     resd 1
    log_area_y:     resd 1
    log_area_w:     resd 1
    log_area_h:     resd 1

    metrics_x:      resd 1
    metrics_y:      resd 1

section .text

extern gfx_init
extern gfx_shutdown
extern gfx_clear
extern gfx_pixel
extern gfx_line
extern gfx_rect
extern gfx_fill_rect
extern gfx_flip
extern gfx_poll_event

extern sym_get_stats
extern usleep

;; ============================================================
;; vis_init — Initialize visualizer
;; Returns: rax = 1 on success
;; ============================================================
global vis_init
vis_init:
    push rbx
    push r12
    sub rsp, 8              ; Align stack

    ; Initialize graphics (1280x800 window)
    mov edi, 1280
    mov esi, 800
    call gfx_init
    test eax, eax
    jz .fail

    ; Wait for Expose event before drawing
    mov r12d, 200           ; max wait iterations
.wait_expose:
    call gfx_poll_event
    cmp eax, 12             ; Expose event
    je .exposed
    mov edi, 50000          ; 50ms
    call usleep
    dec r12d
    jnz .wait_expose
.exposed:

    ; Setup layout
    ; Region map: left side, 800x600
    mov dword [rel region_map_x], 10
    mov dword [rel region_map_y], 40
    mov dword [rel region_map_w], 800
    mov dword [rel region_map_h], 500

    ; Log area: right side
    mov dword [rel log_area_x], 820
    mov dword [rel log_area_y], 40
    mov dword [rel log_area_w], 450
    mov dword [rel log_area_h], 400

    ; Metrics: bottom
    mov dword [rel metrics_x], 10
    mov dword [rel metrics_y], 560

    mov dword [rel vis_running], 1
    mov qword [rel vis_frame], 0
    mov qword [rel vis_last_step], 0

    mov eax, 1
    jmp .done

.fail:
    xor eax, eax

.done:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; vis_shutdown — Clean up visualizer
;; ============================================================
global vis_shutdown
vis_shutdown:
    push rbx                    ; Align stack
    mov dword [rel vis_running], 0
    call gfx_shutdown
    pop rbx
    ret

;; ============================================================
;; vis_update — Update visualization (call each frame)
;; rdi = surface base pointer
;; Returns: rax = 1 if should continue, 0 if quit requested
;; ============================================================
global vis_update
vis_update:
    push rbx
    push r12
    push r13
    push r14
    push r15
    ; 5 pushes = rsp % 16 == 0, correct for calling

    mov r12, rdi            ; surface base

    ; Check events
    call gfx_poll_event
    cmp eax, 2              ; KeyPress
    je .quit
    cmp eax, 33             ; ClientMessage (close)
    je .quit

    ; Clear screen
    mov edi, [rel color_bg]
    call gfx_clear

    ; Draw region map
    mov rdi, r12
    call vis_draw_region_map

    ; Draw symbolic log
    mov rdi, r12
    call vis_draw_symbolic_log

    ; Draw metrics
    mov rdi, r12
    call vis_draw_metrics

    ; Draw frame border/title
    call vis_draw_chrome

    ; Flip to screen
    call gfx_flip

    inc qword [rel vis_frame]

    mov eax, 1
    jmp .done

.quit:
    xor eax, eax

.done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; vis_draw_region_map — Draw memory map showing all regions
;; rdi = surface base
;; ============================================================
vis_draw_region_map:
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov r12, rdi            ; surface base

    ; Draw border
    mov edi, [rel region_map_x]
    mov esi, [rel region_map_y]
    mov edx, [rel region_map_w]
    mov ecx, [rel region_map_h]
    mov r8d, [rel color_grid]
    call gfx_rect

    ; Get region count
    lea rax, [r12 + STATE_OFFSET + ST_REGION_COUNT]
    mov r13d, [rax]         ; region count
    test r13d, r13d
    jz .done

    ; Calculate cell size based on region count
    ; Grid: sqrt(count) x sqrt(count)
    mov eax, r13d
    ; Approximate sqrt
    mov ecx, 1
.sqrt_loop:
    mov eax, ecx
    imul eax, ecx
    cmp eax, r13d
    jge .sqrt_done
    inc ecx
    jmp .sqrt_loop
.sqrt_done:
    mov r14d, ecx           ; grid dimension

    ; Cell size
    mov eax, [rel region_map_w]
    sub eax, 20
    xor edx, edx
    div r14d
    mov r15d, eax           ; cell width

    mov eax, [rel region_map_h]
    sub eax, 20
    xor edx, edx
    div r14d
    mov ebx, eax            ; cell height

    ; Draw each region
    lea r12, [r12 + REGION_TABLE_OFFSET]
    xor ecx, ecx            ; index

.region_loop:
    cmp ecx, r13d
    jge .done
    push rcx

    ; Calculate grid position
    xor edx, edx
    mov eax, ecx
    push rbx
    mov ebx, r14d
    div ebx                 ; eax = row, edx = col
    pop rbx

    ; Calculate pixel position
    mov edi, edx
    imul edi, r15d
    add edi, [rel region_map_x]
    add edi, 10

    push rax
    mov esi, eax
    imul esi, ebx
    add esi, [rel region_map_y]
    add esi, 10
    pop rax

    ; Get region entry
    pop rcx
    push rcx
    mov rax, rcx
    imul rax, RTE_SIZE
    add rax, r12

    ; Get flags to determine color
    movzx r8d, word [rax + RTE_FLAGS]

    ; Choose color based on flags
    mov r9d, [rel color_dispatch]  ; default

    test r8d, RFLAG_CONDEMNED
    jz .not_condemned
    mov r9d, [rel color_condemned]
    jmp .draw_cell
.not_condemned:

    test r8d, RFLAG_NURSERY
    jz .not_nursery
    mov r9d, [rel color_nursery]
    jmp .draw_cell
.not_nursery:

    test r8d, RFLAG_FROZEN
    jz .not_frozen
    mov r9d, [rel color_frozen]
    jmp .draw_cell
.not_frozen:

    test r8d, RFLAG_ACTIVE
    jz .draw_cell
    mov r9d, [rel color_active]

.draw_cell:
    ; Draw filled rectangle for region
    push rbx
    mov edx, r15d
    sub edx, 2              ; width
    mov ecx, ebx
    sub ecx, 2              ; height
    mov r8d, r9d            ; color
    call gfx_fill_rect
    pop rbx

    ; Get hits for brightness/pulse effect
    pop rcx
    push rcx
    mov rax, rcx
    imul rax, RTE_SIZE
    add rax, r12
    mov rax, [rax + RTE_ADDR]
    test rax, rax
    jz .no_hits

    mov eax, [rax + RHDR_HITS]
    test eax, eax
    jz .no_hits

    ; Draw highlight for active regions (has hits)
    ; Recompute position
    pop rcx
    push rcx
    xor edx, edx
    mov eax, ecx
    push rbx
    mov ebx, r14d
    div ebx
    pop rbx

    mov edi, edx
    imul edi, r15d
    add edi, [rel region_map_x]
    add edi, 12

    mov esi, eax
    imul esi, ebx
    add esi, [rel region_map_y]
    add esi, 12

    mov edx, r15d
    sub edx, 6
    mov ecx, ebx
    sub ecx, 6
    mov r8d, [rel color_highlight]
    call gfx_rect

.no_hits:
    pop rcx
    inc ecx
    jmp .region_loop

.done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; vis_draw_symbolic_log — Draw recent symbolic observations
;; rdi = surface base
;; ============================================================
vis_draw_symbolic_log:
    push rbx
    push r12
    sub rsp, 8              ; Align stack for calls

    mov r12, rdi

    ; Draw border
    mov edi, [rel log_area_x]
    mov esi, [rel log_area_y]
    mov edx, [rel log_area_w]
    mov ecx, [rel log_area_h]
    mov r8d, [rel color_grid]
    call gfx_rect

    ; Get symbolic stats
    call sym_get_stats
    ; rax = mods, rbx = blocked, rcx = anomalies

    ; Draw bars representing activity
    ; Mods bar (green)
    mov edi, [rel log_area_x]
    add edi, 10
    mov esi, [rel log_area_y]
    add esi, 30

    ; Scale mods to bar width (max 400 pixels)
    push rax
    push rbx
    push rcx

    cmp eax, 1000
    jle .mods_ok
    mov eax, 1000
.mods_ok:
    imul edx, eax, 400
    xor eax, eax
    mov eax, edx
    mov ecx, 1000
    xor edx, edx
    div ecx
    mov edx, eax            ; width

    mov ecx, 20             ; height
    mov r8d, [rel color_green]
    call gfx_fill_rect

    ; Blocked bar (red)
    mov edi, [rel log_area_x]
    add edi, 10
    mov esi, [rel log_area_y]
    add esi, 60

    pop rcx
    pop rbx
    pop rax
    push rax
    push rbx
    push rcx

    mov eax, ebx
    cmp eax, 100
    jle .blocked_ok
    mov eax, 100
.blocked_ok:
    imul edx, eax, 400
    mov ecx, 100
    xor edx, edx
    push rax
    mov eax, [rsp + 8]
    imul eax, 400
    xor edx, edx
    div ecx
    mov edx, eax
    pop rax

    mov ecx, 20
    mov r8d, [rel color_red]
    call gfx_fill_rect

    ; Anomalies bar (yellow)
    mov edi, [rel log_area_x]
    add edi, 10
    mov esi, [rel log_area_y]
    add esi, 90

    pop rcx
    pop rbx
    pop rax

    mov eax, ecx
    cmp eax, 50
    jle .anom_ok
    mov eax, 50
.anom_ok:
    imul edx, eax, 400
    mov ecx, 50
    xor edx, edx
    div ecx
    mov edx, eax

    mov ecx, 20
    mov r8d, [rel color_yellow]
    call gfx_fill_rect

    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; vis_draw_metrics — Draw current metrics
;; rdi = surface base
;; ============================================================
vis_draw_metrics:
    push rbx
    push r12
    sub rsp, 8              ; Align stack for calls

    mov r12, rdi

    ; Get global step
    lea rax, [r12 + STATE_OFFSET + ST_GLOBAL_STEP]
    mov ebx, [rax]

    ; Draw step indicator as a progress bar
    mov edi, [rel metrics_x]
    mov esi, [rel metrics_y]

    ; Bar background
    mov edx, 600
    mov ecx, 30
    mov r8d, [rel color_darkgray]
    call gfx_fill_rect

    ; Progress (step mod 600)
    mov eax, ebx
    xor edx, edx
    mov ecx, 600
    div ecx                 ; edx = step mod 600

    mov edi, [rel metrics_x]
    mov esi, [rel metrics_y]
    mov ecx, 30
    mov r8d, [rel color_cyan]
    call gfx_fill_rect

    ; Region count indicator
    lea rax, [r12 + STATE_OFFSET + ST_REGION_COUNT]
    mov ebx, [rax]

    mov edi, [rel metrics_x]
    add edi, 620
    mov esi, [rel metrics_y]

    ; Scale to bar (max 256 regions = full bar)
    cmp ebx, 256
    jle .rc_ok
    mov ebx, 256
.rc_ok:
    imul edx, ebx, 200
    shr edx, 8              ; /256
    mov ecx, 30
    mov r8d, [rel color_green]
    call gfx_fill_rect

    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; vis_draw_chrome — Draw window decorations
;; ============================================================
vis_draw_chrome:
    push rbx

    ; Top bar
    xor edi, edi
    xor esi, esi
    mov edx, 1280
    mov ecx, 30
    mov r8d, [rel color_darkgray]
    call gfx_fill_rect

    ; Section labels - draw colored dots as simple labels
    ; Region map label
    mov edi, [rel region_map_x]
    mov esi, 15
    mov edx, [rel color_white]
    call gfx_pixel
    inc edi
    call gfx_pixel
    inc edi
    call gfx_pixel

    ; Log area label
    mov edi, [rel log_area_x]
    mov esi, 15
    mov edx, [rel color_white]
    call gfx_pixel
    inc edi
    call gfx_pixel
    inc edi
    call gfx_pixel

    pop rbx
    ret

;; ============================================================
;; vis_is_running — Check if visualizer is still running
;; Returns: rax = 1 if running
;; ============================================================
global vis_is_running
vis_is_running:
    mov eax, [rel vis_running]
    ret
