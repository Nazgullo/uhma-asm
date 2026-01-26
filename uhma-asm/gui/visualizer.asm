; visualizer.asm — UHMA Command & Control Center
; Real interactive GUI with clickable regions, live token stream, and system control
;
; @entry vis_init(rdi=surface_ptr) -> eax=1 on success
; @entry vis_update() -> eax=1 to continue, 0 to quit
; @entry vis_shutdown()
; @calls gfx.asm:gfx_init, gfx.asm:gfx_fill_rect, gfx.asm:gfx_text
; @calledby viz_main.asm:main
;
; GOTCHAS:
;   - X11 calls need 16-byte aligned stack: count pushes + sub rsp carefully
;   - gfx_fill_rect takes 5 args: edi=x, esi=y, edx=w, ecx=h, r8d=color
;   - Callee-saved regs (rbx, r12-r15) must be preserved across gfx_* calls
;   - ecx is caller-saved: reload h from memory before each gfx call, don't push/pop
;
; Layout:
;   ┌─────────────────────────────────────────────────────────────────────┐
;   │ Menu Bar: [DREAM] [OBSERVE] [EVOLVE] [STEP] [RUN] [SAVE] [LOAD]... │
;   ├───────────────────────┬─────────────────────────────────────────────┤
;   │ MEMORY MAP (click)    │ REGION DETAIL (selected)                    │
;   │                       │                                             │
;   │                       ├─────────────────────────────────────────────┤
;   │                       │ TOKEN STREAM (live)                         │
;   │                       │                                             │
;   ├───────────────────────┴─────────────────────────────────────────────┤
;   │ SYSTEM STATE & PREDICTION                                           │
;   ├─────────────────────────────────────────────────────────────────────┤
;   │ INPUT: _                                           [SEND] [CLEAR]   │
;   ├─────────────────────────────────────────────────────────────────────┤
;   │ Status Bar                                                          │
;   └─────────────────────────────────────────────────────────────────────┘

%include "constants.inc"

section .data
    ; Panel labels
    lbl_memory:     db "MEMORY MAP", 0
    lbl_region:     db "REGION DETAIL", 0
    lbl_stream:     db "TOKEN STREAM", 0
    lbl_state:      db "SYSTEM STATE", 0
    lbl_input:      db "INPUT", 0
    lbl_journey:    db "TOKEN JOURNEY", 0
    lbl_log:        db "ACTIVITY LOG", 0
    lbl_timeline:   db "PREDICTION TIMELINE", 0
    lbl_heatmap:    db "TOKEN HEATMAP", 0
    lbl_conns:      db "REGION CONNECTIONS", 0
    lbl_waveform:   db "WAVEFORM", 0
    lbl_patterns:   db "PATTERNS", 0
    lbl_output:     db "REPL OUTPUT", 0

    ; Mind map node labels
    node_uhma:      db "UHMA", 0
    node_brain:     db "BRAIN", 0
    node_regions:   db "REGIONS", 0
    node_tokens:    db "TOKENS", 0
    node_state:     db "STATE", 0
    node_predict:   db "PREDICT", 0
    node_dispatch:  db "DISPATCH", 0
    node_accuracy:  db "ACCURACY", 0
    node_patterns:  db "PATTERNS", 0
    node_na:        db "N/A", 0
    hint_click:     db "Click any node to expand/inspect", 0
    hint_back:      db "Click outside to go back", 0
    detail_expanded: db "EXPANDED VIEW", 0

    ; Button labels
    btn_dream:      db "DREAM", 0
    btn_observe:    db "OBSERVE", 0
    btn_evolve:     db "EVOLVE", 0
    btn_step:       db "STEP", 0
    btn_run:        db "RUN", 0
    btn_save:       db "SAVE", 0
    btn_load:       db "LOAD", 0
    btn_file:       db "FILE", 0
    btn_dir:        db "DIR", 0
    btn_trace:      db "TRACE", 0
    btn_geom:       db "GEOM", 0
    btn_send:       db "SEND", 0
    btn_clear:      db "CLEAR", 0
    btn_quit:       db "QUIT", 0

    ; New feature labels
    lbl_rosetta:    db "ROSETTA STONE", 0
    lbl_hive:       db "HIVE MIND (Pheromones)", 0
    lbl_mycorrhiza: db "MYCORRHIZA COLONY", 0
    lbl_spore:      db "SPORE SYSTEM", 0
    lbl_geom_mode:  db "Verify Mode: ", 0
    lbl_safety:     db "Safety Score: ", 0
    lbl_danger:     db "Danger Score: ", 0
    lbl_dream_p:    db "Dream: ", 0
    lbl_observe_p:  db "Observe: ", 0
    lbl_evolve_p:   db "Evolve: ", 0
    lbl_threshold:  db "Threshold: 0.5", 0
    lbl_colony_mode:db "Mode: ", 0
    lbl_colony_size:db "Colony: ", 0
    lbl_valence:    db "Valence: ", 0
    lbl_genes:      db "Genes: ", 0

    ; Verification mode names
    geom_mode_abstract: db "ABSTRACT", 0
    geom_mode_geometric:db "GEOMETRIC", 0
    geom_mode_both:     db "BOTH", 0

    ; Colony mode names
    colony_solo:    db "SOLO", 0
    colony_hive:    db "HIVE MIND", 0

    ; Pheromone bar scale (60 pixels = 1.0)
    bar_scale_60:   dq 60.0

    ; Pheromone bar labels
    lbl_d:          db "D", 0
    lbl_o:          db "O", 0
    lbl_e:          db "E", 0

    ; Node labels for new features
    node_rosetta:   db "ROSETTA", 0
    node_hive:      db "HIVE", 0
    node_myco:      db "MYCO", 0
    node_spore:     db "SPORE", 0

    ; Region detail labels
    lbl_rgn_addr:   db "Addr: ", 0
    lbl_rgn_type:   db "Type: ", 0
    lbl_rgn_flags:  db "Flags: ", 0
    lbl_rgn_hits:   db "Hits: ", 0
    lbl_rgn_miss:   db "Miss: ", 0
    lbl_rgn_acc:    db "Acc: ", 0
    lbl_rgn_ctx:    db "Ctx: ", 0
    lbl_rgn_pred:   db "Pred: ", 0
    lbl_rgn_excite: db "Excite: ", 0
    lbl_rgn_inhib:  db "Inhibit: ", 0
    lbl_none_sel:   db "(click a region)", 0

    ; Region type names
    rtype_dispatch: db "DISPATCH", 0
    rtype_vsa:      db "VSA_OP", 0
    rtype_modifier: db "MODIFIER", 0
    rtype_observer: db "OBSERVER", 0
    rtype_emitter:  db "EMITTER", 0
    rtype_hook:     db "HOOK", 0
    rtype_gate:     db "GATE", 0
    rtype_dream:    db "DREAM", 0

    ; Flag names
    flag_active:    db "ACT ", 0
    flag_frozen:    db "FRZ ", 0
    flag_nursery:   db "NUR ", 0
    flag_condemned: db "CON ", 0

    ; State labels
    lbl_intro:      db "State: ", 0
    lbl_mode:       db "Mode: ", 0
    lbl_energy:     db "Energy: ", 0
    lbl_tokens:     db "Tokens: ", 0
    lbl_unique:     db "Unique: ", 0
    lbl_regions:    db "Regions: ", 0
    lbl_predict:    db "NEXT: ", 0
    lbl_conf:       db " (", 0
    lbl_conf_end:   db "%)", 0

    ; Introspective state names
    intro_names:
    dq intro_0, intro_1, intro_2, intro_3, intro_4, intro_5, intro_6
    intro_0: db "IDLE", 0
    intro_1: db "CONFUSED", 0
    intro_2: db "CONFIDENT", 0
    intro_3: db "LEARNING", 0
    intro_4: db "STUCK", 0
    intro_5: db "EXPLORING", 0
    intro_6: db "CONSOLIDATING", 0

    ; Dispatch mode names
    mode_names:
    dq mode_0, mode_1, mode_2, mode_3, mode_4, mode_5, mode_6, mode_7
    mode_0: db "FAST", 0
    mode_1: db "BEST", 0
    mode_2: db "EXPLORE", 0
    mode_3: db "DELIBERATE", 0
    mode_4: db "MULTI", 0
    mode_5: db "STRUCTURAL", 0
    mode_6: db "CAUSAL", 0
    mode_7: db "ANALOGICAL", 0

    ; Token stream markers
    tok_hit:        db " HIT", 0
    tok_miss:       db " MISS", 0
    tok_new:        db " NEW", 0
    tok_arrow:      db " -> ", 0

    ; Zenity commands
    zenity_file:    db "zenity --file-selection --title='Select File' 2>/dev/null > /tmp/.uhma_pick", 0
    zenity_dir:     db "zenity --file-selection --directory --title='Select Folder' 2>/dev/null > /tmp/.uhma_pick", 0
    tmp_pick_path:  db "/tmp/.uhma_pick", 0
    read_mode:      db "rb", 0

    ; Feedback messages
    msg_dream:      db "Dream complete", 0
    msg_observe:    db "Observe complete", 0
    msg_evolve:     db "Evolve complete", 0
    msg_step:       db "Step complete", 0
    msg_run:        db "100 steps done", 0
    msg_save:       db "State saved", 0
    msg_load:       db "State loaded", 0
    msg_sent:       db "Input sent", 0
    msg_clear:      db "Cleared", 0
    msg_file_ok:    db "File loaded", 0
    msg_dir_ok:     db "Dir loaded", 0
    msg_file_err:   db "Load failed", 0
    msg_trace_on:   db "Trace ON", 0
    msg_trace_off:  db "Trace OFF", 0
    msg_geom_abs:   db "Verify: ABSTRACT", 0
    msg_geom_vec:   db "Verify: GEOMETRIC", 0
    msg_geom_both:  db "Verify: BOTH", 0

    ; MCP command strings
    raw_tool:       db "raw", 0
    step_text:      db " ", 0
    run_cmd_arg:    db '"command":"run 100"', 0
    trace_on_arg:   db '"command":"listen"', 0
    trace_off_arg:  db '"command":"trace"', 0
    geom_cmd_arg:   db '"command":"geom"', 0
    eat_cmd_pre:    db '"command":"eat ', 0

    ; Number format
    hex_prefix:     db "0x", 0
    pct_sign:       db "%", 0
    quote_char:     db "'", 0

    ; Colors (0x00RRGGBB)
    col_bg:         dd 0x001a1a2e
    col_panel:      dd 0x00252545
    col_panel_hi:   dd 0x00303060
    col_border:     dd 0x00404070
    col_btn:        dd 0x00354065
    col_btn_hover:  dd 0x00456085
    col_input_bg:   dd 0x00151530
    col_text:       dd 0x00e0e0e0
    col_text_dim:   dd 0x00909090
    col_cyan:       dd 0x0000ccff
    col_green:      dd 0x0044ff88
    col_red:        dd 0x00ff5555
    col_yellow:     dd 0x00ffcc44
    col_orange:     dd 0x00ff8844
    col_magenta:    dd 0x00ff44ff
    col_white:      dd 0x00ffffff

    ; Region colors
    col_active:     dd 0x00ffff00
    col_frozen:     dd 0x006688ff
    col_nursery:    dd 0x0066ff88
    col_condemned:  dd 0x00ff4444
    col_dispatch:   dd 0x00ff8800
    col_selected:   dd 0x00ffffff

    ; Action IDs
    ACT_DREAM       equ 1
    ACT_OBSERVE     equ 2
    ACT_EVOLVE      equ 3
    ACT_STEP        equ 4
    ACT_RUN         equ 5
    ACT_SAVE        equ 6
    ACT_LOAD        equ 7
    ACT_FILE        equ 8
    ACT_DIR         equ 9
    ACT_TRACE       equ 10
    ACT_GEOM        equ 11
    ACT_SEND        equ 12
    ACT_CLEAR       equ 13
    ACT_QUIT        equ 14

    ; Layout constants
    WIN_W           equ 1280
    WIN_H           equ 800
    MENU_H          equ 45
    STATUS_H        equ 25
    INPUT_H         equ 40
    ; Mind map canvas (main visualization area)
    CANVAS_X        equ 10
    CANVAS_Y        equ MENU_H + 5
    CANVAS_W        equ WIN_W - 20
    CANVAS_H        equ WIN_H - MENU_H - STATUS_H - INPUT_H - 20
    ; Node sizes
    NODE_W          equ 120
    NODE_H          equ 50
    NODE_SMALL_W    equ 80
    NODE_SMALL_H    equ 35
    ; Legacy constants for click handling
    MAP_W           equ CANVAS_W
    MAP_H           equ CANVAS_H

section .bss
    ; Core state
    vis_running:    resd 1
    vis_frame:      resq 1
    surface_ptr:    resq 1

    ; Mouse
    mouse_x:        resd 1
    mouse_y:        resd 1
    hover_btn:      resd 1

    ; Input buffer
    input_buf:      resb 256
    input_len:      resd 1

    ; Feedback
    feedback_msg:   resq 1
    feedback_timer: resd 1

    ; Buttons: x, y, w, h for each (max 16)
    buttons:        resd 64
    num_buttons:    resd 1

    ; Region selection
    selected_rgn:   resd 1          ; -1 = none
    map_x:          resd 1          ; memory map panel x
    map_y:          resd 1          ; memory map panel y
    map_cell_w:     resd 1          ; cell width
    map_cell_h:     resd 1          ; cell height
    map_grid_dim:   resd 1          ; grid dimension
    map_grid_x:     resd 1          ; grid start x within panel
    map_grid_y:     resd 1          ; grid start y within panel

    ; Token stream ring buffer (last 8 tokens)
    stream_buf:     resd 32         ; 8 entries: token(u32), result(u32), region(u32), pad(u32)
    stream_pos:     resd 1
    stream_count:   resd 1

    ; File loading
    file_path_buf:  resb 512
    file_read_buf:  resb 4096
    eat_cmd_buf:    resb 1024
    ; Dummy surface for state reads (GUI doesn't access real surface via MCP)
    dummy_surface:  resb 131072

    ; Trace state
    trace_active:   resd 1
    geom_mode:      resd 1

    ; Token journey buffer (last traced path)
    ; Each entry: region_id(u32), action(u32) - 0=enter, 1=hit, 2=miss, 3=exit
    journey_buf:    resd 32         ; 16 entries
    journey_len:    resd 1
    journey_token:  resd 1          ; token being traced

    ; Activity log ring buffer (last 6 events)
    ; Each entry: 64 bytes text
    log_buf:        resb 384        ; 6 * 64
    log_pos:        resd 1
    log_count:      resd 1

    ; Prediction timeline (last 64 predictions: 1=hit, 0=miss)
    timeline_buf:   resb 64
    timeline_pos:   resd 1

    ; Token heatmap (256 entries for byte values)
    heatmap_buf:    resd 256

    ; Waveform buffer (last 128 token values)
    waveform_buf:   resd 128
    waveform_pos:   resd 1

    ; REPL output buffer (last 6 lines, 80 chars each)
    repl_buf:       resb 480
    repl_pos:       resd 1

    ; Mind map node system
    ; Node types: 0=root, 1=region, 2=token, 3=pattern, 4=state, 5=prediction
    MAX_NODES       equ 64
    ; Each node: x(i32), y(i32), type(i32), data_id(i32), parent(i32), flags(i32) = 24 bytes
    nodes:          resd 144        ; 64 nodes * 6 dwords (simplified to 24 visible)
    node_count:     resd 1
    ; Currently focused/selected node (-1 = root view)
    focus_node:     resd 1
    ; View mode: 0 = mindmap, 1 = brain/memory map
    view_mode:      resd 1
    ; View offset for panning
    view_x:         resd 1
    view_y:         resd 1
    ; Zoom level (100 = 100%)
    zoom_level:     resd 1
    ; Node rects for click detection (x,y,w,h for each node, max 16 nodes)
    panel_rects:    resd 64

section .text

extern gfx_init
extern gfx_shutdown
extern gfx_clear
extern gfx_rect
extern gfx_fill_rect
extern gfx_line
extern gfx_flip
extern gfx_poll_event
extern gfx_text
extern gfx_get_last_keycode
extern gfx_get_mouse_pos
extern usleep
extern system
extern fopen
extern fgets
extern fread
extern fclose
extern opendir
extern readdir
extern closedir
extern strcpy
extern strcat
extern strlen
; MCP client functions (replaces direct UHMA calls)
extern mcp_dream
extern mcp_observe
extern mcp_evolve
extern mcp_send_text
extern mcp_save
extern mcp_load
extern mcp_get_status
extern mcp_call

; Rosetta Stone / Geometric Verification
; Verify mode handled via MCP raw commands

; Colony / Mycorrhiza
; Shared mode queried via MCP

;; ============================================================
;; vis_init — Initialize visualizer
;; rdi = surface base pointer
;; ============================================================
global vis_init
vis_init:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 24                 ; Align to 16 bytes: 6 pushes (48) + 24 = 72, + 8 (ret) = 80

    ; Use dummy surface for state reads (real data comes via MCP)
    lea rax, [rel dummy_surface]
    mov [rel surface_ptr], rax

    ; Initialize graphics
    mov edi, WIN_W
    mov esi, WIN_H
    call gfx_init
    test eax, eax
    jz .fail

    ; Wait for window
    mov r12d, 100
.wait_expose:
    call gfx_poll_event
    cmp eax, 12
    je .exposed
    mov edi, 20000
    call usleep
    dec r12d
    jnz .wait_expose
.exposed:

    ; Setup buttons on menu bar
    lea rdi, [rel buttons]
    mov ecx, 0              ; button index

    ; DREAM (0)
    mov dword [rdi], 10
    mov dword [rdi+4], 8
    mov dword [rdi+8], 60
    mov dword [rdi+12], 30
    add rdi, 16

    ; OBSERVE (1)
    mov dword [rdi], 80
    mov dword [rdi+4], 8
    mov dword [rdi+8], 70
    mov dword [rdi+12], 30
    add rdi, 16

    ; EVOLVE (2)
    mov dword [rdi], 160
    mov dword [rdi+4], 8
    mov dword [rdi+8], 60
    mov dword [rdi+12], 30
    add rdi, 16

    ; STEP (3)
    mov dword [rdi], 230
    mov dword [rdi+4], 8
    mov dword [rdi+8], 50
    mov dword [rdi+12], 30
    add rdi, 16

    ; RUN (4)
    mov dword [rdi], 290
    mov dword [rdi+4], 8
    mov dword [rdi+8], 45
    mov dword [rdi+12], 30
    add rdi, 16

    ; SAVE (5)
    mov dword [rdi], 345
    mov dword [rdi+4], 8
    mov dword [rdi+8], 50
    mov dword [rdi+12], 30
    add rdi, 16

    ; LOAD (6)
    mov dword [rdi], 405
    mov dword [rdi+4], 8
    mov dword [rdi+8], 50
    mov dword [rdi+12], 30
    add rdi, 16

    ; FILE (7)
    mov dword [rdi], 465
    mov dword [rdi+4], 8
    mov dword [rdi+8], 45
    mov dword [rdi+12], 30
    add rdi, 16

    ; DIR (8)
    mov dword [rdi], 520
    mov dword [rdi+4], 8
    mov dword [rdi+8], 40
    mov dword [rdi+12], 30
    add rdi, 16

    ; TRACE (9)
    mov dword [rdi], 570
    mov dword [rdi+4], 8
    mov dword [rdi+8], 55
    mov dword [rdi+12], 30
    add rdi, 16

    ; GEOM (10) - toggle verification mode
    mov dword [rdi], 635
    mov dword [rdi+4], 8
    mov dword [rdi+8], 50
    mov dword [rdi+12], 30
    add rdi, 16

    ; SEND (11) - near input
    mov dword [rdi], 1100
    mov dword [rdi+4], WIN_H - INPUT_H - STATUS_H + 10
    mov dword [rdi+8], 55
    mov dword [rdi+12], 30
    add rdi, 16

    ; CLEAR (12)
    mov dword [rdi], 1165
    mov dword [rdi+4], WIN_H - INPUT_H - STATUS_H + 10
    mov dword [rdi+8], 55
    mov dword [rdi+12], 30
    add rdi, 16

    ; QUIT (13)
    mov dword [rdi], 1210
    mov dword [rdi+4], 8
    mov dword [rdi+8], 50
    mov dword [rdi+12], 30

    mov dword [rel num_buttons], 14

    ; Initialize state
    mov dword [rel vis_running], 1
    mov qword [rel vis_frame], 0
    mov dword [rel input_len], 0
    mov dword [rel hover_btn], -1
    mov qword [rel feedback_msg], 0
    mov dword [rel feedback_timer], 0
    mov dword [rel selected_rgn], -1
    mov dword [rel trace_active], 0
    mov dword [rel stream_pos], 0
    mov dword [rel stream_count], 0
    mov dword [rel focus_node], -1      ; Start at root view
    mov dword [rel view_mode], 0        ; Start in mindmap view
    mov dword [rel view_x], 0
    mov dword [rel view_y], 0
    mov dword [rel zoom_level], 100
    mov dword [rel node_count], 0
    mov dword [rel timeline_pos], 0
    mov dword [rel waveform_pos], 0
    mov dword [rel repl_pos], 0

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
;; vis_shutdown
;; ============================================================
global vis_shutdown
vis_shutdown:
    push rbx
    mov dword [rel vis_running], 0
    call gfx_shutdown
    pop rbx
    ret

;; ============================================================
;; vis_update — Main update, returns 0 to quit
;; ============================================================
global vis_update
vis_update:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 24                 ; Align to 16 bytes

.poll_loop:
    call gfx_poll_event
    test eax, eax
    jz .events_done

    mov r12d, eax

    ; KeyPress
    cmp r12d, 2
    jne .not_key
    call gfx_get_last_keycode
    mov edi, eax
    call handle_key
    test eax, eax
    jz .quit
    jmp .poll_loop

.not_key:
    ; ButtonPress
    cmp r12d, 4
    jne .not_click
    call handle_click
    test eax, eax
    jz .quit
    jmp .poll_loop

.not_click:
    ; Motion
    cmp r12d, 6
    jne .not_motion
    call handle_motion
    jmp .poll_loop

.not_motion:
    ; Window close
    cmp r12d, 33
    je .quit
    jmp .poll_loop

.events_done:
    ; Decrement feedback timer
    mov eax, [rel feedback_timer]
    test eax, eax
    jz .no_dec
    dec eax
    mov [rel feedback_timer], eax
.no_dec:

    ; Draw
    call draw_frame
    call gfx_flip
    inc qword [rel vis_frame]

    mov eax, 1
    jmp .done

.quit:
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
;; handle_key
;; ============================================================
handle_key:
    push rbx
    push r12
    sub rsp, 8

    mov ebx, edi

    ; Escape = quit
    cmp ebx, 9
    je .quit

    ; Enter = send
    cmp ebx, 36
    je .send

    ; Backspace
    cmp ebx, 22
    je .backspace

    ; Printable
    call keycode_to_char
    test al, al
    jz .continue

    ; Add to input
    mov ecx, [rel input_len]
    cmp ecx, 250
    jge .continue
    lea rdx, [rel input_buf]
    mov [rdx + rcx], al
    inc ecx
    mov [rel input_len], ecx
    jmp .continue

.backspace:
    mov ecx, [rel input_len]
    test ecx, ecx
    jz .continue
    dec ecx
    mov [rel input_len], ecx
    jmp .continue

.send:
    call do_send_input
    jmp .continue

.continue:
    mov eax, 1
    jmp .done

.quit:
    xor eax, eax
.done:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; keycode_to_char — X11 keycode to ASCII
;; ============================================================
keycode_to_char:
    cmp edi, 65
    jne .not_space
    mov al, ' '
    ret
.not_space:
    ; Numbers 10-19 -> 1-9,0
    cmp edi, 10
    jl .try_qrow
    cmp edi, 19
    jg .try_qrow
    mov eax, edi
    sub eax, 10
    add eax, '1'
    cmp al, '9'+1
    jne .ret
    mov al, '0'
.ret:
    ret
.try_qrow:
    ; q-p: 24-33
    cmp edi, 24
    jl .try_arow
    cmp edi, 33
    jg .try_arow
    lea rax, [rel kmap_qrow]
    movzx eax, byte [rax + rdi - 24]
    ret
.try_arow:
    ; a-l: 38-46
    cmp edi, 38
    jl .try_zrow
    cmp edi, 46
    jg .try_zrow
    lea rax, [rel kmap_arow]
    movzx eax, byte [rax + rdi - 38]
    ret
.try_zrow:
    ; z-m: 52-58
    cmp edi, 52
    jl .no_char
    cmp edi, 58
    jg .no_char
    lea rax, [rel kmap_zrow]
    movzx eax, byte [rax + rdi - 52]
    ret
.no_char:
    xor eax, eax
    ret

section .data
    kmap_qrow: db "qwertyuiop"
    kmap_arow: db "asdfghjkl"
    kmap_zrow: db "zxcvbnm"

section .text

;; ============================================================
;; handle_click — Mouse click
;; ============================================================
handle_click:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 8

    call gfx_get_mouse_pos
    mov [rel mouse_x], eax
    mov [rel mouse_y], edx
    mov r12d, eax           ; x
    mov r13d, edx           ; y

    ; Check buttons first
    lea rbx, [rel buttons]
    xor ecx, ecx
.btn_loop:
    cmp ecx, [rel num_buttons]
    jge .no_button

    mov eax, [rbx]          ; btn x
    cmp r12d, eax
    jl .next_btn
    add eax, [rbx + 8]      ; + width
    cmp r12d, eax
    jge .next_btn

    mov eax, [rbx + 4]      ; btn y
    cmp r13d, eax
    jl .next_btn
    add eax, [rbx + 12]     ; + height
    cmp r13d, eax
    jge .next_btn

    ; Button clicked
    inc ecx
    mov edi, ecx
    call do_action
    test eax, eax
    jz .quit
    jmp .done_click

.next_btn:
    add rbx, 16
    inc ecx
    jmp .btn_loop

.no_button:
    ; Check current view mode
    mov eax, [rel view_mode]
    test eax, eax
    jnz .in_brain_view

    ; === MINDMAP VIEW: Check for node clicks ===
    ; Check BRAIN node (panel_rects + 16)
    lea rax, [rel panel_rects]
    mov edi, [rax + 16]         ; brain x
    cmp r12d, edi
    jl .check_other_nodes
    add edi, [rax + 24]         ; + width
    cmp r12d, edi
    jge .check_other_nodes
    mov edi, [rax + 20]         ; brain y
    cmp r13d, edi
    jl .check_other_nodes
    add edi, [rax + 28]         ; + height
    cmp r13d, edi
    jge .check_other_nodes
    ; Clicked BRAIN - switch to brain view
    mov dword [rel view_mode], 1
    jmp .done_click

.check_other_nodes:
    ; Could add other node click handling here
    jmp .done_click

.in_brain_view:
    ; === BRAIN VIEW: Check for region clicks or back ===
    ; Check if in memory map grid area
    mov eax, [rel map_x]
    cmp r12d, eax
    jl .back_to_mindmap
    add eax, MAP_W
    cmp r12d, eax
    jge .back_to_mindmap

    mov eax, [rel map_y]
    cmp r13d, eax
    jl .back_to_mindmap
    add eax, MAP_H
    cmp r13d, eax
    jge .back_to_mindmap

    ; Click in memory map - find which region
    call find_clicked_region
    mov [rel selected_rgn], eax
    jmp .done_click

.back_to_mindmap:
    ; Click outside map - go back to mindmap
    mov dword [rel view_mode], 0
    mov dword [rel selected_rgn], -1

.done_click:
    mov eax, 1
    jmp .done

.quit:
    xor eax, eax
.done:
    add rsp, 8
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; find_clicked_region — Returns region index or -1
;; Uses mouse_x, mouse_y and map grid params
;; ============================================================
find_clicked_region:
    push rbx
    push r12

    ; Get grid params
    mov eax, [rel map_grid_dim]
    test eax, eax
    jz .none
    mov r12d, eax           ; grid dimension

    mov ecx, [rel map_cell_w]
    test ecx, ecx
    jz .none

    mov edx, [rel map_cell_h]
    test edx, edx
    jz .none

    ; Calculate column: (mouse_x - map_grid_x) / cell_w
    mov eax, [rel mouse_x]
    sub eax, [rel map_grid_x]
    jl .none
    xor edx, edx
    div dword [rel map_cell_w]
    cmp eax, r12d
    jge .none
    mov ebx, eax            ; col

    ; Calculate row: (mouse_y - map_grid_y) / cell_h
    mov eax, [rel mouse_y]
    sub eax, [rel map_grid_y]
    jl .none
    xor edx, edx
    div dword [rel map_cell_h]
    cmp eax, r12d
    jge .none
    ; row in eax, col in ebx

    ; Index = row * dim + col
    imul eax, r12d
    add eax, ebx

    ; Check against region count
    mov rcx, [rel surface_ptr]
    mov ecx, [rcx + STATE_OFFSET + ST_REGION_COUNT]
    cmp eax, ecx
    jge .none

    jmp .done

.none:
    mov eax, -1
.done:
    pop r12
    pop rbx
    ret

;; ============================================================
;; handle_motion — Mouse motion for hover
;; ============================================================
handle_motion:
    push rbx
    push r12
    push r13

    call gfx_get_mouse_pos
    mov [rel mouse_x], eax
    mov [rel mouse_y], edx
    mov r12d, eax
    mov r13d, edx

    ; Find hovered button
    lea rbx, [rel buttons]
    xor ecx, ecx
    mov dword [rel hover_btn], -1

.hover_loop:
    cmp ecx, [rel num_buttons]
    jge .done

    mov eax, [rbx]
    cmp r12d, eax
    jl .next_hover
    add eax, [rbx + 8]
    cmp r12d, eax
    jge .next_hover

    mov eax, [rbx + 4]
    cmp r13d, eax
    jl .next_hover
    add eax, [rbx + 12]
    cmp r13d, eax
    jge .next_hover

    mov [rel hover_btn], ecx
    jmp .done

.next_hover:
    add rbx, 16
    inc ecx
    jmp .hover_loop

.done:
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; do_action — Execute button action
;; edi = action ID (1-based)
;; ============================================================
do_action:
    push rbx
    push r12
    sub rsp, 8

    mov ebx, edi

    cmp ebx, ACT_QUIT
    je .quit

    cmp ebx, ACT_DREAM
    je .do_dream
    cmp ebx, ACT_OBSERVE
    je .do_observe
    cmp ebx, ACT_EVOLVE
    je .do_evolve
    cmp ebx, ACT_STEP
    je .do_step
    cmp ebx, ACT_RUN
    je .do_run
    cmp ebx, ACT_SAVE
    je .do_save
    cmp ebx, ACT_LOAD
    je .do_load
    cmp ebx, ACT_FILE
    je .do_file
    cmp ebx, ACT_DIR
    je .do_dir
    cmp ebx, ACT_TRACE
    je .do_trace
    cmp ebx, ACT_GEOM
    je .do_geom
    cmp ebx, ACT_SEND
    je .do_send
    cmp ebx, ACT_CLEAR
    je .do_clear
    jmp .continue

.do_dream:
    call mcp_dream
    lea rax, [rel msg_dream]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 60
    jmp .continue

.do_observe:
    call mcp_observe
    lea rax, [rel msg_observe]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 60
    jmp .continue

.do_evolve:
    call mcp_evolve
    lea rax, [rel msg_evolve]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 60
    jmp .continue

.do_step:
    ; Send a space character via MCP
    lea rdi, [rel step_text]
    call mcp_send_text
    lea rax, [rel msg_step]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 60
    jmp .continue

.do_run:
    ; Send run100 via MCP raw command
    lea rdi, [rel raw_tool]
    lea rsi, [rel run_cmd_arg]
    call mcp_call
    lea rax, [rel msg_run]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 60
    jmp .continue

.do_save:
    call mcp_save
    lea rax, [rel msg_save]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 60
    jmp .continue

.do_load:
    call mcp_load
    lea rax, [rel msg_load]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 60
    jmp .continue

.do_file:
    call load_file
    jmp .continue

.do_dir:
    call load_dir
    jmp .continue

.do_trace:
    ; Toggle trace via MCP
    mov eax, [rel trace_active]
    test eax, eax
    jnz .trace_off
    mov dword [rel trace_active], 1
    lea rdi, [rel raw_tool]
    lea rsi, [rel trace_on_arg]
    call mcp_call
    lea rax, [rel msg_trace_on]
    jmp .trace_msg
.trace_off:
    mov dword [rel trace_active], 0
    lea rdi, [rel raw_tool]
    lea rsi, [rel trace_off_arg]
    call mcp_call
    lea rax, [rel msg_trace_off]
.trace_msg:
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 60
    jmp .continue

.do_geom:
    ; Cycle geom mode (0→1→2→0)
    mov eax, [rel geom_mode]
    inc eax
    cmp eax, 3
    jl .geom_set
    xor eax, eax
.geom_set:
    mov [rel geom_mode], eax
    ; Send to MCP
    push rax
    lea rdi, [rel raw_tool]
    lea rsi, [rel geom_cmd_arg]
    call mcp_call
    pop rax
    ; Feedback based on mode
    cmp eax, 0
    jne .geom_not_abs
    lea rax, [rel msg_geom_abs]
    jmp .geom_msg
.geom_not_abs:
    cmp eax, 1
    jne .geom_not_vec
    lea rax, [rel msg_geom_vec]
    jmp .geom_msg
.geom_not_vec:
    lea rax, [rel msg_geom_both]
.geom_msg:
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 60
    jmp .continue

.do_send:
    call do_send_input
    jmp .continue

.do_clear:
    mov dword [rel input_len], 0
    mov dword [rel selected_rgn], -1
    lea rax, [rel msg_clear]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 60
    jmp .continue

.continue:
    mov eax, 1
    jmp .done

.quit:
    xor eax, eax
.done:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; do_send_input
;; ============================================================
do_send_input:
    push rbx
    push r12
    sub rsp, 8

    mov r12d, [rel input_len]
    test r12d, r12d
    jz .done

    ; Null-terminate input buffer
    lea rdi, [rel input_buf]
    mov byte [rdi + r12], 0

    ; Send via MCP
    lea rdi, [rel input_buf]
    call mcp_send_text

    mov dword [rel input_len], 0
    lea rax, [rel msg_sent]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 60

.done:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; draw_frame — Draw complete frame
;; ============================================================
draw_frame:
    push rbx
    push r12
    sub rsp, 8

    ; Clear
    mov edi, [rel col_bg]
    call gfx_clear

    ; Menu bar
    xor edi, edi
    xor esi, esi
    mov edx, WIN_W
    mov ecx, MENU_H
    mov r8d, 0x00101025
    call gfx_fill_rect

    ; Draw buttons
    call draw_buttons

    ; Main mind map canvas
    call draw_canvas

    ; Draw all nodes and connections
    call draw_mindmap

    ; Input area
    call draw_input

    ; Status bar
    call draw_status

    ; Feedback
    call draw_feedback

    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; draw_buttons
;; ============================================================
draw_buttons:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8

    lea rbx, [rel buttons]
    xor r12d, r12d

.btn_loop:
    cmp r12d, [rel num_buttons]
    jge .done

    ; Get rect (h loaded directly before each gfx call since ecx is caller-saved)
    mov r13d, [rbx]         ; x
    mov r14d, [rbx + 4]     ; y
    mov r15d, [rbx + 8]     ; w

    ; Color
    mov r8d, [rel col_btn]
    cmp r12d, [rel hover_btn]
    jne .not_hover
    mov r8d, [rel col_btn_hover]
.not_hover:

    ; Fill - args: edi=x, esi=y, edx=w, ecx=h, r8d=color
    mov edi, r13d
    mov esi, r14d
    mov edx, r15d
    mov ecx, [rbx + 12]         ; reload h (don't use push - breaks alignment!)
    call gfx_fill_rect

    ; Border - args: edi=x, esi=y, edx=w, ecx=h, r8d=color
    mov edi, r13d
    mov esi, r14d
    mov edx, r15d
    mov ecx, [rbx + 12]         ; reload h
    mov r8d, [rel col_border]
    call gfx_rect

    ; Label - center text in button
    add r13d, 5
    add r14d, 20
    mov edi, r13d
    mov esi, r14d
    mov r8d, [rel col_text]

    ; Get label and length for this button
    mov eax, r12d
    lea rdx, [rel btn_dream]
    mov ecx, 5
    cmp eax, 0
    je .draw_lbl
    lea rdx, [rel btn_observe]
    mov ecx, 7
    cmp eax, 1
    je .draw_lbl
    lea rdx, [rel btn_evolve]
    mov ecx, 6
    cmp eax, 2
    je .draw_lbl
    lea rdx, [rel btn_step]
    mov ecx, 4
    cmp eax, 3
    je .draw_lbl
    lea rdx, [rel btn_run]
    mov ecx, 3
    cmp eax, 4
    je .draw_lbl
    lea rdx, [rel btn_save]
    mov ecx, 4
    cmp eax, 5
    je .draw_lbl
    lea rdx, [rel btn_load]
    mov ecx, 4
    cmp eax, 6
    je .draw_lbl
    lea rdx, [rel btn_file]
    mov ecx, 4
    cmp eax, 7
    je .draw_lbl
    lea rdx, [rel btn_dir]
    mov ecx, 3
    cmp eax, 8
    je .draw_lbl
    lea rdx, [rel btn_trace]
    mov ecx, 5
    cmp eax, 9
    je .draw_lbl
    lea rdx, [rel btn_geom]
    mov ecx, 4
    cmp eax, 10
    je .draw_lbl
    lea rdx, [rel btn_send]
    mov ecx, 4
    cmp eax, 11
    je .draw_lbl
    lea rdx, [rel btn_clear]
    mov ecx, 5
    cmp eax, 12
    je .draw_lbl
    lea rdx, [rel btn_quit]
    mov ecx, 4

.draw_lbl:
    call gfx_text

    add rbx, 16
    inc r12d
    jmp .btn_loop

.done:
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; draw_canvas — Draw the main mind map canvas background
;; ============================================================
draw_canvas:
    push rbx
    sub rsp, 8

    ; Canvas background
    mov edi, CANVAS_X
    mov esi, CANVAS_Y
    mov edx, CANVAS_W
    mov ecx, CANVAS_H
    mov r8d, 0x00101520
    call gfx_fill_rect

    ; Canvas border
    mov edi, CANVAS_X
    mov esi, CANVAS_Y
    mov edx, CANVAS_W
    mov ecx, CANVAS_H
    mov r8d, [rel col_border]
    call gfx_rect

    add rsp, 8
    pop rbx
    ret

;; ============================================================
;; draw_mindmap — Draw the interactive mind map visualization
;; Central node with connected child nodes, all clickable
;; ============================================================
draw_mindmap:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 40

    mov rbx, [rel surface_ptr]

    ; Check view mode first
    mov eax, [rel view_mode]
    cmp eax, 1
    je .draw_brain_view

    ; Calculate center of canvas
    mov r12d, CANVAS_X + CANVAS_W / 2   ; center x
    mov r13d, CANVAS_Y + CANVAS_H / 2   ; center y

    ; Check if a node is focused (zoomed)
    mov eax, [rel focus_node]
    cmp eax, -1
    jne .draw_focused

    ; === ROOT VIEW: Show main system nodes ===
    
    ; Central "UHMA" node
    lea edi, [r12d - 60]
    lea esi, [r13d - 25]
    mov edx, 120
    mov ecx, 50
    mov r8d, [rel col_dispatch]
    call gfx_fill_rect

    lea edi, [r12d - 60]
    lea esi, [r13d - 25]
    mov edx, 120
    mov ecx, 50
    mov r8d, [rel col_white]
    call gfx_rect

    lea edi, [r12d - 30]
    lea esi, [r13d + 5]
    lea rdx, [rel node_uhma]
    mov ecx, 4
    mov r8d, [rel col_white]
    call gfx_text

    ; Store central node rect for clicks
    lea rax, [rel panel_rects]
    lea edi, [r12d - 60]
    mov [rax], edi
    lea edi, [r13d - 25]
    mov [rax + 4], edi
    mov dword [rax + 8], 120
    mov dword [rax + 12], 50

    ; === BRAIN node (top-center, above UHMA) ===
    ; Position: centered above UHMA
    lea r14d, [r12d - 50]       ; x = center - 50
    lea r15d, [r13d - 120]      ; y = above UHMA

    ; Connection line: from UHMA top to BRAIN bottom
    mov edi, r12d               ; UHMA center x
    lea esi, [r13d - 25]        ; UHMA top edge
    mov edx, r12d               ; BRAIN center x
    lea ecx, [r15d + 45]        ; BRAIN bottom edge
    mov r8d, [rel col_magenta]
    call gfx_line

    ; Node box - magenta/purple for brain
    mov edi, r14d
    mov esi, r15d
    mov edx, 100
    mov ecx, 45
    mov r8d, 0x00663388         ; purple fill
    call gfx_fill_rect

    mov edi, r14d
    mov esi, r15d
    mov edx, 100
    mov ecx, 45
    mov r8d, [rel col_magenta]
    call gfx_rect

    ; Label
    lea edi, [r14d + 25]
    lea esi, [r15d + 28]
    lea rdx, [rel node_brain]
    mov ecx, 5
    mov r8d, [rel col_white]
    call gfx_text

    ; Store BRAIN rect for clicks (panel_rects + 16)
    lea rax, [rel panel_rects]
    mov [rax + 16], r14d
    mov [rax + 20], r15d
    mov dword [rax + 24], 100
    mov dword [rax + 28], 45

    ; === Draw child nodes around center ===
    ; Lines connect from UHMA edges to child node edges (not through center)
    ; UHMA box: x=[r12-60, r12+60], y=[r13-25, r13+25]

    ; Node 1: REGIONS (top-left)
    lea r14d, [r12d - 200]
    lea r15d, [r13d - 150]

    ; Connection line: from UHMA top-left edge to REGIONS bottom-right
    lea edi, [r12d - 50]        ; UHMA left side, slightly in
    lea esi, [r13d - 25]        ; UHMA top edge
    lea edx, [r14d + 100]       ; REGIONS right edge
    lea ecx, [r15d + 40]        ; REGIONS bottom edge
    mov r8d, [rel col_cyan]
    call gfx_line

    ; Node box
    mov edi, r14d
    mov esi, r15d
    mov edx, 100
    mov ecx, 40
    mov r8d, [rel col_nursery]
    call gfx_fill_rect
    
    mov edi, r14d
    mov esi, r15d
    mov edx, 100
    mov ecx, 40
    mov r8d, [rel col_white]
    call gfx_rect

    ; Region count inside node
    mov eax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    lea rdi, [rsp]
    call format_num
    mov [rsp + 32], eax

    lea edi, [r14d + 10]
    lea esi, [r15d + 15]
    lea rdx, [rel node_regions]
    mov ecx, 7
    mov r8d, [rel col_text]
    call gfx_text

    lea edi, [r14d + 10]
    lea esi, [r15d + 28]
    lea rdx, [rsp]
    mov ecx, [rsp + 32]
    mov r8d, [rel col_white]
    call gfx_text

    ; Node 2: TOKENS (top-right)
    lea r14d, [r12d + 100]
    lea r15d, [r13d - 150]

    ; Connection line: from UHMA top-right edge to TOKENS bottom-left
    lea edi, [r12d + 50]        ; UHMA right side, slightly in
    lea esi, [r13d - 25]        ; UHMA top edge
    mov edx, r14d               ; TOKENS left edge
    lea ecx, [r15d + 40]        ; TOKENS bottom edge
    mov r8d, [rel col_cyan]
    call gfx_line

    mov edi, r14d
    mov esi, r15d
    mov edx, 100
    mov ecx, 40
    mov r8d, 0x00448866
    call gfx_fill_rect

    mov edi, r14d
    mov esi, r15d
    mov edx, 100
    mov ecx, 40
    mov r8d, [rel col_white]
    call gfx_rect

    mov eax, [rbx + STATE_OFFSET + ST_TOKEN_COUNT]
    lea rdi, [rsp]
    call format_num
    mov [rsp + 32], eax

    lea edi, [r14d + 10]
    lea esi, [r15d + 15]
    lea rdx, [rel node_tokens]
    mov ecx, 6
    mov r8d, [rel col_text]
    call gfx_text

    lea edi, [r14d + 10]
    lea esi, [r15d + 28]
    lea rdx, [rsp]
    mov ecx, [rsp + 32]
    mov r8d, [rel col_white]
    call gfx_text

    ; Node 3: STATE (left)
    lea r14d, [r12d - 250]
    mov r15d, r13d
    
    mov edi, r12d
    sub edi, 60
    mov esi, r13d
    mov edx, r14d
    add edx, 90
    mov ecx, r15d
    add ecx, 20
    mov r8d, [rel col_cyan]
    call gfx_line

    mov edi, r14d
    mov esi, r15d
    mov edx, 90
    mov ecx, 40
    mov r8d, 0x00664488
    call gfx_fill_rect

    mov edi, r14d
    mov esi, r15d
    mov edx, 90
    mov ecx, 40
    mov r8d, [rel col_white]
    call gfx_rect

    ; Get state name
    mov eax, [rbx + STATE_OFFSET + ST_INTRO_STATE]
    cmp eax, 7
    jl .state_ok
    xor eax, eax
.state_ok:
    lea rcx, [rel intro_names]
    mov rdx, [rcx + rax * 8]
    
    lea edi, [r14d + 8]
    lea esi, [r15d + 15]
    lea rdx, [rel node_state]
    mov ecx, 5
    mov r8d, [rel col_text]
    call gfx_text

    mov eax, [rbx + STATE_OFFSET + ST_INTRO_STATE]
    cmp eax, 7
    jl .state_ok2
    xor eax, eax
.state_ok2:
    lea rcx, [rel intro_names]
    mov rdx, [rcx + rax * 8]
    lea edi, [r14d + 8]
    lea esi, [r15d + 28]
    mov ecx, 10
    mov r8d, [rel col_green]
    call gfx_text

    ; Node 4: PREDICT (right)
    lea r14d, [r12d + 160]
    mov r15d, r13d
    
    mov edi, r12d
    add edi, 60
    mov esi, r13d
    mov edx, r14d
    mov ecx, r15d
    add ecx, 20
    mov r8d, [rel col_cyan]
    call gfx_line

    mov edi, r14d
    mov esi, r15d
    mov edx, 100
    mov ecx, 40
    mov r8d, 0x00886644
    call gfx_fill_rect

    mov edi, r14d
    mov esi, r15d
    mov edx, 100
    mov ecx, 40
    mov r8d, [rel col_white]
    call gfx_rect

    lea edi, [r14d + 10]
    lea esi, [r15d + 15]
    lea rdx, [rel node_predict]
    mov ecx, 7
    mov r8d, [rel col_text]
    call gfx_text

    ; Show last prediction
    mov eax, [rbx + STATE_OFFSET + ST_LAST_PREDICT]
    lea rdi, [rsp]
    call format_hex
    mov [rsp + 32], eax

    lea edi, [r14d + 10]
    lea esi, [r15d + 28]
    lea rdx, [rsp]
    mov ecx, [rsp + 32]
    mov r8d, [rel col_yellow]
    call gfx_text

    ; Node 5: DISPATCH (bottom-left)
    lea r14d, [r12d - 200]
    lea r15d, [r13d + 120]

    ; Connection line: from UHMA bottom-left edge to DISPATCH top-right
    lea edi, [r12d - 50]        ; UHMA left side
    lea esi, [r13d + 25]        ; UHMA bottom edge
    lea edx, [r14d + 100]       ; DISPATCH right edge
    mov ecx, r15d               ; DISPATCH top edge
    mov r8d, [rel col_cyan]
    call gfx_line

    mov edi, r14d
    mov esi, r15d
    mov edx, 100
    mov ecx, 40
    mov r8d, [rel col_dispatch]
    call gfx_fill_rect

    mov edi, r14d
    mov esi, r15d
    mov edx, 100
    mov ecx, 40
    mov r8d, [rel col_white]
    call gfx_rect

    lea edi, [r14d + 10]
    lea esi, [r15d + 15]
    lea rdx, [rel node_dispatch]
    mov ecx, 8
    mov r8d, [rel col_text]
    call gfx_text

    ; Get mode name
    mov eax, [rbx + STATE_OFFSET + ST_DISPATCH_MODE]
    cmp eax, 8
    jl .mode_ok
    xor eax, eax
.mode_ok:
    lea rcx, [rel mode_names]
    mov rdx, [rcx + rax * 8]
    lea edi, [r14d + 10]
    lea esi, [r15d + 28]
    mov ecx, 10
    mov r8d, [rel col_yellow]
    call gfx_text

    ; Node 6: ACCURACY (bottom-right)
    lea r14d, [r12d + 100]
    lea r15d, [r13d + 120]

    ; Connection line: from UHMA bottom-right edge to ACCURACY top-left
    lea edi, [r12d + 50]        ; UHMA right side
    lea esi, [r13d + 25]        ; UHMA bottom edge
    mov edx, r14d               ; ACCURACY left edge
    mov ecx, r15d               ; ACCURACY top edge
    mov r8d, [rel col_cyan]
    call gfx_line

    mov edi, r14d
    mov esi, r15d
    mov edx, 100
    mov ecx, 40
    mov r8d, 0x00446688
    call gfx_fill_rect

    mov edi, r14d
    mov esi, r15d
    mov edx, 100
    mov ecx, 40
    mov r8d, [rel col_white]
    call gfx_rect

    lea edi, [r14d + 10]
    lea esi, [r15d + 15]
    lea rdx, [rel node_accuracy]
    mov ecx, 8
    mov r8d, [rel col_text]
    call gfx_text

    ; Calculate and display accuracy %
    mov eax, [rbx + STATE_OFFSET + ST_SELF_PRED_HITS]
    mov ecx, [rbx + STATE_OFFSET + ST_SELF_PRED_MISSES]
    add ecx, eax
    test ecx, ecx
    jz .no_acc
    imul eax, 100
    xor edx, edx
    div ecx
    lea rdi, [rsp]
    call format_num
    mov [rsp + 32], eax
    lea edi, [r14d + 10]
    lea esi, [r15d + 28]
    lea rdx, [rsp]
    mov ecx, [rsp + 32]
    mov r8d, [rel col_green]
    call gfx_text
    ; Add % sign
    lea edi, [r14d + 35]
    lea esi, [r15d + 28]
    lea rdx, [rel pct_sign]
    mov ecx, 1
    mov r8d, [rel col_green]
    call gfx_text
    jmp .acc_done
.no_acc:
    lea edi, [r14d + 10]
    lea esi, [r15d + 28]
    lea rdx, [rel node_na]
    mov ecx, 3
    mov r8d, [rel col_text_dim]
    call gfx_text
.acc_done:

    ; === NEW FEATURE NODES ===

    ; Node 7: HIVE (pheromone-driven swarm intelligence) - far bottom left
    lea r14d, [r12d - 350]
    lea r15d, [r13d + 60]

    ; Connection line from UHMA bottom-left
    lea edi, [r12d - 60]
    lea esi, [r13d + 20]
    lea edx, [r14d + 110]
    mov ecx, r15d
    mov r8d, [rel col_yellow]
    call gfx_line

    ; Node box - yellow/gold for hive
    mov edi, r14d
    mov esi, r15d
    mov edx, 110
    mov ecx, 70
    mov r8d, 0x00554422
    call gfx_fill_rect

    mov edi, r14d
    mov esi, r15d
    mov edx, 110
    mov ecx, 70
    mov r8d, [rel col_yellow]
    call gfx_rect

    ; Label
    lea edi, [r14d + 20]
    lea esi, [r15d + 15]
    lea rdx, [rel node_hive]
    mov ecx, 4
    mov r8d, [rel col_white]
    call gfx_text

    ; Draw pheromone level bars
    ; Dream pheromone bar
    movsd xmm0, [rbx + STATE_OFFSET + ST_DREAM_PRESSURE]
    mulsd xmm0, qword [rel bar_scale_60]
    cvttsd2si eax, xmm0
    cmp eax, 60
    jle .hive_d_ok
    mov eax, 60
.hive_d_ok:
    test eax, eax
    js .hive_d_zero
    jmp .hive_d_draw
.hive_d_zero:
    xor eax, eax
.hive_d_draw:
    push rax
    lea edi, [r14d + 8]
    lea esi, [r15d + 28]
    mov edx, eax
    mov ecx, 8
    mov r8d, [rel col_magenta]
    call gfx_fill_rect
    pop rax

    ; Observe pheromone bar
    movsd xmm0, [rbx + STATE_OFFSET + ST_OBSERVE_PRESSURE]
    mulsd xmm0, qword [rel bar_scale_60]
    cvttsd2si eax, xmm0
    cmp eax, 60
    jle .hive_o_ok
    mov eax, 60
.hive_o_ok:
    test eax, eax
    js .hive_o_zero
    jmp .hive_o_draw
.hive_o_zero:
    xor eax, eax
.hive_o_draw:
    push rax
    lea edi, [r14d + 8]
    lea esi, [r15d + 40]
    mov edx, eax
    mov ecx, 8
    mov r8d, [rel col_green]
    call gfx_fill_rect
    pop rax

    ; Evolve pheromone bar
    movsd xmm0, [rbx + STATE_OFFSET + ST_EVOLVE_PRESSURE]
    mulsd xmm0, qword [rel bar_scale_60]
    cvttsd2si eax, xmm0
    cmp eax, 60
    jle .hive_e_ok
    mov eax, 60
.hive_e_ok:
    test eax, eax
    js .hive_e_zero
    jmp .hive_e_draw
.hive_e_zero:
    xor eax, eax
.hive_e_draw:
    lea edi, [r14d + 8]
    lea esi, [r15d + 52]
    mov edx, eax
    mov ecx, 8
    mov r8d, [rel col_orange]
    call gfx_fill_rect

    ; Bar labels (D/O/E)
    lea edi, [r14d + 75]
    lea esi, [r15d + 35]
    lea rdx, [rel lbl_d]
    mov ecx, 1
    mov r8d, [rel col_magenta]
    call gfx_text

    lea edi, [r14d + 75]
    lea esi, [r15d + 47]
    lea rdx, [rel lbl_o]
    mov ecx, 1
    mov r8d, [rel col_green]
    call gfx_text

    lea edi, [r14d + 75]
    lea esi, [r15d + 59]
    lea rdx, [rel lbl_e]
    mov ecx, 1
    mov r8d, [rel col_orange]
    call gfx_text

    ; Node 8: ROSETTA (geometric verification) - far bottom right
    lea r14d, [r12d + 240]
    lea r15d, [r13d + 60]

    ; Connection line from UHMA bottom-right
    lea edi, [r12d + 60]
    lea esi, [r13d + 20]
    mov edx, r14d
    mov ecx, r15d
    mov r8d, [rel col_cyan]
    call gfx_line

    ; Node box - cyan for rosetta
    mov edi, r14d
    mov esi, r15d
    mov edx, 100
    mov ecx, 50
    mov r8d, 0x00224455
    call gfx_fill_rect

    mov edi, r14d
    mov esi, r15d
    mov edx, 100
    mov ecx, 50
    mov r8d, [rel col_cyan]
    call gfx_rect

    ; Label
    lea edi, [r14d + 10]
    lea esi, [r15d + 15]
    lea rdx, [rel node_rosetta]
    mov ecx, 7
    mov r8d, [rel col_white]
    call gfx_text

    ; Get cached verify mode and display
    mov eax, [rel geom_mode]
    cmp eax, 0
    jne .ros_not_abs
    lea rdx, [rel geom_mode_abstract]
    mov ecx, 8
    jmp .ros_show
.ros_not_abs:
    cmp eax, 1
    jne .ros_not_geom
    lea rdx, [rel geom_mode_geometric]
    mov ecx, 9
    jmp .ros_show
.ros_not_geom:
    lea rdx, [rel geom_mode_both]
    mov ecx, 4
.ros_show:
    lea edi, [r14d + 10]
    lea esi, [r15d + 35]
    mov r8d, [rel col_cyan]
    call gfx_text

    ; Node 9: MYCO (mycorrhiza colony) - far left middle
    lea r14d, [r12d - 370]
    lea r15d, [r13d - 80]

    ; Connection line from UHMA left
    mov edi, r12d
    sub edi, 60
    lea esi, [r13d - 10]
    lea edx, [r14d + 90]
    lea ecx, [r15d + 25]
    mov r8d, [rel col_green]
    call gfx_line

    ; Node box - green for myco
    mov edi, r14d
    mov esi, r15d
    mov edx, 90
    mov ecx, 50
    mov r8d, 0x00224422
    call gfx_fill_rect

    mov edi, r14d
    mov esi, r15d
    mov edx, 90
    mov ecx, 50
    mov r8d, [rel col_green]
    call gfx_rect

    ; Label
    lea edi, [r14d + 15]
    lea esi, [r15d + 15]
    lea rdx, [rel node_myco]
    mov ecx, 4
    mov r8d, [rel col_white]
    call gfx_text

    ; Get colony status
    mov eax, [rbx + STATE_OFFSET + ST_SHARED_MODE]
    test eax, eax
    jz .myco_solo
    lea rdx, [rel colony_hive]
    mov ecx, 9
    jmp .myco_show
.myco_solo:
    lea rdx, [rel colony_solo]
    mov ecx, 4
.myco_show:
    lea edi, [r14d + 10]
    lea esi, [r15d + 35]
    mov r8d, [rel col_green]
    call gfx_text

    ; Node 10: SPORE (gene export/import) - far right middle
    lea r14d, [r12d + 280]
    lea r15d, [r13d - 80]

    ; Connection line from UHMA right
    mov edi, r12d
    add edi, 60
    lea esi, [r13d - 10]
    mov edx, r14d
    lea ecx, [r15d + 25]
    mov r8d, [rel col_orange]
    call gfx_line

    ; Node box - orange for spore
    mov edi, r14d
    mov esi, r15d
    mov edx, 90
    mov ecx, 50
    mov r8d, 0x00443322
    call gfx_fill_rect

    mov edi, r14d
    mov esi, r15d
    mov edx, 90
    mov ecx, 50
    mov r8d, [rel col_orange]
    call gfx_rect

    ; Label
    lea edi, [r14d + 15]
    lea esi, [r15d + 15]
    lea rdx, [rel node_spore]
    mov ecx, 5
    mov r8d, [rel col_white]
    call gfx_text

    ; Show gene status
    lea edi, [r14d + 10]
    lea esi, [r15d + 35]
    lea rdx, [rel lbl_genes]
    mov ecx, 7
    mov r8d, [rel col_orange]
    call gfx_text

    ; Instructions at bottom
    mov edi, CANVAS_X + 20
    mov esi, CANVAS_Y + CANVAS_H - 25
    lea rdx, [rel hint_click]
    mov ecx, 30
    mov r8d, [rel col_text_dim]
    call gfx_text

    jmp .done

.draw_brain_view:
    ; === BRAIN VIEW: Memory map grid showing all regions ===
    ; Title
    mov edi, CANVAS_X + 20
    mov esi, CANVAS_Y + 25
    lea rdx, [rel lbl_memory]
    mov ecx, 10
    mov r8d, [rel col_magenta]
    call gfx_text

    ; Back hint
    mov edi, CANVAS_X + CANVAS_W - 200
    mov esi, CANVAS_Y + 25
    lea rdx, [rel hint_back]
    mov ecx, 25
    mov r8d, [rel col_text_dim]
    call gfx_text

    ; Get region count
    mov eax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    test eax, eax
    jz .brain_empty
    mov [rsp + 32], eax         ; save region count

    ; Calculate grid dimensions
    ; sqrt approximation: find n where n*n >= count
    mov ecx, 1
.find_dim:
    mov eax, ecx
    imul eax, ecx
    cmp eax, [rsp + 32]
    jge .dim_found
    inc ecx
    cmp ecx, 20
    jl .find_dim
.dim_found:
    mov [rel map_grid_dim], ecx

    ; Calculate cell size
    mov eax, CANVAS_W - 40
    xor edx, edx
    div ecx
    mov [rel map_cell_w], eax

    mov eax, CANVAS_H - 80
    xor edx, edx
    div ecx
    mov [rel map_cell_h], eax

    ; Set map position for click detection
    mov dword [rel map_x], CANVAS_X + 20
    mov dword [rel map_y], CANVAS_Y + 50
    mov eax, CANVAS_X + 20
    mov [rel map_grid_x], eax
    mov eax, CANVAS_Y + 50
    mov [rel map_grid_y], eax

    ; Draw grid of regions
    xor r12d, r12d              ; region index
    mov r13d, CANVAS_Y + 50     ; current y
    mov r14d, [rel map_grid_dim] ; grid dim

.brain_row:
    mov r15d, CANVAS_X + 20     ; current x
    xor ecx, ecx                ; col counter

.brain_col:
    cmp r12d, [rsp + 32]        ; check if past region count
    jge .brain_grid_done

    push rcx

    ; Get region table entry pointer
    mov rdi, REGION_TABLE_OFFSET
    mov eax, r12d
    imul eax, RTE_SIZE
    add rdi, rax

    ; Get region type for color
    movzx eax, word [rdi + RTE_TYPE]
    mov r8d, [rel col_panel]    ; default

    cmp eax, 0                  ; DISPATCH
    jne .not_dispatch_col
    mov r8d, [rel col_dispatch]
    jmp .got_color
.not_dispatch_col:
    cmp eax, 1                  ; VSA
    jne .not_vsa_col
    mov r8d, [rel col_cyan]
    jmp .got_color
.not_vsa_col:
    cmp eax, 2                  ; MODIFIER
    jne .not_mod_col
    mov r8d, [rel col_yellow]
    jmp .got_color
.not_mod_col:
    cmp eax, 3                  ; OBSERVER
    jne .not_obs_col
    mov r8d, [rel col_green]
    jmp .got_color
.not_obs_col:
    cmp eax, 4                  ; EMITTER
    jne .not_emit_col
    mov r8d, [rel col_orange]
    jmp .got_color
.not_emit_col:
    cmp eax, 7                  ; DREAM
    jne .got_color
    mov r8d, [rel col_magenta]

.got_color:
    ; Check if selected
    cmp r12d, [rel selected_rgn]
    jne .not_selected
    mov r8d, [rel col_white]
.not_selected:

    ; Draw cell
    mov edi, r15d
    mov esi, r13d
    mov edx, [rel map_cell_w]
    sub edx, 2
    mov ecx, [rel map_cell_h]
    sub ecx, 2
    call gfx_fill_rect

    ; Border
    mov edi, r15d
    mov esi, r13d
    mov edx, [rel map_cell_w]
    sub edx, 2
    mov ecx, [rel map_cell_h]
    sub ecx, 2
    mov r8d, [rel col_border]
    call gfx_rect

    pop rcx

    ; Next cell
    inc r12d
    add r15d, [rel map_cell_w]
    inc ecx
    cmp ecx, r14d
    jl .brain_col

    ; Next row
    add r13d, [rel map_cell_h]
    mov eax, r12d
    xor edx, edx
    div r14d
    cmp eax, r14d
    jl .brain_row

.brain_grid_done:
    ; Draw selected region info if any
    mov eax, [rel selected_rgn]
    cmp eax, -1
    je .brain_no_sel

    ; Show region details on right side
    mov edi, CANVAS_X + CANVAS_W - 250
    mov esi, CANVAS_Y + 60
    lea rdx, [rel lbl_region]
    mov ecx, 13
    mov r8d, [rel col_cyan]
    call gfx_text

    ; Get region table entry pointer
    mov rdi, REGION_TABLE_OFFSET
    mov eax, [rel selected_rgn]
    imul eax, RTE_SIZE
    add rdi, rax

    ; Show type
    movzx eax, word [rdi + RTE_TYPE]
    mov edi, CANVAS_X + CANVAS_W - 250
    mov esi, CANVAS_Y + 80
    lea rdx, [rel lbl_rgn_type]
    mov ecx, 6
    mov r8d, [rel col_text]
    call gfx_text

.brain_no_sel:
    jmp .done

.brain_empty:
    mov edi, CANVAS_X + 100
    mov esi, CANVAS_Y + 200
    lea rdx, [rel lbl_none_sel]
    mov ecx, 16
    mov r8d, [rel col_text_dim]
    call gfx_text
    jmp .done

.draw_focused:
    ; === FOCUSED VIEW: Show detail of selected node ===
    ; eax = focused node id
    
    ; For now, show expanded view with "Back" hint
    mov edi, CANVAS_X + 20
    mov esi, CANVAS_Y + 30
    lea rdx, [rel hint_back]
    mov ecx, 20
    mov r8d, [rel col_cyan]
    call gfx_text

    ; Draw expanded content based on focus_node
    mov eax, [rel focus_node]
    
    ; Large centered node
    lea edi, [r12d - 150]
    lea esi, [r13d - 100]
    mov edx, 300
    mov ecx, 200
    mov r8d, [rel col_panel_hi]
    call gfx_fill_rect

    lea edi, [r12d - 150]
    lea esi, [r13d - 100]
    mov edx, 300
    mov ecx, 200
    mov r8d, [rel col_white]
    call gfx_rect

    ; Show detail based on which node (simplified for now)
    lea edi, [r12d - 130]
    lea esi, [r13d - 70]
    lea rdx, [rel detail_expanded]
    mov ecx, 15
    mov r8d, [rel col_cyan]
    call gfx_text

.done:
    add rsp, 40
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================
;; format_hex — Format number as hex string
;; eax = number, rdi = buffer
;; Returns length in eax
;; ============================================================
format_hex:
    push rbx
    push r12
    mov r12, rdi
    mov ebx, eax

    mov byte [rdi], '0'
    mov byte [rdi + 1], 'x'
    add rdi, 2

    mov ecx, 8
.hex_loop:
    rol ebx, 4
    mov eax, ebx
    and eax, 0xF
    cmp eax, 10
    jl .digit
    add eax, 'a' - 10
    jmp .store
.digit:
    add eax, '0'
.store:
    mov [rdi], al
    inc rdi
    dec ecx
    jnz .hex_loop

    mov byte [rdi], 0
    mov eax, 10
    pop r12
    pop rbx
    ret

;; ============================================================
;; draw_input — Input field
;; ============================================================
draw_input:
    push rbx
    push r12
    push r13
    sub rsp, 8

    mov r12d, 10
    mov r13d, WIN_H - INPUT_H - STATUS_H

    ; Background
    mov edi, r12d
    mov esi, r13d
    mov edx, WIN_W - 20
    mov ecx, INPUT_H - 5
    mov r8d, [rel col_panel]
    call gfx_fill_rect

    ; Input field bg
    lea edi, [r12d + 60]
    lea esi, [r13d + 10]
    mov edx, 1000
    mov ecx, 28
    mov r8d, [rel col_input_bg]
    call gfx_fill_rect

    ; Border
    lea edi, [r12d + 60]
    lea esi, [r13d + 10]
    mov edx, 1000
    mov ecx, 28
    mov r8d, [rel col_border]
    call gfx_rect

    ; Label
    lea edi, [r12d + 10]
    lea esi, [r13d + 25]
    lea rdx, [rel lbl_input]
    mov ecx, 5
    mov r8d, [rel col_text]
    call gfx_text

    ; Input text
    mov ecx, [rel input_len]
    test ecx, ecx
    jz .no_input

    lea edi, [r12d + 70]
    lea esi, [r13d + 25]
    lea rdx, [rel input_buf]
    mov r8d, [rel col_text]
    call gfx_text

.no_input:
    ; Cursor
    mov rax, [rel vis_frame]
    and eax, 0x1F
    cmp eax, 0x10
    jge .no_cursor

    lea edi, [r12d + 70]
    mov eax, [rel input_len]
    imul eax, 8
    add edi, eax
    lea esi, [r13d + 12]
    mov edx, 2
    mov ecx, 22
    mov r8d, [rel col_white]
    call gfx_fill_rect

.no_cursor:
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; draw_status — Status bar
;; ============================================================
draw_status:
    push rbx
    push r12
    push r13
    sub rsp, 24

    mov r12d, 0
    mov r13d, WIN_H - STATUS_H

    ; Background
    mov edi, r12d
    mov esi, r13d
    mov edx, WIN_W
    mov ecx, STATUS_H
    mov r8d, 0x00101020
    call gfx_fill_rect

    mov rbx, [rel surface_ptr]

    ; Step count
    lea edi, [r12d + 20]
    lea esi, [r13d + 22]
    mov eax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    lea rdi, [rsp]
    call format_num
    mov [rsp + 16], eax

    mov edi, 20
    lea esi, [r13d + 22]
    lea rdx, [rsp]
    mov ecx, [rsp + 16]
    mov r8d, [rel col_cyan]
    call gfx_text

    add rsp, 24
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; draw_feedback — Feedback message
;; ============================================================
draw_feedback:
    push rbx

    mov eax, [rel feedback_timer]
    test eax, eax
    jz .done

    mov rax, [rel feedback_msg]
    test rax, rax
    jz .done

    mov edi, 800
    mov esi, 25
    mov rdx, rax
    mov ecx, 20
    mov r8d, [rel col_green]
    call gfx_text

.done:
    pop rbx
    ret

;; ============================================================
;; format_num — Convert number to string
;; eax = number, rdi = buffer
;; Returns length in eax
;; ============================================================
format_num:
    push rbx
    push r12

    mov r12, rdi
    mov ebx, eax

    test eax, eax
    jnz .not_zero
    mov byte [rdi], '0'
    mov byte [rdi + 1], 0
    mov eax, 1
    jmp .done

.not_zero:
    add rdi, 15
    mov byte [rdi], 0
    dec rdi

    mov eax, ebx
    mov ecx, 10

.digit:
    test eax, eax
    jz .reverse
    xor edx, edx
    div ecx
    add dl, '0'
    mov [rdi], dl
    dec rdi
    jmp .digit

.reverse:
    inc rdi
    xor ecx, ecx
.copy:
    mov al, [rdi + rcx]
    mov [r12 + rcx], al
    test al, al
    jz .finish
    inc ecx
    jmp .copy

.finish:
    mov eax, ecx

.done:
    pop r12
    pop rbx
    ret

;; ============================================================
;; load_file — File picker and load
;; ============================================================
load_file:
    push rbx
    push r12
    sub rsp, 8

    ; Use zenity to pick file
    lea rdi, [rel zenity_file]
    call system
    test eax, eax
    jnz .error

    ; Read selected path from temp file
    lea rdi, [rel tmp_pick_path]
    lea rsi, [rel read_mode]
    call fopen
    test rax, rax
    jz .error
    mov rbx, rax

    lea rdi, [rel file_path_buf]
    mov esi, 511
    mov rdx, rbx
    call fgets
    mov r12, rax

    mov rdi, rbx
    call fclose

    test r12, r12
    jz .error

    ; Strip newline
    lea rdi, [rel file_path_buf]
    xor ecx, ecx
.strip:
    mov al, [rdi + rcx]
    test al, al
    jz .stripped
    cmp al, 10
    je .do_strip
    cmp al, 13
    je .do_strip
    inc ecx
    jmp .strip
.do_strip:
    mov byte [rdi + rcx], 0
.stripped:

    cmp byte [rdi], 0
    je .error

    ; Build "eat /path" command and send via MCP
    lea rdi, [rel eat_cmd_buf]
    lea rsi, [rel eat_cmd_pre]
    call strcpy
    lea rdi, [rel eat_cmd_buf]
    lea rsi, [rel file_path_buf]
    call strcat
    ; Close the quote
    lea rdi, [rel eat_cmd_buf]
    call strlen
    lea rdi, [rel eat_cmd_buf + rax]
    mov byte [rdi], '"'
    mov byte [rdi + 1], 0

    ; Send via MCP raw command
    lea rdi, [rel raw_tool]
    lea rsi, [rel eat_cmd_buf]
    call mcp_call

    lea rax, [rel msg_file_ok]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 90
    jmp .done

.error:
    lea rax, [rel msg_file_err]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 90

.done:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; load_dir — Directory picker and load all files
;; ============================================================
load_dir:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 520

    lea rdi, [rel zenity_dir]
    call system
    test eax, eax
    jnz .error

    lea rdi, [rel tmp_pick_path]
    lea rsi, [rel read_mode]
    call fopen
    test rax, rax
    jz .error
    mov rbx, rax

    lea rdi, [rel file_path_buf]
    mov esi, 511
    mov rdx, rbx
    call fgets
    mov r14, rax

    mov rdi, rbx
    call fclose

    test r14, r14
    jz .error

    ; Strip newline
    lea rdi, [rel file_path_buf]
    xor ecx, ecx
.strip:
    mov al, [rdi + rcx]
    test al, al
    jz .stripped
    cmp al, 10
    je .do_strip
    cmp al, 13
    je .do_strip
    inc ecx
    jmp .strip
.do_strip:
    mov byte [rdi + rcx], 0
.stripped:

    cmp byte [rdi], 0
    je .error

    lea rdi, [rel file_path_buf]
    call opendir
    test rax, rax
    jz .error
    mov r12, rax

.next:
    mov rdi, r12
    call readdir
    test rax, rax
    jz .dir_done
    mov r13, rax

    ; Skip . and ..
    cmp byte [r13 + 19], '.'
    jne .check
    cmp byte [r13 + 20], 0
    je .next
    cmp byte [r13 + 20], '.'
    jne .check
    cmp byte [r13 + 21], 0
    je .next

.check:
    cmp byte [r13 + 18], 8
    jne .next

    ; Build path
    lea rsi, [rel file_path_buf]
    lea rdi, [rsp]
    xor ecx, ecx
.cp_dir:
    mov al, [rsi + rcx]
    mov [rdi + rcx], al
    test al, al
    jz .cp_slash
    inc ecx
    jmp .cp_dir
.cp_slash:
    mov byte [rdi + rcx], '/'
    inc ecx
    lea rsi, [r13 + 19]
.cp_name:
    mov al, [rsi]
    mov [rdi + rcx], al
    test al, al
    jz .open
    inc rsi
    inc ecx
    jmp .cp_name

.open:
    ; Build "eat /path" command and send via MCP
    lea rdi, [rel eat_cmd_buf]
    lea rsi, [rel eat_cmd_pre]
    call strcpy
    lea rdi, [rel eat_cmd_buf]
    lea rsi, [rsp]              ; full file path
    call strcat
    ; Close the quote
    lea rdi, [rel eat_cmd_buf]
    call strlen
    lea rdi, [rel eat_cmd_buf + rax]
    mov byte [rdi], '"'
    mov byte [rdi + 1], 0

    ; Send via MCP
    push r12
    push r13
    lea rdi, [rel raw_tool]
    lea rsi, [rel eat_cmd_buf]
    call mcp_call
    pop r13
    pop r12
    jmp .next

.dir_done:
    mov rdi, r12
    call closedir
    ; Dream already happens inside eat via MCP
    call mcp_dream

    lea rax, [rel msg_dir_ok]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 90
    jmp .done

.error:
    lea rax, [rel msg_file_err]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 90

.done:
    add rsp, 520
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; vis_is_running
;; ============================================================
global vis_is_running
vis_is_running:
    mov eax, [rel vis_running]
    ret
