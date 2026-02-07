; visualizer.asm — UHMA Command & Control Center
; Real interactive GUI with clickable regions, live token stream, and system control
;
; @entry vis_init(rdi=surface_ptr) -> eax=1 on success
; @entry vis_update() -> eax=1 to continue, 0 to quit
; @entry vis_shutdown()
; @entry copy_ring_to_clipboard(rdi=ring_buf) -> copies 8KB ring buffer to X clipboard
; @entry poll_channels() -> sends status/receipts to QUERY/DEBUG panels (gateway subnets)
; @calls gfx.asm:gfx_init, gfx.asm:gfx_fill_rect, gfx.asm:gfx_text
; @calls mcp_client.asm:mcp_spawn_uhma, mcp_client.asm:mcp_send_async, mcp_client.asm:mcp_read_stream
; @calledby viz_main.asm:main
;
; FEATURES:
;   - DREAM button spawns UHMA in live/autonomous mode (batch OFF)
;   - FEED menu spawns UHMA in feed mode (batch ON)
;   - Side panels (FEED/QUERY/DEBUG) click to toggle pause + copy to clipboard
;   - FEED panel is live run-log (receipt stream), not polled
;   - Carousel nodes expand on click, collapse copies content to clipboard
;   - Auto-polling every ~3 seconds populates QUERY/DEBUG panels
;
; GOTCHAS:
;   - Stack alignment (x86-64 ABI): ODD pushes → aligned → sub must be multiple of 16
;                                   EVEN pushes → unaligned → sub must be 8 mod 16
;   - gfx_fill_rect takes 5 args: edi=x, esi=y, edx=w, ecx=h, r8d=color
;   - Callee-saved regs (rbx, r12-r15) must be preserved across gfx_* calls
;   - ecx is caller-saved: reload h from memory before each gfx call, don't push/pop
;   - Clipboard copy uses xclip via system() - requires xclip installed
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
    lbl_feed:       db "FEED", 0
    lbl_query:      db "QUERY", 0
    lbl_debug:      db "DEBUG", 0
    lbl_paused:     db "[PAUSED]", 0

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
    btn_url:        db "URL", 0
    btn_intro:      db "INTRO", 0
    btn_trace:      db "TRACE", 0
    btn_geom:       db "GEOM", 0
    btn_send:       db "SEND", 0
    btn_clear:      db "CLEAR", 0
    btn_quit:       db "QUIT", 0
    btn_feed:       db "FEED", 0
    btn_stop:       db "STOP", 0

    ; FEED dropdown menu items
    menu_quick:     db "Quick", 0
    menu_mastery:   db "Mastery", 0
    menu_live:      db "Live", 0
    menu_stop:      db "Stop", 0

    ; Feed commands for dropdown (tools/feeder training client)
    feed_cmd_quick:    db "./tools/feeder --corpus corpus/ --cycles 1 --pause 5 &", 0
    feed_cmd_mastery:  db "./tools/feeder --corpus corpus/ --cycles 0 --consolidate 30 &", 0
    feed_cmd_live:     db "./tools/feeder --corpus corpus/ --cycles 0 --consolidate 30 &", 0
    feed_cmd_stop:     db "./tools/feeder --shutdown", 0

    ; Clipboard copy (xclip)
    clip_tmpfile:      db "/tmp/uhma_clip.txt", 0
    clip_fmode:        db "w", 0
    clip_xclip:        db "xclip -selection clipboard < /tmp/uhma_clip.txt", 0
    clip_copied_msg:   db "Copied to clipboard", 0

    ; Polling commands for QUERY/DEBUG panels
    poll_status_cmd:   db "status", 0
    poll_receipts_cmd: db "receipts 5", 0

    ; Startup countdown message
    startup_msg:    db "Starting UHMA in ", 0
    startup_sec:    db "s... [S]erver mode", 0
    startup_wait:   db "Waiting for server...", 0

    ; MCP commands for node clicks
    cmd_status:     db "status", 0
    cmd_intro:      db "intro", 0
    cmd_hive:       db "hive", 0
    cmd_regions:    db "regions", 0
    cmd_receipts:   db "receipts 10", 0
    cmd_metacog:    db "metacog", 0
    cmd_drives:     db "drives", 0
    cmd_presence:   db "presence", 0
    cmd_self:       db "self", 0
    cmd_geom:       db "geom", 0
    cmd_colony:     db "colony", 0
    cmd_genes:      db "genes", 0
    cmd_dream:      db "dream", 0
    cmd_observe:    db "observe", 0
    cmd_evolve:     db "evolve", 0
    cmd_geom_0:     db "geom 0", 0
    cmd_geom_1:     db "geom 1", 0
    cmd_geom_2:     db "geom 2", 0

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
    zenity_url:     db "zenity --entry --title='Fetch URL' --text='Enter URL to digest:' 2>/dev/null > /tmp/.uhma_pick", 0
    zenity_feed:    db "zenity --entry --title='Feed Configuration' "
                    db "--text='Edit feeder command:' "
                    db "--entry-text='./tools/feeder --corpus corpus/ --cycles 1 --pause 5' "
                    db "2>/dev/null > /tmp/.uhma_feed", 0
    tmp_feed_path:  db "/tmp/.uhma_feed", 0
    tmp_pick_path:  db "/tmp/.uhma_pick", 0
    read_mode:      db "rb", 0

    ; Feedback messages
    msg_dream:      db "Dream complete", 0
    msg_observe:    db "Observe complete", 0
    msg_evolve:     db "Evolve complete", 0
    msg_step:       db "Step tick queued", 0
    msg_run:        db "Run 100 queued", 0
    msg_save:       db "State saved", 0
    msg_load:       db "State loaded", 0
    msg_sent:       db "Input sent", 0
    msg_clear:      db "Cleared", 0
    msg_file_ok:    db "File loaded", 0
    msg_dir_ok:     db "Dir loaded", 0
    msg_url_ok:     db "URL fed", 0
    msg_url_err:    db "URL fetch failed", 0
    msg_intro_ok:   db "Intro shown", 0
    msg_file_err:   db "Load failed", 0
    msg_trace_on:   db "Trace armed", 0
    msg_trace_off:  db "Trace shown", 0
    msg_geom_abs:   db "Verify: ABSTRACT", 0
    msg_geom_vec:   db "Verify: GEOMETRIC", 0
    msg_geom_both:  db "Verify: BOTH", 0
    msg_feed_start: db "Training started", 0
    msg_feed_stop:  db "Training stopped", 0
    msg_spawn_fail: db "Failed to spawn UHMA", 0
    msg_live_mode:  db "Live mode started", 0
    dbg_dream:      db "DEBUG: do_dream called", 10, 0
    dbg_spawning:   db "DEBUG: mcp_running=0, calling spawn", 10, 0

    ; Command strings
    cmd_step:       db "step", 0
    cmd_run_100:    db "run 100", 0
    cmd_trace:      db "trace", 0
    cmd_ccmode:     db "ccmode on", 0
    cmd_listen:     db "listen", 0
    eat_cmd_pre:    db "eat ", 0

    ; URL fetch helpers
    url_tmp_path:   db "/tmp/.uhma_url.txt", 0
    url_curl_pre:   db "curl -L -s --max-time 15 --fail -o /tmp/.uhma_url.txt -- '", 0
    url_curl_post:  db "'", 0
    url_wget_pre:   db "wget -q -O /tmp/.uhma_url.txt --timeout=15 -- '", 0
    url_wget_post:  db "'", 0

    ; Number format
    hex_prefix:     db "0x", 0
    pct_sign:       db "%", 0
    quote_char:     db "'", 0

    ; Feed control commands
    feed_start_cmd: db "./tools/feeder --corpus corpus/ --cycles 1 --pause 5 &", 0
    feed_stop_cmd:  db "pkill -TERM -f feeder", 0
    feed_check_cmd: db "pgrep -f feeder >/dev/null 2>&1", 0

    ; Sine table (64 entries for quarter wave, values 0-127 representing 0.0-1.0)
    ; sin(i * 90 / 64) * 127
    align 4
    sine_table:
    db 0, 3, 6, 9, 12, 16, 19, 22, 25, 28, 31, 34, 37, 40, 43, 46
    db 49, 51, 54, 57, 60, 63, 65, 68, 71, 73, 76, 78, 81, 83, 85, 88
    db 90, 92, 94, 96, 98, 100, 102, 104, 106, 107, 109, 111, 112, 113, 115, 116
    db 117, 118, 120, 121, 122, 122, 123, 124, 125, 125, 126, 126, 127, 127, 127, 127

    ; Node angles (0-255 = 0-360 degrees) for ring positions
    ; UHMA at center (no angle), others evenly distributed (256/11 ≈ 23 apart)
    ; Format: angle for each NODE_* (0=right, 64=top, 128=left, 192=bottom)
    node_angles:
    db 0        ; NODE_UHMA - center, not on ring
    db 70       ; NODE_BRAIN - top
    db 93       ; NODE_REGIONS - upper-left
    db 47       ; NODE_TOKENS - upper-right
    db 140     ; NODE_STATE - left
    db 0        ; NODE_PREDICT - right
    db 186      ; NODE_DISPATCH - lower-left
    db 233      ; NODE_ACCURACY - lower-right
    db 163      ; NODE_HIVE - left-bottom
    db 210      ; NODE_ROSETTA - bottom-right
    db 116      ; NODE_MYCO - left-top
    db 23       ; NODE_SPORE - right-top

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
    ACT_URL         equ 10
    ACT_INTRO       equ 11
    ACT_TRACE       equ 12
    ACT_GEOM        equ 13
    ACT_SEND        equ 14
    ACT_CLEAR       equ 15
    ACT_QUIT        equ 16
    ACT_FEED        equ 17
    ACT_STOP        equ 18

    ; Node type constants for click handling
    NODE_UHMA       equ 0
    NODE_BRAIN      equ 1
    NODE_REGIONS    equ 2
    NODE_TOKENS     equ 3
    NODE_STATE      equ 4
    NODE_PREDICT    equ 5
    NODE_DISPATCH   equ 6
    NODE_ACCURACY   equ 7
    NODE_HIVE       equ 8
    NODE_ROSETTA    equ 9
    NODE_MYCO       equ 10
    NODE_SPORE      equ 11
    NODE_COUNT      equ 12

    ; Animation constants
    ANIM_NONE       equ 0
    ANIM_EXPANDING  equ 1
    ANIM_COLLAPSING equ 2
    ANIM_DURATION   equ 20          ; frames (~333ms at 60fps)
    ANIM_STEP       equ 13          ; 255 / 20 ≈ 13

    ; Ring layout constants (semi-3D orbital view)
    RING_CENTER_X   equ CANVAS_X + CANVAS_W/3         ; left third
    RING_CENTER_Y   equ CANVAS_Y + CANVAS_H/2         ; center (UHMA position was correct)
    RING_RADIUS_X   equ 350         ; even wider spread to prevent overlap
    RING_RADIUS_Y   equ 120         ; taller ellipse for more vertical spread
    RING_DEPTH_SCALE equ 100        ; strong 3D depth (far=small, near=big)
    UHMA_Y_OFFSET   equ 0           ; UHMA at center
    ; Node sizes for ring nodes (smaller to prevent overlap)
    RNODE_W         equ 80          ; ring node width
    RNODE_H         equ 40          ; ring node height

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
    ; Output panel layout (right side, larger panels)
    OUT_PANEL_W     equ 280               ; wider panels
    OUT_PANEL_X     equ WIN_W - OUT_PANEL_W - 10  ; 10px from right edge
    OUT_PANEL_H     equ 180               ; taller panels
    OUT_PANEL_GAP   equ 5                 ; gap between panels
    OUT_FEED_Y      equ MENU_H + 10       ; FEED panel y
    OUT_QUERY_Y     equ OUT_FEED_Y + OUT_PANEL_H + OUT_PANEL_GAP
    OUT_DEBUG_Y     equ OUT_QUERY_Y + OUT_PANEL_H + OUT_PANEL_GAP

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
    key_mods:       resd 1

    ; Feedback
    feedback_msg:   resq 1
    feedback_timer: resd 1

    ; Buttons: x, y, w, h for each (max 20)
    buttons:        resd 80
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

    ; Ring buffers for live streaming panels
    ; Each buffer: data + write_pos + view_pos + paused flag

    ; FEED output (SUBNET_CONSOL) - eat/dream/observe responses
    feed_ring:      resb 8192
    feed_write:     resd 1          ; write position (0-8191)
    feed_view:      resd 1          ; view position (follows write unless paused)
    feed_paused:    resd 1          ; 1 = paused (user examining), 0 = live

    ; QUERY output (SUBNET_REPL) - status/why/misses responses
    query_ring:     resb 8192
    query_write:    resd 1
    query_view:     resd 1
    query_paused:   resd 1

    ; DEBUG output (SUBNET_SELF) - trace/receipts
    debug_ring:     resb 8192
    debug_write:    resd 1
    debug_view:     resd 1
    debug_paused:   resd 1

    ; Polling state for QUERY/DEBUG panels
    poll_counter:   resd 1          ; frames since last poll
    POLL_INTERVAL   equ 180         ; poll every 180 frames (~3 seconds at 60fps)

    RING_SIZE       equ 8192
    RING_MASK       equ 8191        ; for wrapping (size - 1)

    ; Feed control state
    feed_running:   resd 1          ; 1 if feeder is running
    feed_check_ctr: resd 1          ; counter for polling (every 60 frames)
    feed_cmd_buf:   resb 512        ; dynamically built feeder command
    feed_cfg_buf:   resb 256        ; parsed config from zenity

    ; Dropdown menu state
    menu_open:      resd 1          ; 1 if dropdown is visible
    menu_x:         resd 1          ; menu position x
    menu_y:         resd 1          ; menu position y
    menu_hover:     resd 1          ; hovered item (-1 = none)
    MENU_W          equ 80          ; menu width
    MENU_ITEM_H     equ 24          ; menu item height
    MENU_ITEMS      equ 4           ; number of items (Quick/Mastery/Live/Stop)

    ; Startup countdown state
    startup_mode:   resd 1          ; 0=normal, 1=countdown, 2=waiting for server
    startup_count:  resd 1          ; countdown seconds remaining
    startup_frames: resd 1          ; frame counter for 1-second ticks

    ; Animation state (TheBrain-style smooth transitions)
    ; States: 0=NONE, 1=EXPANDING, 2=COLLAPSING
    anim_state:     resd 1
    anim_progress:  resd 1          ; 0-255 (0=start, 255=end)
    anim_node:      resd 1          ; which node is animating
    anim_src_x:     resd 1          ; source rect x
    anim_src_y:     resd 1          ; source rect y
    anim_src_w:     resd 1          ; source rect width
    anim_src_h:     resd 1          ; source rect height
    anim_dst_x:     resd 1          ; destination rect x
    anim_dst_y:     resd 1          ; destination rect y
    anim_dst_w:     resd 1          ; destination rect width
    anim_dst_h:     resd 1          ; destination rect height
    anim_cur_x:     resd 1          ; current interpolated x
    anim_cur_y:     resd 1          ; current interpolated y
    anim_cur_w:     resd 1          ; current interpolated width
    anim_cur_h:     resd 1          ; current interpolated height

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
    ; Node rects array: 16 nodes * 4 dwords (x,y,w,h) = 64 dwords
    ; Index: 0=UHMA, 1=BRAIN, 2=REGIONS, 3=TOKENS, 4=STATE, 5=PREDICT
    ;        6=DISPATCH, 7=ACCURACY, 8=HIVE, 9=ROSETTA, 10=MYCO
    node_rects:     resd 64
    node_rect_cnt:  resd 1

section .text

extern printf
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
extern gfx_get_last_keystate
extern gfx_get_mouse_pos
extern usleep
extern system
extern fopen
extern fgets
extern fread
extern fclose
extern fwrite
extern fflush
extern opendir
extern readdir
extern closedir
extern strcpy
extern strcat
extern strlen
extern strcmp
; MCP client functions (gateway framed I/O)
extern mcp_save
extern mcp_load
extern mcp_send_async
extern mcp_read_stream
extern mcp_last_subnet
extern mcp_spawn_uhma
extern mcp_spawn_uhma_feed
extern mcp_try_connect
extern mcp_running

; Rosetta Stone / Geometric Verification
; Verify mode handled via gateway commands

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

    ; URL (9)
    mov dword [rdi], 570
    mov dword [rdi+4], 8
    mov dword [rdi+8], 40
    mov dword [rdi+12], 30
    add rdi, 16

    ; INTRO (10) - show self-awareness reading
    mov dword [rdi], 620
    mov dword [rdi+4], 8
    mov dword [rdi+8], 50
    mov dword [rdi+12], 30
    add rdi, 16

    ; TRACE (11)
    mov dword [rdi], 680
    mov dword [rdi+4], 8
    mov dword [rdi+8], 55
    mov dword [rdi+12], 30
    add rdi, 16

    ; GEOM (12) - toggle verification mode
    mov dword [rdi], 745
    mov dword [rdi+4], 8
    mov dword [rdi+8], 50
    mov dword [rdi+12], 30
    add rdi, 16

    ; SEND (13) - near input
    mov dword [rdi], 1100
    mov dword [rdi+4], WIN_H - INPUT_H - STATUS_H + 5
    mov dword [rdi+8], 55
    mov dword [rdi+12], 28
    add rdi, 16

    ; CLEAR (14)
    mov dword [rdi], 1165
    mov dword [rdi+4], WIN_H - INPUT_H - STATUS_H + 5
    mov dword [rdi+8], 55
    mov dword [rdi+12], 28
    add rdi, 16

    ; QUIT (15)
    mov dword [rdi], 1210
    mov dword [rdi+4], 8
    mov dword [rdi+8], 50
    mov dword [rdi+12], 30
    add rdi, 16

    ; FEED (16) - start training
    mov dword [rdi], 805
    mov dword [rdi+4], 8
    mov dword [rdi+8], 50
    mov dword [rdi+12], 30
    add rdi, 16

    ; STOP (17) - stop training
    mov dword [rdi], 865
    mov dword [rdi+4], 8
    mov dword [rdi+8], 50
    mov dword [rdi+12], 30

    mov dword [rel num_buttons], 18

    ; Initialize state
    mov dword [rel vis_running], 1
    mov qword [rel vis_frame], 0
    mov dword [rel input_len], 0
    mov dword [rel hover_btn], -1
    mov qword [rel feedback_msg], 0
    mov dword [rel feedback_timer], 0
    mov dword [rel selected_rgn], -1
    mov dword [rel trace_active], 0
    mov dword [rel feed_running], 0
    mov dword [rel feed_check_ctr], 0
    mov dword [rel anim_state], ANIM_NONE
    mov dword [rel anim_progress], 0
    mov dword [rel anim_node], -1
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

    ; Try to connect to existing UHMA (sets mcp_running=1 if successful)
    call mcp_try_connect
    ; If connected, populate panels instantly
    cmp dword [rel mcp_running], 0
    je .no_initial_poll
    call enable_feed_stream
    call poll_channels
.no_initial_poll:

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
    ; ButtonRelease (event type 5)
    cmp r12d, 5
    jne .not_release
    call handle_release
    jmp .poll_loop

.not_release:
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

    ; Poll QUERY/DEBUG panels periodically (only if UHMA running)
    cmp dword [rel mcp_running], 0
    je .skip_poll
    ; Don't auto-poll while a node is expanded (preserve node-specific data in query_ring)
    cmp dword [rel focus_node], -1
    jne .skip_poll
    inc dword [rel poll_counter]
    mov eax, [rel poll_counter]
    cmp eax, POLL_INTERVAL
    jl .skip_poll
    mov dword [rel poll_counter], 0
    call poll_channels
.skip_poll:

    ; Read any available data from UHMA output streams
    call read_streams

    ; Sync view to write for unpaused panels (auto-scroll)
    cmp dword [rel feed_paused], 0
    jne .feed_no_sync
    mov eax, [rel feed_write]
    mov [rel feed_view], eax
.feed_no_sync:
    cmp dword [rel query_paused], 0
    jne .query_no_sync
    mov eax, [rel query_write]
    mov [rel query_view], eax
.query_no_sync:
    cmp dword [rel debug_paused], 0
    jne .debug_no_sync
    mov eax, [rel debug_write]
    mov [rel debug_view], eax
.debug_no_sync:

    ; Update animation
    call update_animation

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

    ; Capture modifier state (ShiftMask) for key translation
    call gfx_get_last_keystate
    mov [rel key_mods], eax

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
    %define SHIFT_MASK 1
    cmp edi, 65
    jne .not_space
    mov al, ' '
    ret
.not_space:
    ; Common punctuation for paths/URLs
    cmp edi, 20                 ; '-'
    jne .not_dash
    mov eax, [rel key_mods]
    test eax, SHIFT_MASK
    jz .dash
    mov al, '_'
    ret
.dash:
    mov al, '-'
    ret
.not_dash:
    cmp edi, 60                 ; '.'
    jne .not_dot
    mov al, '.'
    ret
.not_dot:
    cmp edi, 61                 ; '/'
    jne .not_slash
    mov al, '/'
    ret
.not_slash:
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

    ; Check if dropdown menu is open
    mov eax, [rel menu_open]
    test eax, eax
    jz .no_menu_check

    ; Check if click is inside menu
    mov eax, [rel menu_x]
    cmp r12d, eax
    jl .click_outside_menu
    add eax, MENU_W
    cmp r12d, eax
    jge .click_outside_menu

    mov eax, [rel menu_y]
    cmp r13d, eax
    jl .click_outside_menu
    mov ebx, eax                    ; save menu_y
    add eax, MENU_ITEM_H * MENU_ITEMS
    cmp r13d, eax
    jge .click_outside_menu

    ; Click inside menu - determine which item
    mov eax, r13d
    sub eax, ebx                    ; y - menu_y
    xor edx, edx
    mov ecx, MENU_ITEM_H
    div ecx                         ; eax = item index
    cmp eax, MENU_ITEMS
    jge .click_outside_menu

    ; Execute menu item
    mov dword [rel menu_open], 0    ; close menu
    cmp eax, 0
    je .menu_quick
    cmp eax, 1
    je .menu_mastery
    cmp eax, 2
    je .menu_live
    cmp eax, 3
    je .menu_stop
    jmp .done_click

.menu_quick:
    ; Spawn UHMA with batch ON if not running
    cmp dword [rel mcp_running], 0
    jne .quick_start
    call mcp_spawn_uhma_feed        ; batch ON for feeding
    test eax, eax
    jz .menu_spawn_fail
.quick_start:
    lea rdi, [rel feed_cmd_quick]
    call system
    mov dword [rel feed_running], 1
    lea rax, [rel msg_feed_start]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 90
    jmp .done_click

.menu_mastery:
    ; Spawn UHMA with batch ON if not running
    cmp dword [rel mcp_running], 0
    jne .mastery_start
    call mcp_spawn_uhma_feed
    test eax, eax
    jz .menu_spawn_fail
.mastery_start:
    lea rdi, [rel feed_cmd_mastery]
    call system
    mov dword [rel feed_running], 1
    lea rax, [rel msg_feed_start]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 90
    jmp .done_click

.menu_live:
    ; Spawn UHMA with batch ON if not running
    cmp dword [rel mcp_running], 0
    jne .live_start
    call mcp_spawn_uhma_feed
    test eax, eax
    jz .menu_spawn_fail
.live_start:
    lea rdi, [rel feed_cmd_live]
    call system
    mov dword [rel feed_running], 1
    lea rax, [rel msg_feed_start]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 90
    jmp .done_click

.menu_spawn_fail:
    lea rax, [rel msg_spawn_fail]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 90
    jmp .done_click

.menu_stop:
    lea rdi, [rel feed_cmd_stop]
    call system
    mov dword [rel feed_running], 0
    lea rax, [rel msg_feed_stop]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 90
    jmp .done_click

.click_outside_menu:
    ; Close menu on click outside
    mov dword [rel menu_open], 0
    jmp .done_click

.no_menu_check:
    ; Check output panels first (click = pause)
    ; Check x bounds (same for all 3 panels)
    cmp r12d, OUT_PANEL_X
    jl .not_output_panel
    cmp r12d, OUT_PANEL_X + OUT_PANEL_W
    jge .not_output_panel

    ; FEED panel - toggle pause + copy to clipboard
    cmp r13d, OUT_FEED_Y
    jl .not_output_panel
    cmp r13d, OUT_FEED_Y + OUT_PANEL_H
    jge .check_query_panel
    cmp dword [rel feed_paused], 0
    jne .feed_unpause
    ; Pausing - copy to clipboard
    mov dword [rel feed_paused], 1
    lea rdi, [rel feed_ring]
    call copy_ring_to_clipboard
    jmp .done_click
.feed_unpause:
    ; Unpausing - snap view to latest
    mov dword [rel feed_paused], 0
    mov eax, [rel feed_write]
    mov [rel feed_view], eax
    jmp .done_click

.check_query_panel:
    cmp r13d, OUT_QUERY_Y
    jl .check_debug_panel
    cmp r13d, OUT_QUERY_Y + OUT_PANEL_H
    jge .check_debug_panel
    cmp dword [rel query_paused], 0
    jne .query_unpause
    mov dword [rel query_paused], 1
    lea rdi, [rel query_ring]
    call copy_ring_to_clipboard
    jmp .done_click
.query_unpause:
    mov dword [rel query_paused], 0
    mov eax, [rel query_write]
    mov [rel query_view], eax
    jmp .done_click

.check_debug_panel:
    cmp r13d, OUT_DEBUG_Y
    jl .not_output_panel
    cmp r13d, OUT_DEBUG_Y + OUT_PANEL_H
    jge .not_output_panel
    cmp dword [rel debug_paused], 0
    jne .debug_unpause
    mov dword [rel debug_paused], 1
    lea rdi, [rel debug_ring]
    call copy_ring_to_clipboard
    jmp .done_click
.debug_unpause:
    mov dword [rel debug_paused], 0
    mov eax, [rel debug_write]
    mov [rel debug_view], eax
    jmp .done_click

.not_output_panel:
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

    ; Check if we're in focused/expanded view
    mov eax, [rel focus_node]
    cmp eax, -1
    je .check_nodes
    ; We're in expanded view - click outside should go back
    ; Panel is at canvas center +/- 200x150
    ; Canvas center: x = CANVAS_X + CANVAS_W/2, y = CANVAS_Y + CANVAS_H/2
    ; Panel: x=[center-200, center+200], y=[center-150, center+150]
    mov eax, CANVAS_X + CANVAS_W/2 - 200    ; panel left
    cmp r12d, eax
    jl .unfocus
    mov eax, CANVAS_X + CANVAS_W/2 + 200    ; panel right
    cmp r12d, eax
    jg .unfocus
    mov eax, CANVAS_Y + CANVAS_H/2 - 150    ; panel top
    cmp r13d, eax
    jl .unfocus
    mov eax, CANVAS_Y + CANVAS_H/2 + 150    ; panel bottom
    cmp r13d, eax
    jg .unfocus
    jmp .done_click                 ; clicked inside panel, do nothing

.unfocus:
    ; Start collapse animation instead of instant unfocus
    call start_collapse_anim
    jmp .done_click

.check_nodes:
    ; === MINDMAP VIEW: Check for node clicks ===
    ; Loop through all nodes in node_rects
    lea rbx, [rel node_rects]
    xor ecx, ecx                    ; node index
.node_loop:
    cmp ecx, NODE_COUNT
    jge .done_click                 ; no node clicked

    ; Check if click is inside this node's rect
    mov edi, [rbx]                  ; node x
    test edi, edi                   ; skip if x=0 (not initialized)
    jz .next_node
    cmp r12d, edi
    jl .next_node
    add edi, [rbx + 8]              ; + width
    cmp r12d, edi
    jge .next_node
    mov edi, [rbx + 4]              ; node y
    cmp r13d, edi
    jl .next_node
    add edi, [rbx + 12]             ; + height
    cmp r13d, edi
    jge .next_node

    ; Node clicked! ecx = node index
    cmp ecx, NODE_BRAIN
    je .click_brain
    ; For other nodes, start expand animation and send relevant command
    push rcx
    mov edi, ecx
    call start_expand_anim
    pop rcx
    ; Send MCP command based on node type (each node gets unique data)
    cmp ecx, NODE_REGIONS
    je .send_regions
    cmp ecx, NODE_TOKENS
    je .send_receipts
    cmp ecx, NODE_STATE
    je .send_intro
    cmp ecx, NODE_PREDICT
    je .send_metacog
    cmp ecx, NODE_DISPATCH
    je .send_drives
    cmp ecx, NODE_ACCURACY
    je .send_presence
    cmp ecx, NODE_HIVE
    je .send_hive
    cmp ecx, NODE_ROSETTA
    je .send_geom
    cmp ecx, NODE_SPORE
    je .send_genes
    cmp ecx, NODE_MYCO
    je .send_colony
    jmp .done_click
.send_regions:
    mov edi, 1                      ; channel 1 → SUBNET_REPL → query_ring
    lea rsi, [rel cmd_regions]
    call mcp_send_async
    jmp .done_click
.send_receipts:
    mov edi, 1
    lea rsi, [rel cmd_receipts]
    call mcp_send_async
    jmp .done_click
.send_intro:
    mov edi, 1
    lea rsi, [rel cmd_intro]
    call mcp_send_async
    jmp .done_click
.send_metacog:
    mov edi, 1
    lea rsi, [rel cmd_metacog]
    call mcp_send_async
    jmp .done_click
.send_drives:
    mov edi, 1
    lea rsi, [rel cmd_drives]
    call mcp_send_async
    jmp .done_click
.send_presence:
    mov edi, 1
    lea rsi, [rel cmd_presence]
    call mcp_send_async
    jmp .done_click
.send_hive:
    mov edi, 1
    lea rsi, [rel cmd_hive]
    call mcp_send_async
    jmp .done_click
.send_self:
    mov edi, 1
    lea rsi, [rel cmd_self]
    call mcp_send_async
    jmp .done_click
.send_status:
    mov edi, 1
    lea rsi, [rel cmd_status]
    call mcp_send_async
    jmp .done_click
.send_geom:
    mov edi, 1
    lea rsi, [rel cmd_geom]
    call mcp_send_async
    jmp .done_click
.send_colony:
    mov edi, 1
    lea rsi, [rel cmd_colony]
    call mcp_send_async
    jmp .done_click
.send_genes:
    mov edi, 1
    lea rsi, [rel cmd_genes]
    call mcp_send_async
    jmp .done_click

.click_brain:
    ; BRAIN node - switch to brain/memory view
    mov dword [rel view_mode], 1
    ; Clear query_ring and send regions command (async → query_ring)
    mov dword [rel query_write], 0
    mov edi, 1
    lea rsi, [rel cmd_regions]
    call mcp_send_async
    jmp .done_click

.next_node:
    add rbx, 16                     ; each node rect is 4 dwords = 16 bytes
    inc ecx
    jmp .node_loop

.in_brain_view:
    ; === BRAIN VIEW: Click anywhere to go back to mindmap ===
    mov dword [rel view_mode], 0

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
;; handle_release — Mouse button release
;; No-op - toggle is on click
;; ============================================================
handle_release:
    mov eax, 1
    ret

;; snap_view_to_write — Sync view position to write position for all panels
;; Called when unpausing to catch up to latest content
snap_view_to_write:
    mov eax, [rel feed_write]
    mov [rel feed_view], eax
    mov eax, [rel query_write]
    mov [rel query_view], eax
    mov eax, [rel debug_write]
    mov [rel debug_view], eax
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

    ; Check if menu is open - update menu hover
    mov eax, [rel menu_open]
    test eax, eax
    jz .check_buttons

    ; Check if mouse is inside menu
    mov dword [rel menu_hover], -1
    mov eax, [rel menu_x]
    cmp r12d, eax
    jl .check_buttons
    add eax, MENU_W
    cmp r12d, eax
    jge .check_buttons

    mov eax, [rel menu_y]
    cmp r13d, eax
    jl .check_buttons
    mov ebx, eax                    ; save menu_y
    add eax, MENU_ITEM_H * MENU_ITEMS
    cmp r13d, eax
    jge .check_buttons

    ; Mouse inside menu - calculate hovered item
    mov eax, r13d
    sub eax, ebx                    ; y - menu_y
    xor edx, edx
    mov ecx, MENU_ITEM_H
    div ecx                         ; eax = item index
    cmp eax, MENU_ITEMS
    jge .check_buttons
    mov [rel menu_hover], eax
    jmp .done

.check_buttons:
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
    cmp ebx, ACT_CLEAR
    je .do_clear
    cmp ebx, ACT_GEOM
    je .do_geom

    ; Ensure UHMA connected before any command-sending action
    cmp dword [rel mcp_running], 0
    jne .connected
    ; Try passive connect (UHMA may be running externally)
    ; Save ebx in local stack space (stack already 16-aligned here)
    mov [rsp], ebx
    call mcp_try_connect
    mov ebx, [rsp]
    test eax, eax
    jnz .connected
    ; Not running — spawn in autonomous mode
    mov [rsp], ebx
    call mcp_spawn_uhma
    mov ebx, [rsp]
    test eax, eax
    jz .spawn_failed
    ; Just connected — populate panels immediately
    mov [rsp], ebx
    call poll_channels
    mov ebx, [rsp]
    jmp .connected
.spawn_failed:
    ; Spawn failed — show error and bail
    lea rax, [rel msg_spawn_fail]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 90
    jmp .continue
.connected:
    ; Ensure FEED panel is bound to live run-log stream
    call enable_feed_stream

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
    cmp ebx, ACT_URL
    je .do_url
    cmp ebx, ACT_INTRO
    je .do_intro
    cmp ebx, ACT_TRACE
    je .do_trace
    cmp ebx, ACT_GEOM
    je .do_geom
    cmp ebx, ACT_SEND
    je .do_send
    cmp ebx, ACT_CLEAR
    je .do_clear
    cmp ebx, ACT_FEED
    je .do_feed
    cmp ebx, ACT_STOP
    je .do_stop
    jmp .continue

.do_dream:
    ; Debug output
    lea rdi, [rel dbg_dream]
    xor eax, eax
    call printf

    ; Check if UHMA running, if not spawn in live mode (batch OFF)
    cmp dword [rel mcp_running], 0
    jne .dream_send

    ; Debug: spawning
    lea rdi, [rel dbg_spawning]
    xor eax, eax
    call printf

    call mcp_spawn_uhma             ; spawns with batch OFF (live mode)
    test eax, eax
    jz .dream_fail
.dream_send:
    mov edi, 0                  ; FEED channel
    lea rsi, [rel cmd_dream]
    call mcp_send_async
    lea rax, [rel msg_dream]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 60
    jmp .continue
.dream_fail:
    lea rax, [rel msg_spawn_fail]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 90
    jmp .continue

.do_observe:
    mov edi, 0                  ; FEED channel
    lea rsi, [rel cmd_observe]
    call mcp_send_async
    lea rax, [rel msg_observe]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 60
    jmp .continue

.do_evolve:
    mov edi, 0                  ; FEED channel
    lea rsi, [rel cmd_evolve]
    call mcp_send_async
    lea rax, [rel msg_evolve]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 60
    jmp .continue

.do_step:
    ; Run a single autonomous tick
    mov edi, 1                  ; QUERY channel
    lea rsi, [rel cmd_step]
    call mcp_send_async
    lea rax, [rel msg_step]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 60
    jmp .continue

.do_run:
    ; Run 100 autonomous ticks
    mov edi, 1                  ; QUERY channel
    lea rsi, [rel cmd_run_100]
    call mcp_send_async
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

.do_url:
    ; URL fetch/digest - use zenity or input field, then eat fetched file
    call load_url
    jmp .continue

.do_intro:
    ; Show introspective state via MCP
    mov edi, 1                  ; QUERY channel
    lea rsi, [rel cmd_intro]
    call mcp_send_async
    lea rax, [rel msg_intro_ok]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 90
    jmp .continue

.do_trace:
    ; Toggle trace via MCP
    mov eax, [rel trace_active]
    test eax, eax
    jnz .trace_off
    mov dword [rel trace_active], 1
    mov edi, 2                  ; DEBUG channel
    lea rsi, [rel cmd_trace]
    call mcp_send_async
    lea rax, [rel msg_trace_on]
    jmp .trace_msg
.trace_off:
    mov dword [rel trace_active], 0
    mov edi, 2                  ; DEBUG channel
    lea rsi, [rel cmd_trace]
    call mcp_send_async
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
    mov edi, 1                  ; QUERY channel
    cmp eax, 0
    jne .geom_not_abs
    lea rsi, [rel cmd_geom_0]
    call mcp_send_async
    lea rax, [rel msg_geom_abs]
    jmp .geom_msg
.geom_not_abs:
    cmp eax, 1
    jne .geom_not_vec
    lea rsi, [rel cmd_geom_1]
    call mcp_send_async
    lea rax, [rel msg_geom_vec]
    jmp .geom_msg
.geom_not_vec:
    lea rsi, [rel cmd_geom_2]
    call mcp_send_async
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

.do_feed:
    ; Toggle dropdown menu
    mov eax, [rel menu_open]
    test eax, eax
    jnz .close_menu
    ; Open menu below FEED button (x=805, y=38)
    mov dword [rel menu_open], 1
    mov dword [rel menu_x], 805
    mov dword [rel menu_y], 38
    mov dword [rel menu_hover], -1
    jmp .continue
.close_menu:
    mov dword [rel menu_open], 0
    jmp .continue

.do_stop:
    ; Stop training
    lea rdi, [rel feed_stop_cmd]
    call system
    mov dword [rel feed_running], 0
    lea rax, [rel msg_feed_stop]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 90
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

    ; Send ccmode + input via gateway (FEED channel)
    xor edi, edi                ; FEED channel
    lea rsi, [rel cmd_ccmode]
    call mcp_send_async
    xor edi, edi
    lea rsi, [rel input_buf]
    call mcp_send_async

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

    ; Output panels (3 live streams: FEED, QUERY, DEBUG)
    call draw_output

    ; Status bar
    call draw_status

    ; Feedback
    call draw_feedback

    ; Dropdown menu (on top of everything)
    call draw_menu

    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; draw_menu — Draw dropdown menu if open
;; ============================================================
draw_menu:
    push rbx
    push r12
    push r13
    sub rsp, 8

    ; Check if menu is open
    mov eax, [rel menu_open]
    test eax, eax
    jz .menu_done

    mov r12d, [rel menu_x]          ; menu x
    mov r13d, [rel menu_y]          ; menu y

    ; Draw menu background
    mov edi, r12d
    mov esi, r13d
    mov edx, MENU_W
    mov ecx, MENU_ITEM_H * MENU_ITEMS
    mov r8d, 0x002a2a3e             ; dark background
    call gfx_fill_rect

    ; Draw menu border
    mov edi, r12d
    mov esi, r13d
    mov edx, MENU_W
    mov ecx, MENU_ITEM_H * MENU_ITEMS
    mov r8d, 0x00505070             ; border color
    call gfx_rect

    ; Draw menu items
    xor ebx, ebx                    ; item index

.menu_item_loop:
    cmp ebx, MENU_ITEMS
    jge .menu_done

    ; Calculate item y position
    mov eax, ebx
    imul eax, MENU_ITEM_H
    add eax, r13d                   ; item_y = menu_y + index * MENU_ITEM_H

    ; Check if hovered
    cmp ebx, [rel menu_hover]
    jne .not_hovered
    ; Draw hover highlight
    push rax
    mov edi, r12d
    mov esi, eax
    mov edx, MENU_W
    mov ecx, MENU_ITEM_H
    mov r8d, 0x00404060             ; hover color
    call gfx_fill_rect
    pop rax
.not_hovered:

    ; Draw separator before Stop (item 3)
    cmp ebx, 3
    jne .no_separator
    push rax
    mov edi, r12d
    add edi, 5
    mov esi, eax
    mov edx, r12d
    add edx, MENU_W - 5
    mov ecx, eax
    mov r8d, 0x00505070
    call gfx_line
    pop rax
.no_separator:

    ; Draw item text
    push rax
    add eax, 6                      ; text y offset
    mov esi, eax
    mov edi, r12d
    add edi, 10                     ; text x offset
    mov r8d, 0x00e0e0e0             ; text color

    ; Get label for this item
    cmp ebx, 0
    jne .not_quick
    lea rdx, [rel menu_quick]
    mov ecx, 5
    jmp .draw_item_text
.not_quick:
    cmp ebx, 1
    jne .not_mastery
    lea rdx, [rel menu_mastery]
    mov ecx, 7
    jmp .draw_item_text
.not_mastery:
    cmp ebx, 2
    jne .not_live
    lea rdx, [rel menu_live]
    mov ecx, 4
    jmp .draw_item_text
.not_live:
    lea rdx, [rel menu_stop]
    mov ecx, 4

.draw_item_text:
    call gfx_text
    pop rax

    inc ebx
    jmp .menu_item_loop

.menu_done:
    add rsp, 8
    pop r13
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
    sub rsp, 16

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

    ; Label position - will be adjusted for centering in .draw_lbl
    mov [rsp], r13d         ; save btn_x
    mov [rsp+4], r15d       ; save btn_w
    add r14d, 20            ; y position
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
    lea rdx, [rel btn_url]
    mov ecx, 3
    cmp eax, 9
    je .draw_lbl
    lea rdx, [rel btn_intro]
    mov ecx, 5
    cmp eax, 10
    je .draw_lbl
    lea rdx, [rel btn_trace]
    mov ecx, 5
    cmp eax, 11
    je .draw_lbl
    lea rdx, [rel btn_geom]
    mov ecx, 4
    cmp eax, 12
    je .draw_lbl
    lea rdx, [rel btn_send]
    mov ecx, 4
    cmp eax, 13
    je .draw_lbl
    lea rdx, [rel btn_clear]
    mov ecx, 5
    cmp eax, 14
    je .draw_lbl
    lea rdx, [rel btn_quit]
    mov ecx, 4
    cmp eax, 15
    je .draw_lbl
    lea rdx, [rel btn_feed]
    mov ecx, 4
    cmp eax, 16
    je .draw_lbl
    lea rdx, [rel btn_stop]
    mov ecx, 4

.draw_lbl:
    ; Calculate centered x: btn_x + (btn_w - text_len*8) / 2
    mov eax, ecx            ; text length
    shl eax, 3              ; * 8 (pixels per char)
    mov edi, [rsp+4]        ; btn_w
    sub edi, eax            ; btn_w - text_pixels
    shr edi, 1              ; / 2
    add edi, [rsp]          ; + btn_x
    mov esi, r14d           ; y
    call gfx_text

    add rbx, 16
    inc r12d
    jmp .btn_loop

.done:
    add rsp, 16
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
    sub rsp, 16

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

    add rsp, 16
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

    ; Calculate center (use ring center for carousel alignment)
    mov r12d, RING_CENTER_X             ; center x (same as ring)
    mov r13d, RING_CENTER_Y             ; ring orbit center y

    ; Always draw nodes first, then overlay animation or focused panel

    ; === ROOT VIEW: Show main system nodes ===

    ; Central "UHMA" node (drawn in front - below ring center for 3D depth)
    lea edi, [r12d - 60]
    lea esi, [r13d + UHMA_Y_OFFSET - 25]   ; offset down = in front
    mov edx, 120
    mov ecx, 50
    mov r8d, [rel col_dispatch]
    call gfx_fill_rect

    lea edi, [r12d - 60]
    lea esi, [r13d + UHMA_Y_OFFSET - 25]
    mov edx, 120
    mov ecx, 50
    mov r8d, [rel col_white]
    call gfx_rect

    lea edi, [r12d - 13]
    lea esi, [r13d + UHMA_Y_OFFSET + 5]
    lea rdx, [rel node_uhma]
    mov ecx, 4
    mov r8d, [rel col_white]
    call gfx_text

    ; Store UHMA node rect for clicks (node_rects + NODE_UHMA*16)
    lea rax, [rel node_rects]
    lea edi, [r12d - 60]
    mov [rax + NODE_UHMA*16], edi
    lea edi, [r13d + UHMA_Y_OFFSET - 25]
    mov [rax + NODE_UHMA*16 + 4], edi
    mov dword [rax + NODE_UHMA*16 + 8], 120
    mov dword [rax + NODE_UHMA*16 + 12], 50

    ; === BRAIN node - using ring layout ===
    mov edi, 64                 ; angle 64 = top of ring
    call calc_ring_pos          ; eax=x (center), edx=y (center)
    mov r14d, eax               ; node center x
    mov r15d, edx               ; node center y

    ; Connection line: from BRAIN center to UHMA edge
    mov edi, r14d               ; outer x
    mov esi, r15d               ; outer y
    call calc_uhma_edge         ; eax=edge_x, edx=edge_y
    mov [rsp], eax              ; save edge_x
    mov [rsp+4], edx            ; save edge_y
    mov edi, r14d               ; BRAIN center x
    mov esi, r15d               ; BRAIN center y
    mov edx, [rsp]              ; UHMA edge x
    mov ecx, [rsp+4]            ; UHMA edge y
    mov r8d, [rel col_magenta]
    call gfx_line

    ; Adjust to top-left for drawing
    sub r14d, RNODE_W/2
    sub r15d, RNODE_H/2

    ; Node box - magenta/purple for brain
    mov edi, r14d
    mov esi, r15d
    mov edx, RNODE_W
    mov ecx, RNODE_H
    mov r8d, 0x00663388         ; purple fill
    call gfx_fill_rect

    mov edi, r14d
    mov esi, r15d
    mov edx, RNODE_W
    mov ecx, RNODE_H
    mov r8d, [rel col_magenta]
    call gfx_rect

    ; Label centered: "BRAIN" = 5 chars
    lea edi, [r14d + 26]
    lea esi, [r15d + 25]
    lea rdx, [rel node_brain]
    mov ecx, 5
    mov r8d, [rel col_white]
    call gfx_text

    ; Store BRAIN rect for clicks
    lea rax, [rel node_rects]
    mov [rax + NODE_BRAIN*16], r14d
    mov [rax + NODE_BRAIN*16 + 4], r15d
    mov dword [rax + NODE_BRAIN*16 + 8], RNODE_W
    mov dword [rax + NODE_BRAIN*16 + 12], RNODE_H

    ; === REGIONS node - ring layout ===
    movzx edi, byte [rel node_angles + NODE_REGIONS]
    call calc_ring_pos
    mov r14d, eax               ; center x
    mov r15d, edx               ; center y

    ; Connection line: from REGIONS center to UHMA edge
    mov edi, r14d
    mov esi, r15d
    call calc_uhma_edge
    mov [rsp], eax
    mov [rsp+4], edx
    mov edi, r14d
    mov esi, r15d
    mov edx, [rsp]
    mov ecx, [rsp+4]
    mov r8d, [rel col_cyan]
    call gfx_line

    ; Adjust to top-left
    sub r14d, RNODE_W/2
    sub r15d, RNODE_H/2

    ; Node box
    mov edi, r14d
    mov esi, r15d
    mov edx, RNODE_W
    mov ecx, RNODE_H
    mov r8d, [rel col_nursery]
    call gfx_fill_rect

    mov edi, r14d
    mov esi, r15d
    mov edx, RNODE_W
    mov ecx, RNODE_H
    mov r8d, [rel col_white]
    call gfx_rect

    ; Store REGIONS rect
    lea rax, [rel node_rects]
    mov [rax + NODE_REGIONS*16], r14d
    mov [rax + NODE_REGIONS*16 + 4], r15d
    mov dword [rax + NODE_REGIONS*16 + 8], RNODE_W
    mov dword [rax + NODE_REGIONS*16 + 12], RNODE_H

    ; Region count inside node
    mov eax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    lea rdi, [rsp]
    call format_num
    mov [rsp + 32], eax

    ; Label centered: "REGIONS" = 7 chars
    lea edi, [r14d + 22]
    lea esi, [r15d + 19]
    lea rdx, [rel node_regions]
    mov ecx, 7
    mov r8d, [rel col_text]
    call gfx_text

    ; Count centered below
    lea edi, [r14d + 36]
    lea esi, [r15d + 30]
    lea rdx, [rsp]
    mov ecx, [rsp + 32]
    mov r8d, [rel col_white]
    call gfx_text

    ; === TOKENS node - ring layout ===
    movzx edi, byte [rel node_angles + NODE_TOKENS]
    call calc_ring_pos
    mov r14d, eax
    mov r15d, edx

    ; Connection line: to UHMA edge
    mov edi, r14d
    mov esi, r15d
    call calc_uhma_edge
    mov [rsp], eax
    mov [rsp+4], edx
    mov edi, r14d
    mov esi, r15d
    mov edx, [rsp]
    mov ecx, [rsp+4]
    mov r8d, [rel col_cyan]
    call gfx_line

    sub r14d, RNODE_W/2
    sub r15d, RNODE_H/2

    mov edi, r14d
    mov esi, r15d
    mov edx, RNODE_W
    mov ecx, RNODE_H
    mov r8d, 0x00448866
    call gfx_fill_rect

    mov edi, r14d
    mov esi, r15d
    mov edx, RNODE_W
    mov ecx, RNODE_H
    mov r8d, [rel col_white]
    call gfx_rect

    ; Store TOKENS rect
    lea rax, [rel node_rects]
    mov [rax + NODE_TOKENS*16], r14d
    mov [rax + NODE_TOKENS*16 + 4], r15d
    mov dword [rax + NODE_TOKENS*16 + 8], RNODE_W
    mov dword [rax + NODE_TOKENS*16 + 12], RNODE_H

    mov eax, [rbx + STATE_OFFSET + ST_TOKEN_COUNT]
    lea rdi, [rsp]
    call format_num
    mov [rsp + 32], eax

    ; Label centered: "TOKENS" = 6 chars
    lea edi, [r14d + 24]
    lea esi, [r15d + 18]
    lea rdx, [rel node_tokens]
    mov ecx, 6
    mov r8d, [rel col_text]
    call gfx_text

    ; Count centered
    lea edi, [r14d + 36]
    lea esi, [r15d + 30]
    lea rdx, [rsp]
    mov ecx, [rsp + 32]
    mov r8d, [rel col_white]
    call gfx_text

    ; === STATE node - ring layout ===
    movzx edi, byte [rel node_angles + NODE_STATE]
    call calc_ring_pos
    mov r14d, eax
    mov r15d, edx

    ; Connection line: to UHMA edge
    mov edi, r14d
    mov esi, r15d
    call calc_uhma_edge
    mov [rsp], eax
    mov [rsp+4], edx
    mov edi, r14d
    mov esi, r15d
    mov edx, [rsp]
    mov ecx, [rsp+4]
    mov r8d, [rel col_cyan]
    call gfx_line

    sub r14d, RNODE_W/2
    sub r15d, RNODE_H/2

    mov edi, r14d
    mov esi, r15d
    mov edx, RNODE_W
    mov ecx, RNODE_H
    mov r8d, 0x00664488
    call gfx_fill_rect

    mov edi, r14d
    mov esi, r15d
    mov edx, RNODE_W
    mov ecx, RNODE_H
    mov r8d, [rel col_white]
    call gfx_rect

    ; Store STATE rect
    lea rax, [rel node_rects]
    mov [rax + NODE_STATE*16], r14d
    mov [rax + NODE_STATE*16 + 4], r15d
    mov dword [rax + NODE_STATE*16 + 8], RNODE_W
    mov dword [rax + NODE_STATE*16 + 12], RNODE_H

    ; Label centered: "STATE" = 5 chars
    lea edi, [r14d + 28]
    lea esi, [r15d + 18]
    lea rdx, [rel node_state]
    mov ecx, 5
    mov r8d, [rel col_text]
    call gfx_text

    ; State name centered
    mov eax, [rbx + STATE_OFFSET + ST_INTRO_STATE]
    cmp eax, 7
    jl .state_ok2
    xor eax, eax
.state_ok2:
    lea rcx, [rel intro_names]
    mov rdx, [rcx + rax * 8]
    lea edi, [r14d + 12]
    lea esi, [r15d + 30]
    mov ecx, 9
    mov r8d, [rel col_green]
    call gfx_text

    ; === PREDICT node - ring layout ===
    movzx edi, byte [rel node_angles + NODE_PREDICT]
    call calc_ring_pos
    mov r14d, eax
    mov r15d, edx

    ; Connection line: to UHMA edge
    mov edi, r14d
    mov esi, r15d
    call calc_uhma_edge
    mov [rsp], eax
    mov [rsp+4], edx
    mov edi, r14d
    mov esi, r15d
    mov edx, [rsp]
    mov ecx, [rsp+4]
    mov r8d, [rel col_cyan]
    call gfx_line

    sub r14d, RNODE_W/2
    sub r15d, RNODE_H/2

    mov edi, r14d
    mov esi, r15d
    mov edx, RNODE_W
    mov ecx, RNODE_H
    mov r8d, 0x00886644
    call gfx_fill_rect

    mov edi, r14d
    mov esi, r15d
    mov edx, RNODE_W
    mov ecx, RNODE_H
    mov r8d, [rel col_white]
    call gfx_rect

    ; Store PREDICT rect
    lea rax, [rel node_rects]
    mov [rax + NODE_PREDICT*16], r14d
    mov [rax + NODE_PREDICT*16 + 4], r15d
    mov dword [rax + NODE_PREDICT*16 + 8], RNODE_W
    mov dword [rax + NODE_PREDICT*16 + 12], RNODE_H

    ; Label centered: "PREDICT" = 7 chars
    lea edi, [r14d + 22]
    lea esi, [r15d + 18]
    lea rdx, [rel node_predict]
    mov ecx, 7
    mov r8d, [rel col_text]
    call gfx_text

    ; Show last prediction (hex value, truncate to 8 chars max)
    mov eax, [rbx + STATE_OFFSET + ST_LAST_PREDICT]
    lea rdi, [rsp]
    call format_hex
    mov [rsp + 32], eax

    ; Hex value centered
    lea edi, [r14d + 16]
    lea esi, [r15d + 30]
    lea rdx, [rsp]
    mov ecx, 8              ; max 8 chars to fit
    mov r8d, [rel col_yellow]
    call gfx_text

    ; === DISPATCH node - ring layout ===
    movzx edi, byte [rel node_angles + NODE_DISPATCH]
    call calc_ring_pos
    mov r14d, eax
    mov r15d, edx

    ; Connection line: to UHMA edge
    mov edi, r14d
    mov esi, r15d
    call calc_uhma_edge
    mov [rsp], eax
    mov [rsp+4], edx
    mov edi, r14d
    mov esi, r15d
    mov edx, [rsp]
    mov ecx, [rsp+4]
    mov r8d, [rel col_cyan]
    call gfx_line

    sub r14d, RNODE_W/2
    sub r15d, RNODE_H/2

    mov edi, r14d
    mov esi, r15d
    mov edx, RNODE_W
    mov ecx, RNODE_H
    mov r8d, [rel col_dispatch]
    call gfx_fill_rect

    mov edi, r14d
    mov esi, r15d
    mov edx, RNODE_W
    mov ecx, RNODE_H
    mov r8d, [rel col_white]
    call gfx_rect

    ; Store DISPATCH rect
    lea rax, [rel node_rects]
    mov [rax + NODE_DISPATCH*16], r14d
    mov [rax + NODE_DISPATCH*16 + 4], r15d
    mov dword [rax + NODE_DISPATCH*16 + 8], RNODE_W
    mov dword [rax + NODE_DISPATCH*16 + 12], RNODE_H

    ; Label centered: "DISPATCH" = 8 chars
    lea edi, [r14d + 16]
    lea esi, [r15d + 18]
    lea rdx, [rel node_dispatch]
    mov ecx, 8
    mov r8d, [rel col_text]
    call gfx_text

    ; Mode name centered
    mov eax, [rbx + STATE_OFFSET + ST_DISPATCH_MODE]
    cmp eax, 8
    jl .mode_ok
    xor eax, eax
.mode_ok:
    lea rcx, [rel mode_names]
    mov rdx, [rcx + rax * 8]
    lea edi, [r14d + 12]
    lea esi, [r15d + 30]
    mov ecx, 9                  ; max 9 chars
    mov r8d, [rel col_yellow]
    call gfx_text

    ; === ACCURACY node - ring layout ===
    movzx edi, byte [rel node_angles + NODE_ACCURACY]
    call calc_ring_pos
    mov r14d, eax
    mov r15d, edx

    ; Connection line: to UHMA edge
    mov edi, r14d
    mov esi, r15d
    call calc_uhma_edge
    mov [rsp], eax
    mov [rsp+4], edx
    mov edi, r14d
    mov esi, r15d
    mov edx, [rsp]
    mov ecx, [rsp+4]
    mov r8d, [rel col_cyan]
    call gfx_line

    sub r14d, RNODE_W/2
    sub r15d, RNODE_H/2

    mov edi, r14d
    mov esi, r15d
    mov edx, RNODE_W
    mov ecx, RNODE_H
    mov r8d, 0x00446688
    call gfx_fill_rect

    mov edi, r14d
    mov esi, r15d
    mov edx, RNODE_W
    mov ecx, RNODE_H
    mov r8d, [rel col_white]
    call gfx_rect

    ; Store ACCURACY rect
    lea rax, [rel node_rects]
    mov [rax + NODE_ACCURACY*16], r14d
    mov [rax + NODE_ACCURACY*16 + 4], r15d
    mov dword [rax + NODE_ACCURACY*16 + 8], RNODE_W
    mov dword [rax + NODE_ACCURACY*16 + 12], RNODE_H

    ; Label centered: "ACCURACY" = 8 chars
    lea edi, [r14d + 16]
    lea esi, [r15d + 18]
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
    ; Percentage centered
    lea edi, [r14d + 32]
    lea esi, [r15d + 30]
    lea rdx, [rsp]
    mov ecx, [rsp + 32]
    mov r8d, [rel col_green]
    call gfx_text
    ; Add % sign after number
    mov eax, [rsp + 32]
    shl eax, 3              ; text width in pixels
    lea edi, [r14d + 32]
    add edi, eax            ; after number
    lea esi, [r15d + 30]
    lea rdx, [rel pct_sign]
    mov ecx, 1
    mov r8d, [rel col_green]
    call gfx_text
    jmp .acc_done
.no_acc:
    ; Center "N/A"
    lea edi, [r14d + 32]
    lea esi, [r15d + 30]
    lea rdx, [rel node_na]
    mov ecx, 3
    mov r8d, [rel col_text_dim]
    call gfx_text
.acc_done:

    ; === NEW FEATURE NODES ===

    ; === HIVE node - ring layout (larger node for pheromone bars) ===
    movzx edi, byte [rel node_angles + NODE_HIVE]
    call calc_ring_pos
    mov r14d, eax
    mov r15d, edx

    ; Connection line: to UHMA edge
    mov edi, r14d
    mov esi, r15d
    call calc_uhma_edge
    mov [rsp], eax
    mov [rsp+4], edx
    mov edi, r14d
    mov esi, r15d
    mov edx, [rsp]
    mov ecx, [rsp+4]
    mov r8d, [rel col_yellow]
    call gfx_line

    ; Adjust to top-left (HIVE is 90x55)
    sub r14d, 45
    sub r15d, 27

    ; Node box - yellow/gold for hive
    mov edi, r14d
    mov esi, r15d
    mov edx, 90
    mov ecx, 55
    mov r8d, 0x00554422
    call gfx_fill_rect

    mov edi, r14d
    mov esi, r15d
    mov edx, 90
    mov ecx, 55
    mov r8d, [rel col_yellow]
    call gfx_rect

    ; Store HIVE rect
    lea rax, [rel node_rects]
    mov [rax + NODE_HIVE*16], r14d
    mov [rax + NODE_HIVE*16 + 4], r15d
    mov dword [rax + NODE_HIVE*16 + 8], 90
    mov dword [rax + NODE_HIVE*16 + 12], 55

    ; Label centered: "HIVE" = 4 chars in 90px panel
    lea edi, [r14d + 30]
    lea esi, [r15d + 32]
    lea rdx, [rel node_hive]
    mov ecx, 4
    mov r8d, [rel col_white]
    call gfx_text

    ; Draw pheromone level bars (within 90px width, 8px margin each side = 74px max)
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
    lea esi, [r15d + 23]
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
    lea esi, [r15d + 33]
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
    lea esi, [r15d + 43]
    mov edx, eax
    mov ecx, 8
    mov r8d, [rel col_orange]
    call gfx_fill_rect

    ; Bar labels (D/O/E) - inside panel
    lea edi, [r14d + 72]
    lea esi, [r15d + 23]
    lea rdx, [rel lbl_d]
    mov ecx, 1
    mov r8d, [rel col_magenta]
    call gfx_text

    lea edi, [r14d + 72]
    lea esi, [r15d + 33]
    lea rdx, [rel lbl_o]
    mov ecx, 1
    mov r8d, [rel col_green]
    call gfx_text

    lea edi, [r14d + 72]
    lea esi, [r15d + 43]
    lea rdx, [rel lbl_e]
    mov ecx, 1
    mov r8d, [rel col_orange]
    call gfx_text

    ; === ROSETTA node - ring layout ===
    movzx edi, byte [rel node_angles + NODE_ROSETTA]
    call calc_ring_pos
    mov r14d, eax               ; node center x
    mov r15d, edx               ; node center y

    ; Connection line: to UHMA edge
    mov edi, r14d
    mov esi, r15d
    call calc_uhma_edge
    mov [rsp], eax
    mov [rsp+4], edx
    mov edi, r14d
    mov esi, r15d
    mov edx, [rsp]
    mov ecx, [rsp+4]
    mov r8d, [rel col_cyan]
    call gfx_line

    ; Adjust to top-left for drawing
    sub r14d, RNODE_W/2
    sub r15d, RNODE_H/2

    ; Node box - cyan for rosetta
    mov edi, r14d
    mov esi, r15d
    mov edx, RNODE_W
    mov ecx, RNODE_H
    mov r8d, 0x00224455
    call gfx_fill_rect

    mov edi, r14d
    mov esi, r15d
    mov edx, RNODE_W
    mov ecx, RNODE_H
    mov r8d, [rel col_cyan]
    call gfx_rect

    ; Store ROSETTA rect
    lea rax, [rel node_rects]
    mov [rax + NODE_ROSETTA*16], r14d
    mov [rax + NODE_ROSETTA*16 + 4], r15d
    mov dword [rax + NODE_ROSETTA*16 + 8], RNODE_W
    mov dword [rax + NODE_ROSETTA*16 + 12], RNODE_H

    ; Label centered: "ROSETTA" = 7 chars
    lea edi, [r14d + 22]
    lea esi, [r15d + 18]
    lea rdx, [rel node_rosetta]
    mov ecx, 7
    mov r8d, [rel col_white]
    call gfx_text

    ; Get cached verify mode
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
    lea edi, [r14d + 16]
    lea esi, [r15d + 30]
    mov r8d, [rel col_cyan]
    call gfx_text

    ; === MYCO node - ring layout ===
    movzx edi, byte [rel node_angles + NODE_MYCO]
    call calc_ring_pos
    mov r14d, eax               ; node center x
    mov r15d, edx               ; node center y

    ; Connection line: to UHMA edge
    mov edi, r14d
    mov esi, r15d
    call calc_uhma_edge
    mov [rsp], eax
    mov [rsp+4], edx
    mov edi, r14d
    mov esi, r15d
    mov edx, [rsp]
    mov ecx, [rsp+4]
    mov r8d, [rel col_green]
    call gfx_line

    ; Adjust to top-left for drawing
    sub r14d, RNODE_W/2
    sub r15d, RNODE_H/2

    ; Node box - green for myco
    mov edi, r14d
    mov esi, r15d
    mov edx, RNODE_W
    mov ecx, RNODE_H
    mov r8d, 0x00224422
    call gfx_fill_rect

    mov edi, r14d
    mov esi, r15d
    mov edx, RNODE_W
    mov ecx, RNODE_H
    mov r8d, [rel col_green]
    call gfx_rect

    ; Store MYCO rect
    lea rax, [rel node_rects]
    mov [rax + NODE_MYCO*16], r14d
    mov [rax + NODE_MYCO*16 + 4], r15d
    mov dword [rax + NODE_MYCO*16 + 8], RNODE_W
    mov dword [rax + NODE_MYCO*16 + 12], RNODE_H

    ; Label centered: "MYCO" = 4 chars
    lea edi, [r14d + 30]
    lea esi, [r15d + 18]
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
    lea edi, [r14d + 24]
    lea esi, [r15d + 30]
    mov r8d, [rel col_green]
    call gfx_text

    ; === SPORE node - ring layout ===
    movzx edi, byte [rel node_angles + NODE_SPORE]
    call calc_ring_pos
    mov r14d, eax               ; node center x
    mov r15d, edx               ; node center y

    ; Connection line: to UHMA edge
    mov edi, r14d
    mov esi, r15d
    call calc_uhma_edge
    mov [rsp], eax
    mov [rsp+4], edx
    mov edi, r14d
    mov esi, r15d
    mov edx, [rsp]
    mov ecx, [rsp+4]
    mov r8d, [rel col_orange]
    call gfx_line

    ; Adjust to top-left for drawing
    sub r14d, RNODE_W/2
    sub r15d, RNODE_H/2

    ; Node box - orange for spore
    mov edi, r14d
    mov esi, r15d
    mov edx, RNODE_W
    mov ecx, RNODE_H
    mov r8d, 0x00443322
    call gfx_fill_rect

    mov edi, r14d
    mov esi, r15d
    mov edx, RNODE_W
    mov ecx, RNODE_H
    mov r8d, [rel col_orange]
    call gfx_rect

    ; Store SPORE rect
    lea rax, [rel node_rects]
    mov [rax + NODE_SPORE*16], r14d
    mov [rax + NODE_SPORE*16 + 4], r15d
    mov dword [rax + NODE_SPORE*16 + 8], RNODE_W
    mov dword [rax + NODE_SPORE*16 + 12], RNODE_H

    ; Label centered: "SPORE" = 5 chars
    lea edi, [r14d + 28]
    lea esi, [r15d + 18]
    lea rdx, [rel node_spore]
    mov ecx, 5
    mov r8d, [rel col_white]
    call gfx_text

    ; Show gene status centered
    lea edi, [r14d + 22]
    lea esi, [r15d + 30]
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

    ; Check if animation in progress - draw overlay on top of nodes
    mov eax, [rel anim_state]
    test eax, eax
    jnz .draw_animating

    ; Check if focused - draw focus panel overlay
    mov eax, [rel focus_node]
    cmp eax, -1
    jne .draw_focus_panel

    jmp .done

.draw_brain_view:
    ; === BRAIN VIEW: Display regions command output ===
    ; Draw panel background
    mov edi, CANVAS_X + 10
    mov esi, CANVAS_Y + 10
    mov edx, CANVAS_W - 20
    mov ecx, CANVAS_H - 20
    mov r8d, [rel col_panel_hi]
    call gfx_fill_rect

    ; Border
    mov edi, CANVAS_X + 10
    mov esi, CANVAS_Y + 10
    mov edx, CANVAS_W - 20
    mov ecx, CANVAS_H - 20
    mov r8d, [rel col_cyan]
    call gfx_rect

    ; Title
    mov edi, CANVAS_X + 30
    mov esi, CANVAS_Y + 35
    lea rdx, [rel lbl_memory]
    mov ecx, 10
    mov r8d, [rel col_magenta]
    call gfx_text

    ; Back hint
    mov edi, CANVAS_X + CANVAS_W - 200
    mov esi, CANVAS_Y + 35
    lea rdx, [rel hint_back]
    mov ecx, 25
    mov r8d, [rel col_text_dim]
    call gfx_text

    ; Draw query_ring content (regions response from MCP)
    mov edi, CANVAS_X + 30
    mov esi, CANVAS_Y + 70
    mov edx, 25                  ; max lines (fit in canvas)
    call draw_query_lines
    jmp .done

.draw_animating:
    ; === ANIMATION VIEW: Draw transitioning panel overlay ===
    ; Background nodes already drawn, now overlay the animated panel

    ; Draw animated panel with current interpolated rect
    mov edi, [rel anim_cur_x]
    mov esi, [rel anim_cur_y]
    mov edx, [rel anim_cur_w]
    mov ecx, [rel anim_cur_h]
    mov r8d, [rel col_panel_hi]
    call gfx_fill_rect

    mov edi, [rel anim_cur_x]
    mov esi, [rel anim_cur_y]
    mov edx, [rel anim_cur_w]
    mov ecx, [rel anim_cur_h]
    mov r8d, [rel col_white]
    call gfx_rect

    ; Show node name in the animated panel
    mov eax, [rel anim_node]
    cmp eax, NODE_REGIONS
    jne .anim_not_regions
    lea rdx, [rel node_regions]
    mov ecx, 7
    jmp .anim_show_label
.anim_not_regions:
    cmp eax, NODE_TOKENS
    jne .anim_not_tokens
    lea rdx, [rel node_tokens]
    mov ecx, 6
    jmp .anim_show_label
.anim_not_tokens:
    cmp eax, NODE_STATE
    jne .anim_not_state
    lea rdx, [rel node_state]
    mov ecx, 5
    jmp .anim_show_label
.anim_not_state:
    cmp eax, NODE_PREDICT
    jne .anim_not_predict
    lea rdx, [rel node_predict]
    mov ecx, 7
    jmp .anim_show_label
.anim_not_predict:
    cmp eax, NODE_DISPATCH
    jne .anim_not_dispatch
    lea rdx, [rel node_dispatch]
    mov ecx, 8
    jmp .anim_show_label
.anim_not_dispatch:
    cmp eax, NODE_ACCURACY
    jne .anim_not_accuracy
    lea rdx, [rel node_accuracy]
    mov ecx, 8
    jmp .anim_show_label
.anim_not_accuracy:
    lea rdx, [rel detail_expanded]
    mov ecx, 15

.anim_show_label:
    mov edi, [rel anim_cur_x]
    add edi, 20
    mov esi, [rel anim_cur_y]
    add esi, 25
    mov r8d, [rel col_cyan]
    call gfx_text

    jmp .done

.draw_focus_panel:
    ; "Click outside to go back" hint
    mov edi, CANVAS_X + 20
    mov esi, CANVAS_Y + 30
    lea rdx, [rel hint_back]
    mov ecx, 20
    mov r8d, [rel col_cyan]
    call gfx_text

    ; Draw large expanded panel (fixed position, bigger)
    mov edi, CANVAS_X + 30
    mov esi, CANVAS_Y + 50
    mov edx, 650                 ; bigger panel
    mov ecx, 500                 ; taller panel
    mov r8d, [rel col_panel_hi]
    call gfx_fill_rect

    mov edi, CANVAS_X + 30
    mov esi, CANVAS_Y + 50
    mov edx, 650
    mov ecx, 500
    mov r8d, [rel col_white]
    call gfx_rect

    ; Set text position for panel content
    mov r12d, CANVAS_X + 50      ; text x
    mov r13d, CANVAS_Y + 80      ; text y (title)

    ; Dispatch based on focus_node (each shows context-specific live data)
    mov eax, [rel focus_node]
    cmp eax, NODE_REGIONS
    je .focus_regions
    cmp eax, NODE_TOKENS
    je .focus_tokens
    cmp eax, NODE_STATE
    je .focus_state
    cmp eax, NODE_PREDICT
    je .focus_predict
    cmp eax, NODE_DISPATCH
    je .focus_dispatch
    cmp eax, NODE_ACCURACY
    je .focus_accuracy
    cmp eax, NODE_HIVE
    je .focus_hive
    cmp eax, NODE_ROSETTA
    je .focus_rosetta
    cmp eax, NODE_MYCO
    je .focus_myco
    cmp eax, NODE_SPORE
    je .focus_spore
    jmp .focus_generic

.focus_regions:
    ; Title
    mov edi, r12d
    mov esi, r13d
    lea rdx, [rel node_regions]
    mov ecx, 7
    mov r8d, [rel col_cyan]
    call gfx_text
    ; Draw query_ring content (status response)
    mov edi, r12d
    lea esi, [r13d + 25]
    mov edx, 22                  ; max lines
    call draw_query_lines
    jmp .done

.focus_tokens:
    ; Title
    mov edi, r12d
    mov esi, r13d
    lea rdx, [rel node_tokens]
    mov ecx, 6
    mov r8d, [rel col_cyan]
    call gfx_text
    ; Draw query_ring content
    mov edi, r12d
    lea esi, [r13d + 25]
    mov edx, 22
    call draw_query_lines
    jmp .done

.focus_state:
    ; Title
    mov edi, r12d
    mov esi, r13d
    lea rdx, [rel node_state]
    mov ecx, 5
    mov r8d, [rel col_cyan]
    call gfx_text
    ; Draw query_ring content (intro response)
    mov edi, r12d
    lea esi, [r13d + 25]
    mov edx, 22
    call draw_query_lines
    jmp .done

.focus_predict:
    ; Title
    mov edi, r12d
    mov esi, r13d
    lea rdx, [rel node_predict]
    mov ecx, 7
    mov r8d, [rel col_cyan]
    call gfx_text
    ; Draw query_ring content
    mov edi, r12d
    lea esi, [r13d + 25]
    mov edx, 22
    call draw_query_lines
    jmp .done

.focus_dispatch:
    ; Title
    mov edi, r12d
    mov esi, r13d
    lea rdx, [rel node_dispatch]
    mov ecx, 8
    mov r8d, [rel col_cyan]
    call gfx_text
    ; Draw query_ring content
    mov edi, r12d
    lea esi, [r13d + 25]
    mov edx, 22
    call draw_query_lines
    jmp .done

.focus_accuracy:
    ; Title
    mov edi, r12d
    mov esi, r13d
    lea rdx, [rel node_accuracy]
    mov ecx, 8
    mov r8d, [rel col_cyan]
    call gfx_text
    ; Draw query_ring content (status response)
    mov edi, r12d
    lea esi, [r13d + 25]
    mov edx, 22
    call draw_query_lines
    jmp .done

.focus_hive:
    ; Title
    mov edi, r12d
    mov esi, r13d
    lea rdx, [rel lbl_hive]
    mov ecx, 4
    mov r8d, [rel col_yellow]
    call gfx_text
    ; Draw query_ring content (hive response)
    mov edi, r12d
    lea esi, [r13d + 25]
    mov edx, 22
    call draw_query_lines
    jmp .done

.focus_rosetta:
    ; Title
    mov edi, r12d
    mov esi, r13d
    lea rdx, [rel node_rosetta]
    mov ecx, 7
    mov r8d, [rel col_magenta]
    call gfx_text
    ; Draw query_ring content (geom response)
    mov edi, r12d
    lea esi, [r13d + 25]
    mov edx, 22
    call draw_query_lines
    jmp .done

.focus_myco:
    ; Title
    mov edi, r12d
    mov esi, r13d
    lea rdx, [rel node_myco]
    mov ecx, 4
    mov r8d, [rel col_green]
    call gfx_text
    ; Draw query_ring content (colony response)
    mov edi, r12d
    lea esi, [r13d + 25]
    mov edx, 22
    call draw_query_lines
    jmp .done

.focus_spore:
    ; Title
    mov edi, r12d
    mov esi, r13d
    lea rdx, [rel node_spore]
    mov ecx, 5
    mov r8d, [rel col_orange]
    call gfx_text
    ; Draw query_ring content (genes response)
    mov edi, r12d
    lea esi, [r13d + 25]
    mov edx, 22
    call draw_query_lines
    jmp .done

.focus_generic:
    ; Title
    mov edi, r12d
    mov esi, r13d
    lea rdx, [rel detail_expanded]
    mov ecx, 13
    mov r8d, [rel col_cyan]
    call gfx_text
    ; Draw query_ring content
    mov edi, r12d
    lea esi, [r13d + 25]
    mov edx, 22
    call draw_query_lines

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
;; draw_query_lines — Draw lines from query_ring in expanded panel
;; Args: edi=x, esi=y (top-left), edx=max_lines
;; Uses query_ring content starting from view position
;; ============================================================
draw_query_lines:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 88                  ; line buffer (80) + alignment

    ; Stack layout:
    ; [rsp+0..79]  = line buffer (80 bytes)
    ; [rsp+80]     = saved char count (8 bytes)

    mov r12d, edi                ; x position
    mov r13d, esi                ; y position (will increment)
    mov r14d, edx                ; max lines

    ; Get view position into ring
    mov r15d, [rel query_write]
    sub r15d, 2048               ; show last ~2K of data
    jns .dql_pos_ok
    xor r15d, r15d               ; clamp to 0
.dql_pos_ok:

    ; Loop through lines
    xor ebx, ebx                 ; line counter
.dql_line_loop:
    cmp ebx, r14d
    jge .dql_lines_done

    ; Copy one line from ring to stack buffer
    lea rdi, [rsp]               ; dest = stack buffer
    lea rsi, [rel query_ring]    ; ring base
    mov ecx, 48                  ; max chars per line (panel width)
    xor edx, edx                 ; char count

.dql_copy_char:
    cmp edx, ecx
    jge .dql_line_ready

    mov eax, r15d
    and eax, RING_MASK
    movzx eax, byte [rsi + rax]

    ; Stop on newline
    cmp al, 10
    je .dql_found_newline
    cmp al, 13
    je .dql_found_newline
    cmp al, 0
    je .dql_skip_null

    ; Store char
    mov [rdi + rdx], al
    inc edx
    inc r15d
    jmp .dql_copy_char

.dql_skip_null:
    inc r15d
    jmp .dql_copy_char

.dql_found_newline:
    inc r15d                     ; skip newline

.dql_line_ready:
    ; Null terminate
    mov byte [rdi + rdx], 0

    ; Skip empty lines
    test edx, edx
    jz .dql_next_line

    ; Save char count to stack slot (avoid push/pop to keep alignment)
    mov [rsp + 80], edx

    ; Draw the line
    mov edi, r12d
    mov esi, r13d
    lea rdx, [rsp]               ; line buffer
    mov ecx, [rsp + 80]          ; char count
    mov r8d, [rel col_text]
    call gfx_text

    ; Next line
    add r13d, 18                 ; line height
    inc ebx
    jmp .dql_line_loop

.dql_next_line:
    inc ebx
    jmp .dql_line_loop

.dql_lines_done:
    add rsp, 88
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    pop rbp
    ret

;; ============================================================
;; poll_channels — Send periodic queries to QUERY/DEBUG panels
;; Sends "status" to QUERY (channel 1) and "receipts 5" to DEBUG (channel 2)
;; ============================================================
poll_channels:
    push rbx
    sub rsp, 8                      ; 1 push (odd) = aligned, need multiple of 16

    ; Send "status" to QUERY channel (fire-and-forget, response via read_streams)
    mov edi, 1                      ; QUERY channel
    lea rsi, [rel poll_status_cmd]
    call mcp_send_async

    ; Send "receipts 5" to DEBUG channel (fire-and-forget)
    mov edi, 2                      ; DEBUG channel
    lea rsi, [rel poll_receipts_cmd]
    call mcp_send_async


    add rsp, 8
    pop rbx
    ret

;; ============================================================
;; enable_feed_stream — Bind run-log stream to FEED panel
;; ============================================================
enable_feed_stream:
    sub rsp, 8                      ; align for call
    cmp dword [rel mcp_running], 0
    je .efs_done
    mov edi, 0                      ; FEED channel
    lea rsi, [rel cmd_listen]
    call mcp_send_async
.efs_done:
    add rsp, 8
    ret

;; ============================================================
;; read_streams — Read from gateway socket, route by subnet to ring buffers
;; Single socket, demux by subnet field in frame header
;; ============================================================
read_streams:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 1032               ; temp buffer on stack (1024 + alignment)
                                ; 4 pushes (even) + 1032 → 1064 mod 16 = 8. aligned.

.read_again:
    ; Read one frame from gateway (channel param ignored, single socket)
    xor edi, edi
    lea rsi, [rsp]
    mov edx, 1024
    call mcp_read_stream
    test eax, eax
    jle .streams_done           ; no data or error
    cmp eax, 2048
    jg .streams_done            ; sanity

    mov r12d, eax               ; r12d = bytes read

    ; Route by subnet from last frame header
    movzx eax, byte [rel mcp_last_subnet]
    cmp al, SUBNET_CONSOL       ; 4 = FEED
    je .route_feed
    cmp al, SUBNET_SELF         ; 5 = DEBUG
    je .route_debug
    ; Default (SUBNET_REPL=1 or unknown) → QUERY
    jmp .route_query

.route_feed:
    lea rdi, [rel feed_ring]
    lea r13, [rel feed_write]
    lea r14, [rel feed_paused]
    lea rbx, [rel feed_view]
    jmp .append_ring

.route_query:
    lea rdi, [rel query_ring]
    lea r13, [rel query_write]
    lea r14, [rel query_paused]
    lea rbx, [rel query_view]
    jmp .append_ring

.route_debug:
    lea rdi, [rel debug_ring]
    lea r13, [rel debug_write]
    lea r14, [rel debug_paused]
    lea rbx, [rel debug_view]

.append_ring:
    ; rdi=ring base, r13=&write_pos, r14=&paused, rbx=&view_pos
    ; r12d=byte count, [rsp]=source data
    lea rsi, [rsp]
    mov r8d, [r13]              ; current write pos
    xor ecx, ecx
.ring_copy:
    cmp ecx, r12d
    jge .ring_copy_done
    movzx eax, byte [rsi + rcx]
    mov edx, r8d
    and edx, RING_MASK
    mov [rdi + rdx], al
    inc r8d
    inc ecx
    jmp .ring_copy
.ring_copy_done:
    and r8d, RING_MASK
    mov [r13], r8d              ; update write pos
    cmp dword [r14], 0          ; paused?
    jne .read_again             ; if paused, don't update view, try next frame
    mov [rbx], r8d              ; update view pos
    jmp .read_again             ; drain all available frames

.streams_done:
    add rsp, 1032
    pop r14
    pop r13
    pop r12
    pop rbx
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
    sub rsp, 16

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
    add rsp, 16
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; draw_output — 3 live streaming panels (FEED, QUERY, DEBUG)
;; Click to pause, release to resume live scrolling
;; ============================================================
draw_output:
    push rbx
    push r12
    push r13
    push r14
    push r15
    push rbp                     ; 6 pushes = 48, entry was 16n-8, now 16n-56 -> need sub 8
    sub rsp, 8                   ; align: 16n-64 = 16(n-4) aligned

    ; Draw FEED panel
    mov edi, OUT_PANEL_X
    mov esi, OUT_FEED_Y
    lea rdx, [rel lbl_feed]
    lea rcx, [rel feed_ring]
    mov r8d, [rel feed_view]
    mov r9d, [rel feed_paused]
    call draw_ring_panel

    ; Draw QUERY panel
    mov edi, OUT_PANEL_X
    mov esi, OUT_QUERY_Y
    lea rdx, [rel lbl_query]
    lea rcx, [rel query_ring]
    mov r8d, [rel query_view]
    mov r9d, [rel query_paused]
    call draw_ring_panel

    ; Draw DEBUG panel
    mov edi, OUT_PANEL_X
    mov esi, OUT_DEBUG_Y
    lea rdx, [rel lbl_debug]
    lea rcx, [rel debug_ring]
    mov r8d, [rel debug_view]
    mov r9d, [rel debug_paused]
    call draw_ring_panel

    add rsp, 8
    pop rbp
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; draw_ring_panel — Draw single streaming panel from ring buffer
;; edi = x, esi = y, rdx = label, rcx = ring_buf, r8d = view_pos, r9d = paused
;; ============================================================
draw_ring_panel:
    push rbx
    push r12
    push r13
    push r14
    push r15
    push rbp
    sub rsp, 88                  ; line buffer (64) + saved params (24)

    mov r12d, edi               ; x
    mov r13d, esi               ; y
    mov [rsp + 64], rdx         ; label ptr
    mov r14, rcx                ; ring buffer ptr
    mov r15d, r8d               ; view position
    mov ebp, r9d                ; paused flag

    ; Panel background
    mov edi, r12d
    mov esi, r13d
    mov edx, OUT_PANEL_W
    mov ecx, OUT_PANEL_H
    mov r8d, 0x00151530          ; dark blue-gray
    call gfx_fill_rect

    ; Panel border (highlight if paused)
    mov edi, r12d
    mov esi, r13d
    mov edx, OUT_PANEL_W
    mov ecx, OUT_PANEL_H
    test ebp, ebp
    jz .normal_border
    mov r8d, 0x00FF8800          ; orange when paused
    jmp .draw_border
.normal_border:
    mov r8d, [rel col_border]
.draw_border:
    call gfx_rect

    ; Panel title (use fixed safe length)
    mov edi, r12d
    add edi, 10
    mov esi, r13d
    add esi, 14
    mov rdx, [rsp + 64]         ; label ptr
    mov ecx, 12                 ; max label length
    mov r8d, [rel col_cyan]
    call gfx_text

    ; Draw [PAUSED] indicator if paused
    test ebp, ebp
    jz .not_paused
    lea edi, [r12d + OUT_PANEL_W - 80]
    lea esi, [r13d + 14]
    lea rdx, [rel lbl_paused]
    mov ecx, 8
    mov r8d, 0x00FF8800
    call gfx_text
.not_paused:

    ; Draw ring buffer content (last ~10 lines before view_pos)
    ; Stack layout: [rsp+0..63]=line buffer, [rsp+72]=loop counter, [rsp+76]=scan limit

    ; Calculate start position: scan backwards from view_pos to find start of last 10 lines
    mov eax, r15d               ; view_pos (scan start, moving back)
    xor ecx, ecx                ; newline counter
    mov edx, 2000               ; max bytes to scan back (increased from 600)

.scan_back:
    dec eax
    and eax, RING_MASK
    cmp byte [r14 + rax], 10    ; newline?
    jne .not_nl_back
    inc ecx
    cmp ecx, 10
    je .found_start
.not_nl_back:
    dec edx
    jnz .scan_back

.found_start:
    ; Found start (or hit limit)
    inc eax
    and eax, RING_MASK
    mov ebx, eax                ; scan_pos for drawing loop

    mov eax, r13d
    add eax, 30
    mov r15d, eax               ; y for first line
    mov dword [rsp + 72], 10    ; max lines

.scan_lines:
    ; Copy one line from ring to stack buffer
    lea rdi, [rsp]              ; line buffer
    xor ecx, ecx                ; char count
    mov dword [rsp + 76], 80    ; scan limit (prevent infinite loop on empty buffer)

.copy_line:
    dec dword [rsp + 76]        ; decrement scan limit
    jz .line_done               ; exit if scanned too many bytes
    cmp ecx, 60                 ; max chars per line (increased from 24)
    jge .line_done
    mov eax, ebx
    and eax, RING_MASK
    movzx edx, byte [r14 + rax]
    inc ebx                     ; always advance scan position
    test dl, dl                 ; null?
    jz .copy_line               ; skip nulls, continue scanning
    cmp dl, 10                  ; newline?
    je .line_done
    cmp dl, 13                  ; CR?
    je .copy_line               ; skip CR, continue
    cmp dl, 32                  ; printable? (>= space)
    jl .copy_line               ; skip control chars
    mov [rdi + rcx], dl
    inc ecx
    jmp .copy_line

.line_done:
    mov byte [rdi + rcx], 0     ; null terminate

    ; Draw line if not empty
    test ecx, ecx
    jz .skip_draw

    mov edi, r12d
    add edi, 10
    mov esi, r15d
    lea rdx, [rsp]              ; line buffer
    ; ecx already has char count
    mov r8d, [rel col_text]
    call gfx_text

    add r15d, 14                ; next line y

.skip_draw:
    dec dword [rsp + 72]
    jnz .scan_lines

    add rsp, 88
    pop rbp
    pop r15
    pop r14
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
    sub rsp, 128                ; increased stack for temp buffers

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
    lea esi, [r13d + 18]        ; adjusted y
    mov eax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    lea rdi, [rsp]
    call format_num
    mov [rsp + 16], eax

    mov edi, 20
    lea esi, [r13d + 18]
    lea rdx, [rsp]
    mov ecx, [rsp + 16]
    mov r8d, [rel col_cyan]
    call gfx_text

    ; DEBUG: Display Feed Write/View pointers
    ; Label "W:"
    lea edi, [r12d + 150]
    lea esi, [r13d + 18]
    lea rdx, [rel dbg_w_lbl]
    mov ecx, 2
    mov r8d, [rel col_text]
    call gfx_text

    ; Value Write
    mov eax, [rel feed_write]
    lea rdi, [rsp]
    call format_num
    mov [rsp + 16], eax

    lea edi, [r12d + 170]
    lea esi, [r13d + 18]
    lea rdx, [rsp]
    mov ecx, [rsp + 16]
    mov r8d, [rel col_yellow]
    call gfx_text

    ; Label "V:"
    lea edi, [r12d + 250]
    lea esi, [r13d + 18]
    lea rdx, [rel dbg_v_lbl]
    mov ecx, 2
    mov r8d, [rel col_text]
    call gfx_text

    ; Value View
    mov eax, [rel feed_view]
    lea rdi, [rsp]
    call format_num
    mov [rsp + 16], eax

    lea edi, [r12d + 270]
    lea esi, [r13d + 18]
    lea rdx, [rsp]
    mov ecx, [rsp + 16]
    mov r8d, [rel col_green]
    call gfx_text

    add rsp, 128
    pop r13
    pop r12
    pop rbx
    ret

section .data
    dbg_w_lbl: db "W:", 0
    dbg_v_lbl: db "V:", 0
section .text


;; ============================================================
;; Ring Layout Helper Functions (semi-3D orbital view)
;; ============================================================

;; get_sine — Get sine value for angle
;; edi = angle (0-255, where 64=90°)
;; Returns: eax = sine * 127 (signed, -127 to +127)
get_sine:
    and edi, 0xFF
    cmp edi, 64
    jl .q1
    cmp edi, 128
    jl .q2
    cmp edi, 192
    jl .q3
    ; Q4: 192-255, sin negative, use 255-angle (avoids index 64 out of bounds)
    mov eax, 255
    sub eax, edi
    lea rdi, [rel sine_table]
    movzx eax, byte [rdi + rax]
    neg eax
    ret
.q3: ; Q3: 128-191, sin negative, use angle-128
    sub edi, 128
    lea rax, [rel sine_table]
    movzx eax, byte [rax + rdi]
    neg eax
    ret
.q2: ; Q2: 64-127, sin positive, use 127-angle (avoids index 64 out of bounds)
    mov eax, 127
    sub eax, edi
    lea rdi, [rel sine_table]
    movzx eax, byte [rdi + rax]
    ret
.q1: ; Q1: 0-63, sin positive, use angle directly
    lea rax, [rel sine_table]
    movzx eax, byte [rax + rdi]
    ret

;; get_cosine — Get cosine value for angle (cos = sin(angle + 64))
;; edi = angle (0-255)
;; Returns: eax = cosine * 127 (signed)
get_cosine:
    add edi, 64                 ; cos(x) = sin(x + 90°)
    jmp get_sine

;; calc_ring_pos — Calculate x,y position on ring for given angle
;; edi = angle (0-255)
;; Returns: eax = x, edx = y
calc_ring_pos:
    push rbx
    push r12
    mov r12d, edi               ; save angle

    ; x = center_x + cos(angle) * radius_x
    mov edi, r12d
    call get_cosine
    imul eax, RING_RADIUS_X
    sar eax, 7                  ; divide by 127
    add eax, RING_CENTER_X
    mov ebx, eax                ; save x

    ; y = center_y - sin(angle) * radius_y (- because screen Y is inverted)
    mov edi, r12d
    call get_sine
    imul eax, RING_RADIUS_Y
    sar eax, 7
    mov edx, RING_CENTER_Y
    sub edx, eax                ; y

    mov eax, ebx                ; x
    pop r12
    pop rbx
    ret

;; calc_depth_scale — Calculate size scale based on Y position (3D depth)
;; edi = y position
;; Returns: eax = scale (64-128, where 100 = normal)
calc_depth_scale:
    ; Nodes at top (y small) = far = smaller (scale ~70)
    ; Nodes at bottom (y large) = near = larger (scale ~130)
    ; Linear interpolation based on y within canvas
    mov eax, edi
    sub eax, CANVAS_Y           ; y relative to canvas top
    imul eax, RING_DEPTH_SCALE  ; scale by depth factor
    mov ecx, CANVAS_H
    xor edx, edx
    div ecx                     ; eax = (y_rel * depth) / canvas_h
    add eax, 70                 ; base scale 70, max ~130
    ret

;; calc_uhma_edge — Calculate intersection point of line with UHMA panel edge
;; edi = outer node center x, esi = outer node center y
;; r12d = UHMA center x, r13d = UHMA center y (must be set by caller)
;; UHMA panel is 120x50 (half: 60x25)
;; Returns: eax = edge x, edx = edge y
calc_uhma_edge:
    push rbx
    push rcx

    ; Calculate direction from center to outer point
    mov eax, edi
    sub eax, r12d               ; dx = outer_x - center_x
    mov ebx, eax                ; save dx

    mov ecx, esi
    sub ecx, r13d               ; dy = outer_y - center_y

    ; Get absolute values for comparison
    mov r8d, ebx
    test r8d, r8d
    jns .dx_pos
    neg r8d                     ; |dx|
.dx_pos:
    mov r9d, ecx
    test r9d, r9d
    jns .dy_pos
    neg r9d                     ; |dy|
.dy_pos:

    ; Compare 60*|dy| vs 25*|dx| to determine which edge
    ; If 60*|dy| < 25*|dx|: hits vertical edge (left/right)
    ; Else: hits horizontal edge (top/bottom)
    imul r10d, r9d, 60          ; 60 * |dy|
    imul r11d, r8d, 25          ; 25 * |dx|

    cmp r10d, r11d
    jge .horizontal_edge

.vertical_edge:
    ; Hits left or right edge
    ; edge_x = center_x + sign(dx)*60
    ; edge_y = center_y + (dy * 60) / |dx|
    test r8d, r8d
    jz .at_center               ; avoid div by zero

    mov eax, r12d
    test ebx, ebx
    js .left_edge
    add eax, 60                 ; right edge
    jmp .calc_y_vert
.left_edge:
    sub eax, 60                 ; left edge
.calc_y_vert:
    push rax                    ; save edge_x
    mov eax, ecx                ; dy
    imul eax, 60
    cdq
    idiv r8d                    ; eax = dy * 60 / |dx|
    add eax, r13d               ; edge_y = center_y + result
    mov edx, eax                ; edge_y in edx
    pop rax                     ; edge_x in eax
    jmp .done

.horizontal_edge:
    ; Hits top or bottom edge
    ; edge_y = center_y + sign(dy)*25
    ; edge_x = center_x + (dx * 25) / |dy|
    test r9d, r9d
    jz .at_center               ; avoid div by zero

    mov edx, r13d
    test ecx, ecx
    js .top_edge
    add edx, 25                 ; bottom edge
    jmp .calc_x_horiz
.top_edge:
    sub edx, 25                 ; top edge
.calc_x_horiz:
    push rdx                    ; save edge_y
    mov eax, ebx                ; dx
    imul eax, 25
    cdq
    idiv r9d                    ; eax = dx * 25 / |dy|
    add eax, r12d               ; edge_x = center_x + result
    pop rdx                     ; edge_y in edx
    jmp .done

.at_center:
    ; Outer point at center (shouldn't happen)
    mov eax, r12d
    mov edx, r13d

.done:
    pop rcx
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

    mov edi, 940
    mov esi, 15
    mov rdx, rax
    mov ecx, 20
    mov r8d, [rel col_green]
    call gfx_text

.done:
    pop rbx
    ret

;; ============================================================
;; Animation Functions (TheBrain-style smooth transitions)
;; ============================================================

;; start_expand_anim — Start expanding animation for a node
;; edi = node index
start_expand_anim:
    push rbx
    push r12

    mov [rel anim_node], edi
    mov dword [rel anim_state], ANIM_EXPANDING
    mov dword [rel anim_progress], 0

    ; Clear query_ring so panel shows fresh data from this node's command
    mov dword [rel query_write], 0

    ; Get source rect from node_rects
    lea rax, [rel node_rects]
    imul ebx, edi, 16               ; offset = index * 16
    add rax, rbx

    mov ecx, [rax]                  ; src x
    mov [rel anim_src_x], ecx
    mov ecx, [rax + 4]              ; src y
    mov [rel anim_src_y], ecx
    mov ecx, [rax + 8]              ; src w
    mov [rel anim_src_w], ecx
    mov ecx, [rax + 12]             ; src h
    mov [rel anim_src_h], ecx

    ; Destination is expanded panel at canvas center
    mov dword [rel anim_dst_x], CANVAS_X + CANVAS_W/2 - 200
    mov dword [rel anim_dst_y], CANVAS_Y + CANVAS_H/2 - 150
    mov dword [rel anim_dst_w], 400
    mov dword [rel anim_dst_h], 300

    ; Initialize current to source
    mov ecx, [rel anim_src_x]
    mov [rel anim_cur_x], ecx
    mov ecx, [rel anim_src_y]
    mov [rel anim_cur_y], ecx
    mov ecx, [rel anim_src_w]
    mov [rel anim_cur_w], ecx
    mov ecx, [rel anim_src_h]
    mov [rel anim_cur_h], ecx

    pop r12
    pop rbx
    ret

;; start_collapse_anim — Start collapsing animation
;; Also copies expanded panel content to clipboard
start_collapse_anim:
    push rbx
    push r12                     ; save r12, also aligns stack (2 pushes = even)
    sub rsp, 8                   ; align for calls: 2 pushes (16) + 8 = 24, entry was 16n-8, now 16n-32 = aligned

    ; Get current focus node (more reliable than anim_node)
    mov edi, [rel focus_node]
    cmp edi, -1
    je .no_collapse              ; safety check

    mov r12d, edi                ; save focus_node in callee-saved register

    ; Copy query_ring to clipboard (contains panel content)
    lea rdi, [rel query_ring]
    call copy_ring_to_clipboard

    mov edi, r12d                ; restore focus_node
    mov [rel anim_node], edi

    mov dword [rel anim_state], ANIM_COLLAPSING
    mov dword [rel anim_progress], 0

    ; Source is expanded panel at canvas center
    mov dword [rel anim_src_x], CANVAS_X + CANVAS_W/2 - 200
    mov dword [rel anim_src_y], CANVAS_Y + CANVAS_H/2 - 150
    mov dword [rel anim_src_w], 400
    mov dword [rel anim_src_h], 300

    ; Also set current to source
    mov dword [rel anim_cur_x], CANVAS_X + CANVAS_W/2 - 200
    mov dword [rel anim_cur_y], CANVAS_Y + CANVAS_H/2 - 150
    mov dword [rel anim_cur_w], 400
    mov dword [rel anim_cur_h], 300

    ; Destination is original node rect
    lea rax, [rel node_rects]
    imul ebx, edi, 16
    add rax, rbx

    mov ecx, [rax]
    mov [rel anim_dst_x], ecx
    mov ecx, [rax + 4]
    mov [rel anim_dst_y], ecx
    mov ecx, [rax + 8]
    mov [rel anim_dst_w], ecx
    mov ecx, [rax + 12]
    mov [rel anim_dst_h], ecx

.no_collapse:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; update_animation — Advance animation by one frame
;; Returns: eax = 1 if animation complete, 0 if ongoing
update_animation:
    push rbx
    push r12
    push r13

    mov eax, [rel anim_state]
    test eax, eax
    jz .anim_done                   ; no animation

    ; Advance progress
    mov eax, [rel anim_progress]
    add eax, ANIM_STEP
    cmp eax, 255
    jle .not_done
    mov eax, 255
.not_done:
    mov [rel anim_progress], eax

    ; Apply ease-out-cubic: t' = 1 - (1-t)^3
    ; t is in [0,255], we compute (255-t)^3 / 255^2 then subtract from 255
    mov ebx, 255
    sub ebx, eax                    ; ebx = 255 - t
    imul ebx, ebx                   ; ebx = (255-t)^2
    imul eax, ebx, 1                ; preserve ebx
    mov eax, ebx
    mov ecx, [rel anim_progress]
    mov ebx, 255
    sub ebx, ecx
    imul eax, ebx                   ; eax = (255-t)^3
    ; Divide by 255^2 = 65025
    xor edx, edx
    mov ecx, 65025
    div ecx                         ; eax = (255-t)^3 / 65025
    mov ebx, 255
    sub ebx, eax                    ; ebx = eased progress (0-255)

    ; Interpolate each component: cur = src + (dst - src) * progress / 255
    ; X
    mov eax, [rel anim_dst_x]
    sub eax, [rel anim_src_x]
    imul eax, ebx
    sar eax, 8                      ; divide by 256 (close to 255)
    add eax, [rel anim_src_x]
    mov [rel anim_cur_x], eax

    ; Y
    mov eax, [rel anim_dst_y]
    sub eax, [rel anim_src_y]
    imul eax, ebx
    sar eax, 8
    add eax, [rel anim_src_y]
    mov [rel anim_cur_y], eax

    ; W
    mov eax, [rel anim_dst_w]
    sub eax, [rel anim_src_w]
    imul eax, ebx
    sar eax, 8
    add eax, [rel anim_src_w]
    mov [rel anim_cur_w], eax

    ; H
    mov eax, [rel anim_dst_h]
    sub eax, [rel anim_src_h]
    imul eax, ebx
    sar eax, 8
    add eax, [rel anim_src_h]
    mov [rel anim_cur_h], eax

    ; Check if complete
    mov eax, [rel anim_progress]
    cmp eax, 255
    jl .anim_ongoing

    ; Animation complete
    mov eax, [rel anim_state]
    cmp eax, ANIM_EXPANDING
    jne .collapse_done

    ; Expanding done - set focus_node
    mov eax, [rel anim_node]
    mov [rel focus_node], eax
    jmp .anim_finish

.collapse_done:
    ; Collapsing done - clear focus_node
    mov dword [rel focus_node], -1

.anim_finish:
    mov dword [rel anim_state], ANIM_NONE
    mov dword [rel anim_node], -1

.anim_done:
    mov eax, 1
    jmp .ret

.anim_ongoing:
    xor eax, eax

.ret:
    pop r13
    pop r12
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

    ; Check if UHMA running
    cmp dword [rel mcp_running], 0
    jne .file_have_uhma
    call mcp_spawn_uhma_feed        ; spawn with batch ON for feeding
    test eax, eax
    jz .error
.file_have_uhma:

    ; Check if user typed a path in input_buf
    mov eax, [rel input_len]
    test eax, eax
    jnz .file_from_input

    ; No path typed — open zenity file picker
    lea rdi, [rel zenity_file]
    call system
    test eax, eax
    jnz .error                      ; user cancelled

    ; Read picked path from /tmp/.uhma_pick
    lea rdi, [rel tmp_pick_path]
    lea rsi, [rel read_mode]
    call fopen
    test rax, rax
    jz .error
    mov r12, rax
    lea rdi, [rel file_path_buf]
    mov esi, 511
    mov rdx, r12
    call fgets
    push rax
    mov rdi, r12
    call fclose
    pop rax
    test rax, rax
    jz .error

    ; Strip trailing newline
    lea rdi, [rel file_path_buf]
    call strlen
    test eax, eax
    jz .error
    dec eax
    lea rdi, [rel file_path_buf]
    cmp byte [rdi + rax], 10
    jne .file_build_cmd
    mov byte [rdi + rax], 0
    jmp .file_build_cmd

.file_from_input:
    ; Copy input to file_path_buf and null-terminate
    lea rdi, [rel file_path_buf]
    lea rsi, [rel input_buf]
    mov ecx, [rel input_len]
    cmp ecx, 510
    jle .copy_ok
    mov ecx, 510
.copy_ok:
    rep movsb
    mov byte [rdi], 0

.file_build_cmd:

    ; Build "eat /path" command and send via gateway
    lea rdi, [rel eat_cmd_buf]
    lea rsi, [rel eat_cmd_pre]
    call strcpy
    lea rdi, [rel eat_cmd_buf]
    lea rsi, [rel file_path_buf]
    call strcat
    ; Send via gateway (FEED channel)
    xor edi, edi
    lea rsi, [rel eat_cmd_buf]
    call mcp_send_async

    ; Clear input after use
    mov dword [rel input_len], 0

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
;; load_dir — Load all files from directory (path from input_buf)
;; ============================================================
load_dir:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 528

    ; Check if UHMA running
    cmp dword [rel mcp_running], 0
    jne .dir_have_uhma
    call mcp_spawn_uhma_feed        ; spawn with batch ON for feeding
    test eax, eax
    jz .error
.dir_have_uhma:

    ; Check if user typed a path in input_buf
    mov eax, [rel input_len]
    test eax, eax
    jnz .dir_from_input

    ; No path typed — open zenity directory picker
    lea rdi, [rel zenity_dir]
    call system
    test eax, eax
    jnz .error                      ; user cancelled

    ; Read picked path from /tmp/.uhma_pick
    lea rdi, [rel tmp_pick_path]
    lea rsi, [rel read_mode]
    call fopen
    test rax, rax
    jz .error
    mov r12, rax
    lea rdi, [rel file_path_buf]
    mov esi, 511
    mov rdx, r12
    call fgets
    push rax
    mov rdi, r12
    call fclose
    pop rax
    test rax, rax
    jz .error

    ; Strip trailing newline
    lea rdi, [rel file_path_buf]
    call strlen
    test eax, eax
    jz .error
    dec eax
    lea rdi, [rel file_path_buf]
    cmp byte [rdi + rax], 10
    jne .dir_open
    mov byte [rdi + rax], 0
    jmp .dir_open

.dir_from_input:
    ; Copy input to file_path_buf and null-terminate
    lea rdi, [rel file_path_buf]
    lea rsi, [rel input_buf]
    mov ecx, [rel input_len]
    cmp ecx, 510
    jle .copy_ok
    mov ecx, 510
.copy_ok:
    rep movsb
    mov byte [rdi], 0

    ; Clear input after use
    mov dword [rel input_len], 0

.dir_open:

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
    ; Build "eat /path" command and send via gateway
    lea rdi, [rel eat_cmd_buf]
    lea rsi, [rel eat_cmd_pre]
    call strcpy
    lea rdi, [rel eat_cmd_buf]
    lea rsi, [rsp]              ; full file path
    call strcat
    ; Send via gateway (FEED channel)
    push r12
    push r13
    xor edi, edi
    lea rsi, [rel eat_cmd_buf]
    call mcp_send_async
    pop r13
    pop r12
    jmp .next

.dir_done:
    mov rdi, r12
    call closedir
    ; Kick a dream cycle after bulk ingest
    mov edi, 0
    lea rsi, [rel cmd_dream]
    call mcp_send_async

    lea rax, [rel msg_dir_ok]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 90
    jmp .done

.error:
    lea rax, [rel msg_file_err]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 90

.done:
    add rsp, 528
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; load_url — URL input and web fetch/digest
;; ============================================================
load_url:
    push rbx
    push r12
    push r13
    sub rsp, 8

    ; Ensure UHMA running
    cmp dword [rel mcp_running], 0
    jne .url_have_uhma
    call mcp_spawn_uhma_feed        ; batch ON for feeding
    test eax, eax
    jz .url_error
.url_have_uhma:

    ; Check if user typed a URL in input_buf
    mov eax, [rel input_len]
    test eax, eax
    jnz .url_from_input

    ; Use zenity to get URL
    lea rdi, [rel zenity_url]
    call system
    test eax, eax
    jnz .url_error

    ; Read URL from temp file
    lea rdi, [rel tmp_pick_path]
    lea rsi, [rel read_mode]
    call fopen
    test rax, rax
    jz .url_error
    mov rbx, rax

    lea rdi, [rel file_path_buf]
    mov esi, 511
    mov rdx, rbx
    call fgets
    mov r12, rax

    mov rdi, rbx
    call fclose

    test r12, r12
    jz .url_error
    jmp .url_strip

.url_from_input:
    ; Copy input to file_path_buf and null-terminate
    lea rdi, [rel file_path_buf]
    lea rsi, [rel input_buf]
    mov ecx, [rel input_len]
    cmp ecx, 510
    jle .url_copy_ok
    mov ecx, 510
.url_copy_ok:
    rep movsb
    mov byte [rdi], 0
    mov dword [rel input_len], 0

.url_strip:
    ; Strip newline from URL
    lea rdi, [rel file_path_buf]
    call strlen
    test rax, rax
    jz .url_error
    dec rax
    lea rdi, [rel file_path_buf]
    cmp byte [rdi + rax], 10
    jne .url_fetch
    mov byte [rdi + rax], 0

.url_fetch:
    ; Build curl command into eat_cmd_buf and fetch
    lea rdi, [rel eat_cmd_buf]
    lea rsi, [rel url_curl_pre]
    call strcpy
    lea rdi, [rel eat_cmd_buf]
    lea rsi, [rel file_path_buf]
    call strcat
    lea rdi, [rel eat_cmd_buf]
    lea rsi, [rel url_curl_post]
    call strcat

    lea rdi, [rel eat_cmd_buf]
    call system
    test eax, eax
    jz .url_fetched

    ; Fallback to wget
    lea rdi, [rel eat_cmd_buf]
    lea rsi, [rel url_wget_pre]
    call strcpy
    lea rdi, [rel eat_cmd_buf]
    lea rsi, [rel file_path_buf]
    call strcat
    lea rdi, [rel eat_cmd_buf]
    lea rsi, [rel url_wget_post]
    call strcat

    lea rdi, [rel eat_cmd_buf]
    call system
    test eax, eax
    jnz .url_error

.url_fetched:
    ; Send fetched file via eat
    lea rdi, [rel eat_cmd_buf]
    lea rsi, [rel eat_cmd_pre]
    call strcpy
    lea rdi, [rel eat_cmd_buf]
    lea rsi, [rel url_tmp_path]
    call strcat
    xor edi, edi
    lea rsi, [rel eat_cmd_buf]
    call mcp_send_async

    lea rax, [rel msg_url_ok]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 90
    jmp .url_done

.url_error:
    lea rax, [rel msg_url_err]
    mov [rel feedback_msg], rax
    mov dword [rel feedback_timer], 90

.url_done:
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; show_feed_config — Show zenity entry dialog, read command directly
;; Returns: eax = 1 if command built, 0 if cancelled
;; ============================================================
show_feed_config:
    push rbx
    push r12
    push r13
    sub rsp, 16                     ; 3 pushes (odd) -> aligned, sub must be multiple of 16

    ; Show zenity entry dialog
    lea rdi, [rel zenity_feed]
    call system
    mov r13d, eax                   ; save result
    test r13d, r13d
    jnz .cancelled                  ; user pressed Cancel (non-zero = error/cancel)

    ; Read command from temp file
    lea rdi, [rel tmp_feed_path]
    lea rsi, [rel read_mode]
    call fopen
    test rax, rax
    jz .cancelled
    mov rbx, rax                    ; file handle

    lea rdi, [rel feed_cmd_buf]
    mov esi, 500
    mov rdx, rbx
    call fgets
    mov r12, rax                    ; save result

    mov rdi, rbx
    call fclose

    test r12, r12
    jz .cancelled                   ; fgets failed

    ; Strip newline if present
    lea rdi, [rel feed_cmd_buf]
    call strlen
    test rax, rax
    jz .cancelled                   ; empty string

    ; Check last char for newline (reload rdi since it's caller-saved)
    lea rdi, [rel feed_cmd_buf]
    dec rax                         ; rax = length - 1 (index of last char)
    cmp byte [rdi + rax], 10
    jne .no_strip
    mov byte [rdi + rax], 0
.no_strip:

    ; Append " &" for background execution
    lea rdi, [rel feed_cmd_buf]
    lea rsi, [rel .background]
    call strcat

    mov eax, 1
    jmp .done

.cancelled:
    xor eax, eax

.done:
    add rsp, 16
    pop r13
    pop r12
    pop rbx
    ret

.background:        db " &", 0

;; ============================================================
;; copy_ring_to_clipboard
;; rdi = ring buffer pointer (8192 bytes)
;; Copies content to /tmp/uhma_clip.txt then to X clipboard via xclip
;; ============================================================
copy_ring_to_clipboard:
    push rbx
    push r12
    sub rsp, 8                      ; 2 pushes (even) = unaligned, need 8 mod 16

    mov r12, rdi                    ; save ring buffer pointer

    ; Open temp file for writing
    lea rdi, [rel clip_tmpfile]
    lea rsi, [rel clip_fmode]
    call fopen
    test rax, rax
    jz .clip_done                   ; failed to open
    mov rbx, rax                    ; save FILE*

    ; Write ring buffer content (8192 bytes)
    ; fwrite(ptr, size, count, stream)
    mov rdi, r12                    ; buffer
    mov esi, 1                      ; size = 1
    mov edx, RING_SIZE              ; count = 8192
    mov rcx, rbx                    ; stream
    call fwrite

    ; Flush and close
    mov rdi, rbx
    call fflush
    mov rdi, rbx
    call fclose

    ; Copy to X clipboard via xclip
    lea rdi, [rel clip_xclip]
    call system

.clip_done:
    add rsp, 8
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
