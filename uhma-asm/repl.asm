; repl.asm — Interactive command loop
%include "syscalls.inc"
%include "constants.inc"

section .data
    prompt_str:     db "uhma> ", 0
    prompt_len      equ 6
    banner_str:     db "UHMA x86_64 — Unified Holographic Memory Architecture", 10
                    db "Surface: 8GB RWX | VSA: 1024-dim f64 | Self-modifying dispatch", 10
                    db "Type 'help' for commands, text to process, or Ctrl-D to exit.", 10, 10, 0
    help_str:       db "Commands:", 10
                    db "  help          Show this help", 10
                    db "  status        Show system status (regions, accuracy, drives)", 10
                    db "  self          Show self-knowledge (strengths/weaknesses)", 10
                    db "  metacog       Show metacognitive state (per-context confidence)", 10
                    db "  debugger      Show self-debugger status (breakpoints, learning events)", 10
                    db "  genes         Show gene pool status (composted patterns)", 10
                    db "  subroutines   Show shared subroutines (recursive schemas)", 10
                    db "  regions       List all regions with hit/miss stats", 10
                    db "  presence      Show presence field values", 10
                    db "  drives        Show drive levels and thresholds", 10
                    db "  observe       Trigger observation cycle", 10
                    db "  dream         Trigger dream/consolidation cycle", 10
                    db "  compact       Compact condemned regions", 10
                    db "  save <file>   Save surface to file", 10
                    db "  load <file>   Restore surface from file", 10
                    db "  eat <file>    Digest file as food (extract tokens, gain energy)", 10
                    db "  hive          Show hive pheromone levels (swarm intelligence)", 10
                    db "  share         Enable shared VSA (Mycorrhiza collective consciousness)", 10
                    db "  colony        Show colony status (hive mind instances)", 10
                    db "  export <n>    Export region n as .gene file (spore)", 10
                    db "  import <file> Import .gene file (infect with culture)", 10
                    db "  reset         Reset counters (not knowledge)", 10
                    db "  trace         Toggle debug tracing on/off", 10
                    db "  quit          Exit", 10
                    db "  <text>        Process as token sequence", 10, 0
    debugger_hdr:   db "--- Self-Debugger ---", 10, 0
    debugger_bp:    db "  Breakpoints: ", 0
    debugger_hits:  db "  Total hits: ", 0
    debugger_learn: db "  Learning events: ", 0
    bye_str:        db "Surface frozen. Goodbye.", 10, 0
    trace_next_msg: db "[JOURNEY] Will trace next token. Type text to trace, 'trace' to show.", 10, 0
    unknown_str:    db "Unknown command. Type 'help'.", 10, 0
    status_hdr:     db "--- Status ---", 10, 0
    hive_hdr:       db "--- Hive Mind (Pheromone Levels) ---", 10, 0
    hive_dream:     db "  Dream pheromone:   ", 0
    hive_observe:   db "  Observe pheromone: ", 0
    hive_evolve:    db "  Evolve pheromone:  ", 0
    hive_fatigue:   db "  Fatigue:           ", 0
    hive_thresh:    db "  Activation threshold: 0.5", 10, 0
    colony_hdr:     db "--- Mycorrhiza Colony ---", 10, 0
    colony_mode:    db "  Mode: ", 0
    colony_solo:    db "SOLO (isolated)", 10, 0
    colony_shared:  db "SHARED (hive mind)", 10, 0
    colony_size:    db "  Colony size: ", 0
    colony_valence: db "  Collective valence: ", 0
    colony_instance:db "  Instance ID: 0x", 0
    geom_hdr:       db "--- Rosetta Stone (Geometric Gate) ---", 10, 0
    geom_mode_lbl:  db "  Verification mode: ", 0
    geom_mode_0:    db "ABSTRACT (traditional)", 10, 0
    geom_mode_1:    db "GEOMETRIC (vector-based)", 10, 0
    geom_mode_2:    db "BOTH (most secure)", 10, 0
    geom_status:    db "  Safety templates: ", 0
    geom_init_yes:  db "initialized", 10, 0
    geom_init_no:   db "not initialized", 10, 0
    geom_mode_set_msg: db "  Verification mode set to: ", 0
    geom_usage:     db "  Usage: geom [0|1|2] to set mode", 10, 0
    regions_lbl:    db "Regions: ", 0
    steps_lbl:      db "  Steps: ", 0
    accuracy_lbl:   db "  Accuracy: ", 0
    faults_lbl:     db "  Faults: ", 0
    dispatch_lbl:   db "  Dispatch ptr: ", 0
    nl:             db 10

section .bss
    input_buf:      resb INPUT_BUF_SIZE
    buf_pos:        resq 1            ; current read position in buffer
    buf_end:        resq 1            ; end of valid data in buffer

section .text

extern print_cstr
extern print_str
extern print_u64
extern print_hex64
extern print_f32
extern print_newline
extern print_space
extern read_stdin
extern write_stdout
extern process_input
extern observe_cycle
extern dream_cycle
extern digest_file
extern surface_init_shared
extern get_colony_size
extern is_shared_mode
extern sense_collective_valence
extern gene_export
extern gene_import
extern region_compact
extern get_fault_count
extern persist_save
extern persist_load
extern drives_show
extern presence_show
extern vocab_count
extern holo_dot_f64
extern holo_magnitude_f64
extern fault_safe_rsp
extern fault_safe_rip
extern print_f64
extern verify_set_mode
extern verify_get_mode
extern init_safety_vectors
extern get_safety_template
extern get_danger_template
extern encode_code_to_vector
extern check_code_safety
extern journey_start
extern journey_stop
extern journey_dump
extern metacog_report
extern gene_pool_show
extern subroutines_show       ; from factor.asm - show subroutine table

;; ============================================================
;; repl_run
;; Main interactive loop. Never returns (exits via quit or EOF).
;; ============================================================
global repl_run
repl_run:
    push rbx
    push r12

    ; Print banner
    lea rdi, [rel banner_str]
    call print_cstr

.loop:
    ; Save recovery point for crash handler (longjmp target)
    mov [rel fault_safe_rsp], rsp
    lea rax, [rel .loop]
    mov [rel fault_safe_rip], rax

    ; Check if there's remaining data in the buffer
    mov rax, [rel buf_pos]
    mov rcx, [rel buf_end]
    cmp rax, rcx
    jl .have_data

    ; No data in buffer — print prompt and read new input
    lea rdi, [rel prompt_str]
    mov rsi, prompt_len
    call print_str

    lea rdi, [rel input_buf]
    mov rsi, INPUT_BUF_SIZE - 1
    call read_stdin

    ; Check EOF
    test rax, rax
    jle .quit

    ; Set buffer bounds
    lea rcx, [rel input_buf]
    mov [rel buf_pos], rcx
    add rcx, rax
    mov [rel buf_end], rcx

.have_data:
    ; Extract one line from buffer (up to \n or end)
    mov rbx, [rel buf_pos]    ; start of this line
    mov rcx, [rel buf_end]

    ; Find newline or end
    mov rdi, rbx
.find_nl:
    cmp rdi, rcx
    jge .line_at_end
    cmp byte [rdi], 10
    je .found_nl
    inc rdi
    jmp .find_nl

.found_nl:
    ; Replace \n with \0, advance buf_pos past it
    mov byte [rdi], 0
    inc rdi
    mov [rel buf_pos], rdi
    jmp .have_line

.line_at_end:
    ; No newline found — null-terminate at end
    mov byte [rdi], 0
    mov [rel buf_pos], rdi    ; buf_pos = buf_end → will read next time

.have_line:
    ; rbx points to null-terminated line
    ; Skip empty lines
    cmp byte [rbx], 0
    je .loop

    ; Check for commands
    ; "quit" or "exit"
    cmp dword [rbx], 'quit'
    je .quit
    cmp dword [rbx], 'exit'
    je .quit

    ; "help"
    cmp dword [rbx], 'help'
    je .cmd_help

    ; "status" (full 6-char match + boundary)
    mov eax, [rbx]
    cmp eax, 'stat'
    jne .not_status
    cmp word [rbx + 4], 'us'
    jne .not_status
    movzx eax, byte [rbx + 6]
    test eax, eax
    jz .cmd_status
    cmp eax, ' '
    je .cmd_status
    cmp eax, 10
    je .cmd_status
    jmp .not_status
.not_status:

    ; "regions" (full 7-char match + boundary)
    cmp dword [rbx], 'regi'
    jne .not_regions
    cmp word [rbx + 4], 'on'
    jne .not_regions
    cmp byte [rbx + 6], 's'
    jne .not_regions
    movzx eax, byte [rbx + 7]
    test eax, eax
    jz .cmd_regions
    cmp eax, ' '
    je .cmd_regions
    jmp .not_regions
.not_regions:

    ; "presence" (full 8-char match + boundary)
    cmp dword [rbx], 'pres'
    jne .not_presence
    cmp dword [rbx + 4], 'ence'
    jne .not_presence
    movzx eax, byte [rbx + 8]
    test eax, eax
    jz .cmd_presence
    cmp eax, ' '
    je .cmd_presence
    jmp .not_presence
.not_presence:

    ; "drives" (full 6-char match + boundary)
    cmp dword [rbx], 'driv'
    jne .not_drives
    cmp word [rbx + 4], 'es'
    jne .not_drives
    movzx eax, byte [rbx + 6]
    test eax, eax
    jz .cmd_drives
    cmp eax, ' '
    je .cmd_drives
    jmp .not_drives
.not_drives:

    ; "observe" (full 7-char match + boundary)
    cmp dword [rbx], 'obse'
    jne .not_observe
    cmp word [rbx + 4], 'rv'
    jne .not_observe
    cmp byte [rbx + 6], 'e'
    jne .not_observe
    movzx eax, byte [rbx + 7]
    test eax, eax
    jz .cmd_observe
    cmp eax, ' '
    je .cmd_observe
    cmp eax, 10
    je .cmd_observe
    jmp .not_observe
.not_observe:

    ; "dream" (full 5-char match + boundary)
    cmp dword [rbx], 'drea'
    jne .not_dream
    cmp byte [rbx + 4], 'm'
    jne .not_dream
    movzx eax, byte [rbx + 5]
    test eax, eax
    jz .cmd_dream
    cmp eax, ' '
    je .cmd_dream
    cmp eax, 10
    je .cmd_dream
    jmp .not_dream
.not_dream:

    ; "compact" (full 7-char match + boundary)
    cmp dword [rbx], 'comp'
    jne .not_compact
    cmp word [rbx + 4], 'ac'
    jne .not_compact
    cmp byte [rbx + 6], 't'
    jne .not_compact
    movzx eax, byte [rbx + 7]
    test eax, eax
    jz .cmd_compact
    cmp eax, ' '
    je .cmd_compact
    jmp .not_compact
.not_compact:

    ; "save"
    cmp dword [rbx], 'save'
    jne .not_save
    jmp .cmd_save
.not_save:

    ; "load"
    cmp dword [rbx], 'load'
    jne .not_load
    jmp .cmd_load
.not_load:

    ; "eat <file>" (digest file as food)
    cmp dword [rbx], 'eat '
    jne .not_eat
    jmp .cmd_eat
.not_eat:

    ; "hive" (show hive pheromone levels)
    cmp dword [rbx], 'hive'
    jne .not_hive
    movzx eax, byte [rbx + 4]
    test eax, eax
    jz .cmd_hive
    cmp eax, 10
    je .cmd_hive
    jmp .not_hive
.not_hive:

    ; "geom" (show geometric gate status)
    cmp dword [rbx], 'geom'
    jne .not_geom
    movzx eax, byte [rbx + 4]
    test eax, eax
    jz .cmd_geom
    cmp eax, 10
    je .cmd_geom
    cmp eax, ' '
    jne .not_geom
    jmp .cmd_geom_arg         ; geom with argument (mode setting)
.not_geom:

    ; "share" (enable shared consciousness)
    cmp dword [rbx], 'shar'
    jne .not_share
    cmp byte [rbx + 4], 'e'
    jne .not_share
    movzx eax, byte [rbx + 5]
    test eax, eax
    jz .cmd_share
    cmp eax, 10
    je .cmd_share
    jmp .not_share
.not_share:

    ; "colony" (show colony status)
    cmp dword [rbx], 'colo'
    jne .not_colony
    cmp word [rbx + 4], 'ny'
    jne .not_colony
    movzx eax, byte [rbx + 6]
    test eax, eax
    jz .cmd_colony
    cmp eax, 10
    je .cmd_colony
    jmp .not_colony
.not_colony:

    ; "export <n>" (export region as gene)
    cmp dword [rbx], 'expo'
    jne .not_export
    cmp word [rbx + 4], 'rt'
    jne .not_export
    cmp byte [rbx + 6], ' '
    jne .not_export
    jmp .cmd_export
.not_export:

    ; "import <file>" (import gene file)
    cmp dword [rbx], 'impo'
    jne .not_import
    cmp word [rbx + 4], 'rt'
    jne .not_import
    cmp byte [rbx + 6], ' '
    jne .not_import
    jmp .cmd_import
.not_import:

    ; "reset"
    cmp dword [rbx], 'rese'
    jne .not_reset
    jmp .cmd_reset
.not_reset:

    ; "self"
    cmp dword [rbx], 'self'
    jne .not_self
    cmp byte [rbx + 4], 0
    jne .not_self
    jmp .cmd_self
.not_self:

    ; "trace" (toggle tracing on/off)
    cmp dword [rbx], 'trac'
    jne .not_trace
    cmp byte [rbx + 4], 'e'
    jne .not_trace
    movzx eax, byte [rbx + 5]
    test eax, eax
    jz .cmd_trace
    cmp eax, ' '
    je .cmd_trace
    cmp eax, 10
    je .cmd_trace
    jmp .not_trace
.not_trace:

    ; "metacog" (7-char match: m-e-t-a-c-o-g)
    cmp dword [rbx], 'meta'
    jne .not_metacog
    cmp word [rbx + 4], 'co'
    jne .not_metacog
    cmp byte [rbx + 6], 'g'
    jne .not_metacog
    movzx eax, byte [rbx + 7]
    test eax, eax
    jz .cmd_metacog
    cmp eax, ' '
    je .cmd_metacog
    cmp eax, 10
    je .cmd_metacog
    jmp .not_metacog
.not_metacog:

    ; "debugger" (8-char match: d-e-b-u-g-g-e-r)
    cmp dword [rbx], 'debu'
    jne .not_debugger
    cmp dword [rbx + 4], 'gger'
    jne .not_debugger
    movzx eax, byte [rbx + 8]
    test eax, eax
    jz .cmd_debugger
    cmp eax, ' '
    je .cmd_debugger
    cmp eax, 10
    je .cmd_debugger
    jmp .not_debugger
.not_debugger:

    ; "genes" (5-char match: g-e-n-e-s)
    cmp dword [rbx], 'gene'
    jne .not_genes
    cmp byte [rbx + 4], 's'
    jne .not_genes
    movzx eax, byte [rbx + 5]
    test eax, eax
    jz .cmd_genes
    cmp eax, ' '
    je .cmd_genes
    cmp eax, 10
    je .cmd_genes
    jmp .not_genes
.not_genes:

    ; "subroutines" (11-char match: s-u-b-r-o-u-t-i-n-e-s)
    cmp dword [rbx], 'subr'
    jne .not_subroutines
    cmp dword [rbx + 4], 'outi'
    jne .not_subroutines
    cmp word [rbx + 8], 'ne'
    jne .not_subroutines
    cmp byte [rbx + 10], 's'
    jne .not_subroutines
    movzx eax, byte [rbx + 11]
    test eax, eax
    jz .cmd_subroutines
    cmp eax, ' '
    je .cmd_subroutines
    cmp eax, 10
    je .cmd_subroutines
    jmp .not_subroutines
.not_subroutines:

    ; Not a command → process as text input
    ; Compute string length (rbx is null-terminated)
    mov rdi, rbx
    xor rsi, rsi
.strlen:
    cmp byte [rbx + rsi], 0
    je .strlen_done
    inc rsi
    jmp .strlen
.strlen_done:
    call process_input
    jmp .loop

.cmd_help:
    lea rdi, [rel help_str]
    call print_cstr
    jmp .loop

.cmd_status:
    call repl_show_status
    jmp .loop

.cmd_regions:
    call repl_show_regions
    jmp .loop

.cmd_presence:
    call presence_show
    jmp .loop

.cmd_drives:
    call drives_show
    jmp .loop

.cmd_observe:
    call observe_cycle
    jmp .loop

.cmd_dream:
    call dream_cycle
    jmp .loop

.cmd_compact:
    call region_compact
    push rax
    lea rdi, [rel compact_msg]
    call print_cstr
    pop rdi
    call print_u64
    call print_newline
    jmp .loop

.cmd_save:
    ; Get filename after "save "
    lea rdi, [rbx + 5]
    call persist_save
    jmp .loop

.cmd_load:
    lea rdi, [rbx + 5]
    call persist_load
    jmp .loop

.cmd_eat:
    ; Digest file as "food" — extract tokens and gain energy
    ; Strip trailing newline from filename
    lea rdi, [rbx + 4]        ; skip "eat "
    mov rsi, rdi
.eat_strip:
    mov al, [rsi]
    test al, al
    jz .eat_stripped
    cmp al, 10
    je .eat_null_term
    cmp al, 13
    je .eat_null_term
    inc rsi
    jmp .eat_strip
.eat_null_term:
    mov byte [rsi], 0
.eat_stripped:
    call digest_file
    jmp .loop

.cmd_hive:
    ; Show hive pheromone levels (swarm intelligence state)
    call repl_show_hive
    jmp .loop

.cmd_geom:
    ; Show geometric gate status (Rosetta Stone)
    call repl_show_geom
    jmp .loop

.cmd_geom_arg:
    ; Set geometric verification mode: geom 0/1/2
    lea rdi, [rbx + 5]        ; skip "geom "
    call parse_decimal        ; eax = mode number
    mov edi, eax
    push rdi                  ; save mode for printing
    call verify_set_mode
    lea rdi, [rel geom_mode_set_msg]
    call print_cstr
    pop rdi                   ; restore mode
    call print_u64
    call print_newline
    jmp .loop

.cmd_share:
    ; Enable shared consciousness (Mycorrhiza)
    xor edi, edi              ; mode 0 = create new
    call surface_init_shared
    jmp .loop

.cmd_colony:
    ; Show colony status
    call repl_show_colony
    jmp .loop

.cmd_export:
    ; Export region as .gene file
    ; Parse region index from "export <n>"
    lea rdi, [rbx + 7]        ; skip "export "
    call parse_decimal        ; → eax = region index
    push rax

    ; Get region table entry
    mov rcx, SURFACE_BASE
    lea rsi, [rcx + REGION_TABLE_OFFSET]
    pop rax
    imul rax, rax, RTE_SIZE
    add rax, rsi
    mov rdi, [rax + RTE_ADDR] ; region header ptr

    ; Build filename: /tmp/gene_<index>.gene
    sub rsp, 32
    mov dword [rsp], '/tmp'
    mov dword [rsp + 4], '/gen'
    mov dword [rsp + 8], 'e.ge'
    mov word [rsp + 12], 'ne'
    mov byte [rsp + 14], 0
    lea rsi, [rsp]
    call gene_export
    add rsp, 32
    jmp .loop

.cmd_import:
    ; Import .gene file
    lea rdi, [rbx + 7]        ; skip "import "
    ; Strip trailing newline
    mov rsi, rdi
.import_strip:
    mov al, [rsi]
    test al, al
    jz .import_stripped
    cmp al, 10
    je .import_null
    inc rsi
    jmp .import_strip
.import_null:
    mov byte [rsi], 0
.import_stripped:
    call gene_import
    jmp .loop

.cmd_reset:
    call repl_reset_counters
    jmp .loop

.cmd_self:
    call repl_show_self
    jmp .loop

.cmd_trace:
    ; Toggle journey: if tracing, show journey and stop. If not, trace next token.
    mov rax, SURFACE_BASE
    cmp dword [rax + STATE_OFFSET + ST_JOURNEY_TOKEN], 0
    je .trace_next
    ; Currently tracing - dump and stop
    call journey_dump
    call journey_stop
    jmp .loop
.trace_next:
    ; Set flag to trace next token (0xFFFFFFFF means "trace next")
    mov dword [rax + STATE_OFFSET + ST_JOURNEY_TOKEN], 0xFFFFFFFF
    lea rdi, [rel trace_next_msg]
    call print_cstr
    jmp .loop

.cmd_metacog:
    ; Show metacognitive state for last prediction context
    ; ST_LAST_CTX holds the context from the last prediction (where hit/miss was recorded)
    mov rax, SURFACE_BASE
    mov edi, [rax + STATE_OFFSET + ST_LAST_CTX]  ; get last prediction context
    call metacog_report
    jmp .loop

.cmd_debugger:
    ; Show self-debugger status
    lea rdi, [rel debugger_hdr]
    call print_cstr

    lea rdi, [rel debugger_bp]
    call print_cstr
    mov rax, SURFACE_BASE
    mov edi, [rax + STATE_OFFSET + ST_BREAKPOINT_COUNT]
    call print_u64
    call print_newline

    lea rdi, [rel debugger_hits]
    call print_cstr
    mov rax, SURFACE_BASE
    mov edi, [rax + STATE_OFFSET + ST_BP_TOTAL_HITS]
    call print_u64
    call print_newline

    lea rdi, [rel debugger_learn]
    call print_cstr
    mov rax, SURFACE_BASE
    mov edi, [rax + STATE_OFFSET + ST_BP_LEARNING_EVENTS]
    call print_u64
    call print_newline
    jmp .loop

.cmd_genes:
    ; Show gene pool status
    call gene_pool_show
    jmp .loop

.cmd_subroutines:
    ; Show subroutine table (recursive schema hierarchy)
    call subroutines_show
    jmp .loop

.quit:
    lea rdi, [rel bye_str]
    call print_cstr
    xor edi, edi
    mov rax, SYS_EXIT
    syscall

;; ============================================================
;; repl_show_status — Print system overview
;; ============================================================
repl_show_status:
    push rbx
    mov rbx, SURFACE_BASE

    lea rdi, [rel status_hdr]
    call print_cstr

    ; Region count
    lea rdi, [rel regions_lbl]
    call print_cstr
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov rdi, [rax]
    call print_u64
    call print_newline

    ; Global step
    lea rdi, [rel steps_lbl]
    call print_cstr
    lea rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov rdi, [rax]
    call print_u64
    call print_newline

    ; Compute overall accuracy
    lea rdi, [rel accuracy_lbl]
    call print_cstr
    call compute_total_accuracy
    call print_f32
    call print_newline

    ; Fault count
    lea rdi, [rel faults_lbl]
    call print_cstr
    call get_fault_count
    mov rdi, rax
    call print_u64
    call print_newline

    ; Dispatch pointer
    lea rdi, [rel dispatch_lbl]
    call print_cstr
    lea rax, [rbx + STATE_OFFSET + ST_DISPATCH_PTR]
    mov rdi, [rax]
    call print_hex64
    call print_newline

    ; Introspective state
    lea rdi, [rel intro_lbl]
    call print_cstr
    lea rax, [rbx + STATE_OFFSET + ST_INTRO_STATE]
    mov edi, [rax]
    call print_intro_state
    call print_newline

    ; Self-prediction accuracy
    lea rdi, [rel selfpred_lbl]
    call print_cstr
    lea rax, [rbx + STATE_OFFSET + ST_SELF_PRED_HITS]
    mov edi, [rax]
    call print_u64
    lea rdi, [rel slash_char]
    call print_cstr
    lea rax, [rbx + STATE_OFFSET + ST_SELF_PRED_MISSES]
    mov edi, [rax]
    add edi, [rbx + STATE_OFFSET + ST_SELF_PRED_HITS]
    call print_u64
    call print_newline

    ; Schema coverage
    lea rdi, [rel schema_lbl]
    call print_cstr
    lea rax, [rbx + STATE_OFFSET + ST_SCHEMA_HITS]
    mov edi, [rax]
    call print_u64
    lea rdi, [rel slash_char]
    call print_cstr
    lea rax, [rbx + STATE_OFFSET + ST_SCHEMA_TOTAL]
    mov edi, [rax]
    call print_u64
    call print_newline

    ; Dispatch mode
    lea rdi, [rel dmode_lbl]
    call print_cstr
    lea rax, [rbx + STATE_OFFSET + ST_DISPATCH_MODE]
    mov edi, [rax]
    call print_dispatch_mode
    call print_newline

    ; Last surprise type
    lea rdi, [rel surprise_lbl]
    call print_cstr
    lea rax, [rbx + STATE_OFFSET + ST_SURPRISE_TYPE]
    mov edi, [rax]
    call print_surprise_type
    call print_newline

    ; Causal records
    lea rdi, [rel causal_lbl]
    call print_cstr
    lea rax, [rbx + STATE_OFFSET + ST_CAUSAL_COUNT]
    mov edi, [rax]
    call print_u64
    call print_newline

    ; Hypotheses (NURSERY-flagged regions)
    lea rdi, [rel hypo_lbl]
    call print_cstr
    call count_nursery_regions ; → eax
    mov edi, eax
    call print_u64
    call print_newline

    ; Trace (last dispatch scan)
    lea rdi, [rel trace_lbl]
    call print_cstr
    mov edi, [rbx + STATE_OFFSET + ST_TRACE_CANDIDATES]
    call print_u64
    lea rdi, [rel trace_sep]
    call print_cstr
    mov edi, [rbx + STATE_OFFSET + ST_TRACE_MATCHED]
    call print_u64
    call print_newline

    ; Goal
    lea rdi, [rel goal_lbl]
    call print_cstr
    mov edi, [rbx + STATE_OFFSET + ST_CURRENT_GOAL]
    call print_goal
    call print_newline

    ; Accuracy variance
    lea rdi, [rel variance_lbl]
    call print_cstr
    movss xmm0, [rbx + STATE_OFFSET + ST_ACCURACY_VARIANCE]
    call print_f32
    call print_newline

    ; Expectation bundle
    lea rdi, [rel expect_lbl]
    call print_cstr
    mov rax, [rbx + STATE_OFFSET + ST_EXPECT_REGION]
    test rax, rax
    jz .no_expect
    ; Print confidence
    movss xmm0, [rbx + STATE_OFFSET + ST_EXPECT_CONF]
    call print_f32
    lea rdi, [rel expect_schema]
    mov eax, [rbx + STATE_OFFSET + ST_EXPECT_IS_SCHEMA]
    test eax, eax
    jz .expect_not_schema
    call print_cstr
.expect_not_schema:
    jmp .expect_done
.no_expect:
    lea rdi, [rel expect_none]
    call print_cstr
.expect_done:
    call print_newline

    ; --- Holographic Memory Stats (f64 precision) ---
    ; Vocabulary count
    lea rdi, [rel holo_vocab_lbl]
    call print_cstr
    call vocab_count
    mov edi, eax
    call print_u64
    call print_newline

    ; Holo density: average magnitude of first 16 traces (f64 vectors)
    lea rdi, [rel holo_density_lbl]
    call print_cstr
    sub rsp, 16               ; [rsp]=sum(f64), [rsp+8]=counter
    xorpd xmm0, xmm0
    movsd [rsp], xmm0         ; sum = 0.0
    mov dword [rsp + 8], 0    ; counter
.holo_density_loop:
    cmp dword [rsp + 8], 16
    jge .holo_density_done
    push rbx                  ; save across call
    mov eax, [rsp + 16]       ; counter (+8 for pushed rbx)
    imul rax, rax, HOLO_VEC_BYTES
    lea rdi, [rbx + HOLO_OFFSET]
    add rdi, rax              ; trace ptr (f64[1024])
    call holo_magnitude_f64   ; → xmm0 (f64)
    pop rbx
    addsd xmm0, [rsp]
    movsd [rsp], xmm0
    inc dword [rsp + 8]
    jmp .holo_density_loop
.holo_density_done:
    movsd xmm0, [rsp]
    mov eax, 16
    cvtsi2sd xmm1, eax
    divsd xmm0, xmm1         ; avg magnitude (f64)
    cvtsd2ss xmm0, xmm0      ; convert to f32 for print_f32
    add rsp, 16
    call print_f32
    call print_newline

    ; Holo confidence: average prediction confidence (f64)
    lea rdi, [rel holo_conf_lbl]
    call print_cstr
    mov eax, [rbx + STATE_OFFSET + ST_HOLO_PREDICT_N]
    test eax, eax
    jz .holo_no_conf
    movsd xmm0, [rbx + STATE_OFFSET + ST_HOLO_PREDICT_SUM]
    cvtsi2sd xmm1, eax
    divsd xmm0, xmm1         ; avg confidence (f64)
    cvtsd2ss xmm0, xmm0      ; convert to f32 for print
    jmp .holo_print_conf
.holo_no_conf:
    xorps xmm0, xmm0
.holo_print_conf:
    call print_f32
    lea rdi, [rel holo_conf_n_lbl]
    call print_cstr
    mov edi, [rbx + STATE_OFFSET + ST_HOLO_PREDICT_N]
    call print_u64
    lea rdi, [rel holo_conf_end]
    call print_cstr
    call print_newline

    ; --- Graph Dynamics Stats ---
    call show_graph_stats

    pop rbx
    ret

;; ============================================================
;; repl_show_hive — Show hive pheromone levels (swarm intelligence)
;; ============================================================
repl_show_hive:
    push rbx
    mov rbx, SURFACE_BASE

    lea rdi, [rel hive_hdr]
    call print_cstr

    ; Dream pheromone
    lea rdi, [rel hive_dream]
    call print_cstr
    movsd xmm0, [rbx + STATE_OFFSET + ST_DREAM_PRESSURE]
    cvtsd2ss xmm0, xmm0
    call print_f32
    call print_newline

    ; Observe pheromone
    lea rdi, [rel hive_observe]
    call print_cstr
    movsd xmm0, [rbx + STATE_OFFSET + ST_OBSERVE_PRESSURE]
    cvtsd2ss xmm0, xmm0
    call print_f32
    call print_newline

    ; Evolve pheromone
    lea rdi, [rel hive_evolve]
    call print_cstr
    movsd xmm0, [rbx + STATE_OFFSET + ST_EVOLVE_PRESSURE]
    cvtsd2ss xmm0, xmm0
    call print_f32
    call print_newline

    ; Fatigue
    lea rdi, [rel hive_fatigue]
    call print_cstr
    movss xmm0, [rbx + STATE_OFFSET + ST_PRESENCE + PRES_FATIGUE * 4]
    call print_f32
    call print_newline

    ; Threshold
    lea rdi, [rel hive_thresh]
    call print_cstr

    pop rbx
    ret

;; ============================================================
;; repl_show_colony — Show Mycorrhiza colony status
;; ============================================================
repl_show_colony:
    push rbx
    mov rbx, SURFACE_BASE

    lea rdi, [rel colony_hdr]
    call print_cstr

    ; Mode
    lea rdi, [rel colony_mode]
    call print_cstr
    call is_shared_mode
    test eax, eax
    jz .colony_is_solo
    lea rdi, [rel colony_shared]
    jmp .colony_print_mode
.colony_is_solo:
    lea rdi, [rel colony_solo]
.colony_print_mode:
    call print_cstr

    ; Colony size
    lea rdi, [rel colony_size]
    call print_cstr
    call get_colony_size
    mov rdi, rax
    call print_u64
    call print_newline

    ; Collective valence (only if shared)
    call is_shared_mode
    test eax, eax
    jz .colony_skip_valence
    lea rdi, [rel colony_valence]
    call print_cstr
    call sense_collective_valence
    cvtsd2ss xmm0, xmm0
    call print_f32
    call print_newline

.colony_skip_valence:
    ; Instance ID
    lea rdi, [rel colony_instance]
    call print_cstr
    mov rdi, [rbx + STATE_OFFSET + ST_INSTANCE_ID]
    call print_hex64
    call print_newline

    pop rbx
    ret

;; ============================================================
;; repl_show_geom — Show Rosetta Stone (Geometric Gate) status
;; ============================================================
repl_show_geom:
    push rbx

    lea rdi, [rel geom_hdr]
    call print_cstr

    ; Verification mode
    lea rdi, [rel geom_mode_lbl]
    call print_cstr

    call verify_get_mode
    test eax, eax
    jz .geom_mode_0
    cmp eax, 1
    je .geom_mode_1
    cmp eax, 2
    je .geom_mode_2
    jmp .geom_mode_unknown

.geom_mode_0:
    lea rdi, [rel geom_mode_0]
    jmp .geom_print_mode

.geom_mode_1:
    lea rdi, [rel geom_mode_1]
    jmp .geom_print_mode

.geom_mode_2:
    lea rdi, [rel geom_mode_2]
    jmp .geom_print_mode

.geom_mode_unknown:
    lea rdi, [rel geom_mode_0]      ; default

.geom_print_mode:
    call print_cstr

    ; Safety template status
    lea rdi, [rel geom_status]
    call print_cstr

    ; Check if safety vectors are initialized by calling init
    ; (it's idempotent - won't reinitialize if already done)
    call init_safety_vectors
    call get_safety_template
    test rax, rax
    jz .geom_not_init
    lea rdi, [rel geom_init_yes]
    jmp .geom_print_init
.geom_not_init:
    lea rdi, [rel geom_init_no]
.geom_print_init:
    call print_cstr

    ; Usage hint
    lea rdi, [rel geom_usage]
    call print_cstr

    pop rbx
    ret

;; ============================================================
;; parse_decimal(str) -> eax (parsed number)
;; rdi=null-terminated decimal string
;; Returns the parsed decimal number
;; ============================================================
parse_decimal:
    xor eax, eax              ; result
    xor ecx, ecx              ; digit
.parse_loop:
    movzx ecx, byte [rdi]
    test ecx, ecx
    jz .parse_done
    cmp ecx, 10               ; newline
    je .parse_done
    cmp ecx, ' '
    je .parse_done
    sub ecx, '0'
    js .parse_done
    cmp ecx, 9
    ja .parse_done
    imul eax, eax, 10
    add eax, ecx
    inc rdi
    jmp .parse_loop
.parse_done:
    ret

;; ============================================================
;; repl_show_regions — List regions with stats
;; ============================================================
repl_show_regions:
    push rbx
    push r12
    push r13

    mov rbx, SURFACE_BASE
    lea r12, [rbx + REGION_TABLE_OFFSET]
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov r13, [rax]

    xor ecx, ecx
.rloop:
    cmp rcx, r13
    jge .rdone
    push rcx

    ; Entry address
    imul rdi, rcx, RTE_SIZE
    add rdi, r12

    ; Print index
    push rdi
    mov rdi, rcx
    call print_u64
    lea rdi, [rel colon_space]
    call print_cstr
    pop rdi

    ; Type
    push rdi
    movzx edi, word [rdi + RTE_TYPE]
    call print_region_type
    call print_space
    pop rdi

    ; Hits
    push rdi
    mov edi, [rdi + RTE_HITS]
    call print_u64
    lea rdi, [rel slash_char]
    call print_cstr
    pop rdi

    ; Misses
    push rdi
    mov edi, [rdi + RTE_MISSES]
    call print_u64
    pop rdi

    ; Flags
    push rdi
    movzx edi, word [rdi + RTE_FLAGS]
    push rdi                  ; save flags
    call print_space
    pop rdi                   ; restore flags to rdi (print_flags expects edi)
    call print_flags
    pop rdi

    call print_newline

    pop rcx
    inc rcx
    jmp .rloop
.rdone:
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; compute_total_accuracy → xmm0 (f32)
;; ============================================================
compute_total_accuracy:
    push rbx
    push r12
    push r13

    mov rbx, SURFACE_BASE
    lea r12, [rbx + REGION_TABLE_OFFSET]
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov r13, [rax]

    xor eax, eax              ; total hits
    xor edx, edx              ; total misses
    xor ecx, ecx
.aloop:
    cmp rcx, r13
    jge .adone
    imul rdi, rcx, RTE_SIZE
    add rdi, r12
    add eax, [rdi + RTE_HITS]
    add edx, [rdi + RTE_MISSES]
    inc rcx
    jmp .aloop
.adone:
    ; accuracy = hits / (hits + misses)
    add edx, eax              ; total = hits + misses
    test edx, edx
    jz .azero
    cvtsi2ss xmm0, eax
    cvtsi2ss xmm1, edx
    divss xmm0, xmm1
    jmp .aret
.azero:
    xorps xmm0, xmm0
.aret:
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; show_graph_stats
;; Compute and display graph dynamics metrics:
;; connections, avg prime, avg activation, avg resonance,
;; graph depth, entry table occupancy
;; ============================================================
show_graph_stats:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 32               ; [0]=conn_count, [4]=region_count_f
                              ; [8]=sum_prime(f64), [16]=sum_activ(f64), [24]=sum_reson(f64)
    mov rbx, SURFACE_BASE
    lea r12, [rbx + REGION_TABLE_OFFSET]
    mov r13d, [rbx + STATE_OFFSET + ST_REGION_COUNT]

    mov dword [rsp + 0], 0    ; connection count
    ; Zero f64 sums
    xorpd xmm0, xmm0
    movsd [rsp + 8], xmm0
    movsd [rsp + 16], xmm0
    movsd [rsp + 24], xmm0
    xor r14d, r14d            ; active region count

    xor ecx, ecx
.gs_loop:
    cmp ecx, r13d
    jge .gs_done_scan
    push rcx

    imul rdi, rcx, RTE_SIZE
    add rdi, r12
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_CONDEMNED
    jnz .gs_next

    mov rsi, [rdi + RTE_ADDR]
    inc r14d                  ; active count

    ; Count non-zero connections
    ; NOTE: push rcx above shifted rsp by 8, so all frame offsets are +8
    cmp qword [rsi + RHDR_NEXT_A], 0
    je .gs_c1
    inc dword [rsp + 8]
.gs_c1:
    cmp qword [rsi + RHDR_NEXT_B], 0
    je .gs_c2
    inc dword [rsp + 8]
.gs_c2:
    cmp qword [rsi + RHDR_EXCITE_A], 0
    je .gs_c3
    inc dword [rsp + 8]
.gs_c3:
    cmp qword [rsi + RHDR_EXCITE_B], 0
    je .gs_c4
    inc dword [rsp + 8]
.gs_c4:
    cmp qword [rsi + RHDR_INHIBIT_A], 0
    je .gs_c5
    inc dword [rsp + 8]
.gs_c5:
    cmp qword [rsi + RHDR_INHIBIT_B], 0
    je .gs_c6
    inc dword [rsp + 8]
.gs_c6:
    ; Accumulate prime
    movsd xmm0, [rsi + RHDR_PRIME]
    addsd xmm0, [rsp + 16]
    movsd [rsp + 16], xmm0
    ; Accumulate activation
    movsd xmm0, [rsi + RHDR_ACTIVATION]
    addsd xmm0, [rsp + 24]
    movsd [rsp + 24], xmm0
    ; Accumulate resonance
    movsd xmm0, [rsi + RHDR_RESONANCE]
    addsd xmm0, [rsp + 32]
    movsd [rsp + 32], xmm0

.gs_next:
    pop rcx
    inc ecx
    jmp .gs_loop

.gs_done_scan:
    ; Print connections
    lea rdi, [rel conn_lbl]
    call print_cstr
    mov edi, [rsp + 0]
    call print_u64
    call print_newline

    ; Compute averages (divide by active count)
    test r14d, r14d
    jz .gs_skip_avg

    ; Avg prime
    lea rdi, [rel avg_prime_lbl]
    call print_cstr
    cvtsi2sd xmm7, r14d      ; reload divisor (destroyed by calls)
    movsd xmm0, [rsp + 8]
    divsd xmm0, xmm7
    cvtsd2ss xmm0, xmm0      ; convert to f32 for print
    call print_f32
    call print_newline

    ; Avg activation
    lea rdi, [rel avg_activ_lbl]
    call print_cstr
    cvtsi2sd xmm7, r14d      ; reload divisor
    movsd xmm0, [rsp + 16]
    divsd xmm0, xmm7
    cvtsd2ss xmm0, xmm0
    call print_f32
    call print_newline

    ; Avg resonance
    lea rdi, [rel avg_reson_lbl]
    call print_cstr
    cvtsi2sd xmm7, r14d      ; reload divisor
    movsd xmm0, [rsp + 24]
    divsd xmm0, xmm7
    cvtsd2ss xmm0, xmm0
    call print_f32
    call print_newline
    jmp .gs_depth

.gs_skip_avg:
    ; No active regions — print zeros
    lea rdi, [rel avg_prime_lbl]
    call print_cstr
    xorps xmm0, xmm0
    call print_f32
    call print_newline
    lea rdi, [rel avg_activ_lbl]
    call print_cstr
    xorps xmm0, xmm0
    call print_f32
    call print_newline
    lea rdi, [rel avg_reson_lbl]
    call print_cstr
    xorps xmm0, xmm0
    call print_f32
    call print_newline

.gs_depth:
    ; Graph depth (last traversal)
    lea rdi, [rel graph_depth_lbl]
    call print_cstr
    mov edi, [rbx + STATE_OFFSET + ST_GRAPH_DEPTH]
    call print_u64
    call print_newline

    ; Entry table occupancy
    lea rdi, [rel entry_occ_lbl]
    call print_cstr
    lea rsi, [rbx + STATE_OFFSET + ST_ENTRY_TABLE]
    xor eax, eax              ; occupied count
    xor ecx, ecx
.gs_entry_loop:
    cmp ecx, ST_ENTRY_TABLE_CAP
    jge .gs_entry_done
    cmp qword [rsi + rcx * 8], 0
    je .gs_entry_next
    inc eax
.gs_entry_next:
    inc ecx
    jmp .gs_entry_loop
.gs_entry_done:
    mov edi, eax
    push rax
    call print_u64
    pop rax
    lea rdi, [rel of_16_lbl]
    call print_cstr
    call print_newline

    add rsp, 32
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; print_region_type(type_id)
;; edi=type
;; ============================================================
print_region_type:
    cmp edi, RTYPE_DISPATCH
    je .t_disp
    cmp edi, RTYPE_VSA_OP
    je .t_vsa
    cmp edi, RTYPE_MODIFIER
    je .t_mod
    cmp edi, RTYPE_OBSERVER
    je .t_obs
    cmp edi, RTYPE_EMITTER
    je .t_emit
    cmp edi, RTYPE_HOOK
    je .t_hook
    cmp edi, RTYPE_GATE
    je .t_gate
    cmp edi, RTYPE_DREAM
    je .t_dream
    cmp edi, RTYPE_RESONANT
    je .t_resonant
    cmp edi, RTYPE_SUBROUTINE
    je .t_subroutine
    cmp edi, RTYPE_PRESENCE
    je .t_presence
    lea rdi, [rel type_unknown]
    call print_cstr
    ret
.t_disp:
    lea rdi, [rel type_dispatch]
    call print_cstr
    ret
.t_vsa:
    lea rdi, [rel type_vsa]
    call print_cstr
    ret
.t_mod:
    lea rdi, [rel type_modifier]
    call print_cstr
    ret
.t_obs:
    lea rdi, [rel type_observer]
    call print_cstr
    ret
.t_emit:
    lea rdi, [rel type_emitter]
    call print_cstr
    ret
.t_hook:
    lea rdi, [rel type_hook]
    call print_cstr
    ret
.t_gate:
    lea rdi, [rel type_gate]
    call print_cstr
    ret
.t_dream:
    lea rdi, [rel type_dream]
    call print_cstr
    ret
.t_resonant:
    lea rdi, [rel type_resonant]
    call print_cstr
    ret
.t_subroutine:
    lea rdi, [rel type_subroutine]
    call print_cstr
    ret
.t_presence:
    lea rdi, [rel type_presence]
    call print_cstr
    ret

;; ============================================================
;; print_flags(flags)
;; edi=flags u16
;; ============================================================
print_flags:
    push rbx
    mov ebx, edi
    test ebx, RFLAG_ACTIVE
    jz .no_a
    lea rdi, [rel flag_active]
    call print_cstr
.no_a:
    test ebx, RFLAG_FROZEN
    jz .no_f
    lea rdi, [rel flag_frozen]
    call print_cstr
.no_f:
    test ebx, RFLAG_NURSERY
    jz .no_n
    lea rdi, [rel flag_nursery]
    call print_cstr
.no_n:
    test ebx, RFLAG_CONDEMNED
    jz .no_c
    lea rdi, [rel flag_condemned]
    call print_cstr
.no_c:
    pop rbx
    ret

;; ============================================================
;; repl_reset_counters
;; ============================================================
repl_reset_counters:
    push rbx
    mov rbx, SURFACE_BASE
    ; Reset global step
    lea rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov qword [rax], 0
    ; Reset token count
    lea rax, [rbx + STATE_OFFSET + ST_TOKEN_COUNT]
    mov dword [rax], 0
    ; Reset miss pos
    lea rax, [rbx + STATE_OFFSET + ST_MISS_POS]
    mov qword [rax], 0
    pop rbx
    ret

;; ============================================================
;; count_nursery_regions → eax (count of NURSERY-flagged regions)
;; ============================================================
count_nursery_regions:
    push rbx
    push r12
    mov rbx, SURFACE_BASE
    lea r12, [rbx + REGION_TABLE_OFFSET]
    mov ecx, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    xor eax, eax
    xor edx, edx
.cnr_loop:
    cmp edx, ecx
    jge .cnr_done
    push rcx
    push rdx
    imul rdi, rdx, RTE_SIZE
    add rdi, r12
    movzx ecx, word [rdi + RTE_FLAGS]
    test ecx, RFLAG_NURSERY
    jz .cnr_skip
    inc eax
.cnr_skip:
    pop rdx
    pop rcx
    inc edx
    jmp .cnr_loop
.cnr_done:
    pop r12
    pop rbx
    ret

;; ============================================================
;; print_goal(goal_id)
;; edi=goal enum
;; ============================================================
print_goal:
    cmp edi, GOAL_NONE
    je .g_none
    cmp edi, GOAL_EXPLORE
    je .g_explore
    cmp edi, GOAL_PRUNE
    je .g_prune
    cmp edi, GOAL_ALIGN
    je .g_align
    cmp edi, GOAL_CONSOLIDATE
    je .g_consolidate
    lea rdi, [rel goal_unknown_s]
    call print_cstr
    ret
.g_none:
    lea rdi, [rel goal_none_s]
    call print_cstr
    ret
.g_explore:
    lea rdi, [rel goal_explore_s]
    call print_cstr
    ret
.g_prune:
    lea rdi, [rel goal_prune_s]
    call print_cstr
    ret
.g_align:
    lea rdi, [rel goal_align_s]
    call print_cstr
    ret
.g_consolidate:
    lea rdi, [rel goal_consolidate_s]
    call print_cstr
    ret

;; ============================================================
;; repl_show_self — Semantic self-knowledge display
;; Scans regions, reports strengths (top-hit) and weaknesses (top-miss)
;; ============================================================
repl_show_self:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 48               ; [0-3]=top3_idx, [12-23]=top3_hits
                              ; [24-27]=bot3_idx, [36-47]=bot3_acc(f32)

    mov rbx, SURFACE_BASE
    lea r12, [rbx + REGION_TABLE_OFFSET]
    mov r13d, [rbx + STATE_OFFSET + ST_REGION_COUNT]

    ; Initialize top-3 hits to 0, bot-3 accuracy to 2.0 (above max)
    xor eax, eax
    mov [rsp + 0], eax        ; top1 idx
    mov [rsp + 4], eax        ; top2 idx
    mov [rsp + 8], eax        ; top3 idx
    mov [rsp + 12], eax       ; top1 hits
    mov [rsp + 16], eax       ; top2 hits
    mov [rsp + 20], eax       ; top3 hits
    mov eax, 0x40000000       ; 2.0f
    mov [rsp + 24], eax       ; bot1 idx (reuse as f32 placeholder)
    mov [rsp + 28], eax
    mov [rsp + 32], eax
    mov [rsp + 36], eax       ; bot1 acc
    mov [rsp + 40], eax       ; bot2 acc
    mov [rsp + 44], eax       ; bot3 acc

    xor ecx, ecx
.self_scan:
    cmp ecx, r13d
    jge .self_print
    push rcx

    imul rdi, rcx, RTE_SIZE
    add rdi, r12
    movzx eax, word [rdi + RTE_TYPE]
    cmp eax, RTYPE_DISPATCH
    jne .self_next
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_CONDEMNED
    jnz .self_next

    mov rsi, [rdi + RTE_ADDR]
    mov eax, [rsi + RHDR_HITS]
    mov edx, [rsi + RHDR_MISSES]
    add edx, eax
    cmp edx, 2
    jl .self_next

    ; Check for top-3 by hits
    cmp eax, [rsp + 8 + 12]   ; compare with top1 hits (+8 for pushed rcx)
    jle .self_check_bot
    ; New top1 — shift down
    mov edi, [rsp + 8 + 16]   ; old top2 → top3
    mov [rsp + 8 + 20], edi
    mov edi, [rsp + 8 + 4]
    mov [rsp + 8 + 8], edi
    mov edi, [rsp + 8 + 12]   ; old top1 → top2
    mov [rsp + 8 + 16], edi
    mov edi, [rsp + 8 + 0]
    mov [rsp + 8 + 4], edi
    ; Store new top1
    mov [rsp + 8 + 12], eax
    mov eax, [rsp]            ; current idx from pushed rcx
    mov [rsp + 8 + 0], eax
    jmp .self_next

.self_check_bot:
    ; Check for bottom-3 by accuracy (weaknesses)
    mov eax, [rsi + RHDR_HITS]
    mov edx, [rsi + RHDR_MISSES]
    add edx, eax
    cvtsi2ss xmm0, eax
    cvtsi2ss xmm1, edx
    divss xmm0, xmm1         ; accuracy
    ; Compare with bot1 (lowest accuracy = worst)
    comiss xmm0, [rsp + 8 + 36]
    jae .self_next
    ; New worst — shift up
    movss xmm1, [rsp + 8 + 40]
    movss [rsp + 8 + 44], xmm1
    mov edi, [rsp + 8 + 28]
    mov [rsp + 8 + 32], edi
    movss xmm1, [rsp + 8 + 36]
    movss [rsp + 8 + 40], xmm1
    mov edi, [rsp + 8 + 24]
    mov [rsp + 8 + 28], edi
    movss [rsp + 8 + 36], xmm0
    mov eax, [rsp]
    mov [rsp + 8 + 24], eax

.self_next:
    pop rcx
    inc ecx
    jmp .self_scan

.self_print:
    ; Print header
    lea rdi, [rel self_hdr]
    call print_cstr

    ; Strengths
    lea rdi, [rel self_strength]
    call print_cstr
    ; Print top1
    mov edi, [rsp + 0]
    call print_u64
    lea rdi, [rel self_hits_lbl]
    call print_cstr
    mov edi, [rsp + 12]
    call print_u64
    call print_newline
    ; Top2
    cmp dword [rsp + 16], 0
    je .self_weak
    lea rdi, [rel self_indent]
    call print_cstr
    mov edi, [rsp + 4]
    call print_u64
    lea rdi, [rel self_hits_lbl]
    call print_cstr
    mov edi, [rsp + 16]
    call print_u64
    call print_newline

.self_weak:
    ; Weaknesses
    lea rdi, [rel self_weakness]
    call print_cstr
    mov eax, 0x40000000       ; 2.0f
    movd xmm1, eax
    comiss xmm1, [rsp + 36]
    je .self_done             ; no weaknesses found (still at init value)
    mov edi, [rsp + 24]
    call print_u64
    lea rdi, [rel self_acc_lbl]
    call print_cstr
    movss xmm0, [rsp + 36]
    call print_f32
    call print_newline
    ; Weakness 2
    mov eax, 0x40000000
    movd xmm1, eax
    comiss xmm1, [rsp + 40]
    je .self_done
    lea rdi, [rel self_indent]
    call print_cstr
    mov edi, [rsp + 28]
    call print_u64
    lea rdi, [rel self_acc_lbl]
    call print_cstr
    movss xmm0, [rsp + 40]
    call print_f32
    call print_newline

.self_done:
    add rsp, 48
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; print_intro_state(state_id)
;; edi=state enum
;; ============================================================
print_intro_state:
    cmp edi, INTRO_IDLE
    je .is_idle
    cmp edi, INTRO_CONFUSED
    je .is_confused
    cmp edi, INTRO_CONFIDENT
    je .is_confident
    cmp edi, INTRO_LEARNING
    je .is_learning
    cmp edi, INTRO_STUCK
    je .is_stuck
    cmp edi, INTRO_EXPLORING
    je .is_exploring
    cmp edi, INTRO_CONSOLIDATING
    je .is_consolidating
    lea rdi, [rel intro_unknown]
    call print_cstr
    ret
.is_idle:
    lea rdi, [rel intro_idle_s]
    call print_cstr
    ret
.is_confused:
    lea rdi, [rel intro_confused_s]
    call print_cstr
    ret
.is_confident:
    lea rdi, [rel intro_confident_s]
    call print_cstr
    ret
.is_learning:
    lea rdi, [rel intro_learning_s]
    call print_cstr
    ret
.is_stuck:
    lea rdi, [rel intro_stuck_s]
    call print_cstr
    ret
.is_exploring:
    lea rdi, [rel intro_exploring_s]
    call print_cstr
    ret
.is_consolidating:
    lea rdi, [rel intro_consolidating_s]
    call print_cstr
    ret

;; ============================================================
;; print_dispatch_mode(mode_id)
;; edi=mode enum
;; ============================================================
print_dispatch_mode:
    cmp edi, DMODE_FAST
    je .dm_fast
    cmp edi, DMODE_BEST
    je .dm_best
    cmp edi, DMODE_EXPLORE
    je .dm_explore
    cmp edi, DMODE_DELIBERATE
    je .dm_deliberate
    lea rdi, [rel dmode_unknown_s]
    call print_cstr
    ret
.dm_fast:
    lea rdi, [rel dmode_fast_s]
    call print_cstr
    ret
.dm_best:
    lea rdi, [rel dmode_best_s]
    call print_cstr
    ret
.dm_explore:
    lea rdi, [rel dmode_explore_s]
    call print_cstr
    ret
.dm_deliberate:
    lea rdi, [rel dmode_deliberate_s]
    call print_cstr
    ret

;; ============================================================
;; print_surprise_type(type_id)
;; edi=surprise type enum
;; ============================================================
print_surprise_type:
    cmp edi, SURPRISE_NONE
    je .st_none
    cmp edi, SURPRISE_OUTCOME
    je .st_outcome
    cmp edi, SURPRISE_SELF
    je .st_self
    lea rdi, [rel surprise_unknown_s]
    call print_cstr
    ret
.st_none:
    lea rdi, [rel surprise_none_s]
    call print_cstr
    ret
.st_outcome:
    lea rdi, [rel surprise_outcome_s]
    call print_cstr
    ret
.st_self:
    lea rdi, [rel surprise_self_s]
    call print_cstr
    ret

section .rodata
    compact_msg:    db "Compacted regions: ", 0
    colon_space:    db ": ", 0
    slash_char:     db "/", 0
    type_dispatch:  db "DISPATCH", 0
    type_vsa:       db "VSA_OP  ", 0
    type_modifier:  db "MODIFIER", 0
    type_observer:  db "OBSERVER", 0
    type_emitter:   db "EMITTER ", 0
    type_hook:      db "HOOK    ", 0
    type_gate:      db "GATE    ", 0
    type_dream:     db "DREAM   ", 0
    type_resonant:  db "RESONANT", 0
    type_subroutine: db "SUBROUT ", 0
    type_presence:  db "PRESENCE", 0
    type_unknown:   db "UNKNOWN ", 0
    flag_active:    db "[A]", 0
    flag_frozen:    db "[F]", 0
    flag_nursery:   db "[N]", 0
    flag_condemned: db "[C]", 0
    intro_lbl:      db "  State: ", 0
    selfpred_lbl:   db "  Self-pred: ", 0
    schema_lbl:     db "  Schema: ", 0
    dmode_lbl:      db "  Dispatch: ", 0
    intro_idle_s:       db "IDLE", 0
    intro_confused_s:   db "CONFUSED", 0
    intro_confident_s:  db "CONFIDENT", 0
    intro_learning_s:   db "LEARNING", 0
    intro_stuck_s:      db "STUCK", 0
    intro_exploring_s:  db "EXPLORING", 0
    intro_consolidating_s: db "CONSOLIDATING", 0
    intro_unknown:      db "UNKNOWN", 0
    dmode_fast_s:       db "FAST", 0
    dmode_best_s:       db "BEST", 0
    dmode_explore_s:    db "EXPLORE", 0
    dmode_deliberate_s: db "DELIBERATE", 0
    dmode_unknown_s:    db "UNKNOWN", 0
    surprise_lbl:       db "  Surprise: ", 0
    causal_lbl:         db "  Causal: ", 0
    hypo_lbl:           db "  Hypotheses: ", 0
    trace_lbl:          db "  Trace: ", 0
    trace_sep:          db " candidates, ", 0
    goal_lbl:           db "  Goal: ", 0
    variance_lbl:       db "  Variance: ", 0
    expect_lbl:         db "  Expect: conf=", 0
    expect_schema:      db " [SCHEMA]", 0
    expect_none:        db "none", 0
    surprise_none_s:    db "NONE", 0
    surprise_outcome_s: db "OUTCOME", 0
    surprise_self_s:    db "SELF", 0
    surprise_unknown_s: db "UNKNOWN", 0
    goal_none_s:        db "NONE", 0
    goal_explore_s:     db "EXPLORE", 0
    goal_prune_s:       db "PRUNE", 0
    goal_align_s:       db "ALIGN", 0
    goal_consolidate_s: db "CONSOLIDATE", 0
    goal_unknown_s:     db "UNKNOWN", 0
    self_hdr:           db "--- Self-Knowledge ---", 10, 0
    self_strength:      db "  Strengths: region ", 0
    self_weakness:      db "  Weaknesses: region ", 0
    self_hits_lbl:      db " (hits=", 0
    self_acc_lbl:       db " (acc=", 0
    self_indent:        db "             region ", 0
    ; Holographic memory labels
    holo_vocab_lbl:     db "  Vocabulary: ", 0
    holo_density_lbl:   db "  Holo density: ", 0
    holo_conf_lbl:      db "  Holo confidence: ", 0
    holo_conf_n_lbl:    db " (n=", 0
    holo_conf_end:      db ")", 0
    ; Graph dynamics labels
    conn_lbl:           db "  Connections: ", 0
    avg_prime_lbl:      db "  Avg prime: ", 0
    avg_activ_lbl:      db "  Avg activation: ", 0
    avg_reson_lbl:      db "  Avg resonance: ", 0
    graph_depth_lbl:    db "  Graph depth: ", 0
    entry_occ_lbl:      db "  Entry table: ", 0
    of_16_lbl:          db "/16", 0
