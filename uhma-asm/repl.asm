; repl.asm — Interactive command loop
%include "syscalls.inc"
%include "constants.inc"

section .data
    prompt_str:     db "uhma> ", 0
    prompt_len      equ 6
    banner_str:     db "UHMA x86_64 — Unified Holographic Memory Architecture", 10
                    db "Surface: 8GB RWX | VSA: 1024-dim f32 | Self-modifying dispatch", 10
                    db "Type 'help' for commands, text to process, or Ctrl-D to exit.", 10, 10, 0
    help_str:       db "Commands:", 10
                    db "  help          Show this help", 10
                    db "  status        Show system status (regions, accuracy, drives)", 10
                    db "  self          Show self-knowledge (strengths/weaknesses)", 10
                    db "  regions       List all regions with hit/miss stats", 10
                    db "  presence      Show presence field values", 10
                    db "  drives        Show drive levels and thresholds", 10
                    db "  observe       Trigger observation cycle", 10
                    db "  dream         Trigger dream/consolidation cycle", 10
                    db "  compact       Compact condemned regions", 10
                    db "  save <file>   Save surface to file", 10
                    db "  load <file>   Restore surface from file", 10
                    db "  reset         Reset counters (not knowledge)", 10
                    db "  quit          Exit", 10
                    db "  <text>        Process as token sequence", 10, 0
    bye_str:        db "Surface frozen. Goodbye.", 10, 0
    unknown_str:    db "Unknown command. Type 'help'.", 10, 0
    status_hdr:     db "--- Status ---", 10, 0
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
extern region_compact
extern get_fault_count
extern persist_save
extern persist_load
extern drives_show
extern presence_show

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

    ; "status"
    mov eax, [rbx]
    cmp eax, 'stat'
    jne .not_status
    cmp word [rbx + 4], 'us'
    jne .not_status
    jmp .cmd_status
.not_status:

    ; "regions"
    cmp dword [rbx], 'regi'
    jne .not_regions
    cmp word [rbx + 4], 'on'
    jne .not_regions
    jmp .cmd_regions
.not_regions:

    ; "presence"
    cmp dword [rbx], 'pres'
    jne .not_presence
    jmp .cmd_presence
.not_presence:

    ; "drives"
    cmp dword [rbx], 'driv'
    jne .not_drives
    jmp .cmd_drives
.not_drives:

    ; "observe"
    cmp dword [rbx], 'obse'
    jne .not_observe
    jmp .cmd_observe
.not_observe:

    ; "dream"
    cmp dword [rbx], 'drea'
    jne .not_dream
    jmp .cmd_dream
.not_dream:

    ; "compact"
    cmp dword [rbx], 'comp'
    jne .not_compact
    jmp .cmd_compact
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

.cmd_reset:
    call repl_reset_counters
    jmp .loop

.cmd_self:
    call repl_show_self
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

    pop rbx
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
    call print_space
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
