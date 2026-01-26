; trace.asm — Token journey tracing: record token path through system
;
; ENTRY POINTS:
;   journey_start(token_id)           - begin tracing this token
;   journey_stop()                    - stop tracing
;   journey_step(trace_id)            - record step in journey (if tracing)
;   journey_dump()                    - print full journey path
;   trace_enable(), trace_disable()   - global trace on/off
;   trace_log(msg), trace_dump()      - debug logging
;
; TRACE IDs (function identifiers):
;   TRACE_PROCESS_INPUT, TRACE_PROCESS_TOKEN, TRACE_DISPATCH_PREDICT,
;   TRACE_SPREAD_ACTIVATION, TRACE_LEARN_PATTERN, TRACE_FIND_EXISTING,
;   TRACE_WIRE_NEW_REGION, TRACE_EMIT_PATTERN, TRACE_FIRE_HOOK,
;   TRACE_OBSERVE_CYCLE, TRACE_MODIFY_PRUNE, TRACE_MODIFY_PROMOTE,
;   TRACE_EVOLVE_CYCLE, TRACE_DREAM_CYCLE, TRACE_UPDATE_ORGANIC
;
; ZERO OVERHEAD:
;   journey_step checks ST_JOURNEY_TOKEN first (single cmp)
;   If not tracing current token, returns immediately
;   Only traced tokens incur logging overhead
;
; BUFFER: ST_JOURNEY_BUF (256 entries of trace_id)
;
; CALLED BY: dispatch.asm, learn.asm, hooks.asm, dreams.asm, etc.
;
%include "syscalls.inc"
%include "constants.inc"

section .data
    journey_start_msg:  db "[JOURNEY] Tracing token 0x", 0
    journey_stop_msg:   db "[JOURNEY] Stopped tracing", 10, 0
    journey_step_msg:   db "  -> ", 0
    journey_dump_hdr:   db "[JOURNEY] === Token path (", 0
    journey_dump_mid:   db " steps) ===", 10, 0
    journey_arrow:      db " -> ", 0
    journey_nl:         db 10, 0
    journey_overflow:   db "[JOURNEY] Buffer full!", 10, 0

    ; Function names for readable output
    fn_names:
    fn_000: db "???", 0
    fn_402: db "process_input", 0
    fn_406: db "process_token", 0
    fn_408: db "dispatch_predict", 0
    fn_410: db "spread_activation", 0
    fn_550: db "learn_pattern", 0
    fn_552: db "find_existing", 0
    fn_558: db "wire_new_region", 0
    fn_500: db "emit_pattern", 0
    fn_600: db "fire_hook", 0
    fn_650: db "observe_cycle", 0
    fn_700: db "modify_prune", 0
    fn_702: db "modify_promote", 0
    fn_900: db "evolve_cycle", 0
    fn_950: db "dream_cycle", 0
    fn_1058: db "update_organic", 0

section .text

extern print_cstr
extern print_hex32
extern print_u64
extern print_newline

;; ============================================================
;; journey_start(token_id)
;; edi = token to trace
;; Marks this token for journey recording
;; ============================================================
global journey_start
journey_start:
    push rbx
    mov rbx, SURFACE_BASE

    ; Store the token to trace
    mov [rbx + STATE_OFFSET + ST_JOURNEY_TOKEN], edi
    ; Reset position
    mov dword [rbx + STATE_OFFSET + ST_JOURNEY_POS], 0

    ; Print start message
    push rdi
    lea rdi, [rel journey_start_msg]
    call print_cstr
    pop rdi
    push rdi
    call print_hex32
    call print_newline
    pop rdi

    pop rbx
    ret

;; ============================================================
;; journey_stop
;; Stops journey tracing
;; ============================================================
global journey_stop
journey_stop:
    mov rax, SURFACE_BASE
    mov dword [rax + STATE_OFFSET + ST_JOURNEY_TOKEN], 0
    lea rdi, [rel journey_stop_msg]
    call print_cstr
    ret

;; ============================================================
;; journey_step(func_id)
;; edi = function ID
;; Reads current token from ST_CURRENT_TOKEN
;; If it matches the traced token, record this step
;; FAST PATH: two compares, returns immediately if no match
;; ============================================================
global journey_step
journey_step:
    push rbx
    push r12
    mov r12d, edi                 ; save func_id
    mov rbx, SURFACE_BASE

    ; Fast path: check if we're tracing any token
    mov eax, [rbx + STATE_OFFSET + ST_JOURNEY_TOKEN]
    test eax, eax
    jz .done                      ; no token being traced

    ; Check if current token matches traced token
    mov ecx, [rbx + STATE_OFFSET + ST_CURRENT_TOKEN]
    cmp eax, ecx
    jne .done                     ; not the traced token

    ; This IS the traced token - record the step
    mov ecx, [rbx + STATE_OFFSET + ST_JOURNEY_POS]
    cmp ecx, ST_JOURNEY_CAP
    jge .overflow

    ; Write entry: func_id (u16) + extra (u16)
    lea rax, [rbx + STATE_OFFSET + ST_JOURNEY_BUF]
    mov [rax + rcx * 4], r12w     ; func_id (low 16 bits)
    mov word [rax + rcx * 4 + 2], 0  ; extra (reserved)

    ; Increment position
    inc ecx
    mov [rbx + STATE_OFFSET + ST_JOURNEY_POS], ecx

.done:
    pop r12
    pop rbx
    ret

.overflow:
    lea rdi, [rel journey_overflow]
    call print_cstr
    jmp .done

;; ============================================================
;; journey_dump
;; Print the journey of the traced token
;; ============================================================
global journey_dump
journey_dump:
    push rbx
    push r12
    push r13

    mov rbx, SURFACE_BASE

    ; Print header
    lea rdi, [rel journey_dump_hdr]
    call print_cstr

    mov r12d, [rbx + STATE_OFFSET + ST_JOURNEY_POS]
    mov edi, r12d
    call print_u64

    lea rdi, [rel journey_dump_mid]
    call print_cstr

    ; Print each step
    test r12d, r12d
    jz .dump_done

    lea r13, [rbx + STATE_OFFSET + ST_JOURNEY_BUF]
    xor ecx, ecx

.dump_loop:
    cmp ecx, r12d
    jge .dump_done
    push rcx

    ; Get func_id
    movzx edi, word [r13 + rcx * 4]

    ; Print step number and func_id
    lea rdi, [rel journey_step_msg]
    call print_cstr

    pop rcx
    push rcx
    movzx edi, word [r13 + rcx * 4]
    call print_u64

    ; Try to print function name
    call get_func_name
    test rax, rax
    jz .no_name
    push rcx
    mov rdi, rax
    call print_cstr
    pop rcx
.no_name:
    call print_newline

    pop rcx
    inc ecx
    jmp .dump_loop

.dump_done:
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; get_func_name(func_id) -> rax = string ptr or 0
;; edi = func_id
;; Returns pointer to function name string
;; ============================================================
get_func_name:
    cmp edi, 402
    je .fn_402
    cmp edi, 406
    je .fn_406
    cmp edi, 408
    je .fn_408
    cmp edi, 410
    je .fn_410
    cmp edi, 550
    je .fn_550
    cmp edi, 552
    je .fn_552
    cmp edi, 558
    je .fn_558
    cmp edi, 500
    je .fn_500
    cmp edi, 600
    je .fn_600
    cmp edi, 650
    je .fn_650
    cmp edi, 700
    je .fn_700
    cmp edi, 702
    je .fn_702
    cmp edi, 900
    je .fn_900
    cmp edi, 950
    je .fn_950
    cmp edi, 1058
    je .fn_1058
    xor eax, eax
    ret
.fn_402:
    lea rax, [rel fn_402]
    ret
.fn_406:
    lea rax, [rel fn_406]
    ret
.fn_408:
    lea rax, [rel fn_408]
    ret
.fn_410:
    lea rax, [rel fn_410]
    ret
.fn_550:
    lea rax, [rel fn_550]
    ret
.fn_552:
    lea rax, [rel fn_552]
    ret
.fn_558:
    lea rax, [rel fn_558]
    ret
.fn_500:
    lea rax, [rel fn_500]
    ret
.fn_600:
    lea rax, [rel fn_600]
    ret
.fn_650:
    lea rax, [rel fn_650]
    ret
.fn_700:
    lea rax, [rel fn_700]
    ret
.fn_702:
    lea rax, [rel fn_702]
    ret
.fn_900:
    lea rax, [rel fn_900]
    ret
.fn_950:
    lea rax, [rel fn_950]
    ret
.fn_1058:
    lea rax, [rel fn_1058]
    ret

;; ============================================================
;; Legacy trace functions (keep for compatibility)
;; ============================================================
global trace_enable
trace_enable:
    ret

global trace_disable
trace_disable:
    ret

global trace_log
trace_log:
    ret

global trace_dump
trace_dump:
    call journey_dump
    ret
