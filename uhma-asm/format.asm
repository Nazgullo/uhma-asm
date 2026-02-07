; format.asm — Output formatting with channel routing + buffer capture
; When a GUI stream is active, all prints are mirrored via gateway_stream_send using gw_stream_subnet.
;
; @entry set_output_channel(edi=fd) -> routes output to socket fd
; @entry reset_output_channel() -> resets output to stdout (fd=1)
; @entry set_output_buffer() -> routes output to internal 64KB buffer
; @entry get_output_buffer() -> rax=buf_ptr, edx=length
; @entry print_str(rdi=ptr, rsi=len) -> writes to output_fd or buffer
; @entry print_cstr(rdi=ptr) -> writes null-terminated string
; @entry print_u64(rdi=val) -> prints decimal
; @entry print_i64(rdi=val) -> prints signed decimal
; @entry print_hex64(rdi=val) -> prints 0x-prefixed 16-digit hex
; @entry print_hex32(edi=val) -> prints 8-digit hex (no prefix)
; @entry print_f32(xmm0=val) -> prints float with 3 decimals
; @entry print_f64(xmm0=val) -> prints double with 4 decimals
; @entry print_newline() -> writes '\n'
; @entry print_space() -> writes ' '
; @calledby nearly every module for debug/status output
;
; OUTPUT ROUTING:
;   output_fd variable (default=1) controls where all print functions write
;   output_fd = -1: buffer capture mode (accumulates in output_buffer)
;   set_output_channel(fd) changes target, reset_output_channel() restores stdout
;   set_output_buffer() enables capture, get_output_buffer() retrieves result
;   Used by gateway.asm to capture command responses before framing
;
; GOTCHAS:
;   - All print functions use output_fd, not hardcoded STDOUT
;   - Caller must call reset_output_channel() after TCP response
;   - Buffer mode: output_fd=-1, all writes go to output_buffer
;
%include "syscalls.inc"
%include "constants.inc"

section .bss
    fmt_buf: resb 128                ; scratch buffer for formatting
    output_buffer: resb 65536        ; 64KB response accumulation buffer

section .data
    output_fd: dd 1                  ; current output fd (1=stdout, or channel socket, -1=buffer)
    output_buf_pos: dd 0             ; current write position in output_buffer

section .text

extern gw_stream_client
extern gateway_stream_send

;; ============================================================
;; set_output_channel(fd)
;; edi=fd (1=stdout, or socket fd for channel output)
;; ============================================================
global set_output_channel
set_output_channel:
    mov [rel output_fd], edi
    ret

;; ============================================================
;; reset_output_channel — reset to stdout
;; ============================================================
global reset_output_channel
reset_output_channel:
    mov dword [rel output_fd], 1
    ret

;; ============================================================
;; set_output_buffer — Enable buffer capture mode
;; All print_str calls accumulate in output_buffer instead of writing
;; ============================================================
global set_output_buffer
set_output_buffer:
    mov dword [rel output_fd], -1
    mov dword [rel output_buf_pos], 0
    ret

;; ============================================================
;; get_output_buffer — Get captured buffer contents
;; Returns: rax=buffer ptr, edx=length
;; ============================================================
global get_output_buffer
get_output_buffer:
    lea rax, [rel output_buffer]
    mov edx, [rel output_buf_pos]
    ret

;; ============================================================
;; print_str(str, len)
;; rdi=str ptr, rsi=len
;; Writes to output_fd (stdout or channel socket)
;; ============================================================
global print_str
print_str:
    push rbx
    mov rbx, rdi                ; save ptr
    mov r10, rsi                ; save len

    mov edx, [rel output_fd]
    cmp edx, -1
    je .buffer_mode
    ; Normal write to fd
    mov rdx, r10
    mov rsi, rbx
    mov edi, [rel output_fd]
    mov rax, SYS_WRITE
    syscall
    jmp .maybe_stream
.buffer_mode:
    ; Accumulate into output_buffer
    ; rdi=src, rsi=len
    push rcx
    mov rsi, r10
    mov rdi, rbx
    mov ecx, [rel output_buf_pos]
    lea rdx, [rcx + rsi]
    cmp edx, 65536              ; don't overflow buffer
    jg .buffer_truncate
    mov rdx, rsi                ; bytes to copy
    jmp .buffer_copy
.buffer_truncate:
    mov edx, 65536
    sub edx, ecx               ; remaining space
    test edx, edx
    jle .buffer_done
.buffer_copy:
    ; memcpy: output_buffer+pos <- rdi, edx bytes
    push rdi
    push rsi
    lea rax, [rel output_buffer]
    add rax, rcx                ; dest = output_buffer + pos
    mov ecx, edx               ; count
    mov rsi, rdi                ; src
    mov rdi, rax                ; dest
    rep movsb
    pop rsi
    pop rdi
    ; Update position
    mov ecx, [rel output_buf_pos]
    add ecx, edx
    mov [rel output_buf_pos], ecx
.buffer_done:
    pop rcx
    jmp .maybe_stream

.maybe_stream:
    mov eax, [rel gw_stream_client]
    cmp eax, -1
    je .print_done
    mov rsi, rbx
    mov edx, r10d
    call gateway_stream_send
.print_done:
    pop rbx
    ret

;; ============================================================
;; print_cstr(str)
;; rdi=null-terminated string
;; Writes to output_fd (stdout or channel socket)
;; ============================================================
global print_cstr
print_cstr:
    ; find length
    xor rsi, rsi
.scan:
    cmp byte [rdi + rsi], 0
    je .found
    inc rsi
    jmp .scan
.found:
    ; rdi=str, rsi=len — tail-call print_str
    jmp print_str

;; ============================================================
;; print_u64(val)
;; rdi=u64 value, prints decimal
;; ============================================================
global print_u64
print_u64:
    push rbx
    push rbp
    mov rax, rdi
    lea rdi, [rel fmt_buf + 64]    ; work from end
    mov byte [rdi], 0
    mov rbx, 10
    test rax, rax
    jnz .loop
    ; zero case
    dec rdi
    mov byte [rdi], '0'
    jmp .print
.loop:
    test rax, rax
    jz .print
    xor edx, edx
    div rbx
    add dl, '0'
    dec rdi
    mov [rdi], dl
    jmp .loop
.print:
    ; rdi points to start of digits
    lea rsi, [rel fmt_buf + 64]
    sub rsi, rdi               ; len = end - start
    ; rdi=str, rsi=len — call print_str (handles fd or buffer)
    call print_str
    pop rbp
    pop rbx
    ret

;; ============================================================
;; print_i64(val)
;; rdi=i64 value, prints signed decimal
;; ============================================================
global print_i64
print_i64:
    test rdi, rdi
    jns print_u64              ; positive → same as u64
    push rdi
    ; print minus sign
    lea rdi, [rel minus_char]
    mov rsi, 1
    call print_str
    pop rdi
    neg rdi
    jmp print_u64

;; ============================================================
;; print_hex64(val)
;; rdi=u64 value, prints 0x prefixed hex
;; ============================================================
global print_hex64
print_hex64:
    push rbx
    mov rax, rdi
    lea rdi, [rel fmt_buf]
    mov word [rdi], '0x'
    add rdi, 2
    mov ecx, 16               ; 16 hex digits
.hexloop:
    dec ecx
    mov rbx, rax
    shr rbx, cl
    shr rbx, cl
    shr rbx, cl
    shr rbx, cl
    ; Actually: shift by (cl*4) bits
    jmp .hexcalc
.hexcalc:
    ; recalculate properly
    push rcx
    mov rbx, rax
    mov cl, 60
.shift:
    cmp cl, 0
    jl .hexdone2
    mov rdx, rbx
    shr rdx, cl
    and edx, 0xF
    cmp dl, 10
    jl .hexdigit
    add dl, ('a' - 10)
    jmp .hexstore
.hexdigit:
    add dl, '0'
.hexstore:
    mov [rdi], dl
    inc rdi
    sub cl, 4
    jmp .shift
.hexdone2:
    pop rcx
    ; print
    lea rsi, [rel fmt_buf]
    mov rdi, rsi
    mov rsi, 18                ; "0x" + 16 digits
    call print_str
    pop rbx
    ret

;; ============================================================
;; print_hex32(val)
;; edi=u32 value, prints hex without prefix
;; ============================================================
global print_hex32
print_hex32:
    push rbx
    mov eax, edi
    lea rdi, [rel fmt_buf]
    mov ecx, 28                ; start shift (7*4)
.loop32:
    mov edx, eax
    mov cl, cl
    shr edx, cl
    and edx, 0xF
    cmp dl, 10
    jl .dig
    add dl, ('a' - 10)
    jmp .store
.dig:
    add dl, '0'
.store:
    mov [rdi], dl
    inc rdi
    sub ecx, 4
    jns .loop32
    ; print 8 chars
    lea rdi, [rel fmt_buf]
    mov rsi, 8
    call print_str
    pop rbx
    ret

;; ============================================================
;; print_f32(val)
;; xmm0=f32 value, prints fixed point (3 decimals)
;; ============================================================
global print_f32
print_f32:
    sub rsp, 16
    
    ; Check for NaN/Inf
    movd eax, xmm0
    mov ecx, eax
    and ecx, 0x7F800000        ; mask exponent
    cmp ecx, 0x7F800000
    je .nan_inf

    ; Check negative
    test eax, eax
    jns .pos
    push rax
    lea rdi, [rel minus_char]
    mov rsi, 1
    call print_str
    pop rax
    and eax, 0x7FFFFFFF        ; clear sign bit
    movd xmm0, eax
.pos:
    ; Integer part
    cvttss2si rdi, xmm0
    push rdi                   ; save integer part
    call print_u64
    ; Decimal point
    lea rdi, [rel dot_char]
    mov rsi, 1
    call print_str
    ; Fractional part (3 digits)
    pop rdi
    cvtsi2ss xmm1, rdi
    subss xmm0, xmm1          ; frac = val - int_part
    mov eax, 1000
    cvtsi2ss xmm1, eax
    mulss xmm0, xmm1          ; frac * 1000
    cvttss2si rdi, xmm0
    ; Ensure positive
    test rdi, rdi
    jns .fpos
    neg rdi
.fpos:
    ; Print with leading zeros
    cmp rdi, 100
    jge .f3
    push rdi
    lea rdi, [rel zero_char]
    mov rsi, 1
    call print_str
    pop rdi
.f3:
    cmp rdi, 10
    jge .f2
    push rdi
    lea rdi, [rel zero_char]
    mov rsi, 1
    call print_str
    pop rdi
.f2:
    call print_u64
    add rsp, 16
    ret

.nan_inf:
    ; Check if NaN (mantissa != 0)
    mov ecx, eax
    and ecx, 0x007FFFFF
    jnz .is_nan
    ; Is Inf
    lea rdi, [rel inf_str]
    call print_cstr
    add rsp, 16
    ret
.is_nan:
    lea rdi, [rel nan_str]
    call print_cstr
    add rsp, 16
    ret

section .data
    nan_str: db "NaN", 0
    inf_str: db "Inf", 0
section .text


;; ============================================================
;; print_f64(val)
;; xmm0=f64 value, prints fixed point (4 decimals)
;; ============================================================
global print_f64
print_f64:
    sub rsp, 24
    movsd [rsp], xmm0           ; save original value

    ; Check for NaN/Inf
    movq rax, xmm0
    mov rcx, rax
    shr rcx, 52
    and ecx, 0x7FF             ; mask exponent
    cmp ecx, 0x7FF
    je .f64_nan_inf

    ; Check for negative
    test rax, rax
    jns .f64_pos
    push rax
    lea rdi, [rel minus_char]
    mov rsi, 1
    call print_str
    pop rax
    ; Clear sign bit (make positive)
    mov rcx, 0x7FFFFFFFFFFFFFFF
    and rax, rcx
    movq xmm0, rax
    movsd [rsp], xmm0

.f64_pos:
    ; Integer part
    cvttsd2si rdi, xmm0
    mov [rsp + 8], rdi          ; save integer part
    call print_u64

    ; Decimal point
    lea rdi, [rel dot_char]
    mov rsi, 1
    call print_str

    ; Fractional part (4 digits)
    jmp .f64_frac_continue

.f64_nan_inf:
    ; Check if NaN (mantissa != 0)
    mov rcx, rax
    mov rdx, 0x000FFFFFFFFFFFFF
    and rcx, rdx
    jnz .f64_is_nan
    ; Is Inf
    lea rdi, [rel inf_str]
    call print_cstr
    add rsp, 24
    ret
.f64_is_nan:
    lea rdi, [rel nan_str]
    call print_cstr
    add rsp, 24
    ret

.f64_frac_continue:

    movsd xmm0, [rsp]           ; reload value
    ; Make sure it's positive (in case original was negative)
    movq rax, xmm0
    mov rcx, 0x7FFFFFFFFFFFFFFF
    and rax, rcx
    movq xmm0, rax

    mov rdi, [rsp + 8]          ; integer part
    cvtsi2sd xmm1, rdi
    subsd xmm0, xmm1            ; frac = val - int_part

    ; Multiply by 10000 for 4 decimal places
    mov rax, 10000
    cvtsi2sd xmm1, rax
    mulsd xmm0, xmm1
    cvttsd2si rdi, xmm0

    ; Ensure positive
    test rdi, rdi
    jns .f64_fpos
    neg rdi
.f64_fpos:
    mov [rsp + 16], rdi         ; save fractional

    ; Print with leading zeros
    cmp rdi, 1000
    jge .f64_4
    push rdi
    lea rdi, [rel zero_char]
    mov rsi, 1
    call print_str
    pop rdi
.f64_4:
    cmp rdi, 100
    jge .f64_3
    push rdi
    lea rdi, [rel zero_char]
    mov rsi, 1
    call print_str
    pop rdi
.f64_3:
    cmp rdi, 10
    jge .f64_2
    push rdi
    lea rdi, [rel zero_char]
    mov rsi, 1
    call print_str
    pop rdi
.f64_2:
    call print_u64

    add rsp, 24
    ret

;; ============================================================
;; print_newline
;; ============================================================
global print_newline
print_newline:
    lea rdi, [rel nl_char]
    mov rsi, 1
    call print_str
    ret

;; ============================================================
;; print_space
;; ============================================================
global print_space
print_space:
    lea rdi, [rel sp_char]
    mov rsi, 1
    call print_str
    ret

section .rodata
    minus_char: db '-'
    dot_char:   db '.'
    zero_char:  db '0'
    nl_char:    db 10
    sp_char:    db ' '
