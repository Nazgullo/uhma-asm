; format.asm — Output formatting: integers, floats, hex, strings
;
; ENTRY POINTS:
;   print_str(ptr, len)     - write raw bytes to stdout
;   print_cstr(ptr)         - write null-terminated string to stdout
;   print_u64(val)          - print u64 as decimal
;   print_i64(val)          - print i64 as signed decimal
;   print_hex64(val)        - print 16-digit hex (0-padded)
;   print_hex32(val)        - print 8-digit hex (0-padded)
;   print_f32(val)          - print f32 with 3 decimal places
;   print_f64(val)          - print f64 with 6 decimal places
;   print_newline()         - write '\n' to stdout
;   print_space()           - write ' ' to stdout
;
; IMPLEMENTATION:
;   Uses 128-byte scratch buffer (fmt_buf) for conversions
;   Decimal: divide-by-10 loop, reverse digits
;   Hex: nibble extraction with lookup table
;   Float: integer part + decimal part conversion
;
; CALLED BY: nearly every module for debug/status output
; NO EXTERNAL CALLS (self-contained, uses SYS_WRITE directly)
;
%include "syscalls.inc"
%include "constants.inc"

section .bss
    fmt_buf: resb 128                ; scratch buffer for formatting

section .text

;; ============================================================
;; print_str(str, len)
;; rdi=str ptr, rsi=len
;; ============================================================
global print_str
print_str:
    mov rdx, rsi
    mov rsi, rdi
    mov edi, STDOUT
    mov rax, SYS_WRITE
    syscall
    ret

;; ============================================================
;; print_cstr(str)
;; rdi=null-terminated string
;; ============================================================
global print_cstr
print_cstr:
    push rdi
    ; find length
    xor rcx, rcx
.scan:
    cmp byte [rdi + rcx], 0
    je .found
    inc rcx
    jmp .scan
.found:
    mov rdx, rcx              ; len
    pop rsi                   ; buf
    mov edi, STDOUT
    mov rax, SYS_WRITE
    syscall
    ret

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
    sub rsi, rdi               ; len
    mov rdx, rsi
    mov rsi, rdi
    mov edi, STDOUT
    mov rax, SYS_WRITE
    syscall
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
    ; Check negative
    movd eax, xmm0
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

;; ============================================================
;; print_f64(val)
;; xmm0=f64 value, prints fixed point (4 decimals)
;; ============================================================
global print_f64
print_f64:
    sub rsp, 24
    movsd [rsp], xmm0           ; save original value

    ; Check for negative
    movq rax, xmm0
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
