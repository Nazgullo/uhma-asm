; persist.asm — Save/restore the entire surface to/from file
%include "syscalls.inc"
%include "constants.inc"

section .data
    save_msg:       db "[PERSIST] Saving surface to: ", 0
    save_done:      db "[PERSIST] Save complete. Bytes: ", 0
    load_msg:       db "[PERSIST] Loading surface from: ", 0
    load_done:      db "[PERSIST] Restore complete. Bytes: ", 0
    save_err:       db "[PERSIST] Error: could not open file for writing", 10, 0
    load_err:       db "[PERSIST] Error: could not open file for reading", 10, 0
    persist_nl:     db 10, 0

    ; File header magic
    persist_magic:  db "UHMA", 0, 0, 0, 0   ; 8 bytes
    persist_ver:    dq 2                      ; version (2 = includes holo/vocab)

section .text

extern print_cstr
extern print_u64
extern print_newline
extern fire_hook

;; ============================================================
;; persist_save(filename)
;; rdi=null-terminated filename string
;; Saves the critical surface regions to a file:
;; - Header (magic + version + metadata)
;; - State block
;; - Region table
;; - Dispatch region (bootstrap to alloc ptr)
;; ============================================================
global persist_save
persist_save:
    push rbx
    push r12
    push r13
    push r14

    mov r12, rdi              ; filename

    ; Print save message
    lea rdi, [rel save_msg]
    call print_cstr
    mov rdi, r12
    call print_cstr
    call print_newline

    ; Fire save hook
    mov edi, HOOK_ON_SAVE
    xor esi, esi
    call fire_hook

    ; Open file for writing (create/truncate)
    mov rdi, r12
    mov rsi, O_WRONLY | O_CREAT | O_TRUNC
    mov rdx, FILE_MODE
    mov rax, SYS_OPEN
    syscall

    cmp rax, 0
    jl .save_error
    mov r13, rax              ; fd

    mov rbx, SURFACE_BASE
    xor r14d, r14d            ; total bytes written

    ; --- Write header ---
    ; Magic (8 bytes)
    mov rdi, r13
    lea rsi, [rel persist_magic]
    mov rdx, 8
    mov rax, SYS_WRITE
    syscall
    add r14, rax

    ; Version (8 bytes)
    mov rdi, r13
    lea rsi, [rel persist_ver]
    mov rdx, 8
    mov rax, SYS_WRITE
    syscall
    add r14, rax

    ; Surface base address (8 bytes, for relocation check)
    sub rsp, 8
    mov rax, SURFACE_BASE
    mov [rsp], rax
    mov rdi, r13
    lea rsi, [rsp]
    mov rdx, 8
    mov rax, SYS_WRITE
    syscall
    add r14, rax
    add rsp, 8

    ; --- Write state block ---
    mov rdi, r13
    lea rsi, [rbx + STATE_OFFSET]
    mov rdx, STATE_SIZE
    mov rax, SYS_WRITE
    syscall
    add r14, rax

    ; --- Write region table ---
    mov rdi, r13
    lea rsi, [rbx + REGION_TABLE_OFFSET]
    mov rdx, REGION_TABLE_SIZE
    mov rax, SYS_WRITE
    syscall
    add r14, rax

    ; --- Write dispatch region ---
    ; From DISPATCH_OFFSET to current alloc ptr
    lea rax, [rbx + STATE_OFFSET + ST_DISPATCH_PTR]
    mov rcx, [rax]            ; current alloc ptr (absolute)
    sub rcx, rbx
    sub rcx, DISPATCH_OFFSET  ; size = ptr - dispatch_base
    test rcx, rcx
    jle .skip_dispatch

    mov rdi, r13
    lea rsi, [rbx + DISPATCH_OFFSET]
    mov rdx, rcx
    mov rax, SYS_WRITE
    syscall
    add r14, rax

.skip_dispatch:
    ; --- Write VSA basis vectors (2MB) ---
    mov rdi, r13
    lea rsi, [rbx + VSA_OFFSET]
    mov rdx, VSA_VEC_BYTES * 256  ; 256 basis vectors
    mov rax, SYS_WRITE
    syscall
    add r14, rax

    ; --- Write holographic traces (2MB) ---
    mov rdi, r13
    lea rsi, [rbx + HOLO_OFFSET]
    mov rdx, HOLO_TOTAL
    mov rax, SYS_WRITE
    syscall
    add r14, rax

    ; --- Write vocabulary ---
    ; Size = vocab_count * VOCAB_ENTRY_SIZE
    mov ecx, [rbx + STATE_OFFSET + ST_VOCAB_COUNT]
    test ecx, ecx
    jz .skip_vocab
    imul rcx, rcx, VOCAB_ENTRY_SIZE
    mov rdi, r13
    lea rsi, [rbx + VOCAB_OFFSET]
    mov rdx, rcx
    mov rax, SYS_WRITE
    syscall
    add r14, rax
.skip_vocab:
    ; Close file
    mov rdi, r13
    mov rax, SYS_CLOSE
    syscall

    ; Print done
    lea rdi, [rel save_done]
    call print_cstr
    mov rdi, r14
    call print_u64
    call print_newline

    pop r14
    pop r13
    pop r12
    pop rbx
    ret

.save_error:
    lea rdi, [rel save_err]
    call print_cstr
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; persist_load(filename)
;; rdi=null-terminated filename string
;; Restores surface state from file
;; ============================================================
global persist_load
persist_load:
    push rbx
    push r12
    push r13
    push r14

    mov r12, rdi

    ; Print load message
    lea rdi, [rel load_msg]
    call print_cstr
    mov rdi, r12
    call print_cstr
    call print_newline

    ; Open file for reading
    mov rdi, r12
    mov rsi, O_RDONLY
    xor edx, edx
    mov rax, SYS_OPEN
    syscall

    cmp rax, 0
    jl .load_error
    mov r13, rax              ; fd

    mov rbx, SURFACE_BASE
    xor r14d, r14d            ; total bytes read

    ; --- Read and verify header ---
    sub rsp, 24               ; space for magic + version + base
    mov rdi, r13
    lea rsi, [rsp]
    mov rdx, 24
    mov rax, SYS_READ
    syscall
    add r14, rax

    ; Check magic
    cmp dword [rsp], 'UHMA'
    jne .bad_magic

    ; Check version (accept 1 or 2)
    cmp qword [rsp + 8], 2
    ja .bad_magic
    cmp qword [rsp + 8], 0
    je .bad_magic

    ; Check base address matches
    mov rax, SURFACE_BASE
    cmp [rsp + 16], rax
    jne .bad_magic

    add rsp, 24

    ; --- Read state block ---
    mov rdi, r13
    lea rsi, [rbx + STATE_OFFSET]
    mov rdx, STATE_SIZE
    mov rax, SYS_READ
    syscall
    add r14, rax

    ; --- Read region table ---
    mov rdi, r13
    lea rsi, [rbx + REGION_TABLE_OFFSET]
    mov rdx, REGION_TABLE_SIZE
    mov rax, SYS_READ
    syscall
    add r14, rax

    ; --- Read dispatch region ---
    ; Calculate dispatch size from saved dispatch_ptr
    lea rax, [rbx + STATE_OFFSET + ST_DISPATCH_PTR]
    mov rcx, [rax]            ; dispatch_ptr (absolute)
    sub rcx, rbx
    sub rcx, DISPATCH_OFFSET  ; size = ptr - dispatch_base
    test rcx, rcx
    jle .skip_dispatch_read

    mov rdi, r13
    lea rsi, [rbx + DISPATCH_OFFSET]
    mov rdx, rcx
    mov rax, SYS_READ
    syscall
    add r14, rax

.skip_dispatch_read:
    ; --- Read VSA basis vectors (2MB) ---
    mov rdi, r13
    lea rsi, [rbx + VSA_OFFSET]
    mov rdx, VSA_VEC_BYTES * 256
    mov rax, SYS_READ
    syscall
    add r14, rax

    ; --- Read holographic traces (2MB) ---
    mov rdi, r13
    lea rsi, [rbx + HOLO_OFFSET]
    mov rdx, HOLO_TOTAL
    mov rax, SYS_READ
    syscall
    add r14, rax

    ; --- Read vocabulary ---
    ; Size = vocab_count * VOCAB_ENTRY_SIZE (vocab_count already loaded with state)
    mov ecx, [rbx + STATE_OFFSET + ST_VOCAB_COUNT]
    test ecx, ecx
    jz .read_done
    imul rcx, rcx, VOCAB_ENTRY_SIZE
    mov rdi, r13
    lea rsi, [rbx + VOCAB_OFFSET]
    mov rdx, rcx
    mov rax, SYS_READ
    syscall
    add r14, rax

.read_done:
    ; Close file
    mov rdi, r13
    mov rax, SYS_CLOSE
    syscall

    ; Fire restore hook
    mov edi, HOOK_ON_RESTORE
    xor esi, esi
    call fire_hook

    ; Print done
    lea rdi, [rel load_done]
    call print_cstr
    mov rdi, r14
    call print_u64
    call print_newline

    pop r14
    pop r13
    pop r12
    pop rbx
    ret

.bad_magic:
    add rsp, 24
    ; Close file
    mov rdi, r13
    mov rax, SYS_CLOSE
    syscall
    lea rdi, [rel bad_magic_msg]
    call print_cstr
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

.load_error:
    lea rdi, [rel load_err]
    call print_cstr
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

section .rodata
    bad_magic_msg:  db "[PERSIST] Error: invalid file (bad magic/version/base)", 10, 0
