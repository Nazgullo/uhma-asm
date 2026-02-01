; persist.asm — Save/restore surface state to/from file
;
; @entry persist_save(rdi=filename) -> void         ; save surface to file
; @entry persist_load(rdi=filename) -> void         ; restore from file
; @entry gene_export(rdi=filename) -> void          ; export gene pool
; @entry gene_import(rdi=filename) -> void          ; import gene pool
; @entry gene_auto_export() -> void                 ; periodic auto-save
; @entry gene_scan_library(rdi=dir_path) -> void    ; scan for importable genes
;
; @calls fire_hook (HOOK_SAVE, HOOK_LOAD)
; @calledby repl.asm:save/load, boot.asm:auto-restore
;
; GOTCHAS:
;   - File format: magic "UHMA" + ver(2) + state + regions + dispatch + holo
;   - Gene export: portable JSON-like (ctx_hash, token_id, fitness, metadata)
;   - HOOK_SAVE fires before save, HOOK_LOAD fires after successful load
;   - Version 2 includes holographic arena and vocabulary
;
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
    ; NOTE: VSA_OFFSET = 0x80000000 sign-extends in lea. Use register arithmetic.
    mov rdi, r13
    mov rsi, rbx
    mov rcx, VSA_OFFSET
    add rsi, rcx                  ; 64-bit offset via register
    mov rdx, VSA_VEC_BYTES * 256  ; 256 basis vectors
    mov rax, SYS_WRITE
    syscall
    add r14, rax

    ; --- Write holographic traces (2MB) ---
    mov rdi, r13
    mov rsi, rbx
    mov rcx, HOLO_OFFSET
    add rsi, rcx                  ; 64-bit offset via register
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
    ; NOTE: VOCAB_OFFSET = 0xC0000000 sign-extends in lea. Use register arithmetic.
    mov rsi, rbx
    mov r8, VOCAB_OFFSET
    add rsi, r8
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
    ; NOTE: VSA_OFFSET = 0x80000000 sign-extends in lea. Use register arithmetic.
    mov rdi, r13
    mov rsi, rbx
    mov rcx, VSA_OFFSET
    add rsi, rcx                  ; 64-bit offset via register
    mov rdx, VSA_VEC_BYTES * 256
    mov rax, SYS_READ
    syscall
    add r14, rax

    ; --- Read holographic traces (2MB) ---
    mov rdi, r13
    mov rsi, rbx
    mov rcx, HOLO_OFFSET
    add rsi, rcx                  ; 64-bit offset via register
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
    ; NOTE: VOCAB_OFFSET = 0xC0000000 sign-extends in lea. Use register arithmetic.
    mov rsi, rbx
    mov r8, VOCAB_OFFSET
    add rsi, r8
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

;; ============================================================
;; SPORE SYSTEM: Granular Genetics
;; "Genes" are standalone, serializable units of function.
;; Knowledge survives the death of the process.
;; ============================================================

section .data
    gene_magic:     db "GENE"       ; 4-byte magic header
    gene_version:   dd 1            ; gene format version

    gene_export_msg: db "[SPORE] Exporting gene: ", 0
    gene_import_msg: db "[SPORE] Importing gene: ", 0
    gene_success:    db " OK", 10, 0
    gene_fail:       db " FAILED", 10, 0
    gene_stable:     db "[SPORE] Region stable — auto-exporting", 10, 0
    gene_library:    db "/.uhma/library/", 0  ; relative to $HOME

    ; Gene file structure:
    ;   [0-3]   Magic "GENE" (4 bytes)
    ;   [4-7]   Version (u32)
    ;   [8-11]  Context hash (u32) - the trigger pattern
    ;   [12-15] Token ID (u32) - what this gene predicts
    ;   [16-23] Valence (f64) - success/failure emotional charge
    ;   [24-27] Hits (u32)
    ;   [28-31] Misses (u32)
    ;   [32-35] Birth step (u32)
    ;   [36-39] Code length (u32)
    ;   [40...]  Code bytes (position-independent)

    align 8
    gene_header_size: equ 40

section .text

extern holo_gen_vec
extern holo_store
extern holo_query_valence
extern vsa_extract_valence

;; ============================================================
;; gene_export(region_ptr, filename) -> eax (0=success, -1=fail)
;; rdi=region header pointer, rsi=output filename
;; Exports a region as a .gene file (portable knowledge unit)
;; ============================================================
global gene_export
gene_export:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 64               ; local gene header buffer

    mov r12, rdi              ; region ptr
    mov r13, rsi              ; filename

    ; Print export message
    lea rdi, [rel gene_export_msg]
    call print_cstr
    mov rdi, r13
    call print_cstr

    ; Extract region info
    movzx r14d, word [r12 + RHDR_CODE_LEN]  ; code length
    test r14d, r14d
    jz .export_fail

    ; Build gene header
    ; Magic
    mov dword [rsp], 'GENE'
    ; Version
    mov dword [rsp + 4], 1
    ; Context hash (from code: CMP EAX, imm32 at offset 1)
    mov eax, [r12 + RHDR_SIZE + 1]
    mov [rsp + 8], eax
    ; Token ID (from code: MOV EAX, imm32 at offset 8)
    mov eax, [r12 + RHDR_SIZE + 8]
    mov [rsp + 12], eax
    ; Valence (query from holographic memory using ctx_hash)
    mov edi, [rsp + 8]        ; ctx_hash
    push r12
    push r13
    sub rsp, 8                ; alignment
    call holo_query_valence   ; defined in vsa.asm
    add rsp, 8
    pop r13
    pop r12
    movsd [rsp + 16], xmm0
    ; Hits
    mov eax, [r12 + RHDR_HITS]
    mov [rsp + 24], eax
    ; Misses
    mov eax, [r12 + RHDR_MISSES]
    mov [rsp + 28], eax
    ; Birth step
    mov eax, [r12 + RHDR_BIRTH]
    mov [rsp + 32], eax
    ; Code length
    mov [rsp + 36], r14d

    ; Open file for writing
    mov rdi, r13
    mov rsi, O_WRONLY | O_CREAT | O_TRUNC
    mov rdx, FILE_MODE
    mov rax, SYS_OPEN
    syscall
    test rax, rax
    js .export_fail
    mov r15, rax              ; fd

    ; Write header
    mov rdi, r15
    lea rsi, [rsp]
    mov rdx, gene_header_size
    mov rax, SYS_WRITE
    syscall
    cmp rax, gene_header_size
    jne .export_close_fail

    ; Write code
    mov rdi, r15
    lea rsi, [r12 + RHDR_SIZE]
    mov edx, r14d
    mov rax, SYS_WRITE
    syscall
    cmp eax, r14d
    jne .export_close_fail

    ; Close file
    mov rdi, r15
    mov rax, SYS_CLOSE
    syscall

    lea rdi, [rel gene_success]
    call print_cstr

    xor eax, eax              ; return success
    jmp .export_done

.export_close_fail:
    mov rdi, r15
    mov rax, SYS_CLOSE
    syscall

.export_fail:
    lea rdi, [rel gene_fail]
    call print_cstr
    mov eax, -1

.export_done:
    add rsp, 64
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; gene_import(filename) -> eax (0=success, -1=fail)
;; rdi=gene filename
;; Imports a .gene file and creates a new region from it.
;; The gene is "activated" — becomes live code in this instance.
;; ============================================================
global gene_import
extern region_alloc
extern emit_dispatch_pattern

gene_import:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 128              ; local buffer for header + small code

    mov r12, rdi              ; filename

    ; Print import message
    lea rdi, [rel gene_import_msg]
    call print_cstr
    mov rdi, r12
    call print_cstr

    ; Open file for reading
    mov rdi, r12
    mov rsi, O_RDONLY
    xor edx, edx
    mov rax, SYS_OPEN
    syscall
    test rax, rax
    js .import_fail
    mov r13, rax              ; fd

    ; Read header
    mov rdi, r13
    lea rsi, [rsp]
    mov rdx, gene_header_size
    mov rax, SYS_READ
    syscall
    cmp rax, gene_header_size
    jne .import_close_fail

    ; Verify magic
    cmp dword [rsp], 'GENE'
    jne .import_close_fail

    ; Extract info
    mov r14d, [rsp + 8]       ; ctx_hash
    mov r15d, [rsp + 12]      ; token_id
    mov ebx, [rsp + 36]       ; code_length

    ; Read code into buffer (if small enough)
    cmp ebx, 80               ; max inline code size
    ja .import_close_fail

    mov rdi, r13
    lea rsi, [rsp + 48]       ; code buffer
    mov edx, ebx
    mov rax, SYS_READ
    syscall
    cmp eax, ebx
    jne .import_close_fail

    ; Close file
    mov rdi, r13
    mov rax, SYS_CLOSE
    syscall

    ; Store in holographic memory with valence from gene
    mov edi, r14d             ; ctx_hash
    mov esi, r15d             ; token_id
    movsd xmm0, [rsp + 16]    ; valence from gene
    call holo_store

    ; Create region using emit_dispatch_pattern
    mov edi, r14d             ; ctx_hash
    mov esi, r15d             ; token_id
    mov edx, [rsp + 32]       ; birth_step (from gene)
    call emit_dispatch_pattern

    lea rdi, [rel gene_success]
    call print_cstr

    xor eax, eax              ; return success
    jmp .import_done

.import_close_fail:
    mov rdi, r13
    mov rax, SYS_CLOSE
    syscall

.import_fail:
    lea rdi, [rel gene_fail]
    call print_cstr
    mov eax, -1

.import_done:
    add rsp, 128
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; gene_auto_export(region_ptr) -> eax (0=exported, 1=not stable enough)
;; rdi=region header pointer
;; Checks if region is stable enough for auto-export to library.
;; Criteria: Age > 100 steps, Accuracy > 80%, Hits > 10
;; ============================================================
global gene_auto_export
gene_auto_export:
    push rbx
    push r12
    sub rsp, 280              ; filename buffer

    mov r12, rdi              ; region ptr
    mov rbx, SURFACE_BASE

    ; Check age (birth_step vs global_step)
    mov eax, [r12 + RHDR_BIRTH]
    mov ecx, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    sub ecx, eax              ; age = global_step - birth
    cmp ecx, 100              ; require age > 100
    jl .not_stable

    ; Check hits
    mov eax, [r12 + RHDR_HITS]
    cmp eax, 10               ; require > 10 hits
    jl .not_stable

    ; Check accuracy = hits / (hits + misses)
    mov ecx, [r12 + RHDR_MISSES]
    add ecx, eax              ; total = hits + misses
    jz .not_stable            ; avoid div by zero
    cvtsi2sd xmm0, eax        ; hits
    cvtsi2sd xmm1, ecx        ; total
    divsd xmm0, xmm1          ; accuracy
    mov rax, 0x3FE999999999999A  ; 0.8 f64
    movq xmm1, rax
    ucomisd xmm0, xmm1
    jb .not_stable            ; require > 80%

    ; --- Stable enough: export to library ---
    lea rdi, [rel gene_stable]
    call print_cstr

    ; Build filename: ctx_hash.gene
    mov eax, [r12 + RHDR_SIZE + 1]  ; ctx_hash
    ; Simple hex filename
    lea rdi, [rsp]
    mov dword [rdi], '/tmp'
    mov dword [rdi + 4], '/gen'
    mov dword [rdi + 8], 'e_'
    ; Convert hash to hex string (simplified)
    add rdi, 10
    mov ecx, 8
.hex_loop:
    mov edx, eax
    shr eax, 4
    and edx, 0xF
    cmp edx, 10
    jl .hex_digit
    add edx, 'a' - 10
    jmp .hex_store
.hex_digit:
    add edx, '0'
.hex_store:
    mov [rdi], dl
    inc rdi
    dec ecx
    jnz .hex_loop
    mov dword [rdi], '.gen'
    mov word [rdi + 4], 'e'
    mov byte [rdi + 5], 0

    ; Export
    mov rdi, r12              ; region ptr
    lea rsi, [rsp]            ; filename
    call gene_export

    jmp .auto_done

.not_stable:
    mov eax, 1                ; not stable enough

.auto_done:
    add rsp, 280
    pop r12
    pop rbx
    ret

;; ============================================================
;; gene_scan_library() -> eax (number imported)
;; Scans ~/.uhma/library/ for .gene files and imports them.
;; Called at startup to "infect" new instances with culture.
;; ============================================================
global gene_scan_library
gene_scan_library:
    ; Stub for now — full implementation would use getdents64
    ; to enumerate /tmp/gene_*.gene files
    xor eax, eax
    ret
