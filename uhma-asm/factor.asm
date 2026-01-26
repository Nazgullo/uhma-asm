; factor.asm — Recursive Schema Hierarchy: Subroutine extraction and factoring
;
; This module enables code reuse through suffix factoring:
; - Scans regions for common code endings
; - Extracts shared suffixes into callable RTYPE_SUBROUTINE regions
; - Rewrites callers to CALL the subroutine instead of duplicating code
;
; The system becomes hierarchical: regions can CALL other regions,
; enabling abstraction and knowledge organization.

%include "syscalls.inc"
%include "constants.inc"

section .data
    factor_hdr:         db "[FACTOR] ", 0
    factor_scan:        db "Scanning for common suffixes...", 10, 0
    factor_found:       db "Found ", 0
    factor_regions:     db " regions with ", 0
    factor_byte_suffix: db "-byte common suffix", 10, 0
    factor_creating:    db "Creating subroutine at 0x", 0
    factor_rewriting:   db "Rewriting region ", 0
    factor_to_call:     db " to CALL 0x", 0
    factor_none:        db "No factoring opportunities found", 10, 0
    factor_nl:          db 10, 0
    sub_hdr:            db "--- Subroutines ---", 10, 0
    sub_entry:          db "  Sub ", 0
    sub_at:             db " at 0x", 0
    sub_callers:        db " callers=", 0
    sub_code_hash:      db " hash=0x", 0
    sub_none:           db "  (no subroutines)", 10, 0

section .bss
    ; Subroutine table: 64 entries, each 16 bytes
    subroutine_table:   resb SUBROUTINE_TABLE_MAX * SUBROUTINE_ENTRY_SIZE
    subroutine_count:   resd 1

    ; Suffix hash registry: (u32 hash, u64 region_ptr) = 16 bytes per entry (padded)
    ; This replaces O(n²) comparison with O(n) hash + group
    suffix_registry:    resb FACTOR_SCAN_LIMIT * 16  ; hash + ptr pairs
    suffix_reg_count:   resd 1                        ; entries in registry

    ; Suffix comparison buffer (for groups found by hash)
    suffix_candidates:  resq FACTOR_SCAN_LIMIT   ; pointers to candidate regions
    suffix_count:       resd 1                    ; number of candidates
    common_suffix_len:  resd 1                    ; detected common suffix length

section .text

extern print_cstr
extern print_hex32
extern print_hex64
extern print_u64
extern print_newline
extern region_alloc
extern region_find_by_addr

;; ============================================================
;; factor_init
;; Initialize the subroutine table
;; ============================================================
global factor_init
factor_init:
    xor eax, eax
    mov dword [rel subroutine_count], eax
    ; Zero the table
    lea rdi, [rel subroutine_table]
    mov ecx, SUBROUTINE_TABLE_MAX * SUBROUTINE_ENTRY_SIZE
.zero_loop:
    mov byte [rdi], 0
    inc rdi
    dec ecx
    jnz .zero_loop
    ret

;; ============================================================
;; hash_suffix(region_ptr) → eax = hash of last SUFFIX_HASH_LEN bytes
;; Computes a simple hash of the region's code suffix.
;; rdi = region header pointer
;; Returns: eax = 32-bit hash
;; ============================================================
hash_suffix:
    movzx ecx, word [rdi + RHDR_CODE_LEN]
    cmp ecx, SUFFIX_HASH_LEN
    jge .has_enough
    ; Code too short, use what we have
    jmp .hash_start
.has_enough:
    mov ecx, SUFFIX_HASH_LEN
.hash_start:
    ; Point to last 'ecx' bytes of code
    movzx eax, word [rdi + RHDR_CODE_LEN]
    sub eax, ecx
    lea rsi, [rdi + RHDR_SIZE]
    add rsi, rax              ; rsi = start of suffix

    ; Hash: h = h * 31 + byte
    xor eax, eax
.hash_loop:
    test ecx, ecx
    jz .hash_done
    imul eax, eax, 31
    movzx edx, byte [rsi]
    add eax, edx
    inc rsi
    dec ecx
    jmp .hash_loop
.hash_done:
    ret

;; ============================================================
;; find_common_suffix()
;; Scans active DISPATCH regions for common code endings.
;; Uses O(n) hash-based grouping instead of O(n²) comparison.
;;
;; Algorithm:
;;   1. Scan regions, compute suffix hash for each → O(n)
;;   2. Store (hash, region_ptr) in registry
;;   3. Sort registry by hash → O(n log n) but bounded by FACTOR_SCAN_LIMIT
;;   4. Find runs of same hash → O(n)
;;   5. Take largest run with >= SUFFIX_MIN_CALLERS
;;
;; Returns: eax = number of regions sharing a common suffix (0 = none)
;;          suffix_candidates[] filled with region pointers
;;          common_suffix_len set to SUFFIX_HASH_LEN (fixed)
;; ============================================================
global find_common_suffix
find_common_suffix:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8          ; alignment

    mov rbx, SURFACE_BASE
    lea r12, [rbx + REGION_TABLE_OFFSET]
    mov r13d, [rbx + STATE_OFFSET + ST_REGION_COUNT]

    ; Clear buffers
    mov dword [rel suffix_count], 0
    mov dword [rel common_suffix_len], 0
    mov dword [rel suffix_reg_count], 0

    ; === Phase 1: Collect regions and compute suffix hashes → O(n) ===
    lea r14, [rel suffix_registry]
    xor ecx, ecx        ; region index
    xor r15d, r15d      ; registry count
.collect_loop:
    cmp ecx, r13d
    jge .collect_done
    cmp r15d, FACTOR_SCAN_LIMIT
    jge .collect_done

    push rcx
    imul rdi, rcx, RTE_SIZE
    add rdi, r12

    ; Check type and flags
    movzx eax, word [rdi + RTE_TYPE]
    cmp eax, RTYPE_DISPATCH
    jne .collect_next
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_CONDEMNED
    jnz .collect_next

    ; Get region header pointer
    mov rsi, [rdi + RTE_ADDR]
    movzx eax, word [rsi + RHDR_CODE_LEN]
    cmp eax, SUFFIX_MIN_LEN + 5     ; need enough code to factor
    jl .collect_next

    ; Compute suffix hash
    push rsi
    mov rdi, rsi
    call hash_suffix
    pop rsi
    ; eax = suffix hash, rsi = region ptr

    ; Store in registry: [hash(4) + padding(4) + ptr(8)] = 16 bytes
    imul edi, r15d, 16
    add rdi, r14
    mov [rdi + 0], eax            ; hash
    mov [rdi + 8], rsi            ; region ptr
    inc r15d

.collect_next:
    pop rcx
    inc ecx
    jmp .collect_loop

.collect_done:
    mov [rel suffix_reg_count], r15d

    ; Need at least SUFFIX_MIN_CALLERS regions to factor
    cmp r15d, SUFFIX_MIN_CALLERS
    jl .no_suffix

    ; === Phase 2: Sort registry by hash (insertion sort) → O(n²) worst but small n ===
    ; For FACTOR_SCAN_LIMIT entries, insertion sort is fine
    mov ecx, 1          ; i = 1
.sort_outer:
    cmp ecx, r15d
    jge .sort_done

    ; Load entry[i] into temp
    imul eax, ecx, 16
    add rax, r14
    mov r8d, [rax + 0]        ; temp_hash
    mov r9, [rax + 8]         ; temp_ptr

    mov edx, ecx              ; j = i
.sort_inner:
    test edx, edx
    jz .sort_insert

    ; Compare entry[j-1].hash with temp_hash
    lea eax, [edx - 1]
    imul eax, eax, 16
    add rax, r14
    mov r10d, [rax + 0]       ; entry[j-1].hash
    cmp r10d, r8d
    jle .sort_insert          ; entry[j-1] <= temp, done shifting

    ; Shift entry[j-1] to entry[j]
    imul edi, edx, 16
    add rdi, r14
    mov [rdi + 0], r10d
    mov r11, [rax + 8]
    mov [rdi + 8], r11

    dec edx
    jmp .sort_inner

.sort_insert:
    ; Insert temp at position j
    imul eax, edx, 16
    add rax, r14
    mov [rax + 0], r8d
    mov [rax + 8], r9

    inc ecx
    jmp .sort_outer

.sort_done:
    ; === Phase 3: Find largest run of same hash → O(n) ===
    xor ecx, ecx              ; current position
    xor r8d, r8d              ; best run start
    xor r9d, r9d              ; best run length

.find_runs:
    cmp ecx, r15d
    jge .runs_done

    ; Get current hash
    imul eax, ecx, 16
    add rax, r14
    mov r10d, [rax + 0]       ; current hash

    ; Count run length
    mov edx, ecx              ; run start
    mov r11d, 0               ; run length
.count_run:
    cmp edx, r15d
    jge .run_ended
    imul eax, edx, 16
    add rax, r14
    cmp [rax + 0], r10d
    jne .run_ended
    inc r11d
    inc edx
    jmp .count_run

.run_ended:
    ; Is this run better than best?
    cmp r11d, r9d
    jle .not_better
    mov r8d, ecx              ; best_start = run_start
    mov r9d, r11d             ; best_len = run_len
.not_better:
    mov ecx, edx              ; skip past this run
    jmp .find_runs

.runs_done:
    ; Check if best run meets minimum
    cmp r9d, SUFFIX_MIN_CALLERS
    jl .no_suffix

    ; === Phase 4: Copy best run to suffix_candidates ===
    lea rdi, [rel suffix_candidates]
    xor ecx, ecx
.copy_run:
    cmp ecx, r9d
    jge .copy_done

    ; Get region ptr from registry[best_start + i]
    lea eax, [r8d + ecx]
    imul eax, eax, 16
    add rax, r14
    mov rsi, [rax + 8]        ; region ptr
    mov [rdi + rcx * 8], rsi
    inc ecx
    jmp .copy_run

.copy_done:
    mov [rel suffix_count], r9d
    mov dword [rel common_suffix_len], SUFFIX_HASH_LEN
    mov eax, r9d
    jmp .done

.no_suffix:
    xor eax, eax

.done:
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; emit_subroutine(suffix_ptr, suffix_len)
;; Creates a new RTYPE_SUBROUTINE region containing the given code.
;; rdi = pointer to suffix code bytes
;; esi = length of suffix
;; Returns: rax = pointer to new subroutine region header
;; ============================================================
global emit_subroutine
emit_subroutine:
    push rbx
    push r12
    push r13

    mov r12, rdi            ; suffix source
    mov r13d, esi           ; suffix length

    ; Allocate region for subroutine
    ; The subroutine code is just the suffix bytes (already ends with RET)
    mov rdi, r13            ; code size = suffix length
    mov rsi, RTYPE_SUBROUTINE
    mov rax, SURFACE_BASE
    mov edx, [rax + STATE_OFFSET + ST_GLOBAL_STEP]
    call region_alloc
    mov rbx, rax            ; save header ptr

    ; Copy suffix code into the new region
    lea rdi, [rbx + RHDR_SIZE]
    mov rsi, r12
    mov ecx, r13d
.copy_loop:
    test ecx, ecx
    jz .copy_done
    mov al, [rsi]
    mov [rdi], al
    inc rsi
    inc rdi
    dec ecx
    jmp .copy_loop
.copy_done:

    ; Register in subroutine table
    mov eax, [rel subroutine_count]
    cmp eax, SUBROUTINE_TABLE_MAX
    jge .table_full

    lea rdi, [rel subroutine_table]
    imul edx, eax, SUBROUTINE_ENTRY_SIZE
    add rdi, rdx

    mov [rdi + STE_PTR], rbx
    mov dword [rdi + STE_CALLER_COUNT], 0

    ; Compute simple hash of code for deduplication
    xor eax, eax
    lea rsi, [rbx + RHDR_SIZE]
    mov ecx, r13d
.hash_loop:
    test ecx, ecx
    jz .hash_done
    movzx edx, byte [rsi]
    imul eax, eax, 31
    add eax, edx
    inc rsi
    dec ecx
    jmp .hash_loop
.hash_done:
    mov [rdi + STE_CODE_HASH], eax

    inc dword [rel subroutine_count]

.table_full:
    ; Print creation message
    push rbx
    lea rdi, [rel factor_hdr]
    call print_cstr
    lea rdi, [rel factor_creating]
    call print_cstr
    mov rdi, rbx
    add rdi, RHDR_SIZE
    call print_hex64
    lea rdi, [rel factor_nl]
    call print_cstr
    pop rbx

    mov rax, rbx
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; rewrite_to_call(region_ptr, subroutine_ptr, suffix_len)
;; Rewrites a region to CALL the subroutine instead of having inline code.
;; rdi = region header to rewrite
;; rsi = subroutine to call
;; edx = suffix length (bytes to replace with CALL)
;; ============================================================
global rewrite_to_call
rewrite_to_call:
    push rbx
    push r12
    push r13
    push r14

    mov rbx, rdi            ; region header
    mov r12, rsi            ; subroutine header
    mov r13d, edx           ; suffix length

    ; Get current code length
    movzx r14d, word [rbx + RHDR_CODE_LEN]

    ; Compute where to insert CALL (replace last suffix_len bytes)
    ; New structure: original_prefix + CALL rel32 (5 bytes) + RET (1 byte)
    ; The subroutine handles the RET, so we need: prefix + CALL + RET

    ; Calculate offset where suffix starts (this is where we put CALL)
    mov eax, r14d
    sub eax, r13d           ; offset = code_len - suffix_len

    lea rdi, [rbx + RHDR_SIZE]
    add rdi, rax            ; point to where suffix was

    ; Write CALL rel32 instruction (E8 rel32)
    ; rel32 = target - (current + 5)
    ; target = subroutine_code = r12 + RHDR_SIZE
    lea rcx, [r12 + RHDR_SIZE]   ; call target
    lea rdx, [rdi + 5]           ; address after CALL instruction
    sub rcx, rdx                 ; relative offset

    mov byte [rdi], 0xE8         ; CALL opcode
    mov [rdi + 1], ecx           ; rel32 offset

    ; Write RET after CALL
    mov byte [rdi + 5], 0xC3

    ; Update code length: prefix_len + 6 (CALL + RET)
    mov eax, r14d
    sub eax, r13d
    add eax, 6
    mov word [rbx + RHDR_CODE_LEN], ax

    ; NOP out remaining bytes (if any)
    movzx ecx, word [rbx + RHDR_CODE_LEN]
    lea rsi, [rbx + RHDR_SIZE]
    add rsi, rcx
    mov edx, r14d
    sub edx, ecx                ; bytes to NOP
.nop_loop:
    test edx, edx
    jz .nop_done
    mov byte [rsi], 0x90
    inc rsi
    dec edx
    jmp .nop_loop
.nop_done:

    ; Increment caller count in subroutine table
    mov eax, [rel subroutine_count]
    lea rsi, [rel subroutine_table]
    xor ecx, ecx
.find_sub:
    cmp ecx, eax
    jge .sub_not_found
    imul edx, ecx, SUBROUTINE_ENTRY_SIZE
    cmp [rsi + rdx + STE_PTR], r12
    je .found_sub
    inc ecx
    jmp .find_sub
.found_sub:
    inc dword [rsi + rdx + STE_CALLER_COUNT]
.sub_not_found:

    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; factor_suffix()
;; Main factoring routine: finds common suffixes and creates subroutines.
;; Called from observe_cycle periodically.
;; Returns: eax = number of regions rewritten
;; ============================================================
global factor_suffix
factor_suffix:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8

    ; Print scan message
    lea rdi, [rel factor_hdr]
    call print_cstr
    lea rdi, [rel factor_scan]
    call print_cstr

    ; Find common suffix
    call find_common_suffix
    test eax, eax
    jz .no_opportunities

    mov r14d, eax               ; number of candidates
    mov r15d, [rel common_suffix_len]

    ; Print findings
    lea rdi, [rel factor_hdr]
    call print_cstr
    lea rdi, [rel factor_found]
    call print_cstr
    mov edi, r14d
    call print_u64
    lea rdi, [rel factor_regions]
    call print_cstr
    mov edi, r15d
    call print_u64
    lea rdi, [rel factor_byte_suffix]
    call print_cstr

    ; Get the suffix from first candidate
    lea rbx, [rel suffix_candidates]
    mov r12, [rbx]              ; first candidate
    movzx eax, word [r12 + RHDR_CODE_LEN]
    sub eax, r15d               ; suffix starts at code_len - suffix_len
    lea rdi, [r12 + RHDR_SIZE]
    add rdi, rax                ; pointer to suffix bytes
    mov esi, r15d               ; suffix length

    ; Create the subroutine
    call emit_subroutine
    mov r13, rax                ; subroutine header

    ; Rewrite all candidates to call the subroutine
    xor r14d, r14d              ; rewritten count
    xor ecx, ecx
.rewrite_loop:
    cmp ecx, [rel suffix_count]
    jge .rewrite_done

    push rcx
    lea rbx, [rel suffix_candidates]
    mov rdi, [rbx + rcx * 8]    ; region to rewrite
    mov rsi, r13                ; subroutine
    mov edx, r15d               ; suffix length
    call rewrite_to_call
    inc r14d
    pop rcx

    inc ecx
    jmp .rewrite_loop

.rewrite_done:
    mov eax, r14d
    jmp .done

.no_opportunities:
    lea rdi, [rel factor_hdr]
    call print_cstr
    lea rdi, [rel factor_none]
    call print_cstr
    xor eax, eax

.done:
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; verify_valid_call(call_target)
;; Checks if a CALL target is valid (points to a known subroutine).
;; rdi = absolute target address
;; Returns: eax = 1 if valid, 0 if invalid
;; ============================================================
global verify_valid_call
verify_valid_call:
    push rbx

    mov rbx, rdi

    ; Check if target is within the SURFACE dispatch region
    mov rax, SURFACE_BASE
    add rax, DISPATCH_OFFSET
    cmp rbx, rax
    jl .check_text

    mov rax, SURFACE_BASE
    add rax, DISPATCH_OFFSET
    add rax, DISPATCH_MAX_SIZE
    cmp rbx, rax
    jg .check_text

    ; Target is in dispatch region - check if it's a known subroutine
    ; The target should be RHDR_SIZE into a region (pointing to code)
    sub rbx, RHDR_SIZE          ; get header address

    ; Search subroutine table
    mov eax, [rel subroutine_count]
    test eax, eax
    jz .check_region

    lea rsi, [rel subroutine_table]
    xor ecx, ecx
.search_sub:
    cmp ecx, eax
    jge .check_region
    imul edx, ecx, SUBROUTINE_ENTRY_SIZE
    cmp [rsi + rdx + STE_PTR], rbx
    je .valid_subroutine
    inc ecx
    jmp .search_sub

.valid_subroutine:
    mov eax, 1
    jmp .done

.check_region:
    ; Check if it's a valid region via region_find_by_addr
    mov rdi, rbx
    call region_find_by_addr
    test rax, rax
    jz .check_text

    ; Found a region - check if it's a subroutine type
    movzx eax, word [rax + RTE_TYPE]
    cmp eax, RTYPE_SUBROUTINE
    je .valid_region
    jmp .invalid

.valid_region:
    mov eax, 1
    jmp .done

.check_text:
    ; Allow calls to .text section (system functions)
    ; .text is typically 0x401000 - 0x410000
    add rbx, RHDR_SIZE          ; restore original target
    cmp rbx, 0x401000
    jl .invalid
    cmp rbx, 0x410000
    jl .valid_text
    jmp .invalid

.valid_text:
    mov eax, 1
    jmp .done

.invalid:
    xor eax, eax

.done:
    pop rbx
    ret

;; ============================================================
;; subroutines_show
;; Display all registered subroutines (REPL command)
;; ============================================================
global subroutines_show
subroutines_show:
    push rbx
    push r12
    push r13

    lea rdi, [rel sub_hdr]
    call print_cstr

    mov r12d, [rel subroutine_count]
    test r12d, r12d
    jz .no_subs

    lea rbx, [rel subroutine_table]
    xor r13d, r13d
.show_loop:
    cmp r13d, r12d
    jge .show_done

    ; Print entry
    lea rdi, [rel sub_entry]
    call print_cstr
    mov edi, r13d
    call print_u64

    lea rdi, [rel sub_at]
    call print_cstr
    imul eax, r13d, SUBROUTINE_ENTRY_SIZE
    mov rdi, [rbx + rax + STE_PTR]
    add rdi, RHDR_SIZE          ; code address
    call print_hex64

    lea rdi, [rel sub_callers]
    call print_cstr
    imul eax, r13d, SUBROUTINE_ENTRY_SIZE
    mov edi, [rbx + rax + STE_CALLER_COUNT]
    call print_u64

    lea rdi, [rel sub_code_hash]
    call print_cstr
    imul eax, r13d, SUBROUTINE_ENTRY_SIZE
    mov edi, [rbx + rax + STE_CODE_HASH]
    call print_hex32

    call print_newline

    inc r13d
    jmp .show_loop

.no_subs:
    lea rdi, [rel sub_none]
    call print_cstr

.show_done:
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; garbage_collect_subroutines
;; Remove subroutines with zero callers
;; Returns: eax = number of subroutines removed
;; ============================================================
global garbage_collect_subroutines
garbage_collect_subroutines:
    push rbx
    push r12
    push r13

    lea rbx, [rel subroutine_table]
    mov r12d, [rel subroutine_count]
    xor r13d, r13d              ; removed count

    xor ecx, ecx                ; read index
    xor edx, edx                ; write index
.gc_loop:
    cmp ecx, r12d
    jge .gc_done

    ; Check caller count
    imul eax, ecx, SUBROUTINE_ENTRY_SIZE
    cmp dword [rbx + rax + STE_CALLER_COUNT], 0
    je .gc_remove

    ; Keep this entry - copy if needed
    cmp ecx, edx
    je .gc_no_copy

    imul eax, ecx, SUBROUTINE_ENTRY_SIZE
    imul esi, edx, SUBROUTINE_ENTRY_SIZE

    ; Copy 16 bytes
    mov rdi, [rbx + rax + 0]
    mov [rbx + rsi + 0], rdi
    mov rdi, [rbx + rax + 8]
    mov [rbx + rsi + 8], rdi

.gc_no_copy:
    inc edx                     ; write index++
    jmp .gc_next

.gc_remove:
    ; Condemn the subroutine region
    imul eax, ecx, SUBROUTINE_ENTRY_SIZE
    mov rdi, [rbx + rax + STE_PTR]
    or word [rdi + RHDR_FLAGS], RFLAG_CONDEMNED
    inc r13d

.gc_next:
    inc ecx
    jmp .gc_loop

.gc_done:
    mov [rel subroutine_count], edx
    mov eax, r13d

    pop r13
    pop r12
    pop rbx
    ret
