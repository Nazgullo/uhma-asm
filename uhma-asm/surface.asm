; surface.asm — Surface management: region allocation, headers, compaction
%include "syscalls.inc"
%include "constants.inc"

section .text

extern sys_mmap
extern sym_init

;; ============================================================
;; surface_init
;; mmap the 8GB RWX surface, zero the state block, init allocator
;; Returns: surface base in rax (or exits on failure)
;; ============================================================
global surface_init
surface_init:
    push rbx
    push r12

    ; mmap 8GB RWX anonymous private
    mov rdi, SURFACE_BASE     ; hint address
    mov rsi, SURFACE_SIZE     ; 8GB
    mov rdx, PROT_RWX
    mov rcx, MAP_PRIVATE | MAP_ANONYMOUS  ; goes to r10 in sys_mmap
    mov r8, -1                ; fd = -1 (anonymous)
    xor r9d, r9d             ; offset = 0
    call sys_mmap

    ; Check for MAP_FAILED (-1)
    cmp rax, -1
    je .mmap_fail
    ; Verify we got the requested address (or close)
    mov rbx, rax              ; save surface base

    ; Initialize dispatch allocator pointer
    ; Points to first free byte in dispatch region
    lea rcx, [rbx + DISPATCH_OFFSET]
    lea rdx, [rbx + STATE_OFFSET + ST_DISPATCH_PTR]
    mov [rdx], rcx

    ; Initialize region count to 0
    lea rdx, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov qword [rdx], 0

    ; Initialize observation interval
    lea rdx, [rbx + STATE_OFFSET + ST_OBS_INTERVAL]
    mov dword [rdx], OBSERVE_INTERVAL

    ; Initialize drive thresholds to defaults
    lea rdx, [rbx + STATE_OFFSET + ST_DRIVE_THRESH]
    mov dword [rdx + 0], THRESH_ACCURACY
    mov dword [rdx + 4], THRESH_EFFICIENCY
    mov dword [rdx + 8], THRESH_NOVELTY
    mov dword [rdx + 12], THRESH_COHERENCE

    ; Initialize global step to 0
    lea rdx, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov qword [rdx], 0

    ; Initialize context hash
    lea rdx, [rbx + STATE_OFFSET + ST_CTX_HASH]
    mov rax, FNV64_INIT
    mov [rdx], rax

    ; Zero holographic trace memory (2MB = HOLO_TOTAL, f64 vectors)
    lea rdi, [rbx + HOLO_OFFSET]
    xor eax, eax
    mov ecx, HOLO_TOTAL / 8  ; zero 8 bytes at a time (2MB / 8 = 262144 iters)
.zero_holo:
    mov [rdi], rax
    add rdi, 8
    dec ecx
    jnz .zero_holo

    ; Zero vocabulary area (first 64KB — enough for 8192 entries)
    lea rdi, [rbx + VOCAB_OFFSET]
    xor eax, eax
    mov ecx, 65536 / 8
.zero_vocab:
    mov [rdi], rax
    add rdi, 8
    dec ecx
    jnz .zero_vocab

    ; Zero holographic state fields
    mov dword [rbx + STATE_OFFSET + ST_VOCAB_COUNT], 0
    mov dword [rbx + STATE_OFFSET + ST_VOCAB_TOP_DIRTY], 0
    mov qword [rbx + STATE_OFFSET + ST_HOLO_PREDICT_SUM], 0  ; f64
    mov dword [rbx + STATE_OFFSET + ST_HOLO_PREDICT_N], 0

    ; Initialize symbolic observation system
    push rbx
    call sym_init
    pop rbx

    mov rax, rbx              ; return surface base
    pop r12
    pop rbx
    ret

.mmap_fail:
    ; Exit with error
    mov edi, 1
    mov rax, SYS_EXIT
    syscall

;; ============================================================
;; region_alloc(size, type, step)
;; rdi=code_size (excluding header), rsi=region_type, rdx=birth_step
;; Returns: rax=ptr to header (code starts at rax+RHDR_SIZE)
;; ============================================================
global region_alloc
region_alloc:
    push rbx
    push r12
    push r13
    push r14

    mov r12, rdi              ; code_size
    mov r13, rsi              ; type
    mov r14, rdx              ; birth_step

    ; Get surface base (fixed)
    mov rbx, SURFACE_BASE

    ; Get current dispatch alloc pointer
    lea rax, [rbx + STATE_OFFSET + ST_DISPATCH_PTR]
    mov rcx, [rax]            ; current alloc ptr

    ; Align to 16 bytes
    add rcx, 15
    and rcx, ~15

    ; Zero-initialize all 128 bytes of extended header
    push rdi
    push rcx
    mov rdi, rcx
    xor eax, eax
    mov ecx, RHDR_SIZE
    rep stosb
    pop rcx
    pop rdi

    ; Write core header fields
    mov dword [rcx + RHDR_HITS], 0
    mov dword [rcx + RHDR_MISSES], 0
    mov dword [rcx + RHDR_BIRTH], r14d
    mov word [rcx + RHDR_CODE_LEN], r12w
    mov word [rcx + RHDR_FLAGS], RFLAG_ACTIVE
    ; Connection ptrs, weights, dynamics all zeroed by rep stosb above

    ; Save the region in region table
    push rcx                  ; save header ptr
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov rdi, [rax]            ; current count
    cmp rdi, REGION_TABLE_MAX
    jge .table_full

    ; Calculate table entry address
    lea rsi, [rbx + REGION_TABLE_OFFSET]
    imul rdx, rdi, RTE_SIZE
    add rsi, rdx              ; entry ptr

    ; Fill entry
    mov [rsi + RTE_ADDR], rcx
    mov eax, r12d
    add eax, RHDR_SIZE
    mov [rsi + RTE_LEN], eax
    mov [rsi + RTE_TYPE], r13w
    mov word [rsi + RTE_FLAGS], RFLAG_ACTIVE
    mov dword [rsi + RTE_HITS], 0
    mov dword [rsi + RTE_MISSES], 0
    mov [rsi + RTE_BIRTH], r14d

    ; Increment region count
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    inc qword [rax]

.table_full:
    pop rcx                   ; restore header ptr

    ; Update alloc pointer past this region
    lea rax, [rcx + RHDR_SIZE]
    add rax, r12              ; past the code
    lea rdx, [rbx + STATE_OFFSET + ST_DISPATCH_PTR]
    mov [rdx], rax

    mov rax, rcx              ; return header ptr
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; region_condemn(header_ptr)
;; rdi=ptr to region header
;; Sets CONDEMNED flag
;; ============================================================
global region_condemn
region_condemn:
    or word [rdi + RHDR_FLAGS], RFLAG_CONDEMNED
    ret

;; ============================================================
;; region_compact
;; Walk region table, remove CONDEMNED entries, update pointers
;; Returns: number of reclaimed regions in rax
;; ============================================================
global region_compact
region_compact:
    push rbx
    push r12
    push r13
    push r14

    mov rbx, SURFACE_BASE
    xor r14d, r14d            ; reclaimed count

    lea r12, [rbx + REGION_TABLE_OFFSET]  ; table base
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov r13, [rax]            ; total entries

    xor ecx, ecx              ; read index
    xor edx, edx              ; write index

.scan:
    cmp rcx, r13
    jge .done

    ; Get entry at read index
    imul rdi, rcx, RTE_SIZE
    add rdi, r12

    ; Check if CONDEMNED
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_CONDEMNED
    jnz .skip_entry

    ; Keep this entry — copy to write position if different
    cmp rcx, rdx
    je .no_copy

    ; Copy entry
    imul rsi, rdx, RTE_SIZE
    add rsi, r12
    push rcx
    mov ecx, RTE_SIZE
    push rdi
    push rsi
    ; Manual copy (32 bytes)
    mov rax, [rdi]
    mov [rsi], rax
    mov rax, [rdi + 8]
    mov [rsi + 8], rax
    mov rax, [rdi + 16]
    mov [rsi + 16], rax
    mov rax, [rdi + 24]
    mov [rsi + 24], rax
    pop rsi
    pop rdi
    pop rcx

.no_copy:
    inc rdx                   ; write index++
    jmp .next

.skip_entry:
    ; NOP out the condemned region's code (write 0xCC = INT3)
    mov rsi, [rdi + RTE_ADDR]
    movzx eax, word [rsi + RHDR_CODE_LEN]
    lea rsi, [rsi + RHDR_SIZE]
    test eax, eax
    jz .no_nop
.nop_loop:
    mov byte [rsi], 0xCC
    inc rsi
    dec eax
    jnz .nop_loop
.no_nop:
    inc r14d                  ; reclaimed++

.next:
    inc rcx
    jmp .scan

.done:
    ; Update region count
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov [rax], rdx

    mov rax, r14              ; return reclaimed count
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; region_find_by_addr(addr)
;; rdi=address to find in region table
;; Returns: rax=ptr to table entry, or 0 if not found
;; ============================================================
global region_find_by_addr
region_find_by_addr:
    push rbx
    mov rbx, SURFACE_BASE

    lea rsi, [rbx + REGION_TABLE_OFFSET]
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov rcx, [rax]

    xor edx, edx
.loop:
    cmp rdx, rcx
    jge .not_found
    imul rax, rdx, RTE_SIZE
    add rax, rsi
    cmp [rax + RTE_ADDR], rdi
    je .found
    inc rdx
    jmp .loop

.not_found:
    xor eax, eax
.found:
    pop rbx
    ret

;; ============================================================
;; get_surface_base → rax
;; Returns the fixed surface base address
;; ============================================================
global get_surface_base
get_surface_base:
    mov rax, SURFACE_BASE
    ret

;; ============================================================
;; get_state_ptr → rax
;; Returns pointer to state block
;; ============================================================
global get_state_ptr
get_state_ptr:
    mov rax, SURFACE_BASE + STATE_OFFSET
    ret

;; ============================================================
;; get_vsa_base → rax
;; Returns pointer to VSA arena
;; ============================================================
global get_vsa_base
get_vsa_base:
    mov rax, SURFACE_BASE + VSA_OFFSET
    ret

;; ============================================================
;; region_merge_pass
;; Scans dispatch regions for mergeable pairs (same token_id,
;; similar ctx_hash — upper 20 bits match). Merges hit counts
;; into the stronger region, condemns the weaker, then compacts.
;; Returns: rax = number of regions reclaimed
;; ============================================================
global region_merge_pass
region_merge_pass:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8                ; alignment + merged count at [rsp]

    mov rbx, SURFACE_BASE
    mov dword [rsp], 0        ; merged count

    lea r12, [rbx + REGION_TABLE_OFFSET]
    mov r13d, [rbx + STATE_OFFSET + ST_REGION_COUNT]

    ; Outer loop: for each dispatch region i
    xor ecx, ecx             ; i = 0
.merge_outer:
    cmp ecx, r13d
    jge .merge_compact

    ; Get region i entry
    push rcx
    imul rdi, rcx, RTE_SIZE
    add rdi, r12
    movzx eax, word [rdi + RTE_TYPE]
    cmp eax, RTYPE_DISPATCH
    jne .merge_next_outer
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_CONDEMNED
    jnz .merge_next_outer

    ; Get region i's header → extract ctx_hash and token_id from code
    mov r14, [rdi + RTE_ADDR]         ; header ptr
    mov r15d, [r14 + RHDR_SIZE + 1]   ; ctx_hash (bytes 1-4 of code)
    mov ebx, [r14 + RHDR_SIZE + 8]    ; token_id (bytes 8-11 of code)

    ; Inner loop: for each region j > i
    pop rcx
    push rcx
    mov edx, ecx
    inc edx                   ; j = i + 1

.merge_inner:
    cmp edx, r13d
    jge .merge_next_outer

    push rdx
    imul rdi, rdx, RTE_SIZE
    add rdi, r12
    movzx eax, word [rdi + RTE_TYPE]
    cmp eax, RTYPE_DISPATCH
    jne .merge_skip_inner
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_CONDEMNED
    jnz .merge_skip_inner

    ; Get region j's header
    mov rsi, [rdi + RTE_ADDR]
    ; Same token_id?
    cmp [rsi + RHDR_SIZE + 8], ebx
    jne .merge_skip_inner

    ; Similar ctx_hash? (upper 20 bits match)
    mov eax, [rsi + RHDR_SIZE + 1]    ; j's ctx_hash
    xor eax, r15d                      ; difference
    and eax, 0xFFFFF000                ; mask upper 20 bits
    jnz .merge_skip_inner              ; if any upper bit differs, not similar

    ; --- MERGE: same token, similar context ---
    ; Compare hit counts: keep the stronger one
    mov eax, [r14 + RHDR_HITS]        ; i's hits
    mov ecx, [rsi + RHDR_HITS]        ; j's hits
    cmp eax, ecx
    jge .condemn_j

    ; j is stronger — merge i into j, condemn i
    add [rsi + RHDR_HITS], eax        ; j.hits += i.hits
    mov rdi, r14
    or word [rdi + RHDR_FLAGS], RFLAG_CONDEMNED
    ; NOP out i's code
    movzx eax, word [rdi + RHDR_CODE_LEN]
    lea rdi, [rdi + RHDR_SIZE]
    test eax, eax
    jz .merge_did_merge
.nop_i:
    mov byte [rdi], 0x90      ; NOP (not INT3, safer)
    inc rdi
    dec eax
    jnz .nop_i
    jmp .merge_did_merge

.condemn_j:
    ; i is stronger — merge j into i, condemn j
    add [r14 + RHDR_HITS], ecx        ; i.hits += j.hits
    mov rdi, rsi
    or word [rdi + RHDR_FLAGS], RFLAG_CONDEMNED
    movzx eax, word [rdi + RHDR_CODE_LEN]
    lea rdi, [rdi + RHDR_SIZE]
    test eax, eax
    jz .merge_did_merge
.nop_j:
    mov byte [rdi], 0x90
    inc rdi
    dec eax
    jnz .nop_j

.merge_did_merge:
    inc dword [rsp + 16]      ; merged count (+8 rdx, +8 rcx on stack)
    ; Limit: max 64 merges per pass to avoid O(n^2) blowup
    cmp dword [rsp + 16], 64
    jge .merge_done_inner

.merge_skip_inner:
    pop rdx
    inc edx
    jmp .merge_inner

.merge_done_inner:
    pop rdx                   ; balance the push

.merge_next_outer:
    pop rcx
    inc ecx
    jmp .merge_outer

.merge_compact:
    ; Compact the table (remove condemned entries)
    mov rbx, SURFACE_BASE    ; restore rbx (clobbered above)
    cmp dword [rsp], 0
    je .merge_none
    call region_compact

.merge_none:
    mov eax, [rsp]            ; return merged count
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret
