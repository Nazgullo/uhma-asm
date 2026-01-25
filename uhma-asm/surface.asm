; surface.asm — Surface management: region allocation, headers, compaction
%include "syscalls.inc"
%include "constants.inc"

section .data
    ; Shared memory paths for Mycorrhiza (collective consciousness)
    shm_vsa_path:   db "/dev/shm/uhma_vsa", 0
    shm_holo_path:  db "/dev/shm/uhma_holo", 0
    shm_msg:        db "[MYCORRHIZA] ", 0
    shm_create:     db "Creating shared VSA field", 10, 0
    shm_attach:     db "Attaching to shared VSA field", 10, 0
    shm_solo:       db "Running in solo mode (no shared field)", 10, 0
    shm_instance:   db "Instance ID: 0x", 0

section .text

extern sys_mmap
extern sys_open
extern sys_close
extern sys_getrandom
extern sym_init
extern bp_init
extern gene_pool_init
extern presence_init
extern print_cstr
extern print_hex64
extern print_newline

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

    ; Zero confidence vector (topological metacognition)
    ; Starts neutral — no anxiety or confidence about any context type
    lea rdi, [rbx + CONFIDENCE_VEC_OFFSET]
    xor eax, eax
    mov ecx, CONFIDENCE_VEC_BYTES / 8  ; 1024 f64 elements
.zero_confidence:
    mov [rdi], rax
    add rdi, 8
    dec ecx
    jnz .zero_confidence

    ; Initialize breakpoint table (self-debugger)
    push rbx
    call bp_init
    pop rbx

    ; Zero holographic state fields
    mov dword [rbx + STATE_OFFSET + ST_VOCAB_COUNT], 0
    mov dword [rbx + STATE_OFFSET + ST_VOCAB_TOP_DIRTY], 0
    mov qword [rbx + STATE_OFFSET + ST_HOLO_PREDICT_SUM], 0  ; f64
    mov dword [rbx + STATE_OFFSET + ST_HOLO_PREDICT_N], 0

    ; Initialize symbolic observation system
    push rbx
    call sym_init
    pop rbx

    ; Initialize gene pool (composting system)
    push rbx
    call gene_pool_init
    pop rbx

    ; Initialize presence hyper-regions (hormonal modulators)
    push rbx
    call presence_init
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

;; ============================================================
;; MYCORRHIZA: Shared Consciousness Infrastructure
;; Enables multiple UHMA instances to share the VSA arena
;; and holographic traces, creating a "hive mind" where
;; experiences of one instance ripple through all others.
;; ============================================================

;; ============================================================
;; surface_init_shared(mode)
;; edi=mode (0=create new shared field, 1=attach to existing)
;; Creates or attaches to shared memory for VSA and holographic
;; traces. The dispatch region remains private.
;; Returns: 0 on success, -1 on failure (falls back to solo mode)
;; ============================================================
global surface_init_shared
surface_init_shared:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 24               ; locals

    mov r12d, edi             ; mode
    mov rbx, SURFACE_BASE

    ; Generate unique instance ID
    lea rdi, [rsp]
    mov rsi, 8
    xor edx, edx
    mov rax, SYS_GETRANDOM
    syscall
    mov r13, [rsp]            ; instance ID
    mov [rbx + STATE_OFFSET + ST_INSTANCE_ID], r13

    ; Print mode message
    lea rdi, [rel shm_msg]
    call print_cstr

    test r12d, r12d
    jnz .attach_mode

    ; --- CREATE MODE: Open/create shared VSA file ---
    lea rdi, [rel shm_create]
    call print_cstr

    ; Open/create shared VSA file
    lea rdi, [rel shm_vsa_path]
    mov esi, O_RDWR | O_CREAT ; read/write, create if not exists
    mov edx, 0644o            ; permissions
    mov rax, SYS_OPEN
    syscall
    test rax, rax
    js .shared_fail
    mov r14, rax              ; save fd

    ; Truncate to proper size
    mov rdi, r14              ; fd
    mov rsi, SHARED_VSA_SIZE  ; size
    mov rax, SYS_FTRUNCATE
    syscall
    test rax, rax
    js .close_and_fail

    ; Remap the VSA region as shared
    ; First unmap the private VSA region
    mov rdi, SURFACE_BASE + VSA_OFFSET
    mov rsi, SHARED_VSA_SIZE
    mov rax, SYS_MUNMAP
    syscall

    ; Mmap shared VSA
    mov rdi, SURFACE_BASE + VSA_OFFSET  ; fixed address
    mov rsi, SHARED_VSA_SIZE
    mov rdx, PROT_RWX
    mov rcx, MAP_SHARED | MAP_FIXED
    mov r8, r14               ; fd
    xor r9d, r9d             ; offset 0
    call sys_mmap
    cmp rax, -1
    je .close_and_fail

    ; Close fd (mapping persists)
    mov rdi, r14
    mov rax, SYS_CLOSE
    syscall

    ; Mark as shared mode
    mov dword [rbx + STATE_OFFSET + ST_SHARED_MODE], 1
    jmp .shared_success

.attach_mode:
    ; --- ATTACH MODE: Open existing shared VSA file ---
    lea rdi, [rel shm_attach]
    call print_cstr

    ; Open existing shared VSA file
    lea rdi, [rel shm_vsa_path]
    mov esi, O_RDWR           ; read/write only, must exist
    xor edx, edx
    mov rax, SYS_OPEN
    syscall
    test rax, rax
    js .shared_fail
    mov r14, rax              ; save fd

    ; Unmap private VSA region
    mov rdi, SURFACE_BASE + VSA_OFFSET
    mov rsi, SHARED_VSA_SIZE
    mov rax, SYS_MUNMAP
    syscall

    ; Mmap shared VSA
    mov rdi, SURFACE_BASE + VSA_OFFSET
    mov rsi, SHARED_VSA_SIZE
    mov rdx, PROT_RWX
    mov rcx, MAP_SHARED | MAP_FIXED
    mov r8, r14
    xor r9d, r9d
    call sys_mmap
    cmp rax, -1
    je .close_and_fail

    ; Close fd
    mov rdi, r14
    mov rax, SYS_CLOSE
    syscall

    ; Mark as shared mode
    mov dword [rbx + STATE_OFFSET + ST_SHARED_MODE], 1

.shared_success:
    ; Print instance ID
    lea rdi, [rel shm_instance]
    call print_cstr
    mov rdi, r13
    call print_hex64
    call print_newline

    ; Increment colony size in shared memory
    lock inc dword [rbx + STATE_OFFSET + ST_COLONY_SIZE]

    xor eax, eax              ; return success
    jmp .shared_done

.close_and_fail:
    mov rdi, r14
    mov rax, SYS_CLOSE
    syscall

.shared_fail:
    lea rdi, [rel shm_solo]
    call print_cstr

    ; Mark as solo mode
    mov dword [rbx + STATE_OFFSET + ST_SHARED_MODE], 0
    mov dword [rbx + STATE_OFFSET + ST_COLONY_SIZE], 1

    mov eax, -1               ; return failure

.shared_done:
    add rsp, 24
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; broadcast_pain(intensity)
;; xmm0=pain intensity (f64)
;; Broadcasts a "pain signal" to the shared holographic field.
;; Other instances will detect this shift and react.
;; Pain is encoded as negative valence superposed across traces.
;; ============================================================
global broadcast_pain
broadcast_pain:
    push rbx
    sub rsp, 8

    mov rbx, SURFACE_BASE

    ; Only broadcast if in shared mode
    cmp dword [rbx + STATE_OFFSET + ST_SHARED_MODE], 0
    je .pain_done

    ; Store pain intensity
    movsd [rsp], xmm0

    ; Generate pain vector (negative valence across all traces)
    ; This creates a "disturbance in the field" that others detect
    mov rdi, SURFACE_BASE + HOLO_OFFSET
    mov ecx, HOLO_TRACES

.pain_broadcast_loop:
    test ecx, ecx
    jz .pain_done

    ; Superpose negative valence onto trace
    movsd xmm0, [rsp]
    movsd xmm1, [rdi + VSA_VALENCE_OFFSET]
    subsd xmm1, xmm0          ; reduce valence (pain)
    ; Clamp to [-1, 1]
    mov rax, 0xBFF0000000000000  ; -1.0
    movq xmm2, rax
    maxsd xmm1, xmm2
    movsd [rdi + VSA_VALENCE_OFFSET], xmm1

    add rdi, HOLO_VEC_BYTES
    dec ecx
    jmp .pain_broadcast_loop

.pain_done:
    add rsp, 8
    pop rbx
    ret

;; ============================================================
;; sense_collective_valence() -> xmm0 (average valence f64)
;; Samples the shared holographic field to sense the collective
;; emotional state. Returns average valence across all traces.
;; Negative = collective pain, Positive = collective well-being.
;; ============================================================
global sense_collective_valence
sense_collective_valence:
    push rbx

    mov rbx, SURFACE_BASE
    xorpd xmm0, xmm0          ; accumulator
    mov rdi, SURFACE_BASE + HOLO_OFFSET
    mov ecx, HOLO_TRACES

.sense_loop:
    test ecx, ecx
    jz .sense_done

    addsd xmm0, [rdi + VSA_VALENCE_OFFSET]
    add rdi, HOLO_VEC_BYTES
    dec ecx
    jmp .sense_loop

.sense_done:
    ; Divide by trace count for average
    mov eax, HOLO_TRACES
    cvtsi2sd xmm1, eax
    divsd xmm0, xmm1

    pop rbx
    ret

;; ============================================================
;; get_colony_size() -> eax (number of instances)
;; Returns the number of instances sharing the VSA field.
;; ============================================================
global get_colony_size
get_colony_size:
    mov rax, SURFACE_BASE
    mov eax, [rax + STATE_OFFSET + ST_COLONY_SIZE]
    ret

;; ============================================================
;; is_shared_mode() -> eax (0=solo, 1=shared)
;; Returns whether this instance is connected to the hive.
;; ============================================================
global is_shared_mode
is_shared_mode:
    mov rax, SURFACE_BASE
    mov eax, [rax + STATE_OFFSET + ST_SHARED_MODE]
    ret
