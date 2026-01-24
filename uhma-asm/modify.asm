; modify.asm — Self-modification: prune, reorder, specialize, generalize
%include "syscalls.inc"
%include "constants.inc"

section .data
    prune_msg:      db "[PRUNE] Condemned region at index ", 0
    promote_msg:    db "[PROMOTE] Region ", 0
    promote_to:     db " activation boost", 0
    specialize_msg: db "[SPECIALIZE] Region duplicated", 10, 0
    generalize_msg: db "[GENERALIZE] Context relaxed", 10, 0
    modify_nl:      db 10, 0

    ; f64 constants for promote boost
    align 8
    mod_boost_val:  dq 0.5
    mod_f64_one:    dq 1.0

section .text

extern print_cstr
extern print_u64
extern print_newline
extern region_condemn
extern fire_hook
extern emit_dispatch_pattern

;; ============================================================
;; modify_prune(region_index)
;; edi=index in region table
;; Marks the region as CONDEMNED
;; ============================================================
global modify_prune
modify_prune:
    push rbx
    push r12

    mov r12d, edi
    mov rbx, SURFACE_BASE

    ; Print
    push r12
    lea rdi, [rel prune_msg]
    call print_cstr
    movzx rdi, r12w
    call print_u64
    call print_newline
    pop r12

    ; Get region entry
    lea rax, [rbx + REGION_TABLE_OFFSET]
    imul rcx, r12, RTE_SIZE
    add rax, rcx

    ; Get header addr
    mov rdi, [rax + RTE_ADDR]

    ; Record causal link before modification
    push rdi
    call log_causal
    pop rdi

    ; Condemn
    call region_condemn

    ; Set condemned flag in table too
    lea rax, [rbx + REGION_TABLE_OFFSET]
    imul rcx, r12, RTE_SIZE
    add rax, rcx
    or word [rax + RTE_FLAGS], RFLAG_CONDEMNED

    ; Clear all connections POINTING TO this condemned region
    mov rdi, [rax + RTE_ADDR]  ; condemned region header ptr
    call clear_connections_to

    ; Fire prune hook
    mov edi, HOOK_ON_PRUNE
    mov esi, r12d
    call fire_hook

    ; Log modification
    call log_modification

    pop r12
    pop rbx
    ret

;; ============================================================
;; modify_promote(region_index, target_position)
;; edi=current index, esi=target position (unused, kept for API compat)
;; Graph routing handles priority. Instead of swapping table entries,
;; boost the promoted region's activation and its excite targets.
;; ============================================================
global modify_promote
modify_promote:
    push rbx
    push r12

    mov r12d, edi             ; region index
    mov rbx, SURFACE_BASE

    ; Print
    push r12
    lea rdi, [rel promote_msg]
    call print_cstr
    movzx rdi, r12w
    call print_u64
    lea rdi, [rel promote_to]
    call print_cstr
    call print_newline
    pop r12

    ; Get region header
    lea rax, [rbx + REGION_TABLE_OFFSET]
    imul rcx, r12, RTE_SIZE
    add rax, rcx
    mov rsi, [rax + RTE_ADDR]
    test rsi, rsi
    jz .promote_done

    ; Boost this region's activation
    movsd xmm0, [rsi + RHDR_ACTIVATION]
    addsd xmm0, [rel mod_boost_val]   ; +0.5
    minsd xmm0, [rel mod_f64_one]     ; clamp to 1.0
    movsd [rsi + RHDR_ACTIVATION], xmm0

    ; Boost excite_a target's prime
    mov rax, [rsi + RHDR_EXCITE_A]
    test rax, rax
    jz .promote_try_b
    movsd xmm0, [rax + RHDR_PRIME]
    addsd xmm0, [rel mod_boost_val]
    minsd xmm0, [rel mod_f64_one]
    movsd [rax + RHDR_PRIME], xmm0

.promote_try_b:
    ; Boost excite_b target's prime
    mov rax, [rsi + RHDR_EXCITE_B]
    test rax, rax
    jz .promote_fire_hook
    movsd xmm0, [rax + RHDR_PRIME]
    addsd xmm0, [rel mod_boost_val]
    minsd xmm0, [rel mod_f64_one]
    movsd [rax + RHDR_PRIME], xmm0

.promote_fire_hook:
    ; Fire promote hook
    mov edi, HOOK_ON_PROMOTE
    mov esi, r12d
    call fire_hook

.promote_done:
    pop r12
    pop rbx
    ret

;; ============================================================
;; modify_specialize(region_index)
;; edi=index
;; Duplicates a region with a more specific context match
;; (adds bits to the context hash comparison)
;; ============================================================
global modify_specialize
modify_specialize:
    push rbx
    push r12

    mov r12d, edi
    mov rbx, SURFACE_BASE

    ; Get the region's stored context and token
    lea rax, [rbx + REGION_TABLE_OFFSET]
    imul rcx, r12, RTE_SIZE
    add rax, rcx
    mov rsi, [rax + RTE_ADDR]  ; header ptr

    ; Read context hash from cmp instruction
    cmp byte [rsi + RHDR_SIZE], 0x3D
    jne .fail
    mov edi, [rsi + RHDR_SIZE + 1]   ; ctx_hash

    ; Read token from mov instruction
    mov esi, [rsi + RHDR_SIZE + 8]   ; token_id

    ; Modify context: XOR with current global step to create variant
    lea rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov eax, [rax]
    xor edi, eax              ; specialized context

    ; Get birth step
    lea rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov edx, [rax]

    ; Emit as new pattern
    call emit_dispatch_pattern

    lea rdi, [rel specialize_msg]
    call print_cstr

.fail:
    pop r12
    pop rbx
    ret

;; ============================================================
;; modify_generalize(region_index)
;; edi=index
;; Relaxes a context match by masking out lower bits
;; ============================================================
global modify_generalize
modify_generalize:
    push rbx
    push r12

    mov r12d, edi
    mov rbx, SURFACE_BASE

    ; Get region header
    lea rax, [rbx + REGION_TABLE_OFFSET]
    imul rcx, r12, RTE_SIZE
    add rax, rcx
    mov rsi, [rax + RTE_ADDR]

    ; Check for valid cmp instruction
    cmp byte [rsi + RHDR_SIZE], 0x3D
    jne .fail

    ; Record causal link before modification
    push rsi
    mov rdi, rsi
    call log_causal
    pop rsi

    ; Modify the cmp immediate in-place: mask out low 4 bits
    and dword [rsi + RHDR_SIZE + 1], 0xFFFFFFF0

    lea rdi, [rel generalize_msg]
    call print_cstr

    ; Reset counters (fresh start after generalization)
    mov dword [rsi + RHDR_HITS], 0
    mov dword [rsi + RHDR_MISSES], 0

.fail:
    pop r12
    pop rbx
    ret

;; ============================================================
;; modify_restructure
;; Reorder all dispatch regions by hit rate (descending)
;; Simple bubble sort on the region table
;; ============================================================
global modify_restructure
modify_restructure:
    push rbx
    push r12
    push r13

    mov rbx, SURFACE_BASE
    lea r12, [rbx + REGION_TABLE_OFFSET]
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov r13d, [rax]           ; count

    ; Bubble sort by hits (descending)
    ; Skip index 0 (bootstrap dispatch)
    mov ecx, 1                ; outer loop start
.outer:
    cmp ecx, r13d
    jge .sort_done

    mov edx, ecx              ; inner = outer
.inner:
    cmp edx, r13d
    jge .next_outer

    ; Compare [edx-1] hits vs [edx] hits (for DISPATCH types only)
    lea rdi, [r12]
    imul rax, rdx, RTE_SIZE
    lea rsi, [rdi + rax]                 ; entry[edx]

    ; Only sort DISPATCH regions
    movzx eax, word [rsi + RTE_TYPE]
    cmp eax, RTYPE_DISPATCH
    jne .no_swap

    dec rdx
    imul rax, rdx, RTE_SIZE
    lea rdi, [r12 + rax]               ; entry[edx-1]
    inc rdx

    movzx eax, word [rdi + RTE_TYPE]
    cmp eax, RTYPE_DISPATCH
    jne .no_swap

    ; Compare hits: if entry[edx].hits > entry[edx-1].hits → swap
    mov eax, [rsi + RTE_HITS]
    cmp eax, [rdi + RTE_HITS]
    jle .no_swap

    ; Swap entries (32 bytes)
    push rcx
    push rdx
    mov ecx, 4               ; 4 x 8 bytes
    xor r8d, r8d
.swap_loop:
    mov rax, [rsi + r8]
    mov rdx, [rdi + r8]
    mov [rsi + r8], rdx
    mov [rdi + r8], rax
    add r8, 8
    dec ecx
    jnz .swap_loop
    pop rdx
    pop rcx

.no_swap:
    inc edx
    jmp .inner

.next_outer:
    inc ecx
    jmp .outer

.sort_done:
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; clear_connections_to(condemned_ptr)
;; rdi = region header ptr being condemned
;; Walk all regions, clear any connection pointers that reference
;; this region (excite_a/b, inhibit_a/b, next_a/b).
;; ============================================================
global clear_connections_to
clear_connections_to:
    push rbx
    push r12
    push r13
    push r14

    mov r12, rdi              ; condemned region ptr
    mov rbx, SURFACE_BASE
    lea r13, [rbx + REGION_TABLE_OFFSET]
    mov r14d, [rbx + STATE_OFFSET + ST_REGION_COUNT]

    xor ecx, ecx
.clr_loop:
    cmp ecx, r14d
    jge .clr_done
    push rcx

    imul rdi, rcx, RTE_SIZE
    add rdi, r13
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_CONDEMNED
    jnz .clr_next

    mov rsi, [rdi + RTE_ADDR]

    ; Check and clear each connection pointer
    cmp [rsi + RHDR_NEXT_A], r12
    jne .clr_n1
    mov qword [rsi + RHDR_NEXT_A], 0
.clr_n1:
    cmp [rsi + RHDR_NEXT_B], r12
    jne .clr_n2
    mov qword [rsi + RHDR_NEXT_B], 0
.clr_n2:
    cmp [rsi + RHDR_EXCITE_A], r12
    jne .clr_n3
    mov qword [rsi + RHDR_EXCITE_A], 0
    mov qword [rsi + RHDR_W_EXCITE_A], 0   ; clear weight too (0 bits = 0.0 f64)
.clr_n3:
    cmp [rsi + RHDR_EXCITE_B], r12
    jne .clr_n4
    mov qword [rsi + RHDR_EXCITE_B], 0
    mov qword [rsi + RHDR_W_EXCITE_B], 0
.clr_n4:
    cmp [rsi + RHDR_INHIBIT_A], r12
    jne .clr_n5
    mov qword [rsi + RHDR_INHIBIT_A], 0
    mov qword [rsi + RHDR_W_INHIBIT_A], 0
.clr_n5:
    cmp [rsi + RHDR_INHIBIT_B], r12
    jne .clr_next
    mov qword [rsi + RHDR_INHIBIT_B], 0
    mov qword [rsi + RHDR_W_INHIBIT_B], 0

.clr_next:
    pop rcx
    inc ecx
    jmp .clr_loop

.clr_done:
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; log_modification
;; Records a modification in the mod log ring buffer
;; ============================================================
log_modification:
    push rbx
    mov rbx, SURFACE_BASE

    ; Get current position
    lea rax, [rbx + STATE_OFFSET + ST_MOD_LOG_POS]
    mov ecx, [rax]

    ; Calculate entry address
    lea rdx, [rbx + STATE_OFFSET + ST_MOD_LOG]
    imul rdi, rcx, ST_MOD_ENTRY_SIZE
    add rdi, rdx

    ; Write step
    lea rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov rax, [rax]
    mov [rdi], rax

    ; Advance position
    lea rax, [rbx + STATE_OFFSET + ST_MOD_LOG_POS]
    inc ecx
    cmp ecx, ST_MOD_LOG_CAP
    jl .no_wrap
    xor ecx, ecx
.no_wrap:
    mov [rax], ecx

    pop rbx
    ret

;; ============================================================
;; log_causal(region_header_ptr)
;; rdi = region header being modified
;; Records the causal link: address + pre-modification accuracy
;; Post-accuracy gets computed on next observation cycle
;; ============================================================
global log_causal
log_causal:
    push rbx
    mov rbx, SURFACE_BASE

    ; Record which address is being modified
    lea rax, [rbx + STATE_OFFSET + ST_CAUSAL_MOD_ADDR]
    mov [rax], rdi

    ; Compute pre-modification accuracy for this region
    mov eax, [rdi + RHDR_HITS]
    mov edx, [rdi + RHDR_MISSES]
    add edx, eax
    test edx, edx
    jz .causal_zero
    cvtsi2ss xmm0, eax
    cvtsi2ss xmm1, edx
    divss xmm0, xmm1
    jmp .causal_store
.causal_zero:
    xorps xmm0, xmm0
.causal_store:
    lea rax, [rbx + STATE_OFFSET + ST_CAUSAL_PRE_ACC]
    movss [rax], xmm0

    ; Clear post-accuracy (will be filled by next observe)
    lea rax, [rbx + STATE_OFFSET + ST_CAUSAL_POST_ACC]
    xorps xmm0, xmm0
    movss [rax], xmm0

    ; Increment causal record count
    lea rax, [rbx + STATE_OFFSET + ST_CAUSAL_COUNT]
    inc dword [rax]

    pop rbx
    ret
