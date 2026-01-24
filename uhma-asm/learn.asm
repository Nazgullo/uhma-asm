; learn.asm — Learning: emit new patterns, strengthen/weaken existing
%include "syscalls.inc"
%include "constants.inc"

section .data
    learn_msg:      db "[LEARN] ctx=0x", 0
    learn_tok_msg:  db " → tok=0x", 0
    learn_dup_msg:  db " (exists, skip)", 10, 0
    learn_new_msg:  db " (new pattern)", 10, 0
    strengthen_msg: db "[STRENGTHEN] region at 0x", 0
    weaken_msg:     db "[WEAKEN] region at 0x", 0

    ; Holographic learning rate (f64)
    align 8
    holo_lr:        dq 0.1

    ; f64 constants for STDP learning
    align 8
    learn_rate:     dq 0.05
    tau:            dq 4.0
    tau_window:     dq 8.0
    initial_w:      dq 0.1
    resonance_old:  dq 0.9
    resonance_new:  dq 0.1
    f64_one:        dq 1.0
    f64_zero:       dq 0.0
    f64_half:       dq 0.5
    f64_sixth:      dq 0.16666666666666666

section .text

extern print_cstr
extern print_hex32
extern print_newline
extern emit_dispatch_pattern
extern fire_hook
extern holo_store
extern vocab_register
extern region_merge_pass

;; ============================================================
;; learn_pattern(ctx_hash, token_id)
;; rdi=context_hash (u64, lower 32 used), esi=token_id
;; Learns a new ctx→token association by emitting code
;; First checks if the pattern already exists (avoid duplicates)
;; ============================================================
global learn_pattern
learn_pattern:
    push rbx
    push r12
    push r13

    mov r12d, edi             ; ctx_hash (lower 32)
    mov r13d, esi             ; token_id

    mov rbx, SURFACE_BASE

    ; ALWAYS store holographically (never fails, interference just gets denser)
    mov edi, r12d             ; ctx_hash
    mov esi, r13d             ; token_id
    movsd xmm0, [rel holo_lr]  ; f64 strength
    call holo_store

    ; Register token in vocabulary
    mov edi, r13d
    call vocab_register

    ; Check if this exact pattern already exists
    mov edi, r12d
    mov esi, r13d
    call find_existing_pattern
    test rax, rax
    jnz .already_exists

    ; Check region count — trigger merge if approaching saturation
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov ecx, [rax]
    cmp ecx, REGION_TABLE_MAX - 4    ; leave room
    jl .has_room
    ; Try to merge before giving up
    call region_merge_pass
    ; Re-check count after merge
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov ecx, [rax]
    cmp ecx, REGION_TABLE_MAX - 4
    jge .table_full
.has_room:

    ; Print learn info (only for genuinely NEW patterns)
    push r12
    push r13
    lea rdi, [rel learn_msg]
    call print_cstr
    mov edi, r12d
    call print_hex32
    lea rdi, [rel learn_tok_msg]
    call print_cstr
    mov edi, r13d
    call print_hex32
    pop r13
    pop r12
    lea rdi, [rel learn_new_msg]
    call print_cstr

    ; --- Metabolic cost: emitting a new pattern costs energy ---
    ; If energy is below starvation level, refuse to emit (conserve)
    movsd xmm0, [rbx + STATE_OFFSET + ST_ENERGY]
    mov rax, ENERGY_STARVATION
    movq xmm1, rax
    ucomisd xmm0, xmm1
    jbe .done                  ; starving — refuse to spend energy on emission
    ; Deduct emission cost
    mov rax, ENERGY_EMIT_COST
    movq xmm1, rax
    subsd xmm0, xmm1
    xorpd xmm2, xmm2
    maxsd xmm0, xmm2
    movsd [rbx + STATE_OFFSET + ST_ENERGY], xmm0
    addsd xmm1, [rbx + STATE_OFFSET + ST_ENERGY_SPENT]
    movsd [rbx + STATE_OFFSET + ST_ENERGY_SPENT], xmm1

    mov edi, r12d             ; ctx_hash
    mov esi, r13d             ; token_id
    ; Get current step for birth
    lea rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov edx, [rax]            ; birth_step (lower 32 of global step)
    call emit_dispatch_pattern

    ; Track recent emission count (for introspective state)
    lea rax, [rbx + STATE_OFFSET + ST_RECENT_EMITS]
    inc dword [rax]

    ; Check for auto-generalization opportunity
    mov edi, r12d             ; ctx_hash
    mov esi, r13d             ; token_id
    call check_auto_generalize

    ; Fire learn hook
    mov edi, HOOK_ON_LEARN
    mov esi, r13d
    call fire_hook

    jmp .done

.already_exists:
    ; Pattern exists — strengthen it (boost hit counter)
    push rax                  ; save region header ptr across print call
    lea rdi, [rel learn_dup_msg]
    call print_cstr
    pop rax

    ; Strengthen: increment hits on the existing region
    mov rdi, rax
    call strengthen_region

.table_full:
.done:
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; find_existing_pattern(ctx_hash_32, token_id) → rax
;; edi=ctx_hash, esi=token_id
;; Searches dispatch regions for matching ctx→token
;; Returns: header ptr if found, 0 if not
;; ============================================================
global find_existing_pattern
find_existing_pattern:
    push rbx
    push r12
    push r13

    mov r12d, edi             ; ctx_hash
    mov r13d, esi             ; token_id

    mov rbx, SURFACE_BASE
    lea rax, [rbx + REGION_TABLE_OFFSET]
    push rax                  ; save table base
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov ecx, [rax]
    pop rsi                   ; table base

    xor edx, edx             ; index
.search:
    cmp edx, ecx
    jge .not_found
    push rcx
    push rdx

    imul rdi, rdx, RTE_SIZE
    add rdi, rsi

    ; Check type
    movzx eax, word [rdi + RTE_TYPE]
    cmp eax, RTYPE_DISPATCH
    jne .skip

    ; Check not condemned
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_CONDEMNED
    jnz .skip

    ; Get header address
    mov rdi, [rdi + RTE_ADDR]

    ; Check the CMP immediate (ctx_hash)
    cmp byte [rdi + RHDR_SIZE], 0x3D
    jne .skip
    cmp [rdi + RHDR_SIZE + 1], r12d
    jne .skip

    ; Check the MOV immediate (token_id)
    ; At offset RHDR_SIZE + 7: mov eax, imm32 (B8 xx xx xx xx)
    cmp byte [rdi + RHDR_SIZE + 7], 0xB8
    jne .skip
    cmp [rdi + RHDR_SIZE + 8], r13d
    jne .skip

    ; Found match
    mov rax, rdi
    pop rdx
    pop rcx
    jmp .found

.skip:
    pop rdx
    pop rcx
    inc edx
    jmp .search

.not_found:
    xor eax, eax
.found:
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; strengthen_region(header_ptr)
;; rdi=region header ptr
;; Adds bonus hits to boost the region's fitness
;; ============================================================
global strengthen_region
strengthen_region:
    ; Add 2 bonus hits (reinforcement)
    add dword [rdi + RHDR_HITS], 2
    ret

;; ============================================================
;; weaken_region(header_ptr)
;; rdi=region header ptr
;; Adds penalty misses
;; ============================================================
global weaken_region
weaken_region:
    ; Add 1 penalty miss
    inc dword [rdi + RHDR_MISSES]
    ret

;; ============================================================
;; check_auto_generalize(ctx_hash, token_id)
;; edi=ctx_hash (just emitted), esi=token_id
;; Scans existing dispatch patterns for the same token with
;; a similar context. If found, emits a generalized pattern
;; (masked lower bits) — this is how schemas emerge from
;; repeated observation of the same token in similar contexts.
;; ============================================================
check_auto_generalize:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8                ; [rsp] = existing_ctx local var

    mov r12d, edi             ; new ctx_hash
    mov r13d, esi             ; token_id

    mov rbx, SURFACE_BASE
    lea r14, [rbx + REGION_TABLE_OFFSET]
    mov r15d, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    xor ecx, ecx
.ag_loop:
    cmp ecx, r15d
    jge .ag_done
    push rcx

    imul rdi, rcx, RTE_SIZE
    add rdi, r14
    movzx eax, word [rdi + RTE_TYPE]
    cmp eax, RTYPE_DISPATCH
    jne .ag_skip
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_CONDEMNED
    jnz .ag_skip

    mov rsi, [rdi + RTE_ADDR]
    cmp byte [rsi + RHDR_SIZE], 0x3D
    jne .ag_skip

    ; Check same token (mov eax, imm32 at offset +7)
    cmp byte [rsi + RHDR_SIZE + 7], 0xB8
    jne .ag_skip
    cmp [rsi + RHDR_SIZE + 8], r13d
    jne .ag_skip

    ; Same token! Check different context
    mov eax, [rsi + RHDR_SIZE + 1]
    cmp eax, r12d
    je .ag_skip               ; same context = same pattern

    ; Found overlap — save existing context to local var
    mov [rsp + 8], eax        ; +8 because rcx is pushed

    ; --- Emit schema for NEW context ---
    mov edi, r12d
    and edi, 0xFFFFFFF0
    mov esi, r13d
    call find_existing_pattern
    test rax, rax
    jnz .ag_new_exists
    cmp dword [rbx + STATE_OFFSET + ST_REGION_COUNT], REGION_TABLE_MAX - 4
    jge .ag_done_pop
    mov edi, r12d
    and edi, 0xFFFFFFF0
    mov esi, r13d
    mov edx, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    call emit_dispatch_pattern
    inc dword [rbx + STATE_OFFSET + ST_RECENT_EMITS]
.ag_new_exists:

    ; --- Emit schema for EXISTING context ---
    mov eax, [rsp + 8]       ; reload existing_ctx
    mov edi, eax
    and edi, 0xFFFFFFF0
    mov esi, r13d
    call find_existing_pattern
    test rax, rax
    jnz .ag_done_pop          ; already exists
    cmp dword [rbx + STATE_OFFSET + ST_REGION_COUNT], REGION_TABLE_MAX - 4
    jge .ag_done_pop
    mov eax, [rsp + 8]       ; reload existing_ctx
    mov edi, eax
    and edi, 0xFFFFFFF0
    mov esi, r13d
    mov edx, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    call emit_dispatch_pattern
    inc dword [rbx + STATE_OFFSET + ST_RECENT_EMITS]

    ; One generalization per learn call — done
    pop rcx
    jmp .ag_done

.ag_done_pop:
    pop rcx
    jmp .ag_done
.ag_skip:
    pop rcx
    inc ecx
    jmp .ag_loop
.ag_done:
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; wire_new_region(region_ptr, token_id)
;; rdi=new region header ptr, esi=token_id
;; Bootstraps connectivity: finds existing regions with same token,
;; creates next_a/next_b links and initial excite weights.
;; ============================================================
global wire_new_region
wire_new_region:
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov r12, rdi              ; new region ptr
    mov r13d, esi             ; token_id
    mov rbx, SURFACE_BASE
    xor r15, r15              ; closest same-token region found (0=none)

    lea r14, [rbx + REGION_TABLE_OFFSET]
    mov ecx, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    xor edx, edx

.wire_scan:
    cmp edx, ecx
    jge .wire_done_scan
    push rcx
    push rdx

    imul rdi, rdx, RTE_SIZE
    add rdi, r14
    movzx eax, word [rdi + RTE_TYPE]
    cmp eax, RTYPE_DISPATCH
    jne .wire_skip
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_CONDEMNED
    jnz .wire_skip

    mov rsi, [rdi + RTE_ADDR]
    ; Skip self
    cmp rsi, r12
    je .wire_skip

    ; Check if same token (B8 opcode at +7 of code, token at +8)
    cmp byte [rsi + RHDR_SIZE + 7], 0xB8
    jne .wire_skip
    cmp [rsi + RHDR_SIZE + 8], r13d
    jne .wire_skip

    ; Found a same-token region — use the first one found
    test r15, r15
    jnz .wire_skip             ; already found one
    mov r15, rsi               ; save it

.wire_skip:
    pop rdx
    pop rcx
    inc edx
    jmp .wire_scan

.wire_done_scan:
    ; If we found a same-token region, wire them
    test r15, r15
    jz .wire_no_neighbor

    ; new_region.next_a = existing_region (routing bootstrap)
    mov [r12 + RHDR_NEXT_A], r15

    ; existing_region.next_b = new_region (bidirectional, if slot available)
    cmp qword [r15 + RHDR_NEXT_B], 0
    jne .wire_excite
    mov [r15 + RHDR_NEXT_B], r12

.wire_excite:
    ; Set initial excite weights (weak but nonzero)
    ; existing → new: excite
    cmp qword [r15 + RHDR_EXCITE_A], 0
    jne .wire_try_b
    mov [r15 + RHDR_EXCITE_A], r12
    movsd xmm0, [rel initial_w]
    movsd [r15 + RHDR_W_EXCITE_A], xmm0
    jmp .wire_reverse
.wire_try_b:
    cmp qword [r15 + RHDR_EXCITE_B], 0
    jne .wire_reverse
    mov [r15 + RHDR_EXCITE_B], r12
    movsd xmm0, [rel initial_w]
    movsd [r15 + RHDR_W_EXCITE_B], xmm0

.wire_reverse:
    ; new → existing: excite
    mov [r12 + RHDR_EXCITE_A], r15
    movsd xmm0, [rel initial_w]
    movsd [r12 + RHDR_W_EXCITE_A], xmm0

.wire_no_neighbor:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; learn_connections(region_ptr)
;; rdi=firing region header ptr
;; STDP: scan fire_ring, strengthen connections from recently-fired
;; regions to this one. Implements spike-timing-dependent plasticity.
;; ============================================================
global learn_connections
learn_connections:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 16               ; [rsp+0] = current_time f64, [rsp+8] = scratch

    mov r12, rdi              ; firing region ptr (post-synaptic)
    mov rbx, SURFACE_BASE

    ; Get current timestamp as f64
    mov rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    cvtsi2sd xmm0, rax
    movsd [rsp + 0], xmm0    ; current_time

    ; Scan fire ring
    lea r13, [rbx + STATE_OFFSET + ST_FIRE_RING]
    xor r14d, r14d            ; ring index

.stdp_scan:
    cmp r14d, ST_FIRE_RING_CAP
    jge .stdp_done

    ; Load entry: (region_ptr:u64, timestamp:f64)
    imul eax, r14d, ST_FIRE_RING_ENTRY
    lea rdi, [r13 + rax]
    mov r15, [rdi]            ; prev_ptr
    test r15, r15
    jz .stdp_next             ; empty slot

    ; Skip self
    cmp r15, r12
    je .stdp_next

    ; Compute Δt = current_time - prev_time
    movsd xmm0, [rsp + 0]    ; current_time
    subsd xmm0, [rdi + 8]    ; prev_time
    ; xmm0 = Δt

    ; Skip if Δt <= 0
    xorpd xmm1, xmm1
    ucomisd xmm0, xmm1
    jbe .stdp_next

    ; Skip if Δt > TAU_WINDOW (8.0)
    ucomisd xmm0, [rel tau_window]
    ja .stdp_next

    ; --- Compute Δw = LEARNING_RATE * exp(-Δt / TAU) ---
    ; First: x = Δt / TAU
    movsd xmm1, xmm0         ; xmm1 = Δt
    divsd xmm1, [rel tau]    ; xmm1 = Δt/TAU = x

    ; exp(-x) ≈ 1 - x + x²/2 - x³/6 (Taylor for small x)
    ; xmm2 = x² = x*x
    movsd xmm2, xmm1
    mulsd xmm2, xmm1         ; x²
    ; xmm3 = x³ = x²*x
    movsd xmm3, xmm2
    mulsd xmm3, xmm1         ; x³

    ; result = 1.0 - x + x²/2 - x³/6
    movsd xmm4, [rel f64_one] ; 1.0
    subsd xmm4, xmm1          ; 1 - x
    movsd xmm5, xmm2
    mulsd xmm5, [rel f64_half] ; x²/2
    addsd xmm4, xmm5          ; 1 - x + x²/2
    movsd xmm5, xmm3
    mulsd xmm5, [rel f64_sixth] ; x³/6
    subsd xmm4, xmm5          ; 1 - x + x²/2 - x³/6
    ; xmm4 = exp(-x) approximation

    ; Clamp to [0, 1]
    xorpd xmm5, xmm5
    maxsd xmm4, xmm5
    minsd xmm4, [rel f64_one]

    ; Δw = LEARNING_RATE * exp(-x)
    mulsd xmm4, [rel learn_rate]
    ; xmm4 = Δw

    ; --- Strengthen: prev should excite current ---
    ; Check prev.excite_a
    mov rax, [r15 + RHDR_EXCITE_A]
    test rax, rax
    jz .stdp_set_a             ; empty slot
    cmp rax, r12
    je .stdp_strengthen_a      ; already points to us

    ; Check prev.excite_b
    mov rax, [r15 + RHDR_EXCITE_B]
    test rax, rax
    jz .stdp_set_b
    cmp rax, r12
    je .stdp_strengthen_b

    ; Both slots full — evict weakest if Δw > min(w_a, w_b)
    movsd xmm0, [r15 + RHDR_W_EXCITE_A]
    movsd xmm1, [r15 + RHDR_W_EXCITE_B]
    ucomisd xmm0, xmm1
    jbe .stdp_evict_a          ; w_a <= w_b, evict a
    ; Evict b (weaker)
    ucomisd xmm4, xmm1        ; Δw > w_b?
    jbe .stdp_update_resonance ; no, skip
    mov [r15 + RHDR_EXCITE_B], r12
    movsd [r15 + RHDR_W_EXCITE_B], xmm4
    jmp .stdp_update_resonance

.stdp_evict_a:
    ucomisd xmm4, xmm0        ; Δw > w_a?
    jbe .stdp_update_resonance
    mov [r15 + RHDR_EXCITE_A], r12
    movsd [r15 + RHDR_W_EXCITE_A], xmm4
    jmp .stdp_update_resonance

.stdp_set_a:
    ; Empty slot A — wire new connection
    mov [r15 + RHDR_EXCITE_A], r12
    movsd [r15 + RHDR_W_EXCITE_A], xmm4
    jmp .stdp_update_resonance

.stdp_strengthen_a:
    ; Already connected via A — strengthen
    movsd xmm0, [r15 + RHDR_W_EXCITE_A]
    addsd xmm0, xmm4
    minsd xmm0, [rel f64_one] ; clamp to 1.0
    movsd [r15 + RHDR_W_EXCITE_A], xmm0
    jmp .stdp_update_resonance

.stdp_set_b:
    mov [r15 + RHDR_EXCITE_B], r12
    movsd [r15 + RHDR_W_EXCITE_B], xmm4
    jmp .stdp_update_resonance

.stdp_strengthen_b:
    movsd xmm0, [r15 + RHDR_W_EXCITE_B]
    addsd xmm0, xmm4
    minsd xmm0, [rel f64_one]
    movsd [r15 + RHDR_W_EXCITE_B], xmm0

.stdp_update_resonance:
    ; Update current.resonance = resonance * 0.9 + 0.1 * (1/Δt)
    ; Reload Δt (from current_time - entry timestamp)
    imul eax, r14d, ST_FIRE_RING_ENTRY
    lea rdi, [r13 + rax]
    movsd xmm0, [rsp + 0]
    subsd xmm0, [rdi + 8]     ; Δt
    ; Avoid division by zero
    xorpd xmm1, xmm1
    ucomisd xmm0, xmm1
    jbe .stdp_next
    ; 1/Δt
    movsd xmm1, [rel f64_one]
    divsd xmm1, xmm0          ; 1/Δt
    ; Clamp 1/Δt to max 1.0
    minsd xmm1, [rel f64_one]
    ; resonance = old * 0.9 + new * 0.1
    movsd xmm2, [r12 + RHDR_RESONANCE]
    mulsd xmm2, [rel resonance_old]  ; old * 0.9
    movsd xmm3, xmm1
    mulsd xmm3, [rel resonance_new]  ; (1/Δt) * 0.1
    addsd xmm2, xmm3
    movsd [r12 + RHDR_RESONANCE], xmm2

.stdp_next:
    inc r14d
    jmp .stdp_scan

.stdp_done:
    add rsp, 16
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret
