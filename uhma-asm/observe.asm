; observe.asm — Self-observation: scan regions, compute metrics, update presence
%include "syscalls.inc"
%include "constants.inc"

section .data
    obs_msg:        db "[OBSERVE] step=", 0
    obs_regions:    db " regions=", 0
    obs_accuracy:   db " accuracy=", 0
    obs_condemned:  db " condemned=", 0
    obs_nl:         db 10, 0

    ; f64 constants for connection weight decay
    align 8
    obs_weight_decay:   dq 0.995
    obs_weight_floor:   dq 0.01
    obs_f64_zero:       dq 0.0

section .text

extern print_cstr
extern print_u64
extern print_f32
extern print_newline
extern fire_hook
extern region_condemn
extern modify_promote
extern modify_prune
extern drives_check
extern log_causal
extern dream_cycle
extern introspect_scan_regions

;; ============================================================
;; observe_cycle
;; Main observation pass:
;; 1. Walk all regions, compute accuracy for each
;; 2. Mark low-accuracy old regions as CONDEMNED
;; 3. Identify high-accuracy regions for promotion
;; 4. Update presence fields from aggregate stats
;; 5. Update drive levels
;; 6. Fire observation hook
;; ============================================================
global observe_cycle
observe_cycle:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 64               ; local vars

    mov rbx, SURFACE_BASE

    ; Get global step
    lea rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov r12, [rax]            ; current step

    ; Print observation header
    lea rdi, [rel obs_msg]
    call print_cstr
    mov rdi, r12
    call print_u64

    ; Get region count
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov r13d, [rax]           ; region count

    lea rdi, [rel obs_regions]
    call print_cstr
    mov edi, r13d             ; zero-extends to rdi
    call print_u64

    ; --- Scan all regions ---
    lea r14, [rbx + REGION_TABLE_OFFSET]  ; table base

    ; Accumulators
    xor r15d, r15d            ; condemned count
    mov dword [rsp + 0], 0    ; total_hits
    mov dword [rsp + 4], 0    ; total_misses
    mov dword [rsp + 8], 0    ; active_count
    mov dword [rsp + 12], 0   ; high_accuracy_count
    mov dword [rsp + 16], 0   ; max_hits_idx
    mov dword [rsp + 20], 0   ; max_hits_val

    xor ecx, ecx             ; index
.scan_loop:
    cmp ecx, r13d
    jge .scan_done
    push rcx

    ; Get entry
    imul rdi, rcx, RTE_SIZE
    add rdi, r14

    ; Skip condemned
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_CONDEMNED
    jnz .next_region

    ; Skip frozen
    test eax, RFLAG_FROZEN
    jnz .count_active

    ; Get hits/misses from the actual region header (authoritative)
    mov rsi, [rdi + RTE_ADDR]
    mov eax, [rsi + RHDR_HITS]
    mov edx, [rsi + RHDR_MISSES]

    ; Sync to table
    mov [rdi + RTE_HITS], eax
    mov [rdi + RTE_MISSES], edx

    ; Accumulate
    add [rsp + 8 + 0], eax     ; total_hits (offset adjusted for pushed rcx)
    add [rsp + 8 + 4], edx     ; total_misses

    ; Compute accuracy for this region
    mov ecx, eax
    add ecx, edx              ; total = hits + misses
    test ecx, ecx
    jz .count_active           ; no data yet

    ; Check for prune condition: accuracy < 0.1 AND age > PRUNE_MIN_AGE
    ; age = global_step - birth_step
    mov edx, [rsi + RHDR_BIRTH]
    mov ecx, r12d
    sub ecx, edx              ; age
    cmp ecx, PRUNE_MIN_AGE
    jl .check_promote

    ; Compute accuracy: hits / (hits + misses)
    mov eax, [rsi + RHDR_HITS]
    mov edx, [rsi + RHDR_MISSES]
    add edx, eax
    ; Float comparison: accuracy < 0.1
    cvtsi2ss xmm0, eax
    cvtsi2ss xmm1, edx
    divss xmm0, xmm1          ; accuracy in xmm0

    mov eax, PRUNE_ACCURACY   ; 0.1f
    movd xmm1, eax
    comiss xmm0, xmm1
    ja .check_promote          ; accuracy > 0.1, don't prune

    ; CONDEMN this region — record causal link first
    mov rdi, rsi              ; header ptr
    push rsi
    call log_causal
    pop rsi
    mov rdi, rsi
    call region_condemn
    inc r15d
    jmp .next_region

.check_promote:
    ; Check if this is a high-accuracy region
    mov eax, [rsi + RHDR_HITS]
    mov edx, [rsi + RHDR_MISSES]
    add edx, eax
    test edx, edx
    jz .count_active
    cvtsi2ss xmm0, eax
    cvtsi2ss xmm1, edx
    divss xmm0, xmm1

    mov eax, PROMOTE_ACCURACY ; 0.8f
    movd xmm1, eax
    comiss xmm0, xmm1
    jb .count_active

    ; High accuracy region found
    inc dword [rsp + 8 + 12]   ; high_accuracy_count

.count_active:
    inc dword [rsp + 8 + 8]    ; active_count

.next_region:
    pop rcx
    inc ecx
    jmp .scan_loop

.scan_done:
    ; --- Update causal post-accuracy if pending ---
    lea rax, [rbx + STATE_OFFSET + ST_CAUSAL_MOD_ADDR]
    mov rdi, [rax]
    test rdi, rdi
    jz .no_causal
    ; Compute post-accuracy of the modified region
    mov eax, [rdi + RHDR_HITS]
    mov edx, [rdi + RHDR_MISSES]
    add edx, eax
    test edx, edx
    jz .causal_clear
    cvtsi2ss xmm0, eax
    cvtsi2ss xmm1, edx
    divss xmm0, xmm1
    lea rax, [rbx + STATE_OFFSET + ST_CAUSAL_POST_ACC]
    movss [rax], xmm0
.causal_clear:
    ; Clear the pending causal address (record complete)
    lea rax, [rbx + STATE_OFFSET + ST_CAUSAL_MOD_ADDR]
    mov qword [rax], 0
.no_causal:

    ; --- Update presence fields ---
    call update_presence

    ; --- Update drives ---
    call update_drives

    ; --- Check drive thresholds, set dispatch mode ---
    call drives_check

    ; --- Compute introspective state ---
    call compute_intro_state

    ; --- Detect episodes (state transitions) ---
    call detect_episode

    ; --- Compute accuracy variance across regions ---
    call compute_accuracy_variance

    ; --- Decay connection weights (forgetting) ---
    call decay_connection_weights

    ; --- Repair dead-end routing ---
    call repair_routing

    ; --- Auto-trigger dream if misses exceed threshold ---
    lea rax, [rbx + STATE_OFFSET + ST_MISS_POS]
    mov eax, [rax]
    mov ecx, [rbx + STATE_OFFSET + ST_DREAM_MISS_THRESH]
    test ecx, ecx
    jnz .thresh_ok
    mov ecx, 128              ; default threshold
    mov [rbx + STATE_OFFSET + ST_DREAM_MISS_THRESH], ecx
.thresh_ok:
    cmp eax, ecx
    jl .no_auto_dream
    ; Misses accumulated — trigger dream consolidation
    push r15
    call dream_cycle
    pop r15
    ; Reset miss position after dream
    mov dword [rbx + STATE_OFFSET + ST_MISS_POS], 0
.no_auto_dream:

    ; --- Track recent condemnations for presence decay ---
    mov [rbx + STATE_OFFSET + ST_RECENT_CONDEMNS], r15d

    ; --- Self-consumption: metabolize condemned regions into energy ---
    ; Dead patterns become fuel. Their knowledge is recycled.
    test r15d, r15d
    jz .no_consume
    ; Each condemned region yields ENERGY_CONSUME_RATE energy
    movzx eax, r15w
    cvtsi2sd xmm0, eax
    mov rax, ENERGY_CONSUME_RATE
    movq xmm1, rax
    mulsd xmm0, xmm1            ; total energy from consumption
    ; Add to energy pool
    addsd xmm0, [rbx + STATE_OFFSET + ST_ENERGY]
    ; Cap at ENERGY_MAX
    mov rax, ENERGY_MAX
    movq xmm1, rax
    minsd xmm0, xmm1
    movsd [rbx + STATE_OFFSET + ST_ENERGY], xmm0
    ; Track consumption stats
    add [rbx + STATE_OFFSET + ST_METABOLIZED_COUNT], r15d
    ; Add to consumed_energy accumulator
    movzx eax, r15w
    cvtsi2sd xmm0, eax
    mov rax, ENERGY_CONSUME_RATE
    movq xmm1, rax
    mulsd xmm0, xmm1
    addsd xmm0, [rbx + STATE_OFFSET + ST_CONSUMED_ENERGY]
    movsd [rbx + STATE_OFFSET + ST_CONSUMED_ENERGY], xmm0
.no_consume:

    ; --- Reset novelty window for next observation period ---
    mov dword [rbx + STATE_OFFSET + ST_NOVELTY_RECENT], 0
    mov eax, [rbx + STATE_OFFSET + ST_TOKEN_COUNT]
    mov [rbx + STATE_OFFSET + ST_NOVELTY_WINDOW], eax

    ; Print results
    lea rdi, [rel obs_accuracy]
    call print_cstr
    ; Compute overall accuracy
    mov eax, [rsp + 0]        ; total_hits
    mov edx, [rsp + 4]        ; total_misses
    add edx, eax
    test edx, edx
    jz .zero_acc
    cvtsi2ss xmm0, eax
    cvtsi2ss xmm1, edx
    divss xmm0, xmm1
    jmp .print_acc
.zero_acc:
    xorps xmm0, xmm0
.print_acc:
    call print_f32

    lea rdi, [rel obs_condemned]
    call print_cstr
    movzx rdi, r15w
    call print_u64
    call print_newline

    ; --- Metabolic cost: observation consumes energy ---
    mov rax, ENERGY_OBSERVE_COST
    movq xmm0, rax
    movsd xmm1, [rbx + STATE_OFFSET + ST_ENERGY]
    subsd xmm1, xmm0
    xorpd xmm2, xmm2
    maxsd xmm1, xmm2            ; floor at 0
    movsd [rbx + STATE_OFFSET + ST_ENERGY], xmm1
    ; Track spending
    addsd xmm0, [rbx + STATE_OFFSET + ST_ENERGY_SPENT]
    movsd [rbx + STATE_OFFSET + ST_ENERGY_SPENT], xmm0

    ; --- Reset coherence tracking for next window ---
    mov dword [rbx + STATE_OFFSET + ST_COHERENCE_AGREE], 0
    mov dword [rbx + STATE_OFFSET + ST_COHERENCE_DISAGREE], 0

    ; --- Self-reading: decode own regions, understand what they do ---
    ; The system reads its own code as data — homoiconic introspection
    call introspect_scan_regions

    ; Fire observe hook
    mov edi, HOOK_ON_OBSERVE
    mov esi, r15d
    call fire_hook

    ; Update last observation step
    lea rax, [rbx + STATE_OFFSET + ST_OBS_LAST_STEP]
    mov [rax], r12

    add rsp, 64
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; update_presence
;; Compute all 30 presence fields from actual system state
;; ============================================================
update_presence:
    push rbx
    push r12
    push r13
    mov rbx, SURFACE_BASE
    lea rdi, [rbx + STATE_OFFSET + ST_PRESENCE]
    mov r12, rdi              ; presence base

    ; --- Gather metrics for computations ---
    ; r13d = region_count
    mov r13d, [rbx + STATE_OFFSET + ST_REGION_COUNT]

    ; Compute total hits/misses for accuracy ratio
    xor eax, eax              ; total_hits
    xor edx, edx              ; total_misses
    lea rsi, [rbx + REGION_TABLE_OFFSET]
    xor ecx, ecx
.pres_scan:
    cmp ecx, r13d
    jge .pres_scan_done
    push rcx
    imul rcx, rcx, RTE_SIZE
    add eax, [rsi + rcx + RTE_HITS]
    add edx, [rsi + rcx + RTE_MISSES]
    pop rcx
    inc ecx
    jmp .pres_scan
.pres_scan_done:
    ; eax = total_hits, edx = total_misses
    push rdx
    push rax
    ; Compute overall accuracy → xmm7
    add edx, eax
    test edx, edx
    jz .pres_acc_zero
    cvtsi2ss xmm7, eax
    cvtsi2ss xmm6, edx
    divss xmm7, xmm6         ; xmm7 = overall accuracy
    jmp .pres_start
.pres_acc_zero:
    xorps xmm7, xmm7
.pres_start:
    pop rax                   ; total_hits
    pop rdx                   ; total_misses

    ; [0] TEXTURE: hash of recent token pattern → [0,1]
    mov ecx, [rbx + STATE_OFFSET + ST_CTX_HASH]
    and ecx, 0x7FFFFF
    cvtsi2ss xmm0, ecx
    mov ecx, 0x7FFFFF
    cvtsi2ss xmm1, ecx
    divss xmm0, xmm1
    movss [r12 + PRES_TEXTURE * 4], xmm0

    ; [1] CONTINUITY: low surprise = high continuity
    mov ecx, [rbx + STATE_OFFSET + ST_SURPRISE_TYPE]
    test ecx, ecx
    jnz .pres_cont_low
    mov ecx, 0x3F800000       ; 1.0f (no surprise = full continuity)
    movd xmm0, ecx
    jmp .pres_cont_s
.pres_cont_low:
    mov ecx, 0x3E800000       ; 0.25f (surprise = low continuity)
    movd xmm0, ecx
.pres_cont_s:
    movss [r12 + PRES_CONTINUITY * 4], xmm0

    ; [2] NOVELTY: 1.0 - accuracy (low accuracy = high novelty)
    mov ecx, 0x3F800000       ; 1.0f
    movd xmm0, ecx
    subss xmm0, xmm7
    movss [r12 + PRES_NOVELTY * 4], xmm0

    ; [3] AROUSAL: modification rate (causal_count / step)
    mov ecx, [rbx + STATE_OFFSET + ST_CAUSAL_COUNT]
    cvtsi2ss xmm0, ecx
    mov ecx, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    test ecx, ecx
    jz .pres_arousal_zero
    cvtsi2ss xmm1, ecx
    divss xmm0, xmm1
    ; Clamp to [0,1]
    mov ecx, 0x3F800000
    movd xmm1, ecx
    minss xmm0, xmm1
    jmp .pres_arousal_s
.pres_arousal_zero:
    xorps xmm0, xmm0
.pres_arousal_s:
    movss [r12 + PRES_AROUSAL * 4], xmm0

    ; [4] VALENCE: accuracy trending (accuracy itself)
    movss [r12 + PRES_VALENCE * 4], xmm7

    ; [5] UNCERTAINTY: accuracy variance
    movss xmm0, [rbx + STATE_OFFSET + ST_ACCURACY_VARIANCE]
    movss [r12 + PRES_UNCERTAINTY * 4], xmm0

    ; [6] ENGAGEMENT: trace candidates / region_count (how many dispatches considered)
    mov ecx, [rbx + STATE_OFFSET + ST_TRACE_CANDIDATES]
    cvtsi2ss xmm0, ecx
    cvtsi2ss xmm1, r13d
    test r13d, r13d
    jz .pres_engage_z
    divss xmm0, xmm1
    mov ecx, 0x3F800000
    movd xmm1, ecx
    minss xmm0, xmm1
    jmp .pres_engage_s
.pres_engage_z:
    xorps xmm0, xmm0
.pres_engage_s:
    movss [r12 + PRES_ENGAGEMENT * 4], xmm0

    ; [7] COHERENCE: schema_hits / schema_total
    mov ecx, [rbx + STATE_OFFSET + ST_SCHEMA_HITS]
    mov edx, [rbx + STATE_OFFSET + ST_SCHEMA_TOTAL]
    test edx, edx
    jz .pres_coher_z
    cvtsi2ss xmm0, ecx
    cvtsi2ss xmm1, edx
    divss xmm0, xmm1
    jmp .pres_coher_s
.pres_coher_z:
    xorps xmm0, xmm0
.pres_coher_s:
    movss [r12 + PRES_COHERENCE * 4], xmm0

    ; [8] FOCUS: trace_matched / trace_candidates (how concentrated)
    mov ecx, [rbx + STATE_OFFSET + ST_TRACE_MATCHED]
    mov edx, [rbx + STATE_OFFSET + ST_TRACE_CANDIDATES]
    test edx, edx
    jz .pres_focus_z
    cvtsi2ss xmm0, ecx
    cvtsi2ss xmm1, edx
    divss xmm0, xmm1
    jmp .pres_focus_s
.pres_focus_z:
    xorps xmm0, xmm0
.pres_focus_s:
    movss [r12 + PRES_FOCUS * 4], xmm0

    ; [9] FATIGUE: step / (region_count * 100) — high step per region = fatigue
    mov ecx, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    cvtsi2ss xmm0, ecx
    imul edx, r13d, 100
    test edx, edx
    jz .pres_fatigue_z
    cvtsi2ss xmm1, edx
    divss xmm0, xmm1
    mov ecx, 0x3F800000
    movd xmm1, ecx
    minss xmm0, xmm1
    jmp .pres_fatigue_s
.pres_fatigue_z:
    xorps xmm0, xmm0
.pres_fatigue_s:
    movss [r12 + PRES_FATIGUE * 4], xmm0

    ; [10] MOMENTUM: recent_emits / 10 (learning momentum)
    mov ecx, [rbx + STATE_OFFSET + ST_RECENT_EMITS]
    cvtsi2ss xmm0, ecx
    mov ecx, 10
    cvtsi2ss xmm1, ecx
    divss xmm0, xmm1
    mov ecx, 0x3F800000
    movd xmm1, ecx
    minss xmm0, xmm1
    movss [r12 + PRES_MOMENTUM * 4], xmm0

    ; [11] STABILITY: 1.0 - variance (low variance = stable)
    mov ecx, 0x3F800000
    movd xmm0, ecx
    subss xmm0, [rbx + STATE_OFFSET + ST_ACCURACY_VARIANCE]
    xorps xmm1, xmm1
    maxss xmm0, xmm1         ; clamp >= 0
    movss [r12 + PRES_STABILITY * 4], xmm0

    ; [12] COMPLEXITY: region_count / REGION_TABLE_MAX
    cvtsi2ss xmm0, r13d
    mov ecx, REGION_TABLE_MAX
    cvtsi2ss xmm1, ecx
    divss xmm0, xmm1
    movss [r12 + PRES_COMPLEXITY * 4], xmm0

    ; [13] DENSITY: total_hits / region_count
    cvtsi2ss xmm0, eax       ; total_hits (still in eax from scan)
    cvtsi2ss xmm1, r13d
    test r13d, r13d
    jz .pres_dens_z
    divss xmm0, xmm1
    ; Normalize to [0,1] by dividing by 100
    mov ecx, 100
    cvtsi2ss xmm1, ecx
    divss xmm0, xmm1
    mov ecx, 0x3F800000
    movd xmm1, ecx
    minss xmm0, xmm1
    jmp .pres_dens_s
.pres_dens_z:
    xorps xmm0, xmm0
.pres_dens_s:
    movss [r12 + PRES_DENSITY * 4], xmm0

    ; [14] TEMPERATURE: dispatch_mode / 3.0 (higher mode = hotter)
    mov ecx, [rbx + STATE_OFFSET + ST_DISPATCH_MODE]
    cvtsi2ss xmm0, ecx
    mov ecx, 3
    cvtsi2ss xmm1, ecx
    divss xmm0, xmm1
    movss [r12 + PRES_TEMPERATURE * 4], xmm0

    ; [15] PRESSURE: dispatch_ptr usage / max
    mov rcx, [rbx + STATE_OFFSET + ST_DISPATCH_PTR]
    mov rdx, SURFACE_BASE + DISPATCH_OFFSET
    sub rcx, rdx
    cvtsi2ss xmm0, ecx
    mov ecx, DISPATCH_MAX_SIZE
    cvtsi2ss xmm1, ecx
    divss xmm0, xmm1
    movss [r12 + PRES_PRESSURE * 4], xmm0

    ; [16] ENTROPY: miss_pos / miss_buf_cap
    mov ecx, [rbx + STATE_OFFSET + ST_MISS_POS]
    cvtsi2ss xmm0, ecx
    mov ecx, ST_MISS_BUF_CAP
    cvtsi2ss xmm1, ecx
    divss xmm0, xmm1
    movss [r12 + PRES_ENTROPY * 4], xmm0

    ; [17] RHYTHM: self-prediction accuracy (predictable = rhythmic)
    mov ecx, [rbx + STATE_OFFSET + ST_SELF_PRED_HITS]
    mov edx, [rbx + STATE_OFFSET + ST_SELF_PRED_MISSES]
    add edx, ecx
    test edx, edx
    jz .pres_rhythm_z
    cvtsi2ss xmm0, ecx
    cvtsi2ss xmm1, edx
    divss xmm0, xmm1
    jmp .pres_rhythm_s
.pres_rhythm_z:
    xorps xmm0, xmm0
.pres_rhythm_s:
    movss [r12 + PRES_RHYTHM * 4], xmm0

    ; [18] DEPTH: trace_candidates (normalized)
    mov ecx, [rbx + STATE_OFFSET + ST_TRACE_CANDIDATES]
    cvtsi2ss xmm0, ecx
    mov ecx, r13d
    test ecx, ecx
    jz .pres_depth_z
    cvtsi2ss xmm1, ecx
    divss xmm0, xmm1
    mov ecx, 0x3F800000
    movd xmm1, ecx
    minss xmm0, xmm1
    jmp .pres_depth_s
.pres_depth_z:
    xorps xmm0, xmm0
.pres_depth_s:
    movss [r12 + PRES_DEPTH * 4], xmm0

    ; [19] BREADTH: trace_matched (normalized)
    mov ecx, [rbx + STATE_OFFSET + ST_TRACE_MATCHED]
    cvtsi2ss xmm0, ecx
    mov ecx, 10
    cvtsi2ss xmm1, ecx
    divss xmm0, xmm1
    mov ecx, 0x3F800000
    movd xmm1, ecx
    minss xmm0, xmm1
    movss [r12 + PRES_BREADTH * 4], xmm0

    ; [20] RESONANCE: schema_hits / max(1, total_hits)
    mov ecx, [rbx + STATE_OFFSET + ST_SCHEMA_HITS]
    cvtsi2ss xmm0, ecx
    mov ecx, eax             ; total_hits
    test ecx, ecx
    jnz .pres_res_ok
    mov ecx, 1
.pres_res_ok:
    cvtsi2ss xmm1, ecx
    divss xmm0, xmm1
    mov ecx, 0x3F800000
    movd xmm1, ecx
    minss xmm0, xmm1
    movss [r12 + PRES_RESONANCE * 4], xmm0

    ; [21] DISSONANCE: surprise_type / 2.0
    mov ecx, [rbx + STATE_OFFSET + ST_SURPRISE_TYPE]
    cvtsi2ss xmm0, ecx
    mov ecx, 2
    cvtsi2ss xmm1, ecx
    divss xmm0, xmm1
    movss [r12 + PRES_DISSONANCE * 4], xmm0

    ; [22] GROWTH: recent_emits / step (growth rate)
    mov ecx, [rbx + STATE_OFFSET + ST_RECENT_EMITS]
    cvtsi2ss xmm0, ecx
    mov ecx, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    test ecx, ecx
    jz .pres_growth_z
    cvtsi2ss xmm1, ecx
    divss xmm0, xmm1
    ; Scale up for visibility
    mov ecx, 10
    cvtsi2ss xmm1, ecx
    mulss xmm0, xmm1
    mov ecx, 0x3F800000
    movd xmm1, ecx
    minss xmm0, xmm1
    jmp .pres_growth_s
.pres_growth_z:
    xorps xmm0, xmm0
.pres_growth_s:
    movss [r12 + PRES_GROWTH * 4], xmm0

    ; [23] DECAY: recent_condemns / region_count
    mov ecx, [rbx + STATE_OFFSET + ST_RECENT_CONDEMNS]
    cvtsi2ss xmm0, ecx
    cvtsi2ss xmm1, r13d
    test r13d, r13d
    jz .pres_decay_z
    divss xmm0, xmm1
    jmp .pres_decay_s
.pres_decay_z:
    xorps xmm0, xmm0
.pres_decay_s:
    movss [r12 + PRES_DECAY * 4], xmm0

    ; [24] SYMMETRY: abs(hits - misses) / (hits + misses) inverted
    ; 1.0 when balanced, 0.0 when all one side
    push rax
    mov ecx, eax              ; total_hits in ecx
    pop rax
    push rax
    ; edx still has total_misses from earlier? No, edx was used.
    ; Recompute from drives
    movss xmm0, [rbx + STATE_OFFSET + ST_DRIVES]  ; accuracy
    ; symmetry = 2 * min(acc, 1-acc) — peaks at 0.5
    mov ecx, 0x3F800000
    movd xmm1, ecx
    movss xmm2, xmm1
    subss xmm2, xmm0         ; 1 - acc
    minss xmm0, xmm2         ; min(acc, 1-acc)
    mov ecx, 0x40000000       ; 2.0f
    movd xmm1, ecx
    mulss xmm0, xmm1         ; 2 * min → peaks at 1.0 when acc=0.5
    movss [r12 + PRES_SYMMETRY * 4], xmm0
    pop rax

    ; [25] SURPRISE: surprise_type > 0 → 1.0, else 0.0
    mov ecx, [rbx + STATE_OFFSET + ST_SURPRISE_TYPE]
    test ecx, ecx
    jz .pres_surp_z
    mov ecx, 0x3F800000
    movd xmm0, ecx
    jmp .pres_surp_s
.pres_surp_z:
    xorps xmm0, xmm0
.pres_surp_s:
    movss [r12 + PRES_SURPRISE * 4], xmm0

    ; [26] FAMILIARITY: accuracy (high accuracy = familiar patterns)
    movss [r12 + PRES_FAMILIARITY * 4], xmm7

    ; [27] AGENCY: causal_count > 0 means active self-modification
    mov ecx, [rbx + STATE_OFFSET + ST_CAUSAL_COUNT]
    cvtsi2ss xmm0, ecx
    mov ecx, 100
    cvtsi2ss xmm1, ecx
    divss xmm0, xmm1
    mov ecx, 0x3F800000
    movd xmm1, ecx
    minss xmm0, xmm1
    movss [r12 + PRES_AGENCY * 4], xmm0

    ; [28] INTEGRATION: self_pred_hits / (self_pred_hits + self_pred_misses)
    mov ecx, [rbx + STATE_OFFSET + ST_SELF_PRED_HITS]
    mov edx, [rbx + STATE_OFFSET + ST_SELF_PRED_MISSES]
    add edx, ecx
    test edx, edx
    jz .pres_integ_z
    cvtsi2ss xmm0, ecx
    cvtsi2ss xmm1, edx
    divss xmm0, xmm1
    jmp .pres_integ_s
.pres_integ_z:
    xorps xmm0, xmm0
.pres_integ_s:
    movss [r12 + PRES_INTEGRATION * 4], xmm0

    ; [29] META_AWARENESS: intro_state != IDLE → higher awareness
    mov ecx, [rbx + STATE_OFFSET + ST_INTRO_STATE]
    test ecx, ecx
    jz .pres_meta_z
    ; Non-idle: awareness = 0.5 + state/14 (gives 0.57-1.0 range)
    cvtsi2ss xmm0, ecx
    mov ecx, 14
    cvtsi2ss xmm1, ecx
    divss xmm0, xmm1
    mov ecx, 0x3F000000       ; 0.5f
    movd xmm1, ecx
    addss xmm0, xmm1
    mov ecx, 0x3F800000
    movd xmm1, ecx
    minss xmm0, xmm1
    jmp .pres_meta_s
.pres_meta_z:
    xorps xmm0, xmm0
.pres_meta_s:
    movss [r12 + PRES_META_AWARENESS * 4], xmm0

    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; update_drives
;; Compute drive levels from current metrics
;; ============================================================
update_drives:
    push rbx
    mov rbx, SURFACE_BASE
    lea rdi, [rbx + STATE_OFFSET + ST_DRIVES]

    ; Drive 0: Accuracy — computed from overall hit rate
    ; Higher is better, drive ACTIVATES when accuracy is LOW
    lea rax, [rbx + REGION_TABLE_OFFSET]
    lea rcx, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov ecx, [rcx]
    xor r8d, r8d              ; total hits
    xor r9d, r9d              ; total misses
    xor edx, edx
.drive_scan:
    cmp edx, ecx
    jge .drive_scan_done
    imul rsi, rdx, RTE_SIZE
    add rsi, rax
    add r8d, [rsi + RTE_HITS]
    add r9d, [rsi + RTE_MISSES]
    inc edx
    jmp .drive_scan
.drive_scan_done:
    mov eax, r8d
    add eax, r9d
    test eax, eax
    jz .acc_zero
    cvtsi2ss xmm0, r8d
    cvtsi2ss xmm1, eax
    divss xmm0, xmm1         ; accuracy
    jmp .store_acc
.acc_zero:
    xorps xmm0, xmm0
.store_acc:
    movss [rdi + 0], xmm0     ; drive_accuracy = current accuracy

    ; Drive 1: Efficiency — surface usage ratio
    lea rax, [rbx + STATE_OFFSET + ST_DISPATCH_PTR]
    mov rax, [rax]
    mov rcx, SURFACE_BASE + DISPATCH_OFFSET
    sub rax, rcx
    cvtsi2ss xmm0, rax
    mov eax, DISPATCH_MAX_SIZE
    cvtsi2ss xmm1, eax
    divss xmm0, xmm1
    movss [rdi + 4], xmm0     ; drive_efficiency = usage ratio

    ; Drive 2: Novelty — ratio of new unique tokens to total tokens in window
    ; Genuine novelty: how many previously-unseen patterns appeared recently
    mov ecx, [rbx + STATE_OFFSET + ST_NOVELTY_RECENT]
    mov edx, [rbx + STATE_OFFSET + ST_NOVELTY_WINDOW]
    test edx, edx
    jz .novelty_zero
    cvtsi2ss xmm0, ecx
    cvtsi2ss xmm1, edx
    divss xmm0, xmm1          ; novelty = new_unique / total_window
    mov ecx, 0x3F800000        ; clamp to 1.0
    movd xmm1, ecx
    minss xmm0, xmm1
    movss [rdi + 8], xmm0
    jmp .drive_coherence
.novelty_zero:
    xorps xmm0, xmm0
    movss [rdi + 8], xmm0

.drive_coherence:
    ; Drive 3: Coherence — holographic and graph prediction agreement ratio
    ; When holo and graph agree, system is internally consistent
    ; When they disagree, there's tension that needs resolution
    mov ecx, [rbx + STATE_OFFSET + ST_COHERENCE_AGREE]
    mov edx, [rbx + STATE_OFFSET + ST_COHERENCE_DISAGREE]
    add edx, ecx              ; total predictions with both sources
    test edx, edx
    jz .coherence_zero
    cvtsi2ss xmm0, ecx
    cvtsi2ss xmm1, edx
    divss xmm0, xmm1          ; coherence = agree / (agree + disagree)
    movss [rdi + 12], xmm0
    jmp .drives_computed
.coherence_zero:
    xorps xmm0, xmm0
    movss [rdi + 12], xmm0
.drives_computed:

    pop rbx
    ret

;; ============================================================
;; presence_show
;; Print all 30 presence fields
;; ============================================================
global presence_show
presence_show:
    push rbx
    push r12

    mov rbx, SURFACE_BASE
    lea r12, [rbx + STATE_OFFSET + ST_PRESENCE]

    lea rdi, [rel pres_hdr]
    call print_cstr

    xor ecx, ecx
.show_loop:
    cmp ecx, NUM_PRESENCE
    jge .show_done
    push rcx

    ; Print index
    movzx rdi, cx
    call print_u64

    lea rdi, [rel pres_sep]
    call print_cstr

    ; Print value
    pop rcx
    push rcx
    movss xmm0, [r12 + rcx * 4]
    call print_f32
    call print_newline

    pop rcx
    inc ecx
    jmp .show_loop

.show_done:
    pop r12
    pop rbx
    ret

;; ============================================================
;; compute_intro_state
;; Determines the system's introspective state from metrics:
;;   CONFUSED: accuracy < 0.2 with many regions
;;   CONFIDENT: accuracy > 0.7
;;   LEARNING: recent emissions high
;;   STUCK: low accuracy, no recent emissions, many steps
;;   EXPLORING: dispatch mode is EXPLORE
;;   CONSOLIDATING: many nursery regions
;;   IDLE: default
;; ============================================================
compute_intro_state:
    push rbx
    mov rbx, SURFACE_BASE

    ; Get overall accuracy (drive_accuracy holds this)
    movss xmm0, [rbx + STATE_OFFSET + ST_DRIVES]

    ; Check CONFUSED: accuracy < 0.2 with regions > 5
    mov eax, 0x3E4CCCCD        ; 0.2f
    movd xmm1, eax
    comiss xmm0, xmm1
    ja .not_confused
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    cmp dword [rax], 5
    jle .not_confused
    ; CONFUSED
    lea rax, [rbx + STATE_OFFSET + ST_INTRO_STATE]
    mov dword [rax], INTRO_CONFUSED
    jmp .intro_done

.not_confused:
    ; Check CONFIDENT: accuracy > 0.7
    mov eax, 0x3F333333        ; 0.7f
    movd xmm1, eax
    comiss xmm0, xmm1
    jbe .not_confident
    lea rax, [rbx + STATE_OFFSET + ST_INTRO_STATE]
    mov dword [rax], INTRO_CONFIDENT
    jmp .intro_done

.not_confident:
    ; Check LEARNING: recent emissions > 3
    lea rax, [rbx + STATE_OFFSET + ST_RECENT_EMITS]
    cmp dword [rax], 3
    jle .not_learning
    lea rax, [rbx + STATE_OFFSET + ST_INTRO_STATE]
    mov dword [rax], INTRO_LEARNING
    ; Reset emission counter after observation
    lea rax, [rbx + STATE_OFFSET + ST_RECENT_EMITS]
    mov dword [rax], 0
    jmp .intro_done

.not_learning:
    ; Check EXPLORING: dispatch mode is EXPLORE
    lea rax, [rbx + STATE_OFFSET + ST_DISPATCH_MODE]
    cmp dword [rax], DMODE_EXPLORE
    jne .not_exploring
    lea rax, [rbx + STATE_OFFSET + ST_INTRO_STATE]
    mov dword [rax], INTRO_EXPLORING
    jmp .intro_done

.not_exploring:
    ; Check CONSOLIDATING: nursery regions exist (dreamed but not yet promoted/condemned)
    ; Scan for NURSERY-flagged regions
    push rcx
    lea rax, [rbx + REGION_TABLE_OFFSET]
    mov ecx, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    xor edx, edx
.consolidate_scan:
    cmp edx, ecx
    jge .not_consolidating
    imul esi, edx, RTE_SIZE
    movzx edi, word [rax + rsi + RTE_FLAGS]
    test edi, RFLAG_NURSERY
    jnz .is_consolidating
    inc edx
    jmp .consolidate_scan
.is_consolidating:
    pop rcx
    lea rax, [rbx + STATE_OFFSET + ST_INTRO_STATE]
    mov dword [rax], INTRO_CONSOLIDATING
    jmp .intro_done
.not_consolidating:
    pop rcx

    ; Check STUCK: accuracy < 0.4, many steps, no recent emissions
    mov eax, 0x3ECCCCCD        ; 0.4f
    movd xmm1, eax
    comiss xmm0, xmm1
    ja .not_stuck
    lea rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    cmp qword [rax], 50
    jl .not_stuck
    lea rax, [rbx + STATE_OFFSET + ST_RECENT_EMITS]
    cmp dword [rax], 0
    jne .not_stuck
    lea rax, [rbx + STATE_OFFSET + ST_INTRO_STATE]
    mov dword [rax], INTRO_STUCK
    jmp .intro_done

.not_stuck:
    ; Default: IDLE
    lea rax, [rbx + STATE_OFFSET + ST_INTRO_STATE]
    mov dword [rax], INTRO_IDLE

.intro_done:
    pop rbx
    ret

;; ============================================================
;; detect_episode
;; Compare current intro_state with previous. If changed,
;; write an entry to the episode ring buffer.
;; ============================================================
detect_episode:
    push rbx
    mov rbx, SURFACE_BASE

    ; Compare current with previous
    mov eax, [rbx + STATE_OFFSET + ST_INTRO_STATE]
    cmp eax, [rbx + STATE_OFFSET + ST_PREV_INTRO_STATE]
    je .no_episode

    ; Also check dispatch mode change
    jmp .record_episode

.no_episode:
    mov eax, [rbx + STATE_OFFSET + ST_DISPATCH_MODE]
    cmp eax, [rbx + STATE_OFFSET + ST_PREV_DISPATCH_MODE]
    je .ep_save_prev

.record_episode:
    ; State changed — record episode boundary
    lea rdi, [rbx + STATE_OFFSET + ST_EPISODE_RING]
    mov ecx, [rbx + STATE_OFFSET + ST_EPISODE_POS]
    imul edx, ecx, ST_EPISODE_ENTRY
    add rdi, rdx
    ; Store step
    mov rax, [rbx + STATE_OFFSET + ST_GLOBAL_STEP]
    mov [rdi], rax
    ; Store context hash
    mov rax, [rbx + STATE_OFFSET + ST_CTX_HASH]
    mov [rdi + 8], rax
    ; Advance position
    inc ecx
    cmp ecx, ST_EPISODE_CAP
    jl .ep_no_wrap
    xor ecx, ecx
.ep_no_wrap:
    mov [rbx + STATE_OFFSET + ST_EPISODE_POS], ecx

    ; Fire episode hook
    push rbx
    mov edi, HOOK_ON_EPISODE
    xor esi, esi
    call fire_hook
    pop rbx

.ep_save_prev:
    ; Save current as prev
    mov eax, [rbx + STATE_OFFSET + ST_INTRO_STATE]
    mov [rbx + STATE_OFFSET + ST_PREV_INTRO_STATE], eax
    mov eax, [rbx + STATE_OFFSET + ST_DISPATCH_MODE]
    mov [rbx + STATE_OFFSET + ST_PREV_DISPATCH_MODE], eax

    pop rbx
    ret

;; ============================================================
;; compute_accuracy_variance
;; Walk regions, compute per-region accuracy, compute variance
;; Stores result in ST_ACCURACY_VARIANCE
;; ============================================================
compute_accuracy_variance:
    push rbx
    push r12
    push r13
    sub rsp, 16               ; [0]=sum (f32), [4]=sum_sq (f32), [8]=count (u32)

    mov rbx, SURFACE_BASE
    xorps xmm0, xmm0
    movss [rsp + 0], xmm0    ; sum
    movss [rsp + 4], xmm0    ; sum_sq
    mov dword [rsp + 8], 0   ; count

    lea r12, [rbx + REGION_TABLE_OFFSET]
    mov r13d, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    xor ecx, ecx
.vl:
    cmp ecx, r13d
    jge .vdone
    push rcx

    imul rdi, rcx, RTE_SIZE
    add rdi, r12
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_CONDEMNED
    jnz .vnext

    mov rsi, [rdi + RTE_ADDR]
    mov eax, [rsi + RHDR_HITS]
    mov edx, [rsi + RHDR_MISSES]
    add edx, eax
    cmp edx, 2               ; need at least 2 events
    jl .vnext

    ; accuracy
    cvtsi2ss xmm0, eax
    cvtsi2ss xmm1, edx
    divss xmm0, xmm1         ; xmm0 = acc

    ; sum += acc
    movss xmm2, [rsp + 8 + 0]
    addss xmm2, xmm0
    movss [rsp + 8 + 0], xmm2

    ; sum_sq += acc*acc
    movss xmm2, xmm0
    mulss xmm2, xmm0
    addss xmm2, [rsp + 8 + 4]
    movss [rsp + 8 + 4], xmm2

    ; count++
    inc dword [rsp + 8 + 8]

.vnext:
    pop rcx
    inc ecx
    jmp .vl

.vdone:
    ; variance = sum_sq/count - (sum/count)^2
    mov eax, [rsp + 8]
    test eax, eax
    jz .vzero

    cvtsi2ss xmm2, eax       ; count as float
    movss xmm0, [rsp + 4]    ; sum_sq
    divss xmm0, xmm2         ; E[x^2]
    movss xmm1, [rsp + 0]    ; sum
    divss xmm1, xmm2         ; E[x]
    mulss xmm1, xmm1         ; E[x]^2
    subss xmm0, xmm1         ; variance
    ; Clamp to >= 0 (rounding errors)
    xorps xmm1, xmm1
    maxss xmm0, xmm1
    movss [rbx + STATE_OFFSET + ST_ACCURACY_VARIANCE], xmm0
    jmp .vret

.vzero:
    xorps xmm0, xmm0
    movss [rbx + STATE_OFFSET + ST_ACCURACY_VARIANCE], xmm0

.vret:
    add rsp, 16
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; decay_connection_weights
;; Walk all regions, multiply all 4 weights by WEIGHT_DECAY.
;; Clear connections where weight < WEIGHT_FLOOR.
;; This is the "forgetting" that prevents graph saturation.
;; ============================================================
global decay_connection_weights
decay_connection_weights:
    push rbx
    push r12
    push r13

    mov rbx, SURFACE_BASE
    lea r12, [rbx + REGION_TABLE_OFFSET]
    mov r13d, [rbx + STATE_OFFSET + ST_REGION_COUNT]

    ; Preload constants
    movsd xmm4, [rel obs_weight_decay]   ; 0.995
    movsd xmm5, [rel obs_weight_floor]   ; 0.01
    movsd xmm6, [rel obs_f64_zero]       ; 0.0

    xor ecx, ecx
.wdecay_loop:
    cmp ecx, r13d
    jge .wdecay_done
    push rcx

    imul rdi, rcx, RTE_SIZE
    add rdi, r12
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_CONDEMNED
    jnz .wdecay_next

    mov rsi, [rdi + RTE_ADDR]

    ; --- w_excite_a ---
    movsd xmm0, [rsi + RHDR_W_EXCITE_A]
    mulsd xmm0, xmm4
    ucomisd xmm0, xmm5
    ja .wdecay_ea_ok
    ; Below floor — clear connection
    movsd [rsi + RHDR_W_EXCITE_A], xmm6
    mov qword [rsi + RHDR_EXCITE_A], 0
    jmp .wdecay_eb
.wdecay_ea_ok:
    movsd [rsi + RHDR_W_EXCITE_A], xmm0

.wdecay_eb:
    ; --- w_excite_b ---
    movsd xmm0, [rsi + RHDR_W_EXCITE_B]
    mulsd xmm0, xmm4
    ucomisd xmm0, xmm5
    ja .wdecay_eb_ok
    movsd [rsi + RHDR_W_EXCITE_B], xmm6
    mov qword [rsi + RHDR_EXCITE_B], 0
    jmp .wdecay_ia
.wdecay_eb_ok:
    movsd [rsi + RHDR_W_EXCITE_B], xmm0

.wdecay_ia:
    ; --- w_inhibit_a ---
    movsd xmm0, [rsi + RHDR_W_INHIBIT_A]
    mulsd xmm0, xmm4
    ucomisd xmm0, xmm5
    ja .wdecay_ia_ok
    movsd [rsi + RHDR_W_INHIBIT_A], xmm6
    mov qword [rsi + RHDR_INHIBIT_A], 0
    jmp .wdecay_ib
.wdecay_ia_ok:
    movsd [rsi + RHDR_W_INHIBIT_A], xmm0

.wdecay_ib:
    ; --- w_inhibit_b ---
    movsd xmm0, [rsi + RHDR_W_INHIBIT_B]
    mulsd xmm0, xmm4
    ucomisd xmm0, xmm5
    ja .wdecay_ib_ok
    movsd [rsi + RHDR_W_INHIBIT_B], xmm6
    mov qword [rsi + RHDR_INHIBIT_B], 0
    jmp .wdecay_next
.wdecay_ib_ok:
    movsd [rsi + RHDR_W_INHIBIT_B], xmm0

.wdecay_next:
    pop rcx
    inc ecx
    jmp .wdecay_loop

.wdecay_done:
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; repair_routing
;; Find regions with no next links but neighbors exist.
;; Wire isolated nodes into the graph by scanning for regions
;; with similar context (masked comparison).
;; ============================================================
global repair_routing
repair_routing:
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov rbx, SURFACE_BASE
    lea r12, [rbx + REGION_TABLE_OFFSET]
    mov r13d, [rbx + STATE_OFFSET + ST_REGION_COUNT]

    xor ecx, ecx
.repair_loop:
    cmp ecx, r13d
    jge .repair_done
    push rcx

    imul rdi, rcx, RTE_SIZE
    add rdi, r12
    movzx eax, word [rdi + RTE_TYPE]
    cmp eax, RTYPE_DISPATCH
    jne .repair_next
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_CONDEMNED
    jnz .repair_next

    mov r14, [rdi + RTE_ADDR]  ; region header

    ; Check if isolated (next_a == 0 AND next_b == 0)
    cmp qword [r14 + RHDR_NEXT_A], 0
    jne .repair_next
    cmp qword [r14 + RHDR_NEXT_B], 0
    jne .repair_next

    ; This region has no routing — try to find a neighbor
    ; Get this region's context (if valid dispatch pattern)
    cmp byte [r14 + RHDR_SIZE], 0x3D
    jne .repair_next
    mov r15d, [r14 + RHDR_SIZE + 1]  ; this region's context

    ; Scan for another DISPATCH region with similar context
    pop rcx
    push rcx
    xor edx, edx
.repair_inner:
    cmp edx, r13d
    jge .repair_next
    cmp edx, [rsp]            ; skip self (ecx is at [rsp])
    je .repair_inner_next

    push rdx
    imul rdi, rdx, RTE_SIZE
    add rdi, r12
    movzx eax, word [rdi + RTE_TYPE]
    cmp eax, RTYPE_DISPATCH
    jne .repair_inner_skip
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_CONDEMNED
    jnz .repair_inner_skip

    mov rsi, [rdi + RTE_ADDR]
    cmp rsi, r14
    je .repair_inner_skip      ; same region

    ; Check context similarity (same masked context)
    cmp byte [rsi + RHDR_SIZE], 0x3D
    jne .repair_inner_skip
    mov eax, [rsi + RHDR_SIZE + 1]
    ; Masked comparison: (ctx_a & 0xFFFFFFF0) == (ctx_b & 0xFFFFFFF0)
    mov edi, r15d
    and edi, 0xFFFFFFF0
    and eax, 0xFFFFFFF0
    cmp eax, edi
    jne .repair_inner_skip

    ; Found a neighbor! Wire isolated region → neighbor
    mov [r14 + RHDR_NEXT_A], rsi
    pop rdx
    jmp .repair_next

.repair_inner_skip:
    pop rdx
.repair_inner_next:
    inc edx
    jmp .repair_inner

.repair_next:
    pop rcx
    inc ecx
    jmp .repair_loop

.repair_done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

section .rodata
    pres_hdr:       db "--- Presence (30 fields) ---", 10, 0
    pres_sep:       db ": ", 0

extern print_str
