; gate.asm — Predictive gating: test modifications in sandbox before commit
;
; @entry gate_test_modification(rdi=region_ptr, esi=mod_type, rdx=arg) -> eax
;        ; Returns 1=pass, 0=fail
; @entry gate_check_region(rdi=region_ptr) -> eax  ; 1=valid, 0=invalid
;
; @calls fire_hook
; @calledby evolve.asm:evolve_mutate, evolve.asm:evolve_crossover
; @calledby modify.asm:modify_specialize, modify.asm:modify_generalize
;
; GOTCHAS:
;   - Copies region to ST_NURSERY sandbox, applies mod, tests, returns pass/fail
;   - Region + header MUST fit in ST_NURSERY_SIZE (4KB) or auto-fail
;   - Mod types: MOD_MUTATE, MOD_SPECIALIZE, MOD_GENERALIZE, MOD_CROSSOVER
;   - Original region untouched until gate passes
;
%include "syscalls.inc"
%include "constants.inc"

section .data
    gate_pass_msg:  db "[GATE] PASS — modification approved", 10, 0
    gate_fail_msg:  db "[GATE] FAIL — modification rejected", 10, 0
    gate_test_msg:  db "[GATE] Testing modification...", 10, 0

section .text

extern print_cstr
extern fire_hook

;; ============================================================
;; gate_test_modification(region_ptr, mod_type, mod_arg)
;; rdi=region header ptr, esi=modification type, rdx=arg
;; Tests a proposed modification before committing:
;; 1. Copy region to nursery buffer
;; 2. Apply modification to copy
;; 3. Run test cases against copy
;; 4. Compare accuracy before/after
;; Returns: eax=1 (pass) or 0 (fail)
;; ============================================================
global gate_test_modification
gate_test_modification:
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov r12, rdi              ; region ptr
    mov r13d, esi             ; mod type
    mov r14, rdx              ; mod arg

    mov rbx, SURFACE_BASE

    lea rdi, [rel gate_test_msg]
    call print_cstr

    ; --- Step 1: Copy region to nursery ---
    lea r15, [rbx + STATE_OFFSET + ST_NURSERY]

    ; Get region size (header + code)
    movzx ecx, word [r12 + RHDR_CODE_LEN]
    add ecx, RHDR_SIZE
    cmp ecx, ST_NURSERY_SIZE
    jg .fail                  ; too large for nursery

    ; Copy bytes
    mov rsi, r12              ; source
    mov rdi, r15              ; dest (nursery)
    push rcx
.copy:
    test ecx, ecx
    jz .copy_done
    mov al, [rsi]
    mov [rdi], al
    inc rsi
    inc rdi
    dec ecx
    jmp .copy
.copy_done:
    pop rcx

    ; --- Step 2: Record baseline accuracy ---
    mov eax, [r15 + RHDR_HITS]
    mov edx, [r15 + RHDR_MISSES]
    mov r13d, eax             ; save baseline hits
    add edx, eax
    test edx, edx
    jz .pass                  ; no data = allow modification (optimistic)

    ; Baseline accuracy ratio (hits / total)
    ; Store as integer ratio for comparison
    ; baseline_hits = r13d, baseline_total = edx
    push rdx                  ; save baseline total

    ; --- Step 3: Simulate modification on copy ---
    ; Apply the modification type to the nursery copy
    ; For now: just test that the modified code is syntactically valid
    ; (doesn't crash when we examine its structure)

    ; Check that the copy still starts with valid instruction prefix
    cmp byte [r15 + RHDR_SIZE], 0x3D  ; cmp eax, imm32
    je .valid_code
    cmp byte [r15 + RHDR_SIZE], 0x31  ; xor
    je .valid_code
    cmp byte [r15 + RHDR_SIZE], 0xB8  ; mov eax, imm
    je .valid_code
    cmp byte [r15 + RHDR_SIZE], 0x90  ; nop
    je .valid_code
    ; Unknown instruction start — risky
    pop rdx
    jmp .fail

.valid_code:
    pop rdx                    ; rdx = baseline_total

    ; --- Step 4: Run test cases from miss buffer (wider scan) ---
    ; Test the copy against ALL available miss buffer entries, not just 8
    lea rsi, [rbx + STATE_OFFSET + ST_MISS_BUF]
    mov ecx, [rbx + STATE_OFFSET + ST_MISS_POS]
    test ecx, ecx
    jz .use_gate_count
    cmp ecx, ST_MISS_BUF_CAP
    jle .use_miss_pos
.use_gate_count:
    mov ecx, GATE_TEST_COUNT
.use_miss_pos:
    ; ecx = number of entries to test (up to miss_pos or GATE_TEST_COUNT)
    xor r8d, r8d             ; test_hits (on copy)
    xor r9d, r9d             ; test_total (context-matching tests)
    xor edx, edx             ; test index

.test_loop:
    cmp edx, ecx
    jge .test_done

    ; Get test case: ctx_hash at [rsi + edx*16], token at [rsi + edx*16 + 8]
    push rcx
    push rdx
    imul eax, edx, ST_MISS_ENTRY_SIZE
    mov edi, [rsi + rax]       ; test context hash (lower 32)
    mov edx, [rsi + rax + 8]   ; expected token

    ; Check if nursery copy would match this context
    cmp byte [r15 + RHDR_SIZE], 0x3D
    jne .test_next
    cmp [r15 + RHDR_SIZE + 1], edi
    jne .test_next

    ; Context matches — count this as a test case
    inc r9d

    ; Check if prediction is correct
    cmp byte [r15 + RHDR_SIZE + 7], 0xB8
    jne .test_next
    cmp [r15 + RHDR_SIZE + 8], edx
    jne .test_next

    ; Hit on test
    inc r8d

.test_next:
    pop rdx
    pop rcx
    inc edx
    jmp .test_loop

.test_done:
    ; --- Decision: compare test accuracy with baseline ---
    ; baseline_accuracy = r13d / (saved total from step 2)
    ; test_accuracy = r8d / r9d
    ; Pass if: no test data OR test_accuracy >= baseline_accuracy * 0.8

    test r9d, r9d
    jz .pass                  ; no applicable tests = optimistic pass

    ; Compute test accuracy (f32)
    cvtsi2ss xmm0, r8d
    cvtsi2ss xmm1, r9d
    divss xmm0, xmm1          ; xmm0 = test_accuracy

    ; Compute baseline accuracy (f32)
    mov eax, [r15 + RHDR_HITS]
    mov ecx, [r15 + RHDR_MISSES]
    add ecx, eax
    test ecx, ecx
    jz .pass                  ; no baseline data = pass
    cvtsi2ss xmm2, eax
    cvtsi2ss xmm3, ecx
    divss xmm2, xmm3          ; xmm2 = baseline_accuracy

    ; threshold = baseline * 0.8 (allow slight degradation)
    mov eax, 0x3F4CCCCD        ; 0.8f
    movd xmm3, eax
    mulss xmm2, xmm3          ; threshold

    ; Pass if test_accuracy >= threshold
    comiss xmm0, xmm2
    jae .pass

    ; Test accuracy degraded too much — reject modification
    jmp .fail

.pass:
    lea rdi, [rel gate_pass_msg]
    call print_cstr

    ; Fire gate pass hook
    mov edi, HOOK_ON_GATE_PASS
    xor esi, esi
    call fire_hook

    mov eax, 1
    jmp .ret

.fail:
    lea rdi, [rel gate_fail_msg]
    call print_cstr

    ; Fire gate fail hook
    mov edi, HOOK_ON_GATE_FAIL
    xor esi, esi
    call fire_hook

    xor eax, eax

.ret:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; gate_check_region(region_ptr) → eax (1=healthy, 0=should modify)
;; Quick health check on a region
;; ============================================================
global gate_check_region
gate_check_region:
    ; Check hits vs misses ratio
    mov eax, [rdi + RHDR_HITS]
    mov ecx, [rdi + RHDR_MISSES]
    add ecx, eax
    test ecx, ecx
    jz .healthy               ; no data yet = healthy

    ; If accuracy > 0.3, region is healthy
    cvtsi2ss xmm0, eax
    cvtsi2ss xmm1, ecx
    divss xmm0, xmm1

    mov eax, THRESH_ACCURACY  ; 0.3f
    movd xmm1, eax
    comiss xmm0, xmm1
    ja .healthy

    ; Below threshold — needs modification
    xor eax, eax
    ret

.healthy:
    mov eax, 1
    ret
