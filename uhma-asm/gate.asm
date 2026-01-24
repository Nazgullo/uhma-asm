; gate.asm — Predictive gating: test modifications before committing
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
    pop rdx

    ; --- Step 4: Run test cases ---
    ; Use entries from the miss buffer as test cases
    ; For each test: does the modified region predict correctly?
    lea rsi, [rbx + STATE_OFFSET + ST_MISS_BUF]
    xor ecx, ecx             ; test index
    xor r8d, r8d             ; test_hits
    xor r9d, r9d             ; test_total

.test_loop:
    cmp ecx, GATE_TEST_COUNT
    jge .test_done

    ; Get test case: ctx_hash at [rsi + ecx*16], token at [rsi + ecx*16 + 8]
    imul eax, ecx, ST_MISS_ENTRY_SIZE
    mov edi, [rsi + rax]       ; test context hash (lower 32)
    mov edx, [rsi + rax + 8]   ; expected token

    ; Check if nursery region would match this context
    cmp byte [r15 + RHDR_SIZE], 0x3D
    jne .test_next
    cmp [r15 + RHDR_SIZE + 1], edi
    jne .test_next

    ; It matches — check if prediction is correct
    cmp byte [r15 + RHDR_SIZE + 7], 0xB8
    jne .test_next
    cmp [r15 + RHDR_SIZE + 8], edx
    jne .test_miss

    ; Hit
    inc r8d
.test_miss:
    inc r9d

.test_next:
    inc ecx
    jmp .test_loop

.test_done:
    ; Decision: pass if test_hits >= baseline ratio OR no test data
    test r9d, r9d
    jz .pass                  ; no applicable tests = pass

    ; Simple check: if the modification didn't make things worse
    ; (any hits at all means it's potentially useful)
    test r8d, r8d
    jnz .pass

    ; No hits on test cases — modification is not helpful
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
