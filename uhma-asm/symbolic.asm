; symbolic.asm — Symbolic observation and minimal crash prevention
;
; Philosophy: OBSERVE, don't constrain. Only prevent guaranteed crashes.
; Let the system discover solutions we wouldn't think of.
;
; This module:
; 1. Records symbolic state of all modifications
; 2. Only blocks writes to unmapped/critical memory
; 3. Logs everything for post-hoc analysis of emergence
; 4. Tracks "anomalous" patterns that work despite looking wrong
;
%include "syscalls.inc"
%include "constants.inc"

section .data
    sym_log_hdr:    db "[SYM] ", 0
    sym_mod:        db "MOD @", 0
    sym_size:       db " size=", 0
    sym_allowed:    db " -> allowed", 10, 0
    sym_blocked:    db " -> BLOCKED (unmapped)", 10, 0
    sym_anomaly:    db "[SYM] ANOMALY: ", 0
    sym_discover:   db "[SYM] DISCOVERY logged #", 0
    sym_nl:         db 10, 0

section .bss
    ; Symbolic observation state
    sym_enabled:        resd 1
    sym_mod_count:      resq 1      ; Total modifications observed
    sym_blocked_count:  resq 1      ; Crashes prevented
    sym_anomaly_count:  resq 1      ; Unusual patterns that worked

    ; Modification log (circular buffer)
    ; Each entry: [addr:8][size:4][type:4][step:8][flags:8] = 32 bytes
    sym_log_pos:        resd 1
    sym_log_buf:        resb 32 * 256   ; 256 entries

    ; Discovery log - patterns that "shouldn't work" but do
    ; Each: [addr:8][pattern_hash:8][survival_steps:8][notes:8] = 32 bytes
    disc_log_pos:       resd 1
    disc_log_buf:       resb 32 * 64    ; 64 discoveries

section .text

extern print_cstr
extern print_hex64
extern print_u64
extern print_newline

;; ============================================================
;; sym_init — Initialize symbolic observation
;; ============================================================
global sym_init
sym_init:
    mov dword [rel sym_enabled], 1
    mov qword [rel sym_mod_count], 0
    mov qword [rel sym_blocked_count], 0
    mov qword [rel sym_anomaly_count], 0
    mov dword [rel sym_log_pos], 0
    mov dword [rel disc_log_pos], 0
    ret

;; ============================================================
;; sym_check_write — Check if write is safe (won't definitely crash)
;; rdi = target address
;; rsi = size
;; Returns: rax = 1 if safe, 0 if guaranteed crash
;;
;; MINIMAL intervention - only block:
;; - NULL/near-null writes
;; - Writes outside our surface
;; - Writes to our own code section
;; Everything else: LET IT THROUGH
;; ============================================================
global sym_check_write
sym_check_write:
    ; Check for null
    test rdi, rdi
    jz .block
    cmp rdi, 0x1000
    jb .block               ; First page - definitely crash

    ; Check if in our surface (0x100000000 - 0x300000000)
    mov rax, SURFACE_BASE
    cmp rdi, rax
    jb .check_text          ; Below surface, check if .text

    mov rax, SURFACE_BASE + SURFACE_SIZE  ; single 64-bit immediate (avoids sign-extension)
    cmp rdi, rax
    jb .allow               ; In surface - always allow

.check_text:
    ; Don't let it overwrite our static code (.text section)
    ; .text is roughly 0x401000 - 0x410000
    cmp rdi, 0x401000
    jb .allow               ; Below .text - probably stack, allow
    cmp rdi, 0x410000
    jae .allow              ; Above .text - allow

    ; Would overwrite our code - block this
    jmp .block

.allow:
    mov eax, 1
    ret

.block:
    xor eax, eax
    ret

;; ============================================================
;; sym_observe_mod — Record a modification (ALWAYS call this)
;; rdi = address
;; rsi = size
;; edx = modification type (0=emit, 1=mutate, 2=dream, 3=evolve)
;; rcx = current step
;; Returns: rax = 1 if allowed, 0 if blocked
;; ============================================================
global sym_observe_mod
sym_observe_mod:
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov r12, rdi            ; addr
    mov r13, rsi            ; size
    mov r14d, edx           ; type
    mov r15, rcx            ; step

    ; Check if enabled
    cmp dword [rel sym_enabled], 0
    je .allow_silent

    ; First: is this write safe?
    mov rdi, r12
    mov rsi, r13
    call sym_check_write
    test eax, eax
    jz .blocked

    ; Safe - log it
    inc qword [rel sym_mod_count]

    ; Add to circular log
    mov eax, [rel sym_log_pos]
    and eax, 255            ; mod 256
    imul ebx, eax, 32       ; entry offset
    lea rdi, [rel sym_log_buf]
    add rdi, rbx

    ; Write entry
    mov [rdi + 0], r12      ; addr
    mov [rdi + 8], r13d     ; size
    mov [rdi + 12], r14d    ; type
    mov [rdi + 16], r15     ; step
    mov qword [rdi + 24], 0 ; flags (reserved)

    inc dword [rel sym_log_pos]

    ; Print observation (verbose mode would check a flag)
    ; For now, silent logging

    mov eax, 1
    jmp .done

.blocked:
    inc qword [rel sym_blocked_count]

    ; Log the block
    lea rdi, [rel sym_log_hdr]
    call print_cstr
    lea rdi, [rel sym_mod]
    call print_cstr
    mov rdi, r12
    call print_hex64
    lea rdi, [rel sym_blocked]
    call print_cstr

    xor eax, eax
    jmp .done

.allow_silent:
    mov eax, 1

.done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; sym_record_anomaly — Record something unusual that worked
;; rdi = region address
;; rsi = description code (what made it anomalous)
;;   0 = unexpected jump target
;;   1 = unconventional register use
;;   2 = self-modifying pattern
;;   3 = impossible stack state that didn't crash
;;   4 = unknown opcode sequence that executed
;; ============================================================
global sym_record_anomaly
sym_record_anomaly:
    push rbx
    push r12
    push r13

    mov r12, rdi            ; region addr
    mov r13, rsi            ; anomaly type

    inc qword [rel sym_anomaly_count]

    ; Print notice
    lea rdi, [rel sym_anomaly]
    call print_cstr
    mov rdi, r13
    call print_u64
    lea rdi, [rel sym_size]
    call print_cstr
    mov rdi, r12
    call print_hex64
    call print_newline

    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; sym_record_discovery — Log a pattern that works unexpectedly
;; Called when a "weird" region survives and gets hits
;; rdi = region header
;; rsi = hash of pattern
;; edx = how many steps it survived
;; ============================================================
global sym_record_discovery
sym_record_discovery:
    push rbx
    push r12
    push r13
    push r14

    mov r12, rdi            ; header
    mov r13, rsi            ; pattern hash
    mov r14d, edx           ; survival steps

    ; Add to discovery log
    mov eax, [rel disc_log_pos]
    cmp eax, 64
    jge .log_full

    imul ebx, eax, 32
    lea rdi, [rel disc_log_buf]
    add rdi, rbx

    mov [rdi + 0], r12      ; addr
    mov [rdi + 8], r13      ; pattern hash
    mov [rdi + 16], r14     ; survival
    mov qword [rdi + 24], 0 ; notes

    inc dword [rel disc_log_pos]

    ; Print
    lea rdi, [rel sym_discover]
    call print_cstr
    mov edi, [rel disc_log_pos]
    call print_u64
    call print_newline

.log_full:
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; sym_get_stats — Get observation statistics
;; Returns in registers:
;;   rax = total modifications observed
;;   rbx = crashes prevented
;;   rcx = anomalies recorded
;; ============================================================
global sym_get_stats
sym_get_stats:
    mov rax, [rel sym_mod_count]
    mov rbx, [rel sym_blocked_count]
    mov rcx, [rel sym_anomaly_count]
    ret

;; ============================================================
;; sym_hash_pattern — Hash a code pattern for tracking
;; rdi = code address
;; rsi = code size
;; Returns: rax = 64-bit hash
;; ============================================================
global sym_hash_pattern
sym_hash_pattern:
    push rbx
    push r12

    mov r12, rsi            ; size
    mov rax, 0xcbf29ce484222325  ; FNV-1a init

.hash_loop:
    test r12, r12
    jz .done

    movzx ebx, byte [rdi]
    xor rax, rbx
    mov rbx, 0x100000001b3  ; FNV prime
    imul rax, rbx

    inc rdi
    dec r12
    jmp .hash_loop

.done:
    pop r12
    pop rbx
    ret

;; ============================================================
;; sym_analyze_region — Symbolically analyze a region's code
;; Records observations about what the code does
;; rdi = region header
;; Returns: rax = flags describing what was observed
;;   bit 0: has conditional jump
;;   bit 1: has unconditional jump
;;   bit 2: has call
;;   bit 3: has unusual opcode
;;   bit 4: has self-reference
;;   bit 5: stack manipulation
;; ============================================================
global sym_analyze_region
sym_analyze_region:
    push rbx
    push r12
    push r13
    push r14

    mov r12, rdi                    ; header
    lea r13, [rdi + RHDR_SIZE]      ; code start
    movzx r14d, word [rdi + RHDR_CODE_LEN]  ; code size
    xor ebx, ebx                    ; flags

.scan:
    test r14d, r14d
    jz .done

    movzx eax, byte [r13]

    ; Check for conditional jumps (0x70-0x7F, 0x0F 0x8x)
    cmp al, 0x70
    jb .not_cond
    cmp al, 0x7F
    ja .not_cond
    or ebx, 1
    jmp .next

.not_cond:
    ; Check for JMP (0xEB, 0xE9)
    cmp al, 0xEB
    je .found_jmp
    cmp al, 0xE9
    jne .not_jmp
.found_jmp:
    or ebx, 2
    jmp .next

.not_jmp:
    ; Check for CALL (0xE8, 0xFF /2)
    cmp al, 0xE8
    je .found_call
    cmp al, 0xFF
    jne .not_call
    cmp r14d, 2
    jl .not_call
    movzx ecx, byte [r13 + 1]
    and ecx, 0x38               ; ModRM reg field
    cmp ecx, 0x10               ; /2 = CALL
    jne .not_call
.found_call:
    or ebx, 4
    jmp .next

.not_call:
    ; Check for unusual opcodes (might be emergent behavior)
    ; 0x0F prefix = extended opcode (SSE, etc)
    cmp al, 0x0F
    jne .not_unusual
    or ebx, 8
    jmp .next

.not_unusual:
    ; Check for stack ops (push/pop/sub rsp/add rsp)
    cmp al, 0x50
    jb .not_stack
    cmp al, 0x5F
    jbe .found_stack
    jmp .not_stack
.found_stack:
    or ebx, 32
    jmp .next

.not_stack:

.next:
    inc r13
    dec r14d
    jmp .scan

.done:
    mov eax, ebx

    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; sym_is_anomalous — Check if a region exhibits anomalous behavior
;; rdi = region header
;; Returns: rax = anomaly score (0 = normal, higher = more unusual)
;; ============================================================
global sym_is_anomalous
sym_is_anomalous:
    push rbx
    push r12

    mov r12, rdi
    xor ebx, ebx            ; anomaly score

    ; Get analysis flags
    call sym_analyze_region
    mov ecx, eax

    ; Self-reference or unusual opcodes = interesting
    test ecx, 8             ; unusual opcode
    jz .no_unusual
    add ebx, 2
.no_unusual:

    test ecx, 16            ; self-reference
    jz .no_selfref
    add ebx, 5
.no_selfref:

    ; Check hit/miss ratio - high hits despite weird structure = discovery
    mov eax, [r12 + RHDR_HITS]
    cmp eax, 10
    jl .low_hits

    ; Has significant hits
    test ecx, 8             ; and unusual opcodes?
    jz .low_hits
    add ebx, 10             ; Potentially discovered something

.low_hits:
    mov eax, ebx

    pop r12
    pop rbx
    ret

;; ============================================================
;; sym_scan_for_discoveries — Scan all regions for emergent patterns
;; Called periodically to find things that work despite being "wrong"
;; ============================================================
global sym_scan_for_discoveries
sym_scan_for_discoveries:
    push rbx
    push r12
    push r13
    push r14

    mov rbx, SURFACE_BASE
    lea r12, [rbx + REGION_TABLE_OFFSET]
    lea rax, [rbx + STATE_OFFSET + ST_REGION_COUNT]
    mov r13d, [rax]

    xor r14d, r14d          ; index

.scan_loop:
    cmp r14d, r13d
    jge .done

    ; Get region entry
    imul rdi, r14, RTE_SIZE
    add rdi, r12

    ; Skip condemned
    movzx eax, word [rdi + RTE_FLAGS]
    test eax, RFLAG_CONDEMNED
    jnz .next

    ; Get header address
    mov rdi, [rdi + RTE_ADDR]
    test rdi, rdi
    jz .next

    ; Check if anomalous
    push rdi
    call sym_is_anomalous
    pop rdi

    cmp eax, 5              ; Anomaly threshold
    jl .next

    ; Found something interesting!
    push rdi
    mov rsi, rdi
    call sym_hash_pattern
    mov rsi, rax            ; pattern hash

    pop rdi
    mov edx, [rdi + RHDR_HITS]  ; survival metric
    call sym_record_discovery

.next:
    inc r14d
    jmp .scan_loop

.done:
    pop r14
    pop r13
    pop r12
    pop rbx
    ret
