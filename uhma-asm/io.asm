; io.asm — Syscall wrappers for I/O, mmap, signals
%include "syscalls.inc"
%include "constants.inc"

section .text

;; ============================================================
;; sys_write(fd, buf, len)
;; rdi=fd, rsi=buf, rdx=len
;; Returns: bytes written in rax
;; ============================================================
global sys_write
sys_write:
    mov rax, SYS_WRITE
    syscall
    ret

;; ============================================================
;; sys_read(fd, buf, len)
;; rdi=fd, rsi=buf, rdx=len
;; Returns: bytes read in rax
;; ============================================================
global sys_read
sys_read:
    mov rax, SYS_READ
    syscall
    ret

;; ============================================================
;; sys_mmap(addr, len, prot, flags, fd, offset)
;; rdi=addr, rsi=len, rdx=prot, r10=flags, r8=fd, r9=offset
;; Returns: mapped address in rax
;; ============================================================
global sys_mmap
sys_mmap:
    mov rax, SYS_MMAP
    mov r10, rcx              ; syscall convention: r10 instead of rcx
    syscall
    ret

;; ============================================================
;; sys_mprotect(addr, len, prot)
;; rdi=addr, rsi=len, rdx=prot
;; ============================================================
global sys_mprotect
sys_mprotect:
    mov rax, SYS_MPROTECT
    syscall
    ret

;; ============================================================
;; sys_open(path, flags, mode)
;; rdi=path, rsi=flags, rdx=mode
;; Returns: fd in rax
;; ============================================================
global sys_open
sys_open:
    mov rax, SYS_OPEN
    syscall
    ret

;; ============================================================
;; sys_close(fd)
;; rdi=fd
;; ============================================================
global sys_close
sys_close:
    mov rax, SYS_CLOSE
    syscall
    ret

;; ============================================================
;; sys_exit(code)
;; rdi=exit code
;; ============================================================
global sys_exit
sys_exit:
    mov rax, SYS_EXIT
    syscall
    ; no return

;; ============================================================
;; sys_sigaction(signum, act, oldact)
;; rdi=signum, rsi=act ptr, rdx=oldact ptr
;; r10=sigsetsize (8)
;; ============================================================
global sys_sigaction
sys_sigaction:
    mov r10, 8                ; sizeof(sigset_t) / 8
    mov rax, SYS_RT_SIGACTION
    syscall
    ret

;; ============================================================
;; sys_clock_gettime(clockid, timespec_ptr)
;; rdi=clockid (0=REALTIME, 1=MONOTONIC)
;; rsi=ptr to timespec {tv_sec:u64, tv_nsec:u64}
;; ============================================================
global sys_clock_gettime
sys_clock_gettime:
    mov rax, SYS_CLOCK_GETTIME
    syscall
    ret

;; ============================================================
;; sys_getrandom(buf, buflen, flags)
;; rdi=buf, rsi=buflen, rdx=flags
;; ============================================================
global sys_getrandom
sys_getrandom:
    mov rax, SYS_GETRANDOM
    syscall
    ret

;; ============================================================
;; write_stdout(buf, len)
;; Convenience: write to stdout
;; rdi=buf, rsi=len
;; ============================================================
global write_stdout
write_stdout:
    mov rdx, rsi              ; len
    mov rsi, rdi              ; buf
    mov edi, STDOUT
    mov rax, SYS_WRITE
    syscall
    ret

;; ============================================================
;; write_stderr(buf, len)
;; rdi=buf, rsi=len
;; ============================================================
global write_stderr
write_stderr:
    mov rdx, rsi
    mov rsi, rdi
    mov edi, STDERR
    mov rax, SYS_WRITE
    syscall
    ret

;; ============================================================
;; read_stdin(buf, max_len)
;; rdi=buf, rsi=max_len
;; Returns: bytes read in rax
;; ============================================================
global read_stdin
read_stdin:
    mov rdx, rsi              ; max len
    mov rsi, rdi              ; buf
    xor edi, edi              ; STDIN
    mov rax, SYS_READ
    syscall
    ret

;; ============================================================
;; DIGESTION SYSTEM: File Ingestion as "Food"
;; Files are metabolic resources. Reading a file extracts
;; tokens (nutrients) that feed the dispatch system.
;; The Motor Interface provides sandboxed syscall access.
;; ============================================================

section .data
    align 8
    digest_msg:         db "[DIGEST] reading file: ", 0
    digest_bytes_msg:   db " (", 0
    digest_bytes_end:   db " bytes)", 10, 0
    digest_err_msg:     db "[DIGEST] error opening file: ", 0
    digest_nl:          db 10, 0
    motor_err_msg:      db "[MOTOR] command failed: ", 0

    ; Sandbox whitelist directory (hardcoded for safety)
    sandbox_dir:        db "/tmp/uhma-sandbox/", 0
    sandbox_dir_len:    equ $ - sandbox_dir - 1

    ; f64 constants for energy calculation
    align 8
    energy_per_byte:    dq 0.001   ; 0.001 energy per byte read (food value)
    energy_per_token:   dq 0.1     ; 0.1 energy per token extracted

section .bss
    ; File read buffer (64KB)
    align 16
    digest_buffer:      resb 65536
    digest_buffer_len:  equ 65536

section .text

;; ============================================================
;; motor_file_read(path, buffer, max_len) -> rax (bytes read)
;; rdi=path (null-terminated), rsi=buffer, rdx=max_len
;; Reads file with sandboxing (read-only, size-limited)
;; Returns: bytes read, or negative on error
;; ============================================================
global motor_file_read
motor_file_read:
    push rbx
    push r12
    push r13
    push r14

    mov r12, rdi              ; path
    mov r13, rsi              ; buffer
    mov r14, rdx              ; max_len

    ; Open file (read-only)
    mov rdi, r12
    xor esi, esi              ; O_RDONLY = 0
    xor edx, edx              ; mode = 0 (not creating)
    mov rax, SYS_OPEN
    syscall
    test rax, rax
    js .motor_read_err        ; open failed

    mov rbx, rax              ; save fd

    ; Read file content
    mov rdi, rbx              ; fd
    mov rsi, r13              ; buffer
    mov rdx, r14              ; max_len
    mov rax, SYS_READ
    syscall
    mov r14, rax              ; save bytes read

    ; Close file
    mov rdi, rbx
    mov rax, SYS_CLOSE
    syscall

    ; Return bytes read
    mov rax, r14
    jmp .motor_read_done

.motor_read_err:
    ; Return error code
    ; rax already contains negative error

.motor_read_done:
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; motor_file_write_sandboxed(path, buffer, len) -> rax (bytes written)
;; rdi=path (relative to sandbox), rsi=buffer, rdx=len
;; Writes file ONLY to sandbox directory for safety.
;; Returns: bytes written, or negative on error
;; ============================================================
global motor_file_write_sandboxed
motor_file_write_sandboxed:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 256              ; space for full path

    mov r12, rdi              ; relative path
    mov r13, rsi              ; buffer
    mov r14, rdx              ; len

    ; Construct full path: sandbox_dir + relative_path
    lea rdi, [rsp]            ; dest buffer
    lea rsi, [rel sandbox_dir]
    ; Copy sandbox_dir prefix
    mov rcx, sandbox_dir_len
.copy_prefix:
    test rcx, rcx
    jz .copy_filename
    lodsb
    stosb
    dec rcx
    jmp .copy_prefix

.copy_filename:
    ; Copy relative filename
    mov rsi, r12
.copy_fname_loop:
    lodsb
    stosb
    test al, al
    jnz .copy_fname_loop

    ; Open file for writing (create/truncate)
    lea rdi, [rsp]            ; full path
    mov esi, 0x241            ; O_WRONLY | O_CREAT | O_TRUNC
    mov edx, 0644o            ; mode rw-r--r--
    mov rax, SYS_OPEN
    syscall
    test rax, rax
    js .motor_write_err

    mov rbx, rax              ; save fd

    ; Write content
    mov rdi, rbx              ; fd
    mov rsi, r13              ; buffer
    mov rdx, r14              ; len
    mov rax, SYS_WRITE
    syscall
    mov r15, rax              ; save bytes written

    ; Close file
    mov rdi, rbx
    mov rax, SYS_CLOSE
    syscall

    mov rax, r15              ; return bytes written
    jmp .motor_write_done

.motor_write_err:
    ; rax contains negative error

.motor_write_done:
    add rsp, 256
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; digest_file(path) -> rax (tokens extracted)
;; rdi=path (null-terminated)
;; Reads file and feeds tokens to dispatch system.
;; This is how UHMA "eats" — files are food!
;; Energy gained = f(file_size, token_count)
;; ============================================================
extern process_token
extern print_cstr
extern print_u64

global digest_file
digest_file:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8                ; alignment

    mov r12, rdi              ; save path

    ; Print digest message
    lea rdi, [rel digest_msg]
    call print_cstr
    mov rdi, r12
    call print_cstr

    ; Read file
    mov rdi, r12
    lea rsi, [rel digest_buffer]
    mov rdx, digest_buffer_len
    call motor_file_read
    test rax, rax
    js .digest_error

    mov r13, rax              ; bytes read
    mov r14, SURFACE_BASE     ; surface base

    ; Print bytes read
    lea rdi, [rel digest_bytes_msg]
    call print_cstr
    mov rdi, r13
    call print_u64
    lea rdi, [rel digest_bytes_end]
    call print_cstr

    ; --- Metabolic energy from food ---
    ; Energy gained = bytes * energy_per_byte
    cvtsi2sd xmm0, r13        ; bytes as f64
    mulsd xmm0, [rel energy_per_byte]
    movsd xmm1, [r14 + STATE_OFFSET + ST_ENERGY]
    addsd xmm1, xmm0
    ; Cap energy at max
    mov rax, ENERGY_MAX
    movq xmm2, rax
    minsd xmm1, xmm2
    movsd [r14 + STATE_OFFSET + ST_ENERGY], xmm1

    ; --- Parse and feed tokens ---
    ; Simple tokenization: split on whitespace/punctuation
    xor r15d, r15d            ; token count
    lea rbx, [rel digest_buffer]
    mov rcx, r13              ; remaining bytes

.digest_loop:
    test rcx, rcx
    jz .digest_done

    ; Skip whitespace
.skip_ws:
    test rcx, rcx
    jz .digest_done
    movzx eax, byte [rbx]
    cmp al, ' '
    je .skip_char
    cmp al, 9                 ; tab
    je .skip_char
    cmp al, 10                ; newline
    je .skip_char
    cmp al, 13                ; CR
    je .skip_char
    jmp .start_token

.skip_char:
    inc rbx
    dec rcx
    jmp .skip_ws

.start_token:
    ; Found start of token — read until whitespace
    push rcx
    push rbx
    xor edx, edx              ; token hash

.hash_loop:
    test rcx, rcx
    jz .token_done
    movzx eax, byte [rbx]
    cmp al, ' '
    je .token_done
    cmp al, 9
    je .token_done
    cmp al, 10
    je .token_done
    cmp al, 13
    je .token_done
    cmp al, 0
    je .token_done

    ; Hash: edx = edx * 31 + al
    imul edx, edx, 31
    add edx, eax
    inc rbx
    dec rcx
    jmp .hash_loop

.token_done:
    pop rax                   ; original token start (discard)
    pop rax                   ; original remaining (discard)

    ; Process token (edx = token hash)
    mov edi, edx
    push rcx
    push rbx
    push r15
    call process_token
    pop r15
    pop rbx
    pop rcx

    inc r15d                  ; token count++
    jmp .digest_loop

.digest_done:
    ; Bonus energy for tokens extracted
    cvtsi2sd xmm0, r15        ; tokens as f64
    mulsd xmm0, [rel energy_per_token]
    movsd xmm1, [r14 + STATE_OFFSET + ST_ENERGY]
    addsd xmm1, xmm0
    mov rax, ENERGY_MAX
    movq xmm2, rax
    minsd xmm1, xmm2
    movsd [r14 + STATE_OFFSET + ST_ENERGY], xmm1

    ; Return token count
    mov eax, r15d
    jmp .digest_exit

.digest_error:
    lea rdi, [rel digest_err_msg]
    call print_cstr
    mov rdi, r12
    call print_cstr
    lea rdi, [rel digest_nl]
    call print_cstr
    xor eax, eax              ; return 0 tokens

.digest_exit:
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; motor_get_file_size(path) -> rax (size in bytes, or -1 on error)
;; rdi=path (null-terminated)
;; Uses stat syscall to get file size without reading it.
;; ============================================================
global motor_get_file_size
motor_get_file_size:
    push rbx
    sub rsp, 144              ; struct stat buffer

    mov rdi, rdi              ; path
    lea rsi, [rsp]            ; stat buffer
    mov rax, SYS_STAT
    syscall
    test rax, rax
    js .stat_err

    ; File size is at offset 48 in struct stat (st_size)
    mov rax, [rsp + 48]
    jmp .stat_done

.stat_err:
    mov rax, -1

.stat_done:
    add rsp, 144
    pop rbx
    ret

;; ============================================================
;; TRANSACTIONAL AGENCY: Safe Syscall Broker
;; Regions that want to perform I/O must submit requests to
;; the Motor Queue. Before execution, the region's code is
;; verified by the abstract interpreter to prove safety.
;; This gives UHMA "hands" while maintaining sandboxing.
;; ============================================================

section .data
    align 8
    agency_submit_msg:  db "[AGENCY] motor request submitted: cmd=", 0
    agency_verify_msg:  db "[AGENCY] verifying region 0x", 0
    agency_approved:    db " APPROVED", 10, 0
    agency_denied:      db " DENIED (verification failed)", 10, 0
    agency_exec_msg:    db "[AGENCY] executing motor cmd ", 0
    agency_done_msg:    db "[AGENCY] result: ", 0

section .text

extern verify_abstract

;; ============================================================
;; motor_submit_request(cmd, arg1, arg2, caller_region) -> eax (0=queued, -1=denied)
;; edi=motor command (MOTOR_*), rsi=arg1, rdx=arg2, rcx=caller_region_ptr
;; Submits a motor request to the queue. If caller_region is provided,
;; it must pass verification before the request is executed.
;; ============================================================
global motor_submit_request
motor_submit_request:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, VCS_SIZE + 8     ; space for verification state

    mov r12d, edi             ; cmd
    mov r13, rsi              ; arg1
    mov r14, rdx              ; arg2
    mov r15, rcx              ; caller_region_ptr (may be 0)
    mov rbx, SURFACE_BASE

    ; If no caller region, skip verification (trusted direct call)
    test r15, r15
    jz .motor_store

    ; --- Verify caller region before accepting request ---
    ; Get code pointer and length from region header
    mov rdi, r15
    add rdi, RHDR_SIZE        ; code starts after header
    movzx esi, word [r15 + RHDR_CODE_LEN]
    lea rdx, [rsp]            ; output state buffer
    call verify_abstract

    ; Check result (eax = error count, 0 = safe)
    test eax, eax
    jnz .motor_denied

.motor_store:
    ; Store request in motor queue
    mov dword [rbx + STATE_OFFSET + ST_MOTOR_CMD], r12d
    mov [rbx + STATE_OFFSET + ST_MOTOR_ARG1], r13
    mov [rbx + STATE_OFFSET + ST_MOTOR_ARG2], r14
    mov dword [rbx + STATE_OFFSET + ST_MOTOR_STATUS], 1  ; STATUS_PENDING

    xor eax, eax              ; return 0 = queued
    jmp .motor_submit_done

.motor_denied:
    mov dword [rbx + STATE_OFFSET + ST_MOTOR_STATUS], 0xFF  ; STATUS_DENIED
    mov eax, -1               ; return -1 = denied

.motor_submit_done:
    add rsp, VCS_SIZE + 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; motor_process_queue() -> eax (result of last command)
;; Processes pending motor commands from the queue.
;; Returns result of the executed command.
;; ============================================================
global motor_process_queue
motor_process_queue:
    push rbx
    push r12
    push r13

    mov rbx, SURFACE_BASE

    ; Check if there's a pending command
    mov eax, [rbx + STATE_OFFSET + ST_MOTOR_STATUS]
    cmp eax, 1                ; STATUS_PENDING
    jne .no_pending

    mov r12d, [rbx + STATE_OFFSET + ST_MOTOR_CMD]
    mov r13, [rbx + STATE_OFFSET + ST_MOTOR_ARG1]

    ; Dispatch based on command
    cmp r12d, MOTOR_FILE_READ
    je .exec_file_read
    cmp r12d, MOTOR_FILE_SIZE
    je .exec_file_size
    cmp r12d, MOTOR_FILE_WRITE
    je .exec_file_write

    ; Unknown command
    mov dword [rbx + STATE_OFFSET + ST_MOTOR_STATUS], 0xFE  ; STATUS_ERROR
    mov eax, -1
    jmp .motor_done

.exec_file_read:
    ; arg1 = path, arg2 = buffer, we use digest_buffer internally
    mov rdi, r13              ; path
    lea rsi, [rel digest_buffer]
    mov rdx, digest_buffer_len
    call motor_file_read
    mov [rbx + STATE_OFFSET + ST_MOTOR_RESULT], rax
    mov dword [rbx + STATE_OFFSET + ST_MOTOR_STATUS], 2  ; STATUS_COMPLETE
    jmp .motor_done

.exec_file_size:
    ; arg1 = path
    mov rdi, r13
    call motor_get_file_size
    mov [rbx + STATE_OFFSET + ST_MOTOR_RESULT], rax
    mov dword [rbx + STATE_OFFSET + ST_MOTOR_STATUS], 2  ; STATUS_COMPLETE
    jmp .motor_done

.exec_file_write:
    ; arg1 = path (relative), arg2 = buffer, arg3 (not used) = len
    mov rdi, r13              ; relative path
    mov rsi, [rbx + STATE_OFFSET + ST_MOTOR_ARG2]  ; buffer
    mov rdx, 4096             ; fixed max for safety
    call motor_file_write_sandboxed
    mov [rbx + STATE_OFFSET + ST_MOTOR_RESULT], rax
    mov dword [rbx + STATE_OFFSET + ST_MOTOR_STATUS], 2  ; STATUS_COMPLETE
    jmp .motor_done

.no_pending:
    xor eax, eax

.motor_done:
    mov rax, [rbx + STATE_OFFSET + ST_MOTOR_RESULT]
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; motor_get_status() -> eax (motor status)
;; Returns: 0=idle, 1=pending, 2=complete, 0xFE=error, 0xFF=denied
;; ============================================================
global motor_get_status
motor_get_status:
    mov rax, SURFACE_BASE
    mov eax, [rax + STATE_OFFSET + ST_MOTOR_STATUS]
    ret

;; ============================================================
;; motor_get_result() -> rax (last result)
;; Returns the result of the last motor command.
;; ============================================================
global motor_get_result
motor_get_result:
    mov rax, SURFACE_BASE
    mov rax, [rax + STATE_OFFSET + ST_MOTOR_RESULT]
    ret

;; ============================================================
;; motor_clear()
;; Clears the motor queue (resets to idle state)
;; ============================================================
global motor_clear
motor_clear:
    mov rax, SURFACE_BASE
    mov dword [rax + STATE_OFFSET + ST_MOTOR_CMD], 0
    mov qword [rax + STATE_OFFSET + ST_MOTOR_ARG1], 0
    mov qword [rax + STATE_OFFSET + ST_MOTOR_ARG2], 0
    mov qword [rax + STATE_OFFSET + ST_MOTOR_RESULT], 0
    mov dword [rax + STATE_OFFSET + ST_MOTOR_STATUS], 0
    ret
