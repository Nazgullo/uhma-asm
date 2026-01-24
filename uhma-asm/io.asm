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
