; boot.asm — _start, mmap surface, install handlers, begin REPL
%include "syscalls.inc"
%include "constants.inc"

section .text

global _start

extern surface_init
extern install_fault_handlers
extern repl_run
extern dispatch_init
extern vsa_init_random

;; ============================================================
;; _start — Entry point
;; 1. mmap the 8GB RWX surface
;; 2. Initialize state block
;; 3. Install signal handlers
;; 4. Initialize dispatch tree with echo behavior
;; 5. Initialize VSA arena (random base vectors)
;; 6. Enter REPL
;; ============================================================
_start:
    ; Align stack
    and rsp, ~15

    ; 1. Initialize the surface (mmap + state block setup)
    call surface_init
    ; rax = surface base (should be SURFACE_BASE)

    ; 2. Install fault handlers (SIGSEGV, SIGFPE, SIGBUS)
    call install_fault_handlers

    ; 3. Initialize the dispatch tree with a minimal echo region
    call dispatch_init

    ; 4. Initialize VSA arena with random vectors for initial tokens
    call vsa_init_random

    ; 5. Enter the interactive REPL (never returns)
    call repl_run

    ; Should not reach here
    mov edi, 0
    mov rax, SYS_EXIT
    syscall
