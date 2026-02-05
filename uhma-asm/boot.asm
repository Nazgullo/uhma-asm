; boot.asm — System startup: surface, handlers, channels, REPL
;
; @entry _start -> ELF entry point, never returns
; @calls surface.asm:surface_init
; @calls signal.asm:install_fault_handlers
; @calls dispatch.asm:dispatch_init
; @calls vsa.asm:vsa_init_random
; @calls verify.asm:verify_init
; @calls maturity.asm:maturity_init
; @calls gateway.asm:gateway_init
; @calls repl.asm:repl_run
;
; STARTUP SEQUENCE:
;   1. surface_init()           - mmap 8GB sparse persistent file
;   2. install_fault_handlers() - SIGSEGV/SIGFPE/SIGBUS recovery
;   3. dispatch_init()          - init dispatch tree with echo behavior
;   4. vsa_init_random()        - seed random vectors for VSA arena
;   5. verify_init()            - assembly brittleness protection
;   6. maturity_init()          - developmental gating (Stage 0)
;   7. gateway_init()           - single-port TCP gateway (9999)
;   8. repl_run()               - main loop (stdin + gateway)
;
; NOTE: Stack aligned to 16 at entry. repl_run never returns.
;
%include "syscalls.inc"
%include "constants.inc"

section .text

global _start

extern surface_init
extern install_fault_handlers
extern repl_run
extern dispatch_init
extern vsa_init_random
extern verify_init
extern verify_vsa_math
extern maturity_init
extern gateway_init
extern init_action_registry

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

    ; 5. Initialize verification system (assembly brittleness protection)
    call verify_init

    ; 5b. Verify VSA binding mathematics at startup
    call verify_vsa_math

    ; 6. Initialize maturity/developmental gating
    call maturity_init

    ; 7. Initialize metabolic energy pool
    mov rbx, SURFACE_BASE
    mov rax, ENERGY_INITIAL
    mov [rbx + STATE_OFFSET + ST_ENERGY], rax
    ; Initialize tempo to 1.0 (normal rhythm)
    mov rax, 0x3FF0000000000000    ; 1.0 f64
    mov [rbx + STATE_OFFSET + ST_TEMPO_MULT], rax

    ; 8. Initialize action registry (autonomy loop gates + explore path)
    call init_action_registry

    ; 9. Initialize gateway (single-port TCP on 9999)
    call gateway_init

    ; 10. Enter the interactive REPL (never returns)
    call repl_run

    ; Should not reach here
    mov edi, 0
    mov rax, SYS_EXIT
    syscall
