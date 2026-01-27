; channels.asm — 6-channel paired TCP I/O for UHMA
;
; @entry channels_init() -> eax=1 success, 0 failure
; @entry channels_poll() -> eax=input channel (0,2,4), -1 stdin, -2 timeout
; @entry channels_read(edi=channel, rsi=buf, edx=len) -> eax=bytes read
; @entry channels_write(edi=channel, rsi=buf, edx=len) -> eax=bytes written
; @entry channels_respond(edi=input_ch, rsi=buf, edx=len) -> writes to input_ch+1
; @entry channels_accept() -> accepts pending connection on channel r12d
; @entry get_channel_fd(edi=channel) -> eax=socket fd or -1
; @entry channels_shutdown() -> closes all sockets
; @calls format.asm:print_cstr, print_u64
; @calls signal.asm:set_sigpipe_mode (enables TCP mode on init)
; @calledby boot.asm:_start, repl.asm:repl_run
;
; CHANNEL PAIRS (synchronous request/response):
;   FEED:  CH0:9999 (in) → CH1:9998 (out) - eat, dream, observe
;   QUERY: CH2:9997 (in) → CH3:9996 (out) - status, why, misses
;   DEBUG: CH4:9995 (in) → CH5:9994 (out) - trace, receipts
;
; PROTOCOL:
;   Client connects to input port, sends command, reads from output port
;   UHMA polls input channels, executes command, writes to paired output
;   Output channel = input channel + 1 (handled by channels_respond)
;
; GOTCHAS:
;   - channels_init() sets SIGPIPE to ignore mode (survives client disconnect)
;   - stdin is channel -1 for backwards compat (headless: stdin_active=0)
;   - get_channel_fd() returns socket fd, not channel number
;   - Poll skips dead stdin (fd=-1) and output channels (odd numbers)

%include "syscalls.inc"
%include "constants.inc"

section .data
    NUM_CHANNELS    equ 6
    BASE_PORT       equ 9999

    ; Channel indices
    CH_FEED_IN      equ 0             ; 9999 - feed input
    CH_FEED_OUT     equ 1             ; 9998 - feed output
    CH_QUERY_IN     equ 2             ; 9997 - query input
    CH_QUERY_OUT    equ 3             ; 9996 - query output
    CH_DEBUG_IN     equ 4             ; 9995 - debug input
    CH_DEBUG_OUT    equ 5             ; 9994 - debug output

    ch_init_msg:    db "[CHANNELS] 6-channel I/O ready", 10
                    db "  FEED:  9999->9998", 10
                    db "  QUERY: 9997->9996", 10
                    db "  DEBUG: 9995->9994", 10, 0
    ch_accept_msg:  db "[CH", 0
    ch_accept_end:  db "] connected", 10, 0
    ch_close_msg:   db "[CH", 0
    ch_close_end:   db "] disconnected", 10, 0

section .bss
    ; Listening sockets (one per channel)
    listen_fds:     resd 6

    ; Client sockets (-1 if no client)
    client_fds:     resd 6

    ; Poll structures: stdin + 6 listeners + 6 clients = 13
    ; struct pollfd { int fd; short events; short revents; } = 8 bytes
    poll_fds:       resb 8 * 13

    ; sockaddr_in for bind/accept
    sock_addr:      resb 16
    addr_len:       resd 1

    channels_ready: resd 1

section .text

extern print_cstr
extern print_u64
extern stdin_active               ; from repl.asm - 0 if stdin EOF
extern set_sigpipe_mode           ; from signal.asm - 0=exit, 1=ignore

global channels_init
global channels_poll
global channels_read
global channels_write
global channels_respond
global channels_shutdown
global channels_accept
global get_channel_fd

;; ============================================================
;; channels_init — Create 6 listening sockets
;; Returns: eax=1 success, 0 failure
;; ============================================================
channels_init:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 8                        ; align (4 pushes = even, +8)

    ; Init client fds to -1
    mov eax, -1
    mov [rel client_fds], eax
    mov [rel client_fds + 4], eax
    mov [rel client_fds + 8], eax
    mov [rel client_fds + 12], eax
    mov [rel client_fds + 16], eax
    mov [rel client_fds + 20], eax

    xor r12d, r12d                    ; channel index

.init_loop:
    cmp r12d, NUM_CHANNELS
    jge .init_done

    ; socket(AF_INET, SOCK_STREAM, 0)
    mov eax, SYS_SOCKET
    mov edi, AF_INET
    mov esi, SOCK_STREAM
    xor edx, edx
    syscall
    test eax, eax
    js .init_fail
    mov ebx, eax                      ; save fd

    lea rcx, [rel listen_fds]
    mov [rcx + r12 * 4], eax

    ; setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &1, 4)
    mov eax, SYS_SETSOCKOPT
    mov edi, ebx
    mov esi, SOL_SOCKET
    mov edx, SO_REUSEADDR
    lea r10, [rel addr_len]
    mov dword [r10], 1
    mov r8d, 4
    syscall

    ; Setup sockaddr_in
    lea rdi, [rel sock_addr]
    mov word [rdi], AF_INET           ; sin_family
    ; Port = BASE_PORT - channel
    mov eax, BASE_PORT
    sub eax, r12d
    xchg al, ah                       ; htons
    mov [rdi + 2], ax
    mov dword [rdi + 4], 0            ; INADDR_ANY

    ; bind
    mov eax, SYS_BIND
    mov edi, ebx
    lea rsi, [rel sock_addr]
    mov edx, 16
    syscall
    test eax, eax
    js .init_fail

    ; listen
    mov eax, SYS_LISTEN
    mov edi, ebx
    mov esi, 5
    syscall
    test eax, eax
    js .init_fail

    inc r12d
    jmp .init_loop

.init_done:
    mov dword [rel channels_ready], 1
    ; Enable SIGPIPE ignore mode for TCP operation
    mov edi, 1
    call set_sigpipe_mode
    lea rdi, [rel ch_init_msg]
    call print_cstr
    mov eax, 1
    jmp .init_ret

.init_fail:
    ; Close opened sockets
    xor r13d, r13d
.close_loop:
    cmp r13d, r12d
    jge .init_ret_fail
    lea rcx, [rel listen_fds]
    mov edi, [rcx + r13 * 4]
    mov eax, SYS_CLOSE
    syscall
    inc r13d
    jmp .close_loop

.init_ret_fail:
    xor eax, eax

.init_ret:
    add rsp, 8
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; channels_poll — Wait for data on input channels or stdin
;; Only polls INPUT channels (0,2,4) + stdin
;; Returns: eax = input channel (0,2,4), -1 stdin, -2 timeout
;; ============================================================
channels_poll:
    push rbx
    push r12
    push r13
    sub rsp, 8

    lea rbx, [rel poll_fds]

    ; Entry 0: stdin (use -1 if stdin is dead to skip it)
    mov eax, [rel stdin_active]
    test eax, eax
    jz .stdin_dead
    mov dword [rbx], 0                ; fd = stdin
    jmp .stdin_set
.stdin_dead:
    mov dword [rbx], -1               ; -1 = poll ignores this entry
.stdin_set:
    mov word [rbx + 4], POLLIN
    mov word [rbx + 6], 0

    ; Entries 1-6: listening sockets
    xor r12d, r12d
.add_listeners:
    cmp r12d, NUM_CHANNELS
    jge .add_clients

    lea rcx, [rel listen_fds]
    mov eax, [rcx + r12 * 4]
    lea rcx, [rbx + 8 + r12 * 8]
    mov [rcx], eax
    mov word [rcx + 4], POLLIN
    mov word [rcx + 6], 0

    inc r12d
    jmp .add_listeners

.add_clients:
    ; Entries 7-12: client sockets
    xor r12d, r12d
.add_client:
    cmp r12d, NUM_CHANNELS
    jge .do_poll

    lea rcx, [rel client_fds]
    mov eax, [rcx + r12 * 4]
    lea rcx, [rbx + 56 + r12 * 8]     ; offset 7*8 = 56
    mov [rcx], eax
    mov word [rcx + 4], POLLIN
    mov word [rcx + 6], 0

    inc r12d
    jmp .add_client

.do_poll:
    mov eax, SYS_POLL
    lea rdi, [rel poll_fds]
    mov esi, 13
    mov edx, 100                      ; 100ms timeout
    syscall
    test eax, eax
    jle .poll_timeout

    ; Check stdin
    lea rbx, [rel poll_fds]
    movzx eax, word [rbx + 6]
    test ax, POLLIN
    jnz .poll_stdin

    ; Check listeners for new connections
    xor r12d, r12d
.check_listeners:
    cmp r12d, NUM_CHANNELS
    jge .check_clients

    lea rcx, [rbx + 8 + r12 * 8]
    movzx eax, word [rcx + 6]
    test ax, POLLIN
    jnz .do_accept

    inc r12d
    jmp .check_listeners

.check_clients:
    ; Only check INPUT channels (0, 2, 4) for data
    xor r12d, r12d
.check_input_client:
    cmp r12d, NUM_CHANNELS
    jge .poll_timeout

    ; Skip output channels (1, 3, 5)
    test r12d, 1
    jnz .next_input

    lea rcx, [rbx + 56 + r12 * 8]
    movzx eax, word [rcx + 6]
    test ax, POLLIN
    jnz .got_input

.next_input:
    inc r12d
    jmp .check_input_client

.got_input:
    mov eax, r12d                     ; return input channel
    jmp .poll_ret

.poll_stdin:
    mov eax, -1
    jmp .poll_ret

.do_accept:
    mov r13d, r12d
    call channels_accept
    jmp .do_poll                      ; re-poll (stay in same stack frame)

.poll_timeout:
    mov eax, -2

.poll_ret:
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; channels_accept — Accept connection on channel r12d
;; ============================================================
channels_accept:
    push rbx
    sub rsp, 16

    lea rcx, [rel listen_fds]
    mov edi, [rcx + r12 * 4]

    mov dword [rel addr_len], 16
    mov eax, SYS_ACCEPT
    lea rsi, [rel sock_addr]
    lea rdx, [rel addr_len]
    syscall
    test eax, eax
    js .accept_done

    ; Close old client if exists
    lea rcx, [rel client_fds]
    mov ebx, [rcx + r12 * 4]
    cmp ebx, -1
    je .store_new
    push rax
    mov edi, ebx
    mov eax, SYS_CLOSE
    syscall
    pop rax

.store_new:
    lea rcx, [rel client_fds]
    mov [rcx + r12 * 4], eax

    ; Print [CHn] connected
    lea rdi, [rel ch_accept_msg]
    call print_cstr
    mov edi, r12d
    call print_u64
    lea rdi, [rel ch_accept_end]
    call print_cstr

.accept_done:
    add rsp, 16
    pop rbx
    ret

;; ============================================================
;; channels_read — Read from channel's client socket
;; edi=channel, rsi=buf, edx=len
;; Returns: eax=bytes, 0 on disconnect
;; ============================================================
channels_read:
    push rbx
    push r12
    sub rsp, 8

    mov r12d, edi
    mov rbx, rsi

    cmp r12d, NUM_CHANNELS
    jge .read_err

    lea rcx, [rel client_fds]
    mov edi, [rcx + r12 * 4]
    cmp edi, -1
    je .read_err

    mov eax, SYS_READ
    mov rsi, rbx
    syscall

    test eax, eax
    jle .read_disconnect
    jmp .read_ret

.read_disconnect:
    lea rcx, [rel client_fds]
    mov edi, [rcx + r12 * 4]
    push rax
    mov eax, SYS_CLOSE
    syscall
    pop rax

    lea rcx, [rel client_fds]
    mov dword [rcx + r12 * 4], -1

    lea rdi, [rel ch_close_msg]
    call print_cstr
    mov edi, r12d
    call print_u64
    lea rdi, [rel ch_close_end]
    call print_cstr

    xor eax, eax
    jmp .read_ret

.read_err:
    xor eax, eax

.read_ret:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; channels_write — Write to channel's client socket
;; edi=channel, rsi=buf, edx=len
;; Returns: eax=bytes written
;; ============================================================
channels_write:
    push rbx
    push r12
    sub rsp, 8

    mov r12d, edi

    cmp r12d, NUM_CHANNELS
    jge .write_err

    lea rcx, [rel client_fds]
    mov edi, [rcx + r12 * 4]
    cmp edi, -1
    je .write_err

    mov eax, SYS_WRITE
    syscall
    jmp .write_ret

.write_err:
    xor eax, eax

.write_ret:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; get_channel_fd — Get socket fd for channel
;; edi=channel number (0-5)
;; Returns: eax=socket fd, or -1 if invalid/not connected
;; ============================================================
get_channel_fd:
    cmp edi, NUM_CHANNELS
    jge .invalid
    lea rax, [rel client_fds]
    mov eax, [rax + rdi * 4]
    ret
.invalid:
    mov eax, -1
    ret

;; ============================================================
;; channels_respond — Write response to paired output channel
;; edi=input_channel (0,2,4), rsi=buf, edx=len
;; Automatically routes to edi+1 (output channel)
;; Returns: eax=bytes written
;; ============================================================
channels_respond:
    ; Output channel = input channel + 1
    inc edi
    jmp channels_write

;; ============================================================
;; channels_shutdown — Close all sockets
;; ============================================================
channels_shutdown:
    push rbx
    push r12
    sub rsp, 8

    ; Close clients
    xor r12d, r12d
.shut_clients:
    cmp r12d, NUM_CHANNELS
    jge .shut_listeners

    lea rcx, [rel client_fds]
    mov edi, [rcx + r12 * 4]
    cmp edi, -1
    je .shut_next_client

    mov eax, SYS_CLOSE
    syscall

.shut_next_client:
    inc r12d
    jmp .shut_clients

.shut_listeners:
    xor r12d, r12d
.shut_listen_loop:
    cmp r12d, NUM_CHANNELS
    jge .shut_done

    lea rcx, [rel listen_fds]
    mov edi, [rcx + r12 * 4]
    mov eax, SYS_CLOSE
    syscall

    inc r12d
    jmp .shut_listen_loop

.shut_done:
    mov dword [rel channels_ready], 0

    add rsp, 8
    pop r12
    pop rbx
    ret
