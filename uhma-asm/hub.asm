; hub.asm — Multi-agent communication hub (IRC-like)
;
; Single port, bidirectional sockets, plain text protocol.
; Learned from pitfalls: no paired ports, no timing dance, ignore SIGPIPE.
;
; @entry hub_main() -> noreturn     ; standalone hub entry
; @entry hub_init() -> void         ; init for embedding in UHMA
; @entry hub_poll() -> void         ; poll once (for UHMA integration)
;
; PROTOCOL:
;   Client -> Hub:
;     HELLO <name>\n        - register (max 31 chars)
;     @<name> <msg>\n       - send to specific client
;     * <msg>\n             - broadcast to all
;     <msg>\n               - broadcast (default)
;
;   Hub -> Client:
;     WELCOME\n             - registration ok
;     FROM <name>: <msg>\n  - message from client
;     JOINED <name>\n       - client joined
;     LEFT <name>\n         - client left
;     ERROR <msg>\n         - error
;
; BUILD: nasm -f elf64 hub.asm -o hub.o && ld hub.o -o hub
; or link with UHMA: add hub.o to link step

bits 64
default rel

; ============================================================================
; Constants
; ============================================================================

HUB_PORT        equ 7777
MAX_CLIENTS     equ 16
CLIENT_SIZE     equ 48          ; fd(4) + name(32) + state(4) + pad(8)
NAME_MAX        equ 31
BUF_SIZE        equ 4096

; Client states
STATE_EMPTY     equ 0
STATE_CONNECTED equ 1           ; connected but not registered
STATE_READY     equ 2           ; registered with name

; Syscall numbers
SYS_READ        equ 0
SYS_WRITE       equ 1
SYS_CLOSE       equ 3
SYS_POLL        equ 7
SYS_SOCKET      equ 41
SYS_ACCEPT      equ 43
SYS_BIND        equ 49
SYS_LISTEN      equ 50
SYS_SETSOCKOPT  equ 54
SYS_EXIT        equ 60
SYS_FCNTL       equ 72
SYS_RT_SIGACTION equ 13

; Socket constants
AF_INET         equ 2
SOCK_STREAM     equ 1
SOL_SOCKET      equ 1
SO_REUSEADDR    equ 2
INADDR_ANY      equ 0

; fcntl
F_GETFL         equ 3
F_SETFL         equ 4
O_NONBLOCK      equ 2048

; poll
POLLIN          equ 1
POLLHUP         equ 16
POLLERR         equ 8

; Signal
SIGPIPE         equ 13
SIG_IGN         equ 1

; ============================================================================
; Data Section
; ============================================================================

section .data

hub_running:    dq 1

; Listen socket
listen_fd:      dd -1

; Client table: MAX_CLIENTS entries of CLIENT_SIZE bytes
; Layout per entry: fd(4), name(32), state(4), pad(8)
align 8
client_table:   times (MAX_CLIENTS * CLIENT_SIZE) db 0

; Poll structures: listen socket + MAX_CLIENTS
; pollfd: fd(4), events(2), revents(2) = 8 bytes
align 8
poll_fds:       times ((MAX_CLIENTS + 1) * 8) db 0

; Buffers
align 8
recv_buf:       times BUF_SIZE db 0
send_buf:       times BUF_SIZE db 0

; Messages
msg_welcome:    db "WELCOME", 10, 0
msg_welcome_len equ $ - msg_welcome - 1

msg_error_name: db "ERROR name required", 10, 0
msg_error_name_len equ $ - msg_error_name - 1

msg_error_full: db "ERROR server full", 10, 0
msg_error_full_len equ $ - msg_error_full - 1

msg_error_unknown: db "ERROR unknown recipient", 10, 0
msg_error_unknown_len equ $ - msg_error_unknown - 1

msg_joined:     db "JOINED ", 0
msg_left:       db "LEFT ", 0
msg_from:       db "FROM ", 0

; sockaddr_in for bind (port 7777 = 0x1E61 -> network order 0x611E)
align 4
server_addr:
    dw AF_INET              ; sin_family
    dw 0x611E               ; sin_port (7777 in network byte order)
    dd INADDR_ANY           ; sin_addr
    times 8 db 0            ; padding

; ============================================================================
; Code Section
; ============================================================================

section .text

global _start
global hub_main
global hub_init
global hub_poll
global hub_stop

; ----------------------------------------------------------------------------
; _start - Entry point for standalone hub
; ----------------------------------------------------------------------------
_start:
hub_main:
    ; Ignore SIGPIPE (critical for TCP server)
    call ignore_sigpipe

    ; Initialize hub
    call hub_init
    test eax, eax
    js .init_failed

    ; Print startup message
    mov rdi, 1                  ; stdout
    lea rsi, [rel startup_msg]
    mov rdx, startup_msg_len
    mov rax, SYS_WRITE
    syscall

    ; Main loop
.loop:
    mov rax, [rel hub_running]
    test rax, rax
    jz .shutdown

    call hub_poll
    jmp .loop

.shutdown:
    call hub_cleanup
    xor edi, edi
    mov rax, SYS_EXIT
    syscall

.init_failed:
    mov rdi, 2                  ; stderr
    lea rsi, [rel init_fail_msg]
    mov rdx, init_fail_msg_len
    mov rax, SYS_WRITE
    syscall
    mov edi, 1
    mov rax, SYS_EXIT
    syscall

startup_msg:    db "HUB: listening on port 7777", 10, 0
startup_msg_len equ $ - startup_msg - 1

init_fail_msg:  db "HUB: init failed", 10, 0
init_fail_msg_len equ $ - init_fail_msg - 1

; ----------------------------------------------------------------------------
; ignore_sigpipe - Set SIGPIPE to SIG_IGN
; ----------------------------------------------------------------------------
ignore_sigpipe:
    push rbp
    mov rbp, rsp
    sub rsp, 32                 ; sigaction struct

    ; struct sigaction { handler, flags, restorer, mask }
    mov qword [rsp], SIG_IGN    ; sa_handler = SIG_IGN
    mov qword [rsp+8], 0        ; sa_flags
    mov qword [rsp+16], 0       ; sa_restorer
    mov qword [rsp+24], 0       ; sa_mask

    mov rdi, SIGPIPE
    lea rsi, [rsp]              ; new action
    xor rdx, rdx                ; old action (NULL)
    mov r10, 8                  ; sigsetsize
    mov rax, SYS_RT_SIGACTION
    syscall

    leave
    ret

; ----------------------------------------------------------------------------
; hub_init - Initialize hub (create listen socket)
; Returns: 0 on success, -1 on failure
; ----------------------------------------------------------------------------
hub_init:
    push rbp
    mov rbp, rsp
    push rbx

    ; Clear client table
    lea rdi, [rel client_table]
    mov rcx, MAX_CLIENTS * CLIENT_SIZE
    xor al, al
    rep stosb

    ; Mark all slots as empty (fd = -1)
    lea rdi, [rel client_table]
    mov ecx, MAX_CLIENTS
.clear_loop:
    mov dword [rdi], -1         ; fd = -1
    add rdi, CLIENT_SIZE
    dec ecx
    jnz .clear_loop

    ; Create socket
    mov rdi, AF_INET
    mov rsi, SOCK_STREAM
    xor rdx, rdx
    mov rax, SYS_SOCKET
    syscall
    test eax, eax
    js .fail
    mov ebx, eax                ; save socket fd
    mov [rel listen_fd], eax

    ; Set SO_REUSEADDR
    mov rdi, rbx
    mov rsi, SOL_SOCKET
    mov rdx, SO_REUSEADDR
    lea r10, [rel one_val]
    mov r8, 4
    mov rax, SYS_SETSOCKOPT
    syscall

    ; Set non-blocking
    mov rdi, rbx
    mov rsi, F_GETFL
    xor rdx, rdx
    mov rax, SYS_FCNTL
    syscall
    or eax, O_NONBLOCK
    mov rdi, rbx
    mov rsi, F_SETFL
    mov rdx, rax
    mov rax, SYS_FCNTL
    syscall

    ; Bind
    mov rdi, rbx
    lea rsi, [rel server_addr]
    mov rdx, 16
    mov rax, SYS_BIND
    syscall
    test eax, eax
    js .fail_close

    ; Listen
    mov rdi, rbx
    mov rsi, 16                 ; backlog
    mov rax, SYS_LISTEN
    syscall
    test eax, eax
    js .fail_close

    ; Success
    xor eax, eax
    pop rbx
    leave
    ret

.fail_close:
    mov rdi, rbx
    mov rax, SYS_CLOSE
    syscall
    mov dword [rel listen_fd], -1
.fail:
    mov eax, -1
    pop rbx
    leave
    ret

one_val:    dd 1

; ----------------------------------------------------------------------------
; hub_poll - Poll for events and handle them
; ----------------------------------------------------------------------------
hub_poll:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15

    ; Build poll array
    ; First entry: listen socket
    lea rdi, [rel poll_fds]
    mov eax, [rel listen_fd]
    mov [rdi], eax              ; fd
    mov word [rdi+4], POLLIN    ; events
    mov word [rdi+6], 0         ; revents

    ; Add client sockets
    mov r12d, 1                 ; poll index (starts at 1)
    lea r13, [rel client_table]
    mov r14d, MAX_CLIENTS

.build_poll:
    mov eax, [r13]              ; client fd
    cmp eax, -1
    je .next_client

    ; Add to poll array
    lea rdi, [rel poll_fds]
    imul r15d, r12d, 8          ; offset = index * 8
    add rdi, r15
    mov [rdi], eax              ; fd
    mov word [rdi+4], POLLIN    ; events
    mov word [rdi+6], 0         ; revents
    inc r12d

.next_client:
    add r13, CLIENT_SIZE
    dec r14d
    jnz .build_poll

    ; Call poll
    lea rdi, [rel poll_fds]
    mov rsi, r12                ; nfds
    mov rdx, 100                ; timeout ms
    mov rax, SYS_POLL
    syscall

    test eax, eax
    jle .done                   ; no events or error

    ; Check listen socket for new connections
    lea rdi, [rel poll_fds]
    mov ax, [rdi+6]             ; revents
    test ax, POLLIN
    jz .check_clients
    call accept_client

.check_clients:
    ; Check each client for data
    mov r12d, 1                 ; poll index
    lea r13, [rel client_table]
    mov r14d, MAX_CLIENTS

.check_loop:
    mov eax, [r13]              ; client fd
    cmp eax, -1
    je .next_check

    ; Find this fd in poll array
    lea rdi, [rel poll_fds]
    imul r15d, r12d, 8
    add rdi, r15

    mov ax, [rdi+6]             ; revents
    test ax, POLLIN
    jz .check_hup

    ; Data available - read and process
    mov rdi, r13                ; client entry
    call handle_client_data

.check_hup:
    ; Recalculate poll entry (rdi was clobbered by function call)
    lea rdi, [rel poll_fds]
    imul r15d, r12d, 8
    add rdi, r15
    mov ax, [rdi+6]
    test ax, (POLLHUP | POLLERR)
    jz .next_poll_idx

    ; Client disconnected
    mov rdi, r13
    call remove_client
    jmp .next_check             ; don't increment poll index

.next_poll_idx:
    inc r12d
.next_check:
    add r13, CLIENT_SIZE
    dec r14d
    jnz .check_loop

.done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

; ----------------------------------------------------------------------------
; accept_client - Accept new connection
; ----------------------------------------------------------------------------
accept_client:
    push rbp
    mov rbp, rsp
    push rbx
    push r12

    ; Accept connection
    mov edi, [rel listen_fd]
    xor rsi, rsi                ; addr = NULL
    xor rdx, rdx                ; addrlen = NULL
    mov rax, SYS_ACCEPT
    syscall

    test eax, eax
    js .done                    ; no connection or error
    mov ebx, eax                ; new client fd

    ; Set non-blocking
    mov rdi, rbx
    mov rsi, F_GETFL
    xor rdx, rdx
    mov rax, SYS_FCNTL
    syscall
    or eax, O_NONBLOCK
    mov rdi, rbx
    mov rsi, F_SETFL
    mov rdx, rax
    mov rax, SYS_FCNTL
    syscall

    ; Find empty slot
    lea r12, [rel client_table]
    mov ecx, MAX_CLIENTS
.find_slot:
    cmp dword [r12], -1
    je .found_slot
    add r12, CLIENT_SIZE
    dec ecx
    jnz .find_slot

    ; No slots - close connection
    mov rdi, rbx
    mov rax, SYS_CLOSE
    syscall

    ; Send error
    mov rdi, rbx
    lea rsi, [rel msg_error_full]
    mov rdx, msg_error_full_len
    mov rax, SYS_WRITE
    syscall
    jmp .done

.found_slot:
    ; Initialize client entry
    mov [r12], ebx              ; fd
    mov dword [r12+36], STATE_CONNECTED  ; state

    ; Clear name
    lea rdi, [r12+4]
    mov ecx, 32
    xor al, al
    rep stosb

.done:
    pop r12
    pop rbx
    leave
    ret

; ----------------------------------------------------------------------------
; handle_client_data - Read and process data from client
; rdi = client entry pointer
; Processes ALL lines in received data (multi-line support)
; ----------------------------------------------------------------------------
handle_client_data:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov r12, rdi                ; client entry
    mov ebx, [r12]              ; client fd

    ; Read data
    mov rdi, rbx
    lea rsi, [rel recv_buf]
    mov rdx, BUF_SIZE - 1
    mov rax, SYS_READ
    syscall

    test eax, eax
    jle .disconnect             ; EOF or error

    ; r14 = current line start, r15 = end of data
    lea r14, [rel recv_buf]
    lea r15, [r14 + rax]        ; end of data
    mov byte [r15], 0           ; null terminate

.next_line:
    ; Skip leading whitespace/newlines
    cmp r14, r15
    jge .done
    mov al, [r14]
    cmp al, 10
    je .skip_char
    cmp al, 13
    je .skip_char
    cmp al, 0
    je .done
    jmp .process_line
.skip_char:
    inc r14
    jmp .next_line

.process_line:
    ; Find end of this line (newline or end of buffer)
    mov r13, r14                ; line start
.find_eol:
    cmp r13, r15
    jge .eol_found
    mov al, [r13]
    cmp al, 10
    je .eol_found
    cmp al, 13
    je .eol_found
    cmp al, 0
    je .eol_found
    inc r13
    jmp .find_eol
.eol_found:
    mov byte [r13], 0           ; null-terminate this line

    ; Now process line at r14
    ; Check for HELLO command
    mov rdi, r14
    cmp byte [rdi], 'H'
    jne .not_hello
    cmp dword [rdi], 'HELL'
    jne .not_hello
    cmp byte [rdi+4], 'O'
    jne .not_hello
    cmp byte [rdi+5], ' '
    jne .not_hello

    ; HELLO <name> - register client
    lea rsi, [rdi+6]            ; name starts after "HELLO "
    mov rdi, r12
    call register_client
    jmp .advance_line

.not_hello:
    ; Must be registered to send messages
    cmp dword [r12+36], STATE_READY
    jne .advance_line           ; ignore if not registered

    ; Check for @name (directed message)
    mov rdi, r14
    cmp byte [rdi], '@'
    jne .broadcast

    ; Directed message: @name message
    mov rdi, r12                ; from client
    lea rsi, [r14 + 1]          ; skip @
    call send_directed
    jmp .advance_line

.broadcast:
    ; Check for * prefix (explicit broadcast)
    mov rdi, r14
    cmp byte [rdi], '*'
    jne .default_broadcast
    cmp byte [rdi+1], ' '
    jne .default_broadcast
    lea rsi, [rdi+2]            ; skip "* "
    jmp .do_broadcast

.default_broadcast:
    mov rsi, r14

.do_broadcast:
    mov rdi, r12                ; from client
    call broadcast_message

.advance_line:
    ; Move to next line
    lea r14, [r13 + 1]          ; skip past the null we wrote
    jmp .next_line

.disconnect:
    mov rdi, r12
    call remove_client

.done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

; ----------------------------------------------------------------------------
; register_client - Register client with name
; rdi = client entry, rsi = name (null or newline terminated)
; ----------------------------------------------------------------------------
register_client:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13

    mov r12, rdi                ; client entry
    mov r13, rsi                ; name
    mov ebx, [r12]              ; client fd

    ; Skip leading whitespace
.skip_ws:
    mov al, [r13]
    cmp al, ' '
    je .next_ws
    cmp al, 9                   ; tab
    jne .check_name
.next_ws:
    inc r13
    jmp .skip_ws

.check_name:
    ; Check name not empty
    mov al, [r13]
    cmp al, 0
    je .error_name
    cmp al, 10                  ; newline
    je .error_name
    cmp al, 13                  ; CR
    je .error_name

    ; Copy name (max 31 chars)
    lea rdi, [r12+4]            ; name field
    mov rsi, r13
    mov ecx, NAME_MAX
.copy_name:
    lodsb
    cmp al, 0
    je .name_done
    cmp al, 10
    je .name_done
    cmp al, 13
    je .name_done
    cmp al, ' '
    je .name_done
    stosb
    dec ecx
    jnz .copy_name
.name_done:
    mov byte [rdi], 0           ; null terminate

    ; Set state to ready
    mov dword [r12+36], STATE_READY

    ; Send WELCOME
    mov rdi, rbx
    lea rsi, [rel msg_welcome]
    mov rdx, msg_welcome_len
    mov rax, SYS_WRITE
    syscall

    ; Broadcast JOINED
    mov rdi, r12
    call broadcast_joined
    jmp .done

.error_name:
    mov rdi, rbx
    lea rsi, [rel msg_error_name]
    mov rdx, msg_error_name_len
    mov rax, SYS_WRITE
    syscall

.done:
    pop r13
    pop r12
    pop rbx
    leave
    ret

; ----------------------------------------------------------------------------
; broadcast_joined - Notify all clients that someone joined
; rdi = client entry that joined
; ----------------------------------------------------------------------------
broadcast_joined:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14

    mov r12, rdi                ; new client

    ; Build message: "JOINED <name>\n"
    lea rdi, [rel send_buf]
    lea rsi, [rel msg_joined]
.copy_joined:
    lodsb
    test al, al
    jz .add_name_j
    stosb
    jmp .copy_joined
.add_name_j:
    lea rsi, [r12+4]            ; name
.copy_name_j:
    lodsb
    test al, al
    jz .add_nl_j
    stosb
    jmp .copy_name_j
.add_nl_j:
    mov byte [rdi], 10
    inc rdi

    ; Calculate length
    lea rax, [rel send_buf]
    sub rdi, rax
    mov r13, rdi                ; message length

    ; Send to all OTHER registered clients
    lea r14, [rel client_table]
    mov ebx, MAX_CLIENTS
.send_joined:
    cmp dword [r14], -1
    je .next_joined
    cmp r14, r12                ; skip sender
    je .next_joined
    cmp dword [r14+36], STATE_READY
    jne .next_joined

    mov edi, [r14]              ; fd
    lea rsi, [rel send_buf]
    mov rdx, r13
    mov rax, SYS_WRITE
    syscall

.next_joined:
    add r14, CLIENT_SIZE
    dec ebx
    jnz .send_joined

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

; ----------------------------------------------------------------------------
; broadcast_message - Send message to all clients
; rdi = from client entry, rsi = message
; ----------------------------------------------------------------------------
broadcast_message:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov r12, rdi                ; from client
    mov r13, rsi                ; message

    ; Build message: "FROM <name>: <message>\n"
    lea rdi, [rel send_buf]
    lea rsi, [rel msg_from]
.copy_from:
    lodsb
    test al, al
    jz .add_sender
    stosb
    jmp .copy_from
.add_sender:
    lea rsi, [r12+4]            ; sender name
.copy_sender:
    lodsb
    test al, al
    jz .add_colon
    stosb
    jmp .copy_sender
.add_colon:
    mov byte [rdi], ':'
    mov byte [rdi+1], ' '
    add rdi, 2

    ; Copy message
    mov rsi, r13
.copy_msg:
    lodsb
    cmp al, 0
    je .msg_done
    cmp al, 10
    je .msg_done
    cmp al, 13
    je .msg_done
    stosb
    jmp .copy_msg
.msg_done:
    mov byte [rdi], 10
    inc rdi

    ; Calculate length
    lea rax, [rel send_buf]
    sub rdi, rax
    mov r14, rdi                ; message length

    ; Send to all OTHER registered clients
    lea r15, [rel client_table]
    mov ebx, MAX_CLIENTS
.send_bc:
    cmp dword [r15], -1
    je .next_bc
    cmp r15, r12                ; skip sender
    je .next_bc
    cmp dword [r15+36], STATE_READY
    jne .next_bc

    mov edi, [r15]              ; fd
    lea rsi, [rel send_buf]
    mov rdx, r14
    mov rax, SYS_WRITE
    syscall

.next_bc:
    add r15, CLIENT_SIZE
    dec ebx
    jnz .send_bc

    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

; ----------------------------------------------------------------------------
; send_directed - Send message to specific client
; rdi = from client entry, rsi = "@name message" (without @)
; ----------------------------------------------------------------------------
send_directed:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 40                 ; local: target name buffer

    mov r12, rdi                ; from client
    mov r13, rsi                ; @name message

    ; Extract target name
    lea rdi, [rsp]              ; target name buffer
    mov rsi, r13
    mov ecx, NAME_MAX
.extract_name:
    lodsb
    cmp al, ' '
    je .name_extracted
    cmp al, 0
    je .name_extracted
    cmp al, 10
    je .name_extracted
    stosb
    dec ecx
    jnz .extract_name
.name_extracted:
    mov byte [rdi], 0
    mov r14, rsi                ; message starts here

    ; Find target client by name
    lea r15, [rel client_table]
    mov ebx, MAX_CLIENTS
.find_target:
    cmp dword [r15], -1
    je .next_target
    cmp dword [r15+36], STATE_READY
    jne .next_target

    ; Compare names
    lea rdi, [r15+4]            ; client name
    lea rsi, [rsp]              ; target name
.cmp_names:
    mov al, [rdi]
    mov cl, [rsi]
    cmp al, cl
    jne .next_target
    test al, al
    jz .found_target
    inc rdi
    inc rsi
    jmp .cmp_names

.next_target:
    add r15, CLIENT_SIZE
    dec ebx
    jnz .find_target

    ; Target not found - send error
    mov edi, [r12]              ; sender fd
    lea rsi, [rel msg_error_unknown]
    mov rdx, msg_error_unknown_len
    mov rax, SYS_WRITE
    syscall
    jmp .done

.found_target:
    ; Build message: "FROM <sender>: <message>\n"
    lea rdi, [rel send_buf]
    lea rsi, [rel msg_from]
.copy_from_d:
    lodsb
    test al, al
    jz .add_sender_d
    stosb
    jmp .copy_from_d
.add_sender_d:
    lea rsi, [r12+4]            ; sender name
.copy_sender_d:
    lodsb
    test al, al
    jz .add_colon_d
    stosb
    jmp .copy_sender_d
.add_colon_d:
    mov byte [rdi], ':'
    mov byte [rdi+1], ' '
    add rdi, 2

    ; Copy message
    mov rsi, r14
.copy_msg_d:
    lodsb
    cmp al, 0
    je .msg_done_d
    cmp al, 10
    je .msg_done_d
    cmp al, 13
    je .msg_done_d
    stosb
    jmp .copy_msg_d
.msg_done_d:
    mov byte [rdi], 10
    inc rdi

    ; Calculate length
    lea rax, [rel send_buf]
    sub rdi, rax
    mov r13, rdi

    ; Send to target
    mov edi, [r15]              ; target fd
    lea rsi, [rel send_buf]
    mov rdx, r13
    mov rax, SYS_WRITE
    syscall

.done:
    add rsp, 40
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

; ----------------------------------------------------------------------------
; remove_client - Remove client and notify others
; rdi = client entry
; ----------------------------------------------------------------------------
remove_client:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14

    mov r12, rdi                ; client entry
    mov ebx, [r12]              ; fd

    ; Check if was registered (to send LEFT)
    cmp dword [r12+36], STATE_READY
    jne .just_close

    ; Build LEFT message
    lea rdi, [rel send_buf]
    lea rsi, [rel msg_left]
.copy_left:
    lodsb
    test al, al
    jz .add_name_l
    stosb
    jmp .copy_left
.add_name_l:
    lea rsi, [r12+4]            ; name
.copy_name_l:
    lodsb
    test al, al
    jz .add_nl_l
    stosb
    jmp .copy_name_l
.add_nl_l:
    mov byte [rdi], 10
    inc rdi

    lea rax, [rel send_buf]
    sub rdi, rax
    mov r13, rdi                ; length

    ; Send LEFT to all other clients
    lea r14, [rel client_table]
    mov ecx, MAX_CLIENTS
.send_left:
    cmp dword [r14], -1
    je .next_left
    cmp r14, r12
    je .next_left
    cmp dword [r14+36], STATE_READY
    jne .next_left

    push rcx
    mov edi, [r14]
    lea rsi, [rel send_buf]
    mov rdx, r13
    mov rax, SYS_WRITE
    syscall
    pop rcx

.next_left:
    add r14, CLIENT_SIZE
    dec ecx
    jnz .send_left

.just_close:
    ; Close socket
    mov rdi, rbx
    mov rax, SYS_CLOSE
    syscall

    ; Mark slot as empty
    mov dword [r12], -1
    mov dword [r12+36], STATE_EMPTY

    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret

; ----------------------------------------------------------------------------
; hub_stop - Signal hub to stop
; ----------------------------------------------------------------------------
hub_stop:
    mov qword [rel hub_running], 0
    ret

; ----------------------------------------------------------------------------
; hub_cleanup - Close all sockets
; ----------------------------------------------------------------------------
hub_cleanup:
    push rbp
    mov rbp, rsp
    push rbx
    push r12

    ; Close listen socket
    mov edi, [rel listen_fd]
    cmp edi, -1
    je .close_clients
    mov rax, SYS_CLOSE
    syscall
    mov dword [rel listen_fd], -1

.close_clients:
    lea r12, [rel client_table]
    mov ebx, MAX_CLIENTS
.close_loop:
    mov edi, [r12]
    cmp edi, -1
    je .next_close
    mov rax, SYS_CLOSE
    syscall
    mov dword [r12], -1
.next_close:
    add r12, CLIENT_SIZE
    dec ebx
    jnz .close_loop

    pop r12
    pop rbx
    leave
    ret
