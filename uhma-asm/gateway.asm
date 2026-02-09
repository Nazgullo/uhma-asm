; gateway.asm — Single-port framed TCP gateway
;
; @entry gateway_init() -> eax=1 success, 0 failure
; @entry gateway_poll() -> eax=client_id (0-7), -1 stdin, -2 timeout
; @entry gateway_read(edi=client_id, rsi=buf, edx=maxlen) -> eax=payload bytes
; @entry gateway_respond(edi=client_id, rsi=buf, edx=len) -> sends framed response
; @entry gateway_stream_set(edi=client_id, esi=subnet) -> set live stream target
; @entry gateway_stream_send(rsi=buf, edx=len) -> send framed stream payload
; @entry gateway_shutdown() -> closes all sockets
; @calls format.asm:print_cstr, print_u64
; @calls signal.asm:set_sigpipe_mode
; @calledby boot.asm:_start, repl.asm:repl_run
;
; PROTOCOL:
;   Frame: [2B magic 0x5548] [1B subnet] [1B host] [2B seq_id] [2B payload_len] [payload]
;   Responses use matching seq_id so client can demux by subnet + seq_id
;
; GOTCHAS:
;   - gateway_init() sets SIGPIPE to ignore mode
;   - Max 8 simultaneous clients
;   - gateway_read returns payload only (header already parsed)
;   - gateway_respond clamps to GW_MAX_PAYLOAD (full logs are in stream)

%include "syscalls.inc"
%include "constants.inc"

section .data
    gw_init_msg:    db "[GATEWAY] Single-port gateway on port 9999", 10, 0
    gw_accept_msg:  db "[GW] client ", 0
    gw_accept_end:  db " connected", 10, 0
    gw_close_msg:   db "[GW] client ", 0
    gw_close_end:   db " disconnected", 10, 0
    gw_full_msg:    db "[GW] max clients reached, rejecting", 10, 0

section .bss
    ; Listening socket
    gw_listen_fd:   resd 1

    ; Client table: GW_MAX_CLIENTS entries of GW_CLIENT_SIZE bytes each
    gw_clients:     resb GW_CLIENT_SIZE * GW_MAX_CLIENTS

    gw_client_count: resd 1

    ; Poll structures: stdin + listener + max_clients = 10 entries
    ; struct pollfd { int fd; short events; short revents; } = 8 bytes
    gw_poll_fds:    resb 8 * (2 + GW_MAX_CLIENTS)

    ; sockaddr_in for bind/accept
    gw_sock_addr:   resb 16
    gw_addr_len:    resd 1

    ; Frame read buffer (header + max payload)
    gw_frame_buf:   resb GW_HEADER_SIZE + GW_MAX_PAYLOAD

    ; Set by gateway_read for gateway_respond to use
    gw_last_seq:    resw 1
    gw_last_subnet: resb 1
    gw_last_client: resd 1

    ; Live stream target (run-log feed)
    gw_stream_client: resd 1       ; -1 = disabled
    gw_stream_subnet: resb 1
    gw_stream_seq:    resw 1

    gw_ready:       resd 1

section .text

extern print_cstr
extern print_u64
extern stdin_active
extern set_sigpipe_mode

global gateway_init
global gateway_poll
global gateway_read
global gateway_respond
global gateway_stream_set
global gateway_stream_clear
global gateway_stream_send
global gw_stream_client
global gw_stream_subnet
global gateway_shutdown

;; ============================================================
;; gateway_init — Create single listening socket on port 9999
;; Returns: eax=1 success, 0 failure
;; ============================================================
gateway_init:
    push rbx
    push r12
    sub rsp, 8                        ; align (2 pushes = even, +8)

    ; Init client table: all fds = -1
    xor ecx, ecx
.init_clients:
    cmp ecx, GW_MAX_CLIENTS
    jge .clients_done
    lea rax, [rel gw_clients]
    imul edx, ecx, GW_CLIENT_SIZE
    mov dword [rax + rdx + GW_CLIENT_FD], -1
    inc ecx
    jmp .init_clients
.clients_done:
    mov dword [rel gw_client_count], 0
    mov dword [rel gw_stream_client], -1
    mov byte [rel gw_stream_subnet], SUBNET_CONSOL
    mov word [rel gw_stream_seq], 0

    ; socket(AF_INET, SOCK_STREAM, 0)
    mov eax, SYS_SOCKET
    mov edi, AF_INET
    mov esi, SOCK_STREAM
    xor edx, edx
    syscall
    test eax, eax
    js .init_fail
    mov ebx, eax
    mov [rel gw_listen_fd], eax

    ; setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &1, 4)
    mov eax, SYS_SETSOCKOPT
    mov edi, ebx
    mov esi, SOL_SOCKET
    mov edx, SO_REUSEADDR
    lea r10, [rel gw_addr_len]
    mov dword [r10], 1
    mov r8d, 4
    syscall

    ; Setup sockaddr_in for port 9999
    lea rdi, [rel gw_sock_addr]
    mov word [rdi], AF_INET           ; sin_family
    mov eax, GW_PORT
    xchg al, ah                       ; htons
    mov [rdi + 2], ax
    mov dword [rdi + 4], 0            ; INADDR_ANY

    ; bind
    mov eax, SYS_BIND
    mov edi, ebx
    lea rsi, [rel gw_sock_addr]
    mov edx, 16
    syscall
    test eax, eax
    js .init_fail_close

    ; listen(fd, 8)
    mov eax, SYS_LISTEN
    mov edi, ebx
    mov esi, 8
    syscall
    test eax, eax
    js .init_fail_close

    ; Enable SIGPIPE ignore mode for TCP
    mov edi, 1
    call set_sigpipe_mode

    mov dword [rel gw_ready], 1
    lea rdi, [rel gw_init_msg]
    call print_cstr

    mov eax, 1
    jmp .init_ret

.init_fail_close:
    mov edi, ebx
    mov eax, SYS_CLOSE
    syscall
.init_fail:
    xor eax, eax

.init_ret:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; gateway_poll — Wait for data on gateway or stdin
;; Returns: eax = client_id (0-7), -1 stdin, -2 timeout
;; ============================================================
gateway_poll:
    push rbx
    push r12
    push r13
    sub rsp, 16                       ; align (3 pushes = odd, +16)

    lea rbx, [rel gw_poll_fds]

    ; Entry 0: stdin
    mov eax, [rel stdin_active]
    test eax, eax
    jz .stdin_dead
    mov dword [rbx], 0               ; fd = stdin
    jmp .stdin_set
.stdin_dead:
    mov dword [rbx], -1              ; poll ignores -1
.stdin_set:
    mov word [rbx + 4], POLLIN
    mov word [rbx + 6], 0

    ; Entry 1: listener
    mov eax, [rel gw_listen_fd]
    mov [rbx + 8], eax
    mov word [rbx + 12], POLLIN
    mov word [rbx + 14], 0

    ; Entries 2+: client sockets
    xor r12d, r12d
.add_clients:
    cmp r12d, GW_MAX_CLIENTS
    jge .do_poll

    lea rcx, [rel gw_clients]
    imul eax, r12d, GW_CLIENT_SIZE
    mov eax, [rcx + rax + GW_CLIENT_FD]
    lea rcx, [rbx + 16 + r12 * 8]    ; offset = (2 + r12) * 8
    mov [rcx], eax
    mov word [rcx + 4], POLLIN | POLLHUP
    mov word [rcx + 6], 0

    inc r12d
    jmp .add_clients

.do_poll:
    mov eax, SYS_POLL
    lea rdi, [rel gw_poll_fds]
    mov esi, 2 + GW_MAX_CLIENTS      ; stdin + listener + clients
    mov edx, 100                      ; 100ms timeout
    syscall
    test eax, eax
    jle .poll_timeout

    ; Check stdin
    lea rbx, [rel gw_poll_fds]
    movzx eax, word [rbx + 6]
    test ax, POLLIN
    jnz .poll_stdin

    ; Check listener for new connection
    movzx eax, word [rbx + 14]
    test ax, POLLIN
    jnz .do_accept

    ; Check clients for hangup (always, even if POLLIN is also set)
    xor r12d, r12d
.check_hangup:
    cmp r12d, GW_MAX_CLIENTS
    jge .check_data

    lea rcx, [rbx + 16 + r12 * 8]
    movzx eax, word [rcx + 6]
    test ax, POLLHUP | POLLERR
    jz .next_hangup

    ; Close this client (reaps zombie connections promptly)
    call gw_close_client

.next_hangup:
    inc r12d
    jmp .check_hangup

    ; Check clients for data
.check_data:
    xor r12d, r12d
.check_client:
    cmp r12d, GW_MAX_CLIENTS
    jge .poll_timeout

    lea rcx, [rbx + 16 + r12 * 8]
    movzx eax, word [rcx + 6]
    test ax, POLLIN
    jnz .got_client

    inc r12d
    jmp .check_client

.got_client:
    mov eax, r12d                     ; return client_id
    jmp .poll_ret

.poll_stdin:
    mov eax, -1
    jmp .poll_ret

.do_accept:
    call gw_accept_client
    jmp .do_poll                      ; re-poll after accept

.poll_timeout:
    mov eax, -2

.poll_ret:
    add rsp, 16
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; gw_accept_client — Accept new connection, add to client table
;; Internal helper
;; ============================================================
gw_accept_client:
    push rbx
    push r12
    sub rsp, 8                        ; align (2 pushes = even, +8)

    ; Find empty slot
    xor r12d, r12d
    lea rbx, [rel gw_clients]
.find_slot:
    cmp r12d, GW_MAX_CLIENTS
    jge .table_full

    imul eax, r12d, GW_CLIENT_SIZE
    cmp dword [rbx + rax + GW_CLIENT_FD], -1
    je .found_slot

    inc r12d
    jmp .find_slot

.table_full:
    ; Try to reap zombie connections before rejecting
    xor r12d, r12d
.reap_zombies:
    cmp r12d, GW_MAX_CLIENTS
    jge .truly_full

    imul eax, r12d, GW_CLIENT_SIZE
    mov edi, [rbx + rax + GW_CLIENT_FD]
    cmp edi, -1
    je .reap_found_empty          ; found a free slot after all

    ; poll(fd, POLLIN, 0) — check if this fd is dead
    push r12
    sub rsp, 8                    ; align for syscall
    mov [rsp], edi                ; pollfd.fd
    mov word [rsp + 4], 0x0001   ; events = POLLIN
    mov word [rsp + 6], 0        ; revents = 0
    mov eax, SYS_POLL
    mov rdi, rsp
    mov esi, 1
    xor edx, edx                 ; timeout = 0
    syscall
    movzx eax, word [rsp + 6]    ; revents
    add rsp, 8
    pop r12
    test ax, POLLHUP | POLLERR
    jz .reap_next

    ; Dead connection — close it and use this slot
    call gw_close_client
    jmp .find_slot                ; retry find_slot now that one is free

.reap_next:
    inc r12d
    jmp .reap_zombies

.reap_found_empty:
    jmp .find_slot                ; re-scan, should find the empty slot

.truly_full:
    ; All slots verified alive — reject for real
    mov dword [rel gw_addr_len], 16
    mov eax, SYS_ACCEPT
    mov edi, [rel gw_listen_fd]
    lea rsi, [rel gw_sock_addr]
    lea rdx, [rel gw_addr_len]
    syscall
    test eax, eax
    js .accept_done
    mov edi, eax
    mov eax, SYS_CLOSE
    syscall
    lea rdi, [rel gw_full_msg]
    call print_cstr
    jmp .accept_done

.found_slot:
    mov dword [rel gw_addr_len], 16
    mov eax, SYS_ACCEPT
    mov edi, [rel gw_listen_fd]
    lea rsi, [rel gw_sock_addr]
    lea rdx, [rel gw_addr_len]
    syscall
    test eax, eax
    js .accept_done

    ; Store in client table
    imul ecx, r12d, GW_CLIENT_SIZE
    mov [rbx + rcx + GW_CLIENT_FD], eax
    mov word [rbx + rcx + GW_CLIENT_SEQ], 0
    mov byte [rbx + rcx + GW_CLIENT_SUBNET], 0
    mov byte [rbx + rcx + GW_CLIENT_FLAGS], 0

    inc dword [rel gw_client_count]

    ; Print connection message
    lea rdi, [rel gw_accept_msg]
    call print_cstr
    mov edi, r12d
    call print_u64
    lea rdi, [rel gw_accept_end]
    call print_cstr

.accept_done:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; gw_close_client — Close client r12d and remove from table
;; Internal helper, r12d = client index
;; ============================================================
gw_close_client:
    push rbx
    push r12
    sub rsp, 8                        ; align (2 pushes = even, +8)

    lea rbx, [rel gw_clients]
    imul eax, r12d, GW_CLIENT_SIZE
    mov edi, [rbx + rax + GW_CLIENT_FD]
    cmp edi, -1
    je .close_done

    ; Close socket
    push rax                          ; save offset
    mov eax, SYS_CLOSE
    syscall
    pop rax

    ; Mark slot empty
    mov dword [rbx + rax + GW_CLIENT_FD], -1

    ; If this was the live stream target, clear it
    cmp dword [rel gw_stream_client], r12d
    jne .close_count
    mov dword [rel gw_stream_client], -1
    mov word [rel gw_stream_seq], 0

.close_count:
    dec dword [rel gw_client_count]

    ; Print disconnect
    lea rdi, [rel gw_close_msg]
    call print_cstr
    mov edi, r12d
    call print_u64
    lea rdi, [rel gw_close_end]
    call print_cstr

.close_done:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; gateway_read — Read and parse frame from client
;; edi=client_id, rsi=output_buf, edx=maxlen
;; Returns: eax=payload bytes (command text in output_buf), 0 on error
;; Sets gw_last_seq, gw_last_subnet, gw_last_client for gateway_respond
;; ============================================================
gateway_read:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 16                       ; align (5 pushes = odd, +16)

    mov r12d, edi                     ; client_id
    mov r14, rsi                      ; caller's output buf (PRESERVED)
    mov r15d, edx                     ; max output len (PRESERVED)
    mov [rel gw_last_client], r12d

    ; Get client table offset
    lea rbx, [rel gw_clients]
    imul r13d, r12d, GW_CLIENT_SIZE   ; r13d = table offset

    ; Get client fd
    mov edi, [rbx + r13 + GW_CLIENT_FD]
    cmp edi, -1
    je .read_err

    ; === STRICT FRAMED READ ===
    ; Read full header (blocking, handles partial reads)
    lea rsi, [rel gw_frame_buf]
    mov edx, GW_HEADER_SIZE
    call gw_read_exact
    cmp eax, GW_HEADER_SIZE
    jne .read_disconnect

    ; Validate magic header (accept swapped endian for tolerance)
    lea rdx, [rel gw_frame_buf]
    xor r9d, r9d                    ; swap flag = 0
    movzx eax, word [rdx]
    cmp ax, GW_MAGIC
    je .magic_ok
    xchg al, ah
    cmp ax, GW_MAGIC
    jne .read_protocol_err
    mov r9d, 1                      ; accept swapped header
.magic_ok:

    ; Parse header
    movzx eax, byte [rdx + 2]        ; subnet
    mov byte [rel gw_last_subnet], al
    mov byte [rel gw_stream_subnet], al
    mov byte [rbx + r13 + GW_CLIENT_SUBNET], al

    movzx eax, word [rdx + 4]        ; seq_id
    test r9d, r9d
    jz .seq_ok
    xchg al, ah
.seq_ok:
    mov word [rel gw_last_seq], ax
    mov [rbx + r13 + GW_CLIENT_SEQ], ax

    movzx eax, word [rdx + 6]        ; payload_len
    test r9d, r9d
    jz .len_ok
    xchg al, ah
.len_ok:
    test eax, eax
    jle .read_err
    cmp eax, GW_MAX_PAYLOAD
    jg .read_protocol_err
    mov [rsp], eax                   ; save payload_len on stack (r11 clobbered by syscall)

    ; Read full payload
    mov edi, [rbx + r13 + GW_CLIENT_FD]
    lea rsi, [rel gw_frame_buf + GW_HEADER_SIZE]
    mov edx, eax
    call gw_read_exact
    cmp eax, [rsp]
    jne .read_disconnect

    ; Copy payload to caller's buffer (truncate if needed)
    mov ecx, [rsp]
    cmp ecx, r15d
    jle .payload_copy
    mov ecx, r15d
.payload_copy:
    push rcx
    lea rsi, [rel gw_frame_buf + GW_HEADER_SIZE]  ; src = payload
    mov rdi, r14                                  ; dst = caller's buf
    rep movsb
    pop rax                                       ; return byte count in rax
    jmp .read_ret

.read_protocol_err:
    ; Invalid frame — close client to keep stream sane
    call gw_close_client
    jmp .read_err

.read_disconnect:
    call gw_close_client

.read_err:
    xor eax, eax

.read_ret:
    add rsp, 16
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; gw_read_exact — Read exactly edx bytes from socket
;; edi=fd, rsi=buf, edx=len
;; Returns: eax = bytes read (len) or <=0 on error/EOF
;; ============================================================
gw_read_exact:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 8                        ; 4 pushes (even) + 8 = aligned

    mov ebx, edi                      ; fd
    mov r12, rsi                      ; base buffer
    mov r14d, edx                     ; remaining (callee-saved, safe across syscall)
    xor r13d, r13d                    ; total read (callee-saved, safe across syscall)

.rex_loop:
    test r14d, r14d
    jz .rex_done
    mov eax, SYS_READ
    mov edi, ebx
    lea rsi, [r12 + r13]
    mov edx, r14d
    syscall
    test eax, eax
    jle .rex_err
    add r13d, eax
    sub r14d, eax
    jmp .rex_loop

.rex_done:
    mov eax, r13d
    jmp .rex_ret

.rex_err:
    ; eax already contains <=0
    nop

.rex_ret:
    add rsp, 8
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; gateway_respond — Send response to client
;; edi=client_id, rsi=buf, edx=len
;; Wraps payload in frame with matching seq_id
;; ============================================================
gateway_respond:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 8                        ; align (4 pushes = even, +8)

    mov r12d, edi                     ; client_id
    mov r13, rsi                      ; response buf
    mov r14d, edx                     ; response len

    ; Clamp to protocol max (response stream carries full log)
    cmp r14d, GW_MAX_PAYLOAD
    jle .resp_len_ok
    mov r14d, GW_MAX_PAYLOAD
.resp_len_ok:

    ; Get client info
    lea rbx, [rel gw_clients]
    imul eax, r12d, GW_CLIENT_SIZE
    mov edi, [rbx + rax + GW_CLIENT_FD]
    cmp edi, -1
    je .respond_done

    ; === FRAMED RESPONSE ===
    ; Build frame header in gw_frame_buf, then send header + payload
    lea rcx, [rel gw_frame_buf]
    mov word [rcx], GW_MAGIC          ; magic
    mov al, [rbx + rax + GW_CLIENT_SUBNET]
    mov [rcx + 2], al                 ; subnet (echo back)
    mov byte [rcx + 3], 0            ; host = 0 (from server)
    ; Use the seq_id from the request
    movzx eax, word [rel gw_last_seq]
    mov [rcx + 4], ax                ; seq_id
    mov [rcx + 6], r14w              ; payload_len

    ; Send header
    push r12
    mov eax, SYS_WRITE
    ; edi already has client fd from above — wait, we need to reload
    imul eax, r12d, GW_CLIENT_SIZE
    mov edi, [rbx + rax + GW_CLIENT_FD]
    lea rsi, [rel gw_frame_buf]
    mov edx, GW_HEADER_SIZE
    mov eax, SYS_WRITE
    syscall
    pop r12

    test eax, eax
    js .respond_write_fail

    ; Send payload
    imul eax, r12d, GW_CLIENT_SIZE
    mov edi, [rbx + rax + GW_CLIENT_FD]
    mov rsi, r13
    mov edx, r14d
    mov eax, SYS_WRITE
    syscall
    test eax, eax
    js .respond_write_fail
    jmp .respond_done

.respond_write_fail:
    ; Client gone (EPIPE) — close slot to prevent zombie accumulation
    call gw_close_client

.respond_done:
    add rsp, 8
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; gateway_stream_set — Set live stream target
;; edi=client_id, esi=subnet
;; ============================================================
gateway_stream_set:
    mov [rel gw_stream_client], edi
    mov [rel gw_stream_subnet], sil
    ret

;; ============================================================
;; gateway_stream_clear — Disable live stream
;; ============================================================
gateway_stream_clear:
    mov dword [rel gw_stream_client], -1
    mov word [rel gw_stream_seq], 0
    ret

;; ============================================================
;; gateway_stream_send — Send framed stream payload to live target
;; rsi=buf, edx=len
;; ============================================================
gateway_stream_send:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 8                        ; align (4 pushes = even, +8)

    mov eax, [rel gw_stream_client]
    cmp eax, -1
    je .stream_done
    mov r12d, eax                     ; client_id
    mov r13, rsi                      ; payload ptr
    mov r14d, edx                     ; payload len

    ; Clamp payload to max
    cmp r14d, GW_MAX_PAYLOAD
    jle .len_ok
    mov r14d, GW_MAX_PAYLOAD
.len_ok:

    ; Get client fd
    lea rbx, [rel gw_clients]
    imul eax, r12d, GW_CLIENT_SIZE
    mov edi, [rbx + rax + GW_CLIENT_FD]
    cmp edi, -1
    jne .fd_ok
    mov dword [rel gw_stream_client], -1
    mov word [rel gw_stream_seq], 0
    jmp .stream_done
.fd_ok:

    ; Increment stream seq
    movzx eax, word [rel gw_stream_seq]
    inc eax
    mov [rel gw_stream_seq], ax

    ; Build frame header
    lea rcx, [rel gw_frame_buf]
    mov word [rcx], GW_MAGIC
    mov al, [rel gw_stream_subnet]
    mov [rcx + 2], al
    mov byte [rcx + 3], 0
    mov [rcx + 4], ax
    mov [rcx + 6], r14w

    ; Send header
    imul eax, r12d, GW_CLIENT_SIZE
    mov edi, [rbx + rax + GW_CLIENT_FD]
    lea rsi, [rel gw_frame_buf]
    mov edx, GW_HEADER_SIZE
    mov eax, SYS_WRITE
    syscall
    test eax, eax
    js .stream_done

    ; Send payload
    imul eax, r12d, GW_CLIENT_SIZE
    mov edi, [rbx + rax + GW_CLIENT_FD]
    mov rsi, r13
    mov edx, r14d
    mov eax, SYS_WRITE
    syscall

.stream_done:
    add rsp, 8
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; gateway_shutdown — Close all client sockets and listener
;; ============================================================
gateway_shutdown:
    push rbx
    push r12
    sub rsp, 8                        ; align (2 pushes = even, +8)

    ; Disable live stream
    mov dword [rel gw_stream_client], -1
    mov word [rel gw_stream_seq], 0

    ; Close all clients
    xor r12d, r12d
.shut_clients:
    cmp r12d, GW_MAX_CLIENTS
    jge .shut_listener

    lea rbx, [rel gw_clients]
    imul eax, r12d, GW_CLIENT_SIZE
    mov edi, [rbx + rax + GW_CLIENT_FD]
    cmp edi, -1
    je .shut_next

    push rax
    mov eax, SYS_CLOSE
    syscall
    pop rax
    mov dword [rbx + rax + GW_CLIENT_FD], -1

.shut_next:
    inc r12d
    jmp .shut_clients

.shut_listener:
    mov edi, [rel gw_listen_fd]
    mov eax, SYS_CLOSE
    syscall

    mov dword [rel gw_ready], 0
    mov dword [rel gw_client_count], 0

    add rsp, 8
    pop r12
    pop rbx
    ret
