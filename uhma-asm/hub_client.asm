; hub_client.asm — Hub client for UHMA
;
; Connects UHMA to the multi-agent hub on port 7777.
; UHMA can receive commands from other agents and send responses.
;
; @entry hub_client_init() -> eax (0=success, -1=fail)
; @entry hub_client_poll() -> eax (1=has data, 0=no data, -1=disconnected)
; @entry hub_client_read(buf, len) -> bytes read
; @entry hub_client_send(msg) -> bytes sent
; @entry hub_client_close() -> void
;
; INTEGRATION:
;   In repl.asm, add hub socket to poll alongside stdin.
;   Messages from hub: "FROM <name>: <command>" -> extract command, process, respond
;   Responses: hub_client_send("result text")

bits 64
default rel

; ============================================================================
; Constants
; ============================================================================

HUB_PORT        equ 7777
HUB_BUF_SIZE    equ 4096

; Syscalls
SYS_READ        equ 0
SYS_WRITE       equ 1
SYS_CLOSE       equ 3
SYS_POLL        equ 7
SYS_SOCKET      equ 41
SYS_CONNECT     equ 42
SYS_FCNTL       equ 72

; Socket
AF_INET         equ 2
SOCK_STREAM     equ 1

; fcntl
F_GETFL         equ 3
F_SETFL         equ 4
O_NONBLOCK      equ 2048

; poll
POLLIN          equ 1

; ============================================================================
; Data
; ============================================================================

section .data

global hub_fd
global hub_connected
global hub_registered

hub_fd:         dd -1           ; hub socket fd
hub_connected:  dd 0            ; 1 if connected
hub_registered: dd 0            ; 1 if registered with name

; Hub address (127.0.0.1:7777)
hub_addr:
    dw AF_INET                  ; sin_family
    dw 0x611E                   ; sin_port (7777 in network byte order)
    dd 0x0100007F               ; sin_addr (127.0.0.1 in network byte order)
    times 8 db 0                ; padding

; Registration message
hub_hello:      db "HELLO uhma", 10, 0
hub_hello_len   equ $ - hub_hello - 1

; Response prefix for hub (identifies responses from UHMA)
hub_prefix:     db "* ", 0      ; broadcast responses

section .bss

hub_recv_buf:   resb HUB_BUF_SIZE
hub_send_buf:   resb HUB_BUF_SIZE

; ============================================================================
; Code
; ============================================================================

section .text

global hub_client_init
global hub_client_poll
global hub_client_read
global hub_client_send
global hub_client_broadcast
global hub_client_close
global hub_client_get_fd
global hub_extract_command

; ----------------------------------------------------------------------------
; hub_client_init - Connect to hub and register as "uhma"
; Returns: 0 on success, -1 on failure
; ----------------------------------------------------------------------------
hub_client_init:
    push rbp
    mov rbp, rsp
    push rbx

    ; Create socket
    mov rdi, AF_INET
    mov rsi, SOCK_STREAM
    xor rdx, rdx
    mov rax, SYS_SOCKET
    syscall
    test eax, eax
    js .fail
    mov ebx, eax
    mov [rel hub_fd], eax

    ; Connect to hub
    mov rdi, rbx
    lea rsi, [rel hub_addr]
    mov rdx, 16
    mov rax, SYS_CONNECT
    syscall
    test eax, eax
    js .fail_close

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

    ; Send HELLO
    mov rdi, rbx
    lea rsi, [rel hub_hello]
    mov rdx, hub_hello_len
    mov rax, SYS_WRITE
    syscall
    test eax, eax
    js .fail_close

    mov dword [rel hub_connected], 1
    mov dword [rel hub_registered], 1

    ; Success
    xor eax, eax
    pop rbx
    leave
    ret

.fail_close:
    mov rdi, rbx
    mov rax, SYS_CLOSE
    syscall
    mov dword [rel hub_fd], -1
.fail:
    mov dword [rel hub_connected], 0
    mov eax, -1
    pop rbx
    leave
    ret

; ----------------------------------------------------------------------------
; hub_client_poll - Check if hub has data (non-blocking)
; Returns: 1 if data available, 0 if not, -1 if disconnected
; ----------------------------------------------------------------------------
hub_client_poll:
    push rbp
    mov rbp, rsp
    sub rsp, 16                 ; pollfd struct

    mov eax, [rel hub_fd]
    cmp eax, -1
    je .disconnected

    ; Setup pollfd
    mov [rsp], eax              ; fd
    mov word [rsp+4], POLLIN    ; events
    mov word [rsp+6], 0         ; revents

    ; Poll with 0 timeout (non-blocking check)
    lea rdi, [rsp]
    mov rsi, 1                  ; nfds
    xor rdx, rdx                ; timeout = 0
    mov rax, SYS_POLL
    syscall

    test eax, eax
    jle .no_data

    ; Check revents
    mov ax, [rsp+6]
    test ax, POLLIN
    jz .no_data

    mov eax, 1                  ; has data
    leave
    ret

.no_data:
    xor eax, eax
    leave
    ret

.disconnected:
    mov eax, -1
    leave
    ret

; ----------------------------------------------------------------------------
; hub_client_read - Read data from hub
; rdi = buffer, rsi = max length
; Returns: bytes read, 0 if no data, -1 if error/disconnected
; ----------------------------------------------------------------------------
hub_client_read:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13

    mov r12, rdi                ; buffer
    mov r13, rsi                ; max len

    mov ebx, [rel hub_fd]
    cmp ebx, -1
    je .error

    mov rdi, rbx
    mov rsi, r12
    mov rdx, r13
    mov rax, SYS_READ
    syscall

    ; Check result
    test eax, eax
    jle .check_error
    jmp .done

.check_error:
    ; 0 = EOF (disconnect), negative = error
    cmp eax, 0
    je .disconnected
    ; EAGAIN/EWOULDBLOCK = no data (non-blocking)
    cmp eax, -11                ; EAGAIN
    je .no_data
    cmp eax, -35                ; EWOULDBLOCK
    je .no_data
    jmp .error

.no_data:
    xor eax, eax
    jmp .done

.disconnected:
    call hub_client_close
.error:
    mov eax, -1

.done:
    pop r13
    pop r12
    pop rbx
    leave
    ret

; ----------------------------------------------------------------------------
; hub_client_send - Send message to hub (directed to specific client)
; rdi = target name (null = broadcast), rsi = message
; Returns: bytes sent or -1
; ----------------------------------------------------------------------------
hub_client_send:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13

    mov r12, rdi                ; target (or NULL for broadcast)
    mov r13, rsi                ; message

    mov ebx, [rel hub_fd]
    cmp ebx, -1
    je .error

    ; Build message in hub_send_buf
    lea rdi, [rel hub_send_buf]

    ; If target is NULL, broadcast with "*"
    test r12, r12
    jz .broadcast

    ; Directed: "@target message\n"
    mov byte [rdi], '@'
    inc rdi
    mov rsi, r12
.copy_target:
    lodsb
    test al, al
    jz .add_space
    stosb
    jmp .copy_target
.add_space:
    mov byte [rdi], ' '
    inc rdi
    jmp .copy_message

.broadcast:
    ; Broadcast: "* message\n"
    mov byte [rdi], '*'
    mov byte [rdi+1], ' '
    add rdi, 2

.copy_message:
    mov rsi, r13
.copy_msg:
    lodsb
    cmp al, 0
    je .msg_done
    cmp al, 10
    je .msg_done
    stosb
    jmp .copy_msg
.msg_done:
    mov byte [rdi], 10          ; newline
    inc rdi

    ; Calculate length
    lea rax, [rel hub_send_buf]
    sub rdi, rax
    mov r13, rdi                ; length

    ; Send
    mov rdi, rbx
    lea rsi, [rel hub_send_buf]
    mov rdx, r13
    mov rax, SYS_WRITE
    syscall
    jmp .done

.error:
    mov eax, -1
.done:
    pop r13
    pop r12
    pop rbx
    leave
    ret

; ----------------------------------------------------------------------------
; hub_client_broadcast - Broadcast message to all hub clients
; rdi = message
; Returns: bytes sent or -1
; ----------------------------------------------------------------------------
hub_client_broadcast:
    push rdi
    xor rdi, rdi                ; target = NULL (broadcast)
    pop rsi                     ; message
    jmp hub_client_send

; ----------------------------------------------------------------------------
; hub_client_close - Close hub connection
; ----------------------------------------------------------------------------
hub_client_close:
    push rbp
    mov rbp, rsp

    mov edi, [rel hub_fd]
    cmp edi, -1
    je .done

    mov rax, SYS_CLOSE
    syscall

    mov dword [rel hub_fd], -1
    mov dword [rel hub_connected], 0
    mov dword [rel hub_registered], 0

.done:
    leave
    ret

; ----------------------------------------------------------------------------
; hub_client_get_fd - Get hub socket fd (for poll integration)
; Returns: fd or -1 if not connected
; ----------------------------------------------------------------------------
hub_client_get_fd:
    mov eax, [rel hub_fd]
    ret

; ----------------------------------------------------------------------------
; hub_extract_command - Extract command from "FROM <name>: <command>"
; rdi = input buffer
; rsi = output buffer for sender name (32 bytes)
; rdx = output buffer for command
; Returns: 1 if valid FROM message, 0 otherwise
; ----------------------------------------------------------------------------
hub_extract_command:
    push rbp
    mov rbp, rsp
    push rbx
    push r12
    push r13
    push r14

    mov r12, rdi                ; input
    mov r13, rsi                ; name output
    mov r14, rdx                ; command output

    ; Check for "FROM " prefix
    cmp dword [r12], 'FROM'
    jne .not_from
    cmp byte [r12+4], ' '
    jne .not_from

    ; Skip "FROM "
    add r12, 5

    ; Copy sender name until ':'
    mov rdi, r13
    mov ecx, 31
.copy_name:
    mov al, [r12]
    cmp al, ':'
    je .name_done
    cmp al, 0
    je .not_from
    cmp al, 10
    je .not_from
    stosb
    inc r12
    dec ecx
    jnz .copy_name
.name_done:
    mov byte [rdi], 0           ; null terminate name

    ; Skip ": "
    inc r12                     ; skip ':'
    cmp byte [r12], ' '
    jne .copy_cmd
    inc r12                     ; skip space

.copy_cmd:
    ; Copy command
    mov rdi, r14
.copy_command:
    mov al, [r12]
    cmp al, 0
    je .cmd_done
    cmp al, 10
    je .cmd_done
    cmp al, 13
    je .cmd_done
    stosb
    inc r12
    jmp .copy_command
.cmd_done:
    mov byte [rdi], 0           ; null terminate command

    mov eax, 1                  ; success
    jmp .done

.not_from:
    xor eax, eax
.done:
    pop r14
    pop r13
    pop r12
    pop rbx
    leave
    ret
