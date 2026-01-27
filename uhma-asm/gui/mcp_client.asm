; mcp_client.asm — MCP server communication for GUI via TCP
;
; Connects to MCP server via TCP socket (port 9999). If server not running,
; spawns it in --tcp mode. GUI calls these instead of UHMA directly.
;
; ARCHITECTURE:
;   GUI (this) ──(TCP:9999)──→ MCP Server ──→ UHMA (subprocess)
;                                  ↑
;   Claude Code ──────(stdio)──────┘
;
; Both GUI and Claude Code share the same UHMA instance through MCP.
;
; @entry mcp_init() -> eax=1 on success, 0 on failure
; @entry mcp_shutdown()
; @entry mcp_call(rdi=tool_name, rsi=args_json) -> rax=response_ptr
; @entry mcp_send_text(rdi=text) -> rax=response_ptr
; @entry mcp_get_status() -> rax=status_json_ptr
;
; FLOW: mcp_init connects to TCP:9999, mcp_call sends JSON-RPC, parses response
;
; GOTCHAS:
;   - Response buffer is static, overwritten each call
;   - Uses raw JSON lines over TCP (no Content-Length)
;   - MCP server spawns UHMA internally
;   - Stack alignment (x86-64 ABI): ODD pushes → aligned → sub must be multiple of 16

section .data
    ; MCP server command (for spawning if not running)
    mcp_cmd:        db "python3", 0
    mcp_arg1:       db "../tools/rag/server.py", 0
    mcp_arg2:       db "--tcp", 0
    mcp_arg3:       db "9999", 0
    mcp_argv:       dq 0, 0, 0, 0, 0  ; filled at runtime

    ; TCP connection params
    MCP_PORT        equ 9999
    AF_INET         equ 2
    SOCK_STREAM     equ 1

    ; JSON-RPC templates (raw JSON, no Content-Length for TCP)
    json_init:      db '{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"uhma-gui"}},"id":1}', 10, 0
    json_call_pre:  db '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"', 0
    json_call_mid:  db '","arguments":{', 0
    json_call_post: db '}},"id":', 0
    json_input_arg: db '"text":"', 0

    ; Format strings
    fmt_int:        db "%d", 0

    ; Error messages
    err_connect:    db "MCP: connect failed, spawning server...", 10, 0
    err_spawn:      db "MCP: spawn failed", 10, 0
    err_socket:     db "MCP: socket failed", 10, 0
    msg_connected:  db "MCP: connected to server", 10, 0

section .bss
    ; TCP socket
    mcp_socket:     resd 1
    mcp_running:    resd 1
    mcp_pid:        resd 1
    call_id:        resd 1

    ; sockaddr_in structure (16 bytes)
    sock_addr:      resb 16

    ; Request/response buffers
    req_buf:        resb 4096
    resp_buf:       resb 65536
    status_cache:   resb 8192

section .text

; Libc functions
extern socket
extern connect
extern send
extern recv
extern close
extern fork
extern execvp
extern waitpid
extern kill
extern usleep
extern printf
extern sprintf
extern strlen
extern memset
extern memcpy
extern strcpy
extern strcat
extern htons

global mcp_init
global mcp_shutdown
global mcp_call
global mcp_send_text
global mcp_get_status
global mcp_dream
global mcp_observe
global mcp_evolve
global mcp_save
global mcp_load

;; mcp_init — Connect to MCP server via TCP
;; Returns: eax=1 success, 0 failure
mcp_init:
    push rbx
    push r12
    push r13
    sub rsp, 16

    ; Try to connect first (server may already be running via Claude Code)
    call .try_connect
    test eax, eax
    jnz .connected

    ; Connection failed - spawn server
    lea rdi, [rel err_connect]
    xor eax, eax
    call printf

    call .spawn_server
    test eax, eax
    jz .spawn_fail

    ; Wait for server to start
    mov edi, 1500000        ; 1.5s
    call usleep

    ; Try to connect again
    call .try_connect
    test eax, eax
    jz .spawn_fail

.connected:
    ; Send initialize request
    call .send_init

    mov dword [rel mcp_running], 1
    mov dword [rel call_id], 2

    lea rdi, [rel msg_connected]
    xor eax, eax
    call printf

    mov eax, 1
    jmp .done

.spawn_fail:
    lea rdi, [rel err_spawn]
    xor eax, eax
    call printf
    xor eax, eax
    jmp .done

.done:
    add rsp, 16
    pop r13
    pop r12
    pop rbx
    ret

;; Try to connect to TCP server
;; Returns: eax=1 success, 0 failure
.try_connect:
    push rbx
    sub rsp, 16

    ; Create socket
    mov edi, AF_INET
    mov esi, SOCK_STREAM
    xor edx, edx
    call socket
    cmp eax, -1
    je .conn_fail
    mov [rel mcp_socket], eax
    mov ebx, eax

    ; Setup sockaddr_in
    lea rdi, [rel sock_addr]
    xor esi, esi
    mov edx, 16
    call memset

    mov word [rel sock_addr], AF_INET       ; sin_family
    mov edi, MCP_PORT
    call htons
    mov [rel sock_addr + 2], ax             ; sin_port
    mov dword [rel sock_addr + 4], 0x0100007f  ; 127.0.0.1 in network order

    ; Connect
    mov edi, ebx
    lea rsi, [rel sock_addr]
    mov edx, 16
    call connect
    test eax, eax
    jnz .conn_close_fail

    mov eax, 1
    jmp .conn_done

.conn_close_fail:
    mov edi, [rel mcp_socket]
    call close
.conn_fail:
    xor eax, eax
.conn_done:
    add rsp, 16
    pop rbx
    ret

;; Spawn MCP server in TCP-only mode
;; Returns: eax=1 success, 0 failure
.spawn_server:
    push rbx
    sub rsp, 16

    call fork
    cmp eax, -1
    je .spawn_err
    test eax, eax
    jz .child

    ; Parent: save PID
    mov [rel mcp_pid], eax
    mov eax, 1
    jmp .spawn_done

.child:
    ; Build argv: python3 server.py --tcp 9999
    lea rax, [rel mcp_cmd]
    mov [rel mcp_argv], rax
    lea rax, [rel mcp_arg1]
    mov [rel mcp_argv + 8], rax
    lea rax, [rel mcp_arg2]
    mov [rel mcp_argv + 16], rax
    lea rax, [rel mcp_arg3]
    mov [rel mcp_argv + 24], rax
    mov qword [rel mcp_argv + 32], 0

    ; Exec
    lea rdi, [rel mcp_cmd]
    lea rsi, [rel mcp_argv]
    call execvp

    ; If we get here, exec failed
    mov edi, 1
    mov eax, 60             ; sys_exit
    syscall

.spawn_err:
    xor eax, eax
.spawn_done:
    add rsp, 16
    pop rbx
    ret

;; Send initialize JSON-RPC
.send_init:
    push rbx
    sub rsp, 16

    ; Send init request (raw JSON line)
    lea rdi, [rel json_init]
    call strlen
    mov rdx, rax            ; length
    mov edi, [rel mcp_socket]
    lea rsi, [rel json_init]
    xor ecx, ecx            ; flags = 0
    call send

    ; Read response (discard for init)
    mov edi, [rel mcp_socket]
    lea rsi, [rel resp_buf]
    mov edx, 65535
    xor ecx, ecx
    call recv

    add rsp, 16
    pop rbx
    ret

;; mcp_shutdown — Close TCP connection, optionally kill server
mcp_shutdown:
    push rbx
    sub rsp, 16

    cmp dword [rel mcp_running], 0
    je .done

    ; Send quit command first
    lea rdi, [rel .quit_tool]
    xor esi, esi
    call mcp_call

    ; Close socket
    mov edi, [rel mcp_socket]
    call close

    ; Kill spawned server if we started one
    mov edi, [rel mcp_pid]
    test edi, edi
    jz .no_kill
    mov esi, 15             ; SIGTERM
    call kill
    mov edi, [rel mcp_pid]
    xor esi, esi
    xor edx, edx
    call waitpid
.no_kill:

    mov dword [rel mcp_running], 0

.done:
    add rsp, 16
    pop rbx
    ret

.quit_tool: db "quit", 0

;; mcp_call — Call MCP tool via TCP
;; rdi = tool name (C string)
;; rsi = arguments JSON (C string, can be empty or null)
;; Returns: rax = pointer to response text (in resp_buf)
mcp_call:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 8

    mov r12, rdi            ; tool name
    mov r13, rsi            ; args

    ; Clear request buffer
    lea rdi, [rel req_buf]
    xor esi, esi
    mov edx, 4096
    call memset

    ; Build JSON directly (no Content-Length for TCP, just raw JSON + newline)
    ; {"jsonrpc":"2.0","method":"tools/call","params":{"name":"TOOL","arguments":{ARGS}},"id":N}\n
    lea rdi, [rel req_buf]
    lea rsi, [rel json_call_pre]
    call strcpy

    lea rdi, [rel req_buf]
    mov rsi, r12
    call strcat

    lea rdi, [rel req_buf]
    lea rsi, [rel json_call_mid]
    call strcat

    ; Add arguments if provided
    test r13, r13
    jz .no_args
    cmp byte [r13], 0
    je .no_args

    lea rdi, [rel req_buf]
    mov rsi, r13
    call strcat

.no_args:
    lea rdi, [rel req_buf]
    lea rsi, [rel json_call_post]
    call strcat

    ; Add call ID
    lea rdi, [rel req_buf]
    call strlen
    lea rdi, [rel req_buf + rax]
    lea rsi, [rel fmt_int]
    mov edx, [rel call_id]
    xor eax, eax
    call sprintf

    ; Close JSON and add newline
    lea rdi, [rel req_buf]
    call strlen
    lea rdi, [rel req_buf + rax]
    mov byte [rdi], '}'
    mov byte [rdi + 1], 10      ; newline
    mov byte [rdi + 2], 0

    inc dword [rel call_id]

    ; Send request via TCP
    lea rdi, [rel req_buf]
    call strlen
    mov rdx, rax                ; length
    mov edi, [rel mcp_socket]
    lea rsi, [rel req_buf]
    xor ecx, ecx                ; flags = 0
    call send

    ; Read response via TCP
    mov edi, [rel mcp_socket]
    lea rsi, [rel resp_buf]
    mov edx, 65535
    xor ecx, ecx
    call recv
    mov r14, rax                ; bytes read

    ; Null terminate response
    lea rdi, [rel resp_buf]
    mov byte [rdi + r14], 0

    ; Find "text":" in response and extract value
    lea rax, [rel resp_buf]
    mov rbx, rax

.find_text:
    cmp byte [rbx], 0
    je .not_found
    cmp dword [rbx], 0x74786574     ; "text" little endian = "txet"
    je .check_text
    inc rbx
    jmp .find_text

.check_text:
    ; Check for "text":"
    cmp byte [rbx + 4], '"'
    jne .next_char
    cmp byte [rbx + 5], ':'
    jne .next_char
    cmp byte [rbx + 6], '"'
    jne .next_char

    ; Found it, extract string
    lea rax, [rbx + 7]      ; start of text content
    mov rbx, rax

.find_end:
    cmp byte [rbx], '"'
    je .found_end
    cmp byte [rbx], 0
    je .not_found
    ; Handle escaped quotes
    cmp byte [rbx], '\'
    jne .next_text_char
    inc rbx                 ; skip escaped char
.next_text_char:
    inc rbx
    jmp .find_end

.found_end:
    mov byte [rbx], 0       ; null terminate
    jmp .done

.next_char:
    inc rbx
    jmp .find_text

.not_found:
    lea rax, [rel resp_buf]

.done:
    add rsp, 8
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; mcp_send_text — Send text input to UHMA
;; rdi = text to process
;; Returns: rax = response pointer
mcp_send_text:
    push rbx
    push r12
    sub rsp, 8

    mov r12, rdi

    ; Build args: "text":"VALUE"
    lea rdi, [rel status_cache]
    lea rsi, [rel json_input_arg]
    call strcpy

    lea rdi, [rel status_cache]
    mov rsi, r12
    call strcat

    ; Close quote
    lea rdi, [rel status_cache]
    call strlen
    lea rdi, [rel status_cache + rax]
    mov byte [rdi], '"'
    mov byte [rdi + 1], 0

    lea rdi, [rel .input_tool]
    lea rsi, [rel status_cache]
    call mcp_call

    add rsp, 8
    pop r12
    pop rbx
    ret

.input_tool: db "input", 0

;; Convenience wrappers for common commands
mcp_dream:
    lea rdi, [rel .dream]
    xor esi, esi
    jmp mcp_call
.dream: db "dream", 0

mcp_observe:
    lea rdi, [rel .observe]
    xor esi, esi
    jmp mcp_call
.observe: db "observe", 0

mcp_evolve:
    ; No evolve in MCP tools, use raw
    lea rdi, [rel .raw]
    lea rsi, [rel .evolve_arg]
    jmp mcp_call
.raw: db "raw", 0
.evolve_arg: db '"command":"evolve"', 0

mcp_save:
    lea rdi, [rel .save]
    xor esi, esi
    jmp mcp_call
.save: db "save", 0

mcp_load:
    lea rdi, [rel .load]
    xor esi, esi
    jmp mcp_call
.load: db "load", 0

;; mcp_get_status — Get UHMA status
;; Returns: rax = pointer to status text
mcp_get_status:
    lea rdi, [rel .status]
    xor esi, esi
    call mcp_call
    ret
.status: db "status", 0
