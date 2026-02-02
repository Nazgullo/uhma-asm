; mcp_server.asm — MCP Protocol Handler for Claude Code (replaces server.py)
;
; @entry _start                   ; main entry point
; @reads stdin (JSON-RPC from Claude Code)
; @writes stdout (JSON-RPC responses)
; @connects UHMA TCP ports 9997/9996 (query channel)
;
; PROTOCOL:
;   Claude Code sends JSON-RPC requests:
;     {"jsonrpc":"2.0","id":N,"method":"tools/call","params":{"name":"status","arguments":{}}}
;   Server responds:
;     {"jsonrpc":"2.0","id":N,"result":{"content":[{"type":"text","text":"..."}]}}
;
; TOOLS SUPPORTED:
;   Pass-through (just send command):
;     status, help, self, intro, why, misses, receipts, dream, observe,
;     compact, reset, presence, drives, metacog, debugger, genes,
;     subroutines, regions, hive, colony
;   Special handling:
;     input (wrap with ccmode)
;     quit (close connection)
;     raw (send raw command)
;
; ARCHITECTURE:
;   1. Read line from stdin (Content-Length header or raw JSON)
;   2. Parse JSON to extract method, id, name, arguments
;   3. Map name to UHMA command
;   4. Send command to UHMA via TCP (port 9997)
;   5. Read response from UHMA (port 9996)
;   6. Format as JSON-RPC response
;   7. Write to stdout
;
; GOTCHAS:
;   - MCP uses Content-Length framing OR raw JSON lines (detect at runtime)
;   - UHMA ports must be listening before server starts
;   - JSON parsing is minimal - just pattern matching for known fields
;   - Response timeout of 10 seconds
;
%include "syscalls.inc"

section .data
    ; JSON templates
    json_result_prefix:  db '{"jsonrpc":"2.0","id":', 0
    json_result_mid:     db ',"result":{"content":[{"type":"text","text":"', 0
    json_result_suffix:  db '"}]}}', 10, 0
    json_error_prefix:   db '{"jsonrpc":"2.0","id":', 0
    json_error_mid:      db ',"error":{"code":', 0
    json_error_msg:      db ',"message":"', 0
    json_error_suffix:   db '"}}', 10, 0

    ; MCP initialization response
    mcp_init_resp: db '{"jsonrpc":"2.0","id":1,"result":{"protocolVersion":"2024-11-05","capabilities":{"tools":{}},"serverInfo":{"name":"uhma","version":"1.0.0"}}}', 10, 0

    ; Tool list - all tools in one contiguous string (null terminated at end)
    tools_list_all: db '{"name":"input","description":"Send text to UHMA","inputSchema":{"type":"object","properties":{"text":{"type":"string"}},"required":["text"]}},'
                    db '{"name":"status","description":"Show system status","inputSchema":{"type":"object","properties":{}}},'
                    db '{"name":"help","description":"Show help","inputSchema":{"type":"object","properties":{}}},'
                    db '{"name":"self","description":"Show self-knowledge","inputSchema":{"type":"object","properties":{}}},'
                    db '{"name":"intro","description":"Show introspective state","inputSchema":{"type":"object","properties":{}}},'
                    db '{"name":"why","description":"Explain last miss","inputSchema":{"type":"object","properties":{}}},'
                    db '{"name":"misses","description":"Show recent misses","inputSchema":{"type":"object","properties":{"n":{"type":"integer"}}}},'
                    db '{"name":"dream","description":"Trigger dream cycle","inputSchema":{"type":"object","properties":{}}},'
                    db '{"name":"observe","description":"Trigger observation","inputSchema":{"type":"object","properties":{}}},'
                    db '{"name":"raw","description":"Send raw command","inputSchema":{"type":"object","properties":{"command":{"type":"string"}},"required":["command"]}},'
                    db '{"name":"mem_add","description":"Add to holographic memory","inputSchema":{"type":"object","properties":{"category":{"type":"string"},"content":{"type":"string"}},"required":["category","content"]}},'
                    db '{"name":"mem_query","description":"Query holographic memory","inputSchema":{"type":"object","properties":{"query":{"type":"string"}},"required":["query"]}}', 0
    tools_list_end: db ']}}', 10, 0

    ; Tool names (for matching)
    tool_input:     db "input", 0
    tool_status:    db "status", 0
    tool_help:      db "help", 0
    tool_self:      db "self", 0
    tool_intro:     db "intro", 0
    tool_why:       db "why", 0
    tool_misses:    db "misses", 0
    tool_dream:     db "dream", 0
    tool_observe:   db "observe", 0
    tool_compact:   db "compact", 0
    tool_reset:     db "reset", 0
    tool_raw:       db "raw", 0
    tool_quit:      db "quit", 0
    tool_presence:  db "presence", 0
    tool_drives:    db "drives", 0
    tool_metacog:   db "metacog", 0
    tool_genes:     db "genes", 0
    tool_regions:   db "regions", 0
    tool_hive:      db "hive", 0
    tool_colony:    db "colony", 0
    tool_mem_add:   db "mem_add", 0
    tool_mem_query: db "mem_query", 0

    ; UHMA command templates
    cmd_ccmode_on:  db "ccmode", 10, 0
    cmd_status:     db "status", 10, 0
    cmd_help:       db "help", 10, 0
    cmd_self:       db "self", 10, 0
    cmd_intro:      db "intro", 10, 0
    cmd_why:        db "why", 10, 0
    cmd_dream:      db "dream", 10, 0
    cmd_observe:    db "observe", 10, 0
    cmd_compact:    db "compact", 10, 0
    cmd_reset:      db "reset", 10, 0
    cmd_presence:   db "presence", 10, 0
    cmd_drives:     db "drives", 10, 0
    cmd_metacog:    db "metacog", 10, 0
    cmd_genes:      db "genes", 10, 0
    cmd_regions:    db "regions", 10, 0
    cmd_hive:       db "hive", 10, 0
    cmd_colony:     db "colony", 10, 0
    cmd_misses:     db "misses ", 0
    cmd_mem_add:    db "mem_add ", 0
    cmd_mem_query:  db "mem_query ", 0
    cmd_mem_state:  db "mem_state", 10, 0

    ; Ports
    QUERY_IN:       equ 9997
    QUERY_OUT:      equ 9996

    ; Error messages
    err_uhma:       db "Error: Cannot connect to UHMA", 0
    err_timeout:    db "Error: UHMA response timeout", 0
    err_parse:      db "Error: Failed to parse request", 0

    ; Debug log
    log_prefix:     db "[MCP] ", 0
    log_recv:       db "Received request", 10, 0
    log_send:       db "Sending to UHMA: ", 0
    log_resp:       db "Got response", 10, 0

    ; JSON pattern matching
    pat_method:     db '"method":', 0
    pat_id:         db '"id":', 0
    pat_name:       db '"name":', 0
    pat_text:       db '"text":', 0
    pat_command:    db '"command":', 0
    pat_query:      db '"query":', 0
    pat_category:   db '"category":', 0
    pat_content:    db '"content":', 0
    pat_n:          db '"n":', 0
    pat_init:       db "initialize", 0
    pat_list:       db "tools/list", 0
    pat_call:       db "tools/call", 0

section .bss
    ; Buffers
    read_buf:       resb 65536      ; input buffer
    send_buf:       resb 4096       ; command buffer
    resp_buf:       resb 65536      ; response buffer
    json_buf:       resb 131072     ; output JSON buffer

    ; Parsed request fields
    req_id:         resq 1          ; request ID
    req_method:     resq 1          ; pointer to method string
    req_name:       resq 1          ; pointer to tool name
    req_text:       resq 1          ; pointer to text argument
    req_command:    resq 1          ; pointer to command argument
    req_category:   resq 1          ; pointer to category argument
    req_content:    resq 1          ; pointer to content argument
    req_n:          resd 1          ; numeric argument

    ; Socket fds
    fd_query_in:    resd 1
    fd_query_out:   resd 1

    ; State
    use_content_length: resd 1      ; 1 = Content-Length framing

section .text

global _start

;; ============================================================
;; _start — Entry point
;; ============================================================
_start:
    ; Initialize
    mov dword [rel use_content_length], 1
    mov dword [rel fd_query_in], -1
    mov dword [rel fd_query_out], -1

    ; Connect to UHMA
    call connect_uhma
    test eax, eax
    jz .uhma_failed

    ; Main loop: read requests, process, respond
.main_loop:
    call read_request
    test eax, eax
    jz .eof

    call parse_request
    test eax, eax
    jz .parse_error

    call dispatch_request

    jmp .main_loop

.eof:
    xor edi, edi
    jmp exit

.uhma_failed:
    lea rdi, [rel err_uhma]
    call write_error
    mov edi, 1
    jmp exit

.parse_error:
    mov rdi, [rel req_id]
    lea rsi, [rel err_parse]
    call send_error_response
    jmp .main_loop

;; ============================================================
;; connect_uhma — Connect to UHMA query channel
;; Returns: eax = 1 on success, 0 on failure
;; ============================================================
connect_uhma:
    push rbx
    push r12
    sub rsp, 24

    ; Connect to QUERY_IN (9997)
    mov edi, QUERY_IN
    call connect_port
    test eax, eax
    js .conn_fail
    mov [rel fd_query_in], eax

    ; Connect to QUERY_OUT (9996)
    mov edi, QUERY_OUT
    call connect_port
    test eax, eax
    js .conn_fail
    mov [rel fd_query_out], eax

    mov eax, 1
    jmp .conn_ret

.conn_fail:
    xor eax, eax

.conn_ret:
    add rsp, 24
    pop r12
    pop rbx
    ret

;; ============================================================
;; connect_port — Connect to localhost:port
;; edi = port
;; Returns: eax = fd or -1
;; ============================================================
connect_port:
    push rbx
    push r12
    sub rsp, 24

    mov r12d, edi

    mov eax, SYS_SOCKET
    mov edi, AF_INET
    mov esi, SOCK_STREAM
    xor edx, edx
    syscall
    test eax, eax
    js .port_fail
    mov ebx, eax

    mov word [rsp], AF_INET
    mov eax, r12d
    xchg al, ah
    mov [rsp + 2], ax
    mov dword [rsp + 4], INADDR_LOOPBACK

    mov eax, SYS_CONNECT
    mov edi, ebx
    lea rsi, [rsp]
    mov edx, 16
    syscall
    test eax, eax
    js .port_close

    mov eax, ebx
    jmp .port_ret

.port_close:
    mov eax, SYS_CLOSE
    mov edi, ebx
    syscall

.port_fail:
    mov eax, -1

.port_ret:
    add rsp, 24
    pop r12
    pop rbx
    ret

;; ============================================================
;; read_request — Read JSON-RPC request from stdin
;; Returns: eax = bytes read, 0 on EOF
;; ============================================================
read_request:
    push rbx
    push r12
    sub rsp, 8

    ; Read a line from stdin (simple line-based JSON-RPC)
    lea rsi, [rel read_buf]
    xor r12d, r12d              ; total bytes read

.read_loop:
    mov eax, SYS_READ
    xor edi, edi                ; stdin
    lea rsi, [rel read_buf]
    add rsi, r12
    mov edx, 65536
    sub edx, r12d
    syscall
    test eax, eax
    jle .read_check_got

    add r12d, eax

    ; Check if we have a complete line (ends with newline)
    lea rdi, [rel read_buf]
    add rdi, r12
    dec rdi
    cmp byte [rdi], 10
    jne .read_loop              ; keep reading if no newline

    ; Remove trailing newline
    mov byte [rdi], 0

    mov eax, r12d
    jmp .read_ret

.read_check_got:
    ; EOF or error - return what we have
    mov eax, r12d
    test eax, eax
    jz .read_eof
    ; Null terminate
    lea rdi, [rel read_buf]
    add rdi, r12
    mov byte [rdi], 0
    jmp .read_ret

.read_eof:
    xor eax, eax

.read_ret:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; parse_request — Parse JSON-RPC request
;; Returns: eax = 1 on success, 0 on failure
;; ============================================================
parse_request:
    push rbx
    push r12
    push r13
    sub rsp, 8

    ; Clear parsed fields
    mov qword [rel req_id], 0
    mov qword [rel req_method], 0
    mov qword [rel req_name], 0
    mov qword [rel req_text], 0
    mov qword [rel req_command], 0
    mov qword [rel req_category], 0
    mov qword [rel req_content], 0
    mov dword [rel req_n], 10

    lea rbx, [rel read_buf]

    ; Find "id":
    lea rdi, [rel pat_id]
    mov rsi, rbx
    call find_pattern
    test rax, rax
    jz .parse_fail
    add rax, 5              ; skip "id":
    mov rdi, rax            ; parse_number expects rdi
    call parse_number
    mov [rel req_id], rax

    ; Find "method":
    lea rdi, [rel pat_method]
    mov rsi, rbx
    call find_pattern
    test rax, rax
    jz .parse_fail
    add rax, 10             ; skip "method":"
    mov [rel req_method], rax

    ; Check method type
    mov rsi, [rel req_method]

    ; Check for "initialize"
    lea rdi, [rel pat_init]
    call strncmp
    test eax, eax
    jz .method_init

    ; Check for "tools/list"
    mov rsi, [rel req_method]
    lea rdi, [rel pat_list]
    call strncmp
    test eax, eax
    jz .method_list

    ; Check for "tools/call"
    mov rsi, [rel req_method]
    lea rdi, [rel pat_call]
    call strncmp
    test eax, eax
    jz .method_call

    ; Unknown method
    jmp .parse_ok

.method_init:
    mov qword [rel req_name], 0
    jmp .parse_ok

.method_list:
    mov qword [rel req_name], 1
    jmp .parse_ok

.method_call:
    ; Find tool name
    lea rdi, [rel pat_name]
    mov rsi, rbx
    call find_pattern
    test rax, rax
    jz .parse_fail
    add rax, 8              ; skip "name":"
    mov [rel req_name], rax

    ; Try to find arguments
    ; text
    lea rdi, [rel pat_text]
    mov rsi, rbx
    call find_pattern
    test rax, rax
    jz .try_command
    add rax, 8              ; skip "text":"
    mov [rel req_text], rax

.try_command:
    lea rdi, [rel pat_command]
    mov rsi, rbx
    call find_pattern
    test rax, rax
    jz .try_query
    add rax, 11             ; skip "command":"
    mov [rel req_command], rax

.try_query:
    lea rdi, [rel pat_query]
    mov rsi, rbx
    call find_pattern
    test rax, rax
    jz .try_category
    add rax, 9              ; skip "query":"
    mov [rel req_text], rax

.try_category:
    lea rdi, [rel pat_category]
    mov rsi, rbx
    call find_pattern
    test rax, rax
    jz .try_content
    add rax, 12             ; skip "category":"
    mov [rel req_category], rax

.try_content:
    lea rdi, [rel pat_content]
    mov rsi, rbx
    call find_pattern
    test rax, rax
    jz .try_n
    add rax, 11             ; skip "content":"
    mov [rel req_content], rax

.try_n:
    lea rdi, [rel pat_n]
    mov rsi, rbx
    call find_pattern
    test rax, rax
    jz .parse_ok
    add rax, 4              ; skip "n":
    mov rdi, rax            ; parse_number expects rdi
    call parse_number
    mov [rel req_n], eax

.parse_ok:
    mov eax, 1
    jmp .parse_ret

.parse_fail:
    xor eax, eax

.parse_ret:
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; dispatch_request — Route request to handler
;; ============================================================
dispatch_request:
    push rbx
    push r12
    sub rsp, 8

    mov rax, [rel req_name]

    ; Check for initialize (req_name = 0)
    test rax, rax
    jz .do_init

    ; Check for tools/list (req_name = 1)
    cmp rax, 1
    je .do_list

    ; Tool call - match name
    mov rbx, rax

    ; status
    lea rdi, [rel tool_status]
    mov rsi, rbx
    call match_tool_name
    test eax, eax
    jnz .do_status

    ; help
    lea rdi, [rel tool_help]
    mov rsi, rbx
    call match_tool_name
    test eax, eax
    jnz .do_help

    ; self
    lea rdi, [rel tool_self]
    mov rsi, rbx
    call match_tool_name
    test eax, eax
    jnz .do_self

    ; intro
    lea rdi, [rel tool_intro]
    mov rsi, rbx
    call match_tool_name
    test eax, eax
    jnz .do_intro

    ; why
    lea rdi, [rel tool_why]
    mov rsi, rbx
    call match_tool_name
    test eax, eax
    jnz .do_why

    ; dream
    lea rdi, [rel tool_dream]
    mov rsi, rbx
    call match_tool_name
    test eax, eax
    jnz .do_dream

    ; observe
    lea rdi, [rel tool_observe]
    mov rsi, rbx
    call match_tool_name
    test eax, eax
    jnz .do_observe

    ; input
    lea rdi, [rel tool_input]
    mov rsi, rbx
    call match_tool_name
    test eax, eax
    jnz .do_input

    ; raw
    lea rdi, [rel tool_raw]
    mov rsi, rbx
    call match_tool_name
    test eax, eax
    jnz .do_raw

    ; misses
    lea rdi, [rel tool_misses]
    mov rsi, rbx
    call match_tool_name
    test eax, eax
    jnz .do_misses

    ; presence
    lea rdi, [rel tool_presence]
    mov rsi, rbx
    call match_tool_name
    test eax, eax
    jnz .do_presence

    ; drives
    lea rdi, [rel tool_drives]
    mov rsi, rbx
    call match_tool_name
    test eax, eax
    jnz .do_drives

    ; mem_add
    lea rdi, [rel tool_mem_add]
    mov rsi, rbx
    call match_tool_name
    test eax, eax
    jnz .do_mem_add

    ; mem_query
    lea rdi, [rel tool_mem_query]
    mov rsi, rbx
    call match_tool_name
    test eax, eax
    jnz .do_mem_query

    ; Unknown tool - return error
    jmp .dispatch_unknown

.do_init:
    ; Send initialize response
    lea rsi, [rel mcp_init_resp]
    call write_stdout
    jmp .dispatch_ret

.do_list:
    ; Build and send tools/list response
    call send_tools_list
    jmp .dispatch_ret

.do_status:
    lea rsi, [rel cmd_status]
    call uhma_command
    jmp .dispatch_ret

.do_help:
    lea rsi, [rel cmd_help]
    call uhma_command
    jmp .dispatch_ret

.do_self:
    lea rsi, [rel cmd_self]
    call uhma_command
    jmp .dispatch_ret

.do_intro:
    lea rsi, [rel cmd_intro]
    call uhma_command
    jmp .dispatch_ret

.do_why:
    lea rsi, [rel cmd_why]
    call uhma_command
    jmp .dispatch_ret

.do_dream:
    lea rsi, [rel cmd_dream]
    call uhma_command
    jmp .dispatch_ret

.do_observe:
    lea rsi, [rel cmd_observe]
    call uhma_command
    jmp .dispatch_ret

.do_presence:
    lea rsi, [rel cmd_presence]
    call uhma_command
    jmp .dispatch_ret

.do_drives:
    lea rsi, [rel cmd_drives]
    call uhma_command
    jmp .dispatch_ret

.do_input:
    ; Send ccmode then text
    lea rdi, [rel send_buf]
    mov rsi, [rel req_text]
    test rsi, rsi
    jz .dispatch_unknown
    call extract_json_string
    call uhma_input
    jmp .dispatch_ret

.do_raw:
    mov rsi, [rel req_command]
    test rsi, rsi
    jz .dispatch_unknown
    lea rdi, [rel send_buf]
    call extract_json_string
    ; Add newline
    lea rdi, [rel send_buf]
    call strlen
    lea rdi, [rel send_buf]
    add rdi, rax
    mov byte [rdi], 10
    mov byte [rdi + 1], 0
    lea rsi, [rel send_buf]
    call uhma_command
    jmp .dispatch_ret

.do_misses:
    ; Build "misses N" command
    lea rdi, [rel send_buf]
    lea rsi, [rel cmd_misses]
.copy_misses:
    lodsb
    test al, al
    jz .add_n
    stosb
    jmp .copy_misses
.add_n:
    mov eax, [rel req_n]
    call itoa
    mov byte [rdi], 10
    mov byte [rdi + 1], 0
    lea rsi, [rel send_buf]
    call uhma_command
    jmp .dispatch_ret

.do_mem_add:
    ; Build "mem_add <category> <content>" command
    lea rdi, [rel send_buf]
    lea rsi, [rel cmd_mem_add]
.copy_mem_add:
    lodsb
    test al, al
    jz .add_cat
    stosb
    jmp .copy_mem_add
.add_cat:
    ; Copy category (up to quote)
    mov rsi, [rel req_category]
    test rsi, rsi
    jz .add_space
.copy_cat:
    lodsb
    cmp al, '"'
    je .add_space
    test al, al
    jz .add_space
    stosb
    jmp .copy_cat
.add_space:
    mov byte [rdi], ' '
    inc rdi
    ; Copy content (up to quote)
    mov rsi, [rel req_content]
    test rsi, rsi
    jz .finish_mem_add
.copy_content:
    lodsb
    cmp al, '"'
    je .finish_mem_add
    test al, al
    jz .finish_mem_add
    stosb
    jmp .copy_content
.finish_mem_add:
    mov byte [rdi], 10
    mov byte [rdi + 1], 0
    lea rsi, [rel send_buf]
    call uhma_command
    jmp .dispatch_ret

.do_mem_query:
    ; Build "mem_query <query>" command
    lea rdi, [rel send_buf]
    lea rsi, [rel cmd_mem_query]
.copy_mem_query:
    lodsb
    test al, al
    jz .add_query
    stosb
    jmp .copy_mem_query
.add_query:
    ; Copy query text (from req_text which holds "query" value)
    mov rsi, [rel req_text]
    test rsi, rsi
    jz .finish_mem_query
.copy_query:
    lodsb
    cmp al, '"'
    je .finish_mem_query
    test al, al
    jz .finish_mem_query
    stosb
    jmp .copy_query
.finish_mem_query:
    mov byte [rdi], 10
    mov byte [rdi + 1], 0
    lea rsi, [rel send_buf]
    call uhma_command
    jmp .dispatch_ret

.dispatch_unknown:
    mov rdi, [rel req_id]
    lea rsi, [rel err_parse]
    call send_error_response

.dispatch_ret:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; drain_output — Drain any stale data from output channel
;; Non-blocking read to clear buffer
;; ============================================================
drain_output:
    push rbx
    sub rsp, 24

    ; Quick poll with 0 timeout (non-blocking check)
    mov eax, [rel fd_query_out]
    mov [rsp], eax
    mov word [rsp + 4], POLLIN
    mov word [rsp + 6], 0

.drain_loop:
    mov eax, SYS_POLL
    lea rdi, [rsp]
    mov esi, 1
    xor edx, edx            ; 0 timeout = non-blocking
    syscall

    test eax, eax
    jle .drain_done

    movzx eax, word [rsp + 6]
    test ax, POLLIN
    jz .drain_done

    ; Read and discard
    mov eax, SYS_READ
    mov edi, [rel fd_query_out]
    lea rsi, [rel resp_buf]
    mov edx, 65536
    syscall
    test eax, eax
    jg .drain_loop          ; keep draining if more data

.drain_done:
    add rsp, 24
    pop rbx
    ret

;; ============================================================
;; check_connection — Verify UHMA connection is still alive
;; Returns: eax = 1 if OK, 0 if needs reconnect
;; ============================================================
check_connection:
    ; Simple check - just verify fds are valid (not -1)
    ; Don't poll - that can give false positives on new connections
    mov eax, [rel fd_query_in]
    cmp eax, -1
    je .conn_bad
    mov eax, [rel fd_query_out]
    cmp eax, -1
    je .conn_bad
    mov eax, 1
    ret

.conn_bad:
    xor eax, eax
    ret

;; ============================================================
;; ensure_connection — Check connection, reconnect if needed
;; Returns: eax = 1 if connected, 0 if failed
;; ============================================================
ensure_connection:
    push rbx
    sub rsp, 8

    call check_connection
    test eax, eax
    jnz .already_ok

    ; Close old sockets if any
    mov edi, [rel fd_query_in]
    cmp edi, -1
    je .close_out
    mov eax, SYS_CLOSE
    syscall

.close_out:
    mov edi, [rel fd_query_out]
    cmp edi, -1
    je .reconnect
    mov eax, SYS_CLOSE
    syscall

.reconnect:
    mov dword [rel fd_query_in], -1
    mov dword [rel fd_query_out], -1

    ; Try to reconnect
    call connect_uhma
    jmp .ensure_ret

.already_ok:
    mov eax, 1

.ensure_ret:
    add rsp, 8
    pop rbx
    ret

;; ============================================================
;; uhma_command — Send command to UHMA, get response, format JSON
;; rsi = command string
;; ============================================================
uhma_command:
    push rbx
    push r12
    sub rsp, 8

    mov r12, rsi

    ; Ensure connection is valid
    call ensure_connection
    test eax, eax
    jz .uhma_cmd_fail

    ; Drain any stale data first
    call drain_output

    ; Send command - first get string length
    mov rdi, r12                ; strlen expects string in rdi
    call strlen
    mov rdx, rax                ; length for write
    mov rsi, r12                ; buffer for write
    mov eax, SYS_WRITE
    mov edi, [rel fd_query_in]  ; fd for write
    syscall

    ; Check write succeeded
    test eax, eax
    jle .uhma_cmd_fail

    ; Wait for response (poll with timeout)
    call read_uhma_response

    ; Format as JSON-RPC response
    call send_success_response
    jmp .uhma_cmd_ret

.uhma_cmd_fail:
    ; Return error response
    mov rdi, [rel req_id]
    lea rsi, [rel err_uhma]
    call send_error_response

.uhma_cmd_ret:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; uhma_input — Send input text to UHMA with ccmode
;; send_buf contains extracted text
;; ============================================================
uhma_input:
    push rbx
    sub rsp, 8

    ; Ensure connection is valid
    call ensure_connection
    test eax, eax
    jz .input_fail

    ; Enable ccmode
    mov edi, [rel fd_query_in]
    lea rsi, [rel cmd_ccmode_on]
    mov edx, 7
    mov eax, SYS_WRITE
    syscall

    ; Small delay
    sub rsp, 16
    mov qword [rsp], 0          ; 0 seconds
    mov qword [rsp + 8], 100000000  ; 100ms
    lea rdi, [rsp]
    xor esi, esi
    mov eax, SYS_NANOSLEEP
    syscall
    add rsp, 16

    ; Send the text
    lea rdi, [rel send_buf]
    call strlen
    mov rdx, rax
    lea rsi, [rel send_buf]
    mov eax, SYS_WRITE
    mov edi, [rel fd_query_in]
    syscall

    ; Send newline
    mov eax, SYS_WRITE
    mov edi, [rel fd_query_in]
    lea rsi, [rel json_buf]
    mov byte [rsi], 10
    mov edx, 1
    syscall

    ; Read response
    call read_uhma_response
    call send_success_response
    jmp .input_ret

.input_fail:
    mov rdi, [rel req_id]
    lea rsi, [rel err_uhma]
    call send_error_response

.input_ret:
    add rsp, 8
    pop rbx
    ret

;; ============================================================
;; read_uhma_response — Read response from UHMA output port
;; Result in resp_buf
;; ============================================================
read_uhma_response:
    push rbx
    push r12
    push r13
    sub rsp, 32

    ; Clear resp_buf first (first 256 bytes is enough for null detection)
    lea rdi, [rel resp_buf]
    xor eax, eax
    mov ecx, 256
    rep stosb

    xor r12d, r12d          ; total bytes read
    xor r13d, r13d          ; consecutive empty polls

    ; Setup pollfd struct
    mov eax, [rel fd_query_out]
    mov [rsp], eax          ; fd
    mov word [rsp + 4], POLLIN  ; events
    mov word [rsp + 6], 0   ; revents

.read_loop:
    ; Reset revents before poll
    mov word [rsp + 6], 0

    ; Poll with 2 second timeout (shorter, loop more)
    mov eax, SYS_POLL
    lea rdi, [rsp]
    mov esi, 1
    mov edx, 2000           ; 2 seconds
    syscall

    test eax, eax
    jl .read_done           ; error
    jz .poll_timeout        ; timeout

    ; Check revents
    movzx eax, word [rsp + 6]
    test ax, POLLIN
    jz .poll_timeout

    ; Read available data
    mov eax, SYS_READ
    mov edi, [rel fd_query_out]
    lea rsi, [rel resp_buf]
    add rsi, r12
    mov edx, 65536
    sub edx, r12d
    syscall
    test eax, eax
    jle .read_done

    add r12d, eax
    xor r13d, r13d          ; reset timeout counter on successful read

    ; Check for prompt "uhma> " at end of response
    cmp r12d, 6
    jl .read_loop

    lea rdi, [rel resp_buf]
    add rdi, r12
    sub rdi, 6
    ; Check "uhma> " byte by byte
    cmp byte [rdi], 'u'
    jne .read_loop
    cmp byte [rdi + 1], 'h'
    jne .read_loop
    cmp byte [rdi + 2], 'm'
    jne .read_loop
    cmp byte [rdi + 3], 'a'
    jne .read_loop
    cmp byte [rdi + 4], '>'
    jne .read_loop
    ; Found prompt, we're done
    jmp .read_done

.poll_timeout:
    ; After 3 consecutive timeouts with data, assume response complete
    inc r13d
    cmp r13d, 3
    jl .read_loop
    ; If we have any data, consider it complete
    test r12d, r12d
    jnz .read_done
    ; No data after 6 seconds - continue waiting up to 15 seconds total
    cmp r13d, 8
    jl .read_loop

.read_done:
    ; Null terminate
    lea rdi, [rel resp_buf]
    mov byte [rdi + r12], 0

    ; Strip trailing prompt if present
    cmp r12d, 6
    jl .read_ret
    lea rdi, [rel resp_buf]
    add rdi, r12
    sub rdi, 6
    cmp byte [rdi], 'u'
    jne .read_ret
    cmp byte [rdi + 4], '>'
    jne .read_ret
    ; Remove prompt from response
    mov byte [rdi], 0
    sub r12d, 6

.read_ret:
    add rsp, 32
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; send_success_response — Format JSON-RPC success response
;; resp_buf contains UHMA output
;; ============================================================
send_success_response:
    push rbx
    push r12
    sub rsp, 8

    lea rdi, [rel json_buf]

    ; {"jsonrpc":"2.0","id":
    lea rsi, [rel json_result_prefix]
.copy_prefix:
    lodsb
    test al, al
    jz .add_id
    stosb
    jmp .copy_prefix

.add_id:
    mov rax, [rel req_id]
    call itoa_inline

    ; ,"result":{"content":[{"type":"text","text":"
    lea rsi, [rel json_result_mid]
.copy_mid:
    lodsb
    test al, al
    jz .add_text
    stosb
    jmp .copy_mid

.add_text:
    ; Escape and copy response text
    lea rsi, [rel resp_buf]
    call escape_json_string

    ; "}]}}
    lea rsi, [rel json_result_suffix]
.copy_suffix:
    lodsb
    stosb
    test al, al
    jnz .copy_suffix

    ; Write to stdout
    lea rsi, [rel json_buf]
    call write_stdout

    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; send_error_response — Format JSON-RPC error response
;; rdi = id, rsi = error message
;; ============================================================
send_error_response:
    push rbx
    push r12
    sub rsp, 8

    mov rbx, rdi            ; id
    mov r12, rsi            ; message

    lea rdi, [rel json_buf]

    ; {"jsonrpc":"2.0","id":
    lea rsi, [rel json_error_prefix]
.copy_err_prefix:
    lodsb
    test al, al
    jz .add_err_id
    stosb
    jmp .copy_err_prefix

.add_err_id:
    mov rax, rbx
    call itoa_inline

    ; ,"error":{"code":
    lea rsi, [rel json_error_mid]
.copy_err_mid:
    lodsb
    test al, al
    jz .add_code
    stosb
    jmp .copy_err_mid

.add_code:
    mov eax, -32000
    call itoa_inline

    ; ,"message":"
    lea rsi, [rel json_error_msg]
.copy_msg:
    lodsb
    test al, al
    jz .add_err_text
    stosb
    jmp .copy_msg

.add_err_text:
    mov rsi, r12
    call escape_json_string

    ; "}}
    lea rsi, [rel json_error_suffix]
.copy_err_suffix:
    lodsb
    stosb
    test al, al
    jnz .copy_err_suffix

    lea rsi, [rel json_buf]
    call write_stdout

    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; send_tools_list — Send tools/list response
;; ============================================================
send_tools_list:
    push rbx
    sub rsp, 8

    ; Build response in json_buf
    lea rdi, [rel json_buf]

    ; Prefix with id
    lea rsi, [rel json_result_prefix]
.tl_copy1:
    lodsb
    test al, al
    jz .tl_id
    stosb
    jmp .tl_copy1

.tl_id:
    mov rax, [rel req_id]
    call itoa_inline

    ; ,"result":{"tools":[
    mov byte [rdi], ','
    inc rdi
    mov dword [rdi], '"res'
    add rdi, 4
    mov dword [rdi], 'ult"'
    add rdi, 4
    mov dword [rdi], ':{"t'
    add rdi, 4
    mov dword [rdi], 'ools'
    add rdi, 4
    mov word [rdi], '":'
    add rdi, 2
    mov byte [rdi], '['
    inc rdi

    ; Copy all tool definitions
    lea rsi, [rel tools_list_all]
.tl_copy_tools:
    lodsb
    test al, al
    jz .tl_close
    stosb
    jmp .tl_copy_tools

.tl_close:
    lea rsi, [rel tools_list_end]
.tl_close_loop:
    lodsb
    stosb
    test al, al
    jnz .tl_close_loop

    lea rsi, [rel json_buf]
    call write_stdout

    add rsp, 8
    pop rbx
    ret

;; ============================================================
;; Helper functions
;; ============================================================

; write_stdout - rsi = string
write_stdout:
    push rbx
    mov rbx, rsi
    mov rdi, rsi            ; strlen expects rdi
    call strlen
    mov edx, eax
    mov eax, SYS_WRITE
    mov edi, STDOUT
    mov rsi, rbx
    syscall
    pop rbx
    ret

; write_error - rdi = string (write to stderr)
write_error:
    push rbx
    mov rbx, rdi
    ; rdi already correct for strlen
    call strlen
    mov edx, eax
    mov eax, SYS_WRITE
    mov edi, STDERR
    mov rsi, rbx
    syscall
    pop rbx
    ret

; strlen - rdi = string, returns eax
strlen:
    push rdi
    xor ecx, ecx
.len_loop:
    mov al, [rdi + rcx]
    test al, al
    jz .len_done
    inc ecx
    jmp .len_loop
.len_done:
    mov eax, ecx
    pop rdi
    ret

; find_pattern - rdi = pattern, rsi = haystack
; Returns: rax = pointer to match or 0
find_pattern:
    push rbx
    push r12
    mov r12, rdi            ; pattern
    mov rbx, rsi            ; haystack

.find_loop:
    mov al, [rbx]
    test al, al
    jz .find_notfound

    ; Compare pattern
    mov rdi, r12
    mov rsi, rbx
.cmp_loop:
    mov al, [rdi]
    test al, al
    jz .find_found
    mov cl, [rsi]
    cmp al, cl
    jne .find_next
    inc rdi
    inc rsi
    jmp .cmp_loop

.find_next:
    inc rbx
    jmp .find_loop

.find_found:
    mov rax, rbx
    jmp .find_ret

.find_notfound:
    xor eax, eax

.find_ret:
    pop r12
    pop rbx
    ret

; parse_number - rdi = string, returns rax
parse_number:
    xor eax, eax
    mov ecx, 10
.num_loop:
    movzx edx, byte [rdi]
    cmp dl, '0'
    jb .num_done
    cmp dl, '9'
    ja .num_done
    sub dl, '0'
    imul eax, ecx
    add eax, edx
    inc rdi
    jmp .num_loop
.num_done:
    ret

; strncmp - compare until quote or end
; rdi = str1, rsi = str2
; Returns: eax = 0 if match
strncmp:
    xor eax, eax
.cmp:
    mov cl, [rdi]
    mov dl, [rsi]
    cmp cl, '"'
    je .cmp_end
    test cl, cl
    jz .cmp_end
    cmp cl, dl
    jne .cmp_diff
    inc rdi
    inc rsi
    jmp .cmp
.cmp_end:
    xor eax, eax
    ret
.cmp_diff:
    mov eax, 1
    ret

; match_tool_name - check if tool name matches
; rdi = expected name, rsi = json string (starts at first char after quote)
; Returns: eax = 1 if match
match_tool_name:
    push rbx
.match_loop:
    mov al, [rdi]
    test al, al
    jz .match_check_end
    mov cl, [rsi]
    cmp al, cl
    jne .match_no
    inc rdi
    inc rsi
    jmp .match_loop
.match_check_end:
    ; Expected name ended - check if JSON string also ends
    mov al, [rsi]
    cmp al, '"'
    jne .match_no
    mov eax, 1
    jmp .match_ret
.match_no:
    xor eax, eax
.match_ret:
    pop rbx
    ret

; extract_json_string - copy string from JSON, stop at quote
; rdi = dest, rsi = src (points to first char after opening quote)
extract_json_string:
    push rbx
    mov rbx, rdi
.extract_loop:
    mov al, [rsi]
    cmp al, '"'
    je .extract_done
    test al, al
    jz .extract_done
    cmp al, '\'
    jne .extract_store
    ; Handle escape
    inc rsi
    mov al, [rsi]
    cmp al, 'n'
    jne .check_t
    mov al, 10
    jmp .extract_store
.check_t:
    cmp al, 't'
    jne .check_r
    mov al, 9
    jmp .extract_store
.check_r:
    cmp al, 'r'
    jne .extract_store
    mov al, 13
.extract_store:
    mov [rdi], al
    inc rdi
    inc rsi
    jmp .extract_loop
.extract_done:
    mov byte [rdi], 0
    pop rbx
    ret

; escape_json_string - copy and escape for JSON
; rdi = dest, rsi = src
escape_json_string:
    push rbx
.esc_loop:
    mov al, [rsi]
    test al, al
    jz .esc_done

    ; Check for chars that need escaping
    cmp al, '"'
    je .esc_quote
    cmp al, '\'
    je .esc_backslash
    cmp al, 10
    je .esc_newline
    cmp al, 13
    je .esc_cr
    cmp al, 9
    je .esc_tab
    cmp al, 32
    jb .esc_skip         ; skip control chars

    ; Normal char
    stosb
    inc rsi
    jmp .esc_loop

.esc_quote:
    mov byte [rdi], '\'
    inc rdi
    mov byte [rdi], '"'
    inc rdi
    inc rsi
    jmp .esc_loop

.esc_backslash:
    mov byte [rdi], '\'
    inc rdi
    mov byte [rdi], '\'
    inc rdi
    inc rsi
    jmp .esc_loop

.esc_newline:
    mov byte [rdi], '\'
    inc rdi
    mov byte [rdi], 'n'
    inc rdi
    inc rsi
    jmp .esc_loop

.esc_cr:
    mov byte [rdi], '\'
    inc rdi
    mov byte [rdi], 'r'
    inc rdi
    inc rsi
    jmp .esc_loop

.esc_tab:
    mov byte [rdi], '\'
    inc rdi
    mov byte [rdi], 't'
    inc rdi
    inc rsi
    jmp .esc_loop

.esc_skip:
    inc rsi
    jmp .esc_loop

.esc_done:
    pop rbx
    ret

; itoa_inline - write number to [rdi], advance rdi
; rax = number
itoa_inline:
    push rbx
    push r12

    mov r12, rdi            ; save start
    mov rbx, rax
    test rbx, rbx
    jns .itoa_pos

    ; Negative
    mov byte [rdi], '-'
    inc rdi
    neg rbx

.itoa_pos:
    ; Count digits
    mov rax, rbx
    xor ecx, ecx
    mov r8d, 10
.itoa_count:
    inc ecx
    xor edx, edx
    div r8
    test rax, rax
    jnz .itoa_count

    ; Write digits in reverse
    add rdi, rcx
    mov r12, rdi            ; save end
    mov rax, rbx
.itoa_write:
    dec rdi
    xor edx, edx
    div r8
    add dl, '0'
    mov [rdi], dl
    test rax, rax
    jnz .itoa_write

    mov rdi, r12            ; restore to end
    pop r12
    pop rbx
    ret

; itoa - write number at [rdi], return pointer past end in rdi
; eax = number
itoa:
    push rbx
    mov ebx, eax
    mov eax, ebx
    jmp itoa_inline
    pop rbx
    ret

; atoi - parse number from [rdi]
; Returns: eax
atoi:
    xor eax, eax
    mov ecx, 10
.atoi_loop:
    movzx edx, byte [rdi]
    cmp dl, '0'
    jb .atoi_done
    cmp dl, '9'
    ja .atoi_done
    sub dl, '0'
    imul eax, ecx
    add eax, edx
    inc rdi
    jmp .atoi_loop
.atoi_done:
    ret

; exit - edi = code
exit:
    mov eax, SYS_EXIT
    syscall
