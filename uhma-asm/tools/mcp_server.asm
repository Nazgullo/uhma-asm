; mcp_server.asm — MCP Protocol Handler for Claude Code
;
; @entry _start                   ; main entry point
; @reads stdin (JSON-RPC from Claude Code)
; @writes stdout (JSON-RPC responses)
; @connects UHMA TCP gateway port 9999 (framed, single socket)
;
; PROTOCOL:
;   Claude Code sends JSON-RPC requests:
;     {"jsonrpc":"2.0","id":N,"method":"tools/call","params":{"name":"status","arguments":{}}}
;   Server responds:
;     {"jsonrpc":"2.0","id":N,"result":{"content":[{"type":"text","text":"..."}]}}
;
; TOOLS SUPPORTED:
;   Pass-through (just send command):
;     status, help, self, intro, why, misses, dream, observe,
;     presence, drives, metacog, genes, regions, hive, colony,
;     compact, reset
;   Special handling:
;     input (wrap with ccmode)
;     raw (send raw command)
;     mem_rag_refresh (rebuild code RAG in holographic memory)
;
; ARCHITECTURE:
;   1. Read line from stdin (Content-Length header or raw JSON)
;   2. Parse JSON to extract method, id, name, arguments
;   3. Map name to UHMA command
;   4. Send framed command to UHMA gateway (port 9999)
;   5. Read framed response from UHMA gateway (same socket)
;   6. Format as JSON-RPC response
;   7. Write to stdout
;
; GOTCHAS:
;   - MCP uses Content-Length framing OR raw JSON lines (detect at runtime)
;   - UHMA gateway must be listening before server starts
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
                    db '{"name":"presence","description":"Show presence field","inputSchema":{"type":"object","properties":{}}},'
                    db '{"name":"drives","description":"Show drive levels","inputSchema":{"type":"object","properties":{}}},'
                    db '{"name":"metacog","description":"Show metacognitive state","inputSchema":{"type":"object","properties":{}}},'
                    db '{"name":"genes","description":"Show gene pool status","inputSchema":{"type":"object","properties":{}}},'
                    db '{"name":"regions","description":"List regions with hit/miss stats","inputSchema":{"type":"object","properties":{}}},'
                    db '{"name":"hive","description":"Show hive pheromone levels","inputSchema":{"type":"object","properties":{}}},'
                    db '{"name":"colony","description":"Show colony status","inputSchema":{"type":"object","properties":{}}},'
                    db '{"name":"compact","description":"Compact condemned regions","inputSchema":{"type":"object","properties":{}}},'
                    db '{"name":"reset","description":"Reset counters (not knowledge)","inputSchema":{"type":"object","properties":{}}},'
                    db '{"name":"raw","description":"Send raw command","inputSchema":{"type":"object","properties":{"command":{"type":"string"}},"required":["command"]}},'
                    db '{"name":"mem_add","description":"Add to holographic memory","inputSchema":{"type":"object","properties":{"category":{"type":"string"},"content":{"type":"string"},"context":{"type":"string"}},"required":["category","content"]}},'
                    db '{"name":"mem_query","description":"Query holographic memory","inputSchema":{"type":"object","properties":{"query":{"type":"string"},"limit":{"type":"integer"}},"required":["query"]}},'
                    db '{"name":"mem_state","description":"Get memory cognitive state","inputSchema":{"type":"object","properties":{}}},'
                    db '{"name":"mem_recent","description":"Get recent memory entries","inputSchema":{"type":"object","properties":{"limit":{"type":"integer"}}}},'
                    db '{"name":"mem_summary","description":"Get memory summary","inputSchema":{"type":"object","properties":{}}},'
                    db '{"name":"mem_rag_refresh","description":"Rebuild code RAG entries","inputSchema":{"type":"object","properties":{}}}', 0
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
    tool_presence:  db "presence", 0
    tool_drives:    db "drives", 0
    tool_metacog:   db "metacog", 0
    tool_genes:     db "genes", 0
    tool_regions:   db "regions", 0
    tool_hive:      db "hive", 0
    tool_colony:    db "colony", 0
    tool_mem_add:   db "mem_add", 0
    tool_mem_query: db "mem_query", 0
    tool_mem_state: db "mem_state", 0
    tool_mem_recent: db "mem_recent", 0
    tool_mem_summary: db "mem_summary", 0
    tool_mem_rag_refresh: db "mem_rag_refresh", 0

    ; UHMA command templates
    cmd_ccmode_on:  db "ccmode on", 10, 0
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

    ; Gateway port (single connection)
    GW_PORT:        equ 9999
    GW_MAGIC:       equ 0x5548
    GW_HEADER_SIZE: equ 8
    GW_MAX_PAYLOAD: equ 4096
    SUBNET_REPL:    equ 1
    SUBNET_CONSOL:  equ 4

    ; Framing
    cl_header:      db "Content-Length: ", 0
    cl_crlf:        db 13, 10, 13, 10, 0   ; \r\n\r\n
    newline_char:   db 10                   ; \n for stdio framing

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
    gw_frame_buf:   resb GW_HEADER_SIZE + GW_MAX_PAYLOAD

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

    ; Content-Length write buffer (header + crlf)
    cl_buf:         resb 64

    ; State
    use_content_length: resd 1      ; 1 = Content-Length framing
    hm_initialized:     resd 1      ; 1 = holo_mem_init done
    uhma_connected:     resd 1      ; 1 = connected to UHMA
    gw_seq_counter:     resw 1      ; seq counter for framed gateway
    gw_expect_seq:      resw 1      ; expected response seq
    gw_last_seq_rx:     resw 1      ; last received seq
    gw_last_subnet_rx:  resb 1      ; last received subnet

section .text

; Holographic memory functions (from holo_mem.asm)
extern holo_mem_init
extern holo_mem_add
extern holo_mem_query
extern holo_mem_state
extern holo_mem_recent
extern holo_mem_summary
extern holo_mem_rag_refresh

; Output buffer capture (from format.asm)
extern set_output_channel
extern set_output_buffer
extern get_output_buffer
extern reset_output_channel

global _start

;; ============================================================
;; _start — Entry point
;; ============================================================
_start:
    ; Initialize state only — no slow init here (deferred to first use)
    mov dword [rel use_content_length], 1
    mov dword [rel fd_query_in], -1
    mov dword [rel fd_query_out], -1
    mov dword [rel hm_initialized], 0
    mov dword [rel uhma_connected], 0

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
    ; If no "id" field, this is a notification — silently skip
    cmp qword [rel req_id], 0
    je .main_loop
    mov rdi, [rel req_id]
    lea rsi, [rel err_parse]
    call send_error_response
    jmp .main_loop

;; ============================================================
;; connect_uhma — Connect to UHMA gateway (single socket)
;; Returns: eax = 1 on success, 0 on failure
;; ============================================================
connect_uhma:
    push rbx
    push r12
    sub rsp, 24

    ; Connect to gateway port 9999
    mov edi, GW_PORT
    call connect_port
    test eax, eax
    js .conn_fail
    ; Same socket for both read and write (gateway is bidirectional)
    mov [rel fd_query_in], eax
    mov [rel fd_query_out], eax
    mov word [rel gw_seq_counter], 0

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
;; Supports both newline-delimited JSON (MCP stdio spec) and
;; Content-Length framing. Auto-detects based on first byte.
;; Returns: eax = bytes read, 0 on EOF
;; ============================================================
read_request:
    push rbx
    push r12
    push r13
    sub rsp, 16                 ; align (3 pushes = odd, +16)

    ; Read first byte to detect framing
    xor r12d, r12d              ; position = 0
.rr_read_first:
    mov eax, SYS_READ
    xor edi, edi                ; stdin
    lea rsi, [rel read_buf]
    mov edx, 1
    syscall
    test eax, eax
    jle .rr_eof

    ; Skip blank lines (newlines between messages)
    movzx eax, byte [rel read_buf]
    cmp al, 10                  ; \n
    je .rr_read_first
    cmp al, 13                  ; \r
    je .rr_read_first

    ; Check if first byte is '{' — newline-delimited JSON
    cmp al, '{'
    je .rr_json_line

    ; Otherwise assume Content-Length framing — read rest of header line
    mov r12d, 1                 ; already have 1 byte
    jmp .rr_cl_header_byte

    ; === Newline-delimited JSON mode ===
    ; First byte '{' already in read_buf[0], read until \n
.rr_json_line:
    mov r12d, 1                 ; already have '{'
.rr_json_byte:
    mov eax, SYS_READ
    xor edi, edi                ; stdin
    lea rsi, [rel read_buf]
    add rsi, r12
    mov edx, 1
    syscall
    test eax, eax
    jle .rr_json_done           ; EOF — use what we have

    lea rcx, [rel read_buf]
    movzx eax, byte [rcx + r12]
    cmp al, 10                  ; newline = end of message
    je .rr_json_done

    inc r12d
    cmp r12d, 65534             ; overflow protection
    jl .rr_json_byte

.rr_json_done:
    ; Strip trailing \r if present
    lea rdi, [rel read_buf]
    test r12d, r12d
    jz .rr_eof
    cmp byte [rdi + r12 - 1], 13
    jne .rr_json_term
    dec r12d
.rr_json_term:
    mov byte [rdi + r12], 0
    mov eax, r12d
    jmp .rr_ret

    ; === Content-Length framing mode ===
    ; First byte already in read_buf[0], finish reading header
.rr_cl_header_byte:
    mov eax, SYS_READ
    xor edi, edi
    lea rsi, [rel cl_buf]       ; use cl_buf for header
    add rsi, r12
    mov edx, 1
    syscall
    test eax, eax
    jle .rr_eof

    ; Copy first byte to cl_buf if this is the start
    cmp r12d, 1
    jne .rr_cl_check_nl
    movzx eax, byte [rel read_buf]
    mov byte [rel cl_buf], al

.rr_cl_check_nl:
    lea rcx, [rel cl_buf]
    movzx eax, byte [rcx + r12]
    cmp al, 10
    je .rr_cl_got_line

    inc r12d
    cmp r12d, 60
    jl .rr_cl_header_byte
    ; Line too long, restart
    xor r12d, r12d
    jmp .rr_cl_header_byte

.rr_cl_got_line:
    lea rcx, [rel cl_buf]
    mov byte [rcx + r12], 0
    ; Strip \r
    test r12d, r12d
    jz .rr_cl_blank
    cmp byte [rcx + r12 - 1], 13
    jne .rr_cl_check_cl
    dec r12d
    mov byte [rcx + r12], 0
    test r12d, r12d
    jz .rr_cl_blank

.rr_cl_check_cl:
    ; Check for "Content-Length: "
    lea rdi, [rel cl_header]
    lea rsi, [rel cl_buf]
.rr_cl_cmp:
    mov al, [rdi]
    test al, al
    jz .rr_cl_match
    cmp al, [rsi]
    jne .rr_cl_next_line
    inc rdi
    inc rsi
    jmp .rr_cl_cmp

.rr_cl_match:
    mov rdi, rsi
    call atoi
    mov r13d, eax
    ; Fall through to read next line

.rr_cl_next_line:
    xor r12d, r12d
    jmp .rr_cl_header_byte

.rr_cl_blank:
    test r13d, r13d
    jz .rr_cl_next_line         ; no Content-Length yet

    ; Read exactly r13d bytes of JSON payload
    xor r12d, r12d
.rr_cl_payload:
    mov eax, SYS_READ
    xor edi, edi
    lea rsi, [rel read_buf]
    add rsi, r12
    mov edx, r13d
    sub edx, r12d
    syscall
    test eax, eax
    jle .rr_cl_check_got
    add r12d, eax
    cmp r12d, r13d
    jl .rr_cl_payload

    lea rdi, [rel read_buf]
    mov byte [rdi + r12], 0
    mov eax, r12d
    jmp .rr_ret

.rr_cl_check_got:
    test r12d, r12d
    jz .rr_eof
    lea rdi, [rel read_buf]
    mov byte [rdi + r12], 0
    mov eax, r12d
    jmp .rr_ret

.rr_eof:
    xor eax, eax

.rr_ret:
    add rsp, 16
    pop r13
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

    ; metacog
    lea rdi, [rel tool_metacog]
    mov rsi, rbx
    call match_tool_name
    test eax, eax
    jnz .do_metacog

    ; genes
    lea rdi, [rel tool_genes]
    mov rsi, rbx
    call match_tool_name
    test eax, eax
    jnz .do_genes

    ; regions
    lea rdi, [rel tool_regions]
    mov rsi, rbx
    call match_tool_name
    test eax, eax
    jnz .do_regions

    ; hive
    lea rdi, [rel tool_hive]
    mov rsi, rbx
    call match_tool_name
    test eax, eax
    jnz .do_hive

    ; colony
    lea rdi, [rel tool_colony]
    mov rsi, rbx
    call match_tool_name
    test eax, eax
    jnz .do_colony

    ; compact
    lea rdi, [rel tool_compact]
    mov rsi, rbx
    call match_tool_name
    test eax, eax
    jnz .do_compact

    ; reset
    lea rdi, [rel tool_reset]
    mov rsi, rbx
    call match_tool_name
    test eax, eax
    jnz .do_reset

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

    ; mem_state
    lea rdi, [rel tool_mem_state]
    mov rsi, rbx
    call match_tool_name
    test eax, eax
    jnz .do_mem_state

    ; mem_recent
    lea rdi, [rel tool_mem_recent]
    mov rsi, rbx
    call match_tool_name
    test eax, eax
    jnz .do_mem_recent

    ; mem_summary
    lea rdi, [rel tool_mem_summary]
    mov rsi, rbx
    call match_tool_name
    test eax, eax
    jnz .do_mem_summary

    ; mem_rag_refresh
    lea rdi, [rel tool_mem_rag_refresh]
    mov rsi, rbx
    call match_tool_name
    test eax, eax
    jnz .do_mem_rag_refresh

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

.do_metacog:
    lea rsi, [rel cmd_metacog]
    call uhma_command
    jmp .dispatch_ret

.do_genes:
    lea rsi, [rel cmd_genes]
    call uhma_command
    jmp .dispatch_ret

.do_regions:
    lea rsi, [rel cmd_regions]
    call uhma_command
    jmp .dispatch_ret

.do_hive:
    lea rsi, [rel cmd_hive]
    call uhma_command
    jmp .dispatch_ret

.do_colony:
    lea rsi, [rel cmd_colony]
    call uhma_command
    jmp .dispatch_ret

.do_compact:
    lea rsi, [rel cmd_compact]
    call uhma_command
    jmp .dispatch_ret

.do_reset:
    lea rsi, [rel cmd_reset]
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
    call ensure_holo_mem
    ; Call holo_mem_add(category, content, context, source)
    ; Parse category from req_category
    mov rsi, [rel req_category]
    test rsi, rsi
    jz .dispatch_unknown
    call parse_category_num     ; returns eax = category number
    push rax                    ; save category
    ; Get content
    mov rsi, [rel req_content]
    test rsi, rsi
    jz .dispatch_unknown
    lea rdi, [rel send_buf]
    call extract_json_string
    ; Capture output from holo_mem_add
    call set_output_buffer
    ; Call holo_mem_add(edi=category, rsi=content, rdx=context, ecx=source)
    pop rdi                     ; category
    lea rsi, [rel send_buf]     ; content
    xor edx, edx                ; context = NULL
    xor ecx, ecx                ; source = NULL
    call holo_mem_add
    mov [rel req_n], eax        ; save entry_id
    call get_output_buffer       ; rax=buf, edx=len
    call reset_output_channel
    ; Format response: captured output + "Added #N"
    lea rdi, [rel resp_buf]
    mov rsi, rax
    mov ecx, edx
    rep movsb
    mov byte [rdi], 'A'
    mov byte [rdi+1], 'd'
    mov byte [rdi+2], 'd'
    mov byte [rdi+3], 'e'
    mov byte [rdi+4], 'd'
    mov byte [rdi+5], ' '
    mov byte [rdi+6], '#'
    add rdi, 7
    mov eax, [rel req_n]
    call itoa_inline
    mov byte [rdi], 0
    call send_success_response
    jmp .dispatch_ret

.do_mem_query:
    call ensure_holo_mem
    ; Call holo_mem_query(query, limit)
    mov rsi, [rel req_text]
    test rsi, rsi
    jz .dispatch_unknown
    lea rdi, [rel send_buf]
    call extract_json_string
    ; Capture output into buffer
    call set_output_buffer
    lea rdi, [rel send_buf]
    mov esi, [rel req_n]
    test esi, esi
    jnz .query_limit_ok
    mov esi, 10
.query_limit_ok:
    call holo_mem_query
    call get_output_buffer       ; rax=buf, edx=len
    call reset_output_channel
    ; Copy captured output to resp_buf
    lea rdi, [rel resp_buf]
    mov rsi, rax
    mov ecx, edx
    test ecx, ecx
    jz .query_empty
    rep movsb
    mov byte [rdi], 0
    call send_success_response
    jmp .dispatch_ret
.query_empty:
    mov byte [rdi], 0
    lea rdi, [rel resp_buf]
    mov dword [rdi], '(no '
    mov dword [rdi+4], 'resu'
    mov dword [rdi+8], 'lts)'
    mov byte [rdi+12], 0
    call send_success_response
    jmp .dispatch_ret

.do_mem_state:
    call ensure_holo_mem
    call set_output_buffer
    call holo_mem_state
    call get_output_buffer       ; rax=buf, edx=len
    call reset_output_channel
    lea rdi, [rel resp_buf]
    mov rsi, rax
    mov ecx, edx
    rep movsb
    mov byte [rdi], 0
    call send_success_response
    jmp .dispatch_ret

.do_mem_recent:
    call ensure_holo_mem
    call set_output_buffer
    mov edi, [rel req_n]
    test edi, edi
    jnz .recent_limit_ok
    mov edi, 10
.recent_limit_ok:
    call holo_mem_recent
    call get_output_buffer       ; rax=buf, edx=len
    call reset_output_channel
    lea rdi, [rel resp_buf]
    mov rsi, rax
    mov ecx, edx
    rep movsb
    mov byte [rdi], 0
    call send_success_response
    jmp .dispatch_ret

.do_mem_summary:
    call ensure_holo_mem
    call set_output_buffer
    call holo_mem_summary
    call get_output_buffer       ; rax=buf, edx=len
    call reset_output_channel
    lea rdi, [rel resp_buf]
    mov rsi, rax
    mov ecx, edx
    rep movsb
    mov byte [rdi], 0
    call send_success_response
    jmp .dispatch_ret

.do_mem_rag_refresh:
    call ensure_holo_mem
    call set_output_buffer
    call holo_mem_rag_refresh
    call get_output_buffer       ; rax=buf, edx=len
    call reset_output_channel
    lea rdi, [rel resp_buf]
    mov rsi, rax
    mov ecx, edx
    rep movsb
    mov byte [rdi], 0
    call send_success_response
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
;; ensure_holo_mem — Lazy init holographic memory on first use
;; ============================================================
ensure_holo_mem:
    cmp dword [rel hm_initialized], 1
    je .ehm_done
    push rbx
    sub rsp, 8
    ; Redirect output to stderr during init
    mov edi, 2
    call set_output_channel
    call holo_mem_init
    call reset_output_channel
    mov dword [rel hm_initialized], 1
    add rsp, 8
    pop rbx
.ehm_done:
    ret

;; ============================================================
;; ensure_uhma — Lazy connect to UHMA gateway on first use
;; ============================================================
ensure_uhma:
    cmp dword [rel uhma_connected], 1
    je .eu_done
    push rbx
    sub rsp, 8
    call connect_uhma
    test eax, eax
    jz .eu_ret
    mov dword [rel uhma_connected], 1
.eu_ret:
    add rsp, 8
    pop rbx
.eu_done:
    ret

;; ============================================================
;; check_connection — Verify UHMA connection is still alive
;; Returns: eax = 1 if OK, 0 if needs reconnect
;; ============================================================
check_connection:
    ; Simple check - verify gateway fd is valid (not -1)
    mov eax, [rel fd_query_in]
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

    ; Close old socket if any (single gateway fd)
    mov edi, [rel fd_query_in]
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
;; gw_send_frame — Send framed payload to UHMA gateway
;; edi = subnet, rsi = payload ptr, edx = payload len
;; Returns: ax = seq_id (0 on failure)
;; ============================================================
gw_send_frame:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8

    mov r12, rsi                      ; payload ptr
    mov r13d, edx                     ; payload len
    mov r15b, dil                     ; subnet

    mov ebx, [rel fd_query_in]
    cmp ebx, -1
    je .gs_fail

    cmp r13d, GW_MAX_PAYLOAD
    ja .gs_fail

    ; Next seq
    movzx eax, word [rel gw_seq_counter]
    inc eax
    mov [rel gw_seq_counter], ax
    mov r14d, eax                     ; seq_id

    ; Build header
    lea rdi, [rel gw_frame_buf]
    mov word [rdi], GW_MAGIC
    mov [rdi + 2], r15b
    mov byte [rdi + 3], 0
    mov [rdi + 4], r14w
    mov [rdi + 6], r13w

    ; Copy payload
    lea rdi, [rel gw_frame_buf + GW_HEADER_SIZE]
    mov rsi, r12
    mov ecx, r13d
    rep movsb

    ; Send frame
    mov eax, SYS_WRITE
    mov edi, ebx
    lea rsi, [rel gw_frame_buf]
    mov edx, GW_HEADER_SIZE
    add edx, r13d
    syscall
    test eax, eax
    jle .gs_fail

    mov eax, r14d
    jmp .gs_ret

.gs_fail:
    xor eax, eax

.gs_ret:
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; gw_read_exact — Read exactly edx bytes from gateway socket
;; edi = fd, rsi = buf, edx = len
;; Returns: eax = bytes read (len) or <=0 on error/EOF
;; ============================================================
gw_read_exact:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 8

    mov ebx, edi                      ; fd
    mov r12, rsi                      ; base buffer
    mov r14d, edx                     ; remaining (callee-saved, safe across syscall)
    xor r13d, r13d                    ; total read (callee-saved, safe across syscall)

.gr_loop:
    test r14d, r14d
    jz .gr_done
    mov eax, SYS_READ
    mov edi, ebx
    lea rsi, [r12 + r13]
    mov edx, r14d
    syscall
    test eax, eax
    jle .gr_err
    add r13d, eax
    sub r14d, eax
    jmp .gr_loop

.gr_done:
    mov eax, r13d
    jmp .gr_ret

.gr_err:
    ; eax already <= 0
    nop

.gr_ret:
    add rsp, 8
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; gw_read_frame — Read one framed response into resp_buf
;; Returns: eax = payload len (0 on error)
;; Sets gw_last_seq_rx and gw_last_subnet_rx
;; ============================================================
gw_read_frame:
    push rbx
    push r12
    push r13
    sub rsp, 8

    mov ebx, [rel fd_query_out]
    cmp ebx, -1
    je .gf_fail

    ; Read header
    mov edi, ebx
    lea rsi, [rel gw_frame_buf]
    mov edx, GW_HEADER_SIZE
    call gw_read_exact
    cmp eax, GW_HEADER_SIZE
    jne .gf_fail

    ; Validate magic
    lea rdx, [rel gw_frame_buf]
    movzx eax, word [rdx]
    cmp ax, GW_MAGIC
    jne .gf_fail

    ; Capture subnet + seq
    movzx eax, byte [rdx + 2]
    mov [rel gw_last_subnet_rx], al
    movzx eax, word [rdx + 4]
    mov [rel gw_last_seq_rx], ax

    ; Payload length
    movzx eax, word [rdx + 6]
    test eax, eax
    jle .gf_fail
    cmp eax, GW_MAX_PAYLOAD
    jg .gf_fail
    mov r13d, eax

    ; Read payload into resp_buf
    mov edi, ebx
    lea rsi, [rel resp_buf]
    mov edx, r13d
    call gw_read_exact
    cmp eax, r13d
    jne .gf_fail

    ; Null-terminate
    lea rdi, [rel resp_buf]
    mov byte [rdi + r13], 0
    mov eax, r13d
    jmp .gf_ret

.gf_fail:
    xor eax, eax

.gf_ret:
    add rsp, 8
    pop r13
    pop r12
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

    ; Send framed command on REPL subnet
    mov rdi, r12                ; strlen expects string in rdi
    call strlen
    mov edx, eax                ; payload length
    mov rsi, r12                ; payload ptr
    mov edi, SUBNET_REPL
    call gw_send_frame
    test eax, eax
    jz .uhma_cmd_fail
    mov [rel gw_expect_seq], ax

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

    ; Enable ccmode (framed)
    lea rdi, [rel cmd_ccmode_on]
    call strlen
    mov edx, eax
    lea rsi, [rel cmd_ccmode_on]
    mov edi, SUBNET_CONSOL
    call gw_send_frame

    ; Send the text (ensure newline terminator)
    lea rdi, [rel send_buf]
    call strlen
    lea rdi, [rel send_buf]
    add rdi, rax
    mov byte [rdi], 10
    mov byte [rdi + 1], 0
    inc eax
    mov edx, eax                ; payload length incl newline
    lea rsi, [rel send_buf]
    mov edi, SUBNET_CONSOL
    call gw_send_frame
    test eax, eax
    jz .input_fail
    mov [rel gw_expect_seq], ax

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
    sub rsp, 16

    ; Clear resp_buf (first byte)
    mov byte [rel resp_buf], 0

    movzx r12d, word [rel gw_expect_seq]
    xor r13d, r13d              ; consecutive timeouts

    ; Setup pollfd struct
    mov eax, [rel fd_query_out]
    mov [rsp], eax              ; fd
    mov word [rsp + 4], POLLIN  ; events
    mov word [rsp + 6], 0       ; revents

.read_loop:
    mov word [rsp + 6], 0
    mov eax, SYS_POLL
    lea rdi, [rsp]
    mov esi, 1
    mov edx, 2000               ; 2 seconds
    syscall

    test eax, eax
    jl .read_done               ; error
    jz .poll_timeout

    movzx eax, word [rsp + 6]
    test ax, POLLIN
    jz .poll_timeout

    ; Read one framed response
    call gw_read_frame
    test eax, eax
    jz .poll_timeout

    ; Reset timeout counter on any frame
    xor r13d, r13d

    ; Check seq match
    movzx eax, word [rel gw_last_seq_rx]
    cmp ax, r12w
    jne .read_loop              ; discard mismatched frames
    jmp .read_done

.poll_timeout:
    inc r13d
    cmp r13d, 5                 ; ~10s total
    jl .read_loop

.read_done:
    add rsp, 16
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

; write_stdout - rsi = null-terminated JSON string
; Writes newline-delimited JSON per MCP stdio spec: JSON\n
write_stdout:
    push rbx
    push r12
    sub rsp, 8              ; align (2 pushes = even, +8)

    mov rbx, rsi            ; save JSON pointer
    mov rdi, rsi
    call strlen
    mov r12d, eax           ; r12d = JSON length

    ; Strip trailing newline if present (we'll add our own)
    test r12d, r12d
    jz .ws_done
    lea rdi, [rbx + r12 - 1]
    cmp byte [rdi], 10
    jne .ws_write
    dec r12d

.ws_write:
    ; Write the JSON body
    mov eax, SYS_WRITE
    mov edi, STDOUT
    mov rsi, rbx
    mov edx, r12d
    syscall

    ; Write terminating newline
    lea rsi, [rel newline_char]
    mov eax, SYS_WRITE
    mov edi, STDOUT
    mov edx, 1
    syscall

.ws_done:
    add rsp, 8
    pop r12
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

;; ============================================================
;; parse_category_num — Convert category string to number
;; rsi = category string (from JSON, with quotes stripped)
;; Returns: eax = category number (0-13)
;; ============================================================
parse_category_num:
    push rbx
    mov rbx, rsi

    ; "finding" = 0
    cmp dword [rbx], 'find'
    jne .not_finding
    mov eax, 0
    jmp .cat_ret
.not_finding:

    ; "failed" = 1
    cmp dword [rbx], 'fail'
    jne .not_failed
    mov eax, 1
    jmp .cat_ret
.not_failed:

    ; "success" = 2
    cmp dword [rbx], 'succ'
    jne .not_success
    mov eax, 2
    jmp .cat_ret
.not_success:

    ; "insight" = 3
    cmp dword [rbx], 'insi'
    jne .not_insight
    mov eax, 3
    jmp .cat_ret
.not_insight:

    ; "warning" = 4
    cmp dword [rbx], 'warn'
    jne .not_warning
    mov eax, 4
    jmp .cat_ret
.not_warning:

    ; "session" = 5
    cmp dword [rbx], 'sess'
    jne .not_session
    mov eax, 5
    jmp .cat_ret
.not_session:

    ; "location" = 6
    cmp dword [rbx], 'loca'
    jne .not_location
    mov eax, 6
    jmp .cat_ret
.not_location:

    ; "question" = 7
    cmp dword [rbx], 'ques'
    jne .not_question
    mov eax, 7
    jmp .cat_ret
.not_question:

    ; "todo" = 8
    cmp dword [rbx], 'todo'
    jne .not_todo
    mov eax, 8
    jmp .cat_ret
.not_todo:

    ; "context" = 9
    cmp dword [rbx], 'cont'
    jne .not_context
    mov eax, 9
    jmp .cat_ret
.not_context:

    ; "request" = 10
    cmp dword [rbx], 'requ'
    jne .not_request
    mov eax, 10
    jmp .cat_ret
.not_request:

    ; Default to finding
    mov eax, 0

.cat_ret:
    pop rbx
    ret
