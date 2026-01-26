; mcp_client.asm — MCP server communication for GUI
;
; Spawns MCP server (python3 tools/rag/server.py) and handles JSON-RPC
; communication over pipes. GUI calls these instead of UHMA directly.
;
; @entry mcp_init() -> eax=1 on success, 0 on failure
; @entry mcp_shutdown()
; @entry mcp_call(rdi=tool_name, rsi=args_json) -> rax=response_ptr
; @entry mcp_send_text(rdi=text) -> rax=response_ptr
; @entry mcp_get_status() -> rax=status_json_ptr
;
; FLOW: mcp_init spawns server, mcp_call sends JSON-RPC, parses response
;
; GOTCHAS:
;   - Response buffer is static, overwritten each call
;   - JSON-RPC uses Content-Length header framing
;   - MCP server spawns UHMA internally
;   - Stack alignment (x86-64 ABI): ODD pushes → aligned → sub must be multiple of 16
;                                   EVEN pushes → unaligned → sub must be 8 mod 16

section .data
    ; MCP server command
    mcp_cmd:        db "python3", 0
    mcp_arg1:       db "../tools/rag/server.py", 0
    mcp_argv:       dq 0, 0, 0  ; filled at runtime

    ; JSON-RPC templates
    json_init:      db '{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"uhma-gui"}},"id":1}', 0
    json_call_pre:  db '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"', 0
    json_call_mid:  db '","arguments":{', 0
    json_call_post: db '}},"id":', 0
    json_input_arg: db '"text":"', 0

    ; Content-Length header
    content_hdr:    db "Content-Length: ", 0
    crlf2:          db 13, 10, 13, 10, 0

    ; Format strings
    fmt_int:        db "%d", 0

    ; Error messages
    err_fork:       db "MCP: fork failed", 10, 0
    err_pipe:       db "MCP: pipe failed", 10, 0
    err_exec:       db "MCP: exec failed", 10, 0
    msg_started:    db "MCP: server started", 10, 0

section .bss
    ; Pipe file descriptors [read, write]
    pipe_to_mcp:    resd 2      ; GUI writes, MCP reads
    pipe_from_mcp:  resd 2      ; MCP writes, GUI reads

    mcp_pid:        resd 1
    mcp_running:    resd 1
    call_id:        resd 1

    ; Request/response buffers
    req_buf:        resb 4096
    resp_buf:       resb 65536
    status_cache:   resb 8192

section .text

; Libc functions
extern fork
extern execvp
extern pipe
extern close
extern dup2
extern read
extern write
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

;; mcp_init — Spawn MCP server, establish pipes
;; Returns: eax=1 success, 0 failure
mcp_init:
    push rbx
    push r12
    push r13
    sub rsp, 16

    ; Create pipes
    lea rdi, [rel pipe_to_mcp]
    call pipe
    test eax, eax
    jnz .pipe_fail

    lea rdi, [rel pipe_from_mcp]
    call pipe
    test eax, eax
    jnz .pipe_fail

    ; Fork
    call fork
    cmp eax, -1
    je .fork_fail
    test eax, eax
    jz .child

    ; Parent: save PID, close unused ends
    mov [rel mcp_pid], eax

    ; Close read end of pipe_to_mcp (we write)
    mov edi, [rel pipe_to_mcp]
    call close

    ; Close write end of pipe_from_mcp (we read)
    mov edi, [rel pipe_from_mcp + 4]
    call close

    ; Wait for server to start
    mov edi, 500000         ; 500ms
    call usleep

    ; Send initialize request
    call .send_init

    mov dword [rel mcp_running], 1
    mov dword [rel call_id], 2

    lea rdi, [rel msg_started]
    xor eax, eax
    call printf

    mov eax, 1
    jmp .done

.child:
    ; Child: set up pipes and exec MCP server

    ; stdin = pipe_to_mcp[0] (read end)
    mov edi, [rel pipe_to_mcp]
    xor esi, esi            ; STDIN_FILENO = 0
    call dup2

    ; stdout = pipe_from_mcp[1] (write end)
    mov edi, [rel pipe_from_mcp + 4]
    mov esi, 1              ; STDOUT_FILENO = 1
    call dup2

    ; Close all pipe fds
    mov edi, [rel pipe_to_mcp]
    call close
    mov edi, [rel pipe_to_mcp + 4]
    call close
    mov edi, [rel pipe_from_mcp]
    call close
    mov edi, [rel pipe_from_mcp + 4]
    call close

    ; Build argv
    lea rax, [rel mcp_cmd]
    mov [rel mcp_argv], rax
    lea rax, [rel mcp_arg1]
    mov [rel mcp_argv + 8], rax
    mov qword [rel mcp_argv + 16], 0

    ; Exec
    lea rdi, [rel mcp_cmd]
    lea rsi, [rel mcp_argv]
    call execvp

    ; If we get here, exec failed
    lea rdi, [rel err_exec]
    xor eax, eax
    call printf
    mov edi, 1
    mov eax, 60             ; sys_exit
    syscall

.pipe_fail:
    lea rdi, [rel err_pipe]
    xor eax, eax
    call printf
    xor eax, eax
    jmp .done

.fork_fail:
    lea rdi, [rel err_fork]
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

;; Send initialize JSON-RPC
.send_init:
    push rbx
    sub rsp, 16

    ; Build request with Content-Length header
    lea rdi, [rel json_init]
    call strlen
    mov rbx, rax            ; json length

    ; Format: "Content-Length: N\r\n\r\n{json}"
    lea rdi, [rel req_buf]
    lea rsi, [rel content_hdr]
    call strcpy

    lea rdi, [rel req_buf]
    call strlen
    lea rdi, [rel req_buf + rax]
    lea rsi, [rel fmt_int]
    mov rdx, rbx
    xor eax, eax
    call sprintf

    lea rdi, [rel req_buf]
    lea rsi, [rel crlf2]
    call strcat

    lea rdi, [rel req_buf]
    lea rsi, [rel json_init]
    call strcat

    ; Write to MCP
    lea rdi, [rel req_buf]
    call strlen
    mov rdx, rax            ; length
    mov edi, [rel pipe_to_mcp + 4]  ; write end
    lea rsi, [rel req_buf]
    call write

    ; Read response (discard for init)
    mov edi, [rel pipe_from_mcp]    ; read end
    lea rsi, [rel resp_buf]
    mov edx, 65535
    call read

    add rsp, 16
    pop rbx
    ret

;; mcp_shutdown — Kill MCP server
mcp_shutdown:
    push rbx
    sub rsp, 16

    cmp dword [rel mcp_running], 0
    je .done

    ; Send quit command first
    lea rdi, [rel .quit_tool]
    xor esi, esi
    call mcp_call

    ; Close pipes
    mov edi, [rel pipe_to_mcp + 4]
    call close
    mov edi, [rel pipe_from_mcp]
    call close

    ; Kill process
    mov edi, [rel mcp_pid]
    mov esi, 15             ; SIGTERM
    call kill

    ; Wait for it
    mov edi, [rel mcp_pid]
    xor esi, esi
    xor edx, edx
    call waitpid

    mov dword [rel mcp_running], 0

.done:
    add rsp, 16
    pop rbx
    ret

.quit_tool: db "quit", 0

;; mcp_call — Call MCP tool
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

    ; Build JSON: {"jsonrpc":"2.0","method":"tools/call","params":{"name":"TOOL","arguments":{ARGS}},"id":N}
    lea rdi, [rel req_buf + 256]  ; build json here, header at start
    lea rsi, [rel json_call_pre]
    call strcpy

    lea rdi, [rel req_buf + 256]
    mov rsi, r12
    call strcat

    lea rdi, [rel req_buf + 256]
    lea rsi, [rel json_call_mid]
    call strcat

    ; Add arguments if provided
    test r13, r13
    jz .no_args
    cmp byte [r13], 0
    je .no_args

    lea rdi, [rel req_buf + 256]
    mov rsi, r13
    call strcat

.no_args:
    lea rdi, [rel req_buf + 256]
    lea rsi, [rel json_call_post]
    call strcat

    ; Add call ID
    lea rdi, [rel req_buf + 256]
    call strlen
    lea rdi, [rel req_buf + 256 + rax]
    lea rsi, [rel fmt_int]
    mov edx, [rel call_id]
    xor eax, eax
    call sprintf

    ; Close JSON
    lea rdi, [rel req_buf + 256]
    call strlen
    lea rdi, [rel req_buf + 256 + rax]
    mov byte [rdi], '}'
    mov byte [rdi + 1], 0

    inc dword [rel call_id]

    ; Get JSON length
    lea rdi, [rel req_buf + 256]
    call strlen
    mov r14, rax

    ; Build header at start of req_buf
    lea rdi, [rel req_buf]
    lea rsi, [rel content_hdr]
    call strcpy

    lea rdi, [rel req_buf]
    call strlen
    lea rdi, [rel req_buf + rax]
    lea rsi, [rel fmt_int]
    mov rdx, r14
    xor eax, eax
    call sprintf

    lea rdi, [rel req_buf]
    lea rsi, [rel crlf2]
    call strcat

    lea rdi, [rel req_buf]
    lea rsi, [rel req_buf + 256]
    call strcat

    ; Send request
    lea rdi, [rel req_buf]
    call strlen
    mov rdx, rax
    mov edi, [rel pipe_to_mcp + 4]
    lea rsi, [rel req_buf]
    call write

    ; Read response
    mov edi, [rel pipe_from_mcp]
    lea rsi, [rel resp_buf]
    mov edx, 65535
    call read
    mov r14, rax            ; bytes read

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
