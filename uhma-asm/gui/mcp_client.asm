; mcp_client.asm — GUI client for UHMA 6-channel TCP I/O
;
; @entry mcp_init() -> eax=1 success, 0 failure (connects to all 6 ports)
; @entry mcp_shutdown() -> closes all sockets
; @entry mcp_call(rdi=tool_name, rsi=args_json) -> rax=response_ptr
; @entry mcp_call_ch(edi=channel, rsi=tool_name, rdx=args_json) -> rax=response_ptr
; @entry mcp_send_text(rdi=text) -> rax=response_ptr
; @entry mcp_get_status() -> rax=status_json_ptr
; @calledby gui/visualizer.asm
;
; DUAL MODE OPERATION:
;   Mode 1: GUI spawns ./uhma directly (simple, for exploration)
;   Mode 2: User starts ./feed.sh first (training mode with consolidation)
;   GUI auto-detects: if ports 9999-9994 respond, connects to existing UHMA
;   If no UHMA running, GUI spawns ./uhma automatically
;
; CHANNEL PAIRS (client perspective - write to input, read from output):
;   FEED:  write 9999 (CH0) → read 9998 (CH1) - eat, dream, observe
;   QUERY: write 9997 (CH2) → read 9996 (CH3) - status, why, misses
;   DEBUG: write 9995 (CH4) → read 9994 (CH5) - trace, receipts
;
; PROTOCOL:
;   1. Connect to both ports of a channel pair
;   2. Send command to input port (even: 0,2,4)
;   3. Read response from output port (odd: 1,3,5)
;   Synchronous request/response, no async
;
; GOTCHAS:
;   - Response buffer is static, overwritten each call
;   - Stack alignment: ODD pushes → sub rsp must be multiple of 16
;   - UHMA blocks if output ports not drained - GUI must read responses

section .data
    ; UHMA command (for spawning if not running)
    mcp_cmd:        db "./uhma", 0
    mcp_arg1:       db 0              ; no args needed
    mcp_arg2:       db 0
    mcp_arg3:       db 0
    mcp_arg4:       db 0
    mcp_argv:       dq 0, 0, 0, 0, 0, 0  ; filled at runtime

    ; TCP connection params (6-channel)
    MCP_NUM_CHANNELS equ 6
    MCP_PORT_BASE    equ 9999         ; channels use 9999-9994
    MCP_CH_FEED      equ 0            ; channel 0 = feed (async)
    MCP_CH_QUERY     equ 1            ; channel 1 = query (sync)
    MCP_CH_TRACE     equ 2            ; channel 2 = trace/receipts
    MCP_CH_STREAM    equ 3            ; channel 3 = event stream
    MCP_CH_LLM       equ 4            ; channel 4 = LLM communication
    MCP_CH_RESERVED  equ 5            ; channel 5 = reserved
    AF_INET          equ 2
    SOCK_STREAM      equ 1

    ; JSON-RPC templates (raw JSON, no Content-Length for TCP)
    json_init:      db '{"jsonrpc":"2.0","method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"uhma-gui"}},"id":1}', 10, 0
    json_call_pre:  db '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"', 0
    json_call_mid:  db '","arguments":{', 0
    json_call_post: db '}},"id":', 0
    json_input_arg: db '"text":"', 0

    ; Format strings
    fmt_int:        db "%d", 0

    ; Error messages
    err_connect:    db "GUI: no UHMA found, spawning ./uhma...", 10, 0
    err_spawn:      db "GUI: failed to spawn UHMA (try ./feed.sh first)", 10, 0
    err_socket:     db "GUI: socket error", 10, 0
    msg_connected:  db "GUI: connected to UHMA (%d channels)", 10, 0
    msg_autonomous: db "GUI: autonomous mode enabled", 10, 0

    ; Command to enable autonomous mode (toggles batch_mode 1→0)
    cmd_batch:      db "batch", 10, 0
    cmd_batch_len   equ 6

section .bss
    ; TCP sockets (6-channel)
    mcp_sockets:      resd 6          ; socket FDs for channels 0-5
    mcp_sock_valid:   resd 6          ; 1 if channel connected, 0 if not
    mcp_running:      resd 1
    mcp_pid:          resd 1
    call_id:          resd 1
    spawned_uhma:     resd 1          ; 1 if GUI spawned UHMA (vs connecting to existing)

    ; sockaddr_in structure (16 bytes)
    sock_addr:        resb 16

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
global mcp_call_ch
global mcp_call_feed
global mcp_send_text
global mcp_get_status
global mcp_dream
global mcp_observe
global mcp_evolve
global mcp_save
global mcp_load
global mcp_get_channel_socket

;; mcp_init — Connect to MCP server via TCP (all channels)
;; Returns: eax=1 success (at least query channel connected), 0 failure
mcp_init:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8              ; 5 pushes (odd) + 8 = 48, aligned

    ; Clear socket validity flags (6 channels)
    xor eax, eax
    mov [rel mcp_sock_valid], eax
    mov [rel mcp_sock_valid + 4], eax
    mov [rel mcp_sock_valid + 8], eax
    mov [rel mcp_sock_valid + 12], eax
    mov [rel mcp_sock_valid + 16], eax
    mov [rel mcp_sock_valid + 20], eax

    ; Try to connect all channels
    call .try_connect_all
    mov r12d, eax           ; save connected count

    ; If no channels connected, spawn server
    test r12d, r12d
    jnz .some_connected

    ; Connection failed - spawn server
    lea rdi, [rel err_connect]
    xor eax, eax
    call printf

    call .spawn_server
    test eax, eax
    jz .spawn_fail

    ; Mark that we spawned UHMA (for enabling autonomous mode later)
    mov dword [rel spawned_uhma], 1

    ; Wait for server to start
    mov edi, 2000000        ; 2s for multi-channel startup
    call usleep

    ; Try to connect again
    call .try_connect_all
    mov r12d, eax

.some_connected:
    ; Clear spawned flag if we connected to existing (didn't go through spawn path)
    ; spawned_uhma is only set above, so if we jumped here directly, it's 0
    ; Need at least query channel (channel 1)
    cmp dword [rel mcp_sock_valid + 4], 0
    je .spawn_fail

    ; Send initialize on query channel
    mov edi, MCP_CH_QUERY
    call .send_init_ch

    mov dword [rel mcp_running], 1
    mov dword [rel call_id], 2

    lea rdi, [rel msg_connected]
    mov esi, r12d           ; channels connected
    xor eax, eax
    call printf

    ; If we spawned UHMA, enable autonomous mode (toggle batch_mode 1→0)
    cmp dword [rel spawned_uhma], 0
    je .skip_autonomous
    call .enable_autonomous
.skip_autonomous:

    mov eax, 1
    jmp .done

.spawn_fail:
    lea rdi, [rel err_spawn]
    xor eax, eax
    call printf
    xor eax, eax
    jmp .done

.done:
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; Try to connect all channels
;; Returns: eax = number of channels connected
.try_connect_all:
    push rbx
    push r12
    push r13
    sub rsp, 8

    xor r12d, r12d          ; connected count
    xor r13d, r13d          ; channel index

.connect_loop:
    cmp r13d, MCP_NUM_CHANNELS
    jge .connect_done

    ; Calculate port: base - channel (9999, 9998, 9997, 9996)
    mov eax, MCP_PORT_BASE
    sub eax, r13d
    mov ebx, eax            ; port for this channel

    ; Create socket
    mov edi, AF_INET
    mov esi, SOCK_STREAM
    xor edx, edx
    call socket
    cmp eax, -1
    je .next_channel

    ; Save socket fd
    lea rcx, [rel mcp_sockets]
    mov [rcx + r13 * 4], eax
    push rax                ; save socket fd

    ; Setup sockaddr_in
    lea rdi, [rel sock_addr]
    xor esi, esi
    mov edx, 16
    call memset

    mov word [rel sock_addr], AF_INET
    mov edi, ebx            ; port
    call htons
    mov [rel sock_addr + 2], ax
    mov dword [rel sock_addr + 4], 0x0100007f  ; 127.0.0.1

    ; Connect
    pop rdi                 ; socket fd
    push rdi
    lea rsi, [rel sock_addr]
    mov edx, 16
    call connect
    pop rbx                 ; socket fd
    test eax, eax
    jnz .close_failed

    ; Mark channel as valid
    lea rcx, [rel mcp_sock_valid]
    mov dword [rcx + r13 * 4], 1
    inc r12d
    jmp .next_channel

.close_failed:
    mov edi, ebx
    call close

.next_channel:
    inc r13d
    jmp .connect_loop

.connect_done:
    mov eax, r12d
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    ret

;; Spawn UHMA directly (creates 6 TCP channels itself)
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
    ; Build argv: ./uhma (no extra args - UHMA creates 6 TCP channels itself)
    lea rax, [rel mcp_cmd]
    mov [rel mcp_argv], rax
    mov qword [rel mcp_argv + 8], 0   ; NULL terminate argv

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

;; Enable autonomous mode - send "batch" to toggle batch_mode 1→0
;; Sends to CH0 (FEED input), reads response from CH1 (FEED output)
.enable_autonomous:
    push rbx
    push r12
    sub rsp, 8

    ; Print message
    lea rdi, [rel msg_autonomous]
    xor eax, eax
    call printf

    ; Check FEED channels are valid
    cmp dword [rel mcp_sock_valid], 0       ; CH0
    je .auto_done
    cmp dword [rel mcp_sock_valid + 4], 0   ; CH1
    je .auto_done

    ; Send "batch\n" to CH0 (FEED input, port 9999)
    mov edi, [rel mcp_sockets]              ; CH0 socket
    lea rsi, [rel cmd_batch]
    mov edx, cmd_batch_len
    xor ecx, ecx
    call send

    ; Small delay for UHMA to process
    mov edi, 100000                         ; 100ms
    call usleep

    ; Read response from CH1 (FEED output, port 9998) - drain it
    mov edi, [rel mcp_sockets + 4]          ; CH1 socket
    lea rsi, [rel resp_buf]
    mov edx, 65535
    xor ecx, ecx
    call recv

.auto_done:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; Send initialize JSON-RPC on specific channel
;; edi = channel number
.send_init_ch:
    push rbx
    push r12
    sub rsp, 8

    mov r12d, edi           ; channel

    ; Check channel valid
    lea rax, [rel mcp_sock_valid]
    cmp dword [rax + r12 * 4], 0
    je .init_done

    ; Get socket for channel
    lea rax, [rel mcp_sockets]
    mov ebx, [rax + r12 * 4]

    ; Send init request
    lea rdi, [rel json_init]
    call strlen
    mov rdx, rax
    mov edi, ebx
    lea rsi, [rel json_init]
    xor ecx, ecx
    call send

    ; Read response (discard for init)
    mov edi, ebx
    lea rsi, [rel resp_buf]
    mov edx, 65535
    xor ecx, ecx
    call recv

.init_done:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; mcp_get_channel_socket — Get socket FD for channel
;; edi = channel number (0-3)
;; Returns: eax = socket FD, or -1 if invalid
mcp_get_channel_socket:
    cmp edi, MCP_NUM_CHANNELS
    jge .invalid_ch
    lea rax, [rel mcp_sock_valid]
    cmp dword [rax + rdi * 4], 0
    je .invalid_ch
    lea rax, [rel mcp_sockets]
    mov eax, [rax + rdi * 4]
    ret
.invalid_ch:
    mov eax, -1
    ret

;; mcp_shutdown — Close all TCP channels, optionally kill server
mcp_shutdown:
    push rbx
    push r12
    sub rsp, 8

    cmp dword [rel mcp_running], 0
    je .shut_done

    ; Send quit command on query channel
    lea rdi, [rel .quit_tool]
    xor esi, esi
    call mcp_call

    ; Close all connected sockets
    xor r12d, r12d
.close_loop:
    cmp r12d, MCP_NUM_CHANNELS
    jge .close_done

    lea rax, [rel mcp_sock_valid]
    cmp dword [rax + r12 * 4], 0
    je .next_close

    lea rax, [rel mcp_sockets]
    mov edi, [rax + r12 * 4]
    call close

    lea rax, [rel mcp_sock_valid]
    mov dword [rax + r12 * 4], 0

.next_close:
    inc r12d
    jmp .close_loop

.close_done:
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

.shut_done:
    add rsp, 8
    pop r12
    pop rbx
    ret

.quit_tool: db "quit", 0

;; mcp_call — Call MCP tool via TCP (uses QUERY channel)
;; rdi = tool name (C string)
;; rsi = arguments JSON (C string, can be empty or null)
;; Returns: rax = pointer to response text (in resp_buf)
mcp_call:
    ; Route to query channel (channel 1)
    mov rdx, rsi            ; args -> rdx
    mov rsi, rdi            ; tool -> rsi
    mov edi, MCP_CH_QUERY   ; channel 1
    jmp mcp_call_ch

;; mcp_call_feed — Call MCP tool via FEED channel (async)
;; rdi = tool name (C string)
;; rsi = arguments JSON (C string, can be empty or null)
;; Returns: rax = pointer to response text
mcp_call_feed:
    mov rdx, rsi
    mov rsi, rdi
    mov edi, MCP_CH_FEED    ; channel 0
    jmp mcp_call_ch

;; mcp_call_ch — Call MCP tool on specific channel
;; edi = channel number (0-3)
;; rsi = tool name (C string)
;; rdx = arguments JSON (C string, can be empty or null)
;; Returns: rax = pointer to response text (in resp_buf)
mcp_call_ch:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8              ; 5 pushes (odd) + 8 = 48, aligned

    mov r14d, edi           ; channel
    mov r12, rsi            ; tool name
    mov r13, rdx            ; args

    ; Check channel valid
    lea rax, [rel mcp_sock_valid]
    cmp dword [rax + r14 * 4], 0
    je .ch_not_found

    ; Get socket for channel
    lea rax, [rel mcp_sockets]
    mov r15d, [rax + r14 * 4]  ; socket fd

    ; Clear request buffer
    lea rdi, [rel req_buf]
    xor esi, esi
    mov edx, 4096
    call memset

    ; Build JSON: {"jsonrpc":"2.0","method":"tools/call","params":{"name":"TOOL","arguments":{ARGS}},"id":N}\n
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

    ; Send request via TCP on selected channel
    lea rdi, [rel req_buf]
    call strlen
    mov rdx, rax                ; length
    mov edi, r15d               ; socket fd
    lea rsi, [rel req_buf]
    xor ecx, ecx                ; flags = 0
    call send

    ; Read response via TCP
    mov edi, r15d
    lea rsi, [rel resp_buf]
    mov edx, 65535
    xor ecx, ecx
    call recv
    mov rbx, rax                ; bytes read

    ; Null terminate response
    lea rdi, [rel resp_buf]
    cmp rbx, 0
    jle .ch_not_found
    mov byte [rdi + rbx], 0

    ; Find "text":" in response and extract value
    lea rax, [rel resp_buf]
    mov rbx, rax

.find_text:
    cmp byte [rbx], 0
    je .ch_not_found
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
    je .ch_not_found
    ; Handle escaped quotes
    cmp byte [rbx], '\'
    jne .next_text_char
    inc rbx                 ; skip escaped char
.next_text_char:
    inc rbx
    jmp .find_end

.found_end:
    mov byte [rbx], 0       ; null terminate
    jmp .ch_done

.next_char:
    inc rbx
    jmp .find_text

.ch_not_found:
    lea rax, [rel resp_buf]

.ch_done:
    add rsp, 8
    pop r15
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
;; Feed operations (async) use channel 0
;; Query operations (sync) use channel 1

mcp_dream:
    ; Dream is async - use feed channel
    lea rdi, [rel .dream]
    xor esi, esi
    jmp mcp_call_feed
.dream: db "dream", 0

mcp_observe:
    ; Observe is async - use feed channel
    lea rdi, [rel .observe]
    xor esi, esi
    jmp mcp_call_feed
.observe: db "observe", 0

mcp_evolve:
    ; Evolve is async - use feed channel
    lea rdi, [rel .raw]
    lea rsi, [rel .evolve_arg]
    jmp mcp_call_feed
.raw: db "raw", 0
.evolve_arg: db '"command":"evolve"', 0

mcp_save:
    ; Save needs response - use query channel
    lea rdi, [rel .save]
    xor esi, esi
    jmp mcp_call
.save: db "save", 0

mcp_load:
    ; Load needs response - use query channel
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
