; mcp_client.asm — GUI client for UHMA 6-channel TCP I/O
;
; @entry mcp_init() -> eax=1 success, 0 failure (connects to all 6 ports)
; @entry mcp_shutdown() -> closes all sockets
; @entry mcp_call(rdi=tool_name, rsi=args_json) -> rax=response_ptr
; @entry mcp_call_ch(edi=channel, rsi=tool_name, rdx=args_json) -> rax=response_ptr
; @entry mcp_send_text(rdi=text) -> rax=response_ptr
; @entry mcp_get_status() -> rax=status_json_ptr
; @entry mcp_spawn_uhma() -> eax=1 success, spawns UHMA + sends "batch" for live mode
; @entry mcp_spawn_uhma_feed() -> eax=1 success, spawns UHMA without batch toggle (feed mode)
; @entry mcp_read_stream(edi=channel, rsi=buf, edx=size) -> eax=bytes_read
; @global mcp_running -> dword, 1 if UHMA connected
; @calledby gui/visualizer.asm
;
; DUAL MODE OPERATION:
;   Mode 1: GUI spawns ./uhma directly (simple, for exploration)
;   Mode 2: User starts ./feed.sh first (training mode with consolidation)
;   GUI auto-detects: if ports 9999-9994 respond, connects to existing UHMA
;   If no UHMA running, GUI spawns ./uhma via system() call
;
; SPAWN MODES:
;   mcp_spawn_uhma() - spawns UHMA then sends "batch" to toggle OFF (live/autonomous)
;   mcp_spawn_uhma_feed() - spawns UHMA without batch toggle (stays ON for feeding)
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
;   - Spawn uses absolute path /home/peter/Desktop/STARWARS/uhma-asm/uhma

section .data
    ; UHMA command (for spawning if not running)
    ; NOTE: GUI should be run from project root: ./gui/uhma-viz
    mcp_cmd:        db "/home/peter/Desktop/STARWARS/uhma-asm/uhma", 0
    spawn_cmd:      db "cd /home/peter/Desktop/STARWARS/uhma-asm && ./uhma < /dev/null &", 0
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
    SOL_SOCKET       equ 1
    SO_SNDTIMEO      equ 21        ; send timeout (affects connect on Linux)

    ; UHMA uses plain text protocol, no JSON-RPC needed

    ; Error messages
    err_connect:    db "GUI: spawning UHMA...", 10, 0
    err_spawn:      db "GUI: failed to spawn UHMA (try ./feed.sh first)", 10, 0
    err_socket:     db "GUI: socket error", 10, 0
    msg_connected:  db "GUI: connected to UHMA (%d channels)", 10, 0
    msg_autonomous: db "GUI: enabled autonomous mode (batch_mode=0)", 10, 0
    cmd_batch:      db "batch", 0

section .bss
    ; TCP sockets (6-channel)
    mcp_sockets:      resd 6          ; socket FDs for channels 0-5
    mcp_sock_valid:   resd 6          ; 1 if channel connected, 0 if not
    mcp_running:      resd 1
    mcp_pid:          resd 1
    call_id:          resd 1

    ; sockaddr_in structure (16 bytes)
    sock_addr:        resb 16

    ; Request/response buffers
    req_buf:        resb 4096
    resp_buf:       resb 65536
    status_cache:   resb 8192

    ; pollfd struct: { int fd; short events; short revents; }
    poll_fd:        resd 1          ; fd
    poll_events:    resw 1          ; events (POLLIN = 1)
    poll_revents:   resw 1          ; revents

section .text

; Libc functions
extern socket
extern connect
extern send
extern recv
extern close
extern fork
extern _exit
extern system
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
extern poll
extern setsockopt

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
global mcp_try_connect
global mcp_spawn_uhma
global mcp_spawn_uhma_feed
global mcp_running

;; mcp_try_connect — Try to connect to existing UHMA (no spawn)
;; Returns: eax=1 if connected, 0 if not
mcp_try_connect:
    push rbx
    sub rsp, 8

    ; Clear socket validity flags
    xor eax, eax
    mov [rel mcp_sock_valid], eax
    mov [rel mcp_sock_valid + 4], eax
    mov [rel mcp_sock_valid + 8], eax
    mov [rel mcp_sock_valid + 12], eax
    mov [rel mcp_sock_valid + 16], eax
    mov [rel mcp_sock_valid + 20], eax

    ; Try to connect all channels
    call mcp_try_connect_all
    mov ebx, eax                    ; save connected count

    ; Need at least one channel
    test ebx, ebx
    jz .try_not_connected

    mov dword [rel mcp_running], 1
    mov eax, 1
    jmp .try_done

.try_not_connected:
    xor eax, eax

.try_done:
    add rsp, 8
    pop rbx
    ret

;; mcp_spawn_uhma — Spawn UHMA and connect
;; Returns: eax=1 success, 0 failure
mcp_spawn_uhma:
    push rbx
    push r12
    sub rsp, 8

    lea rdi, [rel err_connect]
    xor eax, eax
    call printf

    call mcp_do_spawn
    test eax, eax
    jz .spawn_uhma_fail

    ; Wait for UHMA to start
    mov edi, 2000000                ; 2s
    call usleep

    ; Try to connect
    call mcp_try_connect_all
    mov r12d, eax

    ; Need at least query channel
    test r12d, r12d
    jz .spawn_uhma_fail

    mov dword [rel mcp_running], 1
    mov dword [rel call_id], 2

    lea rdi, [rel msg_connected]
    mov esi, r12d
    xor eax, eax
    call printf

    ; Enable autonomous mode (batch_mode=0)
    mov edi, 500000
    call usleep
    lea rdi, [rel cmd_batch]
    xor esi, esi
    call mcp_call

    lea rdi, [rel msg_autonomous]
    xor eax, eax
    call printf

    mov eax, 1
    jmp .spawn_uhma_done

.spawn_uhma_fail:
    xor eax, eax

.spawn_uhma_done:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; mcp_spawn_uhma_feed — Spawn UHMA for feeding (batch ON, no autonomous mode)
;; Returns: eax=1 success, 0 failure
mcp_spawn_uhma_feed:
    push rbx
    push r12
    sub rsp, 8

    lea rdi, [rel err_connect]
    xor eax, eax
    call printf

    call mcp_do_spawn
    test eax, eax
    jz .spawn_feed_fail

    ; Wait for UHMA to start
    mov edi, 2000000                ; 2s
    call usleep

    ; Try to connect
    call mcp_try_connect_all
    mov r12d, eax

    ; Need at least query channel
    test r12d, r12d
    jz .spawn_feed_fail

    mov dword [rel mcp_running], 1
    mov dword [rel call_id], 2

    lea rdi, [rel msg_connected]
    mov esi, r12d
    xor eax, eax
    call printf

    ; DO NOT send batch command - stay in batch mode ON for feeding

    mov eax, 1
    jmp .spawn_feed_done

.spawn_feed_fail:
    xor eax, eax

.spawn_feed_done:
    add rsp, 8
    pop r12
    pop rbx
    ret

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
    call mcp_try_connect_all
    mov r12d, eax           ; save connected count

    ; If no channels connected, spawn server
    test r12d, r12d
    jnz .some_connected

    ; Connection failed - spawn server
    lea rdi, [rel err_connect]
    xor eax, eax
    call printf

    call mcp_do_spawn
    test eax, eax
    jz .spawn_fail

    ; Wait for server to start
    mov edi, 2000000        ; 2s for multi-channel startup
    call usleep

    ; Try to connect again
    call mcp_try_connect_all
    mov r12d, eax

.some_connected:
    ; Need at least query channel (channel 1)
    cmp dword [rel mcp_sock_valid + 4], 0
    je .spawn_fail

    ; UHMA uses plain text protocol, no JSON-RPC init needed
    mov dword [rel mcp_running], 1
    mov dword [rel call_id], 2

    lea rdi, [rel msg_connected]
    mov esi, r12d           ; channels connected
    xor eax, eax
    call printf

    ; If we spawned UHMA (not connecting to existing), enable autonomous mode
    ; batch_mode=1 by default (batch), sending "batch" toggles to 0 (autonomous)
    cmp dword [rel mcp_pid], 0
    je .skip_autonomous      ; connected to existing UHMA, respect its mode

    ; Small delay for UHMA to be ready
    mov edi, 500000          ; 500ms
    call usleep

    ; Send "batch" to toggle to autonomous mode
    lea rdi, [rel cmd_batch]
    xor esi, esi
    call mcp_call

    lea rdi, [rel msg_autonomous]
    xor eax, eax
    call printf

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
mcp_try_connect_all:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 24             ; 4 pushes (even) + 24 = 56 bytes, aligned
                            ; Use [rsp..rsp+15] for timeval struct

    xor r12d, r12d          ; connected count
    xor r13d, r13d          ; channel index

mcp_tca_loop:
    cmp r13d, MCP_NUM_CHANNELS
    jge mcp_tca_done

    ; Calculate port: base - channel (9999, 9998, 9997, 9996)
    mov eax, MCP_PORT_BASE
    sub eax, r13d
    mov r14d, eax           ; port for this channel

    ; Create socket
    mov edi, AF_INET
    mov esi, SOCK_STREAM
    xor edx, edx
    call socket
    cmp eax, -1
    je mcp_tca_next

    ; Save socket fd
    lea rcx, [rel mcp_sockets]
    mov [rcx + r13 * 4], eax
    mov ebx, eax            ; save socket fd in ebx

    ; Set 1-second connection timeout (prevents 60s freeze)
    mov qword [rsp], 1      ; tv_sec = 1
    mov qword [rsp + 8], 0  ; tv_usec = 0
    mov edi, ebx            ; socket fd
    mov esi, SOL_SOCKET
    mov edx, SO_SNDTIMEO
    lea rcx, [rsp]          ; timeval struct
    mov r8d, 16
    call setsockopt

    ; Setup sockaddr_in
    lea rdi, [rel sock_addr]
    xor esi, esi
    mov edx, 16
    call memset

    mov word [rel sock_addr], AF_INET
    mov edi, r14d           ; port
    call htons
    mov [rel sock_addr + 2], ax
    mov dword [rel sock_addr + 4], 0x0100007f  ; 127.0.0.1

    ; Connect (now with 1s timeout)
    mov edi, ebx            ; socket fd
    lea rsi, [rel sock_addr]
    mov edx, 16
    call connect
    test eax, eax
    jnz .close_failed

    ; Mark channel as valid
    lea rcx, [rel mcp_sock_valid]
    mov dword [rcx + r13 * 4], 1
    inc r12d
    jmp mcp_tca_next

.close_failed:
    mov edi, ebx
    call close

mcp_tca_next:
    inc r13d
    jmp mcp_tca_loop

mcp_tca_done:
    mov eax, r12d
    add rsp, 24
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; Spawn UHMA directly (creates 6 TCP channels itself)
;; Returns: eax=1 success, 0 failure
mcp_do_spawn:
    push rbx
    sub rsp, 16

    ; Use system() to spawn UHMA in background
    lea rdi, [rel spawn_cmd]
    call system
    ; For background command, always assume success
    ; The process is started asynchronously
    mov eax, 1

    add rsp, 16
    pop rbx
    ret

; (JSON-RPC init removed - UHMA uses plain text protocol)

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

;; mcp_call_ch — Call MCP tool on specific channel pair
;; edi = logical channel (0=FEED, 1=QUERY, 2=DEBUG)
;; rsi = tool name (C string)
;; rdx = arguments JSON (C string, can be empty or null)
;; Returns: rax = pointer to response text (in resp_buf)
;;
;; CHANNEL PAIRING: logical_channel maps to socket pair:
;;   FEED(0):  send socket[0] (9999), recv socket[1] (9998)
;;   QUERY(1): send socket[2] (9997), recv socket[3] (9996)
;;   DEBUG(2): send socket[4] (9995), recv socket[5] (9994)
mcp_call_ch:
    push rbx
    push r12
    push r13
    push r14
    push r15
    push rbp
    sub rsp, 8              ; 6 pushes (even) + 8 = 56, aligned

    mov r14d, edi           ; logical channel (0-2)
    mov r12, rsi            ; tool name
    mov r13, rdx            ; args

    ; Map logical channel to socket indices
    ; input_idx = logical_channel * 2
    ; output_idx = logical_channel * 2 + 1
    mov eax, r14d
    shl eax, 1              ; eax = logical * 2 = input socket index
    mov ebp, eax            ; save input index in ebp
    inc eax                 ; eax = output socket index

    ; Check both sockets valid
    lea rcx, [rel mcp_sock_valid]
    cmp dword [rcx + rbp * 4], 0    ; input socket valid?
    je .ch_no_response
    cmp dword [rcx + rax * 4], 0    ; output socket valid?
    je .ch_no_response

    ; Get both socket FDs
    lea rcx, [rel mcp_sockets]
    mov r15d, [rcx + rbp * 4]       ; r15 = input socket (for send)
    mov ebx, [rcx + rax * 4]        ; ebx = output socket (for recv)

    ; Build plain text command: "command args\n"
    ; UHMA expects plain text, not JSON-RPC
    lea rdi, [rel req_buf]
    mov rsi, r12                ; command/tool name
    call strcpy

    ; Add arguments if provided (space-separated)
    test r13, r13
    jz .no_args
    cmp byte [r13], 0
    je .no_args

    ; Add space before args
    lea rdi, [rel req_buf]
    call strlen
    lea rdi, [rel req_buf + rax]
    mov byte [rdi], ' '
    mov byte [rdi + 1], 0

    lea rdi, [rel req_buf]
    mov rsi, r13
    call strcat

.no_args:
    ; Add newline
    lea rdi, [rel req_buf]
    call strlen
    lea rdi, [rel req_buf + rax]
    mov byte [rdi], 10          ; newline
    mov byte [rdi + 1], 0

    ; Send request via TCP on INPUT socket
    lea rdi, [rel req_buf]
    call strlen
    mov rdx, rax                ; length
    mov edi, r15d               ; INPUT socket fd (send here)
    lea rsi, [rel req_buf]
    xor ecx, ecx                ; flags = 0
    call send

    ; Poll OUTPUT socket with short timeout (100ms) to prevent GUI freeze
    mov [rel poll_fd], ebx              ; fd = output socket
    mov word [rel poll_events], 1       ; POLLIN = 1
    mov word [rel poll_revents], 0
    lea rdi, [rel poll_fd]              ; pollfd struct
    mov esi, 1                          ; nfds = 1
    mov edx, 100                        ; timeout = 100ms (was 2000ms)
    call poll

    ; Check if data ready (poll returns > 0 and revents has POLLIN)
    cmp eax, 0
    jle .ch_no_response                 ; timeout or error

    ; Read response via TCP from OUTPUT socket
    mov edi, ebx                ; OUTPUT socket fd (recv here)
    lea rsi, [rel resp_buf]
    mov edx, 65535
    xor ecx, ecx
    call recv
    mov r14, rax                ; bytes read (use r14, ebx is output socket)

    ; Null terminate response and return it
    ; UHMA returns plain text, no JSON parsing needed
    cmp r14, 0
    jle .ch_no_response
    lea rax, [rel resp_buf]
    mov byte [rax + r14], 0
    jmp .ch_done

.ch_no_response:
    ; Return empty string on error
    lea rax, [rel resp_buf]
    mov byte [rax], 0

.ch_done:
    add rsp, 8
    pop rbp
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
    ; UHMA processes text directly at REPL - just send as command
    ; mcp_call sends "command args\n", so text becomes the command
    xor esi, esi            ; no args
    jmp mcp_call

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
    lea rdi, [rel .evolve]
    xor esi, esi
    jmp mcp_call_feed
.evolve: db "evolve", 0

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

;; mcp_send_async — Fire-and-forget send (no wait for response)
;; edi = logical channel (0=FEED, 1=QUERY, 2=DEBUG)
;; rsi = command string
;; Used for periodic polling - response picked up by mcp_read_stream
global mcp_send_async
mcp_send_async:
    push rbx
    push r12
    push r13
    sub rsp, 8

    mov r12d, edi               ; logical channel
    mov r13, rsi                ; command string

    ; Get input socket index: logical * 2
    mov eax, r12d
    shl eax, 1
    mov ebx, eax

    ; Check socket valid
    cmp ebx, MCP_NUM_CHANNELS
    jge .async_done
    lea rcx, [rel mcp_sock_valid]
    cmp dword [rcx + rbx * 4], 0
    je .async_done

    ; Build command with newline
    lea rdi, [rel req_buf]
    mov rsi, r13
    call strcpy
    lea rdi, [rel req_buf]
    call strlen
    lea rdi, [rel req_buf + rax]
    mov byte [rdi], 10
    mov byte [rdi + 1], 0

    ; Send (fire and forget)
    lea rdi, [rel req_buf]
    call strlen
    mov rdx, rax
    lea rcx, [rel mcp_sockets]
    mov edi, [rcx + rbx * 4]
    lea rsi, [rel req_buf]
    xor ecx, ecx
    call send

.async_done:
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    ret

;; mcp_read_stream — Non-blocking read from output channel
;; edi = logical channel (0=FEED, 1=QUERY, 2=DEBUG)
;; rsi = buffer to store data
;; edx = buffer size
;; Returns: eax = bytes read (0 if nothing available)
global mcp_read_stream
mcp_read_stream:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 24

    mov r12d, edi               ; logical channel
    mov r13, rsi                ; buffer
    mov r14d, edx               ; size

    ; Get output socket index: logical * 2 + 1
    mov eax, r12d
    shl eax, 1
    inc eax                     ; output socket index
    mov ebx, eax

    ; Check socket valid
    cmp ebx, MCP_NUM_CHANNELS
    jge .no_data
    lea rcx, [rel mcp_sock_valid]
    cmp dword [rcx + rbx * 4], 0
    je .no_data

    ; Get socket fd
    lea rcx, [rel mcp_sockets]
    mov edi, [rcx + rbx * 4]

    ; Poll with 0 timeout (non-blocking check)
    mov [rsp], edi              ; pollfd.fd
    mov word [rsp + 4], 1       ; POLLIN
    mov word [rsp + 6], 0       ; revents

    lea rdi, [rsp]
    mov esi, 1                  ; nfds
    xor edx, edx                ; timeout = 0 (non-blocking)
    call poll

    cmp eax, 0
    jle .no_data

    ; Check if POLLIN set
    movzx eax, word [rsp + 6]
    test eax, 1                 ; POLLIN
    jz .no_data

    ; Data available - recv it
    lea rcx, [rel mcp_sockets]
    mov edi, [rcx + rbx * 4]
    mov rsi, r13                ; buffer
    mov edx, r14d               ; size
    xor ecx, ecx                ; flags
    call recv

    test eax, eax
    jle .no_data
    jmp .done

.no_data:
    xor eax, eax

.done:
    add rsp, 24
    pop r14
    pop r13
    pop r12
    pop rbx
    ret
