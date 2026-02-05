; mcp_client.asm — GUI client for UHMA single-port gateway
;
; @entry mcp_init() -> eax=1 success, 0 failure (connects to gateway port 9999)
; @entry mcp_shutdown() -> closes socket
; @entry mcp_call(rdi=tool_name, rsi=args_json) -> rax=response_ptr
; @entry mcp_call_ch(edi=channel, rsi=tool_name, rdx=args_json) -> rax=response_ptr
; @entry mcp_send_text(rdi=text) -> rax=response_ptr
; @entry mcp_get_status() -> rax=status_json_ptr
; @entry mcp_spawn_uhma() -> eax=1 success, spawns UHMA + sends "batch" for live mode
; @entry mcp_spawn_uhma_feed() -> eax=1 success, spawns UHMA without batch toggle
; @entry mcp_read_stream(edi=channel, rsi=buf, edx=size) -> eax=bytes_read
; @entry mcp_send_async(edi=channel, rsi=command) -> fire-and-forget
; @global mcp_running -> dword, 1 if UHMA connected
; @calledby gui/visualizer.asm
;
; GATEWAY PROTOCOL:
;   Frame: [2B magic 0x5548] [1B subnet] [1B host] [2B seq_id] [2B payload_len] [payload]
;   Single socket to port 9999, framed messages, multiplexed by seq_id
;
; CHANNEL→SUBNET MAPPING:
;   MCP_CH_FEED  (0) → SUBNET_CONSOL (4)
;   MCP_CH_QUERY (1) → SUBNET_REPL   (1)
;   MCP_CH_DEBUG (2) → SUBNET_SELF   (5)
;
; GOTCHAS:
;   - Response buffer is static, overwritten each call
;   - Stack alignment: ODD pushes → sub rsp must be multiple of 16
;   - Spawn uses absolute path /home/peter/Desktop/STARWARS/uhma-asm/uhma

section .data
    ; UHMA spawn command
    spawn_cmd:      db "cd /home/peter/Desktop/STARWARS/uhma-asm && ./uhma < /dev/null &", 0

    ; Gateway connection params
    GW_PORT          equ 9999
    GW_MAGIC         equ 0x5548
    GW_HEADER_SIZE   equ 8
    GW_MAX_PAYLOAD   equ 4096
    AF_INET          equ 2
    SOCK_STREAM      equ 1
    SOL_SOCKET       equ 1
    SO_SNDTIMEO      equ 21

    ; Logical channel → subnet mapping
    MCP_CH_FEED      equ 0
    MCP_CH_QUERY     equ 1
    MCP_CH_DEBUG     equ 2
    SUBNET_REPL      equ 1
    SUBNET_CONSOL    equ 4
    SUBNET_SELF      equ 5

    ; Channel to subnet lookup table
    ch_to_subnet:   db SUBNET_CONSOL    ; CH_FEED  (0) → SUBNET_CONSOL (4)
                    db SUBNET_REPL      ; CH_QUERY (1) → SUBNET_REPL   (1)
                    db SUBNET_SELF      ; CH_DEBUG (2) → SUBNET_SELF   (5)
                    db SUBNET_REPL      ; fallback

    ; Messages
    err_connect:    db "GUI: spawning UHMA...", 10, 0
    err_spawn:      db "GUI: failed to spawn UHMA (try ./feed.sh first)", 10, 0
    msg_connected:  db "GUI: connected to UHMA gateway", 10, 0
    msg_autonomous: db "GUI: enabled autonomous mode (batch_mode=0)", 10, 0
    cmd_batch:      db "batch", 0

section .bss
    ; Single gateway socket
    mcp_socket:       resd 1            ; gateway socket FD (-1 = not connected)
    mcp_running:      resd 1
    mcp_pid:          resd 1
    mcp_seq_counter:  resw 1            ; frame sequence ID counter

    ; sockaddr_in structure (16 bytes)
    sock_addr:        resb 16

    ; Request/response buffers
    req_buf:        resb GW_HEADER_SIZE + GW_MAX_PAYLOAD  ; frame header + payload
    resp_buf:       resb 65536
    status_cache:   resb 8192

    ; pollfd struct: { int fd; short events; short revents; }
    poll_fd:        resd 1
    poll_events:    resw 1
    poll_revents:   resw 1

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
global mcp_send_async
global mcp_read_stream

;; ============================================================
;; mcp_connect_gateway — Connect single socket to port 9999
;; Returns: eax=1 connected, 0 failed
;; ============================================================
mcp_connect_gateway:
    push rbx
    push r12
    sub rsp, 24                 ; 2 pushes (even) + 24 → 56, need 8 mod 16
                                ; 2*8=16+24=40... 40 mod 16 = 8. aligned.

    ; Create socket
    mov edi, AF_INET
    mov esi, SOCK_STREAM
    xor edx, edx
    call socket
    cmp eax, -1
    je .gw_connect_fail

    mov ebx, eax                ; save socket fd
    mov [rel mcp_socket], eax

    ; Set 1-second connection timeout
    mov qword [rsp], 1          ; tv_sec = 1
    mov qword [rsp + 8], 0      ; tv_usec = 0
    mov edi, ebx
    mov esi, SOL_SOCKET
    mov edx, SO_SNDTIMEO
    lea rcx, [rsp]
    mov r8d, 16
    call setsockopt

    ; Setup sockaddr_in for 127.0.0.1:9999
    lea rdi, [rel sock_addr]
    xor esi, esi
    mov edx, 16
    call memset

    mov word [rel sock_addr], AF_INET
    mov edi, GW_PORT
    call htons
    mov [rel sock_addr + 2], ax
    mov dword [rel sock_addr + 4], 0x0100007f  ; 127.0.0.1

    ; Connect
    mov edi, ebx
    lea rsi, [rel sock_addr]
    mov edx, 16
    call connect
    test eax, eax
    jnz .gw_connect_close

    ; Success
    mov eax, 1
    jmp .gw_connect_done

.gw_connect_close:
    mov edi, ebx
    call close
    mov dword [rel mcp_socket], -1
.gw_connect_fail:
    xor eax, eax

.gw_connect_done:
    add rsp, 24
    pop r12
    pop rbx
    ret

;; ============================================================
;; mcp_try_connect — Try to connect to existing UHMA (no spawn)
;; Returns: eax=1 if connected, 0 if not
;; ============================================================
mcp_try_connect:
    push rbx
    ; 1 push (odd) = aligned

    call mcp_connect_gateway
    mov ebx, eax

    test ebx, ebx
    jz .try_not_connected

    mov dword [rel mcp_running], 1
    mov eax, 1
    jmp .try_done

.try_not_connected:
    xor eax, eax

.try_done:
    pop rbx
    ret

;; ============================================================
;; mcp_spawn_uhma — Spawn UHMA and connect (autonomous mode)
;; Returns: eax=1 success, 0 failure
;; ============================================================
mcp_spawn_uhma:
    push rbx
    push r12
    sub rsp, 8                  ; 2 pushes (even) + 8 = aligned

    lea rdi, [rel err_connect]
    xor eax, eax
    call printf

    call mcp_do_spawn
    test eax, eax
    jz .spawn_uhma_fail

    ; Wait for UHMA to start
    mov edi, 2000000            ; 2s
    call usleep

    ; Try to connect
    call mcp_connect_gateway
    test eax, eax
    jz .spawn_uhma_fail

    mov dword [rel mcp_running], 1
    mov word [rel mcp_seq_counter], 1

    lea rdi, [rel msg_connected]
    xor eax, eax
    call printf

    ; Enable autonomous mode
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

;; ============================================================
;; mcp_spawn_uhma_feed — Spawn UHMA for feeding (no autonomous)
;; Returns: eax=1 success, 0 failure
;; ============================================================
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

    mov edi, 2000000
    call usleep

    call mcp_connect_gateway
    test eax, eax
    jz .spawn_feed_fail

    mov dword [rel mcp_running], 1
    mov word [rel mcp_seq_counter], 1

    lea rdi, [rel msg_connected]
    xor eax, eax
    call printf

    mov eax, 1
    jmp .spawn_feed_done

.spawn_feed_fail:
    xor eax, eax

.spawn_feed_done:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; mcp_init — Connect to UHMA gateway, spawn if needed
;; Returns: eax=1 success, 0 failure
;; ============================================================
mcp_init:
    push rbx
    push r12
    sub rsp, 8

    mov dword [rel mcp_socket], -1

    ; Try to connect
    call mcp_connect_gateway
    mov r12d, eax

    test r12d, r12d
    jnz .init_connected

    ; Not running — spawn
    lea rdi, [rel err_connect]
    xor eax, eax
    call printf

    call mcp_do_spawn
    test eax, eax
    jz .init_fail

    mov edi, 2000000
    call usleep

    call mcp_connect_gateway
    test eax, eax
    jz .init_fail

.init_connected:
    mov dword [rel mcp_running], 1
    mov word [rel mcp_seq_counter], 1

    lea rdi, [rel msg_connected]
    xor eax, eax
    call printf

    ; If we spawned UHMA, enable autonomous mode
    cmp dword [rel mcp_pid], 0
    je .skip_autonomous

    mov edi, 500000
    call usleep
    lea rdi, [rel cmd_batch]
    xor esi, esi
    call mcp_call

    lea rdi, [rel msg_autonomous]
    xor eax, eax
    call printf

.skip_autonomous:
    mov eax, 1
    jmp .init_done

.init_fail:
    lea rdi, [rel err_spawn]
    xor eax, eax
    call printf
    xor eax, eax

.init_done:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; mcp_do_spawn — Spawn UHMA in background
;; Returns: eax=1 success
;; ============================================================
mcp_do_spawn:
    push rbx
    sub rsp, 16                 ; 1 push (odd) + 16 = aligned

    lea rdi, [rel spawn_cmd]
    call system
    mov eax, 1

    add rsp, 16
    pop rbx
    ret

;; ============================================================
;; mcp_get_channel_socket — Get gateway socket FD
;; edi = channel number (ignored, returns gateway socket)
;; Returns: eax = socket FD, or -1 if not connected
;; ============================================================
mcp_get_channel_socket:
    mov eax, [rel mcp_socket]
    ret

;; ============================================================
;; mcp_shutdown — Close gateway socket, optionally kill UHMA
;; ============================================================
mcp_shutdown:
    push rbx
    push r12
    sub rsp, 8

    cmp dword [rel mcp_running], 0
    je .shut_done

    ; Send quit
    lea rdi, [rel .quit_tool]
    xor esi, esi
    call mcp_call

    ; Close gateway socket
    mov edi, [rel mcp_socket]
    cmp edi, -1
    je .no_close
    call close
    mov dword [rel mcp_socket], -1
.no_close:

    ; Kill spawned server if we started one
    mov edi, [rel mcp_pid]
    test edi, edi
    jz .no_kill
    mov esi, 15                 ; SIGTERM
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

;; ============================================================
;; mcp_call — Call via QUERY channel (SUBNET_REPL)
;; rdi = tool name, rsi = args (can be NULL)
;; Returns: rax = pointer to response text
;; ============================================================
mcp_call:
    mov rdx, rsi
    mov rsi, rdi
    mov edi, MCP_CH_QUERY
    jmp mcp_call_ch

;; ============================================================
;; mcp_call_feed — Call via FEED channel (SUBNET_CONSOL)
;; rdi = tool name, rsi = args
;; Returns: rax = pointer to response text
;; ============================================================
mcp_call_feed:
    mov rdx, rsi
    mov rsi, rdi
    mov edi, MCP_CH_FEED
    jmp mcp_call_ch

;; ============================================================
;; mcp_call_ch — Send framed command on logical channel, wait for response
;; edi = logical channel (0=FEED, 1=QUERY, 2=DEBUG)
;; rsi = tool name (C string)
;; rdx = arguments (C string, can be NULL)
;; Returns: rax = pointer to response text (in resp_buf)
;; ============================================================
mcp_call_ch:
    push rbx
    push r12
    push r13
    push r14
    push r15
    push rbp
    sub rsp, 8                  ; 6 pushes (even) + 8 = aligned

    mov r14d, edi               ; logical channel
    mov r12, rsi                ; tool name
    mov r13, rdx                ; args

    ; Check connected
    mov eax, [rel mcp_socket]
    cmp eax, -1
    je .ch_no_response
    mov ebx, eax                ; ebx = socket fd

    ; Build payload in req_buf + GW_HEADER_SIZE (leave room for header)
    lea rdi, [rel req_buf + GW_HEADER_SIZE]
    mov rsi, r12                ; command
    call strcpy

    ; Add args if provided
    test r13, r13
    jz .no_args
    cmp byte [r13], 0
    je .no_args

    lea rdi, [rel req_buf + GW_HEADER_SIZE]
    call strlen
    lea rdi, [rel req_buf + GW_HEADER_SIZE + rax]
    mov byte [rdi], ' '
    mov byte [rdi + 1], 0

    lea rdi, [rel req_buf + GW_HEADER_SIZE]
    mov rsi, r13
    call strcat

.no_args:
    ; Add newline
    lea rdi, [rel req_buf + GW_HEADER_SIZE]
    call strlen
    lea rdi, [rel req_buf + GW_HEADER_SIZE + rax]
    mov byte [rdi], 10
    mov byte [rdi + 1], 0
    inc eax                     ; payload_len includes newline
    mov r15d, eax               ; r15d = payload length

    ; Get subnet for this channel
    lea rcx, [rel ch_to_subnet]
    cmp r14d, 3
    jge .use_default_subnet
    movzx ebp, byte [rcx + r14] ; ebp = subnet
    jmp .build_header
.use_default_subnet:
    mov ebp, SUBNET_REPL
.build_header:

    ; Increment seq counter
    movzx eax, word [rel mcp_seq_counter]
    inc eax
    mov [rel mcp_seq_counter], ax
    mov r14d, eax               ; r14d = seq_id (reuse r14, channel no longer needed)

    ; Build frame header at req_buf
    lea rcx, [rel req_buf]
    mov word [rcx], GW_MAGIC        ; magic
    mov [rcx + 2], bpl              ; subnet
    mov byte [rcx + 3], 0           ; host
    mov [rcx + 4], r14w             ; seq_id
    mov [rcx + 6], r15w             ; payload_len

    ; Send frame (header + payload)
    mov eax, r15d
    add eax, GW_HEADER_SIZE         ; total frame size
    mov edx, eax
    mov edi, ebx                    ; socket fd
    lea rsi, [rel req_buf]
    xor ecx, ecx                    ; flags = 0
    call send

    ; Poll for response (100ms timeout)
    mov [rel poll_fd], ebx
    mov word [rel poll_events], 1   ; POLLIN
    mov word [rel poll_revents], 0
    lea rdi, [rel poll_fd]
    mov esi, 1
    mov edx, 100                    ; 100ms
    call poll

    cmp eax, 0
    jle .ch_no_response

    ; Read response frame
    mov edi, ebx
    lea rsi, [rel resp_buf]
    mov edx, 65535
    xor ecx, ecx
    call recv
    mov r15, rax                    ; bytes received

    cmp r15, GW_HEADER_SIZE
    jl .ch_no_response

    ; Check if framed response (has magic header)
    lea rax, [rel resp_buf]
    movzx ecx, word [rax]
    cmp cx, GW_MAGIC
    jne .ch_legacy_response

    ; Parse frame: extract payload after header
    movzx ecx, word [rax + 6]      ; payload_len from header
    mov rdx, r15
    sub rdx, GW_HEADER_SIZE         ; actual payload bytes
    cmp ecx, edx
    jle .payload_ok
    mov ecx, edx                    ; clamp
.payload_ok:
    ; Move payload to beginning of resp_buf for caller
    lea rdi, [rel resp_buf]
    lea rsi, [rel resp_buf + GW_HEADER_SIZE]
    mov edx, ecx
    push rcx
    call memcpy
    pop rcx
    lea rax, [rel resp_buf]
    mov byte [rax + rcx], 0         ; null terminate
    jmp .ch_done

.ch_legacy_response:
    ; Server sent unframed response (legacy compat)
    lea rax, [rel resp_buf]
    mov byte [rax + r15], 0         ; null terminate
    jmp .ch_done

.ch_no_response:
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

;; ============================================================
;; mcp_send_text — Send text input to UHMA
;; rdi = text, Returns: rax = response pointer
;; ============================================================
mcp_send_text:
    xor esi, esi
    jmp mcp_call

;; ============================================================
;; Convenience wrappers
;; ============================================================

mcp_dream:
    lea rdi, [rel .dream]
    xor esi, esi
    jmp mcp_call_feed
.dream: db "dream", 0

mcp_observe:
    lea rdi, [rel .observe]
    xor esi, esi
    jmp mcp_call_feed
.observe: db "observe", 0

mcp_evolve:
    lea rdi, [rel .evolve]
    xor esi, esi
    jmp mcp_call_feed
.evolve: db "evolve", 0

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

mcp_get_status:
    lea rdi, [rel .status]
    xor esi, esi
    call mcp_call
    ret
.status: db "status", 0

;; ============================================================
;; mcp_send_async — Fire-and-forget framed send
;; edi = logical channel (0=FEED, 1=QUERY, 2=DEBUG)
;; rsi = command string
;; ============================================================
mcp_send_async:
    push rbx
    push r12
    push r13
    sub rsp, 16                 ; 3 pushes (odd) + 16 = aligned

    mov r12d, edi               ; logical channel
    mov r13, rsi                ; command string

    ; Check connected
    mov eax, [rel mcp_socket]
    cmp eax, -1
    je .async_done
    mov ebx, eax

    ; Build payload
    lea rdi, [rel req_buf + GW_HEADER_SIZE]
    mov rsi, r13
    call strcpy
    lea rdi, [rel req_buf + GW_HEADER_SIZE]
    call strlen
    lea rdi, [rel req_buf + GW_HEADER_SIZE + rax]
    mov byte [rdi], 10
    mov byte [rdi + 1], 0
    inc eax                     ; include newline
    mov r13d, eax               ; payload len

    ; Get subnet
    lea rcx, [rel ch_to_subnet]
    cmp r12d, 3
    jge .async_default
    movzx r12d, byte [rcx + r12]
    jmp .async_header
.async_default:
    mov r12d, SUBNET_REPL
.async_header:

    ; Increment seq
    movzx eax, word [rel mcp_seq_counter]
    inc eax
    mov [rel mcp_seq_counter], ax

    ; Build frame header
    lea rcx, [rel req_buf]
    mov word [rcx], GW_MAGIC
    mov [rcx + 2], r12b             ; subnet
    mov byte [rcx + 3], 0
    mov [rcx + 4], ax               ; seq_id
    mov [rcx + 6], r13w             ; payload_len

    ; Send
    mov eax, r13d
    add eax, GW_HEADER_SIZE
    mov edx, eax
    mov edi, ebx
    lea rsi, [rel req_buf]
    xor ecx, ecx
    call send

.async_done:
    add rsp, 16
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; mcp_read_stream — Non-blocking read from gateway
;; edi = logical channel (0=FEED, 1=QUERY, 2=DEBUG) — currently ignored
;; rsi = buffer to store payload data
;; edx = buffer size
;; Returns: eax = bytes read (0 if nothing available)
;; ============================================================
mcp_read_stream:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 24                 ; 4 pushes (even) + 24 = 56, need 8 mod 16
                                ; 4*8=32+24=56, 56 mod 16 = 8. aligned.

    mov r12d, edi               ; channel (for future subnet filtering)
    mov r13, rsi                ; output buffer
    mov r14d, edx               ; buffer size

    ; Check connected
    mov eax, [rel mcp_socket]
    cmp eax, -1
    je .stream_no_data
    mov ebx, eax

    ; Poll with 0 timeout (non-blocking)
    mov [rsp], ebx              ; pollfd.fd
    mov word [rsp + 4], 1       ; POLLIN
    mov word [rsp + 6], 0       ; revents

    lea rdi, [rsp]
    mov esi, 1
    xor edx, edx                ; timeout = 0
    call poll

    cmp eax, 0
    jle .stream_no_data

    movzx eax, word [rsp + 6]
    test eax, 1                 ; POLLIN
    jz .stream_no_data

    ; Data available — read into resp_buf first (may be framed)
    mov edi, ebx
    lea rsi, [rel resp_buf]
    mov edx, 65535
    xor ecx, ecx
    call recv

    test eax, eax
    jle .stream_no_data

    mov ebx, eax                ; total bytes received

    ; Check if framed
    cmp ebx, GW_HEADER_SIZE
    jl .stream_raw

    lea rcx, [rel resp_buf]
    movzx edx, word [rcx]
    cmp dx, GW_MAGIC
    jne .stream_raw

    ; Framed — extract payload
    movzx eax, word [rcx + 6]  ; payload_len
    mov edx, ebx
    sub edx, GW_HEADER_SIZE
    cmp eax, edx
    jle .stream_payload_ok
    mov eax, edx
.stream_payload_ok:
    ; Clamp to caller buffer
    cmp eax, r14d
    jle .stream_copy_framed
    mov eax, r14d
.stream_copy_framed:
    push rax
    mov rdi, r13                ; dst
    lea rsi, [rel resp_buf + GW_HEADER_SIZE]  ; src
    mov edx, eax
    call memcpy
    pop rax
    jmp .stream_done

.stream_raw:
    ; Unframed — copy as-is
    mov eax, ebx
    cmp eax, r14d
    jle .stream_copy_raw
    mov eax, r14d
.stream_copy_raw:
    push rax
    mov rdi, r13
    lea rsi, [rel resp_buf]
    mov edx, eax
    call memcpy
    pop rax
    jmp .stream_done

.stream_no_data:
    xor eax, eax

.stream_done:
    add rsp, 24
    pop r14
    pop r13
    pop r12
    pop rbx
    ret
