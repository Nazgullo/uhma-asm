; feeder.asm — TCP training client for UHMA (assembly-only)
;
; @entry _start                   ; main entry point
; @calls UHMA TCP gateway port 9999 (single connection)
; @drains gateway responses (single socket)
;
; COMMAND LINE:
;   ./feeder [OPTIONS]
;     --corpus DIR      Directory with .txt files (default: corpus/)
;     --pause N         Seconds between files (default: 5)
;     --consolidate N   Minutes between observe+dream (default: 30)
;     --cycles N        Number of cycles, 0=infinite (default: 1)
;     --shutdown        Graceful shutdown: save + quit
;     --help            Show usage
;
; PROTOCOL:
;   1. Connect to UHMA gateway (port 9999, single socket)
;   2. Send commands and read responses on same socket
;   3. Poll drainers to prevent UHMA blocking
;   4. Fire and forget - responses handled by drain loop
;
; GOTCHAS:
;   - Gateway responses MUST be drained continuously or UHMA blocks
;   - Use poll() for multiplexed I/O, not threads
;   - getdents64 for directory scanning (not opendir/readdir)
;   - Connect failures retry with backoff
;
%include "syscalls.inc"

section .data
    ; Defaults
    default_corpus:  db "corpus/", 0
    default_pause:   equ 5              ; seconds
    default_cons:    equ 30             ; minutes
    default_cycles:  equ 1

    ; UHMA gateway port (single connection)
    GW_PORT:         equ 9999
    GW_MAGIC:        equ 0x5548
    GW_HEADER_SIZE:  equ 8
    GW_MAX_PAYLOAD:  equ 1024
    SUBNET_REPL:     equ 1
    SUBNET_CONSOL:   equ 4

    ; Messages
    msg_start:       db "[FEEDER] Starting UHMA training client", 10, 0
    msg_connect:     db "[FEEDER] Connecting to UHMA...", 10, 0
    msg_connected:   db "[FEEDER] Connected to gateway", 10, 0
    msg_conn_fail:   db "[FEEDER] Connection failed, retrying...", 10, 0
    msg_feeding:     db "[FEEDER] Feeding: ", 0
    msg_cycle:       db "[FEEDER] === CYCLE ", 0
    msg_cycle_end:   db " ===", 10, 0
    msg_consolidate: db "[FEEDER] Consolidating (observe + dream)", 10, 0
    msg_shutdown:    db "[FEEDER] Graceful shutdown...", 10, 0
    msg_done:        db "[FEEDER] Done", 10, 0
    msg_no_corpus:   db "[FEEDER] ERROR: Corpus directory not found", 10, 0
    msg_usage:       db "Usage: feeder [OPTIONS]", 10
                     db "  --corpus DIR     Training corpus (default: corpus/)", 10
                     db "  --pause N        Seconds between files (default: 5)", 10
                     db "  --consolidate N  Minutes between consolidation (default: 30)", 10
                     db "  --cycles N       Number of cycles (default: 1, 0=infinite)", 10
                     db "  --spawn          Spawn UHMA if not running", 10
                     db "  --shutdown       Send save+quit to running UHMA", 10
                     db "  --help           Show this help", 10, 0
    newline:         db 10, 0

    ; Commands to send
    cmd_eat:         db "eat ", 0
    cmd_observe:     db "observe", 10, 0
    cmd_dream:       db "dream", 10, 0
    cmd_save:        db "save feeder_checkpoint", 10, 0
    cmd_quit:        db "quit", 10, 0
    cmd_status:      db "status", 10, 0

    ; Argument strings
    arg_corpus:      db "--corpus", 0
    arg_pause:       db "--pause", 0
    arg_consolidate: db "--consolidate", 0
    arg_cycles:      db "--cycles", 0
    arg_shutdown:    db "--shutdown", 0
    arg_spawn:       db "--spawn", 0
    arg_help:        db "--help", 0

    ; UHMA binary path for spawn
    uhma_path:       db "./uhma", 0
    uhma_argv0:      db "uhma", 0
    dev_null:        db "/dev/null", 0
    msg_spawning:    db "[FEEDER] Spawning UHMA...", 10, 0
    msg_spawned:     db "[FEEDER] UHMA spawned, waiting for ports...", 10, 0
    msg_spawn_fail:  db "[FEEDER] Failed to spawn UHMA", 10, 0

section .bss
    ; Configuration
    corpus_path:     resq 1             ; pointer to corpus directory
    pause_secs:      resd 1             ; seconds between files
    consolidate_min: resd 1             ; minutes between consolidation
    max_cycles:      resd 1             ; number of cycles (0=infinite)
    do_shutdown:     resd 1             ; just shutdown flag
    do_spawn:        resd 1             ; spawn UHMA if not running
    uhma_pid:        resd 1             ; PID of spawned UHMA

    ; Gateway socket (single fd)
    gw_fd:           resd 1             ; gateway socket fd
    gw_seq_counter:  resw 1             ; framed seq counter

    ; Directory scanning
    dir_fd:          resd 1
    dir_buf:         resb 8192          ; getdents64 buffer
    file_path:       resb 512           ; full file path buffer

    ; Timing
    last_consolidate: resq 1            ; unix timestamp
    current_time:    resq 2             ; timeval struct

    ; Poll structure (single fd)
    ; struct pollfd { int fd; short events; short revents; } = 8 bytes
    poll_fds:        resb 8

    ; Send buffer
    send_buf:        resb 1024

    ; Framed buffer (header + payload)
    gw_frame_buf:    resb GW_HEADER_SIZE + GW_MAX_PAYLOAD

    ; Drain buffer
    drain_buf:       resb 4096

section .text

global _start

;; ============================================================
;; _start — Entry point
;; ============================================================
_start:
    ; Initialize defaults
    lea rax, [rel default_corpus]
    mov [rel corpus_path], rax
    mov dword [rel pause_secs], default_pause
    mov dword [rel consolidate_min], default_cons
    mov dword [rel max_cycles], default_cycles
    mov dword [rel do_shutdown], 0
    mov dword [rel do_spawn], 0
    mov dword [rel uhma_pid], 0

    ; Parse command line
    ; Stack on entry: [argc] [argv0] [argv1] ...
    mov rdi, [rsp]                      ; argc
    lea r12, [rsp + 8]                  ; r12 = &argv[0]
    add r12, 8                          ; skip program name, r12 = &argv[1]
    dec edi                             ; argc - 1
    jz .args_done

.parse_args:
    test edi, edi
    jz .args_done

    mov rsi, [r12]                      ; current arg
    add r12, 8                          ; advance to next
    dec edi                             ; decrement remaining

    ; Check --help
    push rdi
    push r12
    lea rdi, [rel arg_help]
    call strcmp
    pop r12
    pop rdi
    test eax, eax
    jz .show_usage

    ; Check --shutdown
    push rdi
    push r12
    lea rdi, [rel arg_shutdown]
    mov rsi, [r12 - 8]                  ; restore current arg
    call strcmp
    pop r12
    pop rdi
    test eax, eax
    jz .set_shutdown

    ; Check --spawn
    push rdi
    push r12
    lea rdi, [rel arg_spawn]
    mov rsi, [r12 - 8]
    call strcmp
    pop r12
    pop rdi
    test eax, eax
    jz .set_spawn

    ; Check --corpus
    push rdi
    push r12
    lea rdi, [rel arg_corpus]
    mov rsi, [r12 - 8]
    call strcmp
    pop r12
    pop rdi
    test eax, eax
    jz .parse_corpus

    ; Check --pause
    push rdi
    push r12
    lea rdi, [rel arg_pause]
    mov rsi, [r12 - 8]
    call strcmp
    pop r12
    pop rdi
    test eax, eax
    jz .parse_pause

    ; Check --consolidate
    push rdi
    push r12
    lea rdi, [rel arg_consolidate]
    mov rsi, [r12 - 8]
    call strcmp
    pop r12
    pop rdi
    test eax, eax
    jz .parse_consolidate

    ; Check --cycles
    push rdi
    push r12
    lea rdi, [rel arg_cycles]
    mov rsi, [r12 - 8]
    call strcmp
    pop r12
    pop rdi
    test eax, eax
    jz .parse_cycles

    jmp .parse_args

.parse_corpus:
    test edi, edi
    jz .args_done
    mov rax, [r12]                      ; value arg
    add r12, 8
    dec edi
    mov [rel corpus_path], rax
    jmp .parse_args

.parse_pause:
    test edi, edi
    jz .args_done
    mov rsi, [r12]
    add r12, 8
    dec edi
    push rdi
    push r12
    call atoi
    pop r12
    pop rdi
    mov [rel pause_secs], eax
    jmp .parse_args

.parse_consolidate:
    test edi, edi
    jz .args_done
    mov rsi, [r12]
    add r12, 8
    dec edi
    push rdi
    push r12
    call atoi
    pop r12
    pop rdi
    mov [rel consolidate_min], eax
    jmp .parse_args

.parse_cycles:
    test edi, edi
    jz .args_done
    mov rsi, [r12]
    add r12, 8
    dec edi
    push rdi
    push r12
    call atoi
    pop r12
    pop rdi
    mov [rel max_cycles], eax
    jmp .parse_args

.set_shutdown:
    mov dword [rel do_shutdown], 1
    jmp .parse_args

.set_spawn:
    mov dword [rel do_spawn], 1
    jmp .parse_args

.show_usage:
    lea rdi, [rel msg_usage]
    call print_str
    xor edi, edi
    jmp exit

.args_done:
    ; Print startup message
    lea rdi, [rel msg_start]
    call print_str

    ; Try to connect to UHMA
    lea rdi, [rel msg_connect]
    call print_str
    call connect_all
    test eax, eax
    jnz .connected_ok

    ; Connection failed - check if we should spawn
    cmp dword [rel do_spawn], 1
    jne .connect_failed

    ; Spawn UHMA
    call spawn_uhma
    test eax, eax
    jz .connect_failed

    ; Wait for UHMA to start listening
    mov edi, 3                          ; wait up to 3 seconds
    call wait_for_uhma
    test eax, eax
    jz .connect_failed

.connected_ok:
    lea rdi, [rel msg_connected]
    call print_str

    ; Check if just shutdown
    cmp dword [rel do_shutdown], 1
    je .do_shutdown

    ; Initialize timing
    call get_time
    mov [rel last_consolidate], rax

    ; Main training loop
    call training_loop

    jmp .exit_ok

.do_shutdown:
    lea rdi, [rel msg_shutdown]
    call print_str

    ; Send save command
    lea rsi, [rel cmd_save]
    mov edi, SUBNET_CONSOL
    call send_cmd

    ; Wait a bit
    mov edi, 2
    call sleep_sec

    ; Send quit command
    lea rsi, [rel cmd_quit]
    mov edi, SUBNET_CONSOL
    call send_cmd

    ; Drain any pending output
    mov edi, 100
    call drain_outputs

.exit_ok:
    lea rdi, [rel msg_done]
    call print_str
    xor edi, edi
    jmp exit

.connect_failed:
    lea rdi, [rel msg_conn_fail]
    call print_str
    mov edi, 1
    jmp exit

;; ============================================================
;; connect_all — Connect to UHMA gateway (single socket)
;; Returns: eax=1 success, 0 failure
;; ============================================================
connect_all:
    push rbx
    push r12
    push r13
    sub rsp, 8

    ; Connect to gateway port 9999
    mov edi, GW_PORT
    call connect_port
    test eax, eax
    js .conn_fail

    ; Store gateway socket
    mov [rel gw_fd], eax
    mov word [rel gw_seq_counter], 0

    mov eax, 1
    jmp .conn_ret

.conn_fail:
    xor eax, eax

.conn_ret:
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; connect_port — Connect to localhost:port
;; edi = port number
;; Returns: eax = socket fd, or -1 on failure
;; ============================================================
connect_port:
    push rbx
    push r12
    sub rsp, 24                         ; sockaddr_in + alignment

    mov r12d, edi                       ; save port

    ; socket(AF_INET, SOCK_STREAM, 0)
    mov eax, SYS_SOCKET
    mov edi, AF_INET
    mov esi, SOCK_STREAM
    xor edx, edx
    syscall
    test eax, eax
    js .port_fail
    mov ebx, eax                        ; save fd

    ; Setup sockaddr_in
    mov word [rsp], AF_INET             ; sin_family
    mov eax, r12d
    xchg al, ah                         ; htons
    mov [rsp + 2], ax                   ; sin_port
    mov dword [rsp + 4], INADDR_LOOPBACK  ; 127.0.0.1

    ; connect(fd, &addr, 16)
    mov eax, SYS_CONNECT
    mov edi, ebx
    lea rsi, [rsp]
    mov edx, 16
    syscall
    test eax, eax
    js .port_close_fail

    mov eax, ebx                        ; return fd
    jmp .port_ret

.port_close_fail:
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
;; spawn_uhma — Fork and exec UHMA in background
;; Returns: eax = 1 on success, 0 on failure
;; ============================================================
spawn_uhma:
    push rbx
    push r12
    sub rsp, 40                         ; space for argv array

    lea rdi, [rel msg_spawning]
    call print_str

    ; Fork
    mov eax, SYS_FORK
    syscall
    test eax, eax
    js .spawn_fail
    jnz .spawn_parent

    ; === CHILD PROCESS ===
    ; Close stdin, redirect to /dev/null
    mov eax, SYS_CLOSE
    xor edi, edi                        ; close stdin
    syscall

    ; Open /dev/null for stdin
    lea rdi, [rel dev_null]
    mov esi, O_RDONLY
    xor edx, edx
    mov eax, SYS_OPEN
    syscall
    ; Now /dev/null is fd 0

    ; Setup argv: ["uhma", NULL]
    lea rax, [rel uhma_argv0]
    mov [rsp], rax
    mov qword [rsp + 8], 0

    ; execve("./uhma", argv, NULL)
    lea rdi, [rel uhma_path]
    lea rsi, [rsp]                      ; argv
    xor edx, edx                        ; envp = NULL
    mov eax, SYS_EXECVE
    syscall

    ; If we get here, execve failed
    mov edi, 1
    mov eax, SYS_EXIT
    syscall

.spawn_parent:
    ; Save child PID
    mov [rel uhma_pid], eax

    lea rdi, [rel msg_spawned]
    call print_str

    mov eax, 1
    jmp .spawn_ret

.spawn_fail:
    lea rdi, [rel msg_spawn_fail]
    call print_str
    xor eax, eax

.spawn_ret:
    add rsp, 40
    pop r12
    pop rbx
    ret

;; ============================================================
;; wait_for_uhma — Wait for UHMA to start listening
;; edi = max seconds to wait
;; Returns: eax = 1 if connected, 0 if timeout
;; ============================================================
wait_for_uhma:
    push rbx
    push r12
    sub rsp, 8

    mov r12d, edi                       ; max retries

.wait_loop:
    test r12d, r12d
    jz .wait_fail

    ; Try to connect
    call connect_all
    test eax, eax
    jnz .wait_ok

    ; Sleep 1 second
    mov edi, 1
    call sleep_sec

    dec r12d
    jmp .wait_loop

.wait_ok:
    mov eax, 1
    jmp .wait_ret

.wait_fail:
    xor eax, eax

.wait_ret:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; training_loop — Main training loop
;; ============================================================
training_loop:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8

    xor r12d, r12d                      ; cycle counter

.cycle_start:
    inc r12d

    ; Check max cycles
    mov eax, [rel max_cycles]
    test eax, eax
    jz .no_cycle_limit
    cmp r12d, eax
    jg .training_done

.no_cycle_limit:
    ; Print cycle number
    lea rdi, [rel msg_cycle]
    call print_str
    mov edi, r12d
    call print_num
    lea rdi, [rel msg_cycle_end]
    call print_str

    ; Open corpus directory
    mov rax, [rel corpus_path]
    mov rdi, rax
    mov esi, O_RDONLY
    xor edx, edx
    mov eax, SYS_OPEN
    syscall
    test eax, eax
    js .no_corpus
    mov [rel dir_fd], eax

    ; Scan and feed files
.read_dir:
    mov eax, SYS_GETDENTS64
    mov edi, [rel dir_fd]
    lea rsi, [rel dir_buf]
    mov edx, 8192
    syscall
    test eax, eax
    jle .dir_done

    ; Process directory entries
    xor r13d, r13d                      ; offset into buffer
    mov r14d, eax                       ; bytes read

.process_entry:
    cmp r13d, r14d
    jge .read_dir

    ; Get entry at offset
    lea rsi, [rel dir_buf]
    add rsi, r13

    ; struct linux_dirent64:
    ;   u64 d_ino
    ;   u64 d_off
    ;   u16 d_reclen
    ;   u8  d_type
    ;   char d_name[]
    movzx eax, word [rsi + 16]          ; d_reclen
    mov r15d, eax                       ; save for next

    ; Check d_type (regular file = 8)
    movzx eax, byte [rsi + 18]          ; d_type
    cmp eax, 8                          ; DT_REG
    jne .next_entry

    ; Get filename
    lea rdi, [rsi + 19]                 ; d_name

    ; Check if .txt file
    call check_txt_extension
    test eax, eax
    jz .next_entry

    ; Build full path
    call build_file_path

    ; Feed the file
    call feed_file

    ; Drain outputs
    mov edi, 10
    call drain_outputs

    ; Sleep between files
    mov edi, [rel pause_secs]
    call sleep_sec

    ; Check consolidation timing
    call maybe_consolidate

.next_entry:
    add r13d, r15d
    jmp .process_entry

.dir_done:
    ; Close directory
    mov eax, SYS_CLOSE
    mov edi, [rel dir_fd]
    syscall

    ; End of cycle consolidation
    call do_consolidate

    jmp .cycle_start

.no_corpus:
    lea rdi, [rel msg_no_corpus]
    call print_str

.training_done:
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; check_txt_extension — Check if filename ends with .txt
;; rdi = filename
;; Returns: eax=1 if .txt, 0 otherwise
;; ============================================================
check_txt_extension:
    push rbx
    mov rbx, rdi

    ; Find end of string
    xor ecx, ecx
.find_end:
    mov al, [rbx + rcx]
    test al, al
    jz .check_ext
    inc ecx
    jmp .find_end

.check_ext:
    cmp ecx, 4
    jl .not_txt

    ; Check last 4 chars
    lea rax, [rbx + rcx - 4]
    cmp byte [rax], '.'
    jne .not_txt
    cmp byte [rax + 1], 't'
    jne .not_txt
    cmp byte [rax + 2], 'x'
    jne .not_txt
    cmp byte [rax + 3], 't'
    jne .not_txt

    mov eax, 1
    jmp .ext_ret

.not_txt:
    xor eax, eax

.ext_ret:
    pop rbx
    ret

;; ============================================================
;; build_file_path — Build full path: corpus/filename
;; rdi = filename
;; Result in file_path buffer
;; ============================================================
build_file_path:
    push rbx
    push r12
    mov r12, rdi                        ; save filename

    ; Copy corpus path
    lea rdi, [rel file_path]
    mov rsi, [rel corpus_path]
.copy_corpus:
    lodsb
    test al, al
    jz .add_slash
    stosb
    jmp .copy_corpus

.add_slash:
    ; Add trailing slash if needed
    cmp byte [rdi - 1], '/'
    je .copy_filename
    mov byte [rdi], '/'
    inc rdi

.copy_filename:
    mov rsi, r12
.copy_name:
    lodsb
    stosb
    test al, al
    jnz .copy_name

    pop r12
    pop rbx
    ret

;; ============================================================
;; feed_file — Send "eat filepath" to UHMA
;; ============================================================
feed_file:
    push rbx
    sub rsp, 8

    ; Print feeding message
    lea rdi, [rel msg_feeding]
    call print_str
    lea rdi, [rel file_path]
    call print_str
    lea rdi, [rel newline]
    call print_str

    ; Build command: "eat filepath\n"
    lea rdi, [rel send_buf]
    lea rsi, [rel cmd_eat]
.copy_eat:
    lodsb
    test al, al
    jz .copy_path
    stosb
    jmp .copy_eat

.copy_path:
    lea rsi, [rel file_path]
.copy_fp:
    lodsb
    test al, al
    jz .add_newline
    stosb
    jmp .copy_fp

.add_newline:
    mov byte [rdi], 10
    inc rdi
    mov byte [rdi], 0

    ; Send to FEED_IN
    lea rsi, [rel send_buf]
    mov edi, SUBNET_CONSOL
    call send_cmd

    add rsp, 8
    pop rbx
    ret

;; ============================================================
;; send_cmd — Send framed command to gateway
;; edi = subnet, rsi = string
;; ============================================================
send_cmd:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8

    mov r12, rsi                ; payload ptr
    mov r15b, dil               ; subnet

    ; Get string length
    xor ecx, ecx
.len_loop:
    mov al, [r12 + rcx]
    test al, al
    jz .do_send
    inc ecx
    jmp .len_loop

.do_send:
    cmp ecx, GW_MAX_PAYLOAD
    jg .send_done

    ; Next seq
    movzx eax, word [rel gw_seq_counter]
    inc eax
    mov [rel gw_seq_counter], ax
    mov r14d, eax               ; seq_id

    ; Build header
    lea rdi, [rel gw_frame_buf]
    mov word [rdi], GW_MAGIC
    mov [rdi + 2], r15b
    mov byte [rdi + 3], 0
    mov [rdi + 4], r14w
    mov [rdi + 6], cx           ; payload_len (u16)

    ; Copy payload
    lea rdi, [rel gw_frame_buf + GW_HEADER_SIZE]
    mov rsi, r12
    mov r13d, ecx
    rep movsb

    ; Send frame
    mov ebx, [rel gw_fd]
    mov eax, SYS_WRITE
    mov edi, ebx
    lea rsi, [rel gw_frame_buf]
    mov edx, GW_HEADER_SIZE
    add edx, r13d
    syscall

.send_done:
    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; drain_outputs — Poll and drain gateway responses
;; edi = timeout in milliseconds
;; ============================================================
drain_outputs:
    push rbx
    push r12
    sub rsp, 8

    mov r12d, edi                       ; save timeout

    ; Setup pollfd for gateway socket
    lea rbx, [rel poll_fds]
    mov eax, [rel gw_fd]
    mov [rbx], eax
    mov word [rbx + 4], POLLIN
    mov word [rbx + 6], 0

.drain_loop:
    mov eax, SYS_POLL
    lea rdi, [rel poll_fds]
    mov esi, 1
    mov edx, r12d
    syscall
    test eax, eax
    jle .drain_ret

    movzx eax, word [rbx + 6]
    test ax, POLLIN
    jz .drain_ret

    ; Drain one frame, then continue with 0 timeout
    call gw_drain_frame
    xor r12d, r12d
    jmp .drain_loop

.drain_ret:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; gw_read_exact — Read exactly edx bytes
;; edi = fd, rsi = buf, edx = len
;; Returns: eax = bytes read or <=0 on error
;; ============================================================
gw_read_exact:
    push rbx
    push r12
    sub rsp, 8

    mov ebx, edi
    mov r12, rsi
    mov r10d, edx
    xor ecx, ecx

.gre_loop:
    test r10d, r10d
    jz .gre_done
    mov eax, SYS_READ
    mov edi, ebx
    lea rsi, [r12 + rcx]
    mov edx, r10d
    syscall
    test eax, eax
    jle .gre_err
    add ecx, eax
    sub r10d, eax
    jmp .gre_loop

.gre_done:
    mov eax, ecx
    jmp .gre_ret

.gre_err:
    ; eax already <= 0
    nop

.gre_ret:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; gw_drain_frame — Read and discard one framed response
;; Returns: eax = 1 if drained, 0 on error
;; ============================================================
gw_drain_frame:
    push rbx
    push r12
    sub rsp, 8

    mov ebx, [rel gw_fd]
    cmp ebx, -1
    je .df_fail

    ; Read header
    mov edi, ebx
    lea rsi, [rel gw_frame_buf]
    mov edx, GW_HEADER_SIZE
    call gw_read_exact
    cmp eax, GW_HEADER_SIZE
    jne .df_fail

    ; Validate magic
    lea rdx, [rel gw_frame_buf]
    movzx eax, word [rdx]
    cmp ax, GW_MAGIC
    jne .df_fail

    ; Payload length
    movzx eax, word [rdx + 6]
    test eax, eax
    jle .df_fail
    cmp eax, GW_MAX_PAYLOAD
    jg .df_fail
    mov r12d, eax

    ; Read payload into drain_buf
    mov edi, ebx
    lea rsi, [rel drain_buf]
    mov edx, r12d
    call gw_read_exact
    cmp eax, r12d
    jne .df_fail

    mov eax, 1
    jmp .df_ret

.df_fail:
    xor eax, eax

.df_ret:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; maybe_consolidate — Check timing and consolidate if needed
;; ============================================================
maybe_consolidate:
    push rbx
    sub rsp, 8

    call get_time
    mov rbx, rax

    ; Calculate elapsed seconds
    sub rbx, [rel last_consolidate]

    ; Check against consolidate interval (in minutes -> seconds)
    mov eax, [rel consolidate_min]
    imul eax, eax, 60
    cmp rbx, rax
    jl .no_consolidate

    call do_consolidate
    call get_time
    mov [rel last_consolidate], rax

.no_consolidate:
    add rsp, 8
    pop rbx
    ret

;; ============================================================
;; do_consolidate — Run observe and dream
;; ============================================================
do_consolidate:
    push rbx
    sub rsp, 8

    lea rdi, [rel msg_consolidate]
    call print_str

    ; Send observe
    mov edi, SUBNET_CONSOL
    lea rsi, [rel cmd_observe]
    call send_cmd

    ; Wait and drain
    mov edi, 5
    call sleep_sec
    mov edi, 100
    call drain_outputs

    ; Send dream
    mov edi, SUBNET_CONSOL
    lea rsi, [rel cmd_dream]
    call send_cmd

    ; Wait and drain
    mov edi, 5
    call sleep_sec
    mov edi, 100
    call drain_outputs

    add rsp, 8
    pop rbx
    ret

;; ============================================================
;; get_time — Get current unix timestamp
;; Returns: rax = seconds
;; ============================================================
get_time:
    sub rsp, 8
    lea rdi, [rel current_time]
    xor esi, esi
    mov eax, SYS_GETTIMEOFDAY
    syscall
    mov rax, [rel current_time]
    add rsp, 8
    ret

;; ============================================================
;; sleep_sec — Sleep for N seconds
;; edi = seconds
;; ============================================================
sleep_sec:
    sub rsp, 24

    mov [rsp], rdi                      ; tv_sec
    mov qword [rsp + 8], 0              ; tv_nsec

    mov eax, SYS_NANOSLEEP
    lea rdi, [rsp]
    xor esi, esi
    syscall

    add rsp, 24
    ret

;; ============================================================
;; print_str — Print null-terminated string
;; rdi = string
;; ============================================================
print_str:
    push rbx
    mov rbx, rdi

    ; Get length
    xor ecx, ecx
.len:
    mov al, [rbx + rcx]
    test al, al
    jz .print
    inc ecx
    jmp .len

.print:
    mov eax, SYS_WRITE
    mov edi, STDOUT
    mov rsi, rbx
    mov edx, ecx
    syscall

    pop rbx
    ret

;; ============================================================
;; print_num — Print number in decimal
;; edi = number
;; ============================================================
print_num:
    push rbx
    sub rsp, 24

    mov eax, edi
    lea rdi, [rsp + 20]
    mov byte [rdi], 0
    dec rdi
    mov ebx, 10

.num_loop:
    xor edx, edx
    div ebx
    add dl, '0'
    mov [rdi], dl
    dec rdi
    test eax, eax
    jnz .num_loop

    inc rdi
    mov rsi, rdi
    lea rax, [rsp + 20]
    sub rax, rdi
    mov edx, eax

    mov eax, SYS_WRITE
    mov edi, STDOUT
    syscall

    add rsp, 24
    pop rbx
    ret

;; ============================================================
;; strcmp — Compare two strings
;; rdi = str1, rsi = str2
;; Returns: eax=0 if equal, non-zero otherwise
;; ============================================================
strcmp:
.cmp_loop:
    mov al, [rdi]
    mov cl, [rsi]
    cmp al, cl
    jne .not_equal
    test al, al
    jz .equal
    inc rdi
    inc rsi
    jmp .cmp_loop

.equal:
    xor eax, eax
    ret

.not_equal:
    mov eax, 1
    ret

;; ============================================================
;; atoi — Convert string to integer
;; rsi = string
;; Returns: eax = integer value
;; ============================================================
atoi:
    xor eax, eax
    mov ecx, 10

.atoi_loop:
    movzx edx, byte [rsi]
    test dl, dl
    jz .atoi_done
    cmp dl, '0'
    jb .atoi_done
    cmp dl, '9'
    ja .atoi_done
    sub dl, '0'
    imul eax, ecx
    add eax, edx
    inc rsi
    jmp .atoi_loop

.atoi_done:
    ret

;; ============================================================
;; exit — Exit program
;; edi = exit code
;; ============================================================
exit:
    mov eax, SYS_EXIT
    syscall
