; tokenize.asm — BPE/WordPiece tokenizer for transformer inference
;
; @entry tokenizer_init(vocab_path) -> 0=success, -1=error
;        Load vocabulary from binary file
; @entry tokenize(text, text_len, output_ids, max_tokens) -> num_tokens
;        Convert text to token IDs
; @entry token_to_string(token_id, buf, buf_len) -> string_len
;        Convert token ID back to string
;
; Vocab format (vocab.bin):
;   [num_tokens:u32]
;   [offsets:u32 * num_tokens] - byte offset into string table
;   [strings...] - each: [len:u16][utf8_bytes...]
;
; Tokenization algorithm (simplified WordPiece):
;   1. Lowercase and split on whitespace
;   2. For each word:
;      a. Try exact match in vocab
;      b. If not found, try ## prefixed subwords
;      c. If still not found, use [UNK] token
;
; Special tokens:
;   [PAD] = 0, [UNK] = 100, [CLS] = 101, [SEP] = 102, [MASK] = 103
;
; GOTCHAS:
;   - Vocab is memory-mapped for fast access
;   - Hash table provides O(1) lookup after init (linear scan fallback)

%include "syscalls.inc"

%define TOK_PAD     0
%define TOK_UNK     100
%define TOK_CLS     101
%define TOK_SEP     102
%define TOK_MASK    103

%define HASH_SIZE   65536
%define HASH_MASK   0xFFFF

section .data
    vocab_path_default: db "embed/weights/vocab.bin", 0
    err_vocab_open:     db "[TOKENIZE] Failed to open vocab", 10, 0
    err_vocab_mmap:     db "[TOKENIZE] Failed to mmap vocab", 10, 0

section .bss
    vocab_ptr:      resq 1      ; pointer to mmaped vocab
    vocab_size:     resd 1      ; file size
    vocab_count:    resd 1      ; number of tokens
    vocab_offsets:  resq 1      ; pointer to offsets array
    vocab_strings:  resq 1      ; pointer to string table

    ; Hash table for fast lookups
    hash_ready:     resd 1
    alignb 8
    hash_table:     resq HASH_SIZE

    ; Scratch for tokenization
    word_buf:       resb 256    ; current word being tokenized
    word_len:       resd 1

section .text
global tokenizer_init
global tokenize
global token_to_string

;; ============================================================================
;; tokenizer_init - Load vocabulary
;;
;; Args:
;;   rdi = vocab path (or NULL for default)
;; Returns:
;;   eax = 0 on success, -1 on error
;; ============================================================================
tokenizer_init:
    push rbx
    push r12
    sub rsp, 8

    ; Use default path if NULL
    test rdi, rdi
    jnz .have_path
    lea rdi, [rel vocab_path_default]
.have_path:
    mov r12, rdi

    ; Open file
    mov eax, SYS_OPEN
    mov rsi, 0              ; O_RDONLY
    xor edx, edx
    syscall
    test eax, eax
    js .open_failed
    mov ebx, eax            ; fd

    ; Get file size via fstat
    sub rsp, 144            ; struct stat
    mov eax, SYS_FSTAT
    mov edi, ebx
    mov rsi, rsp
    syscall
    mov r12d, [rsp + 48]    ; st_size
    add rsp, 144

    ; mmap the file
    mov eax, SYS_MMAP
    xor edi, edi            ; addr = NULL
    mov esi, r12d           ; length
    mov edx, 1              ; PROT_READ
    mov r10d, 2             ; MAP_PRIVATE
    mov r8d, ebx            ; fd
    xor r9d, r9d            ; offset = 0
    syscall
    test rax, rax
    js .mmap_failed

    ; Save pointers
    mov [rel vocab_ptr], rax
    mov [rel vocab_size], r12d

    ; Parse header
    mov ecx, [rax]          ; num_tokens
    mov [rel vocab_count], ecx

    lea rdx, [rax + 4]      ; offsets array
    mov [rel vocab_offsets], rdx

    ; String table starts after offsets
    mov eax, ecx
    shl eax, 2              ; * 4 bytes per offset
    add eax, 4              ; + 4 for count
    mov rdx, [rel vocab_ptr]
    add rdx, rax
    mov [rel vocab_strings], rdx

    ; Build hash table for O(1) vocab lookups
    call build_hash_table

    ; Close fd (mmap persists)
    mov eax, SYS_CLOSE
    mov edi, ebx
    syscall

    xor eax, eax
    jmp .done

.open_failed:
    lea rdi, [rel err_vocab_open]
    call print_str
    mov eax, -1
    jmp .done

.mmap_failed:
    lea rdi, [rel err_vocab_open]
    call print_str
    mov eax, -1

.done:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================================
;; build_hash_table - Build vocab hash table for O(1) lookup
;; Uses vocab_offsets + vocab_strings
;; ============================================================================
build_hash_table:
    push rbx
    push r12
    push r13
    push r14
    push r15

    ; Initialize table to empty (0xFFFFFFFF)
    lea rdi, [rel hash_table]
    mov ecx, HASH_SIZE
    mov rax, 0xFFFFFFFFFFFFFFFF
    rep stosq

    mov dword [rel hash_ready], 0
    lea r11, [rel hash_table]

    mov r12, [rel vocab_offsets]
    mov r13, [rel vocab_strings]
    mov r14d, [rel vocab_count]
    xor r15d, r15d                  ; token_id

.bh_loop:
    cmp r15d, r14d
    jge .bh_done

    ; offset = vocab_offsets[token_id]
    mov eax, [r12 + r15*4]
    lea rsi, [r13 + rax]            ; string record
    movzx edx, word [rsi]           ; len
    lea rdi, [rsi + 2]              ; string bytes
    mov esi, edx
    call hash_word                  ; rax = hash
    and eax, HASH_MASK
    mov ebx, eax                    ; idx

.bh_probe:
    mov rax, [r11 + rbx*8]
    mov edx, eax                    ; token_id in slot
    cmp edx, 0xFFFFFFFF
    jne .bh_next_slot

    ; Pack {offset, token_id} into slot
    mov eax, [r12 + r15*4]          ; offset
    mov rdx, rax
    shl rdx, 32
    mov eax, r15d                   ; token_id
    or rdx, rax
    mov [r11 + rbx*8], rdx
    jmp .bh_next_token

.bh_next_slot:
    inc ebx
    and ebx, HASH_MASK
    jmp .bh_probe

.bh_next_token:
    inc r15d
    jmp .bh_loop

.bh_done:
    mov dword [rel hash_ready], 1
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================================
;; hash_word - FNV-1a hash over bytes
;;
;; Args:
;;   rdi = byte pointer
;;   esi = length
;; Returns:
;;   rax = hash
;; ============================================================================
hash_word:
    mov rax, 0xCBF29CE484222325
    mov rcx, 0x100000001B3
.hw_loop:
    test esi, esi
    jz .hw_done
    movzx edx, byte [rdi]
    xor rax, rdx
    imul rax, rcx
    inc rdi
    dec esi
    jmp .hw_loop
.hw_done:
    ret

;; ============================================================================
;; tokenize - Convert text to token IDs
;;
;; Args:
;;   rdi = text pointer
;;   esi = text length
;;   rdx = output_ids pointer (u32 array)
;;   ecx = max_tokens
;; Returns:
;;   eax = number of tokens produced
;; ============================================================================
tokenize:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 8

    mov r10, rdi            ; text
    mov r11d, esi           ; text_len
    mov r12, rdx            ; output_ids
    mov r13d, ecx           ; max_tokens
    xor r14d, r14d          ; token_count = 0
    xor r15d, r15d          ; text_pos = 0

    ; Add [CLS] token at start
    cmp r14d, r13d
    jge .tok_done
    mov dword [r12], TOK_CLS
    add r12, 4
    inc r14d

.tok_loop:
    cmp r15d, r11d
    jge .tok_end
    cmp r14d, r13d
    jge .tok_done

    ; Skip whitespace
    movzx eax, byte [r10 + r15]
    cmp al, ' '
    je .skip_ws
    cmp al, 9               ; tab
    je .skip_ws
    cmp al, 10              ; newline
    je .skip_ws
    cmp al, 13              ; carriage return
    je .skip_ws
    jmp .start_word

.skip_ws:
    inc r15d
    jmp .tok_loop

.start_word:
    ; Extract word into word_buf
    lea rdi, [rel word_buf]
    xor ecx, ecx            ; word length

.copy_word:
    cmp r15d, r11d
    jge .end_word
    movzx eax, byte [r10 + r15]
    ; Stop at whitespace or punctuation
    cmp al, ' '
    je .end_word
    cmp al, 9
    je .end_word
    cmp al, 10
    je .end_word
    cmp al, 13
    je .end_word
    ; Also stop at common punctuation
    cmp al, '.'
    je .end_word_punct
    cmp al, ','
    je .end_word_punct
    cmp al, '!'
    je .end_word_punct
    cmp al, '?'
    je .end_word_punct
    cmp al, ';'
    je .end_word_punct
    cmp al, ':'
    je .end_word_punct

    ; Lowercase
    cmp al, 'A'
    jb .no_lower
    cmp al, 'Z'
    ja .no_lower
    add al, 32              ; to lowercase
.no_lower:
    mov [rdi + rcx], al
    inc ecx
    inc r15d
    cmp ecx, 250            ; max word length
    jl .copy_word

.end_word_punct:
    inc r15d
.end_word:
    mov byte [rdi + rcx], 0 ; null terminate
    mov [rel word_len], ecx
    test ecx, ecx
    jz .tok_loop            ; empty word, continue

    ; Look up word in vocabulary
    push r10
    push r11
    push r12
    push r13
    push r14
    push r15

    lea rdi, [rel word_buf]
    mov esi, [rel word_len]
    call vocab_lookup

    pop r15
    pop r14
    pop r13
    pop r12
    pop r11
    pop r10

    ; eax = token ID or -1 if not found
    test eax, eax
    js .use_unk

    mov [r12], eax
    add r12, 4
    inc r14d
    jmp .tok_loop

.use_unk:
    mov dword [r12], TOK_UNK
    add r12, 4
    inc r14d
    jmp .tok_loop

.tok_end:
    ; Add [SEP] token at end
    cmp r14d, r13d
    jge .tok_done
    mov dword [r12], TOK_SEP
    inc r14d

.tok_done:
    mov eax, r14d

    add rsp, 8
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================================
;; vocab_lookup - Look up word in vocabulary (linear scan)
;;
;; Args:
;;   rdi = word pointer (null-terminated)
;;   esi = word length
;; Returns:
;;   eax = token ID, or -1 if not found
;; ============================================================================
vocab_lookup:
    push rbx
    push r12
    push r13
    push r14
    push r15

    mov r10, rdi            ; word
    mov r11d, esi           ; word_len

    ; Use hash table if available
    cmp dword [rel hash_ready], 0
    je .linear_scan

    ; Hash lookup
    mov rdi, r10
    mov esi, r11d
    call hash_word
    and eax, HASH_MASK
    mov r15d, eax
    lea r12, [rel hash_table]
    mov r13, [rel vocab_strings]
    mov ecx, HASH_SIZE

.hash_probe:
    mov rax, [r12 + r15*8]
    mov r9d, eax                    ; token_id
    cmp r9d, 0xFFFFFFFF
    je .not_found
    mov rbx, rax
    shr rbx, 32                     ; offset
    lea rsi, [r13 + rbx]
    movzx eax, word [rsi]           ; length
    cmp eax, r11d
    jne .hash_next
    lea rsi, [rsi + 2]
    mov rdi, r10
    mov edx, r11d
.hash_cmp:
    test edx, edx
    jz .hash_found
    mov al, [rdi]
    cmp al, [rsi]
    jne .hash_next
    inc rdi
    inc rsi
    dec edx
    jmp .hash_cmp
.hash_found:
    mov eax, r9d
    jmp .lookup_done

.hash_next:
    inc r15d
    and r15d, HASH_MASK
    dec ecx
    jnz .hash_probe
    jmp .not_found

.linear_scan:
    mov r12, [rel vocab_offsets]
    mov r13, [rel vocab_strings]
    mov r14d, [rel vocab_count]
    xor ebx, ebx            ; token_id = 0

.lookup_loop:
    cmp ebx, r14d
    jge .not_found

    ; Get offset for this token
    mov eax, [r12 + rbx*4]
    ; String at vocab_strings + offset: [len:u16][bytes...]
    lea rcx, [r13 + rax]

    ; Compare length
    movzx eax, word [rcx]   ; token string length
    cmp eax, r11d
    jne .next_token

    ; Compare bytes
    add rcx, 2              ; skip length
    mov rdi, r10            ; our word
    mov rsi, rcx            ; vocab word
    mov edx, r11d           ; length

.cmp_loop:
    test edx, edx
    jz .found
    mov al, [rdi]
    cmp al, [rsi]
    jne .next_token
    inc rdi
    inc rsi
    dec edx
    jmp .cmp_loop

.found:
    mov eax, ebx
    jmp .lookup_done

.next_token:
    inc ebx
    jmp .lookup_loop

.not_found:
    mov eax, -1

.lookup_done:
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================================
;; token_to_string - Convert token ID to string
;;
;; Args:
;;   edi = token_id
;;   rsi = output buffer
;;   edx = buffer length
;; Returns:
;;   eax = string length, or -1 if invalid
;; ============================================================================
token_to_string:
    push rbx

    mov ebx, edi            ; token_id

    ; Bounds check
    cmp ebx, [rel vocab_count]
    jge .tts_invalid

    ; Get offset
    mov rdi, [rel vocab_offsets]
    mov eax, [rdi + rbx*4]

    ; Get string pointer
    mov rdi, [rel vocab_strings]
    add rdi, rax

    ; Length
    movzx ecx, word [rdi]
    add rdi, 2

    ; Check buffer size
    cmp ecx, edx
    jg .tts_truncate
    mov edx, ecx

.tts_truncate:
    ; Copy
    push rcx
    mov ecx, edx
    rep movsb
    pop rax                 ; return original length

    jmp .tts_done

.tts_invalid:
    mov eax, -1

.tts_done:
    pop rbx
    ret

;; ============================================================================
;; print_str - Print null-terminated string (helper)
;; ============================================================================
print_str:
    push rdi
    ; Find length
    mov rsi, rdi
    xor ecx, ecx
.ps_len:
    cmp byte [rsi + rcx], 0
    je .ps_write
    inc ecx
    jmp .ps_len
.ps_write:
    mov eax, SYS_WRITE
    mov edi, 2              ; stderr
    pop rsi                 ; string
    mov edx, ecx            ; length
    syscall
    ret
