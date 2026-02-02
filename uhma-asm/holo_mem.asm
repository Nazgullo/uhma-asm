; holo_mem.asm — Holographic Memory for Claude Integration
;
; @entry holo_mem_init() -> void
; @entry holo_mem_add(edi=category, rsi=content, rdx=context, ecx=source) -> eax=entry_id
; @entry holo_mem_query(rdi=query, esi=limit) -> void  ; prints results
; @entry holo_mem_recent(edi=limit) -> void  ; prints recent entries
; @entry holo_mem_state() -> void  ; prints cognitive state
; @entry holo_mem_outcome(edi=entry_id, esi=worked) -> void  ; record outcome
; @entry holo_mem_summary() -> void  ; prints summary stats
;
; STANDALONE: Uses own 6GB mmap file, NOT UHMA's surface.
; @calledby tools/mcp_server.asm (mem_* commands)
;
; DUAL PURPOSE:
;   1. Chat Sessions Memory - findings, insights, sessions, warnings
;   2. 3-Layer Code RAG - file/function/implementation fidelity levels
;
; CATEGORIES (Chat Sessions):
;   0 = finding     Confirmed facts (decay 0.95)
;   1 = failed      What didn't work (decay 0.90)
;   2 = success     What worked (decay 0.95)
;   3 = insight     Aha moments (decay 0.95)
;   4 = warning     Gotchas to remember (decay 0.92)
;   5 = session     Session summaries (decay 0.85)
;   6 = location    Code locations (decay 0.98)
;   7 = question    Open questions (decay 0.80)
;   8 = todo        Tasks (decay 0.85)
;   9 = context     Temporary context (decay 0.70)
;  10 = request     User requests (decay 0.80)
;
; CATEGORIES (3-Layer Code RAG):
;  11 = code_high   1:1 raw source (decay 0.98) - full code verbatim
;  12 = code_mid    Function-level (decay 0.96) - signatures, entry points, @calls
;  13 = code_low    Summaries (decay 0.92) - gotchas, patterns, descriptions
;
; STORAGE:
;   Entries stored at HOLO_MEM_OFFSET in surface
;   Each entry: 2048 bytes
;     [0-7]      u64 entry_id
;     [8-11]     u32 category
;     [12-15]    u32 outcome_count
;     [16-19]    f32 outcome_ratio (success_count / outcome_count)
;     [20-23]    u32 timestamp
;     [24-511]   char[488] content
;     [512-767]  char[256] context
;     [768-895]  char[128] source
;     [896-1023] reserved
;     [1024-2047] f64[128] compressed_vec (holographic summary)
;
;   Category traces stored at HOLO_MEM_TRACE_OFFSET
;   Each trace: 8KB (f64[1024])
;
; GOTCHAS:
;   - Entries are stored in surface file (persisted)
;   - VSA similarity search is O(n) but fast with dot product
;   - Category traces enable "find similar warnings" type queries
;
%include "syscalls.inc"

; Standalone 6GB surface (same file as Python version)
%define HOLO_SURFACE_SIZE     0x180000000   ; 6GB
%define HOLO_VEC_DIM          1024
%define HOLO_VEC_BYTES        8192          ; 1024 * 8

; Layout offsets from mmap'd surface base
%define HOLO_MEM_OFFSET       0             ; Entries at start
%define HOLO_MEM_ENTRY_SIZE   2048          ; bytes per entry
%define HOLO_MEM_MAX_ENTRIES  4096          ; 8MB total
%define HOLO_MEM_TRACE_OFFSET 0x10A00000    ; Category traces (14 * 8KB = 112KB)
%define HOLO_MEM_STATE_OFFSET 0x10A20000    ; State counters

; Entry field offsets
%define HME_ID          0           ; u64
%define HME_CATEGORY    8           ; u32
%define HME_OUTCOMES    12          ; u32 (total outcome recordings)
%define HME_SUCCESS     16          ; f32 (success ratio 0.0-1.0)
%define HME_TIMESTAMP   20          ; u32 (unix timestamp low 32 bits)
%define HME_CONTENT     24          ; char[488]
%define HME_CONTENT_LEN 488
%define HME_CONTEXT     512         ; char[256]
%define HME_CONTEXT_LEN 256
%define HME_SOURCE      768         ; char[128]
%define HME_SOURCE_LEN  128
%define HME_RESERVED    896         ; 128 bytes padding
%define HME_VEC         1024        ; f64[128] compressed vector
%define HME_VEC_SIZE    1024        ; 128 * 8

; Categories (Chat Sessions)
%define CAT_FINDING     0
%define CAT_FAILED      1
%define CAT_SUCCESS     2
%define CAT_INSIGHT     3
%define CAT_WARNING     4
%define CAT_SESSION     5
%define CAT_LOCATION    6
%define CAT_QUESTION    7
%define CAT_TODO        8
%define CAT_CONTEXT     9
%define CAT_REQUEST     10
; Categories (3-Layer Code RAG)
%define CAT_CODE_HIGH   11      ; File-level (architecture, purposes)
%define CAT_CODE_MID    12      ; Function-level (signatures, entry points)
%define CAT_CODE_LOW    13      ; Implementation (gotchas, patterns)
%define CAT_COUNT       14

; State offsets
%define HMS_ENTRY_COUNT 0           ; u32 total entries
%define HMS_NEXT_ID     4           ; u64 next entry ID
%define HMS_LAST_ADD    12          ; u64 timestamp of last add
%define HMS_QUERY_COUNT 20          ; u32 total queries

section .data
    hm_surface_path: db "HOLO-memory/memory/holo_surface.dat", 0
    hm_init_msg:    db "[HOLO_MEM] Holographic memory initialized", 10, 0
    hm_mmap_fail:   db "[HOLO_MEM] ERROR: mmap failed", 10, 0
    hm_add_msg:     db "[HOLO_MEM] Added entry #", 0
    hm_query_msg:   db "[HOLO_MEM] Query results:", 10, 0
    hm_recent_msg:  db "[HOLO_MEM] Recent entries:", 10, 0
    hm_state_msg:   db "[HOLO_MEM] Cognitive state:", 10, 0
    hm_summary_msg: db "[HOLO_MEM] Summary:", 10, 0
    hm_no_entries:  db "  (no entries)", 10, 0
    hm_entry_prefix: db "  [", 0
    hm_entry_sep:   db "] ", 0
    hm_entry_sim:   db " (sim=", 0
    hm_entry_end:   db ")", 10, 0
    hm_nl:          db 10, 0
    hm_colon:       db ": ", 0
    hm_entries_lbl: db "  Entries: ", 0
    hm_failed_lbl:  db "  Failed: ", 0
    hm_success_lbl: db "  Success: ", 0
    hm_warnings_lbl: db "  Warnings: ", 0
    hm_findings_lbl: db "  Findings: ", 0
    hm_insights_lbl: db "  Insights: ", 0

    ; Category names (14 categories)
    cat_names:
        dq cat_finding, cat_failed, cat_success, cat_insight, cat_warning
        dq cat_session, cat_location, cat_question, cat_todo, cat_context
        dq cat_request, cat_code_high, cat_code_mid, cat_code_low
    cat_finding:    db "finding", 0
    cat_failed:     db "failed", 0
    cat_success:    db "success", 0
    cat_insight:    db "insight", 0
    cat_warning:    db "warning", 0
    cat_session:    db "session", 0
    cat_location:   db "location", 0
    cat_question:   db "question", 0
    cat_todo:       db "todo", 0
    cat_context:    db "context", 0
    cat_request:    db "request", 0
    cat_code_high:  db "code_high", 0
    cat_code_mid:   db "code_mid", 0
    cat_code_low:   db "code_low", 0

section .bss
    ; Surface base (mmap'd)
    hm_surface_base: resq 1

    ; Embed status (0=available, -1=failed)
    embed_available: resd 1

    ; Scratch vectors for query encoding
    alignb 64
    hm_query_vec:   resb HOLO_VEC_BYTES
    hm_entry_vec:   resb HOLO_VEC_BYTES
    hm_scratch_vec: resb HOLO_VEC_BYTES

    ; Results buffer for sorting
    hm_results:     resb 256 * 16   ; 256 entries * (entry_id:u64 + similarity:f64)

section .text

extern print_cstr
extern print_u64
extern print_f64
extern print_newline

; MPNet semantic embeddings
extern embed_init
extern embed_text

;; ============================================================
;; holo_mem_init — Initialize holographic memory system
;; mmaps the 6GB surface file used by Python version
;; ============================================================
global holo_mem_init
holo_mem_init:
    push rbx
    push r12
    sub rsp, 8

    ; Open surface file (create if not exists)
    mov eax, SYS_OPEN
    lea rdi, [rel hm_surface_path]
    mov esi, O_RDWR | O_CREAT
    mov edx, 0644o
    syscall
    test eax, eax
    js .mmap_fail
    mov r12d, eax               ; save fd

    ; Extend file to 6GB if needed
    mov eax, SYS_FTRUNCATE
    mov edi, r12d
    mov rsi, HOLO_SURFACE_SIZE
    syscall

    ; mmap the file
    mov eax, SYS_MMAP
    xor edi, edi                ; addr = NULL
    mov rsi, HOLO_SURFACE_SIZE  ; length
    mov edx, PROT_READ | PROT_WRITE
    mov r10d, MAP_SHARED
    mov r8d, r12d               ; fd
    xor r9d, r9d                ; offset = 0
    syscall
    cmp rax, -1
    je .mmap_fail
    mov [rel hm_surface_base], rax
    mov rbx, rax

    ; Close fd (mmap keeps reference)
    mov eax, SYS_CLOSE
    mov edi, r12d
    syscall

    ; Initialize MPNet embeddings
    call embed_init
    mov [rel embed_available], eax

    ; Check if state already initialized (non-zero entry count)
    mov rax, [rbx + HOLO_MEM_STATE_OFFSET + HMS_NEXT_ID]
    test rax, rax
    jnz .already_init

    ; Initialize state counters (first run only)
    mov dword [rbx + HOLO_MEM_STATE_OFFSET + HMS_ENTRY_COUNT], 0
    mov qword [rbx + HOLO_MEM_STATE_OFFSET + HMS_NEXT_ID], 1
    mov qword [rbx + HOLO_MEM_STATE_OFFSET + HMS_LAST_ADD], 0
    mov dword [rbx + HOLO_MEM_STATE_OFFSET + HMS_QUERY_COUNT], 0

.already_init:
    lea rdi, [rel hm_init_msg]
    call print_cstr

    add rsp, 8
    pop r12
    pop rbx
    ret

.mmap_fail:
    lea rdi, [rel hm_mmap_fail]
    call print_cstr
    mov qword [rel hm_surface_base], 0
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; holo_mem_add — Add entry to holographic memory
;; edi = category (0-9)
;; rsi = content string pointer
;; rdx = context string pointer (can be 0)
;; ecx = source string pointer (can be 0)
;; Returns: eax = entry_id
;; ============================================================
global holo_mem_add
holo_mem_add:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 24

    mov rbx, [rel hm_surface_base]

    ; Save parameters
    mov r12d, edi           ; category
    mov r13, rsi            ; content
    mov r14, rdx            ; context
    mov r15d, ecx           ; source

    ; Get next entry ID and increment
    mov rax, [rbx + HOLO_MEM_STATE_OFFSET + HMS_NEXT_ID]
    mov [rsp], rax          ; save entry_id
    inc qword [rbx + HOLO_MEM_STATE_OFFSET + HMS_NEXT_ID]

    ; Calculate entry offset
    mov ecx, [rbx + HOLO_MEM_STATE_OFFSET + HMS_ENTRY_COUNT]
    cmp ecx, HOLO_MEM_MAX_ENTRIES
    jge .add_full

    ; Entry address = HOLO_MEM_OFFSET + entry_count * HOLO_MEM_ENTRY_SIZE
    imul rax, rcx, HOLO_MEM_ENTRY_SIZE
    lea rdi, [rbx + HOLO_MEM_OFFSET]
    add rdi, rax            ; entry ptr
    mov [rsp + 8], rdi      ; save entry ptr

    ; Zero the entry
    push rdi
    push rcx
    mov rcx, HOLO_MEM_ENTRY_SIZE
    xor eax, eax
    rep stosb
    pop rcx
    pop rdi

    ; Fill entry fields
    mov rax, [rsp]          ; entry_id
    mov [rdi + HME_ID], rax
    mov [rdi + HME_CATEGORY], r12d
    mov dword [rdi + HME_OUTCOMES], 0
    mov dword [rdi + HME_SUCCESS], 0

    ; Get timestamp
    push rdi
    sub rsp, 16
    lea rdi, [rsp]
    xor esi, esi
    mov eax, SYS_GETTIMEOFDAY
    syscall
    mov eax, [rsp]          ; low 32 bits
    add rsp, 16
    pop rdi
    mov [rdi + HME_TIMESTAMP], eax

    ; Copy content
    push rdi
    lea rdi, [rdi + HME_CONTENT]
    mov rsi, r13
    mov ecx, HME_CONTENT_LEN - 1
    call .copy_string
    pop rdi

    ; Copy context (if provided)
    test r14, r14
    jz .skip_context
    push rdi
    lea rdi, [rdi + HME_CONTEXT]
    mov rsi, r14
    mov ecx, HME_CONTEXT_LEN - 1
    call .copy_string
    pop rdi

.skip_context:
    ; Copy source (if provided)
    test r15d, r15d
    jz .skip_source
    push rdi
    lea rdi, [rdi + HME_SOURCE]
    mov rsi, r15
    mov ecx, HME_SOURCE_LEN - 1
    call .copy_string
    pop rdi

.skip_source:
    ; Generate holographic vector from content
    mov rdi, [rsp + 8]      ; entry ptr
    call .encode_entry_vec

    ; Superpose into category trace
    mov rdi, [rsp + 8]      ; entry ptr
    mov r12d, [rdi + HME_CATEGORY]

    ; Category trace address
    imul rax, r12, HOLO_VEC_BYTES
    lea rdi, [rbx + HOLO_MEM_TRACE_OFFSET]
    add rdi, rax            ; category trace ptr
    lea rsi, [rel hm_entry_vec]
    call holo_superpose_f64

    ; Increment entry count
    inc dword [rbx + HOLO_MEM_STATE_OFFSET + HMS_ENTRY_COUNT]

    ; Update last add timestamp
    sub rsp, 16
    lea rdi, [rsp]
    xor esi, esi
    mov eax, SYS_GETTIMEOFDAY
    syscall
    mov rax, [rsp]
    add rsp, 16
    mov [rbx + HOLO_MEM_STATE_OFFSET + HMS_LAST_ADD], rax

    ; Print confirmation
    lea rdi, [rel hm_add_msg]
    call print_cstr
    mov rdi, [rsp]
    call print_u64
    call print_newline

    mov eax, [rsp]          ; return entry_id
    jmp .add_ret

.add_full:
    xor eax, eax            ; return 0 on failure

.add_ret:
    add rsp, 24
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

; Helper: copy string with length limit
; rdi=dst, rsi=src, ecx=max_len
.copy_string:
    test rsi, rsi
    jz .copy_done
.copy_loop:
    test ecx, ecx
    jz .copy_done
    lodsb
    test al, al
    jz .copy_done
    stosb
    dec ecx
    jmp .copy_loop
.copy_done:
    mov byte [rdi], 0
    ret

; Helper: encode entry content into holographic vector
; rdi = entry ptr
; Uses MPNet semantic embeddings if available, falls back to hash
.encode_entry_vec:
    push rbx
    push r12
    push r13
    push r14
    sub rsp, 8

    mov r12, rdi            ; save entry ptr

    ; Get content string length
    lea rdi, [r12 + HME_CONTENT]
    mov r13, rdi            ; save content ptr
    xor ecx, ecx
.strlen_loop:
    cmp byte [rdi + rcx], 0
    je .strlen_done
    inc ecx
    cmp ecx, HME_CONTENT_LEN
    jl .strlen_loop
.strlen_done:
    mov r14d, ecx           ; save text_len

    ; Check if embeddings available
    cmp dword [rel embed_available], 0
    jne .use_hash_fallback

    ; Try MPNet embedding: embed_text(text, text_len, output_vec)
    mov rdi, r13            ; text ptr
    mov esi, r14d           ; text_len
    lea rdx, [rel hm_entry_vec]  ; output (8192-dim f64)
    call embed_text
    test eax, eax
    jns .embed_ok

.use_hash_fallback:
    ; Fallback: hash-based encoding if embed_text fails
    lea rdi, [r12 + HME_CONTENT]
    call fnv64_hash_string
    mov edi, eax
    lea rsi, [rel hm_entry_vec]
    call holo_gen_vec

.embed_ok:
    ; Bind with category vector for category-aware retrieval
    mov edi, [r12 + HME_CATEGORY]
    add edi, 0x43415400     ; "CAT" + category number as seed
    lea rsi, [rel hm_scratch_vec]
    call holo_gen_vec

    lea rdi, [rel hm_entry_vec]
    lea rsi, [rel hm_scratch_vec]
    lea rdx, [rel hm_entry_vec]
    call holo_bind_f64

    ; Normalize
    lea rdi, [rel hm_entry_vec]
    call vsa_normalize

    ; Copy compressed vec to entry (first 128 f64 values)
    lea rsi, [rel hm_entry_vec]
    lea rdi, [r12 + HME_VEC]
    mov ecx, HME_VEC_SIZE / 8
.copy_vec:
    mov rax, [rsi]
    mov [rdi], rax
    add rsi, 8
    add rdi, 8
    dec ecx
    jnz .copy_vec

    add rsp, 8
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; holo_mem_query — Search by semantic similarity
;; rdi = query string pointer
;; esi = max results (default 10)
;; ============================================================
global holo_mem_query
holo_mem_query:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, 24

    mov rbx, [rel hm_surface_base]

    ; Save query and limit
    mov [rsp], rdi          ; query string
    mov r15d, esi           ; limit
    test r15d, r15d
    jnz .limit_ok
    mov r15d, 10            ; default limit
.limit_ok:

    ; Check entry count
    mov r14d, [rbx + HOLO_MEM_STATE_OFFSET + HMS_ENTRY_COUNT]
    test r14d, r14d
    jz .no_results

    ; Increment query count
    inc dword [rbx + HOLO_MEM_STATE_OFFSET + HMS_QUERY_COUNT]

    ; Get query string length
    mov rdi, [rsp]
    xor ecx, ecx
.query_strlen:
    cmp byte [rdi + rcx], 0
    je .query_strlen_done
    inc ecx
    cmp ecx, 512
    jl .query_strlen
.query_strlen_done:
    mov [rsp + 8], ecx      ; save text_len

    ; Check if embeddings available
    cmp dword [rel embed_available], 0
    jne .query_use_hash

    ; Generate query vector via MPNet embedding
    mov rdi, [rsp]          ; text ptr
    mov esi, [rsp + 8]      ; text_len
    lea rdx, [rel hm_query_vec]
    call embed_text
    test eax, eax
    jns .query_embed_ok

.query_use_hash:
    ; Fallback to hash-based
    mov rdi, [rsp]
    call fnv64_hash_string
    mov edi, eax
    lea rsi, [rel hm_query_vec]
    call holo_gen_vec

.query_embed_ok:
    ; Normalize query vector
    lea rdi, [rel hm_query_vec]
    call vsa_normalize

    ; Print header
    lea rdi, [rel hm_query_msg]
    call print_cstr

    ; Scan all entries, compute similarity, collect top N
    xor r12d, r12d          ; entry index
    xor r13d, r13d          ; results count

.scan_loop:
    cmp r12d, r14d
    jge .scan_done

    ; Get entry address
    imul rax, r12, HOLO_MEM_ENTRY_SIZE
    lea rdi, [rbx + HOLO_MEM_OFFSET]
    add rdi, rax
    mov [rsp + 8], rdi      ; save entry ptr

    ; Reconstruct entry vector from compressed storage
    push rdi
    lea rsi, [rdi + HME_VEC]
    lea rdi, [rel hm_entry_vec]
    mov ecx, HME_VEC_SIZE / 8
.expand_vec:
    mov rax, [rsi]
    mov [rdi], rax
    add rsi, 8
    add rdi, 8
    dec ecx
    jnz .expand_vec
    pop rdi

    ; Pad rest of vector with zeros
    lea rdi, [rel hm_entry_vec + HME_VEC_SIZE]
    mov ecx, (HOLO_VEC_BYTES - HME_VEC_SIZE) / 8
    xor eax, eax
.pad_vec:
    mov [rdi], rax
    add rdi, 8
    dec ecx
    jnz .pad_vec

    ; Compute cosine similarity
    lea rdi, [rel hm_query_vec]
    lea rsi, [rel hm_entry_vec]
    call holo_cosim_f64
    movsd [rsp + 16], xmm0  ; save similarity

    ; Only include if similarity > 0.01
    mov rax, 0x3F847AE147AE147B  ; 0.01
    movq xmm1, rax
    ucomisd xmm0, xmm1
    jbe .next_entry

    ; Store in results buffer
    lea rdi, [rel hm_results]
    imul rax, r13, 16
    add rdi, rax
    mov rax, [rsp + 8]      ; entry ptr
    mov rax, [rax + HME_ID]
    mov [rdi], rax          ; entry_id
    movsd xmm0, [rsp + 16]
    movsd [rdi + 8], xmm0   ; similarity
    inc r13d

.next_entry:
    inc r12d
    jmp .scan_loop

.scan_done:
    ; Sort results by similarity (simple bubble sort, small N)
    cmp r13d, 1
    jle .print_results
    call .sort_results

.print_results:
    ; Print top N results
    xor r12d, r12d
.print_loop:
    cmp r12d, r13d
    jge .query_done
    cmp r12d, r15d          ; limit
    jge .query_done

    ; Get result entry
    lea rdi, [rel hm_results]
    imul rax, r12, 16
    add rdi, rax
    mov rax, [rdi]          ; entry_id
    movsd xmm0, [rdi + 8]   ; similarity
    movsd [rsp + 16], xmm0

    ; Find entry by ID
    push rax
    call hm_find_entry_by_id
    mov rdi, rax            ; entry ptr
    pop rax
    test rdi, rdi
    jz .print_next

    ; Print: [category] content (sim=X.XX)
    lea rsi, [rel hm_entry_prefix]
    push rdi
    mov rdi, rsi
    call print_cstr
    pop rdi

    ; Category name
    push rdi
    mov eax, [rdi + HME_CATEGORY]
    cmp eax, CAT_COUNT
    jge .unknown_cat
    lea rsi, [rel cat_names]
    mov rdi, [rsi + rax * 8]
    call print_cstr
    jmp .cat_printed
.unknown_cat:
    mov rdi, rax
    call print_u64
.cat_printed:
    pop rdi

    lea rsi, [rel hm_entry_sep]
    push rdi
    mov rdi, rsi
    call print_cstr
    pop rdi

    ; Content (truncated)
    push rdi
    lea rdi, [rdi + HME_CONTENT]
    call print_cstr
    pop rdi

    ; Similarity
    lea rdi, [rel hm_entry_sim]
    call print_cstr
    movsd xmm0, [rsp + 16]
    call print_f64
    lea rdi, [rel hm_entry_end]
    call print_cstr

.print_next:
    inc r12d
    jmp .print_loop

.query_done:
    add rsp, 24
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

.no_results:
    lea rdi, [rel hm_query_msg]
    call print_cstr
    lea rdi, [rel hm_no_entries]
    call print_cstr
    jmp .query_done

; Helper: sort results by similarity (descending)
; r13d = result count
.sort_results:
    push r12
    push r13
    push r14

    xor r12d, r12d          ; i
.outer:
    mov eax, r13d
    dec eax
    cmp r12d, eax
    jge .sort_done

    mov r14d, r12d          ; j = i
    inc r14d
.inner:
    cmp r14d, r13d
    jge .outer_next

    ; Compare results[i] and results[j]
    lea rdi, [rel hm_results]
    imul rax, r12, 16
    movsd xmm0, [rdi + rax + 8]   ; sim[i]
    imul rax, r14, 16
    movsd xmm1, [rdi + rax + 8]   ; sim[j]

    ucomisd xmm1, xmm0      ; if sim[j] > sim[i], swap
    jbe .no_swap

    ; Swap
    lea rdi, [rel hm_results]
    imul rax, r12, 16
    mov rcx, [rdi + rax]
    movsd xmm0, [rdi + rax + 8]
    imul rdx, r14, 16
    mov r8, [rdi + rdx]
    movsd xmm1, [rdi + rdx + 8]
    mov [rdi + rax], r8
    movsd [rdi + rax + 8], xmm1
    mov [rdi + rdx], rcx
    movsd [rdi + rdx + 8], xmm0

.no_swap:
    inc r14d
    jmp .inner

.outer_next:
    inc r12d
    jmp .outer

.sort_done:
    pop r14
    pop r13
    pop r12
    ret

;; ============================================================
;; hm_find_entry_by_id — Find entry by ID (helper)
;; rax = entry_id
;; Returns: rax = entry ptr or 0
;; ============================================================
hm_find_entry_by_id:
    push rbx
    push r12
    push r13

    mov rbx, [rel hm_surface_base]
    mov r12, rax            ; target ID
    mov r13d, [rbx + HOLO_MEM_STATE_OFFSET + HMS_ENTRY_COUNT]

    xor ecx, ecx
.find_loop:
    cmp ecx, r13d
    jge .not_found

    imul rax, rcx, HOLO_MEM_ENTRY_SIZE
    lea rax, [rbx + HOLO_MEM_OFFSET + rax]
    cmp [rax + HME_ID], r12
    je .found

    inc ecx
    jmp .find_loop

.not_found:
    xor eax, eax
    jmp .find_ret

.found:
    ; rax already has entry ptr

.find_ret:
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; holo_mem_recent — Show recent entries
;; edi = limit (default 10)
;; ============================================================
global holo_mem_recent
holo_mem_recent:
    push rbx
    push r12
    push r13
    sub rsp, 8

    mov rbx, [rel hm_surface_base]

    mov r12d, edi           ; limit
    test r12d, r12d
    jnz .recent_limit_ok
    mov r12d, 10
.recent_limit_ok:

    mov r13d, [rbx + HOLO_MEM_STATE_OFFSET + HMS_ENTRY_COUNT]
    test r13d, r13d
    jz .recent_none

    lea rdi, [rel hm_recent_msg]
    call print_cstr

    ; Start from most recent
    mov ecx, r13d
    sub ecx, r12d
    test ecx, ecx
    jns .recent_start_ok
    xor ecx, ecx
.recent_start_ok:

.recent_loop:
    cmp ecx, r13d
    jge .recent_done

    ; Get entry
    imul rax, rcx, HOLO_MEM_ENTRY_SIZE
    lea rdi, [rbx + HOLO_MEM_OFFSET + rax]
    push rcx

    ; Print: [category] content
    push rdi
    lea rdi, [rel hm_entry_prefix]
    call print_cstr
    pop rdi

    push rdi
    mov eax, [rdi + HME_CATEGORY]
    cmp eax, CAT_COUNT
    jge .recent_unknown
    lea rsi, [rel cat_names]
    mov rdi, [rsi + rax * 8]
    call print_cstr
    jmp .recent_cat_done
.recent_unknown:
    mov rdi, rax
    call print_u64
.recent_cat_done:
    pop rdi

    push rdi
    lea rdi, [rel hm_entry_sep]
    call print_cstr
    pop rdi

    lea rdi, [rdi + HME_CONTENT]
    call print_cstr
    call print_newline

    pop rcx
    inc ecx
    jmp .recent_loop

.recent_none:
    lea rdi, [rel hm_recent_msg]
    call print_cstr
    lea rdi, [rel hm_no_entries]
    call print_cstr

.recent_done:
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; holo_mem_state — Show cognitive state
;; ============================================================
global holo_mem_state
holo_mem_state:
    push rbx
    push r12
    push r13
    sub rsp, 8

    mov rbx, [rel hm_surface_base]

    lea rdi, [rel hm_state_msg]
    call print_cstr

    ; Count entries by category
    mov r13d, [rbx + HOLO_MEM_STATE_OFFSET + HMS_ENTRY_COUNT]

    ; Count findings
    xor r12d, r12d          ; count
    xor ecx, ecx
.count_findings:
    cmp ecx, r13d
    jge .print_findings
    imul rax, rcx, HOLO_MEM_ENTRY_SIZE
    lea rax, [rbx + HOLO_MEM_OFFSET + rax]
    cmp dword [rax + HME_CATEGORY], CAT_FINDING
    jne .next_finding
    inc r12d
.next_finding:
    inc ecx
    jmp .count_findings

.print_findings:
    lea rdi, [rel hm_findings_lbl]
    call print_cstr
    mov rdi, r12
    call print_u64
    call print_newline

    ; Count warnings
    xor r12d, r12d
    xor ecx, ecx
.count_warnings:
    cmp ecx, r13d
    jge .print_warnings
    imul rax, rcx, HOLO_MEM_ENTRY_SIZE
    lea rax, [rbx + HOLO_MEM_OFFSET + rax]
    cmp dword [rax + HME_CATEGORY], CAT_WARNING
    jne .next_warning
    inc r12d
.next_warning:
    inc ecx
    jmp .count_warnings

.print_warnings:
    lea rdi, [rel hm_warnings_lbl]
    call print_cstr
    mov rdi, r12
    call print_u64
    call print_newline

    ; Count successes
    xor r12d, r12d
    xor ecx, ecx
.count_success:
    cmp ecx, r13d
    jge .print_success
    imul rax, rcx, HOLO_MEM_ENTRY_SIZE
    lea rax, [rbx + HOLO_MEM_OFFSET + rax]
    cmp dword [rax + HME_CATEGORY], CAT_SUCCESS
    jne .next_success
    inc r12d
.next_success:
    inc ecx
    jmp .count_success

.print_success:
    lea rdi, [rel hm_success_lbl]
    call print_cstr
    mov rdi, r12
    call print_u64
    call print_newline

    ; Count failures
    xor r12d, r12d
    xor ecx, ecx
.count_failed:
    cmp ecx, r13d
    jge .print_failed
    imul rax, rcx, HOLO_MEM_ENTRY_SIZE
    lea rax, [rbx + HOLO_MEM_OFFSET + rax]
    cmp dword [rax + HME_CATEGORY], CAT_FAILED
    jne .next_failed
    inc r12d
.next_failed:
    inc ecx
    jmp .count_failed

.print_failed:
    lea rdi, [rel hm_failed_lbl]
    call print_cstr
    mov rdi, r12
    call print_u64
    call print_newline

    ; Count insights
    xor r12d, r12d
    xor ecx, ecx
.count_insights:
    cmp ecx, r13d
    jge .print_insights
    imul rax, rcx, HOLO_MEM_ENTRY_SIZE
    lea rax, [rbx + HOLO_MEM_OFFSET + rax]
    cmp dword [rax + HME_CATEGORY], CAT_INSIGHT
    jne .next_insight
    inc r12d
.next_insight:
    inc ecx
    jmp .count_insights

.print_insights:
    lea rdi, [rel hm_insights_lbl]
    call print_cstr
    mov rdi, r12
    call print_u64
    call print_newline

    add rsp, 8
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; holo_mem_outcome — Record outcome for entry
;; edi = entry_id
;; esi = worked (1=success, 0=failure)
;; ============================================================
global holo_mem_outcome
holo_mem_outcome:
    push rbx
    push r12
    push r13
    sub rsp, 8

    mov rbx, [rel hm_surface_base]
    mov r12d, edi           ; entry_id
    mov r13d, esi           ; worked

    ; Find entry by ID
    mov eax, r12d
    call hm_find_entry_by_id
    test rax, rax
    jz .outcome_done

    mov rdi, rax            ; entry ptr

    ; Update outcome counters
    inc dword [rdi + HME_OUTCOMES]

    ; Update success ratio
    ; success_ratio = success_count / outcome_count
    ; We store running success_ratio, need to track count
    test r13d, r13d
    jz .outcome_failure

    ; Success: new_ratio = (old_ratio * (n-1) + 1.0) / n
    movss xmm0, [rdi + HME_SUCCESS]     ; old ratio
    mov eax, [rdi + HME_OUTCOMES]
    dec eax                              ; n-1
    cvtsi2ss xmm1, eax
    mulss xmm0, xmm1                     ; old_ratio * (n-1)
    mov eax, 0x3F800000                  ; 1.0f
    movd xmm2, eax
    addss xmm0, xmm2                     ; + 1.0
    mov eax, [rdi + HME_OUTCOMES]
    cvtsi2ss xmm1, eax
    divss xmm0, xmm1                     ; / n
    movss [rdi + HME_SUCCESS], xmm0
    jmp .outcome_done

.outcome_failure:
    ; Failure: new_ratio = (old_ratio * (n-1)) / n
    movss xmm0, [rdi + HME_SUCCESS]
    mov eax, [rdi + HME_OUTCOMES]
    dec eax
    cvtsi2ss xmm1, eax
    mulss xmm0, xmm1
    mov eax, [rdi + HME_OUTCOMES]
    cvtsi2ss xmm1, eax
    divss xmm0, xmm1
    movss [rdi + HME_SUCCESS], xmm0

.outcome_done:
    add rsp, 8
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; holo_mem_summary — Print summary statistics
;; ============================================================
global holo_mem_summary
holo_mem_summary:
    push rbx
    sub rsp, 8

    mov rbx, [rel hm_surface_base]

    lea rdi, [rel hm_summary_msg]
    call print_cstr

    lea rdi, [rel hm_entries_lbl]
    call print_cstr
    mov edi, [rbx + HOLO_MEM_STATE_OFFSET + HMS_ENTRY_COUNT]
    call print_u64
    call print_newline

    add rsp, 8
    pop rbx
    ret

;; ============================================================
;; fnv64_hash_string — Hash string with FNV-1a
;; rdi = string pointer
;; Returns: eax = 32-bit hash
;; ============================================================
global fnv64_hash_string
fnv64_hash_string:
    mov rax, 0xCBF29CE484222325    ; FNV offset basis
    mov rcx, 0x100000001B3         ; FNV prime

.hash_loop:
    movzx edx, byte [rdi]
    test dl, dl
    jz .hash_done
    xor al, dl
    imul rax, rcx
    inc rdi
    jmp .hash_loop

.hash_done:
    ; Fold to 32 bits
    mov rdx, rax
    shr rdx, 32
    xor eax, edx
    ret

;; ============================================================
;; INLINE VSA FUNCTIONS (standalone, no UHMA dependency)
;; ============================================================

;; ------------------------------------------------------------
;; holo_gen_vec — Generate pseudo-random vector from seed
;; edi = seed (32-bit)
;; rsi = output vector ptr (1024 f64 = 8192 bytes)
;; Uses xorshift64 PRNG, normalizes to approximately unit length
;; ------------------------------------------------------------
global holo_gen_vec
holo_gen_vec:
    push rbx
    push r12
    push r13

    mov r12, rsi                ; output ptr

    ; Expand seed to 64-bit state
    mov eax, edi
    test eax, eax
    jnz .seed_ok
    mov eax, 0x12345678         ; default seed if 0
.seed_ok:
    ; Mix seed with golden ratio
    imul rax, rax, 0x9E3779B97F4A7C15
    mov rbx, rax                ; PRNG state

    mov r13d, HOLO_VEC_DIM      ; counter

.gen_loop:
    ; xorshift64
    mov rax, rbx
    shr rax, 12
    xor rbx, rax
    mov rax, rbx
    shl rax, 25
    xor rbx, rax
    mov rax, rbx
    shr rax, 27
    xor rbx, rax

    ; Convert to f64 in range [-1, 1]
    ; Take bits, mask to 52-bit mantissa, create double
    mov rax, rbx
    imul rax, rax, 0x5851F42D4C957F2D  ; more mixing

    ; Create f64: sign from bit 63, exponent 0x3FF (1.x), mantissa from bits
    mov rcx, rax
    and rcx, 0x000FFFFFFFFFFFFF     ; mantissa bits
    or rcx, 0x3FF0000000000000      ; exponent for [1.0, 2.0)
    mov [r12], rcx

    ; Subtract 1.5 to center around 0 in range [-0.5, 0.5]
    movsd xmm0, [r12]
    mov rax, 0x3FF8000000000000     ; 1.5
    movq xmm1, rax
    subsd xmm0, xmm1

    ; Scale to [-1, 1]
    mov rax, 0x4000000000000000     ; 2.0
    movq xmm1, rax
    mulsd xmm0, xmm1
    movsd [r12], xmm0

    add r12, 8
    dec r13d
    jnz .gen_loop

    ; Normalize the vector
    sub r12, HOLO_VEC_BYTES     ; restore ptr to start
    mov rdi, r12
    call vsa_normalize

    pop r13
    pop r12
    pop rbx
    ret

;; ------------------------------------------------------------
;; holo_bind_f64 — Element-wise multiplication (binding)
;; rdi = vec_a ptr (input)
;; rsi = vec_b ptr (input)
;; rdx = output ptr (can be same as vec_a or vec_b)
;; ------------------------------------------------------------
global holo_bind_f64
holo_bind_f64:
    push rbx
    push r12
    push r13
    push r14

    mov r12, rdi                ; vec_a
    mov r13, rsi                ; vec_b
    mov r14, rdx                ; output

    mov ebx, HOLO_VEC_DIM / 4   ; process 4 at a time with AVX

.bind_loop:
    ; Load 4 doubles from each vector
    vmovupd ymm0, [r12]
    vmovupd ymm1, [r13]
    vmulpd ymm0, ymm0, ymm1
    vmovupd [r14], ymm0

    add r12, 32
    add r13, 32
    add r14, 32
    dec ebx
    jnz .bind_loop

    vzeroupper

    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ------------------------------------------------------------
;; holo_superpose_f64 — Element-wise addition (superposition)
;; rdi = target ptr (accumulator, modified in place)
;; rsi = source ptr (added to target)
;; ------------------------------------------------------------
global holo_superpose_f64
holo_superpose_f64:
    push rbx
    push r12
    push r13

    mov r12, rdi                ; target
    mov r13, rsi                ; source

    mov ebx, HOLO_VEC_DIM / 4   ; process 4 at a time with AVX

.super_loop:
    vmovupd ymm0, [r12]
    vmovupd ymm1, [r13]
    vaddpd ymm0, ymm0, ymm1
    vmovupd [r12], ymm0

    add r12, 32
    add r13, 32
    dec ebx
    jnz .super_loop

    vzeroupper

    pop r13
    pop r12
    pop rbx
    ret

;; ------------------------------------------------------------
;; holo_cosim_f64 — Cosine similarity
;; rdi = vec_a ptr
;; rsi = vec_b ptr
;; Returns: xmm0 = dot(a,b) / (|a| * |b|)
;; ------------------------------------------------------------
global holo_cosim_f64
holo_cosim_f64:
    push rbx
    push r12
    push r13
    sub rsp, 24                 ; local storage for dot, mag_a, mag_b

    mov r12, rdi                ; vec_a
    mov r13, rsi                ; vec_b

    ; Initialize accumulators
    vxorpd ymm2, ymm2, ymm2     ; dot product
    vxorpd ymm3, ymm3, ymm3     ; magnitude a squared
    vxorpd ymm4, ymm4, ymm4     ; magnitude b squared

    mov ebx, HOLO_VEC_DIM / 4

.cosim_loop:
    vmovupd ymm0, [r12]         ; a
    vmovupd ymm1, [r13]         ; b

    ; dot += a * b
    vmulpd ymm5, ymm0, ymm1
    vaddpd ymm2, ymm2, ymm5

    ; mag_a += a * a
    vmulpd ymm5, ymm0, ymm0
    vaddpd ymm3, ymm3, ymm5

    ; mag_b += b * b
    vmulpd ymm5, ymm1, ymm1
    vaddpd ymm4, ymm4, ymm5

    add r12, 32
    add r13, 32
    dec ebx
    jnz .cosim_loop

    ; Horizontal sum for dot product (ymm2)
    vextractf128 xmm0, ymm2, 1
    vaddpd xmm2, xmm2, xmm0
    vhaddpd xmm2, xmm2, xmm2
    movsd [rsp], xmm2           ; dot

    ; Horizontal sum for mag_a (ymm3)
    vextractf128 xmm0, ymm3, 1
    vaddpd xmm3, xmm3, xmm0
    vhaddpd xmm3, xmm3, xmm3
    sqrtsd xmm3, xmm3           ; sqrt(mag_a)
    movsd [rsp + 8], xmm3       ; |a|

    ; Horizontal sum for mag_b (ymm4)
    vextractf128 xmm0, ymm4, 1
    vaddpd xmm4, xmm4, xmm0
    vhaddpd xmm4, xmm4, xmm4
    sqrtsd xmm4, xmm4           ; sqrt(mag_b)
    movsd [rsp + 16], xmm4      ; |b|

    vzeroupper

    ; cosim = dot / (|a| * |b|)
    movsd xmm0, [rsp]           ; dot
    movsd xmm1, [rsp + 8]       ; |a|
    movsd xmm2, [rsp + 16]      ; |b|
    mulsd xmm1, xmm2            ; |a| * |b|

    ; Check for zero magnitude
    xorpd xmm3, xmm3
    ucomisd xmm1, xmm3
    jbe .cosim_zero

    divsd xmm0, xmm1
    jmp .cosim_done

.cosim_zero:
    xorpd xmm0, xmm0            ; return 0 if either vector is zero

.cosim_done:
    add rsp, 24
    pop r13
    pop r12
    pop rbx
    ret

;; ------------------------------------------------------------
;; vsa_normalize — Normalize vector to unit length
;; rdi = vector ptr (modified in place)
;; ------------------------------------------------------------
global vsa_normalize
vsa_normalize:
    push rbx
    push r12
    sub rsp, 8

    mov r12, rdi                ; vector ptr

    ; Compute magnitude squared
    vxorpd ymm2, ymm2, ymm2
    mov ebx, HOLO_VEC_DIM / 4
    mov rdi, r12

.norm_mag_loop:
    vmovupd ymm0, [rdi]
    vmulpd ymm0, ymm0, ymm0
    vaddpd ymm2, ymm2, ymm0
    add rdi, 32
    dec ebx
    jnz .norm_mag_loop

    ; Horizontal sum
    vextractf128 xmm0, ymm2, 1
    vaddpd xmm2, xmm2, xmm0
    vhaddpd xmm2, xmm2, xmm2

    ; sqrt to get magnitude
    sqrtsd xmm2, xmm2
    movsd [rsp], xmm2

    vzeroupper

    ; Check for zero magnitude
    xorpd xmm0, xmm0
    ucomisd xmm2, xmm0
    jbe .norm_done              ; skip if zero

    ; Divide all elements by magnitude
    movsd xmm1, [rsp]           ; magnitude
    mov ebx, HOLO_VEC_DIM / 4
    mov rdi, r12

    ; Broadcast magnitude to ymm1 for parallel division
    vbroadcastsd ymm1, xmm1

.norm_div_loop:
    vmovupd ymm0, [rdi]
    vdivpd ymm0, ymm0, ymm1
    vmovupd [rdi], ymm0
    add rdi, 32
    dec ebx
    jnz .norm_div_loop

    vzeroupper

.norm_done:
    add rsp, 8
    pop r12
    pop rbx
    ret
