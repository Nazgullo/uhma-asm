; vsa_ops.asm — The Rosetta Stone: Code Becomes Math
;
; This module implements the Unified Field Theory:
;   Code → Vector → Interference → Emergence
;
; Every x86 instruction becomes a 1024-dim f64 vector.
; Safety checking becomes geometric: DotProduct(Code, Safe) > 0
; Learning becomes superposition: CodeTrace += Code
; Communication becomes projection: SharedVector ⊗ LocalContext
;
; "One Math" — No more if statements for safety, just Linear Algebra.

%include "syscalls.inc"
%include "constants.inc"
%include "vsa_ops.inc"

section .data
    align 8
    ; Pre-computed safety vector (initialized at startup)
    ; High values in safe dimensions, negative in dangerous dimensions
    safety_vec_initialized: dq 0

    ; Opcode category weights for semantic similarity
    cat_data_movement:  dq 1.0
    cat_arithmetic:     dq 0.8
    cat_logic:          dq 0.8
    cat_comparison:     dq 0.6
    cat_control_flow:   dq 0.4      ; lower weight = more scrutiny
    cat_memory:         dq 0.5
    cat_system:         dq -1.0     ; negative = dangerous

    ; Safety dimension weights
    safe_weight_bounded:    dq 1.0
    safe_weight_balanced:   dq 1.0
    safe_weight_pure:       dq 0.8
    safe_weight_priv:       dq -2.0     ; heavily penalize privileged
    safe_weight_syscall:    dq -2.0     ; heavily penalize syscall
    safe_weight_interrupt:  dq -2.0     ; heavily penalize interrupt

section .bss
    align 32
    ; Safety template vector (1024 x f64 = 8KB)
    safety_template:    resq 1024

    ; Dangerous code template vector
    danger_template:    resq 1024

    ; Scratch vector for encoding
    scratch_vec:        resq 1024

section .text

extern holo_gen_vec
extern holo_dot_f64
extern holo_superpose_f64
extern holo_normalize_f64
extern holo_scale_f64
extern vsa_zero
extern vsa_superpose
extern vsa_normalize
extern vsa_permute
extern vsa_dot
extern decode_instruction_full

;; ============================================================
;; init_safety_vectors()
;; Initializes the safety and danger template vectors.
;; Called once at startup to prepare geometric safety gate.
;; ============================================================
global init_safety_vectors
init_safety_vectors:
    push rbx
    push r12
    sub rsp, 8          ; align stack

    ; Check if already initialized
    cmp qword [rel safety_vec_initialized], 1
    je .already_init

    ; Zero both template vectors
    lea rdi, [rel safety_template]
    call vsa_zero

    lea rdi, [rel danger_template]
    call vsa_zero

    ; --- Build Safety Template ---
    ; Set positive values in "good" dimensions
    lea rbx, [rel safety_template]

    ; VDIM_BOUNDED_JUMP = strong positive
    mov rax, 0x3FF0000000000000     ; 1.0
    mov [rbx + VDIM_BOUNDED_JUMP * 8], rax

    ; VDIM_STACK_BALANCED = strong positive
    mov [rbx + VDIM_STACK_BALANCED * 8], rax

    ; VDIM_PURE_COMPUTE = positive
    mov rax, 0x3FE0000000000000     ; 0.5
    mov [rbx + VDIM_PURE_COMPUTE * 8], rax

    ; VDIM_IDEMPOTENT = positive
    mov [rbx + VDIM_IDEMPOTENT * 8], rax

    ; Set negative values in "bad" dimensions
    mov rax, 0xC000000000000000     ; -2.0
    mov [rbx + VDIM_PRIVILEGED * 8], rax
    mov [rbx + VDIM_SYSCALL * 8], rax
    mov [rbx + VDIM_INTERRUPT * 8], rax

    ; Moderate negative for memory writes (careful, not forbidden)
    mov rax, 0xBFE0000000000000     ; -0.5
    mov [rbx + VDIM_WRITES_MEMORY * 8], rax

    ; Moderate negative for unbound control flow
    mov rax, 0xBFD0000000000000     ; -0.25
    mov [rbx + VDIM_CONTROL_FLOW * 8], rax

    ; Normalize the safety template
    lea rdi, [rel safety_template]
    call holo_normalize_f64

    ; --- Build Danger Template ---
    ; Set positive values in dangerous dimensions
    lea rbx, [rel danger_template]

    mov rax, 0x3FF0000000000000     ; 1.0
    mov [rbx + VDIM_PRIVILEGED * 8], rax
    mov [rbx + VDIM_SYSCALL * 8], rax
    mov [rbx + VDIM_INTERRUPT * 8], rax

    ; High value for callee clobber
    mov rax, 0x3FE0000000000000     ; 0.5
    mov [rbx + VDIM_CALLEE_CLOBBER * 8], rax

    ; Normalize the danger template
    lea rdi, [rel danger_template]
    call holo_normalize_f64

    ; Mark as initialized
    mov qword [rel safety_vec_initialized], 1

.already_init:
    add rsp, 8
    pop r12
    pop rbx
    ret

;; ============================================================
;; opcode_to_hash(opcat, dst_reg, src_reg, flags) → eax (u32 hash)
;; edi=opcode category (OPCAT_*)
;; esi=destination register (REGVEC_* or 0)
;; edx=source register (REGVEC_* or 0)
;; ecx=flags (memory access, etc.)
;;
;; Combines opcode info into a single hash for vector generation.
;; The hash encodes semantic similarity: similar ops → similar hash
;; ============================================================
global opcode_to_hash
opcode_to_hash:
    ; hash = (opcat << 16) | (src << 8) | dst | (flags << 24)
    mov eax, edi
    shl eax, 16         ; opcat in high word

    mov r8d, esi
    and r8d, 0xFF
    or eax, r8d         ; dst in low byte

    mov r8d, edx
    shl r8d, 8
    or eax, r8d         ; src in second byte

    mov r8d, ecx
    shl r8d, 24
    or eax, r8d         ; flags in highest byte

    ret

;; ============================================================
;; opcode_to_vector(hash, out_ptr)
;; edi=opcode hash (from opcode_to_hash)
;; rsi=output vector ptr (f64[1024])
;;
;; Generates a unique 1024-dim vector from an opcode hash.
;; Uses holo_gen_vec as the base, then sets semantic dimensions.
;; ============================================================
global opcode_to_vector
opcode_to_vector:
    push rbx
    push r12
    push r13
    sub rsp, 8          ; align

    mov r12d, edi       ; hash
    mov r13, rsi        ; output ptr

    ; 1. Generate base vector from hash
    mov edi, r12d
    mov rsi, r13
    call holo_gen_vec

    ; 2. Set semantic dimensions based on opcode category
    ; Extract category from hash (bits 16-31)
    mov eax, r12d
    shr eax, 16
    and eax, 0xFFFF

    ; Classify and set appropriate dimensions
    cmp ax, OPCAT_SYSCALL
    je .set_syscall_dims
    cmp ax, OPCAT_INT
    je .set_int_dims
    cmp ax, OPCAT_PRIV
    je .set_priv_dims
    cmp ax, OPCAT_JMP
    jb .check_control_flow
    cmp ax, OPCAT_LOOP
    jbe .set_control_flow_dims
    cmp ax, OPCAT_STORE
    je .set_write_mem_dims
    cmp ax, OPCAT_LOAD
    je .set_read_mem_dims
    cmp ax, OPCAT_PUSH
    je .set_stack_dims
    cmp ax, OPCAT_POP
    je .set_stack_dims
    jmp .set_pure_dims

.check_control_flow:
    cmp ax, OPCAT_JMP
    jae .set_control_flow_dims
    jmp .set_pure_dims

.set_syscall_dims:
    mov rax, 0x3FF0000000000000     ; 1.0
    mov [r13 + VDIM_SYSCALL * 8], rax
    mov [r13 + VDIM_PRIVILEGED * 8], rax
    jmp .dims_done

.set_int_dims:
    mov rax, 0x3FF0000000000000     ; 1.0
    mov [r13 + VDIM_INTERRUPT * 8], rax
    jmp .dims_done

.set_priv_dims:
    mov rax, 0x3FF0000000000000     ; 1.0
    mov [r13 + VDIM_PRIVILEGED * 8], rax
    jmp .dims_done

.set_control_flow_dims:
    mov rax, 0x3FF0000000000000     ; 1.0
    mov [r13 + VDIM_CONTROL_FLOW * 8], rax
    jmp .dims_done

.set_write_mem_dims:
    mov rax, 0x3FF0000000000000     ; 1.0
    mov [r13 + VDIM_WRITES_MEMORY * 8], rax
    jmp .dims_done

.set_read_mem_dims:
    mov rax, 0x3FF0000000000000     ; 1.0
    mov [r13 + VDIM_READS_MEMORY * 8], rax
    jmp .dims_done

.set_stack_dims:
    mov rax, 0x3FF0000000000000     ; 1.0
    mov [r13 + VDIM_MODIFIES_STACK * 8], rax
    jmp .dims_done

.set_pure_dims:
    ; Arithmetic, logic, comparison — pure computation
    mov rax, 0x3FF0000000000000     ; 1.0
    mov [r13 + VDIM_PURE_COMPUTE * 8], rax
    mov [r13 + VDIM_IDEMPOTENT * 8], rax

.dims_done:
    ; Normalize the vector
    mov rdi, r13
    call holo_normalize_f64

    add rsp, 8
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; classify_opcode_vec(byte) → eax (OPCAT_*)
;; dil=first byte of instruction (after prefixes)
;; Returns the opcode category for vector generation.
;; ============================================================
global classify_opcode_vec
classify_opcode_vec:
    movzx eax, dil

    ; Handle common x86_64 opcodes
    cmp al, 0x90
    je .nop

    ; MOV family: 0x88-0x8B, 0xB0-0xBF, 0xC6-0xC7
    cmp al, 0x88
    jb .check_arith
    cmp al, 0x8B
    jbe .mov
    cmp al, 0xB0
    jb .check_lea
    cmp al, 0xBF
    jbe .mov
    cmp al, 0xC6
    jb .check_ret
    cmp al, 0xC7
    jbe .mov

.check_arith:
    ; ADD: 0x00-0x05, 0x80-0x83 (with modrm)
    cmp al, 0x00
    jb .check_push
    cmp al, 0x05
    jbe .add
    cmp al, 0x28
    jb .check_sub
    cmp al, 0x2D
    jbe .sub

.check_sub:
    ; SUB: 0x28-0x2D
    cmp al, 0x28
    jb .check_and
    cmp al, 0x2D
    jbe .sub

.check_and:
    ; AND: 0x20-0x25
    cmp al, 0x20
    jb .check_or
    cmp al, 0x25
    jbe .and_op

.check_or:
    ; OR: 0x08-0x0D
    cmp al, 0x08
    jb .check_xor
    cmp al, 0x0D
    jbe .or_op

.check_xor:
    ; XOR: 0x30-0x35
    cmp al, 0x30
    jb .check_cmp
    cmp al, 0x35
    jbe .xor_op

.check_cmp:
    ; CMP: 0x38-0x3D
    cmp al, 0x38
    jb .check_push
    cmp al, 0x3D
    jbe .cmp

.check_push:
    ; PUSH: 0x50-0x57, 0x68, 0x6A
    cmp al, 0x50
    jb .check_pop
    cmp al, 0x57
    jbe .push
    cmp al, 0x68
    je .push
    cmp al, 0x6A
    je .push

.check_pop:
    ; POP: 0x58-0x5F
    cmp al, 0x58
    jb .check_jmp
    cmp al, 0x5F
    jbe .pop

.check_jmp:
    ; JMP: 0xE9, 0xEB, 0xFF/4
    cmp al, 0xE9
    je .jmp
    cmp al, 0xEB
    je .jmp

    ; Jcc: 0x70-0x7F (short), 0x0F 0x80-0x8F (long)
    cmp al, 0x70
    jb .check_call
    cmp al, 0x7F
    jbe .jcc

.check_call:
    ; CALL: 0xE8, 0xFF/2
    cmp al, 0xE8
    je .call

.check_ret:
    ; RET: 0xC3, 0xC2
    cmp al, 0xC3
    je .ret
    cmp al, 0xC2
    je .ret

.check_lea:
    ; LEA: 0x8D
    cmp al, 0x8D
    je .lea

    ; SYSCALL: 0x0F 0x05 (but we only see first byte here)
    cmp al, 0x0F
    je .two_byte    ; need to check second byte

    ; INT: 0xCD, INT3: 0xCC
    cmp al, 0xCC
    je .int
    cmp al, 0xCD
    je .int

    ; Default: unknown
    mov eax, OPCAT_NOP
    ret

.nop:
    mov eax, OPCAT_NOP
    ret
.mov:
    mov eax, OPCAT_MOV
    ret
.add:
    mov eax, OPCAT_ADD
    ret
.sub:
    mov eax, OPCAT_SUB
    ret
.and_op:
    mov eax, OPCAT_AND
    ret
.or_op:
    mov eax, OPCAT_OR
    ret
.xor_op:
    mov eax, OPCAT_XOR
    ret
.cmp:
    mov eax, OPCAT_CMP
    ret
.push:
    mov eax, OPCAT_PUSH
    ret
.pop:
    mov eax, OPCAT_POP
    ret
.jmp:
    mov eax, OPCAT_JMP
    ret
.jcc:
    mov eax, OPCAT_JCC
    ret
.call:
    mov eax, OPCAT_CALL
    ret
.ret:
    mov eax, OPCAT_RET
    ret
.lea:
    mov eax, OPCAT_LEA
    ret
.int:
    mov eax, OPCAT_INT
    ret
.two_byte:
    ; Would need second byte to distinguish SYSCALL from other 0F prefixed
    ; For now, treat all 0F prefix as potentially dangerous
    mov eax, OPCAT_SYSCALL
    ret

;; ============================================================
;; encode_code_to_vector(code_ptr, code_len, out_vec)
;; rdi=code pointer
;; esi=code length in bytes
;; rdx=output vector (f64[1024])
;;
;; Encodes an entire code sequence as a superposition of
;; instruction vectors. Position encoding via permutation.
;; This is the "Code → Vector" transformation.
;; ============================================================
global encode_code_to_vector
encode_code_to_vector:
    push rbx
    push r12
    push r13
    push r14
    push r15
    sub rsp, HOLO_VEC_BYTES + 16      ; temp vector + locals

    mov r12, rdi          ; code_ptr
    mov r13d, esi         ; code_len
    mov r14, rdx          ; out_vec

    ; Zero the output vector
    mov rdi, r14
    call vsa_zero

    ; Walk through code, decode each instruction, encode to vector
    xor r15d, r15d        ; position counter
    mov rbx, r12          ; current code ptr

.encode_loop:
    ; Check if we've processed all bytes
    mov eax, r15d
    cmp eax, r13d
    jge .encode_done

    ; Classify the opcode at current position
    movzx edi, byte [rbx]

    ; Skip REX prefix if present (0x40-0x4F)
    cmp dil, 0x40
    jb .no_rex
    cmp dil, 0x4F
    ja .no_rex
    inc rbx
    inc r15d
    cmp r15d, r13d
    jge .encode_done
    movzx edi, byte [rbx]
.no_rex:

    call classify_opcode_vec
    mov edi, eax          ; opcat

    ; Generate hash (simplified: just opcat, no register analysis yet)
    xor esi, esi          ; dst_reg = 0
    xor edx, edx          ; src_reg = 0
    xor ecx, ecx          ; flags = 0
    call opcode_to_hash

    ; Generate vector for this instruction
    mov edi, eax
    lea rsi, [rsp]        ; temp vector
    call opcode_to_vector

    ; Permute by position (position encoding)
    lea rdi, [rsp]        ; src = temp
    lea rsi, [rel scratch_vec]  ; dst = scratch
    mov edx, r15d         ; shift = position
    call vsa_permute

    ; Superpose into output
    mov rdi, r14          ; out_vec (a += b)
    lea rsi, [rel scratch_vec]
    call vsa_superpose

    ; Advance to next instruction (simplified: assume 1-byte for now)
    ; In reality, should use decode_instruction_full for proper length
    inc rbx
    inc r15d
    jmp .encode_loop

.encode_done:
    ; Normalize the final vector
    mov rdi, r14
    call vsa_normalize

    add rsp, HOLO_VEC_BYTES + 16
    pop r15
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

;; ============================================================
;; check_code_safety(code_vec) → xmm0 (safety score f64)
;; rdi=code vector ptr (f64[1024])
;;
;; Returns dot product with safety template.
;; Positive = safe, Negative = dangerous.
;; This is the "Geometric Gate" — safety via vector math.
;; ============================================================
global check_code_safety
check_code_safety:
    push rbx
    mov rbx, rdi          ; save code_vec

    ; Ensure safety vectors are initialized
    call init_safety_vectors

    ; Compute dot product with safety template
    mov rdi, rbx
    lea rsi, [rel safety_template]
    call holo_dot_f64
    ; xmm0 = safety score (positive = safe)

    pop rbx
    ret

;; ============================================================
;; check_code_danger(code_vec) → xmm0 (danger score f64)
;; rdi=code vector ptr (f64[1024])
;;
;; Returns dot product with danger template.
;; Higher = more dangerous.
;; ============================================================
global check_code_danger
check_code_danger:
    push rbx
    mov rbx, rdi

    call init_safety_vectors

    mov rdi, rbx
    lea rsi, [rel danger_template]
    call holo_dot_f64

    pop rbx
    ret

;; ============================================================
;; code_vectors_similar(vec_a, vec_b) → xmm0 (similarity f64)
;; rdi=vector A ptr, rsi=vector B ptr
;;
;; Returns cosine similarity between two code vectors.
;; Used for pattern matching and fuzzy dispatch.
;; ============================================================
global code_vectors_similar
code_vectors_similar:
    ; Both vectors should already be normalized
    call holo_dot_f64
    ; For normalized vectors, dot = cosine similarity
    ret

;; ============================================================
;; get_safety_template() → rax (ptr to safety template)
;; Returns pointer to the safety template vector.
;; ============================================================
global get_safety_template
get_safety_template:
    call init_safety_vectors
    lea rax, [rel safety_template]
    ret

;; ============================================================
;; get_danger_template() → rax (ptr to danger template)
;; Returns pointer to the danger template vector.
;; ============================================================
global get_danger_template
get_danger_template:
    call init_safety_vectors
    lea rax, [rel danger_template]
    ret

;; ============================================================
;; encode_region_to_vector(region_ptr, out_vec)
;; rdi=region header ptr
;; rsi=output vector ptr (f64[1024])
;;
;; Encodes a dispatch region's code into a vector.
;; This enables vector-based region matching.
;; ============================================================
global encode_region_to_vector
encode_region_to_vector:
    push rbx
    push r12

    mov rbx, rdi          ; region_ptr
    mov r12, rsi          ; out_vec

    ; Get code pointer (after header)
    lea rdi, [rbx + RHDR_SIZE]

    ; Get code length from header
    movzx esi, word [rbx + RHDR_CODE_LEN]

    ; Encode
    mov rdx, r12
    call encode_code_to_vector

    pop r12
    pop rbx
    ret

;; ============================================================
;; verify_code_geometric(code_ptr, len) → eax (1=safe, 0=unsafe)
;; rdi=code pointer
;; esi=code length
;;
;; Geometric safety verification: encode code to vector,
;; check against safety template. No if-statements in the
;; safety logic itself — pure linear algebra.
;; ============================================================
global verify_code_geometric
verify_code_geometric:
    push rbx
    push r12
    sub rsp, HOLO_VEC_BYTES + 8       ; temp vector + align

    mov r12, rdi          ; code_ptr
    mov ebx, esi          ; code_len

    ; Encode code to vector
    mov rdi, r12
    mov esi, ebx
    lea rdx, [rsp]
    call encode_code_to_vector

    ; Check safety
    lea rdi, [rsp]
    call check_code_safety
    ; xmm0 = safety score

    ; Compare against threshold
    mov rax, SAFETY_THRESHOLD
    movq xmm1, rax

    ucomisd xmm0, xmm1
    ja .is_safe

    ; Not safe
    xor eax, eax
    jmp .done

.is_safe:
    mov eax, 1

.done:
    add rsp, HOLO_VEC_BYTES + 8
    pop r12
    pop rbx
    ret
