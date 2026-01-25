; vsa_ops.inc — Rosetta Stone: Opcode-to-Vector Mapping
; The bridge that makes Code into Math.
;
; Every x86 instruction becomes a vector. Every register becomes a vector.
; This enables:
;   1. Geometric Safety: DotProduct(CodeVector, SafetyVector) > 0 = safe
;   2. Semantic Similarity: similar opcodes have similar vectors
;   3. Interference Learning: code patterns superpose into traces
;   4. Telepathic Communication: instances share code as vectors
;
; Philosophy: "One Math" — Linear Algebra replaces if statements

%ifndef VSA_OPS_INC
%define VSA_OPS_INC

;; ============================================================
;; Opcode Category Seeds (high 16 bits of vector hash)
;; Opcodes in the same category share vector similarity
;; ============================================================

; Data Movement Family — vectors cluster together
%define OPCAT_MOV           0x1000      ; data movement
%define OPCAT_PUSH          0x1100      ; stack push
%define OPCAT_POP           0x1200      ; stack pop
%define OPCAT_XCHG          0x1300      ; exchange

; Arithmetic Family
%define OPCAT_ADD           0x2000      ; addition
%define OPCAT_SUB           0x2100      ; subtraction
%define OPCAT_MUL           0x2200      ; multiplication
%define OPCAT_DIV           0x2300      ; division
%define OPCAT_INC           0x2400      ; increment
%define OPCAT_DEC           0x2500      ; decrement
%define OPCAT_NEG           0x2600      ; negation

; Logic Family
%define OPCAT_AND           0x3000      ; bitwise AND
%define OPCAT_OR            0x3100      ; bitwise OR
%define OPCAT_XOR           0x3200      ; bitwise XOR
%define OPCAT_NOT           0x3300      ; bitwise NOT
%define OPCAT_SHL           0x3400      ; shift left
%define OPCAT_SHR           0x3500      ; shift right
%define OPCAT_ROL           0x3600      ; rotate left
%define OPCAT_ROR           0x3700      ; rotate right

; Comparison Family
%define OPCAT_CMP           0x4000      ; compare
%define OPCAT_TEST          0x4100      ; test (AND without store)

; Control Flow Family
%define OPCAT_JMP           0x5000      ; unconditional jump
%define OPCAT_JCC           0x5100      ; conditional jump
%define OPCAT_CALL          0x5200      ; function call
%define OPCAT_RET           0x5300      ; return
%define OPCAT_LOOP          0x5400      ; loop instructions

; Memory Family
%define OPCAT_LEA           0x6000      ; load effective address
%define OPCAT_LOAD          0x6100      ; memory load
%define OPCAT_STORE         0x6200      ; memory store

; System Family (potentially dangerous)
%define OPCAT_SYSCALL       0x7000      ; syscall
%define OPCAT_INT           0x7100      ; interrupt
%define OPCAT_PRIV          0x7200      ; privileged
%define OPCAT_NOP           0x7F00      ; no operation

;; ============================================================
;; Register Vector Seeds (low 16 bits distinguish registers)
;; Registers in the same family have similar vectors
;; ============================================================

; General Purpose (cluster by role)
%define REGVEC_RAX          0x0100      ; accumulator
%define REGVEC_RBX          0x0201      ; base (callee-saved)
%define REGVEC_RCX          0x0102      ; counter
%define REGVEC_RDX          0x0103      ; data
%define REGVEC_RSI          0x0104      ; source index
%define REGVEC_RDI          0x0105      ; destination index
%define REGVEC_RBP          0x0206      ; base pointer (callee-saved)
%define REGVEC_RSP          0x0307      ; stack pointer (special)
%define REGVEC_R8           0x0108      ; extended caller-saved
%define REGVEC_R9           0x0109
%define REGVEC_R10          0x010A
%define REGVEC_R11          0x010B
%define REGVEC_R12          0x020C      ; callee-saved
%define REGVEC_R13          0x020D
%define REGVEC_R14          0x020E
%define REGVEC_R15          0x020F

; Special Registers (distinct cluster)
%define REGVEC_RIP          0x0400      ; instruction pointer
%define REGVEC_RFLAGS       0x0500      ; flags register

;; ============================================================
;; Safety Vector Dimensions (which dimensions encode safety properties)
;; The "Geometric Gate" checks these dimensions
;; ============================================================

; Dimension assignments (out of 1024):
; 0-255:    Opcode signature (from category + specific opcode)
; 256-511:  Operand pattern (register combinations)
; 512-767:  Memory access pattern
; 768-1022: Semantic properties
; 1023:     Valence channel (already defined)

%define VDIM_OPCODE_START   0
%define VDIM_OPCODE_END     255
%define VDIM_OPERAND_START  256
%define VDIM_OPERAND_END    511
%define VDIM_MEMORY_START   512
%define VDIM_MEMORY_END     767
%define VDIM_SEMANTIC_START 768
%define VDIM_SEMANTIC_END   1022

; Safety property dimensions (within semantic range)
%define VDIM_WRITES_MEMORY  768         ; 1.0 if instruction writes to memory
%define VDIM_READS_MEMORY   769         ; 1.0 if instruction reads from memory
%define VDIM_MODIFIES_STACK 770         ; 1.0 if instruction changes RSP
%define VDIM_CONTROL_FLOW   771         ; 1.0 if instruction can change RIP
%define VDIM_PRIVILEGED     772         ; 1.0 if instruction is privileged
%define VDIM_SYSCALL        773         ; 1.0 if instruction is syscall
%define VDIM_INTERRUPT      774         ; 1.0 if instruction triggers interrupt
%define VDIM_CALLEE_CLOBBER 775         ; 1.0 if clobbers callee-saved without save
%define VDIM_BOUNDED_JUMP   776         ; 1.0 if jump target is within bounds
%define VDIM_STACK_BALANCED 777         ; 1.0 if push/pop are balanced
%define VDIM_PURE_COMPUTE   778         ; 1.0 if no side effects
%define VDIM_IDEMPOTENT     779         ; 1.0 if repeating is safe

;; ============================================================
;; Pre-computed Safety Vectors (dot product for quick checks)
;; ============================================================

; SAFE_CODE_SIGNATURE: Dot product with this should be positive for safe code
; High values in: VDIM_BOUNDED_JUMP, VDIM_STACK_BALANCED, VDIM_PURE_COMPUTE
; Low/negative in: VDIM_PRIVILEGED, VDIM_SYSCALL, VDIM_INTERRUPT

; DANGEROUS_CODE_SIGNATURE: Dot product > 0 means potentially dangerous
; High values in: VDIM_PRIVILEGED, VDIM_SYSCALL, VDIM_INTERRUPT

;; ============================================================
;; Instruction Encoding Constants
;; ============================================================

; Maximum instruction sequence length for encoding
%define INSTR_SEQ_MAX       32

; Hash combination for instruction encoding
; instruction_hash = opcat << 16 | (src_reg << 8) | dst_reg | (has_mem << 24)
%define INSTR_HASH_OPCAT_SHIFT  16
%define INSTR_HASH_SRC_SHIFT    8
%define INSTR_HASH_MEM_SHIFT    24

;; ============================================================
;; Vector Arithmetic Macros for Safety Checking
;; ============================================================

; Threshold for safety check (cosine similarity with safe code template)
%define SAFETY_THRESHOLD        0x3FD999999999999A  ; 0.4 f64

; Threshold for recognizing known-good patterns
%define PATTERN_THRESHOLD       0x3FE6666666666666  ; 0.7 f64

; Minimum similarity for fuzzy opcode matching
%define OPCODE_MATCH_THRESH     0x3FE0000000000000  ; 0.5 f64

%endif
