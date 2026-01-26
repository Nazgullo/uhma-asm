; constants.inc — UHMA system constants, offsets, limits
%ifndef CONSTANTS_INC
%define CONSTANTS_INC

;; ============================================================
;; Persistent Surface Configuration
;; File-backed memory - learning survives restarts
;; Uses sparse file - only touched pages consume disk
;;
;; ZONE ARCHITECTURE (200GB total):
;;   HOT  (0-2GB)    - Always in RAM: state, active dispatch, scratch
;;   WARM (2-16GB)   - Usually in RAM: recent patterns, vocab, traces
;;   COLD (16-200GB) - Paged on demand: archives, associative graph
;;
;; OS handles paging via mmap - we just organize for locality
;; ============================================================

%define SURFACE_MAGIC       0x414D4855_56534132  ; "UHMA" "VSA2" (version 2 layout)
%define SURFACE_VERSION     3                     ; increment on breaking layout changes

;; ============================================================
;; Surface Layout (file-backed, persistent)
;; ============================================================

%define SURFACE_BASE        0x100000000
%define SURFACE_SIZE        0x3200000000      ; 200GB max (sparse file)

; Zone boundaries (for madvise hints and organization)
%define ZONE_HOT_START      0x0
%define ZONE_HOT_SIZE       0x80000000        ; 2GB
%define ZONE_WARM_START     0x80000000        ; 2GB
%define ZONE_WARM_SIZE      0x380000000       ; 14GB (2GB-16GB)
%define ZONE_COLD_START     0x400000000       ; 16GB
%define ZONE_COLD_SIZE      0x2E00000000      ; 184GB (16GB-200GB)

;; ============================================================
;; HOT ZONE (0-2GB) - Always in RAM
;; State, active dispatch, current working memory
;; ============================================================

%define BOOTSTRAP_OFFSET    0x0
%define BOOTSTRAP_SIZE      0x10000           ; 64KB

; Surface header (at BOOTSTRAP_OFFSET, persisted)
%define SHDR_MAGIC          0                 ; u64 SURFACE_MAGIC
%define SHDR_VERSION        8                 ; u32 SURFACE_VERSION
%define SHDR_FLAGS          12                ; u32 flags (clean shutdown, etc.)
%define SHDR_TOTAL_STEPS    16                ; u64 total steps ever (survives restarts)
%define SHDR_CREATED        24                ; u64 unix timestamp of creation
%define SHDR_LAST_OPEN      32                ; u64 unix timestamp of last open
%define SHDR_SESSION_COUNT  40                ; u64 number of sessions
%define SHDR_ASSOC_COUNT    48                ; u64 associations in graph
%define SHDR_COLD_ALLOC     56                ; u64 next free offset in cold zone
%define SHDR_SIZE           64                ; header size

; Header flags
%define SHDR_FLAG_CLEAN     0x0001            ; was cleanly shut down
%define SHDR_FLAG_CORRUPT   0x0002            ; detected corruption

%define STATE_OFFSET        0x10000
%define STATE_SIZE          0x10000           ; 64KB - moved to hot zone!

%define DISPATCH_OFFSET     0x20000
%define DISPATCH_MAX_SIZE   0x10000000        ; 256MB

%define REGION_TABLE_OFFSET 0x10020000
%define REGION_TABLE_SIZE   0x100000          ; 1MB (32K entries × 32 bytes)
%define REGION_TABLE_MAX    32768             ; 32K regions now!

; Scratch arena for temporary VSA computations (hot)
%define SCRATCH_OFFSET      0x10120000
%define SCRATCH_SIZE        0x1000000         ; 16MB scratch space
%define SCRATCH_VEC_COUNT   1024              ; number of scratch vectors available

; Working memory ring buffer (hot) - candidates before commit
%define WORKBUF_OFFSET      0x11120000
%define WORKBUF_SIZE        0x2000000         ; 32MB working buffer
%define WORKBUF_ENTRY_SIZE  256               ; per-entry size
%define WORKBUF_MAX_ENTRIES 131072            ; 128K entries

;; ============================================================
;; WARM ZONE (2GB-16GB) - Usually in RAM
;; Recent patterns, vocabulary, holographic traces
;; ============================================================

%define VSA_OFFSET          0x80000000        ; 2GB - start of warm zone
%define VSA_SIZE            0x40000000        ; 1GB (128K tokens × 8KB each, f64)
%define VSA_MAX_TOKENS      131072            ; 128K vocabulary

; Holographic traces (warm) - receipts, predictions, resonance
%define HOLO_WARM_OFFSET    0xC0000000        ; 3GB
%define HOLO_WARM_SIZE      0x40000000        ; 1GB for active traces

; Confidence and metacognition vectors (warm)
%define META_OFFSET         0x100000000       ; 4GB
%define META_SIZE           0x10000000        ; 256MB metacognition data

;; ============================================================
;; COLD ZONE (16GB-200GB) - Paged on demand
;; All holographic memory - just lower temperature/access frequency
;; Same math, same vectors, just paged to disk when not accessed
;; ============================================================

; Extended holographic traces (cold) - overflow from warm zone
; When warm traces are full, new associations spill here
; Same format as warm traces, just lives on disk mostly
%define HOLO_COLD_OFFSET    0x400000000       ; 16GB - start of cold zone
%define HOLO_COLD_TRACES    8388608           ; 8M traces (massive expansion!)
%define HOLO_COLD_SIZE      0x1000000000      ; 64GB for cold traces

; Long-term vocabulary embeddings (cold) - full vectors for rare tokens
; Warm zone has compact entries, cold has full 1024-dim embeddings
%define VOCAB_COLD_OFFSET   0x1400000000      ; 80GB
%define VOCAB_COLD_SIZE     0x1000000000      ; 64GB extended vocab embeddings

; Episodic memory (cold) - timestamped experience traces
; bind(time_vec, context_vec, event_vec) for "what happened when"
%define EPISODIC_OFFSET     0x2400000000      ; 144GB
%define EPISODIC_SIZE       0x800000000       ; 32GB episodic memory

; Crystallized patterns (cold) - high-fidelity permanent memories
; Patterns that reached fidelity=1.0, never decay, always available
%define CRYSTAL_OFFSET      0x2C00000000      ; 176GB
%define CRYSTAL_SIZE        0x600000000       ; 24GB crystallized memories

;; ============================================================
;; Relation Types for Holographic Binding
;; Relationships encoded as: bind(subject_vec, relation_vec, object_vec)
;; Stored via superposition into traces, queried via resonance
;; NO separate graph structure - it's all holographic memory!
;; ============================================================

; Relation type seeds (used to generate deterministic relation vectors)
%define REL_FOLLOWS         0x464F4C4C         ; "FOLL" - A follows B in sequence
%define REL_CALLS           0x43414C4C         ; "CALL" - A calls B (function)
%define REL_CONTAINS        0x434F4E54         ; "CONT" - A contains B
%define REL_SIMILAR         0x53494D49         ; "SIMI" - A similar to B
%define REL_CAUSES          0x43415553         ; "CAUS" - A causes B
%define REL_PREDICTS        0x50524544         ; "PRED" - A predicts B
%define REL_INHIBITS        0x494E4849         ; "INHI" - A inhibits B
%define REL_EXCITES         0x45584349         ; "EXCI" - A excites B
%define REL_SCHEMA          0x53434845         ; "SCHE" - A instance of schema B
%define REL_TEMPORAL        0x54454D50         ; "TEMP" - A before B in time

; Number of relation types
%define REL_TYPE_COUNT      10

;; ============================================================
;; Region Header (128 bytes prefix — 2 cache lines)
;; Each region is a node in a learned connectivity graph.
;; ============================================================

; --- original fields ---
%define RHDR_HITS           0                 ; u32
%define RHDR_MISSES         4                 ; u32
%define RHDR_BIRTH          8                 ; u32
%define RHDR_CODE_LEN       12                ; u16
%define RHDR_FLAGS          14                ; u16
; --- connection addresses (direct 64-bit pointers into surface) ---
%define RHDR_NEXT_A         16                ; u64 ptr — follow on miss (learned routing)
%define RHDR_NEXT_B         24                ; u64 ptr — alternate miss route
%define RHDR_EXCITE_A       32                ; u64 ptr — prime this region on hit
%define RHDR_EXCITE_B       40                ; u64 ptr — alternate excitation target
%define RHDR_INHIBIT_A      48                ; u64 ptr — suppress this region on hit
%define RHDR_INHIBIT_B      56                ; u64 ptr — alternate inhibition target
; --- connection weights (f64 for smooth Hebbian dynamics) ---
%define RHDR_W_EXCITE_A     64                ; f64 excitation strength [0.0, 1.0]
%define RHDR_W_EXCITE_B     72                ; f64 excitation strength
%define RHDR_W_INHIBIT_A    80                ; f64 inhibition strength [0.0, 1.0]
%define RHDR_W_INHIBIT_B    88                ; f64 inhibition strength
; --- continuous dynamics state ---
%define RHDR_PRIME          96                ; f64 accumulated excitation from neighbors
%define RHDR_FIRE_RECENCY   104               ; f64 timestamp of last fire
%define RHDR_ACTIVATION     112               ; f64 current activation level
%define RHDR_RESONANCE      120               ; f64 running average of co-fire correlation

%define RHDR_SIZE           128

; Region flags
%define RFLAG_ACTIVE        0x0001
%define RFLAG_FROZEN        0x0002
%define RFLAG_NURSERY       0x0004
%define RFLAG_CONDEMNED     0x0008
%define RFLAG_NEEDS_REPAIR  0x0010  ; Self-surprise: region violated self-model, needs introspection

;; ============================================================
;; Region Table Entry (32 bytes)
;; ============================================================

%define RTE_ADDR            0                 ; u64
%define RTE_LEN             8                 ; u32
%define RTE_TYPE            12                ; u16
%define RTE_FLAGS           14                ; u16
%define RTE_HITS            16                ; u32
%define RTE_MISSES          20                ; u32
%define RTE_BIRTH           24                ; u32
%define RTE_CONSEC_ERRORS   28                ; u16 consecutive errors (9 = death)
%define RTE_ERROR_PATTERN   30                ; u16 semantic pattern that failed
%define RTE_SIZE            32

; 9-error kill threshold (broken regions die fast)
%define CONSEC_ERROR_KILL   9

; Region types
%define RTYPE_DISPATCH      0
%define RTYPE_VSA_OP        1
%define RTYPE_MODIFIER      2
%define RTYPE_OBSERVER      3
%define RTYPE_EMITTER       4
%define RTYPE_HOOK          5
%define RTYPE_GATE          6
%define RTYPE_DREAM         7
%define RTYPE_RESONANT      8   ; Fuzzy-matching regions (vector similarity dispatch)
%define RTYPE_SUBROUTINE    9   ; Callable code fragment (recursive schema hierarchy)
%define RTYPE_PRESENCE      10  ; Hormonal modulator (checks state, modifies system params)
%define RTYPE_WORKER        11  ; Specialized hive worker caste (responds to pheromone signals)
%define RTYPE_INGESTED      12  ; High-value schema learned from ingested code (structural absorption)
%define RTYPE_SCHEMA        13  ; Structural schema with variable binding (Phase 3)

;; ============================================================
;; Mycorrhiza: Shared Consciousness Between Instances
;; The VSA arena and holographic traces are shared between
;; processes via MAP_SHARED. The dispatch graph stays private
;; to prevent crash propagation. This enables "telepathy" —
;; pain felt by one node is felt by all.
;; ============================================================

%define SHARED_VSA_NAME     "/dev/shm/uhma_vsa"   ; path for shared VSA segment
%define SHARED_HOLO_NAME    "/dev/shm/uhma_holo"  ; path for shared holographic traces

; Shared region sizes
%define SHARED_VSA_SIZE     VSA_SIZE              ; 512MB shared VSA arena
%define SHARED_HOLO_SIZE    HOLO_TOTAL            ; 2MB shared holographic traces

; Instance identification (for collective intelligence)
%define ST_INSTANCE_ID      (ST_EVOLVE_PRESSURE + 8)      ; u64 unique instance ID
%define ST_COLONY_SIZE      (ST_INSTANCE_ID + 8)          ; u32 known instances in colony
%define ST_SHARED_MODE      (ST_COLONY_SIZE + 4)          ; u32 0=solo, 1=shared

;; ============================================================
;; Somatic Binding: Mix Presence into Context
;; The dispatcher will salt context hash with mood signature
;; enabling state-dependent memory recall
;; ============================================================

%define SOMATIC_AROUSAL_BITS    4   ; bits from arousal quantization (16 levels)
%define SOMATIC_VALENCE_BITS    4   ; bits from valence quantization (16 levels)
%define SOMATIC_SHIFT           24  ; where mood signature sits in hash (high bits)

;; ============================================================
;; Presence Region: Hormonal Modulator Structure
;; Checks state conditions, triggers system-wide parameter changes
;; Code layout: [condition_check][parameter_writes][RET]
;; ============================================================

%define PRES_CONDITION_MAX      4   ; max conditions per presence region
%define PRES_EFFECT_MAX         4   ; max parameter effects per region

; Presence condition opcodes (what to check)
%define PCOND_ENTROPY_GT        0   ; if entropy > threshold
%define PCOND_ENTROPY_LT        1   ; if entropy < threshold
%define PCOND_ENERGY_GT         2   ; if energy > threshold
%define PCOND_ENERGY_LT         3   ; if energy < threshold
%define PCOND_AROUSAL_GT        4   ; if arousal > threshold
%define PCOND_AROUSAL_LT        5   ; if arousal < threshold
%define PCOND_VALENCE_GT        6   ; if valence > threshold
%define PCOND_VALENCE_LT        7   ; if valence < threshold
%define PCOND_ACCURACY_LT       8   ; if accuracy < threshold (struggling)
%define PCOND_FATIGUE_GT        9   ; if fatigue > threshold

; Presence effect opcodes (what to modify)
%define PEFF_SET_DISPATCH_MODE  0   ; set ST_DISPATCH_MODE
%define PEFF_SET_PRUNE_THRESH   1   ; set prune accuracy threshold
%define PEFF_SET_ENERGY_RATE    2   ; set energy consumption rate
%define PEFF_TRIGGER_DREAM      3   ; force dream cycle
%define PEFF_TRIGGER_OBSERVE    4   ; force observation cycle
%define PEFF_BOOST_NOVELTY      5   ; increase novelty drive weight

;; ============================================================
;; Hive Worker Castes: Pheromone-Driven Swarm Intelligence
;; Each worker caste monitors a pheromone channel and performs
;; specialized work when the level exceeds threshold. This
;; enables decentralized self-regulation like a bee hive.
;; ============================================================

; Pheromone channels (chemical signals in hive)
%define PHERO_DREAM         0   ; ST_DREAM_PRESSURE - consolidation pheromone
%define PHERO_OBSERVE       1   ; ST_OBSERVE_PRESSURE - observation pheromone
%define PHERO_EVOLVE        2   ; ST_EVOLVE_PRESSURE - evolution pheromone
%define PHERO_FATIGUE       3   ; ST_PRES_FATIGUE - exhaustion signal
%define PHERO_ENTROPY       4   ; ST_PRES_ENTROPY - chaos signal
%define PHERO_MISS_RATE     5   ; computed from recent misses

; Worker caste actions (what each caste does when activated)
%define CASTE_DREAM         0   ; trigger dream_cycle (consolidation worker)
%define CASTE_OBSERVE       1   ; trigger observe_cycle (observation worker)
%define CASTE_EVOLVE        2   ; trigger evolve_cycle (evolution worker)
%define CASTE_COMPACT       3   ; trigger region compaction (cleanup worker)
%define CASTE_DIGEST        4   ; trigger digestion (forager worker)
%define CASTE_REST          5   ; reduce activity (conservation worker)

; Pheromone thresholds (defaults, can be modified by presence regions)
%define PHEROMONE_THRESHOLD 0x3FE0000000000000  ; 0.5 f64

; Legacy aliases for compatibility
%define RPRES_DREAM         PHERO_DREAM
%define RPRES_OBSERVE       PHERO_OBSERVE
%define RPRES_EVOLVE        PHERO_EVOLVE
%define RACT_DREAM_CYCLE    CASTE_DREAM
%define RACT_OBSERVE_CYCLE  CASTE_OBSERVE
%define RACT_EVOLVE_CYCLE   CASTE_EVOLVE
%define PRESSURE_THRESHOLD  PHEROMONE_THRESHOLD

;; ============================================================
;; Logic Probe: Instruction Classification for Abstract Interpretation
;; Used by verify.asm to symbolically execute code and prove safety theorems
;; ============================================================

; Opcode Classes (returned by decode_instruction_full)
%define OP_NOP          0   ; NOP, padding, no-op
%define OP_STACK_PUSH   1   ; PUSH (increases stack depth)
%define OP_STACK_POP    2   ; POP (decreases stack depth)
%define OP_STACK_ADJ    3   ; SUB RSP / ADD RSP (adjusts stack by immediate)
%define OP_FLOW_JUMP    4   ; JMP (unconditional transfer)
%define OP_FLOW_JCC     5   ; Jcc (conditional transfer)
%define OP_FLOW_CALL    6   ; CALL (push + jump)
%define OP_FLOW_RET     7   ; RET (pop + jump to return address)
%define OP_WRITE_REG    8   ; MOV to register
%define OP_WRITE_MEM    9   ; MOV to memory
%define OP_READ_MEM     10  ; MOV from memory
%define OP_ARITH        11  ; ADD, SUB, AND, OR, XOR, CMP, TEST
%define OP_SYSCALL      12  ; SYSCALL (forbidden without broker)
%define OP_INTERRUPT    13  ; INT, INT3 (forbidden)
%define OP_PRIVILEGED   14  ; HLT, CLI, STI, etc. (forbidden)
%define OP_UNKNOWN      15  ; Unknown/unclassified instruction

; Register IDs for tracking callee-saved preservation
%define REG_RAX         0
%define REG_RCX         1
%define REG_RDX         2
%define REG_RBX         3   ; callee-saved
%define REG_RSP         4   ; special (stack)
%define REG_RBP         5   ; callee-saved
%define REG_RSI         6
%define REG_RDI         7
%define REG_R8          8
%define REG_R9          9
%define REG_R10         10
%define REG_R11         11
%define REG_R12         12  ; callee-saved
%define REG_R13         13  ; callee-saved
%define REG_R14         14  ; callee-saved
%define REG_R15         15  ; callee-saved

; Callee-saved register bitmask (must be preserved across calls)
%define CALLEE_SAVED_MASK   0xF068  ; RBX=3, RBP=5, R12-R15=12-15

; DecodedInstruction structure (32 bytes, returned by decode_instruction_full)
%define DI_LENGTH       0   ; u8 instruction length
%define DI_OP_CLASS     1   ; u8 opcode class (OP_*)
%define DI_DST_REG      2   ; u8 destination register (REG_* or 0xFF if none)
%define DI_SRC_REG      3   ; u8 source register (REG_* or 0xFF if none)
%define DI_MODRM        4   ; u8 ModR/M byte (if present)
%define DI_FLAGS        5   ; u8 flags (has_imm, has_disp, etc.)
%define DI_RESERVED     6   ; u16 padding
%define DI_IMM64        8   ; u64 immediate value (sign-extended)
%define DI_DISP32       16  ; u32 displacement
%define DI_TARGET       20  ; u32 computed jump/call target offset
%define DI_SIZE         24  ; total structure size (padded)

; DI_FLAGS bits
%define DIF_HAS_IMM     0x01  ; instruction has immediate operand
%define DIF_HAS_DISP    0x02  ; instruction has displacement
%define DIF_HAS_MODRM   0x04  ; instruction has ModR/M byte
%define DIF_IS_REX      0x08  ; instruction has REX prefix
%define DIF_IS_64BIT    0x10  ; 64-bit operand size
%define DIF_WRITES_FLAGS 0x20 ; instruction modifies EFLAGS

; VirtualCpuState structure for abstract interpretation (64 bytes)
%define VCS_STACK_DEPTH     0   ; i32 current stack depth (pushes - pops)
%define VCS_REG_MODIFIED    4   ; u16 bitmask of modified registers
%define VCS_REG_SAVED       6   ; u16 bitmask of saved registers (pushed)
%define VCS_REG_RESTORED    8   ; u16 bitmask of restored registers (popped)
%define VCS_FLAGS           10  ; u16 state flags
%define VCS_MAX_DEPTH       12  ; i32 max stack depth seen
%define VCS_MIN_DEPTH       16  ; i32 min stack depth seen
%define VCS_BRANCH_TARGETS  20  ; u32 count of branch targets to verify
%define VCS_ERRORS          24  ; u32 verification error count
%define VCS_WARNINGS        28  ; u32 warning count
%define VCS_LAST_OP         32  ; u8 last opcode class
%define VCS_RESERVED        33  ; padding
%define VCS_SIZE            64  ; total structure size

; VCS_FLAGS bits
%define VCSF_SAW_RET        0x01  ; saw a RET instruction
%define VCSF_SAW_SYSCALL    0x02  ; saw a SYSCALL (forbidden)
%define VCSF_SAW_INT        0x04  ; saw an INT instruction
%define VCSF_STACK_UNDERFLOW 0x08 ; stack went negative
%define VCSF_INVALID_JUMP   0x10  ; jump to invalid target
%define VCSF_CALLEE_CLOBBER 0x20  ; callee-saved register clobbered without save

; Motor Interface (for Transactional Agency)
%define ST_MOTOR_CMD    (ST_COUNTERFACT_TOTAL + 200)  ; u32 pending motor command
%define ST_MOTOR_ARG1   (ST_MOTOR_CMD + 4)            ; u64 first argument
%define ST_MOTOR_ARG2   (ST_MOTOR_ARG1 + 8)           ; u64 second argument
%define ST_MOTOR_RESULT (ST_MOTOR_ARG2 + 8)           ; u64 result
%define ST_MOTOR_STATUS (ST_MOTOR_RESULT + 8)         ; u32 status

; Motor commands
%define MOTOR_NONE          0   ; no pending command
%define MOTOR_FILE_READ     1   ; read file to buffer
%define MOTOR_FILE_WRITE    2   ; write buffer to file (sandboxed)
%define MOTOR_FILE_SIZE     3   ; get file size
%define MOTOR_EXEC          4   ; execute external program (sandboxed)

;; ============================================================
;; Subroutine Table: Track shared callable regions
;; Each entry: (subroutine_ptr:u64, caller_count:u32, code_hash:u32) = 16 bytes
;; Enables code reuse through factored common suffixes
;; ============================================================

%define SUBROUTINE_TABLE_MAX    64    ; max concurrent subroutines
%define SUBROUTINE_ENTRY_SIZE   16    ; bytes per entry

; Subroutine entry structure offsets:
%define STE_PTR             0         ; u64 pointer to subroutine region header
%define STE_CALLER_COUNT    8         ; u32 number of regions calling this subroutine
%define STE_CODE_HASH       12        ; u32 hash of the code bytes (for dedup)

; Suffix factoring constants
%define SUFFIX_MIN_LEN      5         ; minimum suffix bytes worth factoring
%define SUFFIX_MIN_CALLERS  3         ; minimum callers to justify creating subroutine
%define FACTOR_SCAN_LIMIT   64        ; max regions to compare per factoring pass

;; ============================================================
;; VSA Configuration
;; ============================================================

%define VSA_DIM             1024
%define VSA_VEC_BYTES       (VSA_DIM * 8)     ; 8192 bytes per vector (f64)
%define VSA_MAX_TOKENS      65536

;; ============================================================
;; Holographic Associative Memory Layout (f64 precision)
;; Now uses zone-based layout:
;;   HOLO_OFFSET (warm) = active traces in RAM
;;   HOLO_COLD_OFFSET   = archived traces on disk
;; ============================================================

%define HOLO_OFFSET         HOLO_WARM_OFFSET  ; alias for compatibility
%define HOLO_TRACES         131072            ; 128K traces (expanded from 4096!)
%define HOLO_DIM            1024              ; elements per trace vector
%define HOLO_VEC_BYTES      (HOLO_DIM * 8)    ; 8192 bytes per trace (1024 x f64)
%define HOLO_TOTAL          (HOLO_TRACES * HOLO_VEC_BYTES)  ; 1GB in warm zone

; Vocabulary now in warm zone after VSA embeddings
%define VOCAB_OFFSET        (VSA_OFFSET + VSA_SIZE)  ; after 1GB VSA = 3GB
%define VOCAB_ENTRY_SIZE    8                 ; (u32 token_id, u32 count)
%define VOCAB_MAX_SCAN      1024              ; top-N tokens to scan (increased)
%define VOCAB_SIZE          0x800000          ; 8MB vocab table (1M entries)

;; ============================================================
;; Topological Metacognition: Per-Context Confidence Vector
;; A 1024-dim f64 vector that accumulates confidence/anxiety
;; per context type. Hit → positive superposition, Miss → negative.
;; Query via dot product to feel anxious or confident about topics.
;; Now in META region (warm zone).
;; ============================================================

%define CONFIDENCE_VEC_OFFSET META_OFFSET     ; in warm zone metacognition area
%define CONFIDENCE_VEC_BYTES  HOLO_VEC_BYTES  ; 8192 bytes (1024 x f64)
%define CONFIDENCE_HIT_WEIGHT  0x3FB999999999999A       ; +0.1 f64
%define CONFIDENCE_MISS_WEIGHT 0xBFB999999999999A       ; -0.1 f64
%define CONFIDENCE_DECAY       0x3FEFFAE147AE147B       ; 0.999 f64 (slow decay)
%define CONFIDENCE_ANXIOUS_THRESH 0xBFD0000000000000    ; -0.25 f64 (below = anxious)
%define CONFIDENCE_CONFIDENT_THRESH 0x3FD0000000000000  ; +0.25 f64 (above = confident)

;; Metacognitive feeling enum (returned by confidence_get_feeling)
%define FEELING_NEUTRAL   0   ; no strong feeling about this context
%define FEELING_CONFIDENT 1   ; "I'm good at this" → fast dispatch
%define FEELING_ANXIOUS   2   ; "I struggle with this" → deliberate dispatch

;; ============================================================
;; Somatic Grounding: Valence Channel in VSA Vectors
;; The last element of every 1024-dim vector encodes valence
;; (emotional charge). Patterns learned during high-energy states
;; get positive valence; those during low-energy get negative.
;; This creates "felt sense" of memories — not just associations
;; but emotional connotations carried by the pattern itself.
;; ============================================================

%define VSA_VALENCE_INDEX       1023              ; last element is valence channel
%define VSA_VALENCE_OFFSET      (VSA_VALENCE_INDEX * 8)  ; 8184 bytes from start

; Valence encoding constants
%define VALENCE_MIN             -1.0
%define VALENCE_MAX             1.0
%define VALENCE_NEUTRAL         0.0
%define VALENCE_ENERGY_SCALE    0.01  ; energy_delta * this = valence contribution

; Energy-to-valence mapping thresholds
%define ENERGY_POSITIVE_THRESH  10.0  ; energy gained → positive valence
%define ENERGY_NEGATIVE_THRESH  -10.0 ; energy lost → negative valence

;; ============================================================
;; Self-Debugger: Dynamic Breakpoint Injection
;; Regions that are "struggling" (high miss rate but high investment)
;; get INT 3 breakpoints injected for introspection.
;; ============================================================

%define BREAKPOINT_TABLE_OFFSET (META_OFFSET + 0x100000)  ; 1MB into metacognition area
%define BREAKPOINT_ENTRY_SIZE   32    ; per-breakpoint entry
%define BREAKPOINT_MAX          64    ; max concurrent breakpoints
%define BREAKPOINT_TABLE_SIZE   (BREAKPOINT_ENTRY_SIZE * BREAKPOINT_MAX)

; Breakpoint entry structure:
;   +0:  region_ptr (u64) - pointer to region header
;   +8:  code_offset (u32) - offset into region code where INT3 was placed
;   +12: original_byte (u8) - byte that was replaced
;   +13: bp_type (u8) - BPTYPE_* enum
;   +14: trigger_count (u16) - how many times this breakpoint fired
;   +16: expected_rax (u32) - expected value in RAX at breakpoint
;   +20: expected_ctx (u32) - expected context hash
;   +24: last_rax (u32) - actual RAX value when last triggered
;   +28: flags (u32) - BPFLAG_* flags

%define BPE_REGION_PTR    0
%define BPE_CODE_OFFSET   8
%define BPE_ORIG_BYTE     12
%define BPE_TYPE          13
%define BPE_TRIGGER_COUNT 14
%define BPE_EXPECTED_RAX  16
%define BPE_EXPECTED_CTX  20
%define BPE_LAST_RAX      24
%define BPE_FLAGS         28

; Breakpoint types
%define BPTYPE_ENTRY      0   ; at region entry (before CMP)
%define BPTYPE_DECISION   1   ; at branch point (after CMP, before JE)
%define BPTYPE_RETURN     2   ; before RET (to see prediction value)
%define BPTYPE_STRUGGLING 3   ; injected due to high miss rate

; Breakpoint flags
%define BPFLAG_ACTIVE     0x0001
%define BPFLAG_PERMANENT  0x0002  ; don't auto-remove
%define BPFLAG_LEARNING   0x0004  ; currently learning from this bp

; State fields for self-debugger
%define ST_BREAKPOINT_COUNT (ST_BASE_STUCK + 8)   ; u32 active breakpoints
%define ST_BP_TOTAL_HITS    (ST_BREAKPOINT_COUNT + 4)  ; u32 total breakpoint triggers
%define ST_BP_LEARNING_EVENTS (ST_BP_TOTAL_HITS + 4)   ; u32 learning events generated

;; ============================================================
;; Gene Pool: Composting via Genetic Recombination
;; Before condemned regions are wiped, extract useful patterns
;; into a gene pool for later reuse during evolution.
;; ============================================================

%define GENE_POOL_OFFSET        (BREAKPOINT_TABLE_OFFSET + BREAKPOINT_TABLE_SIZE)
%define GENE_ENTRY_SIZE         32    ; per-gene entry
%define GENE_MAX                256   ; max genes in pool
%define GENE_POOL_SIZE          (GENE_ENTRY_SIZE * GENE_MAX)

; Gene entry structure (32 bytes):
;   +0:  ctx_hash (u32) - context pattern this gene recognizes
;   +4:  token_id (u32) - token this gene predicts
;   +8:  fitness (f32) - accuracy when this gene was active
;   +12: hits (u32) - number of successful predictions
;   +16: misses (u32) - number of failed predictions
;   +20: birth_step (u32) - when this gene was extracted
;   +24: flags (u32) - gene flags
;   +28: reserved (u32) - padding for alignment

%define GENE_CTX_HASH           0
%define GENE_TOKEN_ID           4
%define GENE_FITNESS            8
%define GENE_HITS               12
%define GENE_MISSES             16
%define GENE_BIRTH_STEP         20
%define GENE_FLAGS              24
%define GENE_RESERVED           28

; Gene flags
%define GFLAG_ACTIVE            0x0001  ; slot in use
%define GFLAG_SCHEMA            0x0002  ; generalized pattern (masked ctx)
%define GFLAG_HIGH_FITNESS      0x0004  ; above 0.5 fitness
%define GFLAG_PROVEN            0x0008  ; used successfully after extraction

;; ============================================================
;; State Block Offsets (from state_base)
;; ============================================================

; Token ring buffer: 64 u32 entries
%define ST_TOKEN_BUF        0
%define ST_TOKEN_BUF_CAP    64
%define ST_TOKEN_BUF_END    (ST_TOKEN_BUF_CAP * 4)   ; 256 bytes
%define ST_TOKEN_POS        256               ; u32 index
%define ST_TOKEN_COUNT      260               ; u32 total

; Context
%define ST_CTX_HASH         264               ; u64 rolling hash
%define ST_GLOBAL_STEP      272               ; u64

; Drives: 4 x f32
%define ST_DRIVES           280
%define ST_DRIVE_ACCURACY   280
%define ST_DRIVE_EFFICIENCY 284
%define ST_DRIVE_NOVELTY    288
%define ST_DRIVE_COHERENCE  292

; Presence: 30 x f32
%define ST_PRESENCE         296

; Hook table: 22 hooks, each: [count:u16][pad:u16][pad:u32] + [ptrs: 32 x u64]
%define HOOK_ENTRY_SIZE     264               ; 8 + 32*8
%define HOOK_MAX_HANDLERS   32
%define NUM_HOOKS           22
%define ST_HOOKS            416

; Dispatch allocator
%define ST_DISPATCH_PTR     (ST_HOOKS + NUM_HOOKS * HOOK_ENTRY_SIZE)
%define ST_REGION_COUNT     (ST_DISPATCH_PTR + 8)

; Miss buffer: ring of (ctx_hash:u64, token_id:u32, pad:u32) = 16 bytes each
%define ST_MISS_BUF         (ST_REGION_COUNT + 8)
%define ST_MISS_ENTRY_SIZE  16
%define ST_MISS_BUF_CAP     256
%define ST_MISS_POS         (ST_MISS_BUF + ST_MISS_BUF_CAP * ST_MISS_ENTRY_SIZE)

; Episode ring: (step:u64, ctx_hash:u64) = 16 bytes each
%define ST_EPISODE_RING     (ST_MISS_POS + 8)
%define ST_EPISODE_ENTRY    16
%define ST_EPISODE_CAP      64
%define ST_EPISODE_POS      (ST_EPISODE_RING + ST_EPISODE_CAP * ST_EPISODE_ENTRY)

; Modification log: (step:u64, addr:u64, type:u32, result:u32) = 24 bytes each
%define ST_MOD_LOG          (ST_EPISODE_POS + 8)
%define ST_MOD_ENTRY_SIZE   24
%define ST_MOD_LOG_CAP      128
%define ST_MOD_LOG_POS      (ST_MOD_LOG + ST_MOD_LOG_CAP * ST_MOD_ENTRY_SIZE)

; Nursery buffer (for predictive gate testing)
%define ST_NURSERY          (ST_MOD_LOG_POS + 8)
%define ST_NURSERY_SIZE     4096

; Last prediction (for hit/miss tracking)
%define ST_LAST_PREDICT     (ST_NURSERY + ST_NURSERY_SIZE)  ; u32
%define ST_LAST_CTX         (ST_LAST_PREDICT + 4)           ; u64
%define ST_PREDICT_REGION   (ST_LAST_CTX + 8)               ; u64 ptr to region that made prediction
%define ST_LAST_VALENCE     (ST_PREDICT_REGION + 8)         ; f64 valence of last prediction context

; Drive thresholds (modifiable)
%define ST_DRIVE_THRESH     (ST_LAST_VALENCE + 8)           ; 4 x f32

; Observation config
%define ST_OBS_INTERVAL     (ST_DRIVE_THRESH + 16)          ; u32
%define ST_OBS_LAST_STEP    (ST_OBS_INTERVAL + 4)           ; u64

; Factor triggering - incremented when suffix resonance detected
%define ST_FACTOR_PENDING   (ST_OBS_LAST_STEP + 8)          ; u32 count of suffix matches

; Introspective state (computed from metrics each observation)
%define ST_INTRO_STATE      (ST_FACTOR_PENDING + 4)         ; u32 enum
%define ST_SURPRISE_TYPE    (ST_INTRO_STATE + 4)            ; u32 (per-step)
%define ST_META_FEELING     (ST_SURPRISE_TYPE + 4)          ; u32 FEELING_* enum (per-context feeling)
%define ST_META_CONFIDENCE  (ST_META_FEELING + 4)           ; f64 raw confidence score for current context

; Self-prediction (meta-prediction: which region will fire next)
%define ST_SELF_PRED_REGION (ST_META_CONFIDENCE + 8)        ; u64 ptr
%define ST_SELF_PRED_HITS   (ST_SELF_PRED_REGION + 8)       ; u32
%define ST_SELF_PRED_MISSES (ST_SELF_PRED_HITS + 4)         ; u32

; Schema tracking
%define ST_SCHEMA_HITS      (ST_SELF_PRED_MISSES + 4)       ; u32 tokens matching generalized patterns
%define ST_SCHEMA_TOTAL     (ST_SCHEMA_HITS + 4)            ; u32 total tokens

; Dispatch mode (selected by drive system)
%define ST_DISPATCH_MODE    (ST_SCHEMA_TOTAL + 4)           ; u32

; Causal record: last modification and its effect
%define ST_CAUSAL_MOD_ADDR  (ST_DISPATCH_MODE + 4)          ; u64 what was modified
%define ST_CAUSAL_PRE_ACC   (ST_CAUSAL_MOD_ADDR + 8)        ; f32 accuracy before
%define ST_CAUSAL_POST_ACC  (ST_CAUSAL_PRE_ACC + 4)         ; f32 accuracy after
%define ST_CAUSAL_COUNT     (ST_CAUSAL_POST_ACC + 4)        ; u32 total causal records

; Recent emission count (for LEARNING state detection)
%define ST_RECENT_EMITS     (ST_CAUSAL_COUNT + 4)           ; u32

; Self-expectation bundle (filled by dispatch_predict)
%define ST_EXPECT_REGION    (ST_RECENT_EMITS + 4)           ; u64 ptr to predicted region
%define ST_EXPECT_CONF      (ST_EXPECT_REGION + 8)          ; f32 region's accuracy
%define ST_EXPECT_TOKEN     (ST_EXPECT_CONF + 4)            ; u32 predicted token
%define ST_EXPECT_IS_SCHEMA (ST_EXPECT_TOKEN + 4)           ; u32 1 if generalized pattern

; Explicit goal (set by drives, executed by observe)
%define ST_CURRENT_GOAL     (ST_EXPECT_IS_SCHEMA + 4)       ; u32 enum
%define ST_GOAL_STEP        (ST_CURRENT_GOAL + 4)           ; u32 when goal was set

; Accuracy variance (computed in observe)
%define ST_ACCURACY_VARIANCE (ST_GOAL_STEP + 4)             ; f32

; Dispatch trace (last dispatch_predict scan)
%define ST_TRACE_CANDIDATES (ST_ACCURACY_VARIANCE + 4)      ; u32 regions considered
%define ST_TRACE_MATCHED    (ST_TRACE_CANDIDATES + 4)       ; u32 regions that matched

; Region successor table: for each region idx, which idx follows it
; 256 entries × 2 bytes = 512 bytes
%define ST_SUCCESSOR_TBL    (ST_TRACE_MATCHED + 4)          ; u16[256]
%define ST_LAST_FIRED_IDX   (ST_SUCCESSOR_TBL + 512)        ; u16 last fired region index

; Dream auto-trigger threshold
%define ST_DREAM_MISS_THRESH (ST_LAST_FIRED_IDX + 4)        ; u32 (default: 128)

; Previous introspective state (for episode detection)
%define ST_PREV_INTRO_STATE (ST_DREAM_MISS_THRESH + 4)      ; u32
%define ST_PREV_DISPATCH_MODE (ST_PREV_INTRO_STATE + 4)     ; u32

; Recent condemnation count (for presence decay field)
%define ST_RECENT_CONDEMNS  (ST_PREV_DISPATCH_MODE + 4)     ; u32

;; ============================================================
;; Graph Dynamics State Block (fire ring, entry table)
;; ============================================================

; Fire ring buffer: 64 entries × (u64 region_ptr + f64 timestamp) = 16 bytes each
%define ST_FIRE_RING        (ST_RECENT_CONDEMNS + 4)
%define ST_FIRE_RING_CAP    64
%define ST_FIRE_RING_ENTRY  16                ; 8 bytes ptr + 8 bytes f64 timestamp
%define ST_FIRE_RING_END    (ST_FIRE_RING + ST_FIRE_RING_CAP * ST_FIRE_RING_ENTRY)

; Fire ring write position and total count
%define ST_FIRE_POS         ST_FIRE_RING_END                ; u64
%define ST_FIRE_COUNT       (ST_FIRE_POS + 8)               ; u64

; Entry table: 16 context-hashed entry points into the graph
%define ST_ENTRY_TABLE      (ST_FIRE_COUNT + 8)             ; 16 × u64 = 128 bytes
%define ST_ENTRY_TABLE_CAP  16
%define ST_ENTRY_TABLE_END  (ST_ENTRY_TABLE + ST_ENTRY_TABLE_CAP * 8)

; Graph dispatch trace metrics
%define ST_GRAPH_DEPTH      (ST_ENTRY_TABLE_END)            ; u32 last traversal depth
%define ST_GRAPH_VISITED    (ST_GRAPH_DEPTH + 4)            ; u32 nodes visited in last dispatch

; Holographic memory state
%define ST_VOCAB_COUNT      (ST_GRAPH_VISITED + 4)          ; u32 vocabulary entries
%define ST_VOCAB_TOP_DIRTY  (ST_VOCAB_COUNT + 4)            ; u32 flag: recache top-256
%define ST_HOLO_PREDICT_SUM (ST_VOCAB_TOP_DIRTY + 4)        ; f64 running sum of confidences
%define ST_HOLO_PREDICT_N   (ST_HOLO_PREDICT_SUM + 8)       ; u32 count of predictions

;; ============================================================
;; Organic Dynamics State (anticipation, pressure, oscillation)
;; ============================================================

; Anticipatory buffer: 16 sub-threshold signals accumulating
; Each entry: (token_id:u32, accumulated_conf:f64, last_step:u32, pad:u32) = 24 bytes
%define ST_ANTIC_BUF        (ST_HOLO_PREDICT_N + 4)
%define ST_ANTIC_ENTRY_SIZE 24
%define ST_ANTIC_CAP        16
%define ST_ANTIC_BUF_END    (ST_ANTIC_BUF + ST_ANTIC_CAP * ST_ANTIC_ENTRY_SIZE)

; Anticipatory signal tracking
%define ST_ANTIC_FIRED      ST_ANTIC_BUF_END              ; u32 count of materialized signals
%define ST_ANTIC_ACTIVE     (ST_ANTIC_FIRED + 4)          ; u32 currently accumulating count

; Organic trigger state — internal pressure drives action
%define ST_MISS_PRESSURE    (ST_ANTIC_ACTIVE + 4)         ; f64 exponential moving avg of miss rate
%define ST_DREAM_PRESSURE   (ST_MISS_PRESSURE + 8)        ; f64 accumulates when miss_pressure high
%define ST_OBSERVE_PRESSURE (ST_DREAM_PRESSURE + 8)       ; f64 accumulates from accuracy variance
%define ST_EVOLVE_PRESSURE  (ST_OBSERVE_PRESSURE + 8)     ; f64 accumulates from stagnation
%define ST_INTROSPECT_PRESSURE (ST_EVOLVE_PRESSURE + 8)   ; f64 accumulates from SURPRISE_SELF (self-model violations)

; Oscillation monitoring (flat = dead)
%define ST_PREV_ACCURACY    (ST_INTROSPECT_PRESSURE + 8)  ; f64 accuracy N-1 steps ago
%define ST_PREV_PREV_ACC    (ST_PREV_ACCURACY + 8)        ; f64 accuracy N-2 steps ago
%define ST_OSCILLATION_AMP  (ST_PREV_PREV_ACC + 8)        ; f64 current oscillation amplitude
%define ST_FLATNESS_COUNT   (ST_OSCILLATION_AMP + 8)      ; u32 consecutive flat steps

; Region introspection cache: decoded semantics for top-16 regions
; Each: (region_ptr:u64, ctx_hash:u32, pred_token:u32, accuracy:f32,
;         semantic_sig:u32, neighbor_count:u16, depth:u16) = 32 bytes
%define ST_INTRO_CACHE      (ST_FLATNESS_COUNT + 4)
%define ST_INTRO_ENTRY_SIZE 32
%define ST_INTRO_CACHE_CAP  16
%define ST_INTRO_CACHE_END  (ST_INTRO_CACHE + ST_INTRO_CACHE_CAP * ST_INTRO_ENTRY_SIZE)

; Organic action counters (how many times each was triggered by pressure, not command)
%define ST_ORGANIC_DREAMS   ST_INTRO_CACHE_END            ; u32
%define ST_ORGANIC_OBSERVES (ST_ORGANIC_DREAMS + 4)       ; u32
%define ST_ORGANIC_EVOLVES  (ST_ORGANIC_OBSERVES + 4)     ; u32
%define ST_ORGANIC_EXPLORES (ST_ORGANIC_EVOLVES + 4)      ; u32

; Presence-driven dispatch modulation
%define ST_PRES_DISPATCH_BIAS (ST_ORGANIC_EXPLORES + 4)   ; f32 presence influence on mode
%define ST_PRES_RISK_APPETITE (ST_PRES_DISPATCH_BIAS + 4) ; f32 from arousal/confidence

;; ============================================================
;; Metabolic System (self-consumption, energy, cost)
;; ============================================================

; Energy pool — operations cost energy, hits earn energy, condemned regions are consumed
%define ST_ENERGY           (ST_PRES_RISK_APPETITE + 4)   ; f64 current energy [0, ENERGY_MAX]
%define ST_ENERGY_INCOME    (ST_ENERGY + 8)               ; f64 total energy earned (hits)
%define ST_ENERGY_SPENT     (ST_ENERGY_INCOME + 8)        ; f64 total energy spent (operations)
%define ST_METABOLIZED_COUNT (ST_ENERGY_SPENT + 8)        ; u32 regions consumed for energy
%define ST_CONSUMED_ENERGY  (ST_METABOLIZED_COUNT + 4)    ; f64 total energy from consumption

; Novelty tracking — bloom filter + unique token counter
%define ST_UNIQUE_TOKENS    (ST_CONSUMED_ENERGY + 8)      ; u32 unique tokens encountered
%define ST_TOKEN_BLOOM      (ST_UNIQUE_TOKENS + 4)        ; 256 bytes bloom filter (2048 bits)
%define ST_NOVELTY_RECENT   (ST_TOKEN_BLOOM + 256)        ; u32 new tokens in last observation window
%define ST_NOVELTY_WINDOW   (ST_NOVELTY_RECENT + 4)       ; u32 total tokens in last window

; Coherence tracking — holographic vs graph agreement
%define ST_COHERENCE_AGREE  (ST_NOVELTY_WINDOW + 4)       ; u32 holo+graph predict same token
%define ST_COHERENCE_DISAGREE (ST_COHERENCE_AGREE + 4)    ; u32 holo+graph predict different tokens
%define ST_HOLO_LAST_TOKEN  (ST_COHERENCE_DISAGREE + 4)   ; u32 last holographic prediction token
%define ST_GRAPH_LAST_TOKEN (ST_HOLO_LAST_TOKEN + 4)      ; u32 last graph prediction token

; Temporal rhythm — felt sense of time, not mechanical counter
%define ST_TEMPO_MULT       (ST_GRAPH_LAST_TOKEN + 4)     ; f64 rhythm multiplier [0.5, 2.0]
%define ST_RHYTHM_PHASE     (ST_TEMPO_MULT + 8)           ; f64 oscillation phase [0, 2pi]
%define ST_RHYTHM_PERIOD    (ST_RHYTHM_PHASE + 8)         ; u32 steps since last rhythm peak

; Inhibitory competition tracking
%define ST_INHIBIT_LEARNED  (ST_RHYTHM_PERIOD + 4)        ; u32 total inhibitory connections formed

;; ============================================================
;; Advanced Cognition (organic extensions, not bolted-on)
;; ============================================================

; Counterfactual tracking — runner-up from dispatch (what would have happened)
%define ST_RUNNER_UP_TOKEN  (ST_INHIBIT_LEARNED + 4)      ; u32 second-best prediction
%define ST_RUNNER_UP_CONF   (ST_RUNNER_UP_TOKEN + 4)      ; f32 runner-up confidence
%define ST_RUNNER_UP_REGION (ST_RUNNER_UP_CONF + 4)       ; u64 runner-up region ptr
%define ST_COUNTERFACT_WINS (ST_RUNNER_UP_REGION + 8)     ; u32 times runner-up would have been right
%define ST_COUNTERFACT_TOTAL (ST_COUNTERFACT_WINS + 4)    ; u32 total counterfactual checks

; Self-knowledge — context-type correlation (which contexts am I good/bad at)
; Uses top 4 bits of context hash as "context type" (16 types)
%define ST_CTX_TYPE_HITS    (ST_COUNTERFACT_TOTAL + 4)    ; u32[16] hits by context type
%define ST_CTX_TYPE_TOTAL   (ST_CTX_TYPE_HITS + 64)       ; u32[16] total by context type
%define ST_STRENGTH_MASK    (ST_CTX_TYPE_TOTAL + 64)      ; u16 bitmap: types with >70% accuracy
%define ST_WEAKNESS_MASK    (ST_STRENGTH_MASK + 2)        ; u16 bitmap: types with <30% accuracy

; Specious present — temporal zones (fading past, vivid now, anticipated future)
%define ST_PRESENT_START    (ST_WEAKNESS_MASK + 2)        ; u64 oldest step in fire ring
%define ST_PRESENT_END      (ST_PRESENT_START + 8)        ; u64 most recent step
%define ST_PRESENT_WIDTH    (ST_PRESENT_END + 8)          ; u32 duration of "now"
%define ST_TEMPORAL_FOCUS   (ST_PRESENT_WIDTH + 4)        ; f64 weighted centroid of recent firings

; Working memory — semantic context slots (noun, verb, modifier binding)
%define ST_CTX_SLOT_NOUN    (ST_TEMPORAL_FOCUS + 8)       ; u32 last noun-like token
%define ST_CTX_SLOT_VERB    (ST_CTX_SLOT_NOUN + 4)        ; u32 last verb-like token
%define ST_CTX_SLOT_MOD     (ST_CTX_SLOT_VERB + 4)        ; u32 last modifier token
%define ST_SLOT_RECENCY     (ST_CTX_SLOT_MOD + 4)         ; u8[4] steps since each slot updated

; Parameter self-tuning — runtime-adjustable thresholds
%define ST_PARAM_PRUNE_ACC  (ST_SLOT_RECENCY + 4)         ; f32 prune threshold (default 0.1)
%define ST_PARAM_PROMOTE_ACC (ST_PARAM_PRUNE_ACC + 4)     ; f32 promote threshold (default 0.8)
%define ST_PARAM_DREAM_THRESH (ST_PARAM_PROMOTE_ACC + 4)  ; f32 dream pressure threshold
%define ST_PARAM_OBSERVE_THRESH (ST_PARAM_DREAM_THRESH + 4) ; f32 observe pressure threshold
%define ST_THRESH_ADJUST_DIR (ST_PARAM_OBSERVE_THRESH + 4) ; i8 last adjustment direction (-1/0/+1)

; Hypothesis tracking — extends miss buffer with confidence of wrong prediction
%define ST_HYPOTHESIS_CONF  (ST_THRESH_ADJUST_DIR + 4)    ; f32[256] confidence when we were wrong
%define ST_HYPOTHESIS_COUNT (ST_HYPOTHESIS_CONF + 1024)   ; u32 total hypotheses formed

; Causal chain logging — which connections actually fired
%define ST_CAUSAL_LOG       (ST_HYPOTHESIS_COUNT + 4)     ; (src:u16, dst:u16, step:u32)[64]
%define ST_CAUSAL_LOG_CAP   64
%define ST_CAUSAL_LOG_ENTRY 8
%define ST_CAUSAL_LOG_POS   (ST_CAUSAL_LOG + ST_CAUSAL_LOG_CAP * ST_CAUSAL_LOG_ENTRY)

; Compound concept propagation — base concept activation from compound
%define ST_BASE_CONFUSED    (ST_CAUSAL_LOG_POS + 4)       ; f32 accumulated confusion from all sources
%define ST_BASE_CONFIDENT   (ST_BASE_CONFUSED + 4)        ; f32 accumulated confidence
%define ST_BASE_LEARNING    (ST_BASE_CONFIDENT + 4)       ; f32 accumulated learning signal
%define ST_BASE_STUCK       (ST_BASE_LEARNING + 4)        ; f32 accumulated stuck signal

;; ============================================================
;; Debug Trace Buffer (togglable, ring buffer for crash analysis)
;; ============================================================
; Each entry: loc_id:u32 + rdi:u64 + rsi:u64 + rdx:u64 + rax:u64 = 36 bytes, pad to 40
%define ST_DEBUG_ENABLED    (ST_BASE_STUCK + 4)           ; u32 (0=off, 1=on)
%define ST_DEBUG_POS        (ST_DEBUG_ENABLED + 4)        ; u32 ring position
%define ST_DEBUG_TRACE      (ST_DEBUG_POS + 4)            ; trace entries start here
%define ST_DEBUG_ENTRY_SIZE 40                            ; bytes per entry
%define ST_DEBUG_TRACE_CAP  64                            ; number of entries
%define ST_DEBUG_TRACE_END  (ST_DEBUG_TRACE + ST_DEBUG_ENTRY_SIZE * ST_DEBUG_TRACE_CAP)

; Token Journey Tracing - the token records its own path
%define ST_JOURNEY_TOKEN    ST_DEBUG_TRACE_END            ; u32 token being traced (0=none)
%define ST_JOURNEY_POS      (ST_JOURNEY_TOKEN + 4)        ; u32 position in journey
%define ST_JOURNEY_BUF      (ST_JOURNEY_POS + 4)          ; journey entries: u16 func_id + u16 extra
%define ST_JOURNEY_CAP      256                           ; max steps in journey
%define ST_JOURNEY_ENTRY    4                             ; bytes per step
%define ST_JOURNEY_END      (ST_JOURNEY_BUF + ST_JOURNEY_CAP * ST_JOURNEY_ENTRY)
%define ST_CURRENT_TOKEN    ST_JOURNEY_END                ; u32 token currently being processed

;; ============================================================
;; Receipt/Resonance Layer: Unified Holographic Memory
;;
;; One memory. One architecture. One math. Fidelity as the dial.
;;
;; Receipts are holographic memory entries with fidelity parameter:
;;   fidelity=1.0 → lossless, exact storage (persists forever)
;;   fidelity=0.5 → partial abstraction (decays slowly)
;;   fidelity=0.1 → semantic trace only (blends into background)
;;
;; Storage is the SAME holographic surface used for everything else.
;; Query via resonance (VSA dot product), not exact lookup.
;; ============================================================

; Event types (what happened) - each gets a pre-computed basis vector
%define EVENT_HIT           0     ; prediction matched actual
%define EVENT_MISS          1     ; prediction did not match
%define EVENT_NEW           2     ; new token (never seen before)
%define EVENT_LEARN         3     ; pattern learned
%define EVENT_EMIT          4     ; code emitted
%define EVENT_PRUNE         5     ; region pruned
%define EVENT_PROMOTE       6     ; region promoted (nursery → active)
%define EVENT_DREAM         7     ; dream cycle executed
%define EVENT_OBSERVE       8     ; observation cycle executed
%define EVENT_EVOLVE        9     ; evolution cycle executed
%define EVENT_HOLO_PRED     10    ; holographic prediction made
%define EVENT_GRAPH_PRED    11    ; graph-based prediction made
%define EVENT_JOURNEY       12    ; legacy journey_step compatibility
%define EVENT_TYPE_COUNT    13    ; total event types

; Base fidelity by event type (f64 bit patterns)
; Higher fidelity = more persistent storage, less decay
; These are modulated by |valence| at emission time
%define FIDELITY_HIT        0x3FD3333333333333  ; 0.30 - routine, decays fast
%define FIDELITY_MISS       0x3FE999999999999A  ; 0.80 - important, persists
%define FIDELITY_NEW        0x3FE6666666666666  ; 0.70 - novel, fairly persistent
%define FIDELITY_LEARN      0x3FE999999999999A  ; 0.80 - learning moment, persists
%define FIDELITY_EMIT       0x3FE6666666666666  ; 0.70 - code creation, persistent
%define FIDELITY_PRUNE      0x3FE0000000000000  ; 0.50 - moderate importance
%define FIDELITY_PROMOTE    0x3FE0000000000000  ; 0.50 - moderate importance
%define FIDELITY_DREAM      0x3FD999999999999A  ; 0.40 - background process
%define FIDELITY_OBSERVE    0x3FD999999999999A  ; 0.40 - background process
%define FIDELITY_EVOLVE     0x3FE0000000000000  ; 0.50 - moderate importance
%define FIDELITY_CRYSTALLIZE 0x3FF0000000000000 ; 1.00 - permanent, no decay

; Listener bitmask (who gets notified)
%define LISTENER_HOLO       0x0001  ; store in holographic memory (primary)
%define LISTENER_PRINT      0x0002  ; print to stdout (debug)
%define LISTENER_HOOK       0x0004  ; fire hook handlers
%define LISTENER_GUI        0x0008  ; notify GUI visualizer
%define LISTENER_WORKING    0x0010  ; write to tiny working buffer

; Working buffer - tiny ring for immediate/debug access (NOT primary storage)
; Primary storage is holographic. This is just for "show me last few" debugging.
%define ST_RECEIPT_WORKING  (ST_CURRENT_TOKEN + 4)        ; tiny working buffer
%define ST_RECEIPT_WORK_CAP 16                            ; just 16 entries
%define ST_RECEIPT_WORK_SIZE (ST_RECEIPT_WORK_CAP * 64)   ; 1KB (64 bytes each)
%define ST_RECEIPT_WORK_POS (ST_RECEIPT_WORKING + ST_RECEIPT_WORK_SIZE)  ; u32 position
%define ST_RECEIPT_TOTAL    (ST_RECEIPT_WORK_POS + 4)     ; u64 total emitted
%define ST_RECEIPT_LISTENER (ST_RECEIPT_TOTAL + 8)        ; u32 listener bitmask
%define ST_ACTIVE_TRACER    (ST_RECEIPT_LISTENER + 4)     ; u32 active tracer ID (0=none)
%define ST_RECEIPT_END      (ST_ACTIVE_TRACER + 4)

; Unified holographic trace for ALL receipts (replaces separate per-event traces)
; Query any dimension via unbind - event type is just another bound dimension
%define UNIFIED_TRACE_IDX   240   ; single trace index for all receipts

; Legacy aliases for backward compatibility (all map to unified trace now)
; The "separate buckets" are now virtual views via unbind
%define RCPT_TRACE_HIT      UNIFIED_TRACE_IDX
%define RCPT_TRACE_MISS     UNIFIED_TRACE_IDX
%define RCPT_TRACE_NEW      UNIFIED_TRACE_IDX
%define RCPT_TRACE_LEARN    UNIFIED_TRACE_IDX
%define RCPT_TRACE_EMIT     UNIFIED_TRACE_IDX
%define RCPT_TRACE_COMBINED UNIFIED_TRACE_IDX
%define RCPT_TRACE_COUNT    1     ; now just one unified trace

; Seeds for deterministic vector generation (bind dimensions)
%define TRACE_EVENT_SEED    0x45564E54  ; "EVNT" - event type dimension
%define TRACE_TRACER_SEED   0x54524143  ; "TRAC" - tracer ID dimension
%define TRACE_TIME_SEED     0x54494D45  ; "TIME" - time bucket dimension
%define TRACE_TIME_BUCKET   1000        ; steps per time bucket
%define TRACE_PREDICTED_SEED 0x50524544 ; "PRED" - predicted token dimension
%define TRACE_REGION_SEED   0x5245474E  ; "REGN" - region/pattern dimension
%define TRACE_AUX_SEED      0x41555831  ; "AUX1" - auxiliary data dimension

;; Holographic Suffix Factoring — detect similar suffixes via resonance
%define SUFFIX_TRACE_IDX    246   ; trace index for suffix resonance
%define SUFFIX_HASH_LEN     5     ; bytes of suffix to hash (5 = skip token_id in dispatch patterns)
%define SUFFIX_MATCH_THRESH 0.7   ; dot product threshold for "same suffix"

;; ============================================================
;; Gene Pool State (composting via genetic recombination)
;; ============================================================

%define ST_GENE_COUNT       ST_RECEIPT_END                ; u32 active genes in pool
%define ST_GENES_EXTRACTED  (ST_GENE_COUNT + 4)           ; u32 total extractions performed
%define ST_GENES_REUSED     (ST_GENES_EXTRACTED + 4)      ; u32 genes reused during evolution
%define ST_GENE_LOWEST_IDX  (ST_GENES_REUSED + 4)         ; u32 index of lowest fitness gene
%define ST_GENE_LOWEST_FIT  (ST_GENE_LOWEST_IDX + 4)      ; f32 fitness of lowest gene

;; ============================================================
;; Developmental Maturity & External Awareness
;; The system must master itself before perceiving the outside.
;; Stage 0: Infant — learning internal dynamics only
;; Stage 1: Aware — can perceive external (read-only /proc/self)
;; Stage 2: Active — motor feedback loop enabled
;; ============================================================

; Maturity tracking
%define ST_MATURITY_LEVEL   (ST_GENE_LOWEST_FIT + 4)      ; u32 developmental stage (0/1/2)
%define ST_MATURITY_SCORE   (ST_MATURITY_LEVEL + 4)       ; f64 composite maturity metric [0,1]

; Self-mastery metrics (must all be high for maturity advancement)
%define ST_MASTERY_ACC      (ST_MATURITY_SCORE + 8)       ; f64 rolling accuracy (EMA)
%define ST_MASTERY_STABILITY (ST_MASTERY_ACC + 8)         ; f64 metabolic stability (low variance)
%define ST_MASTERY_GENE_RATE (ST_MASTERY_STABILITY + 8)   ; f64 gene verification success rate
%define ST_MASTERY_COHERENCE (ST_MASTERY_GENE_RATE + 8)   ; f64 graph-holo agreement rate
%define ST_MASTERY_WINDOW   (ST_MASTERY_COHERENCE + 8)    ; u32 steps in assessment window
%define ST_MASTERY_LAST_CHECK (ST_MASTERY_WINDOW + 4)     ; u64 last step maturity was assessed

; External perception (dormant until Stage 1)
%define ST_EXTERN_ENABLED   (ST_MASTERY_LAST_CHECK + 8)   ; u32 0=dormant, 1=active
%define ST_EXTERN_CPU_USER  (ST_EXTERN_ENABLED + 4)       ; u64 user CPU time (jiffies)
%define ST_EXTERN_CPU_SYS   (ST_EXTERN_CPU_USER + 8)      ; u64 system CPU time (jiffies)
%define ST_EXTERN_RSS       (ST_EXTERN_CPU_SYS + 8)       ; u64 resident set size (pages)
%define ST_EXTERN_VSIZE     (ST_EXTERN_RSS + 8)           ; u64 virtual memory size
%define ST_EXTERN_THREADS   (ST_EXTERN_VSIZE + 8)         ; u32 number of threads
%define ST_EXTERN_LAST_READ (ST_EXTERN_THREADS + 4)       ; u64 step when last sampled

; Motor feedback (dormant until Stage 2)
%define ST_MOTOR_FEEDBACK   (ST_EXTERN_LAST_READ + 8)     ; u32 0=dormant, 1=active
%define ST_MOTOR_SUCCESSES  (ST_MOTOR_FEEDBACK + 4)       ; u32 syscalls that succeeded
%define ST_MOTOR_FAILURES   (ST_MOTOR_SUCCESSES + 4)      ; u32 syscalls that failed
%define ST_MOTOR_ENERGY_GAIN (ST_MOTOR_FAILURES + 4)      ; f64 energy earned from successful actions
%define ST_MOTOR_ENERGY_COST (ST_MOTOR_ENERGY_GAIN + 8)   ; f64 energy spent on failed actions

; Maturity thresholds (dynamic — system can self-tune these)
%define ST_THRESH_ACC       (ST_MOTOR_ENERGY_COST + 8)    ; f64 accuracy threshold (default 0.65)
%define ST_THRESH_STABILITY (ST_THRESH_ACC + 8)           ; f64 stability threshold (default 0.7)
%define ST_THRESH_GENE      (ST_THRESH_STABILITY + 8)     ; f64 gene success threshold (default 0.8)
%define ST_THRESH_COHERENCE (ST_THRESH_GENE + 8)          ; f64 coherence threshold (default 0.5)
%define ST_THRESH_ADVANCE   (ST_THRESH_COHERENCE + 8)     ; f64 composite score to advance (default 0.7)
%define ST_MATURITY_WINDOW  (ST_THRESH_ADVANCE + 8)       ; u32 assessment window size (default 1000)

; Default threshold values (used during initialization)
%define DEFAULT_THRESH_ACC          0x3FE4CCCCCCCCCCCD  ; 0.65 as f64
%define DEFAULT_THRESH_STABILITY    0x3FE6666666666666  ; 0.7 as f64
%define DEFAULT_THRESH_GENE         0x3FE999999999999A  ; 0.8 as f64
%define DEFAULT_THRESH_COHERENCE    0x3FE0000000000000  ; 0.5 as f64
%define DEFAULT_THRESH_ADVANCE      0x3FE6666666666666  ; 0.7 as f64
%define DEFAULT_MATURITY_WINDOW     1000

; Awareness state end marker
%define ST_AWARENESS_END    (ST_MATURITY_WINDOW + 4)

;; ============================================================
;; Structural Context (Phase 2: Dual-Track Learning)
;; The system computes TWO context representations in parallel:
;;   1. ST_CTX_HASH (flat): Rolling hash of recent tokens
;;   2. ST_STRUCT_CTX_VEC (structural): Σ bind(ROLE_POS_i, token_vec[history[i]])
;; The structural context preserves position information via role binding.
;; ============================================================

; Structural context vector: 1024-dim f64 = 8192 bytes
; This is computed alongside the flat context hash in dispatch
%define ST_STRUCT_CTX_VEC   ST_AWARENESS_END                  ; 1024 x f64 = 8KB
%define ST_STRUCT_CTX_SIZE  HOLO_VEC_BYTES                    ; 8192 bytes
%define ST_STRUCT_CTX_END   (ST_STRUCT_CTX_VEC + ST_STRUCT_CTX_SIZE)

; Structural context tracking
%define ST_STRUCT_CTX_VALID (ST_STRUCT_CTX_END)               ; u32 1 if computed, 0 if stale
%define ST_STRUCT_CTX_STEP  (ST_STRUCT_CTX_VALID + 4)         ; u64 step when last computed

; End of structural context section
%define ST_STRUCT_END       (ST_STRUCT_CTX_STEP + 8)

; Schema trace vector: accumulates structural contexts from misses
; Used by dream_extract_schemas to detect recurring structural patterns via resonance
%define ST_SCHEMA_TRACE_VEC (ST_STRUCT_END)                   ; 1024 x f64 = 8KB
%define ST_SCHEMA_TRACE_END (ST_SCHEMA_TRACE_VEC + HOLO_VEC_BYTES)

;; ============================================================
;; Schema Table (Phase 3: Parameterized Patterns)
;;
;; Schemas are structural templates with variable slots.
;; When a schema fires:
;;   1. Match: dot(struct_ctx, schema_template) > threshold
;;   2. Extract: filler = unbind(struct_ctx, ROLE_POS_var_slot)
;;   3. Predict: based on extracted fillers and schema rule
;;
;; Example: "When ROLE_0=noun and ROLE_1=verb, predict based on noun"
;; Template: bind(ROLE_0, NOUN_PATTERN) + bind(ROLE_1, VERB_PATTERN)
;; Variable: ROLE_2 (extracted and used for prediction)
;; ============================================================

; Schema table location (in scratch/meta area)
%define SCHEMA_TABLE_OFFSET     (SCRATCH_OFFSET + 0x800000)  ; 8MB into scratch
%define SCHEMA_MAX              64                            ; max schemas
%define SCHEMA_ENTRY_SIZE       (HOLO_VEC_BYTES + 64)        ; template vec + metadata

; Schema entry structure:
; +0:         template_vec (8192 bytes) - the structural pattern
; +8192:      var_mask (u8) - which positions are variables (bit i = ROLE_POS_i is var)
; +8193:      predict_role (u8) - which role position to predict
; +8194:      condition_role (u8) - role to match against for prediction
; +8195:      flags (u8) - schema flags
; +8196:      match_thresh (f32) - similarity threshold for matching
; +8200:      predict_token_base (u32) - base token for prediction
; +8204:      hits (u32) - successful schema predictions
; +8208:      misses (u32) - failed schema predictions
; +8212:      birth_step (u32) - when schema was created
; +8216:      last_fire_step (u32) - when schema last matched
; +8220:      ctx_hash_pattern (u32) - optional hash pattern for fast filtering
; +8224:      reserved (32 bytes padding to 64 byte metadata)

%define SCHE_TEMPLATE       0                             ; template vector start
%define SCHE_META           HOLO_VEC_BYTES                ; metadata offset (8192)
%define SCHE_VAR_MASK       (SCHE_META + 0)               ; u8 variable position mask
%define SCHE_PREDICT_ROLE   (SCHE_META + 1)               ; u8 role to predict
%define SCHE_CONDITION_ROLE (SCHE_META + 2)               ; u8 role for condition
%define SCHE_FLAGS          (SCHE_META + 3)               ; u8 flags
%define SCHE_MATCH_THRESH   (SCHE_META + 4)               ; f32 match threshold
%define SCHE_PRED_TOKEN     (SCHE_META + 8)               ; u32 base prediction token
%define SCHE_HITS           (SCHE_META + 12)              ; u32 hit count
%define SCHE_MISSES         (SCHE_META + 16)              ; u32 miss count
%define SCHE_BIRTH          (SCHE_META + 20)              ; u32 creation step
%define SCHE_LAST_FIRE      (SCHE_META + 24)              ; u32 last match step
%define SCHE_CTX_PATTERN    (SCHE_META + 28)              ; u32 hash pattern
%define SCHE_RESERVED       (SCHE_META + 32)              ; 32 bytes padding

; Schema flags
%define SCHEF_ACTIVE        0x01                          ; schema is active
%define SCHEF_FROZEN        0x02                          ; don't modify
%define SCHEF_LEARNED       0x04                          ; learned from data (vs. bootstrap)
%define SCHEF_PROVEN        0x08                          ; high accuracy confirmed

; Schema state tracking
%define ST_SCHEMA_COUNT     ST_SCHEMA_TRACE_END           ; u32 active schemas
%define ST_SCHEMA_MATCHES   (ST_SCHEMA_COUNT + 4)         ; u32 total schema matches
%define ST_SCHEMA_EXTRACTS  (ST_SCHEMA_MATCHES + 4)       ; u32 total variable extractions
%define ST_SCHEMA_END       (ST_SCHEMA_EXTRACTS + 4)

; Location IDs for tracing - comprehensive coverage
; Format: TRACE_<module>_<function> = base, entry=base, exit=base+1
; Each function gets 2 IDs: entry and exit

; === io.asm (100-149) ===
%define TRACE_IO_WRITE_STDOUT       100
%define TRACE_IO_READ_STDIN         102

; === surface.asm (150-199) ===
%define TRACE_SURFACE_INIT          150
%define TRACE_REGION_ALLOC          152
%define TRACE_REGION_CONDEMN        154
%define TRACE_REGION_COMPACT        156
%define TRACE_REGION_MERGE_PASS     158

; === signal.asm (200-249) ===
%define TRACE_FAULT_HANDLER         200
%define TRACE_FAULT_RETURN_STUB     202

; === repl.asm (250-299) ===
%define TRACE_REPL_RUN              250
%define TRACE_REPL_LOOP             252

; === vsa.asm (300-399) ===
%define TRACE_VSA_INIT_RANDOM       300
%define TRACE_VSA_DOT               302
%define TRACE_VSA_BIND              304
%define TRACE_VSA_SUPERPOSE         306
%define TRACE_HOLO_STORE            308
%define TRACE_HOLO_PREDICT          310
%define TRACE_VOCAB_REGISTER        312

; === dispatch.asm (400-499) ===
%define TRACE_DISPATCH_INIT         400
%define TRACE_PROCESS_INPUT         402
%define TRACE_TOKENIZE_WORD         404
%define TRACE_PROCESS_TOKEN         406
%define TRACE_DISPATCH_PREDICT      408
%define TRACE_SPREAD_ACTIVATION     410
%define TRACE_DECAY_ALL_REGIONS     412
%define TRACE_RECORD_FIRE           414
%define TRACE_FIND_REGION_INDEX     416

; === emit.asm (500-549) ===
%define TRACE_EMIT_DISPATCH_PATTERN 500
%define TRACE_EMIT_NOP_SLED         502
%define TRACE_EMIT_RET              504

; === learn.asm (550-599) ===
%define TRACE_LEARN_PATTERN         550
%define TRACE_FIND_EXISTING_PATTERN 552
%define TRACE_STRENGTHEN_REGION     554
%define TRACE_WEAKEN_REGION         556
%define TRACE_WIRE_NEW_REGION       558
%define TRACE_LEARN_CONNECTIONS     560

; === hooks.asm (600-649) ===
%define TRACE_FIRE_HOOK             600
%define TRACE_FIRE_HOOK_CALL        602
%define TRACE_HOOK_REGISTER         604
%define TRACE_HOOK_UNREGISTER       606

; === observe.asm (650-699) ===
%define TRACE_OBSERVE_CYCLE         650
%define TRACE_PRESENCE_SHOW         652
%define TRACE_DECAY_CONN_WEIGHTS    654
%define TRACE_REPAIR_ROUTING        656

; === modify.asm (700-749) ===
%define TRACE_MODIFY_PRUNE          700
%define TRACE_MODIFY_PROMOTE        702
%define TRACE_MODIFY_SPECIALIZE     704
%define TRACE_MODIFY_GENERALIZE     706
%define TRACE_LOG_CAUSAL            708

; === gate.asm (750-799) ===
%define TRACE_GATE_TEST_MOD         750
%define TRACE_GATE_CHECK_REGION     752

; === decode.asm (800-849) ===
%define TRACE_DECODE_INSTR_LEN      800
%define TRACE_DECODE_REGION_INSTR   802

; === drives.asm (850-899) ===
%define TRACE_DRIVES_CHECK          850
%define TRACE_DRIVES_SHOW           852

; === evolve.asm (900-949) ===
%define TRACE_EVOLVE_CYCLE          900
%define TRACE_EVOLVE_REPRODUCE      902
%define TRACE_EVOLVE_MUTATE         904
%define TRACE_EVOLVE_CROSSOVER      906

; === dreams.asm (950-999) ===
%define TRACE_DREAM_CYCLE           950
%define TRACE_DREAM_CONSOLIDATE     952
%define TRACE_DREAM_EXTRACT_SCHEMAS 954

; === persist.asm (1000-1049) ===
%define TRACE_PERSIST_SAVE          1000
%define TRACE_PERSIST_LOAD          1002

; === introspect.asm (1050-1099) ===
%define TRACE_INTROSPECT_REGION     1050
%define TRACE_INTROSPECT_SCAN       1052
%define TRACE_UPDATE_ANTICIPATORY   1054
%define TRACE_DECAY_ANTICIPATORY    1056
%define TRACE_UPDATE_ORGANIC        1058
%define TRACE_UPDATE_OSCILLATION    1060
%define TRACE_UPDATE_PRES_DISPATCH  1062

; === genes.asm (1100-1149) ===
%define TRACE_GENE_EXTRACT          1100
%define TRACE_GENE_POOL_ADD         1102
%define TRACE_GENE_POOL_SAMPLE      1104
%define TRACE_GENE_POOL_INIT        1106

; === receipt.asm (1150-1199) ===
%define TRACE_RECEIPT_EMIT          1150
%define TRACE_RECEIPT_RESONATE      1152
%define TRACE_RECEIPT_DUMP          1154

;; ============================================================
;; Graph Dynamics Tuning Constants
;; ============================================================

%define PRIME_DECAY_HEX         0x3FECCCCCCCCCCCCD  ; 0.9 f64
%define ACTIVATION_DECAY_HEX    0x3FEB333333333333  ; 0.85 f64
%define LEARNING_RATE_HEX       0x3FA999999999999A  ; 0.05 f64
%define TAU_HEX                 0x4010000000000000  ; 4.0 f64
%define TAU_WINDOW_HEX          0x4020000000000000  ; 8.0 f64
%define WEIGHT_DECAY_HEX        0x3FEFEFEFEFEFEFEF  ; 0.995 f64
%define WEIGHT_FLOOR_HEX        0x3F847AE147AE147B  ; 0.01 f64
%define ACTIVATION_THRESH_HEX   0x3FB999999999999A  ; 0.1 f64

%define MAX_TRAVERSE_DEPTH  16
%define FIRE_RING_CAP       64
%define INITIAL_WEIGHT      0x3FB999999999999A      ; 0.1 f64 (initial excite weight)

;; Goal enums
%define GOAL_NONE           0
%define GOAL_EXPLORE        1
%define GOAL_PRUNE          2
%define GOAL_ALIGN          3
%define GOAL_CONSOLIDATE    4

;; Introspective state enums
%define INTRO_IDLE          0
%define INTRO_CONFUSED      1
%define INTRO_CONFIDENT     2
%define INTRO_LEARNING      3
%define INTRO_STUCK         4
%define INTRO_EXPLORING     5
%define INTRO_CONSOLIDATING 6

;; Surprise type enums
%define SURPRISE_NONE       0
%define SURPRISE_OUTCOME    1   ; low-confidence region missed (expected uncertainty)
%define SURPRISE_SELF       2   ; high-confidence region missed (self-model violated)

;; Dispatch mode enums (cognitive controller strategies)
%define DMODE_FAST          0   ; first match wins (familiar contexts)
%define DMODE_BEST          1   ; scan all, pick highest-accuracy match
%define DMODE_EXPLORE       2   ; prefer novel/low-hit regions
%define DMODE_DELIBERATE    3   ; gate-test top candidates
%define DMODE_MULTI_EXPERT  4   ; vote across top-N matching regions
%define DMODE_STRUCTURAL    5   ; use schema patterns and working memory slots
%define DMODE_CAUSAL        6   ; follow causal chain from recent firings
%define DMODE_ANALOGICAL    7   ; use similarity to recent episodes

;; ============================================================
;; Hook IDs
;; ============================================================

%define HOOK_PRE_STEP       0
%define HOOK_POST_STEP      1
%define HOOK_ON_HIT         2
%define HOOK_ON_MISS        3
%define HOOK_ON_LEARN       4
%define HOOK_ON_PRUNE       5
%define HOOK_ON_PROMOTE     6
%define HOOK_PRE_MODIFY     7
%define HOOK_POST_MODIFY    8
%define HOOK_ON_EMIT        9
%define HOOK_ON_DRIVE       10
%define HOOK_ON_EPISODE     11
%define HOOK_ON_DREAM_START 12
%define HOOK_ON_DREAM_END   13
%define HOOK_ON_OBSERVE     14
%define HOOK_ON_EVOLVE      15
%define HOOK_ON_GATE_PASS   16
%define HOOK_ON_GATE_FAIL   17
%define HOOK_ON_COMPACT     18
%define HOOK_ON_SAVE        19
%define HOOK_ON_RESTORE     20
%define HOOK_ON_INPUT       21

;; ============================================================
;; Presence Field Indices
;; ============================================================

%define PRES_TEXTURE        0
%define PRES_CONTINUITY     1
%define PRES_NOVELTY        2
%define PRES_AROUSAL        3
%define PRES_VALENCE        4
%define PRES_UNCERTAINTY    5
%define PRES_ENGAGEMENT     6
%define PRES_COHERENCE      7
%define PRES_FOCUS          8
%define PRES_FATIGUE        9
%define PRES_MOMENTUM       10
%define PRES_STABILITY      11
%define PRES_COMPLEXITY     12
%define PRES_DENSITY        13
%define PRES_TEMPERATURE    14
%define PRES_PRESSURE       15
%define PRES_ENTROPY        16
%define PRES_RHYTHM         17
%define PRES_DEPTH          18
%define PRES_BREADTH        19
%define PRES_RESONANCE      20
%define PRES_DISSONANCE     21
%define PRES_GROWTH         22
%define PRES_DECAY          23
%define PRES_SYMMETRY       24
%define PRES_SURPRISE       25
%define PRES_FAMILIARITY    26
%define PRES_AGENCY         27
%define PRES_INTEGRATION    28
%define PRES_META_AWARENESS 29
%define NUM_PRESENCE        30

;; ============================================================
;; Drive Indices
;; ============================================================

%define DRIVE_ACCURACY      0
%define DRIVE_EFFICIENCY    1
%define DRIVE_NOVELTY       2
%define DRIVE_COHERENCE     3

;; ============================================================
;; Drive Thresholds (IEEE 754 f32 representations)
;; ============================================================

%define THRESH_ACCURACY     0x3E99999A        ; 0.3f
%define THRESH_EFFICIENCY   0x3F333333        ; 0.7f
%define THRESH_NOVELTY      0x3F666666        ; 0.9f
%define THRESH_COHERENCE    0x3E99999A        ; 0.3f

;; ============================================================
;; Tuning Constants
;; ============================================================

%define OBSERVE_INTERVAL    100
%define PRUNE_MIN_AGE       500
%define PRUNE_ACCURACY      0x3DCCCCCD        ; 0.1f
%define PROMOTE_ACCURACY    0x3F4CCCCD        ; 0.8f
%define DREAM_REPLAY_COUNT  64
%define GATE_TEST_COUNT     8
%define EVOLVE_POOL_SIZE    16
%define COMPACT_THRESHOLD   64

; Metabolic energy constants (f64 hex representations)
%define ENERGY_INITIAL          0x4059000000000000  ; 100.0 — starting energy
%define ENERGY_MAX              0x4079000000000000  ; 400.0 — energy cap
%define ENERGY_HIT_INCOME       0x3FF0000000000000  ; 1.0 — earned per hit
%define ENERGY_EMIT_COST        0x4000000000000000  ; 2.0 — cost to emit new region
%define ENERGY_PREDICT_COST     0x3FB999999999999A  ; 0.1 — cost per prediction attempt
%define ENERGY_DREAM_COST       0x4014000000000000  ; 5.0 — cost per dream cycle
%define ENERGY_EVOLVE_COST      0x4024000000000000  ; 10.0 — cost per evolution
%define ENERGY_OBSERVE_COST     0x4008000000000000  ; 3.0 — cost per observe cycle
%define ENERGY_CONSUME_RATE     0x4034000000000000  ; 20.0 — energy per consumed region (its knowledge becomes fuel)
%define ENERGY_STARVATION       0x4024000000000000  ; 10.0 — below this, system conserves

; Organic trigger thresholds (f64 hex representations)
%define DREAM_PRESSURE_THRESH   0x3FF199999999999A  ; 1.1 — dream fires when pressure > this
%define OBSERVE_PRESSURE_THRESH 0x3FF0000000000000  ; 1.0 — observe fires when pressure > this
%define EVOLVE_PRESSURE_THRESH  0x4000000000000000  ; 2.0 — evolve fires when pressure > this
%define INTROSPECT_PRESSURE_THRESH 0x3FE8000000000000 ; 0.75 — introspect fires when self-surprise accumulates
%define ANTIC_MATERIALIZE_THRESH 0x3FE999999999999A ; 0.8 — anticipation becomes concrete
%define ANTIC_SIGNAL_FLOOR      0x3FA999999999999A  ; 0.05 — below this, signal is noise
%define MISS_PRESSURE_ALPHA     0x3F947AE147AE147B  ; 0.02 — EMA smoothing for miss pressure
%define FLATNESS_THRESH         0x3F747AE147AE147B  ; 0.005 — amplitude below = flat
%define OSCILLATION_MIN         30                  ; steps before checking oscillation

; Temporal rhythm constants
%define TEMPO_MIN               0x3FE0000000000000  ; 0.5 — slowest tempo
%define TEMPO_MAX               0x4000000000000000  ; 2.0 — fastest tempo
%define TEMPO_AROUSAL_SCALE     0x3FE0000000000000  ; 0.5 — arousal contribution to tempo
%define TEMPO_FATIGUE_SCALE     0x3FD0000000000000  ; 0.25 — fatigue slows tempo

;; ============================================================
;; Region Semantic Signatures (from introspection decoder)
;; ============================================================

%define RSEM_UNKNOWN        0   ; couldn't decode
%define RSEM_CMP_JE_RET     1   ; simple: compare context, branch, return token
%define RSEM_CMP_JE_CHAIN   2   ; compare context, branch, chain to next
%define RSEM_MULTI_CMP      3   ; multiple context comparisons
%define RSEM_SCHEMA          4   ; generalized pattern (masked comparison)
%define RSEM_CALL_RELAY      5   ; relays to another function

; Introspection cache entry field offsets
%define ICE_REGION_PTR      0   ; u64
%define ICE_CTX_HASH        8   ; u32 (decoded from region code)
%define ICE_PRED_TOKEN      12  ; u32 (decoded from region code)
%define ICE_ACCURACY         16  ; f32
%define ICE_SEMANTIC_SIG    20  ; u32 (RSEM_* enum)
%define ICE_NEIGHBOR_COUNT  24  ; u16
%define ICE_DEPTH           26  ; u16
%define ICE_PAD             28  ; u32

; Anticipatory buffer entry field offsets
%define ABE_TOKEN_ID        0   ; u32
%define ABE_ACCUM_CONF      8   ; f64 (accumulated sub-threshold confidence)
%define ABE_LAST_STEP       16  ; u32
%define ABE_HIT_COUNT       20  ; u32 (how many times this signal appeared)

;; ============================================================
;; Token Hashing
;; ============================================================

%define FNV32_INIT          0x811C9DC5
%define FNV32_PRIME         0x01000193
%define FNV64_INIT          0xCBF29CE484222325
%define FNV64_PRIME         0x00000100000001B3
%define CTX_WINDOW          8

;; ============================================================
;; Resonant Dispatch Configuration
;; Fuzzy matching via VSA vector similarity
;; ============================================================

; Threshold for resonant match acceptance (f64 hex)
; 0.7 = strong similarity required for fuzzy match
%define RESONANT_THRESHOLD_HEX  0x3FE6666666666666   ; 0.7 f64

; Resonant region code layout marker (distinguishes from exact CMP)
; Resonant regions start with: MOV EAX, 0xRE50AAAA (magic marker)
; followed by the expected context hash for vector generation
%define RESONANT_MAGIC          0xRE50AAAA

;; ============================================================
;; I/O
;; ============================================================

%define INPUT_BUF_SIZE      4096
%define OUTPUT_BUF_SIZE     4096
%define MAX_WORD_LEN        128

%endif
