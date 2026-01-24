; constants.inc — UHMA system constants, offsets, limits
%ifndef CONSTANTS_INC
%define CONSTANTS_INC

;; ============================================================
;; Surface Layout
;; ============================================================

%define SURFACE_BASE        0x100000000
%define SURFACE_SIZE        0x20100000        ; ~513MB (practical size)

%define BOOTSTRAP_OFFSET    0x0
%define BOOTSTRAP_SIZE      0x10000           ; 64KB

%define DISPATCH_OFFSET     0x10000
%define DISPATCH_MAX_SIZE   0x10000000        ; 256MB

%define REGION_TABLE_OFFSET 0x10010000
%define REGION_TABLE_SIZE   0x2000            ; 8KB
%define REGION_TABLE_MAX    256

%define VSA_OFFSET          0x10020000
%define VSA_SIZE            0x10000000        ; 256MB (65K tokens × 4KB each)

%define STATE_OFFSET        0x20020000
%define STATE_SIZE          0x10000           ; 64KB

;; ============================================================
;; Region Header (16 bytes prefix)
;; ============================================================

%define RHDR_HITS           0                 ; u32
%define RHDR_MISSES         4                 ; u32
%define RHDR_BIRTH          8                 ; u32
%define RHDR_CODE_LEN       12                ; u16
%define RHDR_FLAGS          14                ; u16
%define RHDR_SIZE           16

; Region flags
%define RFLAG_ACTIVE        0x0001
%define RFLAG_FROZEN        0x0002
%define RFLAG_NURSERY       0x0004
%define RFLAG_CONDEMNED     0x0008

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
%define RTE_PAD             28                ; u32
%define RTE_SIZE            32

; Region types
%define RTYPE_DISPATCH      0
%define RTYPE_VSA_OP        1
%define RTYPE_MODIFIER      2
%define RTYPE_OBSERVER      3
%define RTYPE_EMITTER       4
%define RTYPE_HOOK          5
%define RTYPE_GATE          6
%define RTYPE_DREAM         7

;; ============================================================
;; VSA Configuration
;; ============================================================

%define VSA_DIM             1024
%define VSA_VEC_BYTES       (VSA_DIM * 4)     ; 4096 bytes per vector
%define VSA_MAX_TOKENS      65536

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

; Drive thresholds (modifiable)
%define ST_DRIVE_THRESH     (ST_PREDICT_REGION + 8)         ; 4 x f32

; Observation config
%define ST_OBS_INTERVAL     (ST_DRIVE_THRESH + 16)          ; u32
%define ST_OBS_LAST_STEP    (ST_OBS_INTERVAL + 4)           ; u64

; Introspective state (computed from metrics each observation)
%define ST_INTRO_STATE      (ST_OBS_LAST_STEP + 8)          ; u32 enum
%define ST_SURPRISE_TYPE    (ST_INTRO_STATE + 4)            ; u32 (per-step)

; Self-prediction (meta-prediction: which region will fire next)
%define ST_SELF_PRED_REGION (ST_SURPRISE_TYPE + 4)          ; u64 ptr
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

;; Dispatch mode enums
%define DMODE_FAST          0   ; first match wins
%define DMODE_BEST          1   ; scan all, pick highest-accuracy match
%define DMODE_EXPLORE       2   ; prefer novel/low-hit regions
%define DMODE_DELIBERATE    3   ; gate-test top candidates

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

;; ============================================================
;; Token Hashing
;; ============================================================

%define FNV32_INIT          0x811C9DC5
%define FNV32_PRIME         0x01000193
%define FNV64_INIT          0xCBF29CE484222325
%define FNV64_PRIME         0x00000100000001B3
%define CTX_WINDOW          8

;; ============================================================
;; I/O
;; ============================================================

%define INPUT_BUF_SIZE      4096
%define OUTPUT_BUF_SIZE     4096
%define MAX_WORD_LEN        128

%endif
