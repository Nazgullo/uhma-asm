; constants.inc — UHMA system constants, offsets, limits
%ifndef CONSTANTS_INC
%define CONSTANTS_INC

;; ============================================================
;; Surface Layout
;; ============================================================

%define SURFACE_BASE        0x100000000
%define SURFACE_SIZE        0x31000000        ; ~784MB (f64 VSA + holographic memory + vocab)

%define BOOTSTRAP_OFFSET    0x0
%define BOOTSTRAP_SIZE      0x10000           ; 64KB

%define DISPATCH_OFFSET     0x10000
%define DISPATCH_MAX_SIZE   0x10000000        ; 256MB

%define REGION_TABLE_OFFSET 0x10010000
%define REGION_TABLE_SIZE   0x10000           ; 64KB (2048 entries × 32 bytes)
%define REGION_TABLE_MAX    2048

%define VSA_OFFSET          0x10020000
%define VSA_SIZE            0x20000000        ; 512MB (65K tokens × 8KB each, f64)

%define STATE_OFFSET        0x30020000
%define STATE_SIZE          0x10000           ; 64KB

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

;; ============================================================
;; VSA Configuration
;; ============================================================

%define VSA_DIM             1024
%define VSA_VEC_BYTES       (VSA_DIM * 8)     ; 8192 bytes per vector (f64)
%define VSA_MAX_TOKENS      65536

;; ============================================================
;; Holographic Associative Memory Layout (f64 precision)
;; ============================================================

%define HOLO_OFFSET         0x30030000        ; after STATE block
%define HOLO_TRACES         256               ; independent interference traces
%define HOLO_DIM            1024              ; elements per trace vector
%define HOLO_VEC_BYTES      (HOLO_DIM * 8)    ; 8192 bytes per trace (1024 x f64)
%define HOLO_TOTAL          (HOLO_TRACES * HOLO_VEC_BYTES)  ; 2MB

%define VOCAB_OFFSET        (HOLO_OFFSET + 0x200000)  ; token registry after 2MB traces
%define VOCAB_ENTRY_SIZE    8                 ; (u32 token_id, u32 count)
%define VOCAB_MAX_SCAN      256               ; top-N tokens to scan during predict

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

; Oscillation monitoring (flat = dead)
%define ST_PREV_ACCURACY    (ST_EVOLVE_PRESSURE + 8)      ; f64 accuracy N-1 steps ago
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
;; I/O
;; ============================================================

%define INPUT_BUF_SIZE      4096
%define OUTPUT_BUF_SIZE     4096
%define MAX_WORD_LEN        128

%endif
