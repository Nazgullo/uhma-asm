; constants.inc — Minimal UHMA state offsets for pet/x86 visualizer
; Subset of ../../include/constants.inc — only what the GUI reads

%define STATE_OFFSET        0x10000

; Simple offsets (no dependency chain)
%define ST_TOKEN_COUNT      260
%define ST_GLOBAL_STEP      272

; Hook table → dispatch_ptr → region_count chain
%define NUM_HOOKS           22
%define HOOK_ENTRY_SIZE     264
%define ST_HOOKS            416
%define ST_DISPATCH_PTR     (ST_HOOKS + NUM_HOOKS * HOOK_ENTRY_SIZE)
%define ST_REGION_COUNT     (ST_DISPATCH_PTR + 8)

; Miss buffer chain
%define ST_MISS_BUF_CAP     256
%define ST_MISS_ENTRY_SIZE  16
%define ST_MISS_BUF         (ST_REGION_COUNT + 8)
%define ST_MISS_POS         (ST_MISS_BUF + ST_MISS_BUF_CAP * ST_MISS_ENTRY_SIZE)

; Episode chain
%define ST_EPISODE_ENTRY    16
%define ST_EPISODE_CAP      64
%define ST_EPISODE_RING     (ST_MISS_POS + 8)
%define ST_EPISODE_POS      (ST_EPISODE_RING + ST_EPISODE_CAP * ST_EPISODE_ENTRY)

; Mod log chain
%define ST_MOD_ENTRY_SIZE   24
%define ST_MOD_LOG_CAP      128
%define ST_MOD_LOG          (ST_EPISODE_POS + 8)
%define ST_MOD_LOG_POS      (ST_MOD_LOG + ST_MOD_LOG_CAP * ST_MOD_ENTRY_SIZE)

; Nursery → last_predict chain
%define ST_NURSERY_SIZE     4096
%define ST_NURSERY          (ST_MOD_LOG_POS + 8)
%define ST_LAST_PREDICT     (ST_NURSERY + ST_NURSERY_SIZE)
%define ST_LAST_CTX         (ST_LAST_PREDICT + 4)
%define ST_PREDICT_REGION   (ST_LAST_CTX + 8)
%define ST_LAST_VALENCE     (ST_PREDICT_REGION + 8)
%define ST_DRIVE_THRESH     (ST_LAST_VALENCE + 8)
%define ST_OBS_INTERVAL     (ST_DRIVE_THRESH + 16)
%define ST_OBS_LAST_STEP    (ST_OBS_INTERVAL + 4)
%define ST_FACTOR_PENDING   (ST_OBS_LAST_STEP + 8)
%define ST_INTRO_STATE      (ST_FACTOR_PENDING + 4)
%define ST_SURPRISE_TYPE    (ST_INTRO_STATE + 4)
%define ST_META_FEELING     (ST_SURPRISE_TYPE + 4)
%define ST_META_CONFIDENCE  (ST_META_FEELING + 4)
%define ST_SELF_PRED_REGION (ST_META_CONFIDENCE + 8)
%define ST_SELF_PRED_HITS   (ST_SELF_PRED_REGION + 8)
%define ST_SELF_PRED_MISSES (ST_SELF_PRED_HITS + 4)
%define ST_SCHEMA_HITS      (ST_SELF_PRED_MISSES + 4)
%define ST_SCHEMA_TOTAL     (ST_SCHEMA_HITS + 4)
%define ST_DISPATCH_MODE    (ST_SCHEMA_TOTAL + 4)

; Causal chain → expect → fire → successor → dream → dispatch mode prev chain
%define ST_CAUSAL_MOD_ADDR  (ST_DISPATCH_MODE + 4)
%define ST_CAUSAL_PRE_ACC   (ST_CAUSAL_MOD_ADDR + 8)
%define ST_CAUSAL_POST_ACC  (ST_CAUSAL_PRE_ACC + 4)
%define ST_CAUSAL_COUNT     (ST_CAUSAL_POST_ACC + 4)
%define ST_RECENT_EMITS     (ST_CAUSAL_COUNT + 4)
%define ST_EXPECT_REGION    (ST_RECENT_EMITS + 4)
%define ST_EXPECT_CONF      (ST_EXPECT_REGION + 8)
%define ST_EXPECT_TOKEN     (ST_EXPECT_CONF + 4)
%define ST_EXPECT_IS_SCHEMA (ST_EXPECT_TOKEN + 4)
%define ST_CURRENT_GOAL     (ST_EXPECT_IS_SCHEMA + 4)
%define ST_GOAL_STEP        (ST_CURRENT_GOAL + 4)
%define ST_ACCURACY_VARIANCE (ST_GOAL_STEP + 4)
%define ST_TRACE_CANDIDATES (ST_ACCURACY_VARIANCE + 4)
%define ST_TRACE_MATCHED    (ST_TRACE_CANDIDATES + 4)
%define ST_SUCCESSOR_TBL    (ST_TRACE_MATCHED + 4)
%define ST_LAST_FIRED_IDX   (ST_SUCCESSOR_TBL + 512)
%define ST_DREAM_MISS_THRESH (ST_LAST_FIRED_IDX + 4)
%define ST_PREV_INTRO_STATE (ST_DREAM_MISS_THRESH + 4)
%define ST_PREV_DISPATCH_MODE (ST_PREV_INTRO_STATE + 4)
%define ST_RECENT_CONDEMNS  (ST_PREV_DISPATCH_MODE + 4)

; Fire ring chain
%define ST_FIRE_RING_CAP    64
%define ST_FIRE_RING_ENTRY  16
%define ST_FIRE_RING        (ST_RECENT_CONDEMNS + 4)
%define ST_FIRE_RING_END    (ST_FIRE_RING + ST_FIRE_RING_CAP * ST_FIRE_RING_ENTRY)
%define ST_FIRE_POS         ST_FIRE_RING_END
%define ST_FIRE_COUNT       (ST_FIRE_POS + 8)

; Entry table chain
%define ST_ENTRY_TABLE_CAP  16
%define ST_ENTRY_TABLE      (ST_FIRE_COUNT + 8)
%define ST_ENTRY_TABLE_END  (ST_ENTRY_TABLE + ST_ENTRY_TABLE_CAP * 8)
%define ST_GRAPH_DEPTH      (ST_ENTRY_TABLE_END)
%define ST_GRAPH_VISITED    (ST_GRAPH_DEPTH + 4)
%define ST_VOCAB_COUNT      (ST_GRAPH_VISITED + 4)
%define ST_VOCAB_TOP_DIRTY  (ST_VOCAB_COUNT + 4)
%define ST_HOLO_PREDICT_SUM (ST_VOCAB_TOP_DIRTY + 4)
%define ST_HOLO_PREDICT_N   (ST_HOLO_PREDICT_SUM + 8)

; Anticipation chain
%define ST_ANTIC_ENTRY_SIZE 24
%define ST_ANTIC_CAP        16
%define ST_ANTIC_BUF        (ST_HOLO_PREDICT_N + 4)
%define ST_ANTIC_BUF_END    (ST_ANTIC_BUF + ST_ANTIC_CAP * ST_ANTIC_ENTRY_SIZE)
%define ST_ANTIC_FIRED      ST_ANTIC_BUF_END
%define ST_ANTIC_ACTIVE     (ST_ANTIC_FIRED + 4)

; Pressure chain
%define ST_MISS_PRESSURE    (ST_ANTIC_ACTIVE + 4)
%define ST_DREAM_PRESSURE   (ST_MISS_PRESSURE + 8)
%define ST_OBSERVE_PRESSURE (ST_DREAM_PRESSURE + 8)
%define ST_EVOLVE_PRESSURE  (ST_OBSERVE_PRESSURE + 8)

; Instance/colony chain
%define ST_INSTANCE_ID      (ST_EVOLVE_PRESSURE + 8)
%define ST_COLONY_SIZE      (ST_INSTANCE_ID + 8)
%define ST_SHARED_MODE      (ST_COLONY_SIZE + 4)
