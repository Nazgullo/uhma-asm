;;;; ============================================================================
;;;; UHMA FORWARD DECLARATIONS & PACKAGE DEFINITION
;;;; ============================================================================
;;;; This file implements the Robust Package Pattern from Bible v3.0.
;;;; It MUST be loaded FIRST before any other UHMA modules.
;;;; ============================================================================

(in-package :cl-user)

;;; --- SECTION 1: ROBUST PACKAGE DEFINITION ---
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ensure-package-with-exports (pkg-name symbol-names)
    "Create package if needed, ALWAYS ensure exports. Retry-safe."
    (let ((pkg (or (find-package pkg-name)
                   (make-package pkg-name :use '(:cl)))))
      (dolist (sym-name symbol-names)
        (export (intern (string sym-name) pkg) pkg))
      pkg))

  ;; Ensure the UHMA package exists with core exports
  (ensure-package-with-exports :uhma 
    '(#:reset! #:learn! #:generate #:process-text! #:process-file!
      #:execute-sexp #:vibe #:vsa-lookup #:vsa-explain-vector
      #:*step* #:*experts* #:*system-purpose* #:*vocation-fidelity*)))

(in-package :uhma)

;;; --- SECTION 2: OPTIMIZATION POLICY ---
(declaim (optimize (speed 2) (safety 2) (debug 1)))

;;; --- SECTION 3: GLOBAL VARIABLE DECLARATIONS (*earmuffs*) ---
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *EXPERT-COUNTER* 0)
  (defvar *TRAVELER-COUNTER* 0)
  (defvar *TYPE-COUNTER* 0)
  (defvar *STEP* 0)
  (defvar *CURRENT-EXPERT* nil)
  (defvar *CURRENT-CTX* nil)
  (defvar *SEXP-ENV* nil)
  (defvar *BLOCK-TAGS* nil)
  (defvar *EXPERT-RESTARTS* nil))

(defvar *EMB-DIM* 32)
(defvar *CONTEXT-SCALES* '(1 2 3 4 5 6 7 8 9 10))
(defvar *SCALE-WEIGHTS* '(0.5 0.3 0.2))
(defvar *NEIGHBORHOOD-MAX-SIZE* 8)
(defvar *NEIGHBORHOOD-MIN-SIMILARITY* 0.3)
(defvar *NEIGHBORHOOD-DIVERSITY-THRESHOLD* 0.4)
(defvar *INITIAL-EXPERTS* 5)
(defvar *CONSOLIDATION-AGE* 20)
(defvar *CONSOLIDATION-STRENGTH* 3)
(defvar *CONSOLIDATION-INTERVAL* 25)
(defvar *METABOLISM* 0.002)
(defvar *DEATH-THRESHOLD* 0.1)
(defvar *MERGE-SIMILARITY-THRESHOLD* 0.85)
(defvar *PATTERN-PRUNE-THRESHOLD* 2)
(defvar *PATTERN-DECAY-RATE* 0.99)
(defvar *PATTERN-DECAY-INTERVAL* 200)
(defvar *COMPRESSION-INTERVAL* 500)
(defvar *METRICS-WINDOW* 100)
(defvar *IMPROVEMENT-THRESHOLD* 0.05)
(defvar *MAX-CHANGE-HISTORY* 20)
(defvar *LAST-ACCURACY-SNAPSHOT* 0)
(defvar *LAST-CONSOLIDATION* 0)
(defvar *LAST-TRAVELER-PASS* 0)
(defvar *LAST-RECLUSTER* 0)
(defvar *TOTAL-BIRTHS* 0)
(defvar *TOTAL-DEATHS* 0)
(defvar *EXPERTS* nil)
(defvar *NEIGHBORHOODS* nil)
(defvar *ROOT-NEIGHBORHOOD* nil)
(defvar *HOOKS* (make-hash-table :test 'eq))
(defvar *CALL-STACK* nil)
(defvar *EXECUTION-TRACE* nil)
(defvar *EMBEDDINGS* (make-hash-table :test 'eq))
(defvar *CONTEXT-OWNERS* (make-hash-table :test 'equal))
(defvar *PATTERN-STATS* (make-hash-table :test 'equal))
(defvar *ATTENTION-MAP* (make-hash-table :test 'equal))
(defvar *CONTEXT-ENCOUNTERS* (make-hash-table :test 'equal))
(defvar *LAST-ANSWERING-EXPERT* nil)
(defvar *TRAVELERS* nil)
(defvar *VERIFICATIONS-SKIPPED* 0)
(defvar *VERIFICATIONS-PERFORMED* 0)
(defvar *ATTENTION-FORCED-VERIFICATIONS* 0)
(defvar *CONFIDENCE-HISTORY* nil)
(defvar *LOW-CONFIDENCE-COUNT* 0)
(defvar *HIGH-CONFIDENCE-COUNT* 0)
(defvar *LEARNING-RATE-WINDOW* nil)
(defvar *TOKEN-CONTEXTS* (make-hash-table :test 'eq))
(defvar *TYPE-CLUSTERS* (make-hash-table :test 'eq))
(defvar *TYPE-REGISTRY* (make-hash-table :test 'eq))
(defvar *TYPE-CONTEXT-WEIGHT* 0.5)
(defvar *TYPE-CLUSTER-THRESHOLD* 0.7)
(defvar *TYPE-RECLUSTER-INTERVAL* 100)
(defvar *CHUNK-BUFFER* nil)
(defvar *TYPE-BUFFER* nil)
(defvar *PROGRAM-MUTATION-RATE* 0.05)
(defvar *MORPHEME-MAX-LENGTH* 8)
(defvar *MORPHEME-MIN-LENGTH* 3)
(defvar *MORPHEME-LEARNING-RATE* 0.1)
(defvar *MORPHEME-MODEL* nil)
(defvar *CHUNK-EMBEDDING* nil)
(defvar *EXPERTS-MERGED* 0)
(defvar *EXTERNAL-OUTCOMES* nil)
(defvar *PATTERNS-PRUNED* 0)
(defvar *PHRASE-MODEL* nil)
(defvar *SENTENCE-MODEL* nil)
(defvar *PHRASE-MIN-FREQUENCY* 5)
(defvar *PHRASE-DETECTION-INTERVAL* 100)
(defvar *PHRASE-PMI-THRESHOLD* 2.0)
(defvar *CONTEXT-MIN-SCALE* 1)
(defvar *CONTEXT-MAX-SCALE* 10)
(defvar *CONTEXT-SCALE-EXPLORATION* 0.1)
(defvar *SENTENCE-END-TOKENS* '("." "!" "?"))
(defvar *CLAUSE-BOUNDARY-TOKENS* '(";" ":" ","))
(defvar *SENTENCE-BOUNDARY-CONTEXT-DECAY* 0.5)
(defvar *HIERARCHY-WEIGHTS* '(0.5 0.3 0.2))
(defvar *CONSOLIDATION-LOG* nil)
(defvar *LAST-LEARN-RESULT* nil)
(defvar *MIN-ENCOUNTERS-BEFORE-SPARSE* 3)
(defvar *ATTENTION-MISS-BOOST* 0.3)
(defvar *ATTENTION-HIT-DECAY* 0.1)
(defvar *TRAVELER-MAX-NEIGHBORHOODS* 5)
(defvar *TRAVELER-CREATION-INTERVAL* 50)
(defvar *MIN-TYPE-MEMBERS* 2)
(defvar *MORPHEME-SPLIT-BONUS* 0.5)
(defvar *MORPHEME-BOUNDARY-COST* 0.1)
(defvar *MAX-PROGRAM-LENGTH* 12)
(defvar *HIDDEN-STATE-DECAY* 0.85)
(defvar *HIDDEN-STATE-THRESHOLD* 0.3)
(defvar *CHUNK-SIMILARITY-THRESHOLD* 0.4)
(defvar *TRAVELER-LINK-CONFIDENCE* 0.8)
(defvar *LAST-DECAY* 0)
(defvar *SCHEMA-SPAWN-RATE* 0.1)
(defvar *CAUSAL-MODEL* (make-hash-table :test 'equal))
(defvar *COGNITIVE-SCHEMAS* (make-hash-table :test 'equal))
(defvar *MY-FUNCTIONS* (make-hash-table :test 'eq))
(defvar *PRESENCE* nil)
(defvar *CURRENT-GOAL* nil)
(defvar *GOAL-STACK* nil)
(defvar *MODIFICATION-LOG* nil)
(defvar *SCHEMA-ATTEMPTS* 0)
(defvar *SCHEMA-SUCCESSES* 0)
(defvar *GOAL-BEHAVIOR-PARAMS* nil)
(defvar *META-INVESTIGATION-QUEUE* nil)
(defvar *MODIFICATION-BUDGET* 10)
(defvar *SCHEMA-GUIDED-EXECUTION* nil)
(defvar *SYNTHESIZED-OPS* (make-hash-table :test 'equal))
(defvar +HOOK-POST-DELIBERATION+ 'post-deliberation)
(defvar *DRIVES* nil)
(defvar *CONSOLIDATED-MEMORY* (make-hash-table :test 'equal))
(defvar *CONSULTATION-STATS* (make-hash-table :test 'eq))
(defvar *CONTINUOUS-PHASE* :settling)
(defvar *INTEGRATION-STATS* (make-hash-table :test 'eq))
(defvar *HOLO* nil)
(defvar *ALGEBRAIC-SCHEMAS* (make-hash-table :test 'eq))
(defvar *LAST-ANSWERING-EXPERT* nil)
(defvar *DEEPER-MIND-ENABLED* t)
(defvar *CURRENT-SELF-EXPECTATION* nil)
(defvar *SYSTEM-PURPOSE* "Self-observation and pattern emergence.")
(defvar *VOCATION-FIDELITY* 1.0)
(defvar *HEAP-LIMIT-BYTES* (* 14 1024 1024 1024))
(defvar *HOOK-FIRE-COUNTS* (make-hash-table :test 'eq))
(defvar *VSA-SYMBOL-TO-VEC* (make-hash-table :test 'eq))
(defvar *VSA-VEC-TO-SYMBOL* nil)
(defvar *CAUSAL-NARRATIVES* nil)
(defvar *MODIFICATION-HISTORY* nil)
(defvar *HYPOTHESES* (make-hash-table :test 'equal))
(defvar *EPISODIC-MEMORY* nil)
(defvar +HOOK-POST-DRIVE-UPDATE+ 'post-drive-update)
(defvar *TYPE-REGISTRY* (make-hash-table :test 'eq))
(defvar *TYPE-CONTEXT-WEIGHT* 0.5)
(defvar *TYPE-CLUSTER-THRESHOLD* 0.7)
(defvar *TYPE-RECLUSTER-INTERVAL* 100)
(defvar *TRAVELER-BRIDGE-THRESHOLD* 0.6)
(defvar *TRAVELER-KNOWLEDGE-OVERLAP-THRESHOLD* 0.4)
(defvar *LONG-TERM-MEMORY* (make-hash-table :test 'equal))
(defvar *LAST-COMPRESSION* 0)
(defvar *ATTENTION-DECAY* 0.9)
(defvar *ATTENTION-WINDOW* 20)
(defvar *RECENT-OUTCOMES* nil)
(defvar *ATTENTION-FOCUS-THRESHOLD* 0.6)
(defvar *SELF-MODEL* nil)
(defvar *SPARSE-ENABLED* t)
(defvar *SPARSE-THRESHOLD* 0.8)
(defvar *TRAVELER-LINK-MAX* 50)
(defvar *CHUNK-WINDOW-SIZE* 10)
(defvar *TYPE-WINDOW-SIZE* 10)
(defvar *TYPE-PATTERN-LENGTH* 5)
(defvar *COGNITIVE-TRACE-BUFFER* (make-array 1000 :fill-pointer 0 :adjustable t))
(defvar *LAST-HYP-TEST-STEP* 0)
(defvar *GOAL-HISTORY* nil)
(defvar *GOALS-WIRED-FOR-COMPLETION* (make-hash-table :test 'equal))
(defvar *HYPOTHESES-SCHEMA-ATTEMPTED* (make-hash-table :test 'equal))
(defvar *ORIGINAL-LEARN-FROM-SCHEMA-EXECUTION* nil)
(defvar *LAST-EXECUTED-SCHEMA* nil)
(defvar *LAST-SCHEMA-STEP* 0)
(defvar *LAST-CONTEXT* nil)
(defvar *INTROSPECTIVE-CONCEPTS* (make-hash-table :test 'eq))
(defvar *CONCEPT-SOURCE-GROUNDING* (make-hash-table :test 'equal))
(defvar *SEMANTIC-SELF-KNOWLEDGE-CACHE* (make-hash-table :test 'equal))
(defvar *CURRENT-CODE-PATH* nil)
(defvar *RADICAL-EXPERIMENTATION-MODE* nil)
(defvar *ACTIVE-EXPERIMENTS* nil)
(defvar *OUTCOME-TRACKING-WINDOW* nil)
(defvar *SYNTHESIS-COOLDOWN* 100)

;;; --- SECTION 4: CORE STRUCTURES ---

(defstruct type-cluster
  "A semantic category of tokens with similar behavior."
  (id nil :type fixnum)
  (members nil :type list)
  (centroid nil :type (or null (simple-array single-float (*))))
  (access-count 0 :type fixnum)
  (context-distribution (make-hash-table :test 'equal))
  (prediction-contexts nil :type list)
  (created-at 0 :type fixnum))

(defstruct iterator
  "State tracking for code loop execution."
  (type :dotimes :type symbol)
  (var nil :type (or null symbol))
  (current 0 :type fixnum)
  (end 0 :type fixnum)
  (list nil :type list)
  (step nil))

(defstruct cognitive-trace
  "Record of a single cognitive moment."
  (step 0 :type fixnum)
  (timestamp (get-internal-real-time) :type fixnum)
  (context nil :type list)
  (prediction nil)
  (actual nil)
  (confidence 0.5 :type number)
  (surprise 0.0 :type single-float)
  (expert-id nil :type (or null fixnum))
  (meta-level 0 :type fixnum)
  (reasoning-path nil :type list)
  (meta-observations nil :type list)
  (alternatives nil :type list)
  (epistemic-state nil))

(defun cognitive-trace-correct (trace)
  "Computed accessor: was prediction correct?"
  (equal (cognitive-trace-prediction trace)
         (cognitive-trace-actual trace)))

(defstruct op-metrics
  "Performance tracking for an individual operation."
  (name nil :type symbol)
  (invocations 0 :type fixnum)
  (returns 0 :type fixnum)
  (continues 0 :type fixnum)
  (delegates 0 :type fixnum)
  (spawns 0 :type fixnum)
  (last-result-type nil :type (or null symbol))
  (avg-confidence 0.0 :type single-float)
  (total-confidence 0.0 :type single-float)
  (last-invoked 0 :type fixnum))

(defstruct traveler
  "A bridge between neighborhoods representing shared knowledge."
  (id (incf *traveler-counter*) :type fixnum)
  (expert-id nil :type (or null fixnum))
  (home-neighborhood nil :type symbol)
  (visited-neighborhoods nil :type list)
  (bridge-patterns nil :type list)
  (created-at 0 :type fixnum)
  (bridge-count 0 :type fixnum))

(defstruct consolidated-pattern
  "A high-confidence pattern committed to long-term memory."
  (key nil :type list)
  (prediction nil)
  (confidence 1.0 :type single-float)
  (access-count 0 :type fixnum)
  (consolidated-at 0 :type fixnum)
  (last-access 0 :type fixnum))

(defstruct op-result
  "The outcome of an operation execution."
  (type :continue :type symbol)
  (value nil)
  (confidence 0.5 :type single-float)
  (expert-id nil :type (or null fixnum))
  (source nil :type (or null symbol))
  (scale nil :type (or null fixnum))
  (target nil :type (or null fixnum)))

(defstruct expert
  "A competing program that specializes in specific contexts."
  (id (incf *expert-counter*) :type fixnum)
  (program nil :type list)            ; The homoiconic code
  (code-metrics (make-hash-table :test 'eq))
  (previous-program nil)              ; For rollback/evolution
  (previous-metrics nil)              ; For comparison
  (change-history nil)                ; Log of modifications
  (change-step 0 :type fixnum)        ; Last modification step
  ;; TRUE VSA: 1:1 Fidelity superposed knowledge
  (knowledge-vector (if (fboundp 'make-vsa-vec) (funcall 'make-vsa-vec) nil))
  (knowledge (make-hash-table :test 'equal)) ; Explicit lookup (fallback)
  (owned-type nil :type (or null fixnum))
  (owned-contexts nil :type list)
  (confidence-threshold 0.3 :type single-float)
  (life 1.0 :type single-float)
  (hits 0 :type fixnum)
  (misses 0 :type fixnum)
  (parent-id nil :type (or null fixnum))
  (birth-step 0 :type fixnum)
  (centroid nil :type (or null (simple-array single-float (*))))
  (neighborhood nil :type (or null symbol))
  ;; P19: Extended State
  (hidden-state nil)
  (hidden-state-strength 0.0 :type single-float)
  (type-pattern-memory nil)
  (traveler-links nil)
  (program-cost 0.0 :type single-float)
  (last-used 0 :type fixnum)
  (ownership 1.0 :type single-float))

(defstruct neighborhood
  "A fractal group of experts clustered by semantic similarity."
  (id (gensym "NBHD-") :type symbol)
  (parent nil :type (or null neighborhood))
  (children nil :type list)
  (experts nil :type list)            ; List of expert-ids
  (centroid nil :type (or null (simple-array single-float (*))))
  (type-signature nil)
  (depth 0 :type fixnum))

(defstruct morpheme-model
  "Sub-token segmentation model (Morfessor-style)."
  (boundaries (make-hash-table :test 'equal))
  (vocabulary (make-hash-table :test 'equal))
  (frequencies (make-hash-table :test 'equal))
  (known-affixes (make-hash-table :test 'equal))
  (total-words 0 :type fixnum)
  (corpus-chars 0 :type fixnum))

(defstruct code-modification
  "Record of a code change."
  (expert-id nil :type (or null fixnum symbol))
  (modification-type nil :type symbol)
  (target nil)
  (old-value nil)
  (new-value nil)
  (success nil :type boolean)
  (result :pending :type symbol)
  (accuracy-delta 0.0 :type single-float)
  (step 0 :type fixnum))

(defstruct synthesized-op
  "A newly created operation."
  (name nil :type symbol)
  (code nil :type list)
  (purpose nil :type string)
  (created-at 0 :type fixnum)
  (success-count 0 :type fixnum))

(defstruct expert-fitness
  "Composite fitness score for life/death decisions."
  (expert-id nil :type (or null fixnum symbol))
  (life 0.0 :type single-float)
  (accuracy 0.0 :type single-float)
  (program-cost 0.0 :type single-float)
  (prediction-accuracy 0.0 :type single-float)
  (context-coverage 0.0 :type single-float)
  (knowledge-density 0.0 :type single-float)
  (recency 0.0 :type single-float)
  (composite 0.0 :type single-float))

(defstruct schema-fitness
  "Fitness metrics for a schema."
  (schema-id nil :type symbol)
  (success-rate 0.0 :type single-float)
  (application-count 0 :type fixnum)
  (context-generality 0.0 :type single-float)
  (structure-efficiency 0.0 :type single-float)
  (composite 0.0 :type single-float))

(defstruct experience
  "Record of a past event for replay/learning."
  (context nil :type list)
  (prediction nil)
  (actual nil)
  (confidence 0.0 :type single-float)
  (correct-p nil :type boolean)
  (step 0 :type fixnum))

(defstruct episode
  "A bounded chunk of experience."
  (id (gensym "EP-") :type symbol)
  (start-step 0 :type fixnum)
  (end-step 0 :type fixnum)
  (trigger nil)
  (events nil :type list)
  (outcome nil)
  (drive-state-start nil :type list)
  (drive-state-end nil :type list)
  (confidence-arc nil :type list)
  (self-relevant-p nil :type boolean)
  (significance 0.0 :type single-float)
  (tags nil :type list)
  (causal-links nil :type list)
  (self-surprise-total 0.0 :type single-float)
  (outcome-surprise-total 0.0 :type single-float)
  (concepts-activated nil :type list)
  (narrative nil :type (or null string)))

(defstruct episode-event
  "A significant moment within an episode."
  (step 0 :type fixnum)
  (type nil :type symbol)
  (description nil :type (or null string))
  (self-expectation nil)
  (self-surprise 0.0 :type single-float)
  (data nil))

(defstruct (episodic-memory (:constructor make-episodic-memory)
                            (:constructor make-episodic-memory-struct))
  "The system's unified episodic store."
  (episodes (make-array 1000 :fill-pointer 0 :adjustable t))
  (current-episode nil :type (or null episode))
  (episode-count 0 :type fixnum)
  (max-episodes 1000 :type fixnum)
  (by-tag (make-hash-table :test 'eq))
  (by-significance (make-array 50 :fill-pointer 0 :adjustable t))
  (autobiographical nil :type list)
  (boundary-detector-state nil)
  (schemas (make-hash-table :test 'equal))
  (capacity 1000 :type fixnum))

(defstruct boundary-detector
  "State for episode segmentation."
  (last-context nil)
  (last-drive-state nil)
  (last-confidence 0.5 :type single-float)
  (surprise-accumulator 0.0 :type single-float)
  (steps-since-boundary 0 :type fixnum)
  (min-episode-length 10 :type fixnum)
  (max-episode-length 200 :type fixnum))

(defstruct replay-buffer
  (size 100)
  (items nil))

(defstruct drift-detector
  "Detects distribution drift in input data."
  (baseline-accuracy 0.5 :type single-float)
  (baseline-established nil :type boolean)
  (baseline-variance 0.01 :type single-float)
  (current-window nil :type list)
  (token-dist-baseline (make-hash-table :test 'eq) :type hash-table)
  (token-dist-current (make-hash-table :test 'eq) :type hash-table)
  (cusum-pos 0.0 :type single-float)
  (cusum-neg 0.0 :type single-float)
  (drift-history nil :type list)
  (window-count 0 :type fixnum)
  (threshold 5.0 :type single-float))

(defstruct self-model
  "The system's internal model of its own performance."
  (expected-error 0.5 :type single-float)
  (error-baseline 0.5 :type single-float)
  (recent-errors nil :type list)
  (error-window 50 :type fixnum)
  (global-confidence 0.5 :type single-float)
  ;; Adaptive thresholds
  (sparse-threshold-min 0.65 :type single-float)
  (sparse-threshold-max 0.95 :type single-float)
  (current-sparse-threshold 0.85 :type single-float)
  (uncertainty-low 0.3 :type single-float)
  (uncertainty-high 1.5 :type single-float)
  ;; Statistics
  (total-verified 0 :type fixnum)
  (total-skipped 0 :type fixnum)
  (threshold-history nil :type list)
  (behavior-patterns (make-hash-table :test 'equal)) ; ADDED MISSING SLOT
  ;; Program structure tracking
  (program-structure-stats (make-hash-table :test 'equal))
  (best-program-structures nil :type list)
  (param-performance (make-hash-table :test 'equal)))

(defstruct phrase-model
  "Frequency tracking for multi-gram structural atoms."
  (unigram-counts (make-hash-table :test 'eq) :type hash-table)
  (bigram-counts (make-hash-table :test 'equal) :type hash-table)
  (trigram-counts (make-hash-table :test 'equal) :type hash-table)
  (fourgram-counts (make-hash-table :test 'equal) :type hash-table)
  (total-tokens 0 :type fixnum)
  (phrases (make-hash-table :test 'equal) :type hash-table)
  (phrase-index (make-hash-table :test 'eq) :type hash-table)
  (last-detection-step 0 :type fixnum)
  (phrases-learned 0 :type fixnum))

(defstruct phrase-info
  "Metadata for a learned atomic phrase unit."
  (tokens nil :type list)
  (symbol nil :type symbol)
  (frequency 0 :type fixnum)
  (pmi 0.0 :type single-float)
  (created-at 0 :type fixnum)
  (last-seen 0 :type fixnum))

(defstruct context-scale-model
  "Tracking optimal context window sizes per signature."
  (optimal-scales (make-hash-table :test 'equal) :type hash-table)
  (scale-stats (make-hash-table :test 'equal) :type hash-table)
  (global-scale-hits (make-array 10 :initial-element 0) :type (simple-array fixnum (10)))
  (global-scale-total (make-array 10 :initial-element 0) :type (simple-array fixnum (10))))

(defstruct sentence-model
  "Temporal tracking of clausal and sentential boundaries."
  (current-tokens nil :type list)
  (sentence-position 0 :type fixnum)
  (sentence-starts (make-hash-table :test 'eq) :type hash-table)
  (total-sentences 0 :type fixnum)
  (avg-sentence-length 10.0 :type single-float)
  (prev-sentence-end-token nil)
  (prev-sentence-summary nil))

(defstruct hierarchical-votes
  (token-votes (make-hash-table :test 'eq))
  (phrase-votes (make-hash-table :test 'eq))
  (sentence-votes (make-hash-table :test 'eq))
  (combined (make-hash-table :test 'eq)))

(defstruct invented-hypothesis
  "A dynamically composed theory about internal system dynamics."
  (id (gensym "INV-HYP-") :type symbol)
  relation subject predicate context
  (confidence 0.5 :type single-float)
  (created-at 0 :type fixnum))

(defstruct goal
  "A teleological target for the system."
  (id (gensym "GOAL-") :type symbol)
  (type :intrinsic :type symbol)
  (description "" :type string)
  (priority 0.5 :type single-float)
  (status :active :type symbol)
  (drive-source nil :type symbol)
  (strategies nil :type list)
  (attempts nil :type list)
  (created-at 0 :type fixnum))

(defstruct drive
  "An internal homeostatic pressure."
  (name nil :type symbol)
  (current-level 0.0 :type single-float)
  (target-level 0.5 :type single-float)
  (increase-rate 0.01 :type single-float)
  (decrease-rate 0.1 :type single-float)
  (goal-generator nil))

;;; --- SECTION 5: FORWARD FUNCTION DECLARATIONS (ftype) ---
(declaim (ftype function get-heap-saturation))
(declaim (ftype function prune-oldest-episodes!))
(declaim (ftype function attribute-error-to-code!))
(declaim (ftype function targeted-modification!))
(declaim (ftype function logical-nexus-restructure!))
(declaim (ftype function modify-global-function!))
(declaim (ftype function synthesize-op!))
(declaim (ftype function introspect-expert))
(declaim (ftype function mutate-expert-program))
(declaim (ftype function program-crossover!))
(declaim (ftype function inherit-parameters!))
(declaim (ftype function update-system-purpose!))
(declaim (ftype function generate-teleological-goal))
(declaim (ftype function detect-existential-moments!))
(declaim (ftype function existential-moment-hook))
(declaim (ftype function execute-vibe))
(declaim (ftype function vsa-explain-vector))
(declaim (ftype function vsa-lookup))
(declaim (ftype function vsa-encode-rule))
(declaim (ftype function vsa-superpose!))
(declaim (ftype function vsa-unbind))
(declaim (ftype function vsa-bind))
(declaim (ftype function get-vsa-vec))
(declaim (ftype function make-vsa-vec))
(declaim (ftype function vsa-cleanup))
(declaim (ftype function vnorm!))
(declaim (ftype function cosim))
(declaim (ftype function context-embedding))
(declaim (ftype function find-expert-by-id))
(declaim (ftype function learn!))
(declaim (ftype function find-expert-by-id))
(declaim (ftype function find-neighborhood-by-id))
(declaim (ftype function split-neighborhood!))
(declaim (ftype function self-model-current-sparse-threshold))
(declaim (ftype function learn!)) ; Added missing declaim
(declaim (ftype function push-capped!))
(declaim (ftype function prune-old-hypotheses!))

;;; --- Holographic memory functions ---
(declaim (ftype function holo-enforce-capacity!))
(declaim (ftype function holo-remove!))
(declaim (ftype function holo-get-candidates))
(declaim (ftype function holo-promote!))
(declaim (ftype function holo-store-by-layer)) ; Added
(declaim (ftype function holo-store-patterns)) ; Added
(declaim (ftype function holo-store-total-retrieved)) ; Added
(declaim (ftype function holo-store-total-stored)) ; Added
(declaim (ftype function expert-best-prediction)) ; Added
(declaim (ftype function holographic-update-presence-awareness!)) ; Added
(declaim (ftype function holographic-retrieve-layer)) ; Added
(declaim (ftype function holographic-unified-query)) ; Added
(declaim (ftype function initialize-cognitive-control!))
(declaim (ftype function initialize-v65-agentic!))
(declaim (ftype function initialize-introspective-grounding!))
(declaim (ftype function initialize-compositional-reasoning!))
(declaim (ftype function initialize-episodic-memory!))
(declaim (ftype function initialize-deeper-mind!))
(declaim (ftype function initialize-v64-enhancements!))
(declaim (ftype function create-sentence-model))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +HOOK-SELF-EXPECTATION-CREATED+ 'self-expectation-created)
  (defconstant +HOOK-DREAM-END+ 'dream-end)
  (defconstant +HOOK-DREAM-START+ 'dream-start)
  (defconstant +HOOK-POST-CONCEPT-DETECTION+ 'post-concept-detection)
  (defconstant +HOOK-EPISODE-BOUNDARY+ 'episode-boundary)
  (defconstant +HOOK-POST-SEMANTIC-SELF-UPDATE+ 'post-semantic-self-update)
  (defconstant +HOOK-SELF-EXPECTATION-COMPARED+ 'self-expectation-compared)
  (defconstant +HOOK-EPISODE-STORED+ 'episode-stored)
  (defconstant +HOOK-PRE-HYPOTHESIS-TEST+ 'pre-hypothesis-test))

(defun push-capped! (item list &optional (limit 100))
  "Push item to list and cap length."
  (push item list)
  (when (> (length list) limit)
    (setf list (subseq list 0 limit)))
  list)

(defun prune-old-hypotheses! ()
  "Remove old hypotheses."
  (when (boundp '*hypotheses*)
    (let ((to-remove nil))
      (maphash (lambda (k v)
                 (declare (ignore v))
                 (push k to-remove))
               (symbol-value '*hypotheses*))
      (dolist (k (subseq to-remove 0 (min (length to-remove) 50)))
        (remhash k (symbol-value '*hypotheses*))))))

;;; ============================================================================
;;; END FORWARD DECLARATIONS
;;; ============================================================================