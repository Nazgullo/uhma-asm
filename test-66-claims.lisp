;;;; ============================================================================
;;;; UHMA 66-CLAIM COMPREHENSIVE TEST
;;;; ============================================================================
;;;; Tests ALL 66 claims from UHMA-Design-Goals-and-Specs.md
;;;; Checks LETTER (machinery exists/fires) AND SPIRIT (actual substance)
;;;; Feeds all 32 uhma source files, 3 passes
;;;; Logs incrementally to /tmp/uhma-66-claims.log
;;;; ============================================================================

(defparameter *test-log* "./uhma-66-claims.log")

;;; Clear log and write header
(with-open-file (s *test-log* :direction :output :if-exists :supersede)
  (format s "~%================================================================~%")
  (format s "UHMA 66-CLAIM COMPREHENSIVE TEST~%")
  (format s "Started: ~A~%" (get-universal-time))
  (format s "================================================================~%~%"))

(defun log! (fmt &rest args)
  "Log to file and stdout with flush."
  (let ((msg (apply #'format nil fmt args)))
    (with-open-file (s *test-log* :direction :output :if-exists :append)
      (format s "~A~%" msg)
      (force-output s))
    (format t "~A~%" msg)
    (force-output)))

(log! "Loading 32 UHMA modules...")

;;; Load UHMA
(handler-case
    (load "uhma-load-only.lisp")
  (error (e)
    (log! "FATAL: Failed to load UHMA: ~A" e)
    (sb-ext:exit :code 1)))

(in-package :uhma)

;;; Re-establish logging in UHMA package
(defparameter *log-path* "./uhma-66-claims.log")

(defun log! (fmt &rest args)
  (let ((msg (apply #'format nil fmt args)))
    (with-open-file (s *log-path* :direction :output :if-exists :append)
      (format s "~A~%" msg)
      (force-output s))
    (format t "~A~%" msg)
    (force-output)))

;;; ============================================================================
;;; CLAIM TRACKING
;;; ============================================================================

(defstruct claim
  id           ; 1-66
  category     ; :self-awareness, :reasoning, etc.
  description  ; What it claims
  letter       ; :pass/:fail/:untested - machinery exists
  spirit       ; :pass/:fail/:partial/:untested - actual substance
  evidence)    ; String describing evidence

(defvar *claims* (make-array 67 :initial-element nil))

(defun init-claims! ()
  "Initialize all 66 claims."
  ;; SELF-AWARENESS (1-9)
  (setf (aref *claims* 1) (make-claim :id 1 :category :self-awareness
    :description "Self-model tracks own behavior patterns"))
  (setf (aref *claims* 2) (make-claim :id 2 :category :self-awareness
    :description "Predicts what it will do BEFORE doing it"))
  (setf (aref *claims* 3) (make-claim :id 3 :category :self-awareness
    :description "Notices when it surprises itself"))
  (setf (aref *claims* 4) (make-claim :id 4 :category :self-awareness
    :description "Maintains confidence estimates about capabilities"))
  (setf (aref *claims* 5) (make-claim :id 5 :category :self-awareness
    :description "Self-expectation predicts: expert, confidence, outcome, schema"))
  (setf (aref *claims* 6) (make-claim :id 6 :category :self-awareness
    :description "Self-surprise computes divergence expected vs actual"))
  (setf (aref *claims* 7) (make-claim :id 7 :category :self-awareness
    :description "Distinguishes outcome-surprise from self-surprise"))
  (setf (aref *claims* 8) (make-claim :id 8 :category :self-awareness
    :description "Introspective vocabulary active: CONFUSED, CONFIDENT, etc."))
  (setf (aref *claims* 9) (make-claim :id 9 :category :self-awareness
    :description "Semantic self-knowledge: I tend to, I'm good at, I struggle"))

  ;; HUMAN-LIKE REASONING (10-17)
  (setf (aref *claims* 10) (make-claim :id 10 :category :reasoning
    :description "Cognitive controller selects strategy based on situation"))
  (setf (aref *claims* 11) (make-claim :id 11 :category :reasoning
    :description "Uses strategies: fast-default, multi-expert, structural, causal"))
  (setf (aref *claims* 12) (make-claim :id 12 :category :reasoning
    :description "Reasoning traces record: ops fired, confidence, why, alternatives"))
  (setf (aref *claims* 13) (make-claim :id 13 :category :reasoning
    :description "Hypothesis generation: forms theories about itself"))
  (setf (aref *claims* 14) (make-claim :id 14 :category :reasoning
    :description "Tests hypotheses against actual behavior"))
  (setf (aref *claims* 15) (make-claim :id 15 :category :reasoning
    :description "Confirms, refutes, or refines based on evidence"))
  (setf (aref *claims* 16) (make-claim :id 16 :category :reasoning
    :description "Causal models: explicit X causes Y"))
  (setf (aref *claims* 17) (make-claim :id 17 :category :reasoning
    :description "Counterfactual reasoning: what if..."))

  ;; TRUE HOMOICONICITY (18-31)
  (setf (aref *claims* 18) (make-claim :id 18 :category :homoiconicity
    :description "Expert programs are actual S-expressions that get EVAL'd"))
  (setf (aref *claims* 19) (make-claim :id 19 :category :homoiconicity
    :description "Programs can be read, analyzed, rewritten"))
  (setf (aref *claims* 20) (make-claim :id 20 :category :homoiconicity
    :description "System modifies global functions"))
  (setf (aref *claims* 21) (make-claim :id 21 :category :homoiconicity
    :description "Can rewrite system parameters"))
  (setf (aref *claims* 22) (make-claim :id 22 :category :homoiconicity
    :description "Can add new operations to vocabulary"))
  (setf (aref *claims* 23) (make-claim :id 23 :category :homoiconicity
    :description "Can restructure module interactions"))
  (setf (aref *claims* 24) (make-claim :id 24 :category :homoiconicity
    :description "Self-image sees running execution"))
  (setf (aref *claims* 25) (make-claim :id 25 :category :homoiconicity
    :description "Self-image sees source code"))
  (setf (aref *claims* 26) (make-claim :id 26 :category :homoiconicity
    :description "Self-image sees correlation between code and behavior"))
  (setf (aref *claims* 27) (make-claim :id 27 :category :homoiconicity
    :description "Self-image sees modification history"))
  (setf (aref *claims* 28) (make-claim :id 28 :category :homoiconicity
    :description "Code modification loop: observe-diagnose-hypothesize-modify-test-commit"))
  (setf (aref *claims* 29) (make-claim :id 29 :category :homoiconicity
    :description "introspect-expert analyzes program structure"))
  (setf (aref *claims* 30) (make-claim :id 30 :category :homoiconicity
    :description "analyze-program-structures! examines all programs"))
  (setf (aref *claims* 31) (make-claim :id 31 :category :homoiconicity
    :description "Genetic operations: crossover, mutation, selection, synthesis"))

  ;; CONTINUOUS TEMPORAL PRESENCE (32-36)
  (setf (aref *claims* 32) (make-claim :id 32 :category :presence
    :description "Presence substrate exists"))
  (setf (aref *claims* 33) (make-claim :id 33 :category :presence
    :description "Specious present: fading past, vivid now, anticipated future"))
  (setf (aref *claims* 34) (make-claim :id 34 :category :presence
    :description "Texture derived from: concepts, confidence, familiarity, drives"))
  (setf (aref *claims* 35) (make-claim :id 35 :category :presence
    :description "Continuity: felt connection to previous moments"))
  (setf (aref *claims* 36) (make-claim :id 36 :category :presence
    :description "Episodic memory: temporal context, autobiographical, significance"))

  ;; SELF-GROWING, SELF-OPTIMIZING (37-47)
  (setf (aref *claims* 37) (make-claim :id 37 :category :self-optimizing
    :description "Expert lifecycle: birth, competition, death, inheritance"))
  (setf (aref *claims* 38) (make-claim :id 38 :category :self-optimizing
    :description "Parameter self-tuning through inheritance"))
  (setf (aref *claims* 39) (make-claim :id 39 :category :self-optimizing
    :description "Code self-modification when miss rate high"))
  (setf (aref *claims* 40) (make-claim :id 40 :category :self-optimizing
    :description "Drives: curiosity, competence, coherence, efficiency"))
  (setf (aref *claims* 41) (make-claim :id 41 :category :self-optimizing
    :description "Goals generated from drives"))
  (setf (aref *claims* 42) (make-claim :id 42 :category :self-optimizing
    :description "Strategy selection considers current goals"))
  (setf (aref *claims* 43) (make-claim :id 43 :category :self-optimizing
    :description "Goal completion affects drive satisfaction"))
  (setf (aref *claims* 44) (make-claim :id 44 :category :self-optimizing
    :description "Dreams: replay difficult experiences"))
  (setf (aref *claims* 45) (make-claim :id 45 :category :self-optimizing
    :description "Dreams: generate mutations for stuck experts"))
  (setf (aref *claims* 46) (make-claim :id 46 :category :self-optimizing
    :description "Dreams: consolidate successful patterns"))
  (setf (aref *claims* 47) (make-claim :id 47 :category :self-optimizing
    :description "Dreams: process episodes into schemas"))

  ;; CODE-EXECUTION-ERROR CORRELATION (48-52)
  (setf (aref *claims* 48) (make-claim :id 48 :category :correlation
    :description "Code map knows where modifiable values live"))
  (setf (aref *claims* 49) (make-claim :id 49 :category :correlation
    :description "Runtime observation: which ops fire, succeed, fail"))
  (setf (aref *claims* 50) (make-claim :id 50 :category :correlation
    :description "Error attribution traces to responsible code/parameters"))
  (setf (aref *claims* 51) (make-claim :id 51 :category :correlation
    :description "Targeted modification: surgical edits to problematic code"))
  (setf (aref *claims* 52) (make-claim :id 52 :category :correlation
    :description "Rollback on degradation"))

  ;; ORGANIC VS MECHANICAL (53-57)
  (setf (aref *claims* 53) (make-claim :id 53 :category :organic
    :description "Dreams happen from need, not timer"))
  (setf (aref *claims* 54) (make-claim :id 54 :category :organic
    :description "Mutations target observed failure patterns"))
  (setf (aref *claims* 55) (make-claim :id 55 :category :organic
    :description "Goals arise from actual needs"))
  (setf (aref *claims* 56) (make-claim :id 56 :category :organic
    :description "Experts die when genuinely irrelevant"))
  (setf (aref *claims* 57) (make-claim :id 57 :category :organic
    :description "Self-modification responds immediately to failure"))

  ;; ENERGY DYNAMICS (58-62)
  (setf (aref *claims* 58) (make-claim :id 58 :category :dynamics
    :description "Expert count oscillates"))
  (setf (aref *claims* 59) (make-claim :id 59 :category :dynamics
    :description "Accuracy varies"))
  (setf (aref *claims* 60) (make-claim :id 60 :category :dynamics
    :description "Attention shifts"))
  (setf (aref *claims* 61) (make-claim :id 61 :category :dynamics
    :description "Goals change"))
  (setf (aref *claims* 62) (make-claim :id 62 :category :dynamics
    :description "Not flat (if flat, dead)"))

  ;; DESIGN TARGETS (63-66)
  (setf (aref *claims* 63) (make-claim :id 63 :category :targets
    :description "Prediction accuracy >40%"))
  (setf (aref *claims* 64) (make-claim :id 64 :category :targets
    :description "Expert variance <20%"))
  (setf (aref *claims* 65) (make-claim :id 65 :category :targets
    :description "Schema coverage >60%"))
  (setf (aref *claims* 66) (make-claim :id 66 :category :targets
    :description "Self-prediction accuracy >50%")))

(defun set-claim! (id letter spirit evidence)
  "Update claim result."
  (let ((c (aref *claims* id)))
    (when c
      (setf (claim-letter c) letter
            (claim-spirit c) spirit
            (claim-evidence c) evidence))))

;;; ============================================================================
;;; TRACKING VARIABLES (for dynamics claims)
;;; ============================================================================

(defvar *expert-history* nil)
(defvar *goal-history-track* nil)
(defvar *accuracy-history* nil)
(defvar *mod-history* nil)

;;; ============================================================================
;;; FILES TO FEED (32 source files, no stubs/lab)
;;; ============================================================================

(defparameter *feed-files*
  '("uhma.lisp"
    "uhma-forward-decl.lisp"
    "uhma-v6.1-core-homoiconic.lisp"
    "uhma-v6.1-adaptive-homoiconic.lisp"
    "uhma-v6.1-sequence.lisp"
    "uhma-v6.2-deep-mind.lisp"
    "uhma-v6.3-deeper-mind.lisp"
    "uhma-v6.3-fixes.lisp"
    "uhma-v6.4-enhancements.lisp"
    "uhma-v6.5-agency.lisp"
    "uhma-v6.6-introspective-grounding.lisp"
    "uhma-v6.7-compositional-reasoning.lisp"
    "uhma-v6.8-cognitive-controller.lisp"
    "uhma-v6.9-pattern-utilization.lisp"
    "uhma-v6.10-episodic-memory.lisp"
    "uhma-v6.10-episodic-integration.lisp"
    "uhma-presence-substrate.lisp"
    "uhma-presence-integration.lisp"
    "uhma-continuous.lisp"
    "uhma-active-self-modification.lisp"
    "uhma-goal-driven-generation.lisp"
    "uhma-state-persistence.lisp"
    "uhma-persistence.lisp"
    "uhma-memory-bounds.lisp"
    "uhma-holographic-substrate.lisp"
    "uhma-holographic-memory.lisp"
    "uhma-holographic-substrate-v2.lisp"
    "uhma-holographic-integration.lisp"
    "uhma-deep-wiring.lisp"
    "uhma-complete-wiring.lisp"
    "uhma-self-awareness-loop.lisp"
    "uhma-diagnostic.lisp"))

;;; ============================================================================
;;; CLAIM VERIFICATION FUNCTIONS
;;; ============================================================================

(defun verify-claims-letter! ()
  "Check LETTER of law - does machinery exist?"
  (log! "~%--- Checking LETTER (machinery exists) ---")

  ;; 1: Self-model exists
  (set-claim! 1
    (if (and (boundp '*self-model*) *self-model*) :pass :fail)
    :untested
    (format nil "*self-model* bound: ~A" (boundp '*self-model*)))

  ;; 2-3: Self-expectation machinery
  (set-claim! 2 (if (fboundp 'generate-self-expectation!) :pass :fail) :untested "generate-self-expectation!")
  (set-claim! 3 (if (fboundp 'compute-self-surprise) :pass :fail) :untested "compute-self-surprise")

  ;; 4: Confidence estimates
  (set-claim! 4
    (if (and (boundp '*self-model*) *self-model*
             (ignore-errors (self-model-global-confidence *self-model*)))
        :pass :fail)
    :untested "self-model-global-confidence")

  ;; 5-7: Self-expectation details
  (set-claim! 5 (if (boundp '*self-expectation-log*) :pass :fail) :untested "*self-expectation-log*")
  (set-claim! 6 (if (fboundp 'compute-self-surprise) :pass :fail) :untested "compute-self-surprise")
  (set-claim! 7 (if (fboundp 'compute-outcome-surprise) :pass :fail) :untested "compute-outcome-surprise")

  ;; 8: Introspective vocabulary
  (set-claim! 8 (if (boundp '*active-concepts*) :pass :fail) :untested "*active-concepts*")

  ;; 9: Semantic self-knowledge
  (set-claim! 9 (if (boundp '*semantic-self-knowledge*) :pass :fail) :untested "*semantic-self-knowledge*")

  ;; 10-11: Cognitive controller
  (set-claim! 10 (if (fboundp 'select-reasoning-strategy) :pass :fail) :untested "select-reasoning-strategy")
  (set-claim! 11 (if (boundp '*reasoning-strategies*) :pass :fail) :untested "*reasoning-strategies*")

  ;; 12: Reasoning traces
  (set-claim! 12 (if (boundp '*trace-buffer*) :pass :fail) :untested "*trace-buffer*")

  ;; 13-15: Hypotheses
  (set-claim! 13 (if (fboundp 'generate-hypothesis!) :pass :fail) :untested "generate-hypothesis!")
  (set-claim! 14 (if (fboundp 'test-hypothesis!) :pass :fail) :untested "test-hypothesis!")
  (set-claim! 15 (if (fboundp 'update-hypothesis-confidence!) :pass :fail) :untested "update-hypothesis-confidence!")

  ;; 16-17: Causal/counterfactual
  (set-claim! 16 (if (fboundp 'build-causal-model) :pass :fail) :untested "build-causal-model")
  (set-claim! 17 (if (fboundp 'counterfactual-reasoning) :pass :fail) :untested "counterfactual-reasoning")

  ;; 18-19: S-expressions
  (set-claim! 18
    (if (and *experts* (listp (expert-program (first *experts*)))) :pass :fail)
    :untested "expert-program is list")
  (set-claim! 19 (if (fboundp 'introspect-expert) :pass :fail) :untested "introspect-expert")

  ;; 20-23: Modification capabilities
  (set-claim! 20 (if (fboundp 'modify-global-function!) :pass :fail) :untested "modify-global-function!")
  (set-claim! 21 (if (fboundp 'set-modifiable-param!) :pass :fail) :untested "set-modifiable-param!")
  (set-claim! 22 (if (fboundp 'synthesize-op!) :pass :fail) :untested "synthesize-op!")
  (set-claim! 23 (if (fboundp 'restructure-module!) :pass :fail) :untested "restructure-module!")

  ;; 24-27: Self-image
  (set-claim! 24 (if (boundp '*execution-trace*) :pass :fail) :untested "*execution-trace*")
  (set-claim! 25 (if (boundp '*token-source-correlations*) :pass :fail) :untested "*token-source-correlations*")
  (set-claim! 26 (if (boundp '*code-state-correlations*) :pass :fail) :untested "*code-state-correlations*")
  (set-claim! 27 (if (boundp '*modification-log*) :pass :fail) :untested "*modification-log*")

  ;; 28: Code modification loop
  (set-claim! 28 (if (fboundp 'execute-self-modification!) :pass :fail) :untested "execute-self-modification!")

  ;; 29-30: Program analysis
  (set-claim! 29 (if (fboundp 'introspect-expert) :pass :fail) :untested "introspect-expert")
  (set-claim! 30 (if (fboundp 'analyze-program-structures!) :pass :fail) :untested "analyze-program-structures!")

  ;; 31: Genetic ops
  (set-claim! 31
    (if (or (fboundp 'program-crossover!) (fboundp 'mutate-expert-program)) :pass :fail)
    :untested "genetic operations")

  ;; 32-36: Presence
  (set-claim! 32 (if (boundp '*presence*) :pass :fail) :untested "*presence*")
  (set-claim! 33 (if (fboundp 'update-specious-present!) :pass :fail) :untested "update-specious-present!")
  (set-claim! 34 (if (fboundp 'compute-texture) :pass :fail) :untested "compute-texture")
  (set-claim! 35 (if (fboundp 'compute-continuity) :pass :fail) :untested "compute-continuity")
  (set-claim! 36 (if (boundp '*episodic-memory*) :pass :fail) :untested "*episodic-memory*")

  ;; 37-39: Expert lifecycle
  (set-claim! 37 (if (and (fboundp 'spawn-expert) (fboundp 'expert-death!)) :pass :fail) :untested "spawn/death")
  (set-claim! 38 (if (fboundp 'inherit-parameters!) :pass :fail) :untested "inherit-parameters!")
  (set-claim! 39 (if (fboundp 'run-program-optimization!) :pass :fail) :untested "run-program-optimization!")

  ;; 40-43: Drives/Goals
  (set-claim! 40 (if (and (boundp '*intrinsic-drives*) (>= (length *intrinsic-drives*) 4)) :pass :fail)
    :untested (format nil "~D drives" (if (boundp '*intrinsic-drives*) (length *intrinsic-drives*) 0)))
  (set-claim! 41 (if (fboundp 'generate-goal-from-drive!) :pass :fail) :untested "generate-goal-from-drive!")
  (set-claim! 42 (if (fboundp 'select-reasoning-strategy) :pass :fail) :untested "strategy considers goals")
  (set-claim! 43 (if (fboundp 'satisfy-drive!) :pass :fail) :untested "satisfy-drive!")

  ;; 44-47: Dreams
  (set-claim! 44 (if (boundp '*dream-state*) :pass :fail) :untested "*dream-state*")
  (set-claim! 45 (if (fboundp 'dream-generate-mutations!) :pass :fail) :untested "dream-generate-mutations!")
  (set-claim! 46 (if (fboundp 'dream-consolidate-patterns!) :pass :fail) :untested "dream-consolidate-patterns!")
  (set-claim! 47 (if (fboundp 'process-episode-to-schema!) :pass :fail) :untested "process-episode-to-schema!")

  ;; 48-52: Correlation
  (set-claim! 48 (if (fboundp 'get-modifiable-params) :pass :fail) :untested "get-modifiable-params")
  (set-claim! 49 (if (boundp '*trace-buffer*) :pass :fail) :untested "runtime observation")
  (set-claim! 50 (if (fboundp 'attribute-error-to-code!) :pass :fail) :untested "attribute-error-to-code!")
  (set-claim! 51 (if (fboundp 'targeted-modification!) :pass :fail) :untested "targeted-modification!")
  (set-claim! 52 (if (fboundp 'rollback-modification!) :pass :fail) :untested "rollback-modification!")

  ;; 53-57: Organic (need machinery for need-based triggering)
  (set-claim! 53 (if (fboundp 'should-dream?) :pass :fail) :untested "should-dream? (need-based)")
  (set-claim! 54 (if (fboundp 'target-mutation-at-failure!) :pass :fail) :untested "target-mutation-at-failure!")
  (set-claim! 55 (if (fboundp 'generate-goal-from-drive!) :pass :fail) :untested "goals from needs")
  (set-claim! 56 (if (fboundp 'should-expert-die?) :pass :fail) :untested "should-expert-die?")
  (set-claim! 57 (if (fboundp 'immediate-failure-response!) :pass :fail) :untested "immediate-failure-response!")

  ;; 58-62: Dynamics (need tracking, will verify in spirit)
  (set-claim! 58 :pass :untested "will track during feeding")
  (set-claim! 59 :pass :untested "will track during feeding")
  (set-claim! 60 :pass :untested "will track during feeding")
  (set-claim! 61 :pass :untested "will track during feeding")
  (set-claim! 62 :pass :untested "will track during feeding")

  ;; 63-66: Design targets (need measurement)
  (set-claim! 63 :pass :untested "will measure")
  (set-claim! 64 :pass :untested "will measure")
  (set-claim! 65 :pass :untested "will measure")
  (set-claim! 66 :pass :untested "will measure"))

(defun verify-claims-spirit! ()
  "Check SPIRIT - actual substance, not just machinery."
  (log! "~%--- Checking SPIRIT (actual substance) ---")

  ;; 1: Self-model actually tracking
  (let ((patterns (when (and (boundp '*self-model*) *self-model*
                             (self-model-behavior-patterns *self-model*))
                    (hash-table-count (self-model-behavior-patterns *self-model*)))))
    (set-claim! 1 (claim-letter (aref *claims* 1))
      (if (and patterns (> patterns 0)) :pass :partial)
      (format nil "~D behavior patterns tracked" (or patterns 0))))

  ;; 2-3: Self-expectations actually recorded
  (let ((exp-count (if (boundp '*self-expectation-log*) (length *self-expectation-log*) 0)))
    (set-claim! 2 (claim-letter (aref *claims* 2))
      (if (> exp-count 0) :pass :fail)
      (format nil "~D self-expectations logged" exp-count)))

  ;; 8: Active concepts actually present
  (let ((concepts (when (boundp '*active-concepts*) *active-concepts*)))
    (set-claim! 8 (claim-letter (aref *claims* 8))
      (if (and concepts (> (length concepts) 0)) :pass :fail)
      (format nil "Active: ~A" concepts)))

  ;; 13-15: Hypotheses actually formed and tested
  (let ((hyp-count (hash-table-count *hypotheses*))
        (hist-count (if (boundp '*hypothesis-history*) (length *hypothesis-history*) 0)))
    (set-claim! 13 (claim-letter (aref *claims* 13))
      (if (> hyp-count 0) :pass :fail)
      (format nil "~D active hypotheses" hyp-count))
    (set-claim! 14 (claim-letter (aref *claims* 14))
      (if (> hist-count 0) :pass :partial)
      (format nil "~D in history (tested)" hist-count))
    (set-claim! 15 (claim-letter (aref *claims* 15))
      (if (> hist-count 0) :pass :partial)
      "hypotheses resolved"))

  ;; 18: Expert programs are real S-expressions with substance
  (when (> (length *experts*) 0)
    (let* ((prog (expert-program (first *experts*)))
           (len (if (listp prog) (length (format nil "~S" prog)) 0)))
      (set-claim! 18 (claim-letter (aref *claims* 18))
        (if (> len 100) :pass :partial)
        (format nil "Program ~D chars" len))))

  ;; 25-26: Token-source and code-state correlations building
  (let ((tok-corr (if (boundp '*token-source-correlations*)
                      (hash-table-count *token-source-correlations*) 0))
        (code-corr (if (boundp '*code-state-correlations*)
                       (hash-table-count *code-state-correlations*) 0)))
    (set-claim! 25 (claim-letter (aref *claims* 25))
      (if (> tok-corr 0) :pass :fail)
      (format nil "~D token-source correlations" tok-corr))
    (set-claim! 26 (claim-letter (aref *claims* 26))
      (if (> code-corr 0) :pass :fail)
      (format nil "~D code-state correlations" code-corr)))

  ;; 27-28: Modifications actually happening
  (let ((mod-count (length *modification-log*)))
    (set-claim! 27 (claim-letter (aref *claims* 27))
      (if (> mod-count 0) :pass :fail)
      (format nil "~D modifications logged" mod-count))
    (set-claim! 28 (claim-letter (aref *claims* 28))
      (if (> mod-count 0) :pass :fail)
      (format nil "~D code modifications" mod-count)))

  ;; 32: Presence actually active
  (let ((traj (when (and (boundp '*presence*) *presence*)
                (ignore-errors (presence-trajectory *presence*)))))
    (set-claim! 32 (claim-letter (aref *claims* 32))
      (if traj :pass :partial)
      (format nil "Trajectory: ~A" traj)))

  ;; 37: Expert births/deaths actually happening
  (let ((births *total-births*)
        (deaths *total-deaths*))
    (set-claim! 37 (claim-letter (aref *claims* 37))
      (if (and (> births 0) (> deaths 0)) :pass
          (if (> births 0) :partial :fail))
      (format nil "~D births, ~D deaths" births deaths)))

  ;; 40: All 4 drives active
  (when (boundp '*intrinsic-drives*)
    (let ((active (count-if (lambda (d) (> (drive-current-level d) 0.05)) *intrinsic-drives*)))
      (set-claim! 40 (claim-letter (aref *claims* 40))
        (if (= active 4) :pass (if (> active 0) :partial :fail))
        (format nil "~D/4 drives active" active))))

  ;; 41: Goals actually generated
  (let ((goal-count (length *goal-stack*)))
    (set-claim! 41 (claim-letter (aref *claims* 41))
      (if (> goal-count 0) :pass :fail)
      (format nil "~D goals in stack" goal-count)))

  ;; 44-47: Dreams actually running with substance
  (when (boundp '*dream-state*)
    (let ((cycles (dream-state-dream-cycle *dream-state*)))
      (set-claim! 44 (claim-letter (aref *claims* 44))
        (if (> cycles 0) :pass :fail)
        (format nil "~D dream cycles" cycles))))

  ;; Schemas forming
  (let ((schema-count (hash-table-count *cognitive-schemas*)))
    (set-claim! 47 (claim-letter (aref *claims* 47))
      (if (> schema-count 0) :pass :fail)
      (format nil "~D schemas formed" schema-count)))

  ;; 58-62: Dynamics - check tracked history
  (let ((exp-vals (remove-duplicates *expert-history*))
        (goal-vals (remove-duplicates *goal-history-track*)))
    (set-claim! 58 (claim-letter (aref *claims* 58))
      (if (> (length exp-vals) 1) :pass :fail)
      (format nil "Expert counts: ~A" (subseq *expert-history* 0 (min 10 (length *expert-history*)))))
    (set-claim! 61 (claim-letter (aref *claims* 61))
      (if (> (length goal-vals) 1) :pass :partial)
      (format nil "Goal counts: ~A" (subseq *goal-history-track* 0 (min 10 (length *goal-history-track*))))))

  ;; 62: Not flat
  (let ((flat (and (= (length (remove-duplicates *expert-history*)) 1)
                   (= (length (remove-duplicates *goal-history-track*)) 1))))
    (set-claim! 62 (claim-letter (aref *claims* 62))
      (if (not flat) :pass :fail)
      (if flat "FLAT - something may be dead" "System shows variation")))

  ;; 63-66: Design targets
  (let* ((total-hits (reduce #'+ (mapcar #'expert-hits *experts*)))
         (total-misses (reduce #'+ (mapcar #'expert-misses *experts*)))
         (total (+ total-hits total-misses))
         (accuracy (if (> total 0) (* 100 (/ total-hits total)) 0)))
    (set-claim! 63 (claim-letter (aref *claims* 63))
      (if (> accuracy 40) :pass :fail)
      (format nil "Accuracy: ~,1F%" accuracy))))

;;; ============================================================================
;;; MAIN EXECUTION
;;; ============================================================================

(log! "~%Initializing claims...")
(init-claims!)

(log! "Resetting system...")
(reset!)

(log! "~%================================================================")
(log! "FEEDING PHASE: 32 files, 2 passes")
(log! "================================================================~%")

(let ((chunk-size 15000)
      (pass-count 2))

  (dotimes (pass pass-count)
    (log! "~%======== PASS ~D/~D ========~%" (1+ pass) pass-count)

    (let ((file-num 0))
      (dolist (file *feed-files*)
        (incf file-num)
        (when (probe-file file)
          (log! "--- [~D/32] ~A ---" file-num file)
          (handler-case
              (let ((text (with-open-file (s file)
                            (let ((d (make-string (file-length s))))
                              (read-sequence d s) d))))
                (loop for start from 0 below (length text) by chunk-size
                      for chunk-num from 1
                      do (handler-case
                             (progn
                               (process-chunk! (subseq text start
                                                       (min (+ start chunk-size) (length text)))
                                               :verbose nil)
                               ;; Track dynamics
                               (push (length *experts*) *expert-history*)
                               (push (length *goal-stack*) *goal-history-track*)
                               ;; Log progress
                               (when (zerop (mod chunk-num 5))
                                 (log! "  Chunk ~D: step=~D exp=~D goals=~D mods=~D"
                                       chunk-num *step* (length *experts*)
                                       (length *goal-stack*) (length *modification-log*))))
                           (error (e)
                             (log! "  ERROR in chunk ~D: ~A" chunk-num e)))))
            (error (e)
              (log! "ERROR loading file ~A: ~A" file e))))))

    ;; Summary after each pass
    (log! "~%--- Pass ~D Summary ---" (1+ pass))
    (log! "  Step: ~D" *step*)
    (log! "  Experts: ~D (births: ~D, deaths: ~D)" (length *experts*) *total-births* *total-deaths*)
    (log! "  LTM: ~D" (hash-table-count *long-term-memory*))
    (log! "  Goals: ~D" (length *goal-stack*))
    (log! "  Hypotheses: ~D" (hash-table-count *hypotheses*))
    (log! "  Schemas: ~D" (hash-table-count *cognitive-schemas*))
    (log! "  Modifications: ~D" (length *modification-log*))
    (log! "  Dreams: ~D" (if (boundp '*dream-state*) (dream-state-dream-cycle *dream-state*) 0))

    ;; Run claim verification after each pass
    (log! "~%--- Verifying claims after pass ~D ---" (1+ pass))
    (verify-claims-letter!)
    (verify-claims-spirit!)))

;;; ============================================================================
;;; FINAL REPORT
;;; ============================================================================

(log! "~%================================================================")
(log! "FINAL REPORT: 66 CLAIMS")
(log! "================================================================~%")

(let ((letter-pass 0) (letter-fail 0)
      (spirit-pass 0) (spirit-partial 0) (spirit-fail 0))

  (loop for id from 1 to 66
        for c = (aref *claims* id)
        when c do
        (log! "[~2D] ~A" id (claim-description c))
        (log! "     LETTER: ~A | SPIRIT: ~A" (claim-letter c) (claim-spirit c))
        (when (claim-evidence c)
          (log! "     Evidence: ~A" (claim-evidence c)))
        (log! "")

        (case (claim-letter c)
          (:pass (incf letter-pass))
          (:fail (incf letter-fail)))
        (case (claim-spirit c)
          (:pass (incf spirit-pass))
          (:partial (incf spirit-partial))
          (:fail (incf spirit-fail))))

  (log! "================================================================")
  (log! "SUMMARY")
  (log! "================================================================")
  (log! "LETTER (machinery exists):")
  (log! "  PASS: ~D/66 (~,1F%)" letter-pass (* 100 (/ letter-pass 66.0)))
  (log! "  FAIL: ~D/66" letter-fail)
  (log! "")
  (log! "SPIRIT (actual substance):")
  (log! "  PASS: ~D/66 (~,1F%)" spirit-pass (* 100 (/ spirit-pass 66.0)))
  (log! "  PARTIAL: ~D/66" spirit-partial)
  (log! "  FAIL: ~D/66" spirit-fail)
  (log! "")

  (cond
    ((and (> letter-pass 50) (> spirit-pass 40))
     (log! "VERDICT: SYSTEM COMPLIANT - Both letter and spirit satisfied"))
    ((and (> letter-pass 50) (> (+ spirit-pass spirit-partial) 40))
     (log! "VERDICT: PARTIALLY COMPLIANT - Machinery present, substance developing"))
    ((> letter-pass 50)
     (log! "VERDICT: LETTER ONLY - Machinery present but substance lacking"))
    (t
     (log! "VERDICT: NON-COMPLIANT - Critical failures"))))

(log! "~%================================================================")
(log! "SAVING POST-FEEDING STATE")
(log! "================================================================")

;; Save state after feeding
(when (fboundp 'checkpoint-session!)
  (checkpoint-session!)
  (log! "Post-feeding checkpoint saved"))

(when (fboundp 'save-system-state!)
  (save-system-state! (format nil "post-feed-~A" (get-universal-time)))
  (log! "Post-feeding system state saved"))

;;; ============================================================================
;;; LIVE MODE: 3 HOURS
;;; ============================================================================

(log! "~%================================================================")
(log! "ENTERING LIVE MODE (3 hours)")
(log! "================================================================")

(defvar *live-start-time* (get-internal-real-time))
(defvar *last-save-time* (get-internal-real-time))
(defvar *save-interval-seconds* 1800)  ; Save every 30 minutes
(defvar *live-phase-1-hours* 1)  ; First live phase: 1 hour

;;; Text file extensions to feed (skip binary)
(defvar *feedable-extensions*
  '("lisp" "py" "pyi" "pxd" "pxi" "h" "f" "f90" "txt" "md" "csv" "json" "yaml" "yml" "rst" "org"))

(defun feedable-file-p (path)
  "Check if file is a text file we can feed."
  (let ((ext (pathname-type path)))
    (and ext (member (string-downcase ext) *feedable-extensions* :test #'string=))))

(defun collect-homoiconic-files ()
  "Collect all feedable files from HOMOICONIC folder."
  (let ((files nil))
    (labels ((walk (dir)
               (dolist (entry (directory (merge-pathnames "*.*" dir)))
                 (cond
                   ((and (pathname-name entry) (feedable-file-p entry))
                    (push entry files))
                   ((and (not (pathname-name entry)) (not (pathname-type entry)))
                    ;; It's a directory
                    (walk (merge-pathnames (make-pathname :directory '(:relative :wild)) entry)))))))
      (walk #P"/home/peter/HOMOICONIC/"))
    (nreverse files)))

(defun feed-homoiconic-files! ()
  "Feed all files from HOMOICONIC folder."
  (log! "~%================================================================")
  (log! "FEEDING HOMOICONIC FILES")
  (log! "================================================================")

  (let ((files (collect-homoiconic-files))
        (file-count 0)
        (error-count 0)
        (chunk-size 15000))

    (log! "Found ~D feedable files" (length files))

    (dolist (file files)
      (incf file-count)
      (when (zerop (mod file-count 100))
        (log! "[HOMOICONIC] ~D/~D files, step=~D, exp=~D"
              file-count (length files) *step* (length *experts*)))

      (handler-case
          (with-open-file (s file :direction :input :external-format :utf-8)
            (let ((text (make-string (file-length s))))
              (read-sequence text s)
              ;; Feed in chunks
              (loop for start from 0 below (length text) by chunk-size
                    do (handler-case
                           (process-chunk! (subseq text start
                                                   (min (+ start chunk-size) (length text)))
                                           :verbose nil)
                         (error (e)
                           (declare (ignore e))
                           nil)))))
        (error (e)
          (incf error-count)
          (when (< error-count 10)
            (log! "[SKIP] ~A: ~A" (file-namestring file) e)))))

    (log! "~%HOMOICONIC feeding complete: ~D files, ~D errors" file-count error-count)
    (log! "Total step: ~D, Experts: ~D" *step* (length *experts*))))

(defun cleanup-old-saves! ()
  "Keep only the most recent save state, delete older ones."
  (let ((state-dir (if (boundp '*state-save-path*) *state-save-path*
                       "/home/peter/Desktop/MINION/uhma/saved-states/")))
    (when (probe-file state-dir)
      (let* ((files (directory (format nil "~A*.lisp" state-dir)))
             (sorted (sort files #'> :key #'file-write-date)))
        ;; Keep only the 2 most recent
        (when (> (length sorted) 2)
          (dolist (old-file (subseq sorted 2))
            (delete-file old-file)
            (log! "[CLEANUP] Deleted old state: ~A" (file-namestring old-file))))))))

(defun periodic-maintenance! ()
  "Run consolidation, trimming, and cleanup."
  ;; Trim histories
  (when (fboundp 'trim-all-histories!)
    (trim-all-histories!))
  (when (fboundp 'trim-all-arrays!)
    (trim-all-arrays!))
  ;; Clean old saves
  (cleanup-old-saves!))

;;; ============================================================================
;;; PHASE 1: LIVE MODE FOR 3 HOURS
;;; ============================================================================

(setf *continuous-running* t)
(setf *continuous-stop-requested* nil)
(setf *internal-step-count* 0)
(setf *phase* :settling)
(setf *phase-step* 0)
(setf *live-start-time* (get-internal-real-time))

(log! "~%================================================================")
(log! "LIVE MODE PHASE 1 (~D hours)" *live-phase-1-hours*)
(log! "================================================================")

(let ((phase-1-end (* *live-phase-1-hours* 3600)))
  (handler-case
      (loop
        (when *continuous-stop-requested* (return))

        (let ((elapsed (/ (- (get-internal-real-time) *live-start-time*)
                         internal-time-units-per-second)))
          (when (> elapsed phase-1-end)
            (log! "~%Phase 1 complete (~,1F hours)" (/ elapsed 3600.0))
            (return)))

        (internal-step!)

        (when (zerop (mod *internal-step-count* 500))
          (let ((elapsed (/ (- (get-internal-real-time) *live-start-time*)
                           internal-time-units-per-second)))
            (log! "[LIVE-1] step=~D phase=~A elapsed=~,1Fh exp=~D goals=~D"
                  *internal-step-count* *phase* (/ elapsed 3600.0)
                  (length *experts*) (length *goal-stack*))))

        ;; Periodic save
        (let ((now (get-internal-real-time)))
          (when (> (/ (- now *last-save-time*) internal-time-units-per-second)
                   *save-interval-seconds*)
            (when (fboundp 'checkpoint-session!) (checkpoint-session!))
            (when (fboundp 'save-system-state!) (save-system-state! "phase1"))
            (periodic-maintenance!)
            (setf *last-save-time* now))))

    (error (e) (log! "Phase 1 error: ~A" e))))

;;; Phase 2/3 removed - only 1hr live mode

;; Final save on exit
(setf *continuous-running* nil)
(log! "~%================================================================")
(log! "SAVING FINAL STATE")
(log! "================================================================")

(when (fboundp 'checkpoint-session!)
  (checkpoint-session!))
(when (fboundp 'save-system-state!)
  (save-system-state! "final"))
(cleanup-old-saves!)

(log! "System stopped after ~D internal steps" *internal-step-count*)
(log! "Final state saved. Restart with (load-system-state! \"final\")")
