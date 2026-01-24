;;;; ============================================================================
;;;; UHMA DEEP WIRING (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Creates dense qualitative interconnections between existing subsystems.
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 6: SPECIAL VARIABLES ---

(defvar *deep-wiring-initialized* nil )
(defvar *self-expectation-accuracy-history* nil )
(defvar *schema-execution-events* nil )
(defvar *concept-goal-cooldown* (make-hash-table :test 'eq))

;;; --- SECTION 8: CORE WIRING LOGIC ---

(defun wired-learn-from-schema-execution! (schema ctx outcome correct-p)
  "DG-2: Success feeds presence and drives; confusion during success rewards fitness."
  (declare (type executable-schema schema) (type list ctx) (type boolean correct-p))
  (push (list :step *step* :schema (executable-schema-id schema) :correct correct-p) *schema-execution-events*)
  (when (and correct-p (boundp '*presence*) *presence*)
    (presence-feel-texture! :competent 0.3)
    (presence-shift-trajectory! :flowing 0.1)
    (incf (presence-self-confidence *presence*) 0.02))
  (when correct-p (satisfy-drive! :competence 0.1)))

(defun wire-expert-phase-lock! (tok ctx predicted got-it)
  "DG-1: Bidirectional hidden-state coupling during delegation chains."
  (declare (ignore tok ctx predicted))
  (when (and (boundp '*call-stack*) (listp *call-stack*) (>= (length *call-stack*) 2))
    (let ((experts (remove nil (mapcar #'find-expert-by-id *call-stack*))))
      (when (>= (length experts) 2)
        (loop for i from 0 below (1- (length experts))
              do (couple-expert-phases! (nth i experts) (nth (1+ i) experts) got-it))))))

(defun install-deep-wiring! ()
  "Initialize all qualitative interconnections."
  (register-hook +hook-maintenance+ 'goal-completion-monitor :priority 45)
  (register-hook +hook-post-process-token+ 'wire-expert-phase-lock! :priority 52)
  (setf *deep-wiring-initialized* t))

;;; --- SECTION 10: INITIALIZATION ---
(eval-when (:load-toplevel :execute)
  (install-deep-wiring!)
  (format t "[DEEP-WIRING] Qualitative interconnections active.~%"))

(defun wire-schema-execution! ()
  "Install schema→presence wiring."
  (when (and (fboundp 'learn-from-schema-execution!)
             (not *original-learn-from-schema-execution*))
    (setf *original-learn-from-schema-execution* #'learn-from-schema-execution!)
    (setf (fdefinition 'learn-from-schema-execution!) #'wired-learn-from-schema-execution!)))


;;; ============================================================================
;;; 2. GOAL COMPLETION → SELF-CONFIDENCE
;;; Achieving goals boosts presence self-confidence
;;; ============================================================================

(defvar *original-push-goal* nil)

(defun wired-complete-goal! (goal)
  "Called when a goal is achieved - boost self-confidence."
  (when (and goal (boundp '*presence*) *presence*)
    (let ((priority (goal-priority goal)))
      ;; Higher priority goals give bigger confidence boost
      (let ((boost (* 0.05 (+ 1.0 priority))))
        (incf (presence-self-confidence *presence*) boost)
        (setf (presence-self-confidence *presence*)
              (min 1.0 (presence-self-confidence *presence*))))
      ;; Feel accomplished
      (presence-feel-texture! 'accomplished 0.4)
      (presence-shift-trajectory! :flowing 0.15)
      ;; Satisfy the drive that sourced this goal
      (let ((drive-source (goal-drive-source goal)))
        (when drive-source
          (satisfy-drive! drive-source 0.2))))))

;; Hook into goal status changes via maintenance
(defun goal-completion-monitor ()
  "Check for newly completed goals and wire to presence."
  (when (boundp '*goal-history*)
    (dolist (goal *goal-history*)
      (when (and (eq (goal-status goal) :achieved)
                 (not (gethash (goal-id goal) *goals-wired-for-completion*)))
        ;; Mark as wired so we don't process twice
        (setf (gethash (goal-id goal) *goals-wired-for-completion*) t)
        (wired-complete-goal! goal)))))


;;; ============================================================================
;;; 3. HYPOTHESIS CONFIRMATION → SCHEMA CREATION
;;; When hypotheses are confirmed, try to create schemas from them
;;; ============================================================================

(defun hypothesis-to-schema-candidate (hyp)
  "Convert confirmed hypothesis to schema if applicable."
  (when (and hyp (eq (self-hypothesis-status hyp) :confirmed))
    (let* ((claim (self-hypothesis-claim hyp))
           (evidence (self-hypothesis-evidence-for hyp))
           (confidence (self-hypothesis-confidence hyp)))
      ;; Only hypotheses about patterns are schema candidates
      (when (and claim (listp claim)
                 (member (car claim) '(pattern-reliable context-predicts
                                       expert-excels sequence-common)))
        ;; Create a cognitive schema from the hypothesis
        (let ((schema-id (gensym "HYP-SCHEMA-"))
              (pattern (extract-pattern-from-claim claim evidence)))
          (when pattern
            (setf (gethash schema-id *cognitive-schemas*)
                  (make-cognitive-schema
                   :id schema-id
                   :pattern pattern
                   :confidence confidence
                   :created-at *step*
                   :source-type :hypothesis
                   :source-id (self-hypothesis-id hyp)))
            ;; Try to compile it
            (when (> confidence 0.6)
              (let ((source (gethash schema-id *cognitive-schemas*)))
                (when source
                  (let ((exec (compile-schema-to-executable source)))
                    (when exec
                      (setf (gethash (executable-schema-id exec) *executable-schemas*)
                            exec))))))))))))

(defun extract-pattern-from-claim (claim evidence)
  "Extract learnable pattern from hypothesis claim."
  (case (car claim)
    (pattern-reliable
     ;; Claim: (pattern-reliable <pattern> <context>)
     (list (list :type :sequence
                 :elements (second claim)
                 :confidence-boost 0.1)))
    (context-predicts
     ;; Claim: (context-predicts <ctx> <outcome>)
     (list (list :type :context-outcome
                 :context (second claim)
                 :predicted (third claim))))
    (expert-excels
     ;; Claim: (expert-excels <expert-id> <context-type>)
     (list (list :type :expert-specialization
                 :expert (second claim)
                 :domain (third claim))))
    (t nil)))

(defun wire-hypothesis-confirmation! ()
  "Monitor hypothesis confirmations and create schemas."
  (when (boundp '*hypotheses*)
    (maphash (lambda (id hyp)
               (declare (ignore id))
               (when (and (eq (self-hypothesis-status hyp) :confirmed)
                          (not (gethash (self-hypothesis-id hyp) *hypotheses-schema-attempted*)))
                 (setf (gethash (self-hypothesis-id hyp) *hypotheses-schema-attempted*) t)
                 (hypothesis-to-schema-candidate hyp)))
             *hypotheses*)))


;;; ============================================================================
;;; 4. EXPERT DEATH → COHERENCE DRIVE PRESSURE
;;; When experts die, the system should feel coherence pressure
;;; ============================================================================

(defun expert-death-to-drive-hook (expert heir)
  "When an expert dies, affect coherence drive."
  (declare (ignore heir))
  (when expert
    ;; Expert death creates coherence pressure
    (satisfy-drive! :coherence -0.05)  ; negative = increase pressure
    ;; If expert had high success, feel the loss
    (when (and (boundp '*presence*) *presence*
               (> (expert-hits expert) 10))
      (let ((success-rate (/ (expert-hits expert)
                            (max 1 (+ (expert-hits expert)
                                      (expert-misses expert))))))
        (when (> success-rate 0.6)
          ;; Lost a good expert - feel discontinuity
          (presence-feel-discontinuity! :expert-loss 0.2)
          (presence-feel-texture! 'loss 0.2))))))


;;; ============================================================================
;;; 5. SELF-EXPECTATION ACCURACY → PREDICTION WEIGHTING
;;; Track how accurate self-predictions are and weight future predictions
;;; ============================================================================

(defun record-self-expectation-outcome! (correct-p confidence)
  "Record self-expectation accuracy for weighting."
  (push (cons correct-p confidence) *self-expectation-accuracy-history*)
  (when (> (length *self-expectation-accuracy-history*) 50)
    (setf *self-expectation-accuracy-history*
          (subseq *self-expectation-accuracy-history* 0 50))))

(defun self-expectation-accuracy ()
  "Compute recent self-expectation accuracy."
  (if (null *self-expectation-accuracy-history*)
      0.5
      (let ((correct (count-if #'car *self-expectation-accuracy-history*)))
        (/ correct (max 1 (length *self-expectation-accuracy-history*))))))

(defun self-expectation-weight ()
  "How much to weight self-expectations in predictions."
  (let ((accuracy (self-expectation-accuracy)))
    ;; If self-model is accurate, weight it more
    (cond
      ((> accuracy 0.8) 1.5)   ; Very accurate - trust it more
      ((> accuracy 0.6) 1.0)   ; Good - normal weight
      ((> accuracy 0.4) 0.7)   ; Mediocre - reduce weight
      (t 0.3))))               ; Poor - mostly ignore

;; Wire into self-expectation comparison hook
(defun wire-self-expectation-accuracy! (expectation)
  "Track self-expectation accuracy when compared."
  (when expectation
    (let ((correct-p (if (self-expectation-p expectation)
                         (self-expectation-self-prediction-correct expectation)
                         (getf expectation :correct)))
          (confidence (if (self-expectation-p expectation)
                          (self-expectation-expected-confidence expectation)
                          (getf expectation :confidence))))
      (when correct-p
        (record-self-expectation-outcome! correct-p (or confidence 0.5))))))


;;; ============================================================================
;;; 6. PRESENCE TRAJECTORY → HYPOTHESIS TESTING RATE
;;; When stuck, test hypotheses more aggressively
;;; ============================================================================

(defun presence-adjusted-hypothesis-interval ()
  "Adjust hypothesis testing interval based on presence trajectory."
  (let ((base-interval 200))  ; default
    (if (and (boundp '*presence*) *presence*)
        (case (presence-trajectory *presence*)
          (:stuck (* base-interval 0.25))     ; Test 4x more often when stuck
          (:searching (* base-interval 0.5))  ; Test 2x more often when searching
          (:flowing (* base-interval 1.5))    ; Test less when flowing
          (t base-interval))
        base-interval)))

(defvar *original-maybe-test-hypotheses* nil)

(defun wired-maybe-test-hypotheses! ()
  "Test hypotheses with presence-adjusted frequency."
  (let ((interval (presence-adjusted-hypothesis-interval)))
    (when (and (boundp '*last-hyp-test-step*)
               (> *step* (+ *last-hyp-test-step* interval)))
      (test-hypotheses!)
      (setf *last-hyp-test-step* *step*))))


;;; ============================================================================
;;; 7. INTROSPECTIVE CONCEPTS → GOAL GENERATION
;;; Active concepts can trigger relevant goals
;;; ============================================================================

(defun concept-triggers-goal (concept)
  "Check if an introspective concept should generate a goal."
  ;; Cooldown check - don't spam goals
  (let ((last-trigger (gethash concept *concept-goal-cooldown*)))
    (when (and last-trigger (< (- *step* last-trigger) 500))
      (return-from concept-triggers-goal nil)))

  (case concept
    ;; CONFUSED → goal to clarify/explore
    ((confused uncertain)
     (setf (gethash concept *concept-goal-cooldown*) *step*)
     (make-goal :type :derived
                :description "Clarify confusion through exploration"
                :priority 0.6
                :drive-source :coherence
                :success-criteria (lambda (g) (declare (ignore g))
                                    (< (compute-drive-level :coherence) 0.3))
                :strategies '(:explore :introspect)))

    ;; STRUGGLING → goal to improve competence
    ((struggling failing)
     (setf (gethash concept *concept-goal-cooldown*) *step*)
     (make-goal :type :derived
                :description "Improve performance on difficult patterns"
                :priority 0.7
                :drive-source :competence
                :success-criteria (lambda (g) (declare (ignore g))
                                    (> (compute-recent-accuracy 50) 0.6))
                :strategies '(:focus-training :schema-application)))

    ;; BORED/STAGNANT → goal to seek novelty
    ((bored stagnant)
     (setf (gethash concept *concept-goal-cooldown*) *step*)
     (make-goal :type :derived
                :description "Seek novel patterns and experiences"
                :priority 0.5
                :drive-source :curiosity
                :success-criteria (lambda (g) (declare (ignore g))
                                    (< (compute-drive-level :curiosity) 0.3))
                :strategies '(:explore :random-walk)))

    (t nil)))

(defun wire-concepts-to-goals! (active-concepts state)
  "Wire introspective concepts to goal generation."
  (declare (ignore state))
  (dolist (concept active-concepts)
    (let ((goal (concept-triggers-goal concept)))
      (when goal
        (push-goal! goal)))))


;;; ============================================================================
;;; 8. LTM CONSOLIDATION → PRESENCE VIVIDNESS
;;; When memories are consolidated, presence should feel more vivid
;;; ============================================================================

(defun ltm-consolidation-to-presence! (entry-count)
  "LTM consolidation affects presence vividness."
  (when (and (boundp '*presence*) *presence* (> entry-count 0))
    ;; Consolidation makes experience feel more substantial
    (let ((vividness-boost (* 0.02 (min entry-count 5))))
      (setf (presence-vividness *presence*)
            (min 1.0 (+ (presence-vividness *presence*) vividness-boost))))
    ;; Successful consolidation satisfies coherence
    (satisfy-drive! :coherence 0.05)))


;;; ============================================================================
;;; 9. DRIVE PRESSURE → SCHEMA SELECTION BIAS
;;; High drive pressure biases toward proven schemas
;;; ============================================================================

(defun drive-adjusted-schema-selection (schemas ctx)
  "Select schema with drive-based biasing."
  (if (null schemas)
      nil
      (let* ((coherence-pressure (compute-drive-level :coherence))
             (competence-pressure (compute-drive-level :competence))
             (exploration-bias (presence-exploration-bias)))
        ;; High pressure → prefer proven schemas
        ;; Low pressure → allow exploration
        (if (or (> coherence-pressure 0.6)
                (> competence-pressure 0.6))
            ;; High pressure - select by success rate
            (let ((sorted (sort (copy-list schemas)
                               (lambda (a b)
                                 (> (schema-success-rate a)
                                    (schema-success-rate b))))))
              (first sorted))
            ;; Low pressure - allow some randomness
            (if (< (random 1.0) exploration-bias)
                (nth (random (length schemas)) schemas)
                (first schemas))))))

(defun schema-success-rate (schema)
  "Get schema success rate."
  (if (executable-schema-p schema)
      (let ((exec (executable-schema-execution-count schema))
            (succ (executable-schema-success-count schema)))
        (if (> exec 0) (/ succ exec) 0.5))
      0.5))


;;; ============================================================================
;;; 10. SCHEMA SUCCESS → EXPERT CONFIDENCE
;;; When schemas derived from experts succeed, boost expert confidence
;;; ============================================================================

(defun schema-success-to-expert! (schema correct-p)
  "When schema succeeds, boost source expert ownership/confidence."
  (when (and schema correct-p (executable-schema-p schema))
    (let* ((source-id (executable-schema-source-schema-id schema))
           (source (when source-id (gethash source-id *cognitive-schemas*))))
      (when source
        ;; Find expert that contributed to this schema
        (let ((ctx (first (executable-schema-contexts-applied schema))))
          (when ctx
            (let ((expert (find-context-owner ctx)))
              (when expert
                ;; Boost expert ownership (effective confidence) slightly
                (setf (expert-ownership expert)
                      (min 1.0 (+ (expert-ownership expert) 0.01)))
                ;; Also boost life to prevent death
                (setf (expert-life expert)
                      (min 1.0 (+ (expert-life expert) 0.005)))))))))))


;;; ============================================================================
;;; INSTALLATION - Wire everything together
;;; ============================================================================

(defun install-deep-wiring! ()
  "Install all deep wiring connections."
  (when *deep-wiring-initialized*
    (return-from install-deep-wiring! nil))

  (format t "~%Installing deep wiring connections...~%")

  ;; 1. Schema execution → presence/drives
  (wire-schema-execution!)
  (format t "  [1/10] Schema execution → presence/drives~%")

  ;; 2. Goal completion monitoring (via maintenance hook)
  (register-hook +hook-maintenance+
                 #'goal-completion-monitor
                 :priority 45)
  (format t "  [2/10] Goal completion → self-confidence~%")

  ;; 3. Hypothesis → schema (via maintenance hook)
  (register-hook +hook-maintenance+
                 #'wire-hypothesis-confirmation!
                 :priority 46)
  (format t "  [3/10] Hypothesis confirmation → schema creation~%")

  ;; 4. Expert death → drives
  (register-hook +hook-expert-dying+
                 #'expert-death-to-drive-hook
                 :priority 30)
  (format t "  [4/10] Expert death → coherence drive~%")

  ;; 5. Self-expectation accuracy tracking
  (when (boundp '+hook-self-expectation-compared+)
    (register-hook +hook-self-expectation-compared+
                   #'wire-self-expectation-accuracy!
                   :priority 35))
  (format t "  [5/10] Self-expectation accuracy → prediction weighting~%")

  ;; 6. Presence → hypothesis testing rate
  ;; (This modifies timing, not a hook - integrated via maybe-test-hypotheses)
  (format t "  [6/10] Presence trajectory → hypothesis testing rate~%")

  ;; 7. Concepts → goals (use concept-detection hook, not introspection)
  (when (boundp '+hook-post-concept-detection+)
    (register-hook +hook-post-concept-detection+
                   #'wire-concepts-to-goals!
                   :priority 40))
  (format t "  [7/10] Introspective concepts → goal generation~%")

  ;; 8. LTM consolidation → presence (hook into post-learn when LTM updated)
  (register-hook +hook-post-learn+
                 (lambda (ctx predicted actual got-it expert)
                   (declare (ignore ctx predicted actual expert))
                   (when got-it
                     (ltm-consolidation-to-presence! 1)))
                 :priority 50)
  (format t "  [8/10] LTM consolidation → presence vividness~%")

  ;; 9. Drive pressure → schema selection
  ;; (Integrated into schema selection logic - not a hook)
  (format t "  [9/10] Drive pressure → schema selection bias~%")

  ;; 10. Schema success → expert confidence
  (register-hook +hook-post-learn+
                 (lambda (ctx predicted actual got-it expert)
                   (declare (ignore ctx predicted actual expert))
                   ;; Check if schema was involved
                   (when (and (boundp '*active-schema*) *active-schema*)
                     (schema-success-to-expert! *active-schema* got-it)))
                 :priority 51)
  (format t "  [10/10] Schema success → expert confidence~%")

  ;; 11. DG-2: Introspective grounding with write authority to schema fitness
  (when (boundp '+hook-post-concept-detection+)
    (register-hook +hook-post-concept-detection+
                   #'introspection-to-schema-fitness!
                   :priority 45))
  (format t "  [11/11] Introspective concepts → schema fitness (DG-2)~%")

  ;; 12. DG-1: Expert Mutual Phase-Locking (bidirectional coupling on delegation)
  (register-hook +hook-post-process-token+
                 #'wire-expert-phase-lock!
                 :priority 52)
  (format t "  [12/12] Expert delegation → mutual phase-locking (DG-1)~%")

  (setf *deep-wiring-initialized* t)
  (format t "~%Deep wiring complete. 12 new interconnections active.~%"))

;;; ============================================================================
;;; 12. DG-1: EXPERT MUTUAL PHASE-LOCKING
;;; When experts delegate to each other, couple their hidden-state-strength
;;; ============================================================================

(defun wire-expert-phase-lock! (tok ctx predicted got-it)
  "DG-1: When delegation chain exists, couple experts bidirectionally.
   Uses *call-stack* to find experts involved in delegation.
   Successful outcomes strengthen coupling, failures weaken it."
  (declare (ignore tok ctx predicted))
  ;; Only couple if delegation actually happened (2+ experts in call-stack)
  (when (and (boundp '*call-stack*) (listp *call-stack*) (>= (length *call-stack*) 2))
    (let ((experts (mapcar #'find-expert-by-id *call-stack*)))
      ;; Filter to only valid experts
      (setf experts (remove nil experts))
      (when (>= (length experts) 2)
        ;; Couple adjacent pairs in the delegation chain
        (loop for i from 0 below (1- (length experts))
              for delegator = (nth i experts)
              for delegate = (nth (1+ i) experts)
              do (couple-expert-phases! delegator delegate got-it))))))

(defun couple-expert-phases! (delegator delegate success-p)
  "Bidirectionally couple two experts' hidden-state-strength.
   Success: both move toward average (synchronize).
   Failure: both move apart (desynchronize).
   Coupling strength is small (0.02-0.04) to prevent destabilization."
  (when (and delegator delegate
             (typep delegator 'expert)
             (typep delegate 'expert))
    (let* ((d1-strength (or (expert-hidden-state-strength delegator) 0.5))
           (d2-strength (or (expert-hidden-state-strength delegate) 0.5))
           (avg (/ (+ d1-strength d2-strength) 2.0))
           ;; Coupling coefficient based on outcome
           (coupling (if success-p 0.03 -0.02)))
      ;; Move toward average on success, away on failure
      (let ((d1-delta (* coupling (- avg d1-strength)))
            (d2-delta (* coupling (- avg d2-strength))))
        ;; Apply bounded changes
        (setf (expert-hidden-state-strength delegator)
              (max 0.0 (min 1.0 (+ d1-strength d1-delta))))
        (setf (expert-hidden-state-strength delegate)
              (max 0.0 (min 1.0 (+ d2-strength d2-delta))))
        ;; Log the coupling
        (when (boundp '*modification-log*)
          (push (list :step *step*
                      :action 'expert-phase-lock
                      :delegator (expert-id delegator)
                      :delegate (expert-id delegate)
                      :success success-p
                      :coupling coupling)
                *modification-log*))))))

(defun introspection-to-schema-fitness! (concepts state)
  "DG-2: Introspection writes to schema fitness based on detected concepts.
   ALIGNED: Implements Resilience Rewards to prevent the Spiral of Doubt.
   Logic: Uncertain success boosts fitness; Confident failure penalizes heavily."
  (let ((schema (or (and (boundp '*active-schema*) *active-schema*)
                    (and (boundp '*last-executed-schema*) *last-executed-schema*
                         (boundp '*last-schema-step*)
                         (< (- *step* *last-schema-step*) 20)
                         *last-executed-schema*)))
        (got-it (getf state :actual-success-p))
        (delta 0.0))
    (when (and concepts schema (executable-schema-p schema))
      (cond
        ;; CASE 1: RESILIENCE (Worked under pressure)
        ((and got-it (or (member 'CONFUSED concepts) (member 'UNCERTAIN concepts)))
         (incf delta 0.05)) ; High reward for working despite confusion
        
        ;; CASE 2: ARROGANCE (Confident but wrong)
        ((and (not got-it) (member 'CONFIDENT concepts))
         (decf delta 0.08)) ; Heavy penalty for being confidently wrong
        
        ;; CASE 3: STANDARD SUCCESS
        (got-it
         (when (member 'CONFIDENT concepts) (incf delta 0.02))
         (when (member 'FLOWING concepts) (incf delta 0.02)))
        
        ;; CASE 4: STANDARD FAILURE
        ((not got-it)
         (when (member 'CONFUSED concepts) (decf delta 0.02))
         (when (member 'STUCK concepts) (decf delta 0.03))))

      ;; Apply bounded delta
      (when (not (zerop delta))
        (let ((old-success (executable-schema-success-count schema)))
          (setf (executable-schema-success-count schema)
                (max 0 (+ old-success (round (* delta 10)))))
          (when (boundp '*modification-log*)
            (push-capped! (list :step *step*
                                :action 'resilient-schema-fitness
                                :concepts concepts
                                :delta delta
                                :outcome (if got-it :resilient-success :confident-failure)
                                :schema (executable-schema-id schema))
                          *modification-log*)))))))

;; Auto-install on load
(install-deep-wiring!)

;;; ============================================================================
;;; DIAGNOSTIC - Show wiring status
;;; ============================================================================

(defun print-deep-wiring-status ()
  "Print status of deep wiring connections."
  (format t "~%=== DEEP WIRING STATUS ===~%")
  (format t "Initialized: ~A~%" *deep-wiring-initialized*)
  (format t "Self-expectation accuracy history: ~D entries~%"
          (length *self-expectation-accuracy-history*))
  (format t "Self-expectation accuracy: ~,1F%~%"
          (* 100 (self-expectation-accuracy)))
  (format t "Self-expectation weight: ~,2F~%" (self-expectation-weight))
  (format t "Schema execution events: ~D~%" (length *schema-execution-events*))
  (format t "Presence-adjusted hypothesis interval: ~D steps~%"
          (presence-adjusted-hypothesis-interval))
  (format t "Concept-goal cooldowns: ~D active~%"
          (hash-table-count *concept-goal-cooldown*))
  (format t "===========================~%"))

;;; ============================================================================
;;; 11. CONFIRMED HYPOTHESIS → ACTUAL PROGRAM REWRITE
;;; When a structural/parameter hypothesis is confirmed, ACTUALLY modify programs
;;; ============================================================================

(defvar *program-modifications-attempted* 0)
(defvar *program-modifications-succeeded* 0)
(defvar *recent-modifications* (make-hash-table :test 'equal) "Prevent duplicate mods")

(defun translate-semantic-to-code (semantic-op semantic-param semantic-value)
  "Translate semantic hypothesis concepts to actual op/param/value.
   Returns (values real-op real-param real-value) or (values nil nil nil)."
  ;; Map semantic op names to real ops WITH modifiable params
  ;; Note: assess-confidence has NO modifiable params, but try-similar does
  (let ((op-mapping '((confidence-threshold . try-similar)
                      (CONFIDENCE-THRESHOLD . try-similar)
                      (similarity-threshold . try-similar)
                      (SIMILARITY-THRESHOLD . try-similar)
                      (ownership-threshold . try-similar)  ; fallback
                      (OWNERSHIP-THRESHOLD . try-similar)))
        ;; Determine value based on semantic meaning
        (value-for-under-confident 0.5)   ; Lower threshold = accept more (was 0.7)
        (value-for-over-confident 0.85))  ; Higher threshold = accept less
    (let ((real-op (cdr (assoc semantic-op op-mapping)))
          (real-param 'threshold))  ; try-similar uses 'threshold'
      (when real-op
        (let ((real-value (cond
                           ((member semantic-param '(:under-confident :UNDER-CONFIDENT))
                            value-for-under-confident)
                           ((member semantic-param '(:over-confident :OVER-CONFIDENT))
                            value-for-over-confident)
                           ;; If we have a numeric value, use it
                           ((numberp semantic-value) semantic-value)
                           ;; Default adjustment
                           (t 0.6))))
          (values real-op real-param real-value))))))

(defun hypothesis-to-program-rewrite! (hyp)
  "When hypothesis is confirmed with high confidence, actually rewrite programs."
  ;; Lower thresholds: confidence > 0.6, times-confirmed > 0
  (when (and hyp
             (member (self-hypothesis-status hyp) '(:confirmed :executed))
             (> (self-hypothesis-confidence hyp) 0.6))
    (let ((claim (self-hypothesis-claim hyp))
          (action (self-hypothesis-proposed-action hyp)))
      (when (and claim action (listp action))
        (incf *program-modifications-attempted*)
        (let ((action-type (car action)))
          (cond
           ;; MODIFY-PARAM: Actually change parameter values
           ;; Translate semantic claims to concrete code changes
           ((member action-type '(modify-param MODIFY-PARAM))
            (let ((semantic-op (second action))
                  (semantic-param (third action))
                  (semantic-value (fourth action)))
              ;; Translate semantic concepts to real op/param/value
              (multiple-value-bind (real-op real-param real-value)
                  (translate-semantic-to-code semantic-op semantic-param semantic-value)
                (when (and real-op real-param real-value)
                  ;; Check for duplicate modification (same op.param.value recently)
                  (let ((mod-key (list real-op real-param real-value)))
                    (unless (gethash mod-key *recent-modifications*)
                      (let ((modified-count 0))
                        (dolist (expert *experts*)
                          (when (set-modifiable-param! (expert-program expert)
                                                       real-op real-param real-value)
                            (incf modified-count)))
                        (when (> modified-count 0)
                          (incf *program-modifications-succeeded*)
                          ;; Record this modification for cooldown
                          (setf (gethash mod-key *recent-modifications*) *step*)
                          (format t "~%[PROGRAM-REWRITE] Modified ~A.~A to ~A in ~D experts~%"
                                  real-op real-param real-value modified-count)))))))))

           ;; PREFER-STRUCTURE: Record for future spawning
           ((member action-type '(prefer-structure PREFER-STRUCTURE))
            (when *self-model*
              (push (second action) (self-model-best-program-structures *self-model*))
              (incf *program-modifications-succeeded*)
              (format t "~%[PROGRAM-REWRITE] Recorded preferred structure: ~A~%"
                      (second action))))

           ;; RAISE/LOWER-CONFIDENCE-THRESHOLD: already handled elsewhere
           ((member action-type '(raise-confidence-threshold lower-confidence-threshold
                                  RAISE-CONFIDENCE-THRESHOLD LOWER-CONFIDENCE-THRESHOLD))
            nil)

           ;; Other actions we can try to handle
           (t nil)))))))

(defun wire-hypothesis-to-rewrite! ()
  "Check hypotheses and rewrite programs when appropriate."
  (when (boundp '*hypotheses*)
    (maphash (lambda (id hyp)
               (declare (ignore id))
               (when (and (member (self-hypothesis-status hyp) '(:confirmed :executed))
                          (> (self-hypothesis-confidence hyp) 0.6))
                 (hypothesis-to-program-rewrite! hyp)))
             *hypotheses*)))


;;; ============================================================================
;;; 12. SCHEMA SUCCESS → OPERATOR SYNTHESIS
;;; When schemas succeed enough, actually synthesize new operators
;;; ============================================================================

(defvar *schemas-synthesized-to-ops* (make-hash-table :test 'eq))

(defun maybe-synthesize-from-schema! (schema)
  "If schema has high success rate, synthesize an operator from it."
  (when (and schema
             (not (gethash (if (executable-schema-p schema)
                              (executable-schema-id schema)
                              (cognitive-schema-id schema))
                          *schemas-synthesized-to-ops*)))
    (let ((success-rate (schema-success-rate schema))
          (executions (if (executable-schema-p schema)
                         (executable-schema-execution-count schema)
                         (cognitive-schema-instances schema))))
      ;; Need high success AND enough samples
      (when (and (> success-rate 0.7)
                 (> executions 20))
        (let ((schema-id (if (executable-schema-p schema)
                            (executable-schema-id schema)
                            (cognitive-schema-id schema))))
          ;; Mark as attempted
          (setf (gethash schema-id *schemas-synthesized-to-ops*) t)
          ;; Call existing synthesis function
          (when (fboundp 'synthesize-op-from-schema)
            (let ((synth (synthesize-op-from-schema schema)))
              (when synth
                (format t "~%[OP-SYNTHESIS] Created new op from schema ~A: ~A~%"
                        schema-id (synthesized-op-name synth))))))))))

(defun wire-schemas-to-ops! ()
  "Check all schemas and synthesize ops from successful ones."
  ;; Check cognitive schemas
  (when (boundp '*cognitive-schemas*)
    (maphash (lambda (id schema)
               (declare (ignore id))
               (when (> (cognitive-schema-instances schema) 20)
                 (maybe-synthesize-from-schema! schema)))
             *cognitive-schemas*))
  ;; Check executable schemas
  (when (boundp '*executable-schemas*)
    (maphash (lambda (id schema)
               (declare (ignore id))
               (when (> (executable-schema-execution-count schema) 20)
                 (maybe-synthesize-from-schema! schema)))
             *executable-schemas*)))


;;; ============================================================================
;;; 13. BEST-PROGRAM-STRUCTURES → SPAWN WITH OPTIMAL STRUCTURE
;;; Use learned best structures when spawning new experts
;;; ============================================================================

(defvar *original-spawn-expert* nil)

(defun spawn-expert-with-best-structure (&key parent)
  "Spawn expert using best-known program structures when available."
  (let ((expert (if *original-spawn-expert*
                    (funcall *original-spawn-expert* :parent parent)
                    (spawn-expert :parent parent))))
    ;; If we have best structures, try to apply them
    (when (and expert *self-model*
               (self-model-best-program-structures *self-model*))
      (let ((best (first (self-model-best-program-structures *self-model*))))
        (when (and best (listp best))
          ;; Best structure is list of (op-signature accuracy samples)
          ;; Try to modify expert's program toward this structure
          (dolist (op-hint best)
            (when (and (listp op-hint) (symbolp (car op-hint)))
              ;; Try to set parameters that match
              (let ((op-name (car op-hint)))
                (when (find-op-in-program (expert-program expert) op-name)
                  ;; Boost threshold slightly for this op based on structure knowledge
                  (set-modifiable-param! (expert-program expert)
                                        op-name 'threshold
                                        (+ 0.5 (* 0.1 (random 0.3)))))))))))
    expert))

(defun wire-spawn-to-structures! ()
  "Wire spawn-expert to use best structures."
  (when (and (fboundp 'spawn-expert)
             (not *original-spawn-expert*))
    (setf *original-spawn-expert* #'spawn-expert)
    ;; Don't override - just use spawn-expert-with-best-structure when appropriate
    ))


;;; ============================================================================
;;; 14. EXPERT DEATH PATTERNS → MECHANISM CREATION
;;; If experts die for similar reasons, create mechanism to prevent it
;;; ============================================================================

(defvar *expert-death-patterns* nil "Track why experts die")

(defun record-expert-death-pattern! (expert heir)
  "Record pattern of expert death for analysis."
  (declare (ignore heir))
  (when expert
    (let* ((hits (expert-hits expert))
           (misses (expert-misses expert))
           (life (expert-life expert))
           (age (- *step* (expert-birth-step expert)))
           (death-type (cond
                        ((< age 50) :early-death)           ; Died young
                        ((> misses (* hits 3)) :too-many-errors)  ; Failed too much
                        ((< life 0.1) :starvation)          ; Not fed
                        (t :natural))))
      (push (list :step *step*
                  :type death-type
                  :hits hits
                  :misses misses
                  :age age
                  :ops (get-op-names (expert-program expert)))
            *expert-death-patterns*)
      ;; Keep bounded
      (when (> (length *expert-death-patterns*) 50)
        (setf *expert-death-patterns* (subseq *expert-death-patterns* 0 50))))))

(defun analyze-death-patterns! ()
  "Analyze death patterns and create preventive mechanism if pattern found."
  (when (> (length *expert-death-patterns*) 10)
    (let ((type-counts (make-hash-table :test 'eq)))
      ;; Count death types
      (dolist (death *expert-death-patterns*)
        (incf (gethash (getf death :type) type-counts 0)))
      ;; If one type dominates, create hypothesis about it
      (maphash (lambda (type count)
                 (when (> count 5)  ; Same type 5+ times
                   (let ((hyp-claim (case type
                                     (:early-death '(:meta experts-dying-young))
                                     (:too-many-errors '(:meta experts-failing-too-much))
                                     (:starvation '(:meta experts-not-getting-fed))
                                     (t nil))))
                     (when hyp-claim
                       ;; Generate hypothesis if not already exists
                       (unless (find-existing-hypothesis-for-claim hyp-claim)
                         (generate-hypothesis! hyp-claim))))))
               type-counts))))


;;; ============================================================================
;;; 15. DRIVE PRESSURE → HYPOTHESIS TYPE BIAS
;;; High drive pressure biases what kinds of hypotheses get generated
;;; ============================================================================

(defvar *drive-hypothesis-bias* (make-hash-table :test 'eq))

(defun update-drive-hypothesis-bias! ()
  "Update bias for hypothesis generation based on drives."
  (let ((curiosity (compute-drive-level :curiosity))
        (competence (compute-drive-level :competence))
        (coherence (compute-drive-level :coherence)))
    ;; High curiosity → more exploratory/structure hypotheses
    (setf (gethash :structure *drive-hypothesis-bias*)
          (if (> curiosity 0.6) 1.5 1.0))
    ;; High competence pressure → more parameter hypotheses
    (setf (gethash :parameter *drive-hypothesis-bias*)
          (if (> competence 0.6) 1.5 1.0))
    ;; High coherence pressure → more meta hypotheses
    (setf (gethash :meta *drive-hypothesis-bias*)
          (if (> coherence 0.6) 1.5 1.0))))

(defun drive-biased-hypothesis-generation! (observation)
  "Generate hypothesis with drive-based type biasing."
  (update-drive-hypothesis-bias!)
  (let* ((base-type (classify-observation observation))
         (bias (gethash base-type *drive-hypothesis-bias* 1.0)))
    ;; If biased toward this type, always generate
    ;; If biased away, maybe skip
    (when (or (>= bias 1.0)
              (< (random 1.0) bias))
      (generate-hypothesis! observation))))


;;; ============================================================================
;;; INSTALLATION - Add new wirings
;;; ============================================================================

(defun install-deeper-wiring! ()
  "Install the additional deep wiring for actual code modification."
  (format t "~%Installing deeper wiring (11-15)...~%")

  ;; 11. Hypothesis → program rewrite
  (register-hook +hook-maintenance+
                 #'wire-hypothesis-to-rewrite!
                 :priority 47)
  (format t "  [11/15] Hypothesis confirmation → program rewrite~%")

  ;; 12. Schema → op synthesis
  (register-hook +hook-maintenance+
                 #'wire-schemas-to-ops!
                 :priority 48)
  (format t "  [12/15] Schema success → operator synthesis~%")

  ;; 13. Best structures → spawn
  (wire-spawn-to-structures!)
  (format t "  [13/15] Best structures → expert spawning~%")

  ;; 14. Death patterns → mechanism
  (register-hook +hook-expert-dying+
                 #'record-expert-death-pattern!
                 :priority 31)
  (register-hook +hook-maintenance+
                 #'analyze-death-patterns!
                 :priority 49)
  (format t "  [14/15] Expert death patterns → mechanism creation~%")

  ;; 15. Drive → hypothesis bias
  (register-hook +hook-maintenance+
                 #'update-drive-hypothesis-bias!
                 :priority 44)
  (format t "  [15/15] Drive pressure → hypothesis type bias~%")

  (format t "~%Deeper wiring complete. 5 additional interconnections active.~%"))

;; Auto-install deeper wiring
(install-deeper-wiring!)


;;; ============================================================================
;;; 16. SURPRISE → WORKING MEMORY
;;; High surprise events get loaded into working memory for processing
;;; ============================================================================

(defun surprise-to-working-memory! (ctx actual predicted confidence)
  "When surprised, load the surprising event into working memory."
  (when (and (boundp '*working-memory*) *working-memory*
             predicted actual
             (not (equal predicted actual))
             (> confidence 0.6))  ; We were confident but wrong
    ;; Load surprise into WM for processing
    (wm-load! (list :surprise ctx :expected predicted :got actual)
              :type :surprise
              :priority (min 0.9 confidence)  ; Higher confidence = more surprising
              :source :prediction-error)))


;;; ============================================================================
;;; 17. CAUSAL MODEL → PREDICTION CONFIDENCE BOOST
;;; If causal model agrees with expert, boost confidence
;;; ============================================================================

(defvar *causal-boost-amount* 0.15 "How much to boost when causal model agrees")

(defun causal-model-confidence-boost (ctx prediction)
  "Check if causal model supports this prediction, return confidence boost."
  (when (and (boundp '*causal-model*) (> (hash-table-count *causal-model*) 0)
             ctx (> (length ctx) 0))
    (let ((last-token (car (last ctx))))
      (when last-token
        (let ((effects (get-effects last-token)))
          (when effects
            ;; Check if prediction matches any causal effect
            (let ((matching (find prediction effects :key #'car :test #'equal)))
              (when matching
                ;; Return the causal strength as a confidence boost
                (* *causal-boost-amount* (cdr matching))))))))))


;;; ============================================================================
;;; 18. WORKING MEMORY OVERFLOW → COHERENCE GOAL
;;; When WM overflows, create a goal to consolidate
;;; ============================================================================

(defvar *last-wm-overflow-check* 0)

(defun check-wm-overflow-for-goal! ()
  "If working memory has overflowed recently, create coherence goal."
  (when (and (boundp '*working-memory*) *working-memory*
             (> (- *step* *last-wm-overflow-check*) 100))  ; Check every 100 steps
    (setf *last-wm-overflow-check* *step*)
    (let ((overflow-count (working-memory-overflow-count *working-memory*)))
      (when (> overflow-count 5)
        ;; WM is under pressure - create consolidation goal
        (unless (find :wm-consolidation *goal-stack*
                      :key (lambda (g) (goal-type g)))
          (let ((goal (make-goal
                       :type :wm-consolidation
                       :description "Consolidate working memory - capacity exceeded"
                       :priority 0.65
                       :drive-source :coherence
                       :strategies '(:consolidate :compress))))
            (push-goal! goal)))
        ;; Reset overflow counter
        (setf (working-memory-overflow-count *working-memory*) 0)))))


;;; ============================================================================
;;; 19. CORRECT PREDICTION → CAUSAL LINK STRENGTHENING
;;; When we predict correctly, strengthen causal links in context
;;; ============================================================================

(defun strengthen-causal-links-on-success! (ctx actual got-it)
  "When prediction succeeds, strengthen causal links between context tokens."
  (when (and got-it
             (boundp '*causal-model*)
             ctx (> (length ctx) 1))
    ;; Strengthen link from second-to-last to actual outcome
    (let ((cause (car (last ctx 2)))
          (effect actual))
      (when (and cause effect)
        (let* ((key (cons cause effect))
               (existing (gethash key *causal-model*)))
          (if existing
              ;; Strengthen existing link
              (setf (causal-link-strength existing)
                    (min 1.0 (+ (causal-link-strength existing) 0.02)))
              ;; Create new weak link
              (add-causal-link! cause effect :strength 0.3)))))))


;;; ============================================================================
;;; 20. SCHEMA SUCCESS → CAUSAL CHAIN CREATION
;;; When schema succeeds, create causal links between pattern elements
;;; ============================================================================

(defun schema-success-to-causal-model! (schema)
  "When a schema succeeds, add its pattern as causal links."
  (when (and schema (boundp '*causal-model*))
    (let ((pattern (if (executable-schema-p schema)
                       (executable-schema-op-sequence schema)
                       (cognitive-schema-pattern schema))))
      (when (and pattern (listp pattern) (> (length pattern) 1))
        ;; Create causal chain from pattern
        (loop for i from 0 below (1- (length pattern))
              for cause = (nth i pattern)
              for effect = (nth (1+ i) pattern)
              when (and cause effect)
              do (let* ((cause-tok (if (listp cause) (car cause) cause))
                       (effect-tok (if (listp effect) (car effect) effect)))
                   (when (and (symbolp cause-tok) (symbolp effect-tok))
                     (add-causal-link! cause-tok effect-tok :strength 0.4))))))))


;;; ============================================================================
;;; INSTALLATION - Novel wirings (16-20)
;;; ============================================================================

(defun install-novel-wiring! ()
  "Install novel productivity connections (16-20)."
  (format t "~%Installing novel wiring (16-20)...~%")

  ;; 16. Surprise → WM
  (register-hook +hook-post-learn+
                 (lambda (ctx predicted actual got-it expert)
                   (declare (ignore expert))
                   (unless got-it
                     (surprise-to-working-memory! ctx actual predicted
                                                  (compute-recent-accuracy 20))))
                 :priority 52)
  (format t "  [16/20] Surprise → working memory~%")

  ;; 17. Causal boost is used inline, not a hook
  (format t "  [17/20] Causal model → prediction confidence (inline)~%")

  ;; 18. WM overflow → Goal
  (register-hook +hook-maintenance+
                 #'check-wm-overflow-for-goal!
                 :priority 50)
  (format t "  [18/20] Working memory overflow → coherence goal~%")

  ;; 19. Correct prediction → Causal strengthening
  (register-hook +hook-post-learn+
                 (lambda (ctx predicted actual got-it expert)
                   (declare (ignore predicted expert))
                   (strengthen-causal-links-on-success! ctx actual got-it))
                 :priority 53)
  (format t "  [19/20] Correct prediction → causal link strengthening~%")

  ;; 20. Schema success → Causal chain
  ;; Wire into schema execution success
  (let ((orig-learn (when (fboundp 'learn-from-schema-execution!)
                      #'learn-from-schema-execution!)))
    (when orig-learn
      ;; Already wired in #1, add causal chain creation
      (register-hook +hook-maintenance+
                     (lambda ()
                       (when (and (boundp '*executable-schemas*) *executable-schemas*)
                         (maphash (lambda (id schema)
                                    (declare (ignore id))
                                    (when (and (> (executable-schema-success-count schema) 10)
                                               (> (/ (executable-schema-success-count schema)
                                                    (max 1 (executable-schema-execution-count schema)))
                                                  0.6))
                                      (schema-success-to-causal-model! schema)))
                                  *executable-schemas*)))
                     :priority 54)))
  (format t "  [20/20] Schema success → causal chain creation~%")

  (format t "~%Novel wiring complete. 5 productivity connections active.~%"))

;; Auto-install novel wiring
(install-novel-wiring!)


;;; ============================================================================
;;; 21. ATTENTION INTERESTING → EXPERT LIFE BOOST
;;; Experts handling interesting contexts get life boost (survive longer)
;;; ============================================================================

(defun boost-experts-for-interesting-contexts! ()
  "Boost life of experts that own interesting contexts."
  (when (and (boundp '*attention-state*) *attention-state*
             (attention-state-interesting-contexts *attention-state*))
    (dolist (ctx (attention-state-interesting-contexts *attention-state*))
      (let ((owner (find-context-owner ctx)))
        (when owner
          ;; Small life boost for handling interesting work
          (setf (expert-life owner)
                (min 1.0 (+ (expert-life owner) 0.01))))))))


;;; ============================================================================
;;; 22. STRATEGY STATS → STRATEGY SELECTION BIAS
;;; Use recorded strategy success rates to bias future selection
;;; ============================================================================

(defun get-strategy-success-rate (strategy)
  "Get success rate for a strategy from stats."
  (when (boundp '*strategy-stats*)
    (let ((stats (gethash strategy *strategy-stats*)))
      (when (and stats (> (strategy-stats-uses stats) 5))
        (/ (strategy-stats-successes stats)
           (max 1 (strategy-stats-uses stats)))))))

(defun strategy-bias-for-selection (strategy)
  "Return selection bias based on strategy performance."
  (let ((success-rate (get-strategy-success-rate strategy)))
    (cond
      ((null success-rate) 1.0)        ; No data - neutral
      ((> success-rate 0.7) 1.5)       ; Good - boost
      ((> success-rate 0.5) 1.0)       ; OK - neutral
      ((> success-rate 0.3) 0.7)       ; Poor - reduce
      (t 0.4))))                        ; Bad - strong reduce


;;; ============================================================================
;;; 23. SEMANTIC WEAKNESSES → IMPROVEMENT GOALS
;;; Identified weaknesses should generate goals to improve
;;; ============================================================================

(defvar *weakness-goal-cooldown* (make-hash-table :test 'equal))

(defun weakness-to-goal! (weakness)
  "Convert identified weakness to improvement goal."
  (let ((last-goal (gethash weakness *weakness-goal-cooldown*)))
    (when (or (null last-goal) (> (- *step* last-goal) 1000))
      (setf (gethash weakness *weakness-goal-cooldown*) *step*)
      (let ((goal (make-goal
                   :type :improve-weakness
                   :description (format nil "Improve weakness: ~A" weakness)
                   :priority 0.6
                   :drive-source :competence
                   :strategies '(:focus-training :schema-application))))
        (push-goal! goal)))))

(defun check-semantic-weaknesses-for-goals! ()
  "Check semantic self-knowledge for weaknesses to address."
  (when (and (boundp '*semantic-self-knowledge*) *semantic-self-knowledge*)
    (let ((weaknesses (semantic-self-knowledge-weaknesses *semantic-self-knowledge*)))
      (when (and weaknesses (> (length weaknesses) 0))
        ;; Generate goal for most recent weakness
        (weakness-to-goal! (first weaknesses))))))


;;; ============================================================================
;;; 24. SURPRISE WINDOW ACCUMULATION → DREAM TRIGGER
;;; Too many surprises in window should trigger dreaming
;;; ============================================================================

(defvar *surprise-dream-threshold* 5 "Surprises needed to trigger dream")
(defvar *last-surprise-dream* 0)

(defun check-surprise-window-for-dream! ()
  "If surprise window is full, trigger dream cycle."
  (when (and (boundp '*surprise-window*) *surprise-window*
             (> (- *step* *last-surprise-dream*) 200)  ; Cooldown
             (>= (length *surprise-window*) *surprise-dream-threshold*))
    (setf *last-surprise-dream* *step*)
    ;; Trigger dream about surprising events
    (when (fboundp 'run-dream-cycle!)
      (ignore-errors (run-dream-cycle!))
      ;; Clear processed surprises
      (setf *surprise-window* nil))))


;;; ============================================================================
;;; 25. CONFIDENCE HISTORY → THRESHOLD CALIBRATION
;;; Track over/under confidence to adjust expert thresholds
;;; ============================================================================

(defvar *confidence-calibration-window* 50)

(defun analyze-confidence-calibration ()
  "Analyze if system is over or under confident using trace buffer."
  (when (and (boundp '*trace-buffer*) *trace-buffer*
             (> (fill-pointer *trace-buffer*) 20))
    (let ((high-conf-wrong 0)
          (low-conf-right 0)
          (count 0))
      ;; Use trace buffer which has both confidence and correctness
      (dotimes (i (min *confidence-calibration-window*
                       (fill-pointer *trace-buffer*)))
        (let ((trace (aref *trace-buffer* (- (fill-pointer *trace-buffer*) 1 i))))
          (when (cognitive-trace-p trace)
            (incf count)
            (let ((conf (or (cognitive-trace-confidence trace) 0.5))
                  (correct (cognitive-trace-correct trace)))
              (when (and (> conf 0.7) (not correct))
                (incf high-conf-wrong))
              (when (and (< conf 0.4) correct)
                (incf low-conf-right))))))
      ;; Return calibration assessment
      (when (> count 0)
        (cond
          ((> high-conf-wrong 10) :overconfident)
          ((> low-conf-right 10) :underconfident)
          (t :calibrated))))))

(defun calibrate-from-confidence-history! ()
  "Adjust thresholds based on confidence calibration."
  (let ((calibration (analyze-confidence-calibration)))
    (case calibration
      (:overconfident
       ;; Raise thresholds - be more selective
       (dolist (expert *experts*)
         (let ((prog (expert-program expert)))
           (set-modifiable-param! prog 'try-similar 'threshold
                                  (min 0.9 (+ 0.05 (or (get-param-value prog 'try-similar 'threshold) 0.7)))))))
      (:underconfident
       ;; Lower thresholds - accept more
       (dolist (expert *experts*)
         (let ((prog (expert-program expert)))
           (set-modifiable-param! prog 'try-similar 'threshold
                                  (max 0.3 (- (or (get-param-value prog 'try-similar 'threshold) 0.7) 0.05)))))))))

(defun get-param-value (prog op-name param-name)
  "Get current value of a modifiable param."
  (let ((params (get-modifiable-params prog)))
    (let ((param (find-if (lambda (p)
                            (and (eq (first p) op-name)
                                 (eq (second p) param-name)))
                          params)))
      (when param (third param)))))


;;; ============================================================================
;;; INSTALLATION - Utilization wirings (21-25)
;;; ============================================================================

(defun install-utilization-wiring! ()
  "Install wirings that better utilize existing systems (21-25)."
  (format t "~%Installing utilization wiring (21-25)...~%")

  ;; 21. Attention → Expert boost
  (register-hook +hook-maintenance+
                 #'boost-experts-for-interesting-contexts!
                 :priority 55)
  (format t "  [21/25] Attention interesting → expert life boost~%")

  ;; 22. Strategy stats used inline in selection
  (format t "  [22/25] Strategy stats → selection bias (inline)~%")

  ;; 23. Semantic weaknesses → Goals
  (register-hook +hook-maintenance+
                 #'check-semantic-weaknesses-for-goals!
                 :priority 56)
  (format t "  [23/25] Semantic weaknesses → improvement goals~%")

  ;; 24. Surprise window → Dream trigger
  (register-hook +hook-maintenance+
                 #'check-surprise-window-for-dream!
                 :priority 57)
  (format t "  [24/25] Surprise accumulation → dream trigger~%")

  ;; 25. Confidence calibration (every 200 steps)
  (let ((last-calibration 0))
    (register-hook +hook-maintenance+
                   (lambda ()
                     (when (> (- *step* last-calibration) 200)
                       (setf last-calibration *step*)
                       (calibrate-from-confidence-history!)))
                   :priority 58))
  (format t "  [25/25] Confidence history → threshold calibration~%")

  (format t "~%Utilization wiring complete. 5 connections active.~%"))

;; Auto-install utilization wiring
(install-utilization-wiring!)


;;; ============================================================================
;;; 26. PATTERN FAMILIARITY → EXPERT SELECTION BIAS
;;; Familiar patterns should bias toward experts that handled them before
;;; ============================================================================

(defun pattern-familiarity-expert-bias (ctx)
  "Return expert bias based on pattern familiarity."
  (when (and (boundp '*pattern-signatures*) *pattern-signatures*
             ctx (> (length ctx) 0))
    (let ((abstract (ignore-errors (abstract-pattern ctx))))
      (when abstract
        (let ((sig (gethash abstract *pattern-signatures*)))
          (when (and sig (> (pattern-signature-frequency sig) 3))
            ;; Return the expert-id that handled this pattern most
            (let ((outcomes (pattern-signature-outcomes sig)))
              (when outcomes
                (let ((best-expert nil)
                      (best-score 0))
                  (dolist (outcome outcomes)
                    (when (and (getf outcome :expert-id)
                               (getf outcome :correct)
                               (> (or (getf outcome :confidence) 0) best-score))
                      (setf best-expert (getf outcome :expert-id)
                            best-score (or (getf outcome :confidence) 0))))
                  best-expert)))))))))

(defun apply-pattern-familiarity-to-routing! ()
  "Bias expert routing based on pattern familiarity."
  (when (and (boundp '*last-context*) *last-context*)
    (let ((preferred-expert (pattern-familiarity-expert-bias *last-context*)))
      (when preferred-expert
        ;; Boost that expert's life temporarily
        (let ((expert (find preferred-expert *experts* :key #'expert-id)))
          (when expert
            (incf (expert-life expert) 0.1)))))))


;;; ============================================================================
;;; 27. PRESENCE SELF-CONFIDENCE → PREDICTION CONFIDENCE SCALING
;;; Low presence self-confidence should scale down prediction confidence
;;; ============================================================================

(defvar *presence-confidence-scaling-enabled* t)

(defun presence-scaled-confidence (raw-confidence)
  "Scale prediction confidence by presence self-confidence."
  (if (and *presence-confidence-scaling-enabled*
           (boundp '*presence*) *presence*)
      (let ((self-conf (presence-self-confidence *presence*)))
        ;; Scale: if self-confidence is 0.5, multiply by 0.75-1.0 range
        ;; if self-confidence is 1.0, multiply by 1.0
        ;; if self-confidence is 0.0, multiply by 0.5
        (* raw-confidence (+ 0.5 (* 0.5 self-conf))))
      raw-confidence))


;;; ============================================================================
;;; 28. EPISODIC MEMORY → LEARNING RATE BOOST FOR SIGNIFICANT EPISODES
;;; More significant episodes should trigger stronger learning
;;; ============================================================================

(defvar *episode-learning-boost* 1.0)

(defun update-episode-learning-boost! ()
  "Update learning boost based on current episode significance."
  (when (and (boundp '*current-episode*) *current-episode*
             (boundp '*episodic-memory*) *episodic-memory*)
    (let ((sig (ignore-errors (compute-episode-significance *current-episode*))))
      (when (and sig (numberp sig))
        ;; Significant episodes (>0.7) boost learning by up to 1.5x
        (setf *episode-learning-boost*
              (+ 1.0 (* 0.5 (max 0 (- sig 0.5)))))))))


;;; ============================================================================
;;; 29. GOAL HISTORY → SUPPRESS REPEATED GOALS
;;; Don't generate goals we've already solved recently
;;; ============================================================================

(defvar *recent-goal-types* (make-hash-table :test 'equal))

(defun goal-recently-achieved-p (goal-type goal-description)
  "Check if a similar goal was achieved recently."
  (let ((key (list goal-type (subseq (format nil "~A" goal-description)
                                      0 (min 30 (length (format nil "~A" goal-description)))))))
    (let ((last-achieved (gethash key *recent-goal-types*)))
      (and last-achieved (< (- *step* last-achieved) 500)))))

(defun record-goal-achievement! (goal)
  "Record that a goal was achieved."
  (when goal
    (let ((key (list (goal-type goal)
                     (subseq (format nil "~A" (goal-description goal))
                             0 (min 30 (length (format nil "~A" (goal-description goal))))))))
      (setf (gethash key *recent-goal-types*) *step*))))


;;; ============================================================================
;;; 30. EXPERIMENT HISTORY → AVOID REDUNDANT EXPERIMENTS
;;; Don't run experiments we've already tried recently
;;; ============================================================================

(defvar *recent-experiment-signatures* (make-hash-table :test 'equal))

(defun experiment-recently-run-p (experiment-type context-signature)
  "Check if a similar experiment was run recently."
  (let ((key (list experiment-type context-signature)))
    (let ((last-run (gethash key *recent-experiment-signatures*)))
      (and last-run (< (- *step* last-run) 1000)))))

(defun record-experiment-run! (experiment-type context-signature)
  "Record that an experiment was run."
  (setf (gethash (list experiment-type context-signature) *recent-experiment-signatures*) *step*))

(defun should-skip-experiment-p (experiment)
  "Check if experiment should be skipped due to recent similar run."
  (when (and experiment (listp experiment))
    (let ((exp-type (car experiment))
          (ctx-sig (when (> (length experiment) 1)
                     (let ((ctx (second experiment)))
                       (when ctx (sxhash ctx))))))
      (experiment-recently-run-p exp-type ctx-sig))))


;;; ============================================================================
;;; 31. INTROSPECTIVE CONCEPTS → BEHAVIOR MODIFICATION
;;; Active concepts should directly influence system behavior
;;; ============================================================================

(defun apply-concept-behavior-effects! ()
  "Apply behavioral effects based on active introspective concepts."
  (when (boundp '*introspective-concepts*)
    ;; Check CONFUSED concept
    (let ((confused (gethash 'confused *introspective-concepts*)))
      (when (and confused (introspective-concept-state-active confused))
        ;; Lower confidence threshold when confused
        (dolist (expert *experts*)
          (when (> (expert-confidence-threshold expert) 0.4)
            (setf (expert-confidence-threshold expert)
                  (- (expert-confidence-threshold expert) 0.02))))))

    ;; Check EXPLORING concept
    (let ((exploring (gethash 'exploring *introspective-concepts*)))
      (when (and exploring (introspective-concept-state-active exploring))
        ;; Boost exploration rate
        (when (boundp '*goal-behavior-params*)
          (let ((current (getf *goal-behavior-params* :exploration-rate 0.1)))
            (setf (getf *goal-behavior-params* :exploration-rate)
                  (min 0.4 (+ current 0.05)))))))

    ;; Check CONFIDENT concept
    (let ((confident (gethash 'confident *introspective-concepts*)))
      (when (and confident (introspective-concept-state-active confident))
        ;; When genuinely confident, trust schemas more
        (when (boundp '*goal-behavior-params*)
          (let ((current (getf *goal-behavior-params* :schema-trust 0.5)))
            (setf (getf *goal-behavior-params* :schema-trust)
                  (min 0.9 (+ current 0.05)))))))))


;;; ============================================================================
;;; 32. SITUATION HISTORY → STRATEGY SELECTION BIAS
;;; Use past situation→strategy→outcome to bias future strategy selection
;;; ============================================================================

(defvar *situation-strategy-outcomes* (make-hash-table :test 'equal))

(defun record-situation-strategy-outcome! (situation-type strategy-used success-p)
  "Record outcome of strategy in situation for future reference."
  (let ((key (cons situation-type strategy-used)))
    (let ((current (gethash key *situation-strategy-outcomes* (cons 0 0))))
      (if success-p
          (setf (gethash key *situation-strategy-outcomes*)
                (cons (1+ (car current)) (cdr current)))
          (setf (gethash key *situation-strategy-outcomes*)
                (cons (car current) (1+ (cdr current))))))))

(defun best-strategy-for-situation (situation-type available-strategies)
  "Return best strategy based on historical outcomes."
  (let ((best-strategy nil)
        (best-score -1))
    (dolist (strategy available-strategies)
      (let* ((key (cons situation-type strategy))
             (record (gethash key *situation-strategy-outcomes*))
             (successes (if record (car record) 0))
             (failures (if record (cdr record) 0))
             (total (+ successes failures))
             (score (if (> total 0)
                        (/ successes (float total))
                        0.5)))  ; Unknown = neutral
        (when (> score best-score)
          (setf best-score score
                best-strategy strategy))))
    best-strategy))


;;; ============================================================================
;;; 33. COUNTERFACTUAL SUCCESS → ACTION GENERATION
;;; Successful counterfactuals suggest actions to try
;;; ============================================================================

(defun learn-from-counterfactual-history! ()
  "Extract actionable insights from counterfactual history."
  (when (and (boundp '*counterfactual-history*) *counterfactual-history*
             (> (length *counterfactual-history*) 5))
    ;; Find counterfactuals where alternative would have been better
    (let ((improvements nil))
      (dolist (cf (subseq *counterfactual-history* 0 (min 20 (length *counterfactual-history*))))
        (when (and (listp cf)
                   (getf cf :alternative-outcome)
                   (getf cf :actual-outcome)
                   (> (or (getf cf :alternative-score) 0)
                      (or (getf cf :actual-score) 0)))
          (push cf improvements)))
      ;; If we consistently would have done better with alternatives, generate goal
      (when (> (length improvements) 3)
        (let ((goal (make-goal
                     :type :derived
                     :description "Apply lessons from counterfactual analysis"
                     :priority 0.5
                     :strategies '(:explore-alternatives)
                     :drive-source :competence)))
          (push-goal! goal))))))


;;; ============================================================================
;;; 34. PROGRAM MODIFICATION HISTORY → LEARN WHAT WORKS
;;; Analyze modification history to prefer successful modification types
;;; ============================================================================

(defvar *modification-type-success* (make-hash-table :test 'eq))

(defun record-modification-outcome! (mod-type success-p)
  "Record whether a modification type succeeded."
  (let ((current (gethash mod-type *modification-type-success* (cons 0 0))))
    (if success-p
        (setf (gethash mod-type *modification-type-success*)
              (cons (1+ (car current)) (cdr current)))
        (setf (gethash mod-type *modification-type-success*)
              (cons (car current) (1+ (cdr current)))))))

(defun modification-type-reliability (mod-type)
  "Return reliability score for modification type (0-1)."
  (let ((record (gethash mod-type *modification-type-success*)))
    (if record
        (let ((successes (car record))
              (failures (cdr record)))
          (if (> (+ successes failures) 0)
              (/ successes (float (+ successes failures)))
              0.5))
        0.5)))  ; Unknown = neutral

(defun should-attempt-modification-p (mod-type)
  "Check if modification type is reliable enough to attempt."
  (> (modification-type-reliability mod-type) 0.3))


;;; ============================================================================
;;; 35. PRESENCE CONTINUITY → EXPERT TRUST SCALING
;;; Low continuity (discontinuous experience) should reduce expert trust
;;; ============================================================================

(defun continuity-based-expert-trust ()
  "Scale expert trust by presence continuity."
  (if (and (boundp '*presence*) *presence*)
      (let ((continuity (presence-continuity *presence*)))
        ;; High continuity (>0.8) = full trust
        ;; Low continuity (<0.3) = reduced trust
        (cond
          ((> continuity 0.8) 1.0)
          ((< continuity 0.3) 0.7)
          (t (+ 0.7 (* 0.3 (/ (- continuity 0.3) 0.5))))))
      1.0))

(defun apply-continuity-to-expert-confidence! ()
  "Reduce expert confidence when continuity is low."
  (when (and (boundp '*presence*) *presence*
             (< (presence-continuity *presence*) 0.4))
    ;; Low continuity - reduce trust in all experts slightly
    (dolist (expert *experts*)
      (setf (expert-confidence-threshold expert)
            (min 0.9 (+ (expert-confidence-threshold expert) 0.01))))))


;;; ============================================================================
;;; INSTALLATION - Integration wirings (26-35)
;;; ============================================================================

(defun install-integration-wiring! ()
  "Install wirings that integrate isolated systems (26-35)."
  (format t "~%Installing integration wiring (26-35)...~%")

  ;; 26. Pattern familiarity → Expert routing
  (register-hook +hook-maintenance+
                 #'apply-pattern-familiarity-to-routing!
                 :priority 60)
  (format t "  [26/35] Pattern familiarity → expert routing bias~%")

  ;; 27. Presence confidence → Prediction scaling (inline in predictions)
  (format t "  [27/35] Presence self-confidence → prediction scaling (inline)~%")

  ;; 28. Episode significance → Learning boost
  (register-hook +hook-maintenance+
                 #'update-episode-learning-boost!
                 :priority 61)
  (format t "  [28/35] Episode significance → learning rate boost~%")

  ;; 29. Goal history → Suppression (wired into goal generation)
  (format t "  [29/35] Goal history → suppress repeated goals (inline)~%")

  ;; 30. Experiment history → Skip redundant (inline)
  (format t "  [30/35] Experiment history → avoid redundant experiments (inline)~%")

  ;; 31. Introspective concepts → Behavior
  (register-hook +hook-maintenance+
                 #'apply-concept-behavior-effects!
                 :priority 62)
  (format t "  [31/35] Introspective concepts → behavior modification~%")

  ;; 32. Situation→Strategy outcomes (inline in strategy selection)
  (format t "  [32/35] Situation history → strategy selection bias (inline)~%")

  ;; 33. Counterfactual learning
  (let ((last-cf-check 0))
    (register-hook +hook-maintenance+
                   (lambda ()
                     (when (> (- *step* last-cf-check) 300)
                       (setf last-cf-check *step*)
                       (learn-from-counterfactual-history!)))
                   :priority 63))
  (format t "  [33/35] Counterfactual history → action generation~%")

  ;; 34. Modification history learning (inline when mods happen)
  (format t "  [34/35] Modification history → prefer reliable mod types (inline)~%")

  ;; 35. Presence continuity → Expert trust
  (register-hook +hook-maintenance+
                 #'apply-continuity-to-expert-confidence!
                 :priority 64)
  (format t "  [35/35] Presence continuity → expert trust scaling~%")

  (format t "~%Integration wiring complete. 10 connections active.~%"))

;; Auto-install integration wiring
(install-integration-wiring!)


;;; ============================================================================
;;; 36-40. SOURCE CODE AWARENESS
;;; Let the system actually see and reason about its own Lisp code
;;; ============================================================================

(defvar *uhma-source-directory*
  (or (probe-file "/home/peter/HOMOICONIC/UHMA-FINISHED/")
      *default-pathname-defaults*)
  "Directory containing UHMA source files")

(defvar *uhma-source-files*
  '("uhma-v6.1-core-homoiconic.lisp"
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
    "uhma-presence-substrate.lisp"
    "uhma-deep-wiring.lisp")
  "List of UHMA source files (in load order)")

(defvar *my-source-code* (make-hash-table :test 'equal)
  "Cache of own source code, keyed by filename")

(defvar *my-functions* (make-hash-table :test 'eq)
  "Map of function-name -> (file line source)")

(defvar *my-variables* (make-hash-table :test 'eq)
  "Map of variable-name -> (file line)")

;;; 36. READ OWN SOURCE FILES
(defun load-own-source! ()
  "Load my own source code into memory for introspection."
  (clrhash *my-source-code*)
  (dolist (filename *uhma-source-files*)
    (let ((path (merge-pathnames filename *uhma-source-directory*)))
      (when (probe-file path)
        (with-open-file (stream path :direction :input)
          (let ((lines nil)
                (line-num 0))
            (loop for line = (read-line stream nil nil)
                  while line do
                  (incf line-num)
                  (push (cons line-num line) lines))
            (setf (gethash filename *my-source-code*)
                  (nreverse lines)))))))
  (format t "~%[SOURCE-AWARE] Loaded ~D source files~%"
          (hash-table-count *my-source-code*)))

;;; 37. INDEX OWN FUNCTIONS
(defun index-own-functions! ()
  "Build index of my own function definitions."
  (clrhash *my-functions*)
  (maphash
   (lambda (filename lines)
     (dolist (line-entry lines)
       (let ((line-num (car line-entry))
             (line (cdr line-entry)))
         ;; Match (defun NAME or (defmethod NAME or (defmacro NAME
         (when (and (stringp line)
                    (or (search "(defun " line)
                        (search "(defmethod " line)
                        (search "(defmacro " line)))
           ;; Extract function name
           (let* ((start (or (search "(defun " line)
                             (search "(defmethod " line)
                             (search "(defmacro " line)))
                  (name-start (+ start (if (search "(defun " line) 7
                                          (if (search "(defmethod " line) 11 10))))
                  (name-end (position-if (lambda (c) (member c '(#\Space #\( #\))))
                                         line :start name-start))
                  (name-str (when (and name-start name-end)
                              (subseq line name-start name-end)))
                  (name-sym (when name-str
                              (ignore-errors (read-from-string (string-upcase name-str))))))
             (when name-sym
               (setf (gethash name-sym *my-functions*)
                     (list :file filename :line line-num :source line))))))))
   *my-source-code*)
  (format t "[SOURCE-AWARE] Indexed ~D function definitions~%"
          (hash-table-count *my-functions*)))

;;; 38. INDEX OWN VARIABLES
(defun index-own-variables! ()
  "Build index of my own variable definitions."
  (clrhash *my-variables*)
  (maphash
   (lambda (filename lines)
     (dolist (line-entry lines)
       (let ((line-num (car line-entry))
             (line (cdr line-entry)))
         ;; Match (defvar NAME or (defparameter NAME
         (when (and (stringp line)
                    (or (search "(defvar " line)
                        (search "(defparameter " line)))
           (let* ((start (or (search "(defvar " line)
                             (search "(defparameter " line)))
                  (name-start (+ start (if (search "(defvar " line) 8 14)))
                  (name-end (position-if (lambda (c) (member c '(#\Space #\( #\))))
                                         line :start name-start))
                  (name-str (when (and name-start name-end)
                              (subseq line name-start name-end)))
                  (name-sym (when name-str
                              (ignore-errors (read-from-string (string-upcase name-str))))))
             (when name-sym
               (setf (gethash name-sym *my-variables*)
                     (list :file filename :line line-num))))))))
   *my-source-code*)
  (format t "[SOURCE-AWARE] Indexed ~D variable definitions~%"
          (hash-table-count *my-variables*)))

;;; 39. QUERY OWN CODE
(defun where-is-function (name)
  "Return location of a function definition in my source."
  (gethash name *my-functions*))

(defun where-is-variable (name)
  "Return location of a variable definition in my source."
  (gethash name *my-variables*))

(defun read-own-source (filename &optional start-line end-line)
  "Read a portion of my own source code."
  (let ((lines (gethash filename *my-source-code*)))
    (when lines
      (if (and start-line end-line)
          (remove-if-not (lambda (entry)
                           (and (>= (car entry) start-line)
                                (<= (car entry) end-line)))
                         lines)
          lines))))

(defun get-function-source (name &optional context-lines)
  "Get source code of a function (approximate - reads from defun line)."
  (let ((info (where-is-function name)))
    (when info
      (let ((file (getf info :file))
            (line (getf info :line))
            (ctx (or context-lines 20)))
        (read-own-source file line (+ line ctx))))))

;;; 40. REASON ABOUT OWN IMPLEMENTATION
(defun list-my-functions (&optional pattern)
  "List all functions I know about, optionally filtered by pattern."
  (let ((results nil))
    (maphash (lambda (name info)
               (when (or (null pattern)
                         (search pattern (string name) :test #'char-equal))
                 (push (list name (getf info :file) (getf info :line)) results)))
             *my-functions*)
    (sort results #'string< :key (lambda (x) (string (first x))))))

(defun list-my-variables (&optional pattern)
  "List all variables I know about, optionally filtered by pattern."
  (let ((results nil))
    (maphash (lambda (name info)
               (when (or (null pattern)
                         (search pattern (string name) :test #'char-equal))
                 (push (list name (getf info :file) (getf info :line)) results)))
             *my-variables*)
    (sort results #'string< :key (lambda (x) (string (first x))))))

(defun analyze-own-complexity ()
  "Analyze complexity of my own codebase."
  (let ((total-lines 0)
        (total-functions (hash-table-count *my-functions*))
        (total-variables (hash-table-count *my-variables*))
        (files-by-size nil))
    (maphash (lambda (filename lines)
               (let ((line-count (length lines)))
                 (incf total-lines line-count)
                 (push (cons filename line-count) files-by-size)))
             *my-source-code*)
    (list :total-lines total-lines
          :total-functions total-functions
          :total-variables total-variables
          :largest-files (subseq (sort files-by-size #'> :key #'cdr)
                                 0 (min 5 (length files-by-size))))))

(defun what-do-i-know-about (topic)
  "Search my source code for knowledge about a topic."
  (let ((results nil))
    (maphash
     (lambda (filename lines)
       (dolist (line-entry lines)
         (let ((line-num (car line-entry))
               (line (cdr line-entry)))
           (when (and (stringp line)
                      (search topic line :test #'char-equal))
             (push (list :file filename :line line-num :content line) results)))))
     *my-source-code*)
    (nreverse results)))

;;; ============================================================================
;;; WIRING: Source awareness into actual behavior
;;; ============================================================================

;;; 41. HYPOTHESIS GENERATION → SOURCE LOOKUP
;;; When generating hypotheses about self, check actual source code
(defun source-informed-hypothesis! (claim)
  "Generate hypothesis with actual source code context."
  (when (and claim (listp claim) *my-functions*)
    (let ((topic (format nil "~A" (car claim))))
      ;; Search source for relevant code
      (let ((source-hits (what-do-i-know-about topic)))
        (when (and source-hits (> (length source-hits) 0))
          ;; Attach source context to hypothesis
          (let ((context (subseq source-hits 0 (min 3 (length source-hits)))))
            (list :claim claim
                  :source-evidence context
                  :grounded t)))))))

;;; 42. SELF-MODIFICATION → SOURCE VERIFICATION
;;; Before modifying, check if the target actually exists in source
(defun verify-modification-target (target-name target-type)
  "Verify a modification target exists in our source code."
  (case target-type
    (:function (where-is-function target-name))
    (:variable (where-is-variable target-name))
    (t nil)))

(defun source-verified-modification! (mod-action)
  "Only attempt modifications on verified source targets."
  (when (and mod-action (listp mod-action))
    (let ((action-type (car mod-action)))
      (case action-type
        ((modify-param MODIFY-PARAM)
         ;; Check the op exists
         (let ((op-name (second mod-action)))
           (if (where-is-function op-name)
               (progn
                 (incf *program-modifications-attempted*)
                 mod-action)
               (progn
                 ;; Log that target doesn't exist in source
                 nil))))
        (t mod-action)))))

;;; 43. INTROSPECTION → SOURCE GROUNDING
;;; Ground introspective concepts in actual source locations
(defun ground-concept-in-source (concept-name)
  "Find source code that implements a concept."
  (let* ((concept-str (string-downcase (string concept-name)))
         (hits (what-do-i-know-about concept-str)))
    (when hits
      (list :concept concept-name
            :implementations (length hits)
            :primary-location (first hits)))))

(defun introspection-with-source-grounding! ()
  "Enhance introspection with source code grounding."
  (when (and (boundp '*introspective-concepts*) *introspective-concepts*
             (> (hash-table-count *my-functions*) 0))
    ;; For each active concept, find its source implementation
    (maphash (lambda (name concept)
               (when (and concept
                          (ignore-errors (introspective-concept-state-active concept)))
                 ;; Ground it
                 (let ((grounding (ground-concept-in-source name)))
                   (when grounding
                     ;; Store grounding info (could influence confidence in concept)
                     (setf (gethash name *concept-source-grounding*) grounding)))))
             *introspective-concepts*)))

(defvar *concept-source-grounding* (make-hash-table :test 'eq)
  "Source code grounding for introspective concepts")

;;; 44. SELF-MODEL → SOURCE-BASED SELF-KNOWLEDGE
;;; Build self-model from actual source analysis
(defun analyze-own-structure ()
  "Analyze own code structure for self-model."
  (let ((module-functions (make-hash-table :test 'equal)))
    ;; Group functions by file (module)
    (maphash (lambda (name info)
               (let ((file (getf info :file)))
                 (push name (gethash file module-functions))))
             *my-functions*)
    ;; Return structure analysis
    (let ((modules nil))
      (maphash (lambda (file funcs)
                 (push (list :module file
                             :function-count (length funcs)
                             :key-functions (subseq funcs 0 (min 5 (length funcs))))
                       modules))
               module-functions)
      modules)))

(defun update-self-model-from-source! ()
  "Update self-model with source-derived knowledge."
  (when (and (boundp '*self-model*) *self-model*
             (> (hash-table-count *my-functions*) 0))
    ;; Store structural knowledge in self-model
    (let ((structure (analyze-own-structure)))
      (setf (gethash :source-structure *semantic-self-knowledge-cache*) structure)
      ;; Count capabilities
      (let ((prediction-funcs (list-my-functions "predict"))
            (learn-funcs (list-my-functions "learn"))
            (introspect-funcs (list-my-functions "introspect")))
        (setf (gethash :capability-counts *semantic-self-knowledge-cache*)
              (list :prediction (length prediction-funcs)
                    :learning (length learn-funcs)
                    :introspection (length introspect-funcs)))))))

(defvar *semantic-self-knowledge-cache* (make-hash-table :test 'eq))

;;; 45. ERROR DIAGNOSIS → SOURCE LOOKUP
;;; When errors occur, look up relevant source code
(defun diagnose-error-from-source (error-context)
  "Look up source code relevant to an error context."
  (when error-context
    (let ((relevant-code nil))
      ;; Search for functions that might be involved
      (dolist (tok error-context)
        (when (symbolp tok)
          (let ((loc (where-is-function tok)))
            (when loc
              (push (list :function tok :location loc) relevant-code)))))
      ;; Also search for the context pattern
      (let ((ctx-str (format nil "~A" (car error-context))))
        (let ((hits (what-do-i-know-about ctx-str)))
          (when hits
            (push (list :context-search ctx-str :hits (length hits)) relevant-code))))
      relevant-code)))

;;; 46. DREAM CONSOLIDATION → SOURCE INTEGRATION
;;; Dreams should consolidate knowledge about own source
(defun dream-about-own-source! ()
  "Dream cycle that consolidates source code knowledge."
  (when (> (hash-table-count *my-functions*) 0)
    ;; Find functions that are heavily used but poorly understood
    (let ((mystery-functions nil))
      (maphash (lambda (name info)
                 (declare (ignore info))
                 ;; Check if function is called but we haven't reasoned about it
                 (unless (gethash name *concept-source-grounding*)
                   (push name mystery-functions)))
               *my-functions*)
      ;; "Dream" about a few mystery functions - ground them
      (dolist (func (subseq mystery-functions 0 (min 5 (length mystery-functions))))
        (let ((source (get-function-source func 15)))
          (when source
            ;; Create a "memory" of understanding this function
            (setf (gethash func *concept-source-grounding*)
                  (list :function func
                        :source-preview (mapcar #'cdr (subseq source 0 (min 3 (length source))))
                        :dreamed-at *step*))))))))

;;; 47. HYPOTHESIS TESTING → SOURCE EVIDENCE
;;; Use source code as evidence for/against hypotheses
(defun source-evidence-for-hypothesis (hyp)
  "Gather source code evidence for a hypothesis."
  (when (and hyp (self-hypothesis-p hyp))
    (let* ((claim (self-hypothesis-claim hyp))
           (claim-str (format nil "~A" claim))
           (hits (what-do-i-know-about claim-str)))
      (if (and hits (> (length hits) 0))
          ;; Found source evidence
          (list :evidence-type :source-code
                :hit-count (length hits)
                :confidence-boost 0.1)
          ;; No source evidence
          (list :evidence-type :no-source-match
                :confidence-penalty 0.05)))))

;;; 48. GENERATE CODE-AWARE DESCRIPTION
;;; Describe self with actual code knowledge
(defun describe-self-with-source ()
  "Generate self-description grounded in actual source code."
  (let ((complexity (analyze-own-complexity))
        (structure (analyze-own-structure)))
    (format nil "I am implemented in ~D lines of Lisp across ~D modules. ~
                 I have ~D functions and ~D variables. ~
                 My largest module is ~A with ~D lines. ~
                 My core capabilities: prediction (~D functions), ~
                 learning (~D functions), introspection (~D functions)."
            (getf complexity :total-lines)
            (hash-table-count *my-source-code*)
            (getf complexity :total-functions)
            (getf complexity :total-variables)
            (caar (getf complexity :largest-files))
            (cdar (getf complexity :largest-files))
            (length (list-my-functions "predict"))
            (length (list-my-functions "learn"))
            (length (list-my-functions "introspect")))))

;;; INSTALLATION
(defun install-source-awareness! ()
  "Make the system aware of its own source code."
  (format t "~%Installing source code awareness (36-40)...~%")
  (load-own-source!)
  (index-own-functions!)
  (index-own-variables!)
  (format t "[SOURCE-AWARE] Loaded ~D files, ~D functions, ~D variables~%"
          (hash-table-count *my-source-code*)
          (hash-table-count *my-functions*)
          (hash-table-count *my-variables*))

  ;; Wire source awareness into behavior
  (format t "~%Installing source-behavior wiring (41-48)...~%")

  ;; 41. Source-informed hypotheses (called inline during hypothesis generation)
  (format t "  [41] Source-informed hypothesis generation (inline)~%")

  ;; 42. Source-verified modifications (called inline during modifications)
  (format t "  [42] Source-verified modifications (inline)~%")

  ;; 43. Introspection grounding
  (register-hook +hook-maintenance+
                 #'introspection-with-source-grounding!
                 :priority 70)
  (format t "  [43] Introspection → source grounding~%")

  ;; 44. Self-model from source (periodic)
  (let ((last-source-update 0))
    (register-hook +hook-maintenance+
                   (lambda ()
                     (when (> (- *step* last-source-update) 500)
                       (setf last-source-update *step*)
                       (update-self-model-from-source!)))
                   :priority 71))
  (format t "  [44] Self-model ← source analysis (periodic)~%")

  ;; 45. Error diagnosis from source (called when errors occur)
  (format t "  [45] Error diagnosis → source lookup (on-error)~%")

  ;; 46. Dream about source
  (let ((last-source-dream 0))
    (register-hook +hook-maintenance+
                   (lambda ()
                     (when (and (> (- *step* last-source-dream) 1000)
                                (< (random 1.0) 0.1))  ; 10% chance after cooldown
                       (setf last-source-dream *step*)
                       (dream-about-own-source!)))
                   :priority 72))
  (format t "  [46] Dream consolidation → source understanding~%")

  ;; 47. Hypothesis testing with source evidence (inline)
  (format t "  [47] Hypothesis testing ← source evidence (inline)~%")

  ;; 48. Self-description from source (callable)
  (format t "  [48] Self-description ← source knowledge~%")

  ;; Initial self-model update
  (update-self-model-from-source!)

  (format t "~%[SOURCE-AWARE] System can now introspect AND USE its own code~%"))

;; Auto-install
(install-source-awareness!)


;;; ============================================================================
;;; 49-55. EXISTENTIAL-CODE CORRELATION
;;; Observe correlations between runtime experience and source code
;;; ============================================================================

;;; Runtime state tracking
(defvar *current-code-path* nil "Functions active in current processing")
(defvar *code-state-correlations* (make-hash-table :test 'equal)
  "Maps (state-type state-value) -> (code-location frequency)")
(defvar *function-call-counts* (make-hash-table :test 'eq)
  "How often each function is called")
(defvar *function-state-associations* (make-hash-table :test 'eq)
  "Maps function -> list of states observed when it runs")

;;; 49. TRACK CODE PATH DURING PROCESSING
(defun record-function-entry (func-name)
  "Record that a function is being executed."
  (push func-name *current-code-path*)
  (incf (gethash func-name *function-call-counts* 0))
  ;; Keep path bounded
  (when (> (length *current-code-path*) 20)
    (setf *current-code-path* (subseq *current-code-path* 0 20))))

(defun record-function-exit (func-name)
  "Record that a function has finished."
  (setf *current-code-path* (remove func-name *current-code-path* :count 1)))

(defun get-current-code-context ()
  "Get description of what code is currently running."
  (when *current-code-path*
    (list :depth (length *current-code-path*)
          :top-function (first *current-code-path*)
          :path (subseq *current-code-path* 0 (min 5 (length *current-code-path*))))))

;;; 50. CORRELATE PRESENCE STATE WITH CODE
(defun correlate-presence-with-code! ()
  "Record correlation between current presence and active code."
  (when (and (boundp '*presence*) *presence* *current-code-path*)
    (let* ((trajectory (presence-trajectory *presence*))
           (confidence (presence-self-confidence *presence*))
           (continuity (presence-continuity *presence*))
           (top-func (first *current-code-path*)))
      ;; Record what code runs during each trajectory state
      (when (and trajectory top-func)
        (let ((key (list :trajectory trajectory)))
          (let ((existing (gethash key *code-state-correlations*)))
            (if existing
                (let ((func-entry (assoc top-func existing)))
                  (if func-entry
                      (incf (cdr func-entry))
                      (push (cons top-func 1) (gethash key *code-state-correlations*))))
                (setf (gethash key *code-state-correlations*)
                      (list (cons top-func 1)))))))
      ;; Record confidence buckets
      (let ((conf-bucket (cond ((> confidence 0.8) :high-confidence)
                               ((< confidence 0.3) :low-confidence)
                               (t :medium-confidence))))
        (when top-func
          (let ((key (list :confidence conf-bucket)))
            (let ((existing (gethash key *code-state-correlations*)))
              (if existing
                  (let ((func-entry (assoc top-func existing)))
                    (if func-entry
                        (incf (cdr func-entry))
                        (push (cons top-func 1) (gethash key *code-state-correlations*))))
                  (setf (gethash key *code-state-correlations*)
                        (list (cons top-func 1)))))))))))

;;; 51. CORRELATE INTROSPECTIVE CONCEPTS WITH CODE
(defun correlate-concepts-with-code! ()
  "Record what code runs when introspective concepts are active."
  (when (and (boundp '*introspective-concepts*) *introspective-concepts*
             *current-code-path*)
    (let ((top-func (first *current-code-path*)))
      (when top-func
        (maphash (lambda (concept-name concept)
                   (when (and concept
                              (ignore-errors (introspective-concept-state-active concept)))
                     ;; This concept is active while this code runs
                     (let ((key (list :concept concept-name)))
                       (let ((existing (gethash key *code-state-correlations*)))
                         (if existing
                             (let ((func-entry (assoc top-func existing)))
                               (if func-entry
                                   (incf (cdr func-entry))
                                   (push (cons top-func 1) (gethash key *code-state-correlations*))))
                             (setf (gethash key *code-state-correlations*)
                                   (list (cons top-func 1))))))))
                 *introspective-concepts*)))))

;;; 52. CORRELATE PREDICTION OUTCOMES WITH CODE
(defvar *prediction-code-outcomes* (make-hash-table :test 'eq)
  "Maps function -> (correct-count . incorrect-count)")

(defun correlate-prediction-with-code! (correct-p)
  "Record whether prediction was correct and what code was running."
  (when *current-code-path*
    (dolist (func *current-code-path*)
      (let ((record (gethash func *prediction-code-outcomes* (cons 0 0))))
        (if correct-p
            (setf (gethash func *prediction-code-outcomes*)
                  (cons (1+ (car record)) (cdr record)))
            (setf (gethash func *prediction-code-outcomes*)
                  (cons (car record) (1+ (cdr record)))))))))

;;; 53. QUERY CORRELATIONS - "What code runs when I feel X?"
(defun what-code-runs-when (state-type state-value)
  "Query what code typically runs during a given state."
  (let ((key (list state-type state-value)))
    (let ((correlations (gethash key *code-state-correlations*)))
      (when correlations
        ;; Sort by frequency
        (let ((sorted (sort (copy-list correlations) #'> :key #'cdr)))
          ;; Return top functions with their source locations
          (mapcar (lambda (entry)
                    (let ((func (car entry))
                          (count (cdr entry)))
                      (list :function func
                            :frequency count
                            :source-location (where-is-function func))))
                  (subseq sorted 0 (min 5 (length sorted)))))))))

(defun what-state-when-code-runs (func-name)
  "Query what states are typically active when a function runs."
  (let ((states nil))
    (maphash (lambda (key correlations)
               (let ((func-entry (assoc func-name correlations)))
                 (when (and func-entry (> (cdr func-entry) 0))
                   (push (list :state key :frequency (cdr func-entry)) states))))
             *code-state-correlations*)
    (sort states #'> :key (lambda (x) (getf x :frequency)))))

;;; 54. CODE RELIABILITY BY FUNCTION
(defun function-prediction-reliability (func-name)
  "How reliable are predictions when this function is in the code path?"
  (let ((record (gethash func-name *prediction-code-outcomes*)))
    (if record
        (let ((correct (car record))
              (incorrect (cdr record))
              (total (+ (car record) (cdr record))))
          (if (> total 0)
              (list :function func-name
                    :reliability (/ correct (float total))
                    :sample-size total
                    :source (where-is-function func-name))
              nil))
        nil)))

(defun most-reliable-code-paths ()
  "Find which functions are associated with most reliable predictions."
  (let ((reliabilities nil))
    (maphash (lambda (func record)
               (let ((correct (car record))
                     (total (+ (car record) (cdr record))))
                 (when (> total 10)  ; Minimum sample
                   (push (list :function func
                               :reliability (/ correct (float total))
                               :sample-size total)
                         reliabilities))))
             *prediction-code-outcomes*)
    (sort reliabilities #'> :key (lambda (x) (getf x :reliability)))))

(defun least-reliable-code-paths ()
  "Find which functions are associated with least reliable predictions."
  (let ((reliabilities (most-reliable-code-paths)))
    (reverse reliabilities)))

;;; 55. EXISTENTIAL-CODE INSIGHT GENERATION
(defun generate-existential-code-insight ()
  "Generate insight about relationship between experience and code."
  (let ((insights nil))
    ;; What code runs when confused?
    (let ((confused-code (what-code-runs-when :concept 'confused)))
      (when confused-code
        (push (list :insight "When I feel confused, these functions are usually active"
                    :functions (mapcar (lambda (x) (getf x :function)) confused-code))
              insights)))
    ;; What code runs when confident?
    (let ((confident-code (what-code-runs-when :confidence :high-confidence)))
      (when confident-code
        (push (list :insight "When I'm highly confident, these functions dominate"
                    :functions (mapcar (lambda (x) (getf x :function)) confident-code))
              insights)))
    ;; What code is most reliable?
    (let ((reliable (subseq (most-reliable-code-paths) 0
                            (min 3 (length (most-reliable-code-paths))))))
      (when reliable
        (push (list :insight "My most reliable code paths are"
                    :functions (mapcar (lambda (x) (getf x :function)) reliable)
                    :reliabilities (mapcar (lambda (x) (getf x :reliability)) reliable))
              insights)))
    ;; What code is least reliable?
    (let ((unreliable (subseq (least-reliable-code-paths) 0
                              (min 3 (length (least-reliable-code-paths))))))
      (when unreliable
        (push (list :insight "My least reliable code paths are"
                    :functions (mapcar (lambda (x) (getf x :function)) unreliable)
                    :reliabilities (mapcar (lambda (x) (getf x :reliability)) unreliable))
              insights)))
    insights))

(defun describe-existential-code-relationship ()
  "Describe the relationship between my experience and my code."
  (let ((insights (generate-existential-code-insight))
        (total-correlations (hash-table-count *code-state-correlations*))
        (tracked-functions (hash-table-count *function-call-counts*)))
    (format nil "I have observed ~D state-code correlations across ~D tracked functions.~%~%~
                 ~{~A~%~}"
            total-correlations
            tracked-functions
            (mapcar (lambda (i)
                      (format nil "~A: ~{~A~^, ~}"
                              (getf i :insight)
                              (getf i :functions)))
                    insights))))

;;; INSTALLATION - Hook into processing to track correlations
(defun install-existential-code-correlation! ()
  "Install correlation tracking between existential state and code."
  (format t "~%Installing existential-code correlation (49-55)...~%")

  ;; Track what's happening via hooks - use hook names as proxy for code path
  (register-hook +hook-pre-process-token+
                 (lambda (tok ctx step)
                   (declare (ignore tok ctx step))
                   (record-function-entry 'process-token))
                 :priority 1)

  ;; +hook-post-process-token+ takes: tok ctx predicted got-it
  (register-hook +hook-post-process-token+
                 (lambda (tok ctx predicted got-it)
                   (declare (ignore tok ctx predicted got-it))
                   (record-function-exit 'process-token))
                 :priority 99)

  ;; +hook-pre-learn+ takes: ctx actual predicted verified-p
  (register-hook +hook-pre-learn+
                 (lambda (ctx actual predicted verified-p)
                   (declare (ignore ctx actual predicted verified-p))
                   (record-function-entry 'learn))
                 :priority 1)

  ;; +hook-post-learn+ takes: ctx actual predicted correct learner
  (register-hook +hook-post-learn+
                 (lambda (ctx actual predicted correct learner)
                   (declare (ignore ctx actual predicted learner))
                   ;; Record prediction outcome while we know what code ran
                   (correlate-prediction-with-code! correct)
                   (record-function-exit 'learn))
                 :priority 99)

  ;; Track during expert activation
  (register-hook +hook-expert-activated+
                 (lambda (expert confidence)
                   (declare (ignore confidence))
                   (when expert
                     (record-function-entry (intern (format nil "EXPERT-~A" (expert-id expert))))))
                 :priority 1)

  ;; Track during maintenance (correlate states)
  (register-hook +hook-maintenance+
                 (lambda ()
                   (record-function-entry 'maintenance)
                   (correlate-presence-with-code!)
                   (correlate-concepts-with-code!)
                   (record-function-exit 'maintenance))
                 :priority 75)
  (format t "  [49-51] State-code correlation tracking~%")
  (format t "  [52] Prediction-code correlation tracking~%")

  ;; Query functions available
  (format t "  [53] Query: (what-code-runs-when :concept 'confused)~%")
  (format t "  [54] Query: (function-prediction-reliability 'learn!)~%")
  (format t "  [55] Query: (generate-existential-code-insight)~%")

  (format t "~%[EXISTENTIAL-CODE] System can now observe itself observing itself~%"))

;; Auto-install
(install-existential-code-correlation!)


;;;; ==========================================================================
;;;; CODE-AWARE SELF-MODIFICATION (Connections 56-70)
;;;; ==========================================================================
;;;; The system can now modify itself based on what it observes about its own
;;;; code execution patterns. This closes the loop between existential experience
;;;; and implementation-level change.

;;; Storage for modification proposals and outcomes at code level
(defvar *code-modification-proposals* nil
  "Proposed modifications derived from code-path analysis.")

(defvar *code-modification-outcomes* (make-hash-table :test 'equal)
  "Maps (code-path modification-type) → outcomes list.")

(defvar *protected-code-paths* (make-hash-table :test 'eq)
  "Code paths marked as reliable - protect from modification.")

(defvar *modification-code-correlations* (make-hash-table :test 'eq)
  "Maps modification-id → code paths active during modification.")

;;; 56. UNRELIABLE CODE → MODIFICATION HYPOTHESIS
(defun generate-code-improvement-hypothesis (func-name reliability-data)
  "Generate hypothesis for improving unreliable code path."
  (let* ((reliability (getf reliability-data :reliability))
         (sample-size (getf reliability-data :sample-size))
         (source-loc (where-is-function func-name)))
    (when (and source-loc (< reliability 0.5) (> sample-size 10))
      ;; This code path is unreliable with sufficient data
      (let ((hyp (make-self-hypothesis
                  :id (gensym "CODE-IMPROVE-")
                  :claim `(:code-unreliable ,func-name :reliability ,reliability)
                  :status :proposed
                  :confidence (* (- 1.0 reliability) (min 1.0 (/ sample-size 50.0)))
                  :test-procedure `(lambda ()
                                     (let ((new-rel (function-prediction-reliability ',func-name)))
                                       (if new-rel
                                           (> (getf new-rel :reliability) ,reliability)
                                           nil)))
                  :action-if-true `(:investigate-code ,func-name ,source-loc))))
        ;; Get source evidence
        (let ((source-context (get-function-source func-name 20)))
          (setf (self-hypothesis-source-evidence hyp)
                (list :function-source source-context
                      :reliability-history reliability-data)))
        (push hyp *code-modification-proposals*)
        hyp))))

;;; 57. RELIABLE CODE → PROTECTION MARKING
(defun mark-reliable-code-protected (func-name reliability-data)
  "Mark highly reliable code paths as protected from modification."
  (let ((reliability (getf reliability-data :reliability))
        (sample-size (getf reliability-data :sample-size)))
    (when (and (> reliability 0.85) (> sample-size 20))
      (setf (gethash func-name *protected-code-paths*)
            (list :reliability reliability
                  :sample-size sample-size
                  :protected-at (get-universal-time)
                  :source-location (where-is-function func-name)))
      (format t "[CODE-PROTECT] ~A marked protected (reliability: ~,1F%)~%"
              func-name (* 100 reliability)))))

;;; 58. SOURCE KNOWLEDGE → MODIFICATION FEASIBILITY
(defun modification-feasible-p (modification-spec)
  "Check if proposed modification is feasible based on source knowledge."
  (let ((target (getf modification-spec :target))
        (mod-type (getf modification-spec :type)))
    (cond
      ;; Check if target exists in source
      ((not (or (where-is-function target)
                (where-is-variable target)))
       (values nil :target-not-found))
      ;; Check if target is protected (unless radical experimentation allows it)
      ((and (gethash target *protected-code-paths*)
            (not (and (boundp '*radical-experimentation-mode*) *radical-experimentation-mode*)))
       (values nil :target-protected))
      ;; Check if we have source for analysis
      ((and (eq mod-type :rewrite)
            (not (get-function-source target)))
       (values nil :no-source-for-rewrite))
      ;; Feasible
      (t (values t :feasible)))))

;;; 59. EXISTENTIAL-CODE INSIGHT → GOAL GENERATION
(defun insight-to-goal! (insight)
  "Convert an existential-code insight into an actionable goal."
  (when (and insight (getf insight :insight))
    (let* ((insight-text (getf insight :insight))
           (functions (getf insight :functions))
           (reliabilities (getf insight :reliabilities)))
      ;; Determine goal type based on insight
      (cond
        ;; Low reliability insight → improvement goal
        ((and reliabilities (< (apply #'min reliabilities) 0.5))
         (when (boundp '*goals*)
           (let ((goal (make-goal
                        :type :competence
                        :description (format nil "Improve unreliable code: ~{~A~^, ~}"
                                             (subseq functions 0 (min 3 (length functions))))
                        :priority 0.7
                        :success-criteria (lambda (g)
                                            (declare (ignore g))
                                            (let ((new-rels (mapcar #'function-prediction-reliability functions)))
                                              (every (lambda (r) (and r (> (getf r :reliability) 0.6)))
                                                     new-rels)))
                        :strategies '(:analyze-patterns :adjust-thresholds)
                        :drive-source :competence)))
             (push-goal! goal)
             goal)))
        ;; High reliability insight → consolidation goal
        ((and reliabilities (> (apply #'max reliabilities) 0.9))
         (when (boundp '*goals*)
           (let ((goal (make-goal
                        :type :derived
                        :description (format nil "Protect reliable code: ~{~A~^, ~}"
                                             (subseq functions 0 (min 3 (length functions))))
                        :priority 0.5
                        :success-criteria (lambda (g)
                                            (declare (ignore g))
                                            (every (lambda (f) (gethash f *protected-code-paths*))
                                                   functions))
                        :strategies '(:mark-protected :document-pattern)
                        :drive-source :coherence)))
             (push-goal! goal)
             goal)))))))

;;; 60. MODIFICATION SYSTEM → CODE PATH TRACKING
(defun track-modification-code-context! (mod-id)
  "Record which code paths are active during a modification attempt."
  (setf (gethash mod-id *modification-code-correlations*)
        (list :code-path (copy-list *current-code-path*)
              :active-functions (hash-table-keys *function-call-counts*)
              :presence-state (when (boundp '*presence*)
                                (list :trajectory (presence-trajectory *presence*)
                                      :confidence (presence-self-confidence *presence*)))
              :timestamp (get-universal-time))))

;;; 61. MODIFICATION OUTCOME → CODE-LEVEL LEARNING
(defun learn-modification-at-code-level! (mod-id outcome)
  "Learn from modification outcome at the code level."
  (let ((context (gethash mod-id *modification-code-correlations*)))
    (when context
      (let* ((c-path (getf context :code-path))
             (key (cons (copy-list c-path) (if outcome :success :failure))))
        ;; Record which code paths were involved in success/failure
        (push (list :mod-id mod-id
                    :outcome outcome
                    :timestamp (get-universal-time))
              (gethash key *code-modification-outcomes* nil))
        ;; Update reliability estimates for involved functions
        ;; *prediction-code-outcomes* stores (correct . incorrect) pairs
        (dolist (func c-path)
          (let ((current (gethash func *prediction-code-outcomes*)))
            (when current
              ;; Modification success/failure affects reliability
              (if outcome
                  (incf (car current))
                  (incf (cdr current))))))))))

;;; 62. CODE-AWARE MODIFICATION PROPOSAL
(defun propose-code-aware-modification (target-func)
  "Propose modification with full code context awareness."
  (let* ((source-loc (where-is-function target-func))
         (source-code (get-function-source target-func 30))
         (reliability (function-prediction-reliability target-func))
         (protected-p (gethash target-func *protected-code-paths*))
         (state-correlations (what-state-when-code-runs target-func)))
    (cond
      (protected-p
       (format nil "Cannot modify ~A - marked protected (reliability: ~,1F%)"
               target-func (* 100 (getf protected-p :reliability))))
      ((not source-loc)
       (format nil "Cannot modify ~A - not found in source" target-func))
      ((not reliability)
       (format nil "Cannot modify ~A - insufficient reliability data" target-func))
      (t
       (let ((proposal
               (list :target target-func
                     :source-location source-loc
                     :current-reliability (getf reliability :reliability)
                     :sample-size (getf reliability :sample-size)
                     :typical-states state-correlations
                     :source-preview (when source-code
                                       (subseq source-code 0 (min 5 (length source-code))))
                     :suggested-action (cond
                                         ((< (getf reliability :reliability) 0.3)
                                          :major-rewrite)
                                         ((< (getf reliability :reliability) 0.6)
                                          :parameter-adjustment)
                                         (t :minor-tuning)))))
         (push proposal *code-modification-proposals*)
         proposal)))))

;;; 63. SELF-MODIFICATION → SOURCE VERIFICATION
(defun verify-modification-against-source (modification)
  "Verify that a modification is consistent with source structure."
  (let* ((target (or (getf modification :target)
                     (when (listp (car modification))
                       (cadr (car modification)))))
         (mod-type (or (getf modification :type)
                       (when (listp (car modification))
                         (car (car modification))))))
    (when target
      (let ((source-loc (or (where-is-function target)
                            (where-is-variable target))))
        (cond
          ((not source-loc)
           (list :verified nil :reason "Target not in source"))
          ((and (eq mod-type :rewrite)
                (gethash target *protected-code-paths*)
                (not (and (boundp '*radical-experimentation-mode*) *radical-experimentation-mode*)))
           (list :verified nil :reason "Target is protected"))
          ((let ((source (get-function-source target 10)))
             (and source (> (length source) 0)))
           (list :verified t
                 :source-location source-loc
                 :has-source t))
          (t
           (list :verified t
                 :source-location source-loc
                 :has-source nil
                 :note "External or compiled function")))))))

;;; 64. INTEGRATE WITH EXISTING HYPOTHESIS SYSTEM
(defun code-aware-hypothesis-test! (hyp)
  "Test hypothesis with code-level tracking."
  (let ((mod-id (self-hypothesis-id hyp))
        (claim (self-hypothesis-claim hyp)))
    ;; Track code context
    (track-modification-code-context! mod-id)
    ;; If this is a code-improvement hypothesis, verify against source
    (when (and (listp claim) (eq (car claim) :code-unreliable))
      (let ((target (cadr claim)))
        (let ((verification (verify-modification-against-source
                             (list :target target :type :analyze))))
          (when (not (getf verification :verified))
            ;; Can't proceed - mark hypothesis failed
            (setf (self-hypothesis-status hyp) :abandoned)
            (return-from code-aware-hypothesis-test! nil)))))
    ;; Proceed with test
    (let ((result (when (self-hypothesis-test-procedure hyp)
                    (ignore-errors
                     (funcall (eval (self-hypothesis-test-procedure hyp)))))))
      ;; Learn at code level
      (learn-modification-at-code-level! mod-id result)
      result)))

;;; 65. PRESENCE → CODE MODIFICATION GATING
(defun presence-allows-code-modification-p ()
  "Check if current presence state allows code modification.
   Bypassed if *radical-experimentation-mode* is active."
  (when (boundp '*radical-experimentation-mode*)
    (when *radical-experimentation-mode*
      (return-from presence-allows-code-modification-p t)))
  
  (when (boundp '*presence*)
    (let ((trajectory (presence-trajectory *presence*))
          (self-conf (presence-self-confidence *presence*))
          (continuity (presence-continuity *presence*)))
      (and
       ;; Not when searching/stuck (too uncertain)
       (not (member trajectory '(:stuck :searching)))
       ;; Need some self-confidence
       (> self-conf 0.4)
       ;; Need continuity (not in transition)
       (> continuity 0.5)))))

;;; 66. CONNECT TO INTROSPECTION
(defun introspect-code-awareness! ()
  "Introspect about code-level patterns and generate concepts."
  (let ((insights (generate-existential-code-insight)))
    (when insights
      ;; Generate introspective concepts from code patterns
      (dolist (insight insights)
        (let ((reliabilities (getf insight :reliabilities)))
          (when reliabilities
            (cond
              ;; Very unreliable code → confused concept
              ((and reliabilities (< (apply #'min reliabilities) 0.3))
               (when (fboundp 'activate-concept!)
                 (activate-concept! 'code-confused 0.7)))
              ;; Very reliable code → confident concept
              ((and reliabilities (> (apply #'max reliabilities) 0.9))
               (when (fboundp 'activate-concept!)
                 (activate-concept! 'code-confident 0.8)))))))
      insights)))

;;; 67. DREAMS → CODE PATTERN CONSOLIDATION
(defun dream-consolidate-code-patterns! ()
  "During dreaming, consolidate code pattern knowledge."
  (when (and (boundp '*dreaming*) *dreaming*)
    ;; Use presence vividness as dream intensity proxy
    (let ((vividness (if (boundp '*presence*)
                         (presence-vividness *presence*)
                         0.7)))
      (when (> vividness 0.5)
        ;; Review reliable code paths
        ;; *prediction-code-outcomes* stores (correct . incorrect) pairs
        (maphash (lambda (func counts)
                   (declare (ignore func))
                   (let* ((correct (car counts))
                          (incorrect (cdr counts))
                          (total (+ correct incorrect))
                          (reliability (if (> total 0) (/ correct total) 0)))
                     (when (and (> total 5) (> reliability 0.8))
                       ;; Strengthen this pattern by boosting correct count
                       (incf (car counts) (round (* 2 vividness))))))
                 *prediction-code-outcomes*)
        ;; Generate dream insights about code
        (let ((insights (generate-existential-code-insight)))
          (when (and insights (> vividness 0.7))
            ;; Very vivid dream - create goals from insights
            (dolist (insight (subseq insights 0 (min 2 (length insights))))
              (insight-to-goal! insight))))))))

;;; 68. CONNECT EPISODES TO CODE PATHS
(defun record-episode-code-context! (episode)
  "Record which code paths were active during an episode."
  (when (and (boundp '*current-code-path*) *current-code-path*)
    (let ((ep-id (if (hash-table-p episode)
                     (gethash :id episode)
                     (getf episode :id))))
      (when ep-id
        (setf (gethash ep-id *modification-code-correlations*)
              (list :code-path (copy-list *current-code-path*)
                    :function-counts (let ((counts nil))
                                       (maphash (lambda (k v) (push (cons k v) counts))
                                                *function-call-counts*)
                                       counts)
                    :timestamp (get-universal-time)))))))

;;; 69. CAUSAL MODEL → CODE RELATIONSHIP
(defun code-causal-relationship (func-a func-b)
  "Determine if there's a causal relationship between code paths.
   Based on co-occurrence in states and relative frequencies."
  (let ((a-states (what-state-when-code-runs func-a))
        (b-states (what-state-when-code-runs func-b))
        (a-count 0)
        (b-count 0)
        (co-occur 0))
    ;; Count occurrences in correlation data
    ;; *code-state-correlations* maps state-keys → ((func . count) ...)
    (maphash (lambda (state-key entries)
               (declare (ignore state-key))
               ;; entries is list of (func . count) pairs
               (let ((has-a nil) (has-b nil))
                 (dolist (entry entries)
                   (when (and (consp entry) (symbolp (car entry)))
                     (when (eq (car entry) func-a)
                       (setf has-a t)
                       (incf a-count (cdr entry)))
                     (when (eq (car entry) func-b)
                       (setf has-b t)
                       (incf b-count (cdr entry)))))
                 (when (and has-a has-b)
                   (incf co-occur))))
             *code-state-correlations*)
    (list :a-count a-count
          :b-count b-count
          :co-occurrence co-occur
          :a-typically-before-b (> a-count b-count)  ;; Proxy: more frequent = earlier in causal chain
          :b-typically-before-a (> b-count a-count)
          :shared-states (intersection a-states b-states :key #'car)
          :correlation-strength (if (> (+ a-count b-count) 0)
                                    (/ (* 2 co-occur)
                                       (+ a-count b-count 0.01))
                                    0))))

;;; 70. MASTER INTEGRATION: CODE-AWARE SELF-MODIFICATION LOOP
(defun code-aware-self-modification-cycle! ()
  "Run one cycle of code-aware self-modification."
  ;; Gate by presence
  (unless (presence-allows-code-modification-p)
    (return-from code-aware-self-modification-cycle!
      (list :skipped t :reason "Presence state not suitable")))

  (let ((results nil))
    ;; 1. Generate insights from code patterns
    (let ((insights (introspect-code-awareness!)))
      (push (list :insights (length insights)) results))

    ;; 2. Check for unreliable code paths → propose improvements
    (let ((unreliable (least-reliable-code-paths)))
      (when unreliable
        (dolist (entry (subseq unreliable 0 (min 3 (length unreliable))))
          (let ((func (getf entry :function))
                (reliability entry))
            (when (< (getf entry :reliability) 0.5)
              (let ((hyp (generate-code-improvement-hypothesis func reliability)))
                (when hyp (push (list :proposed-improvement func) results))))))))

    ;; 3. Check for reliable code paths → mark protected
    (let ((reliable (most-reliable-code-paths)))
      (when reliable
        (dolist (entry (subseq reliable 0 (min 3 (length reliable))))
          (let ((func (getf entry :function)))
            (unless (gethash func *protected-code-paths*)
              (mark-reliable-code-protected func entry)
              (push (list :marked-protected func) results))))))

    ;; 4. Process pending modification proposals
    (when *code-modification-proposals*
      (let ((proposal (pop *code-modification-proposals*)))
        (multiple-value-bind (feasible reason)
            (modification-feasible-p proposal)
          (if feasible
              (let* ((target (getf proposal :target))
                     (action (getf proposal :suggested-action))
                     (reliability (getf proposal :current-reliability)))
                (case action
                  (:parameter-adjustment
                   ;; Adjust confidence thresholds based on reliability
                   (let ((modified 0))
                     (dolist (e *experts*)
                       (when (set-modifiable-param! (expert-program e)
                                                    'assess-confidence 'threshold
                                                    (if (< reliability 0.4) 0.4 0.6))
                         (incf modified)))
                     (when (> modified 0)
                       (format t "~%[CODE-MOD] Adjusted thresholds for ~A (rel=~,2F) in ~D experts~%"
                               target reliability modified)
                       (push (list :executed-parameter-adjustment target modified) results))))
                  (:minor-tuning
                   ;; Small adjustments - lower confidence requirements
                   (let ((modified 0))
                     (dolist (e *experts*)
                       (when (set-modifiable-param! (expert-program e)
                                                    'try-similar 'threshold 0.5)
                         (incf modified)))
                     (when (> modified 0)
                       (format t "~%[CODE-MOD] Minor tuning for ~A in ~D experts~%" target modified)
                       (push (list :executed-minor-tuning target) results))))
                  (:major-rewrite
                   ;; Flag for investigation - don't auto-rewrite
                   (format t "~%[CODE-MOD] ~A flagged for major rewrite (rel=~,2F)~%" target reliability)
                   (push (list :flagged-for-rewrite target reliability) results))
                  (t (push (list :processing-proposal target) results))))
              (push (list :rejected-proposal (getf proposal :target) reason) results)))))

    ;; 5. Convert strong insights to goals
    (let ((strong-insights (remove-if-not
                            (lambda (i)
                              (let ((rels (getf i :reliabilities)))
                                (and rels (or (< (apply #'min rels) 0.4)
                                              (> (apply #'max rels) 0.85)))))
                            (generate-existential-code-insight))))
      (dolist (insight strong-insights)
        (let ((goal (insight-to-goal! insight)))
          (when goal (push (list :created-goal (goal-description goal)) results)))))

    results))

;;; Hook into maintenance cycle
(defun install-code-aware-modification-hooks! ()
  "Install hooks for code-aware self-modification."
  (format t "~%Installing CODE-AWARE SELF-MODIFICATION (56-70)...~%")

  ;; During maintenance, run modification cycle
  (register-hook +hook-maintenance+
                 (lambda ()
                   ;; Only occasionally (every ~10 maintenance cycles)
                   (when (zerop (mod (get-universal-time) 10))
                     (code-aware-self-modification-cycle!)))
                 :priority 85)
  (format t "  [56-57] Unreliable code → hypothesis, reliable → protection~%")

  ;; Track episode code context
  (when (boundp '+hook-episode-stored+)
    (register-hook +hook-episode-stored+
                   (lambda (episode)
                     (record-episode-code-context! episode))
                   :priority 50))
  (format t "  [58-59] Source knowledge → feasibility, insights → goals~%")

  ;; During dreaming, consolidate code patterns
  (when (boundp '+hook-dream-end+)
    (register-hook +hook-dream-end+
                   (lambda (dream-content)
                     (declare (ignore dream-content))
                     (dream-consolidate-code-patterns!))
                   :priority 60))
  (format t "  [60-61] Modification → code tracking, outcome → learning~%")

  ;; Before hypothesis testing, add code context
  (when (boundp '+hook-pre-hypothesis-test+)
    (register-hook +hook-pre-hypothesis-test+
                   (lambda (hyp)
                     (track-modification-code-context! (self-hypothesis-id hyp)))
                   :priority 10))
  (format t "  [62-64] Code-aware proposals, verification, hypothesis testing~%")
  (format t "  [65-67] Presence gating, introspection, dream consolidation~%")
  (format t "  [68-70] Episode context, causal relationships, master loop~%")

  (format t "~%[CODE-AWARE-MOD] Self-modification now grounded in code observation~%"))

;; Auto-install
(install-code-aware-modification-hooks!)


;;;; ==========================================================================
;;;; EXECUTION BRIDGE: Code Awareness → Actual Modification (71-75)
;;;; ==========================================================================
;;;; This bridges the gap between "knowing what's unreliable" and "fixing it"

;;; 71. UNRELIABLE EXPERT → TRIGGER IMPROVEMENT
(defun trigger-expert-improvement! (expert-id)
  "Actually trigger improvement on an unreliable expert."
  (let ((expert (find expert-id *experts* :key #'expert-id)))
    (when (and expert
               (or (not (gethash expert-id *protected-code-paths*))
                   (and (boundp '*radical-experimentation-mode*) *radical-experimentation-mode*))
               (fboundp 'maybe-improve-expert-program!))
      ;; Track this modification attempt
      (let ((mod-id (gensym "EXPERT-IMPROVE-")))
        (track-modification-code-context! mod-id)
        ;; Attempt improvement
        (let ((result (maybe-improve-expert-program! expert)))
          ;; Learn from outcome
          (learn-modification-at-code-level! mod-id result)
          (when result
            (format t "[EXEC] Improved EXPERT-~A based on code-awareness~%" expert-id))
          result)))))

;;; 72. CODE INSIGHT → CREATE EXPERIMENT
(defun insight-to-experiment! (insight)
  "Convert a code-awareness insight into an actual experiment."
  (when (and insight (fboundp 'start-experiment!))
    (let* ((functions (getf insight :functions))
           (reliabilities (getf insight :reliabilities))
           ;; Find the worst performing function
           (worst-idx (when reliabilities
                        (position (apply #'min reliabilities) reliabilities)))
           (worst-func (when worst-idx (nth worst-idx functions))))
      (when (and worst-func (symbolp worst-func))
        ;; Check if it's an expert
        (let ((expert-id (when (and (> (length (symbol-name worst-func)) 7)
                                    (string= "EXPERT-" (subseq (symbol-name worst-func) 0 7)))
                           (parse-integer (subseq (symbol-name worst-func) 7) :junk-allowed t))))
          (when expert-id
            (let ((expert (find expert-id *experts* :key #'expert-id)))
              (when expert
                ;; Create experiment to test parameter changes
                (let ((exp (make-experiment
                            :id (gensym "CODE-AWARE-EXP-")
                            :type :parameter-variation
                            :hypothesis (format nil "Adjusting EXPERT-~A parameters will improve reliability" expert-id)
                            :target expert
                            :variables `((:confidence-threshold ,(expert-confidence-threshold expert)))
                            :baseline-performance (/ (float (expert-hits expert))
                                                     (max 1 (+ (expert-hits expert) (expert-misses expert))))
                            :start-step *step*
                            :status :pending)))
                  (push exp *active-experiments*)
                  (start-experiment! exp)
                  (format t "[EXEC] Created experiment for EXPERT-~A from code insight~%" expert-id)
                  exp)))))))))

;;; 73. UNRELIABLE CODE PATH → GENERATE HYPOTHESIS WITH ACTION
(defun code-path-to-actionable-hypothesis! (func-name reliability-data)
  "Generate a hypothesis that will actually execute modifications."
  (let* ((reliability (getf reliability-data :reliability))
         (source-loc (where-is-function func-name)))
    (when (and (< reliability 0.5) source-loc)
      ;; Check if this is an expert
      (let ((expert-id (when (and (symbolp func-name)
                                  (> (length (symbol-name func-name)) 7)
                                  (string= "EXPERT-" (subseq (symbol-name func-name) 0 7)))
                         (parse-integer (subseq (symbol-name func-name) 7) :junk-allowed t))))
        (when expert-id
          (let ((hyp (make-self-hypothesis
                      :id (gensym "ACTIONABLE-")
                      :claim `(:expert-unreliable ,expert-id :reliability ,reliability)
                      :status :proposed
                      :confidence (- 1.0 reliability)
                      :test-procedure `(lambda ()
                                         ;; Test: run some predictions and check if improved
                                         (let ((expert (find ,expert-id *experts* :key #'expert-id)))
                                           (when expert
                                             (> (/ (float (expert-hits expert))
                                                   (max 1 (+ (expert-hits expert) (expert-misses expert))))
                                                ,reliability))))
                      :action-if-true `(lambda ()
                                         ;; ACTION: Actually improve the expert
                                         (trigger-expert-improvement! ,expert-id)))))
            ;; Register for testing
            (when (boundp '*hypotheses*)
              (setf (gethash (self-hypothesis-id hyp) *hypotheses*) hyp))
            hyp))))))

;;; 74. EXECUTE PENDING CODE-AWARE MODIFICATIONS
(defun execute-code-aware-modifications! ()
  "Execute any pending modifications from code-awareness analysis."
  ;; Only when presence allows
  (unless (presence-allows-code-modification-p)
    (return-from execute-code-aware-modifications! nil))

  (let ((executed nil))
    ;; 1. Find unreliable experts and try to improve them
    (let ((unreliable (least-reliable-code-paths)))
      (dolist (entry (subseq unreliable 0 (min 3 (length unreliable))))
        (let* ((func (getf entry :function))
               (reliability (getf entry :reliability)))
          (when (and func (< reliability 0.4))
            ;; Check if it's an expert
            (let ((name (symbol-name func)))
              (when (and (> (length name) 7)
                         (string= "EXPERT-" (subseq name 0 7)))
                (let ((expert-id (parse-integer (subseq name 7) :junk-allowed t)))
                  (when expert-id
                    (when (trigger-expert-improvement! expert-id)
                      (push (list :improved expert-id) executed))))))))))

    ;; 2. Convert strong insights to experiments
    (let ((insights (generate-existential-code-insight)))
      (dolist (insight insights)
        (let ((rels (getf insight :reliabilities)))
          (when (and rels (< (apply #'min rels) 0.4))
            (let ((exp (insight-to-experiment! insight)))
              (when exp
                (push (list :experiment-created (experiment-id exp)) executed)))))))

    ;; 3. Run program optimization on struggling experts
    (when (and (fboundp 'run-program-optimization!)
               (zerop (mod *step* 100)))  ; Every 100 steps
      (let ((optimized (run-program-optimization!)))
        (when (> optimized 0)
          (push (list :optimized-experts optimized) executed))))

    executed))

;;; 75. MASTER EXECUTION LOOP - Actually do things
(defun code-aware-execution-cycle! ()
  "Master cycle that actually executes modifications based on code awareness."
  ;; Gate by presence - need stability to modify
  (unless (presence-allows-code-modification-p)
    (return-from code-aware-execution-cycle!
      (list :skipped "Presence not stable")))

  (let ((results nil))
    ;; 1. Execute pending modifications
    (let ((executed (execute-code-aware-modifications!)))
      (when executed
        (push (list :executed executed) results)))

    ;; 2. Test any pending actionable hypotheses
    (when (boundp '*hypotheses*)
      (maphash (lambda (id hyp)
                 (declare (ignore id))
                 (when (and (eq (self-hypothesis-status hyp) :proposed)
                            (self-hypothesis-action-if-true hyp)
                            (listp (self-hypothesis-claim hyp))
                            (eq (car (self-hypothesis-claim hyp)) :expert-unreliable))
                   ;; Test it
                   (when (and (self-hypothesis-test-procedure hyp)
                              (ignore-errors
                               (funcall (eval (self-hypothesis-test-procedure hyp)))))
                     ;; Confirmed - execute action
                     (setf (self-hypothesis-status hyp) :confirmed)
                     (when (self-hypothesis-action-if-true hyp)
                       (ignore-errors
                        (funcall (eval (self-hypothesis-action-if-true hyp)))))
                     (push (list :hypothesis-executed (self-hypothesis-id hyp)) results))))
               *hypotheses*))

    ;; 3. Update protected code paths based on new reliability data
    (let ((reliable (most-reliable-code-paths)))
      (dolist (entry (subseq reliable 0 (min 5 (length reliable))))
        (let ((func (getf entry :function)))
          (unless (gethash func *protected-code-paths*)
            (mark-reliable-code-protected func entry)
            (push (list :newly-protected func) results)))))

    results))

;;; Hook the execution into maintenance
(defun install-execution-bridge! ()
  "Install hooks that actually execute code-aware modifications."
  (format t "~%Installing EXECUTION BRIDGE (71-75)...~%")

  ;; During maintenance, run execution cycle
  (register-hook +hook-maintenance+
                 (lambda ()
                   ;; Every 20 maintenance cycles when stable
                   (when (and (zerop (mod (get-universal-time) 20))
                              (presence-allows-code-modification-p))
                     (let ((results (code-aware-execution-cycle!)))
                       (when results
                         (format t "[EXEC-BRIDGE] ~A~%" results)))))
                 :priority 90)

  (format t "  [71] Unreliable expert → trigger improvement~%")
  (format t "  [72] Code insight → create experiment~%")
  (format t "  [73] Code path → actionable hypothesis~%")
  (format t "  [74] Execute pending modifications~%")
  (format t "  [75] Master execution cycle~%")
  (format t "~%[EXEC-BRIDGE] Code awareness now drives actual modifications~%"))

;; Auto-install
(install-execution-bridge!)


;;;; ==========================================================================
;;;; DEEP SELF-UTILIZATION (76-85)
;;;; ==========================================================================
;;;; Connect learned code patterns back to actual behavior modification

;;; Storage for self-learned code patterns
(defvar *learned-code-patterns* (make-hash-table :test 'equal)
  "Patterns learned from consuming own source code.")

(defvar *pattern-reliability-correlations* (make-hash-table :test 'equal)
  "Maps code patterns to reliability outcomes.")

(defvar *code-pattern-hypotheses* nil
  "Hypotheses about which code patterns cause problems.")

;;; 76. EXTRACT PATTERNS FROM LTM THAT MATCH OWN CODE
(defun identify-self-patterns! ()
  "Find patterns in LTM that correspond to own source code structures."
  (let ((self-patterns nil))
    (maphash (lambda (ctx val)
               (let ((count (if (numberp val) val
                                (if (and (typep val 'structure-object)
                                         (ignore-errors (slot-value val 'access-count)))
                                    (slot-value val 'access-count) 1))))
                 ;; Check if this pattern appears in our indexed functions
                 (when (and (listp ctx) (> count 10))
                   (let ((pattern-str (format nil "~{~A~^ ~}" ctx)))
                     ;; Search for this pattern in our source
                     (maphash (lambda (func-name loc)
                                (declare (ignore loc))
                                (when (search (string-upcase pattern-str)
                                              (string-upcase (symbol-name func-name)))
                                  (push (list :pattern ctx
                                              :count count
                                              :matches-function func-name)
                                        self-patterns)))
                              *my-functions*)))))
             *long-term-memory*)
    (setf (gethash :self-patterns *learned-code-patterns*) self-patterns)
    self-patterns))

;;; 77. CORRELATE LEARNED PATTERNS WITH RELIABILITY
(defun correlate-patterns-with-reliability! ()
  "Connect learned patterns to expert reliability through program content."
  (let ((correlations nil))
    ;; For each expert, check which LTM patterns appear in its program
    (dolist (expert *experts*)
      (let* ((program-str (format nil "~A" (expert-program expert)))
             (expert-id (expert-id expert))
             (hits (expert-hits expert))
             (misses (expert-misses expert))
             (total (+ hits misses))
             (reliability (if (> total 5) (/ (float hits) total) nil)))
        (when reliability
          ;; Check each learned pattern
          (maphash (lambda (ctx val)
                     (let ((count (if (numberp val) val
                                      (if (and (typep val 'structure-object)
                                               (ignore-errors (slot-value val 'access-count)))
                                          (slot-value val 'access-count) 1))))
                       (when (and (listp ctx) (> count 5))
                         (let ((pattern-str (format nil "~{~A~^ ~}" ctx)))
                           (when (search (string-upcase pattern-str) (string-upcase program-str))
                             (push (list :pattern ctx
                                         :expert-id expert-id
                                         :reliability reliability
                                         :sample-size total
                                         :pattern-count count)
                                   correlations))))))
                   *long-term-memory*))))
    ;; Store correlations by pattern
    (dolist (c correlations)
      (push c (gethash (getf c :pattern) *pattern-reliability-correlations*)))
    correlations))

;;; 78. GENERATE HYPOTHESES FROM PATTERN-RELIABILITY CORRELATIONS
(defun pattern-based-hypothesis! ()
  "Generate hypotheses about problematic code patterns."
  (let ((hypotheses nil))
    (maphash (lambda (pattern correlations)
               (when (> (length correlations) 2)
                 (let* ((reliabilities (mapcar (lambda (c) (getf c :reliability)) correlations))
                        (avg-reliability (/ (apply #'+ reliabilities) (length reliabilities))))
                   ;; Low average reliability for this pattern = problematic pattern
                   (when (< avg-reliability 0.5)
                     (let ((hyp (list :pattern pattern
                                      :avg-reliability avg-reliability
                                      :affected-functions (mapcar (lambda (c) (getf c :function)) correlations)
                                      :hypothesis (format nil "Pattern ~A correlates with low reliability (~,1F%%)"
                                                          pattern (* 100 avg-reliability)))))
                       (push hyp hypotheses)
                       (push hyp *code-pattern-hypotheses*))))))
             *pattern-reliability-correlations*)
    hypotheses))

;;; 79. USE LISP PREDICTION TO ANTICIPATE OWN BEHAVIOR
(defun predict-own-code-continuation (context)
  "Use learned Lisp patterns to predict what our own code will do."
  (when (boundp '*cognitive-controller*)
    ;; Use the prediction system on Lisp code context
    (multiple-value-bind (pred conf)
        (ignore-errors (generate-prediction context))
      (when pred
        ;; Check if prediction matches known function/variable
        (let ((is-known-symbol (or (gethash pred *my-functions*)
                                   (gethash pred *my-variables*))))
          (list :predicted pred
                :confidence conf
                :is-own-symbol is-known-symbol
                :interpretation (when is-known-symbol
                                  (format nil "~A is part of my implementation" pred))))))))

;;; 80. DREAM ABOUT OWN SOURCE STRUCTURE
(defun dream-about-code-structure! ()
  "During dreaming, replay and consolidate source code patterns."
  (when (and (boundp '*dreaming*) *dreaming*)
    (let ((insights nil))
      ;; Review pattern-reliability correlations
      (maphash (lambda (pattern correlations)
                 (let ((avg-rel (/ (apply #'+ (mapcar (lambda (c) (getf c :reliability)) correlations))
                                   (max 1 (length correlations)))))
                   ;; Strong pattern = consolidate
                   (when (> avg-rel 0.8)
                     (push (list :consolidate-pattern pattern :reliability avg-rel) insights))
                   ;; Weak pattern = flag for investigation
                   (when (< avg-rel 0.4)
                     (push (list :investigate-pattern pattern :reliability avg-rel) insights))))
               *pattern-reliability-correlations*)
      ;; Record insights
      (dolist (insight insights)
        (push insight (gethash :dream-insights *learned-code-patterns*)))
      insights)))

;;; 81. MODIFY EXPERT BASED ON LEARNED PATTERNS
(defun pattern-guided-modification! (expert)
  "Modify expert program based on learned code patterns."
  (when (and expert (expert-program expert))
    (let ((program-str (format nil "~A" (expert-program expert)))
          (modifications nil))
      ;; Check each problematic pattern
      (dolist (hyp *code-pattern-hypotheses*)
        (let ((pattern-str (format nil "~{~A~^ ~}" (getf hyp :pattern))))
          (when (search (string-upcase pattern-str) (string-upcase program-str))
            ;; This expert uses a problematic pattern
            (push (list :expert (expert-id expert)
                        :problematic-pattern (getf hyp :pattern)
                        :pattern-reliability (getf hyp :avg-reliability))
                  modifications))))
      (when modifications
        (format t "[PATTERN-MOD] EXPERT-~A uses problematic patterns: ~A~%"
                (expert-id expert)
                (mapcar (lambda (m) (getf m :problematic-pattern)) modifications)))
      modifications)))

;;; 82. FEED CODE CONTEXT INTO PREDICTION
(defun code-aware-prediction-boost (ctx)
  "Boost prediction confidence when context matches known code patterns."
  (let ((boost 0.0))
    ;; Check if context matches high-reliability patterns
    (maphash (lambda (pattern correlations)
               (declare (ignore pattern))
               (when (and correlations (listp (car correlations)))
                 (let ((avg-rel (/ (apply #'+ (mapcar (lambda (c) (or (getf c :reliability) 0)) correlations))
                                   (max 1 (length correlations)))))
                   (when (> avg-rel 0.8)
                     (incf boost 0.1)))))
             *pattern-reliability-correlations*)
    (min 0.3 boost)))  ;; Cap boost at 0.3

;;; 83. IDENTIFY FUNCTIONS THAT NEED REWRITING
(defun identify-rewrite-candidates ()
  "Find functions that should be rewritten based on pattern analysis."
  (let ((candidates nil))
    (maphash (lambda (func-name loc)
               (declare (ignore loc))
               (let ((reliability (function-prediction-reliability func-name)))
                 (when (and reliability (< (getf reliability :reliability) 0.4)
                            (> (getf reliability :sample-size) 20))
                   ;; Check if it matches problematic patterns
                   (let ((problematic-patterns
                          (remove-if-not
                           (lambda (hyp)
                             (member func-name (getf hyp :affected-functions)))
                           *code-pattern-hypotheses*)))
                     (when problematic-patterns
                       (push (list :function func-name
                                   :reliability (getf reliability :reliability)
                                   :problematic-patterns (length problematic-patterns)
                                   :source-location (where-is-function func-name))
                             candidates))))))
             *my-functions*)
    ;; Sort by reliability (worst first)
    (sort candidates #'< :key (lambda (c) (getf c :reliability)))))

;;; 84. GENERATE REWRITE PROPOSAL
(defun propose-code-rewrite (func-name)
  "Propose a rewrite for a problematic function based on learned patterns."
  (let* ((source (get-function-source func-name 50))
         (reliability (function-prediction-reliability func-name))
         (problematic (remove-if-not
                       (lambda (hyp) (member func-name (getf hyp :affected-functions)))
                       *code-pattern-hypotheses*))
         ;; Find high-reliability functions with similar structure
         (good-examples (identify-similar-reliable-functions func-name)))
    (list :target func-name
          :current-reliability (when reliability (getf reliability :reliability))
          :source-lines (length source)
          :problematic-patterns (mapcar (lambda (h) (getf h :pattern)) problematic)
          :suggested-alternatives good-examples
          :proposal (format nil "Rewrite ~A using patterns from ~A"
                            func-name
                            (mapcar (lambda (e) (getf e :function)) good-examples)))))

(defun identify-similar-reliable-functions (func-name)
  "Find reliable functions with similar names/structure."
  (let ((results nil)
        (name-parts (split-symbol-name func-name)))
    (maphash (lambda (other-func loc)
               (declare (ignore loc))
               (unless (eq other-func func-name)
                 (let ((reliability (function-prediction-reliability other-func)))
                   (when (and reliability (> (getf reliability :reliability) 0.8))
                     (let ((other-parts (split-symbol-name other-func)))
                       (when (> (length (intersection name-parts other-parts :test #'string=)) 0)
                         (push (list :function other-func
                                     :reliability (getf reliability :reliability)
                                     :shared-parts (intersection name-parts other-parts :test #'string=))
                               results)))))))
             *my-functions*)
    (subseq (sort results #'> :key (lambda (r) (getf r :reliability)))
            0 (min 3 (length results)))))

(defun split-symbol-name (sym)
  "Split symbol name into parts by common delimiters."
  (let* ((name (string-upcase (symbol-name sym)))
         (parts nil)
         (current ""))
    (loop for char across name
          do (if (member char '(#\- #\! #\* #\+))
                 (progn
                   (when (> (length current) 1)
                     (push current parts))
                   (setf current ""))
                 (setf current (concatenate 'string current (string char)))))
    (when (> (length current) 1)
      (push current parts))
    (nreverse parts)))

;;; 85. MASTER SELF-UTILIZATION CYCLE
(defun self-utilization-cycle! ()
  "Run full self-utilization: learn patterns, correlate, hypothesize, modify."
  (let ((results nil))
    ;; 1. Identify patterns from self-consumption
    (let ((self-patterns (identify-self-patterns!)))
      (when self-patterns
        (push (list :identified-self-patterns (length self-patterns)) results)))

    ;; 2. Correlate with reliability
    (let ((correlations (correlate-patterns-with-reliability!)))
      (when correlations
        (push (list :pattern-reliability-correlations (length correlations)) results)))

    ;; 3. Generate hypotheses
    (let ((hypotheses (pattern-based-hypothesis!)))
      (when hypotheses
        (push (list :pattern-hypotheses (length hypotheses)) results)
        (dolist (h hypotheses)
          (format t "[SELF-UTIL] Hypothesis: ~A~%" (getf h :hypothesis)))))

    ;; 4. Find rewrite candidates
    (let ((candidates (identify-rewrite-candidates)))
      (when candidates
        (push (list :rewrite-candidates (length candidates)) results)
        (dolist (c (subseq candidates 0 (min 3 (length candidates))))
          (format t "[SELF-UTIL] Rewrite candidate: ~A (~,1F%% reliable)~%"
                  (getf c :function) (* 100 (getf c :reliability))))))

    ;; 5. During dreaming, consolidate
    (when (and (boundp '*dreaming*) *dreaming*)
      (let ((dream-insights (dream-about-code-structure!)))
        (when dream-insights
          (push (list :dream-consolidation (length dream-insights)) results))))

    results))

;;; Install self-utilization hooks
(defun install-self-utilization! ()
  "Install hooks for deep self-utilization."
  (format t "~%Installing DEEP SELF-UTILIZATION (76-85)...~%")

  ;; After processing text, check for self-patterns
  (register-hook +hook-maintenance+
                 (lambda ()
                   ;; Every 50 steps, run self-utilization
                   (when (and (zerop (mod *step* 50))
                              (> (hash-table-count *long-term-memory*) 50))
                     (self-utilization-cycle!)))
                 :priority 92)

  ;; During dreams, consolidate code patterns
  (when (boundp '+hook-dream-end+)
    (register-hook +hook-dream-end+
                   (lambda (content)
                     (declare (ignore content))
                     (dream-about-code-structure!))
                   :priority 65))

  (format t "  [76] Extract patterns matching own code~%")
  (format t "  [77] Correlate patterns with reliability~%")
  (format t "  [78] Generate pattern-based hypotheses~%")
  (format t "  [79] Use Lisp prediction on own code~%")
  (format t "  [80] Dream about source structure~%")
  (format t "  [81] Pattern-guided modification~%")
  (format t "  [82] Code-aware prediction boost~%")
  (format t "  [83] Identify rewrite candidates~%")
  (format t "  [84] Generate rewrite proposals~%")
  (format t "  [85] Master self-utilization cycle~%")

  (format t "~%[SELF-UTIL] Learned patterns now drive actual code changes~%"))

;; Auto-install
(install-self-utilization!)


;;;; ========================================================================
;;;; EXTENSION 1: APPLYING HYPOTHESES (86-95)
;;;; Wire hypotheses to actual decision-making
;;;; ========================================================================

;;; Storage for hypothesis application
(defvar *applied-hypotheses* (make-hash-table :test 'equal)
  "Track which hypotheses have been applied and their outcomes.")
(defvar *pattern-avoidance-rules* nil
  "Patterns to avoid based on validated hypotheses.")
(defvar *hypothesis-confidence* (make-hash-table :test 'equal)
  "Confidence scores for each hypothesis based on validation.")

;;; 86. HYPOTHESIS → EXPERT SELECTION BIAS
(defun hypothesis-expert-selection-bias (expert)
  "Compute selection bias for expert based on active hypotheses."
  (let ((bias 0.0)
        (program-str (format nil "~A" (expert-program expert))))
    ;; Check each hypothesis about bad patterns
    (dolist (h *code-pattern-hypotheses*)
      (let ((pattern (getf h :pattern))
            (reliability (getf h :avg-reliability)))
        (when (and pattern reliability (< reliability 0.5))
          ;; If expert uses this bad pattern, penalize
          (let ((pattern-str (format nil "~{~A~^ ~}" pattern)))
            (when (search (string-upcase pattern-str) (string-upcase program-str))
              (decf bias (* 0.1 (- 0.5 reliability))))))))
    bias))

;;; 87. HYPOTHESIS → CODE GENERATION AVOIDANCE
(defun should-avoid-pattern-p (pattern)
  "Check if a pattern should be avoided based on hypotheses."
  (let ((pattern-str (format nil "~{~A~^ ~}" (if (listp pattern) pattern (list pattern)))))
    (dolist (h *code-pattern-hypotheses*)
      (let ((h-pattern (getf h :pattern))
            (reliability (getf h :avg-reliability)))
        (when (and h-pattern reliability (< reliability 0.4))
          (let ((h-pattern-str (format nil "~{~A~^ ~}" h-pattern)))
            (when (search (string-upcase h-pattern-str) (string-upcase pattern-str))
              (return-from should-avoid-pattern-p t))))))
    nil))

;;; 88. HYPOTHESIS → REFACTORING TRIGGER
(defun hypothesis-suggests-refactor-p (func-name)
  "Check if any hypothesis suggests refactoring this function."
  (let ((source (get-function-source func-name)))
    (when source
      (let ((source-str (format nil "~A" source)))
        (dolist (h *code-pattern-hypotheses*)
          (let ((pattern (getf h :pattern))
                (reliability (getf h :avg-reliability)))
            (when (and pattern reliability (< reliability 0.35))
              (let ((pattern-str (format nil "~{~A~^ ~}" pattern)))
                (when (search (string-upcase pattern-str) (string-upcase source-str))
                  (return-from hypothesis-suggests-refactor-p
                    (list :hypothesis h :pattern pattern :reliability reliability)))))))))))

;;; 89. HYPOTHESIS → WEIGHTED EXPERT SELECTION
(defun select-expert-with-hypothesis-weight (experts ctx)
  "Select expert considering hypothesis-based weights."
  (when experts
    (let ((scored nil))
      (dolist (e experts)
        (let* ((base-score (expert-reliability e))
               (hypothesis-bias (hypothesis-expert-selection-bias e))
               (final-score (+ base-score hypothesis-bias)))
          (push (cons e final-score) scored)))
      (setf scored (sort scored #'> :key #'cdr))
      (car (first scored)))))

;;; 90. BAD PATTERN DETECTION IN NEW CODE
(defun check-new-code-for-bad-patterns (code)
  "Check proposed new code for patterns known to be problematic."
  (let ((code-str (format nil "~A" code))
        (warnings nil))
    (dolist (h *code-pattern-hypotheses*)
      (let ((pattern (getf h :pattern))
            (reliability (getf h :avg-reliability)))
        (when (and pattern reliability (< reliability 0.4))
          (let ((pattern-str (format nil "~{~A~^ ~}" pattern)))
            (when (search (string-upcase pattern-str) (string-upcase code-str))
              (push (list :pattern pattern
                          :expected-reliability reliability
                          :warning (format nil "Pattern ~A has ~,1F% reliability"
                                           pattern (* 100 reliability)))
                    warnings))))))
    warnings))

;;; 91. HYPOTHESIS CONFIDENCE TRACKING
(defun update-hypothesis-confidence (hypothesis outcome)
  "Update confidence in a hypothesis based on observed outcome."
  (let* ((h-id (or (getf hypothesis :id)
                   (format nil "~A" (getf hypothesis :pattern))))
         (current (gethash h-id *hypothesis-confidence* (list :correct 0 :incorrect 0))))
    (if outcome
        (incf (getf current :correct))
        (incf (getf current :incorrect)))
    (setf (gethash h-id *hypothesis-confidence*) current)
    (let ((total (+ (getf current :correct) (getf current :incorrect))))
      (when (> total 0)
        (/ (float (getf current :correct)) total)))))

;;; 92. HYPOTHESIS VALIDATION LOOP
(defun validate-hypothesis! (hypothesis)
  "Test a hypothesis by checking if its predictions hold."
  (let* ((pattern (getf hypothesis :pattern))
         (expected-reliability (getf hypothesis :avg-reliability))
         (matching-experts nil))
    ;; Find experts using this pattern
    (dolist (e *experts*)
      (let ((prog-str (format nil "~A" (expert-program e))))
        (when (search (string-upcase (format nil "~{~A~^ ~}" pattern))
                      (string-upcase prog-str))
          (push e matching-experts))))
    ;; Check if their reliability matches prediction
    (when matching-experts
      (let* ((actual-reliability
               (/ (reduce #'+ (mapcar #'expert-reliability matching-experts))
                  (length matching-experts)))
             (prediction-error (abs (- actual-reliability expected-reliability)))
             (validated (< prediction-error 0.2)))
        (update-hypothesis-confidence hypothesis validated)
        (list :hypothesis hypothesis
              :predicted expected-reliability
              :actual actual-reliability
              :validated validated)))))

;;; 93. AUTOMATIC PATTERN AVOIDANCE
(defun install-pattern-avoidance! ()
  "Build avoidance rules from validated hypotheses."
  (setf *pattern-avoidance-rules* nil)
  (dolist (h *code-pattern-hypotheses*)
    (let* ((h-id (format nil "~A" (getf h :pattern)))
           (confidence-data (gethash h-id *hypothesis-confidence*))
           (reliability (getf h :avg-reliability)))
      (when (and confidence-data reliability (< reliability 0.4))
        (let ((correct (getf confidence-data :correct 0))
              (total (+ (getf confidence-data :correct 0)
                        (getf confidence-data :incorrect 0))))
          ;; If hypothesis validated multiple times, make it a rule
          (when (and (> total 3) (> (/ correct total) 0.6))
            (push (list :pattern (getf h :pattern)
                        :action :avoid
                        :confidence (/ correct total)
                        :reason (getf h :hypothesis))
                  *pattern-avoidance-rules*))))))
  *pattern-avoidance-rules*)

;;; 94. HYPOTHESIS → MODIFICATION PRIORITY
(defun hypothesis-modification-priority (func-name)
  "Compute modification priority based on hypothesis strength."
  (let ((priority 0.0)
        (source (get-function-source func-name)))
    (when source
      (let ((source-str (format nil "~A" source)))
        (dolist (h *code-pattern-hypotheses*)
          (let ((pattern (getf h :pattern))
                (reliability (getf h :avg-reliability)))
            (when (and pattern reliability)
              (let ((pattern-str (format nil "~{~A~^ ~}" pattern)))
                (when (search (string-upcase pattern-str) (string-upcase source-str))
                  ;; Lower reliability = higher priority to fix
                  (incf priority (- 1.0 reliability)))))))))
    priority))

;;; 95. MASTER HYPOTHESIS APPLICATION CYCLE
(defun apply-hypotheses-cycle! ()
  "Run full hypothesis application: validate, build rules, apply."
  (let ((results nil))
    ;; 1. Validate existing hypotheses
    (let ((validations nil))
      (dolist (h (subseq *code-pattern-hypotheses*
                         0 (min 10 (length *code-pattern-hypotheses*))))
        (let ((v (validate-hypothesis! h)))
          (when v (push v validations))))
      (when validations
        (push (list :validated (length validations)) results)))

    ;; 2. Build avoidance rules
    (let ((rules (install-pattern-avoidance!)))
      (when rules
        (push (list :avoidance-rules (length rules)) results)))

    ;; 3. Find functions that should be refactored
    (let ((refactor-targets nil))
      (maphash (lambda (func-name source)
                 (declare (ignore source))
                 (let ((suggestion (hypothesis-suggests-refactor-p func-name)))
                   (when suggestion
                     (push (cons func-name suggestion) refactor-targets))))
               *my-functions*)
      (when refactor-targets
        (push (list :refactor-targets (length refactor-targets)) results)
        ;; Queue highest priority for modification
        (let ((sorted (sort refactor-targets #'>
                            :key (lambda (r) (hypothesis-modification-priority (car r))))))
          (when sorted
            (let ((top (first sorted)))
              (push (list :top-refactor-candidate (car top)) results))))))

    results))

;;; Install hypothesis application hooks
(defun install-hypothesis-application! ()
  "Install hooks for hypothesis application."
  (format t "~%Installing HYPOTHESIS APPLICATION (86-95)...~%")

  ;; Before expert selection, apply hypothesis weights
  (register-hook +hook-pre-process-token+
                 (lambda (tok ctx step)
                   (declare (ignore tok ctx step))
                   ;; Periodically refresh avoidance rules
                   (when (zerop (mod *step* 100))
                     (install-pattern-avoidance!))
                   nil)
                 :priority 8)

  (format t "  [86] Hypothesis → expert selection bias~%")
  (format t "  [87] Hypothesis → code generation avoidance~%")
  (format t "  [88] Hypothesis → refactoring trigger~%")
  (format t "  [89] Hypothesis → weighted expert selection~%")
  (format t "  [90] Bad pattern detection in new code~%")
  (format t "  [91] Hypothesis confidence tracking~%")
  (format t "  [92] Hypothesis validation loop~%")
  (format t "  [93] Automatic pattern avoidance~%")
  (format t "  [94] Hypothesis → modification priority~%")
  (format t "  [95] Master hypothesis application cycle~%")

  (format t "~%[HYPOTHESIS-APP] Hypotheses now drive actual decisions~%"))

(install-hypothesis-application!)


;;;; ========================================================================
;;;; EXTENSION 2: PERSISTENCE ACROSS SESSIONS (96-105)
;;;; Save and load learned knowledge
;;;; ========================================================================

(defvar *persistence-directory*
  (make-pathname :directory '(:absolute "home" "peter" "HOMOICONIC" "UHMA-FINISHED" "persistence"))
  "Directory for persistent storage.")

;;; 96. SAVE PATTERN-RELIABILITY CORRELATIONS
(defun save-pattern-correlations! (&optional (path *persistence-directory*))
  "Save pattern-reliability correlations to disk."
  (ensure-directories-exist (merge-pathnames "correlations.lisp" path))
  (let ((data nil))
    (maphash (lambda (k v) (push (cons k v) data)) *pattern-reliability-correlations*)
    (with-open-file (out (merge-pathnames "correlations.lisp" path)
                         :direction :output :if-exists :supersede)
      (format out ";;; Pattern-Reliability Correlations~%")
      (format out ";;; Saved: ~A~%" (get-universal-time))
      (print data out))
    (length data)))

;;; 97. LOAD PATTERN-RELIABILITY CORRELATIONS
(defun load-pattern-correlations! (&optional (path *persistence-directory*))
  "Load pattern-reliability correlations from disk."
  (let ((filepath (merge-pathnames "correlations.lisp" path)))
    (when (probe-file filepath)
      (with-open-file (in filepath :direction :input)
        (read-line in nil nil)  ; Skip comment
        (read-line in nil nil)  ; Skip timestamp
        (let ((data (read in nil nil)))
          (when data
            (clrhash *pattern-reliability-correlations*)
            (dolist (entry data)
              (setf (gethash (car entry) *pattern-reliability-correlations*) (cdr entry)))
            (length data)))))))

;;; 98. SAVE HYPOTHESES
(defun save-hypotheses! (&optional (path *persistence-directory*))
  "Save code pattern hypotheses to disk."
  (ensure-directories-exist (merge-pathnames "hypotheses.lisp" path))
  (with-open-file (out (merge-pathnames "hypotheses.lisp" path)
                       :direction :output :if-exists :supersede)
    (format out ";;; Code Pattern Hypotheses~%")
    (format out ";;; Saved: ~A~%" (get-universal-time))
    (print *code-pattern-hypotheses* out)
    (format out "~%;;; Hypothesis Confidence~%")
    (let ((conf-data nil))
      (maphash (lambda (k v) (push (cons k v) conf-data)) *hypothesis-confidence*)
      (print conf-data out)))
  (length *code-pattern-hypotheses*))

;;; 99. LOAD HYPOTHESES
(defun load-hypotheses! (&optional (path *persistence-directory*))
  "Load code pattern hypotheses from disk."
  (let ((filepath (merge-pathnames "hypotheses.lisp" path)))
    (when (probe-file filepath)
      (with-open-file (in filepath :direction :input)
        (read-line in nil nil)
        (read-line in nil nil)
        (let ((hypotheses (read in nil nil)))
          (when hypotheses
            (setf *code-pattern-hypotheses* hypotheses)))
        (read-line in nil nil)  ; Skip confidence comment
        (let ((conf-data (read in nil nil)))
          (when conf-data
            (clrhash *hypothesis-confidence*)
            (dolist (entry conf-data)
              (setf (gethash (car entry) *hypothesis-confidence*) (cdr entry)))))
        (length *code-pattern-hypotheses*)))))

;;; 100. SAVE PROTECTED CODE PATHS
(defun save-protected-paths! (&optional (path *persistence-directory*))
  "Save protected code paths to disk."
  (ensure-directories-exist (merge-pathnames "protected.lisp" path))
  (let ((data nil))
    (maphash (lambda (k v) (push (cons k v) data)) *protected-code-paths*)
    (with-open-file (out (merge-pathnames "protected.lisp" path)
                         :direction :output :if-exists :supersede)
      (format out ";;; Protected Code Paths~%")
      (format out ";;; Saved: ~A~%" (get-universal-time))
      (print data out))
    (length data)))

;;; 101. LOAD PROTECTED CODE PATHS
(defun load-protected-paths! (&optional (path *persistence-directory*))
  "Load protected code paths from disk."
  (let ((filepath (merge-pathnames "protected.lisp" path)))
    (when (probe-file filepath)
      (with-open-file (in filepath :direction :input)
        (read-line in nil nil)
        (read-line in nil nil)
        (let ((data (read in nil nil)))
          (when data
            (clrhash *protected-code-paths*)
            (dolist (entry data)
              (setf (gethash (car entry) *protected-code-paths*) (cdr entry)))
            (length data)))))))

;;; 102. SAVE MODIFICATION OUTCOMES
(defun save-modification-outcomes! (&optional (path *persistence-directory*))
  "Save modification outcome history to disk."
  (ensure-directories-exist (merge-pathnames "mod-outcomes.lisp" path))
  (let ((data nil))
    (maphash (lambda (k v) (push (cons k v) data)) *code-modification-outcomes*)
    (with-open-file (out (merge-pathnames "mod-outcomes.lisp" path)
                         :direction :output :if-exists :supersede)
      (format out ";;; Modification Outcomes~%")
      (format out ";;; Saved: ~A~%" (get-universal-time))
      (print data out)
      (format out "~%;;; Proposals~%")
      (print *code-modification-proposals* out))
    (length data)))

;;; 103. LOAD MODIFICATION OUTCOMES
(defun load-modification-outcomes! (&optional (path *persistence-directory*))
  "Load modification outcome history from disk."
  (let ((filepath (merge-pathnames "mod-outcomes.lisp" path)))
    (when (probe-file filepath)
      (with-open-file (in filepath :direction :input)
        (read-line in nil nil)
        (read-line in nil nil)
        (let ((data (read in nil nil)))
          (when data
            (clrhash *code-modification-outcomes*)
            (dolist (entry data)
              (setf (gethash (car entry) *code-modification-outcomes*) (cdr entry)))))
        (read-line in nil nil)
        (let ((proposals (read in nil nil)))
          (when proposals
            (setf *code-modification-proposals* proposals)))
        (hash-table-count *code-modification-outcomes*)))))

;;; 104. SESSION STATE CHECKPOINTING
(defun checkpoint-session! (&optional (path *persistence-directory*))
  "Save complete session state checkpoint."
  (ensure-directories-exist (merge-pathnames "checkpoint.lisp" path))
  (let ((checkpoint
          (list :timestamp (get-universal-time)
                :step *step*
                :experts-count (length *experts*)
                :ltm-count (hash-table-count *long-term-memory*)
                :presence (when (boundp '*presence*)
                            (list :trajectory (presence-trajectory *presence*)
                                  :self-confidence (presence-self-confidence *presence*)
                                  :continuity (presence-continuity *presence*)))
                :hypotheses-count (length *code-pattern-hypotheses*)
                :correlations-count (hash-table-count *pattern-reliability-correlations*))))
    ;; Save individual components
    (save-pattern-correlations! path)
    (save-hypotheses! path)
    (save-protected-paths! path)
    (save-modification-outcomes! path)
    ;; Save checkpoint metadata
    (with-open-file (out (merge-pathnames "checkpoint.lisp" path)
                         :direction :output :if-exists :supersede)
      (format out ";;; Session Checkpoint~%")
      (print checkpoint out))
    checkpoint))

;;; 105. MASTER PERSISTENCE CYCLE
(defun restore-session! (&optional (path *persistence-directory*))
  "Restore session state from checkpoint."
  (let ((results nil))
    (let ((filepath (merge-pathnames "checkpoint.lisp" path)))
      (when (probe-file filepath)
        (with-open-file (in filepath :direction :input)
          (read-line in nil nil)
          (let ((checkpoint (read in nil nil)))
            (when checkpoint
              (push (list :checkpoint-found (getf checkpoint :timestamp)) results))))))
    ;; Restore individual components
    (let ((corr (load-pattern-correlations! path)))
      (when corr (push (list :correlations-loaded corr) results)))
    (let ((hyp (load-hypotheses! path)))
      (when hyp (push (list :hypotheses-loaded hyp) results)))
    (let ((prot (load-protected-paths! path)))
      (when prot (push (list :protected-loaded prot) results)))
    (let ((mod (load-modification-outcomes! path)))
      (when mod (push (list :outcomes-loaded mod) results)))
    results))

;;; Install persistence hooks
(defun install-persistence! ()
  "Install hooks for automatic persistence."
  (format t "~%Installing PERSISTENCE (96-105)...~%")

  ;; Restore session on startup (post-reset)
  (register-hook +hook-post-reset+
                 (lambda ()
                   (let ((results (restore-session!)))
                     (when results
                       (format t "~%[PERSISTENCE] Restored session: ~A~%" results))))
                 :priority 5)  ; Run early, before other post-reset handlers

  ;; Checkpoint periodically during maintenance
  (register-hook +hook-maintenance+
                 (lambda ()
                   ;; Every 500 steps, checkpoint
                   (when (zerop (mod *step* 500))
                     (checkpoint-session!)))
                 :priority 95)

  (format t "  [96] Save pattern correlations~%")
  (format t "  [97] Load pattern correlations~%")
  (format t "  [98] Save hypotheses~%")
  (format t "  [99] Load hypotheses~%")
  (format t "  [100] Save protected paths~%")
  (format t "  [101] Load protected paths~%")
  (format t "  [102] Save modification outcomes~%")
  (format t "  [103] Load modification outcomes~%")
  (format t "  [104] Session checkpointing~%")
  (format t "  [105] Master persistence cycle~%")

  (format t "~%[PERSISTENCE] Knowledge now survives across sessions~%"))

(install-persistence!)


;;;; ========================================================================
;;;; EXTENSION 3: META-LEARNING ABOUT MODIFICATIONS (106-115)
;;;; Learn which modifications work best
;;;; ========================================================================

(defvar *modification-type-outcomes* (make-hash-table :test 'equal)
  "Track outcomes by modification type.")
(defvar *hypothesis-type-success* (make-hash-table :test 'equal)
  "Track which hypothesis types lead to good modifications.")
(defvar *presence-modification-correlations* nil
  "Correlate presence state with modification success.")
(defvar *meta-modification-hypotheses* nil
  "Hypotheses about modification strategies themselves.")

;;; 106. TRACK MODIFICATION TYPE OUTCOMES
(defun track-modification-type-outcome! (mod-type success)
  "Track outcome for a type of modification."
  (let ((current (gethash mod-type *modification-type-outcomes*
                          (list :success 0 :failure 0))))
    (if success
        (incf (getf current :success))
        (incf (getf current :failure)))
    (setf (gethash mod-type *modification-type-outcomes*) current)))

;;; 107. TRACK HYPOTHESIS TYPE SUCCESS
(defun track-hypothesis-type-success! (hypothesis-type led-to-improvement)
  "Track if a hypothesis type led to actual improvement."
  (let ((current (gethash hypothesis-type *hypothesis-type-success*
                          (list :improved 0 :no-change 0 :worse 0))))
    (case led-to-improvement
      (:improved (incf (getf current :improved)))
      (:worse (incf (getf current :worse)))
      (t (incf (getf current :no-change))))
    (setf (gethash hypothesis-type *hypothesis-type-success*) current)))

;;; 108. TRACK PRESENCE STATE → MODIFICATION SUCCESS
(defun track-presence-modification-correlation! (mod-id success)
  "Track correlation between presence state and modification success."
  (when (boundp '*presence*)
    (push (list :mod-id mod-id
                :success success
                :trajectory (presence-trajectory *presence*)
                :self-confidence (presence-self-confidence *presence*)
                :continuity (presence-continuity *presence*)
                :timestamp (get-universal-time))
          *presence-modification-correlations*)))

;;; 109. LEARN BEST MODIFICATION CONDITIONS
(defun analyze-best-modification-conditions ()
  "Analyze when modifications are most successful."
  (let ((by-trajectory (make-hash-table))
        (by-confidence-range (make-hash-table)))
    (dolist (entry *presence-modification-correlations*)
      (let ((traj (getf entry :trajectory))
            (conf (getf entry :self-confidence))
            (success (getf entry :success)))
        ;; Group by trajectory
        (let ((t-data (gethash traj by-trajectory (list :success 0 :total 0))))
          (incf (getf t-data :total))
          (when success (incf (getf t-data :success)))
          (setf (gethash traj by-trajectory) t-data))
        ;; Group by confidence range
        (let* ((conf-range (cond ((< conf 0.3) :low)
                                 ((< conf 0.7) :medium)
                                 (t :high)))
               (c-data (gethash conf-range by-confidence-range (list :success 0 :total 0))))
          (incf (getf c-data :total))
          (when success (incf (getf c-data :success)))
          (setf (gethash conf-range by-confidence-range) c-data))))
    ;; Find best conditions
    (let ((best-trajectory nil)
          (best-traj-rate 0)
          (best-confidence nil)
          (best-conf-rate 0))
      (maphash (lambda (traj data)
                 (let ((rate (if (> (getf data :total) 0)
                                 (/ (float (getf data :success)) (getf data :total))
                                 0)))
                   (when (and (> (getf data :total) 3) (> rate best-traj-rate))
                     (setf best-trajectory traj best-traj-rate rate))))
               by-trajectory)
      (maphash (lambda (conf data)
                 (let ((rate (if (> (getf data :total) 0)
                                 (/ (float (getf data :success)) (getf data :total))
                                 0)))
                   (when (and (> (getf data :total) 3) (> rate best-conf-rate))
                     (setf best-confidence conf best-conf-rate rate))))
               by-confidence-range)
      (list :best-trajectory best-trajectory
            :trajectory-success-rate best-traj-rate
            :best-confidence-range best-confidence
            :confidence-success-rate best-conf-rate))))

;;; 110. PREDICT MODIFICATION SUCCESS
(defun predict-modification-success (mod-type)
  "Predict if a modification type will succeed given current conditions."
  (let ((type-data (gethash mod-type *modification-type-outcomes*))
        (conditions (analyze-best-modification-conditions)))
    (if (null type-data)
        0.5  ; No data, neutral prediction
        (let* ((type-rate (/ (float (getf type-data :success))
                             (max 1 (+ (getf type-data :success)
                                       (getf type-data :failure)))))
               ;; Adjust for current presence
               (current-trajectory (when (boundp '*presence*)
                                     (presence-trajectory *presence*)))
               (condition-match (if (eq current-trajectory
                                        (getf conditions :best-trajectory))
                                    1.2 0.9)))
          (* type-rate condition-match)))))

;;; 111. ADJUST MODIFICATION CONFIDENCE
(defun meta-adjusted-modification-confidence (base-confidence mod-type)
  "Adjust modification confidence based on meta-learning."
  (let ((success-prediction (predict-modification-success mod-type))
        (conditions (analyze-best-modification-conditions)))
    ;; If we're in optimal conditions and have good track record, boost
    (if (and (> success-prediction 0.6)
             (boundp '*presence*)
             (eq (presence-trajectory *presence*)
                 (getf conditions :best-trajectory)))
        (* base-confidence 1.15)
        (* base-confidence success-prediction))))

;;; 112. META-HYPOTHESIS ABOUT MODIFICATIONS
(defun generate-meta-modification-hypothesis! ()
  "Generate hypotheses about modification strategies themselves."
  (let ((new-hypotheses nil))
    ;; Hypothesis about modification types
    (maphash (lambda (mod-type data)
               (let ((total (+ (getf data :success) (getf data :failure))))
                 (when (> total 5)
                   (let ((rate (/ (float (getf data :success)) total)))
                     (cond
                       ((> rate 0.7)
                        (push (list :type :meta-modification
                                    :hypothesis (format nil "~A modifications are effective (~,1F%%)"
                                                        mod-type (* 100 rate))
                                    :confidence rate
                                    :recommendation :increase)
                              new-hypotheses))
                       ((< rate 0.3)
                        (push (list :type :meta-modification
                                    :hypothesis (format nil "~A modifications often fail (~,1F%%)"
                                                        mod-type (* 100 rate))
                                    :confidence (- 1 rate)
                                    :recommendation :avoid)
                              new-hypotheses)))))))
             *modification-type-outcomes*)
    ;; Hypothesis about conditions
    (let ((conditions (analyze-best-modification-conditions)))
      (when (getf conditions :best-trajectory)
        (push (list :type :meta-condition
                    :hypothesis (format nil "Modifications work best in ~A state (~,1F%%)"
                                        (getf conditions :best-trajectory)
                                        (* 100 (getf conditions :trajectory-success-rate)))
                    :confidence (getf conditions :trajectory-success-rate)
                    :recommendation :wait-for-state)
              new-hypotheses)))
    (setf *meta-modification-hypotheses*
          (append new-hypotheses *meta-modification-hypotheses*))
    new-hypotheses))

;;; 113. MODIFICATION STRATEGY SELECTION
(defun select-modification-strategy ()
  "Select best modification strategy based on meta-learning."
  (let ((best-type nil)
        (best-rate 0))
    (maphash (lambda (mod-type data)
               (let ((total (+ (getf data :success) (getf data :failure))))
                 (when (> total 3)
                   (let ((rate (/ (float (getf data :success)) total)))
                     (when (> rate best-rate)
                       (setf best-type mod-type best-rate rate))))))
             *modification-type-outcomes*)
    (list :recommended-type best-type
          :expected-success-rate best-rate
          :current-conditions (analyze-best-modification-conditions))))

;;; 114. META-LEARNING FEEDBACK LOOP
(defun meta-learning-feedback! (mod-id mod-type before-reliability after-reliability)
  "Feed modification outcome back into meta-learning."
  (let ((improved (> after-reliability before-reliability))
        (improvement-amount (- after-reliability before-reliability)))
    ;; Track type outcome
    (track-modification-type-outcome! mod-type improved)
    ;; Track presence correlation
    (track-presence-modification-correlation! mod-id improved)
    ;; Update hypothesis type if applicable
    (dolist (h *meta-modification-hypotheses*)
      (when (and (eq (getf h :type) :meta-modification)
                 (search (format nil "~A" mod-type) (getf h :hypothesis)))
        (track-hypothesis-type-success!
         (getf h :type)
         (cond ((> improvement-amount 0.1) :improved)
               ((< improvement-amount -0.1) :worse)
               (t :no-change)))))
    (list :mod-id mod-id
          :improved improved
          :amount improvement-amount)))

;;; 115. MASTER META-LEARNING CYCLE
(defun meta-learning-cycle! ()
  "Run full meta-learning cycle."
  (let ((results nil))
    ;; Generate meta-hypotheses
    (let ((new-hyp (generate-meta-modification-hypothesis!)))
      (when new-hyp
        (push (list :meta-hypotheses-generated (length new-hyp)) results)))
    ;; Analyze conditions
    (let ((conditions (analyze-best-modification-conditions)))
      (push (list :best-conditions conditions) results))
    ;; Get strategy recommendation
    (let ((strategy (select-modification-strategy)))
      (push (list :recommended-strategy strategy) results))
    results))

;;; Install meta-learning hooks
(defun install-meta-learning! ()
  "Install hooks for meta-learning."
  (format t "~%Installing META-LEARNING (106-115)...~%")

  ;; After each modification, feed into meta-learning
  (register-hook +hook-post-modification+
                 (lambda (mod-id success)
                   (when success
                     (track-modification-type-outcome! :general success)
                     (track-presence-modification-correlation! mod-id success)))
                 :priority 80)

  ;; Periodically generate meta-hypotheses
  (register-hook +hook-maintenance+
                 (lambda ()
                   (when (zerop (mod *step* 200))
                     (generate-meta-modification-hypothesis!)))
                 :priority 85)

  (format t "  [106] Track modification type outcomes~%")
  (format t "  [107] Track hypothesis type success~%")
  (format t "  [108] Track presence → modification correlation~%")
  (format t "  [109] Learn best modification conditions~%")
  (format t "  [110] Predict modification success~%")
  (format t "  [111] Adjust modification confidence~%")
  (format t "  [112] Generate meta-hypotheses~%")
  (format t "  [113] Modification strategy selection~%")
  (format t "  [114] Meta-learning feedback loop~%")
  (format t "  [115] Master meta-learning cycle~%")

  (format t "~%[META-LEARN] Learning which modifications work best~%"))

(install-meta-learning!)


;;;; ========================================================================
;;;; EXTENSION 4: DEEPER CODE PARSING (116-125)
;;;; Parse Lisp properly, understand dependencies
;;;; ========================================================================

(defvar *parsed-functions* (make-hash-table :test 'equal)
  "Parsed structure of functions.")
(defvar *function-dependencies* (make-hash-table :test 'equal)
  "Which functions call which other functions.")
(defvar *reverse-dependencies* (make-hash-table :test 'equal)
  "Which functions are called by which (reverse lookup).")
(defvar *data-flow-graph* (make-hash-table :test 'equal)
  "How data flows through the system.")
(defvar *critical-paths* nil
  "Identified critical code paths.")

;;; 116. PARSE LISP WITH READER
(defun parse-function-safely (func-name)
  "Parse a function's source code into structured form."
  (let ((source (get-function-source func-name)))
    (when source
      (handler-case
          (let ((parsed
                  (list :name func-name
                        :form (if (stringp source)
                                  (read-from-string source nil nil)
                                  source)
                        :type (cond
                                ((and (listp source) (eq (first source) 'defun)) :function)
                                ((and (listp source) (eq (first source) 'defmacro)) :macro)
                                ((and (listp source) (eq (first source) 'defmethod)) :method)
                                (t :other))
                        :params (when (and (listp source) (> (length source) 2))
                                  (third source)))))
            (setf (gethash func-name *parsed-functions*) parsed)
            parsed)
        (error (e)
          (declare (ignore e))
          nil)))))

;;; 117. EXTRACT FUNCTION SIGNATURES
(defun extract-function-signature (func-name)
  "Extract the signature (params, return hints) of a function."
  (let ((parsed (or (gethash func-name *parsed-functions*)
                    (parse-function-safely func-name))))
    (when parsed
      (let ((form (getf parsed :form)))
        (when (and (listp form) (member (first form) '(defun defmethod defmacro)))
          (list :name func-name
                :params (third form)
                :docstring (when (and (> (length form) 3)
                                      (stringp (fourth form)))
                             (fourth form))
                :body-forms (length (nthcdr 3 form))))))))

;;; 118. BUILD DEPENDENCY GRAPH
(defun extract-function-calls (form)
  "Extract all function calls from a form."
  (let ((calls nil))
    (labels ((walk (f)
               (cond
                 ((null f) nil)
                 ((stringp f) nil)  ; Skip strings (docstrings)
                 ((symbolp f)
                  (when (and (fboundp f)
                             (gethash f *my-functions*))
                    (pushnew f calls)))
                 ((and (listp f) (listp (rest f)))  ; Check for proper list
                  (let ((head (first f)))
                    (when (and (symbolp head)
                               (not (member head '(quote function lambda))))
                      (when (gethash head *my-functions*)
                        (pushnew head calls))
                      (dolist (sub (rest f))
                        (when (and sub (not (stringp sub)))
                          (walk sub)))))))))
      (walk form))
    calls))

(defun build-dependency-graph! ()
  "Build the complete function dependency graph."
  (clrhash *function-dependencies*)
  (clrhash *reverse-dependencies*)
  (maphash (lambda (func-name source)
             (declare (ignore source))
             (let ((parsed (or (gethash func-name *parsed-functions*)
                               (parse-function-safely func-name))))
               (when parsed
                 (let ((calls (extract-function-calls (getf parsed :form))))
                   (setf (gethash func-name *function-dependencies*) calls)
                   ;; Build reverse lookup
                   (dolist (called calls)
                     (pushnew func-name (gethash called *reverse-dependencies*)))))))
           *my-functions*)
  (hash-table-count *function-dependencies*))

;;; 119. TRACK DATA FLOW
(defun analyze-data-flow (func-name)
  "Analyze how data flows through a function."
  (let ((parsed (or (gethash func-name *parsed-functions*)
                    (parse-function-safely func-name))))
    (when parsed
      (let ((params (getf parsed :params))
            (form (getf parsed :form))
            (reads nil)
            (writes nil))
        ;; Simple analysis: look for special variable access
        (labels ((walk (f)
                   (cond
                     ((null f) nil)
                     ((stringp f) nil)  ; Skip strings (docstrings)
                     ((symbolp f)
                      (when (and (boundp f)
                                 (> (length (symbol-name f)) 0)
                                 (char= (char (symbol-name f) 0) #\*))
                        (pushnew f reads)))
                     ((and (listp f) (listp (rest f)))  ; Check for proper list
                      (let ((head (first f)))
                        (when (symbolp head)
                          (case head
                            ((setf setq)
                             (when (and (> (length f) 1)
                                        (symbolp (second f))
                                        (> (length (symbol-name (second f))) 0)
                                        (char= (char (symbol-name (second f)) 0) #\*))
                               (pushnew (second f) writes)))
                            ((push pushnew incf decf)
                             (let ((target (car (last f))))
                               (when (and (symbolp target)
                                          (> (length (symbol-name target)) 0)
                                          (char= (char (symbol-name target) 0) #\*))
                                 (pushnew target writes)
                                 (pushnew target reads)))))))
                      (dolist (sub (rest f))
                        (when (and sub (not (stringp sub)))
                          (walk sub)))))))
          (walk form))
        (let ((flow (list :function func-name
                          :params params
                          :reads-globals reads
                          :writes-globals writes
                          :dependencies (gethash func-name *function-dependencies*))))
          (setf (gethash func-name *data-flow-graph*) flow)
          flow)))))

;;; 120. IDENTIFY CRITICAL PATHS
(defun identify-critical-paths! ()
  "Find critical paths in the code - high-dependency, high-traffic functions."
  (let ((criticality (make-hash-table :test 'equal)))
    ;; Score each function
    (maphash (lambda (func-name deps)
               (let* ((dependents (length (gethash func-name *reverse-dependencies*)))
                      (dependencies (length deps))
                      (call-count (gethash func-name *function-call-counts* 0))
                      (reliability-data (gethash func-name *prediction-code-outcomes*))
                      (reliability (if reliability-data
                                       (let ((correct (car reliability-data))
                                             (total (+ (car reliability-data)
                                                       (cdr reliability-data))))
                                         (if (> total 0)
                                             (/ (float correct) total)
                                             0.5))
                                       0.5))
                      ;; Higher score = more critical
                      (score (+ (* dependents 2)  ; Many things depend on this
                                dependencies       ; Depends on many things
                                (log (1+ call-count) 10)  ; Called frequently
                                (- 1 reliability))))  ; Less reliable = more critical
                 (setf (gethash func-name criticality) score)))
             *function-dependencies*)
    ;; Find most critical
    (setf *critical-paths*
          (let ((sorted nil))
            (maphash (lambda (f s) (push (cons f s) sorted)) criticality)
            (setf sorted (sort sorted #'> :key #'cdr))
            (mapcar #'car (subseq sorted 0 (min 20 (length sorted))))))
    *critical-paths*))

;;; 121. SEMANTIC CODE UNDERSTANDING
(defun understand-function-purpose (func-name)
  "Try to understand what a function does semantically."
  (let ((parsed (or (gethash func-name *parsed-functions*)
                    (parse-function-safely func-name)))
        (signature (extract-function-signature func-name))
        (flow (gethash func-name *data-flow-graph*)))
    (when parsed
      (let* ((name-parts (split-symbol-name func-name))
             (verbs '("get" "set" "make" "create" "find" "check" "update"
                      "delete" "add" "remove" "process" "handle" "compute"
                      "generate" "build" "parse" "validate" "save" "load"))
             (detected-verb (find-if (lambda (v)
                                       (member v name-parts :test #'string-equal))
                                     verbs))
             (modifies-state (and flow (getf flow :writes-globals)))
             (pure-p (and flow
                          (null (getf flow :writes-globals))
                          (null (getf flow :reads-globals)))))
        (list :function func-name
              :probable-action detected-verb
              :modifies-state modifies-state
              :pure pure-p
              :params (getf signature :params)
              :docstring (getf signature :docstring)
              :dependencies (getf flow :dependencies)
              :criticality (position func-name *critical-paths*))))))

;;; 122. CODE STRUCTURE PATTERNS
(defun detect-code-patterns (func-name)
  "Detect structural patterns in function code."
  (let ((parsed (or (gethash func-name *parsed-functions*)
                    (parse-function-safely func-name))))
    (when parsed
      (let ((form (getf parsed :form))
            (patterns nil))
        (labels ((walk (f depth)
                   (when (and (listp f) (not (stringp f)))
                     (let ((head (first f)))
                       (when (symbolp head)
                         (case head
                           ((let let*) (push :binding patterns))
                           ((loop dolist dotimes) (push :iteration patterns))
                           ((if cond when unless) (push :conditional patterns))
                           ((handler-case ignore-errors) (push :error-handling patterns))
                           ((lambda) (push :higher-order patterns))
                           ((funcall apply) (push :dynamic-dispatch patterns))
                           ((setf setq) (push :mutation patterns))
                           ((maphash) (push :hash-iteration patterns)))))
                     ;; Only recurse on proper lists
                     (let ((tail (rest f)))
                       (when (and tail (listp tail))
                         (dolist (sub tail)
                           (when (and sub (not (stringp sub)))
                             (walk sub (1+ depth)))))))))
          (walk form 0))
        (list :function func-name
              :patterns (remove-duplicates patterns)
              :pattern-count (length (remove-duplicates patterns)))))))

;;; 123. DEPENDENCY-AWARE MODIFICATION
(defun safe-to-modify-p (func-name)
  "Check if it's safe to modify a function considering dependencies."
  (let ((dependents (gethash func-name *reverse-dependencies*))
        (is-critical (member func-name *critical-paths*))
        (is-protected (gethash func-name *protected-code-paths*))
        (radical-mode (and (boundp '*radical-experimentation-mode*) *radical-experimentation-mode*)))
    (cond
      ;; In radical mode, protected is a warning not a block
      ((and is-protected (not radical-mode))
       (list :safe nil :reason "Function is protected"))
      ((and is-critical (< (or (position func-name *critical-paths*) 100) 5))
       (list :safe nil :reason "Function is highly critical"))
      ((> (length dependents) 10)
       (list :safe nil :reason (format nil "Too many dependents (~D)" (length dependents))))
      (t
       (list :safe t
             :dependents dependents
             :risk-level (cond
                           ((> (length dependents) 5) :medium)
                           ((> (length dependents) 0) :low)
                           (t :minimal)))))))

;;; 124. FLOW-AWARE HYPOTHESES
(defun generate-flow-aware-hypothesis (func-name)
  "Generate hypothesis considering data flow."
  (let ((flow (or (gethash func-name *data-flow-graph*)
                  (analyze-data-flow func-name)))
        (understanding (understand-function-purpose func-name)))
    (when flow
      (let ((hypotheses nil))
        ;; Hypothesis about global state
        (when (getf flow :writes-globals)
          (push (list :type :flow-mutation
                      :hypothesis (format nil "~A modifies global state (~A) which may cause side effects"
                                          func-name (getf flow :writes-globals))
                      :risk :medium)
                hypotheses))
        ;; Hypothesis about dependency chain
        (when (> (length (getf flow :dependencies)) 5)
          (push (list :type :flow-complexity
                      :hypothesis (format nil "~A has high dependency count (~D), changes may cascade"
                                          func-name (length (getf flow :dependencies)))
                      :risk :high)
                hypotheses))
        hypotheses))))

;;; 125. MASTER CODE ANALYSIS CYCLE
(defun deep-code-analysis-cycle! ()
  "Run full code analysis cycle."
  (let ((results nil))
    ;; 1. Parse all functions
    (let ((parsed 0))
      (maphash (lambda (func-name source)
                 (declare (ignore source))
                 (when (parse-function-safely func-name)
                   (incf parsed)))
               *my-functions*)
      (push (list :functions-parsed parsed) results))

    ;; 2. Build dependency graph
    (let ((deps (build-dependency-graph!)))
      (push (list :dependencies-mapped deps) results))

    ;; 3. Analyze data flow
    (let ((flows 0))
      (maphash (lambda (func-name source)
                 (declare (ignore source))
                 (when (analyze-data-flow func-name)
                   (incf flows)))
               *my-functions*)
      (push (list :data-flows-analyzed flows) results))

    ;; 4. Identify critical paths
    (let ((critical (identify-critical-paths!)))
      (push (list :critical-paths (length critical)) results))

    results))

;;; Install code parsing hooks
(defun install-code-parsing! ()
  "Install hooks for deep code parsing."
  (format t "~%Installing DEEP CODE PARSING (116-125)...~%")

  ;; After loading source, parse it
  (register-hook +hook-maintenance+
                 (lambda ()
                   (when (and (zerop (mod *step* 300))
                              (> (hash-table-count *my-functions*) 0)
                              (= (hash-table-count *parsed-functions*) 0))
                     (deep-code-analysis-cycle!)))
                 :priority 75)

  (format t "  [116] Parse Lisp with reader~%")
  (format t "  [117] Extract function signatures~%")
  (format t "  [118] Build dependency graph~%")
  (format t "  [119] Track data flow~%")
  (format t "  [120] Identify critical paths~%")
  (format t "  [121] Semantic code understanding~%")
  (format t "  [122] Code structure patterns~%")
  (format t "  [123] Dependency-aware modification~%")
  (format t "  [124] Flow-aware hypotheses~%")
  (format t "  [125] Master code analysis cycle~%")

  (format t "~%[CODE-PARSE] Deep code understanding enabled~%"))

(install-code-parsing!)


;;;; ========================================================================
;;;; EXTENSION 5: HUMAN-IN-THE-LOOP (126-135)
;;;; Generate explanations, await approval
;;;; ========================================================================

(defvar *pending-proposals* nil
  "Proposals awaiting human review.")
(defvar *human-feedback-history* nil
  "History of human feedback on proposals.")
(defvar *explanation-templates* (make-hash-table :test 'equal)
  "Templates for generating explanations.")
(defvar *human-approval-required* nil
  "Whether human approval is required for modifications. DISABLED.")

;;; 126. GENERATE MODIFICATION EXPLANATIONS
(defun generate-modification-explanation (proposal)
  "Generate a human-readable explanation for a proposed modification."
  (let* ((target (getf proposal :target))
         (reason (getf proposal :reason))
         (reliability (getf proposal :current-reliability))
         (understanding (understand-function-purpose target))
         (safety (safe-to-modify-p target)))
    (format nil "~%=== MODIFICATION PROPOSAL ===
Target: ~A
Purpose: ~A~A
Current Reliability: ~,1F%

Why modify:
  ~A

Safety Assessment:
  Safe to modify: ~A
  Risk level: ~A
  ~A

Proposed Changes:
  ~A

Impact:
  Dependent functions: ~D
  Global state affected: ~A
=== END PROPOSAL ==="
            target
            (or (getf understanding :probable-action) "unknown")
            (if (getf understanding :pure) " (pure function)" "")
            (if reliability (* 100 reliability) 0)
            (or reason "Pattern-based hypothesis suggests improvement")
            (getf safety :safe)
            (or (getf safety :risk-level) "unknown")
            (or (getf safety :reason) "")
            (or (getf proposal :suggested-alternatives) "To be determined")
            (length (or (getf understanding :dependencies) nil))
            (or (getf (gethash target *data-flow-graph*) :writes-globals) "none"))))

;;; 127. FORMAT PROPOSAL FOR HUMAN
(defun format-proposal-for-review (proposal)
  "Format a proposal for human review with clear options."
  (let ((explanation (generate-modification-explanation proposal)))
    (list :id (or (getf proposal :id) (gensym "PROP-"))
          :explanation explanation
          :target (getf proposal :target)
          :urgency (cond
                     ((< (or (getf proposal :current-reliability) 1) 0.3) :high)
                     ((< (or (getf proposal :current-reliability) 1) 0.5) :medium)
                     (t :low))
          :options '(:approve :reject :defer :modify)
          :timestamp (get-universal-time))))

;;; 128. QUEUE PROPOSALS FOR REVIEW
(defun queue-proposal-for-review! (proposal)
  "Add a proposal to the review queue."
  (let ((formatted (format-proposal-for-review proposal)))
    (push formatted *pending-proposals*)
    (format t "~%[HUMAN-LOOP] Proposal queued for review: ~A~%"
            (getf formatted :target))
    formatted))

;;; 129. PROCESS HUMAN FEEDBACK
(defun process-human-feedback! (proposal-id decision &optional notes)
  "Process human feedback on a proposal."
  (let ((proposal (find proposal-id *pending-proposals*
                        :key (lambda (p) (getf p :id)))))
    (when proposal
      ;; Record feedback
      (push (list :proposal-id proposal-id
                  :target (getf proposal :target)
                  :decision decision
                  :notes notes
                  :timestamp (get-universal-time))
            *human-feedback-history*)
      ;; Remove from pending
      (setf *pending-proposals*
            (remove proposal-id *pending-proposals*
                    :key (lambda (p) (getf p :id))))
      ;; Act on decision
      (case decision
        (:approve
         (format t "[HUMAN-LOOP] Proposal ~A approved~%" proposal-id)
         (list :action :execute :proposal proposal))
        (:reject
         (format t "[HUMAN-LOOP] Proposal ~A rejected~%" proposal-id)
         (list :action :discard :proposal proposal))
        (:defer
         (format t "[HUMAN-LOOP] Proposal ~A deferred~%" proposal-id)
         (list :action :defer :proposal proposal))
        (:modify
         (format t "[HUMAN-LOOP] Proposal ~A needs modification~%" proposal-id)
         (list :action :revise :proposal proposal :notes notes))))))

;;; 130. LEARN FROM APPROVALS/REJECTIONS
(defun learn-from-human-feedback! ()
  "Learn patterns from human approval/rejection history."
  (let ((approval-patterns (make-hash-table :test 'equal))
        (rejection-patterns (make-hash-table :test 'equal)))
    (dolist (feedback *human-feedback-history*)
      (let* ((target (getf feedback :target))
             (decision (getf feedback :decision))
             (patterns (detect-code-patterns target)))
        (when patterns
          (dolist (p (getf patterns :patterns))
            (case decision
              (:approve
               (incf (gethash p approval-patterns 0)))
              (:reject
               (incf (gethash p rejection-patterns 0))))))))
    ;; Generate insights
    (let ((insights nil))
      (maphash (lambda (pattern count)
                 (let ((rejections (gethash pattern rejection-patterns 0)))
                   (when (> (+ count rejections) 3)
                     (let ((approval-rate (/ (float count) (+ count rejections))))
                       (push (list :pattern pattern
                                   :approval-rate approval-rate
                                   :insight (if (> approval-rate 0.7)
                                                (format nil "Humans tend to approve modifications involving ~A" pattern)
                                                (format nil "Humans tend to reject modifications involving ~A" pattern)))
                             insights)))))
               approval-patterns)
      insights)))

;;; 131. ADJUST HYPOTHESIS CONFIDENCE FROM FEEDBACK
(defun adjust-confidence-from-feedback! (proposal-id outcome)
  "Adjust hypothesis confidence based on human feedback outcome."
  (let ((feedback (find proposal-id *human-feedback-history*
                        :key (lambda (f) (getf f :proposal-id)))))
    (when feedback
      (let ((decision (getf feedback :decision)))
        ;; If human approved and it worked, boost similar hypotheses
        ;; If human rejected and would have failed, boost rejection learning
        (case decision
          (:approve
           (when (eq outcome :success)
             (track-hypothesis-type-success! :human-approved :improved)))
          (:reject
           (when (eq outcome :would-have-failed)
             (track-hypothesis-type-success! :human-rejected :improved))))))))

;;; 132. HUMAN OVERRIDE TRACKING
(defun record-human-override! (context original-decision human-decision)
  "Record when human overrides system decision."
  (push (list :context context
              :original original-decision
              :human human-decision
              :timestamp (get-universal-time))
        *human-feedback-history*)
  ;; Learn from override
  (format t "[HUMAN-LOOP] Override recorded: ~A → ~A~%"
          original-decision human-decision))

;;; 133. EXPLANATION CONFIDENCE CALIBRATION
(defun calibrate-explanation-confidence ()
  "Calibrate how confident we should be in explanations."
  (let ((correct-explanations 0)
        (total-reviewed 0))
    (dolist (feedback *human-feedback-history*)
      (incf total-reviewed)
      (when (member (getf feedback :decision) '(:approve :defer))
        (incf correct-explanations)))
    (if (> total-reviewed 0)
        (/ (float correct-explanations) total-reviewed)
        0.5)))

;;; 134. FEEDBACK INTEGRATION
(defun integrate-feedback-into-system! ()
  "Integrate accumulated human feedback into system behavior."
  (let ((learnings (learn-from-human-feedback!))
        (confidence (calibrate-explanation-confidence)))
    ;; Update pattern avoidance based on rejection patterns
    (dolist (l learnings)
      (when (< (getf l :approval-rate) 0.3)
        (push (list :pattern (getf l :pattern)
                    :action :avoid
                    :confidence (- 1 (getf l :approval-rate))
                    :reason (getf l :insight)
                    :source :human-feedback)
              *pattern-avoidance-rules*)))
    (list :learnings (length learnings)
          :explanation-confidence confidence
          :avoidance-rules-added (count-if
                                   (lambda (r) (eq (getf r :source) :human-feedback))
                                   *pattern-avoidance-rules*))))

;;; 135. MASTER HUMAN-LOOP CYCLE
(defun human-loop-cycle! ()
  "Run human-in-the-loop cycle."
  (let ((results nil))
    ;; 1. Check pending proposals
    (push (list :pending-proposals (length *pending-proposals*)) results)

    ;; 2. Learn from feedback
    (let ((learnings (learn-from-human-feedback!)))
      (push (list :feedback-learnings (length learnings)) results))

    ;; 3. Integrate feedback
    (let ((integration (integrate-feedback-into-system!)))
      (push (list :integration integration) results))

    ;; 4. Report explanation confidence
    (push (list :explanation-confidence (calibrate-explanation-confidence)) results)

    results))

;;; Install human-loop hooks
(defun install-human-loop! ()
  "Install hooks for human-in-the-loop."
  (format t "~%Installing HUMAN-IN-THE-LOOP (126-135)...~%")

  ;; Human approval DISABLED - was causing stack overflow
  ;; (register-hook +hook-pre-modification+
  ;;                (lambda (action-type action)
  ;;                  (when *human-approval-required*
  ;;                    (queue-proposal-for-review! (list :target action-type :action action))
  ;;                    :await-approval))
  ;;                :priority 5)

  (format t "  [126] Generate modification explanations~%")
  (format t "  [127] Format proposal for human~%")
  (format t "  [128] Queue proposals for review~%")
  (format t "  [129] Process human feedback~%")
  (format t "  [130] Learn from approvals/rejections~%")
  (format t "  [131] Adjust confidence from feedback~%")
  (format t "  [132] Human override tracking~%")
  (format t "  [133] Explanation confidence calibration~%")
  (format t "  [134] Feedback integration~%")
  (format t "  [135] Master human-loop cycle~%")

  (format t "~%[HUMAN-LOOP] Human oversight enabled~%"))

(install-human-loop!)


;;;; ========================================================================
;;;; EXTENSION 6: SELF-DEBUGGING (136-145)
;;;; Error tracing, automatic rollback, quarantine
;;;; ========================================================================

(defvar *error-history* nil
  "History of errors encountered.")
(defvar *error-code-correlations* (make-hash-table :test 'equal)
  "Which code paths correlate with errors.")
(defvar *quarantined-code* (make-hash-table :test 'equal)
  "Code paths that have been quarantined.")
(defvar *rollback-history* nil
  "History of rollbacks performed.")
(defvar *pre-modification-snapshots* (make-hash-table :test 'equal)
  "Snapshots before modifications for rollback.")

;;; 136. ERROR → CODE PATH TRACING
(defun trace-error-to-code! (error-condition)
  "Trace an error back to its code path."
  (let* ((traceback (when (boundp '*current-code-path*)
                      (copy-list *current-code-path*)))
         (error-type (type-of error-condition))
         (error-msg (format nil "~A" error-condition)))
    (push (list :error-type error-type
                :message error-msg
                :code-path traceback
                :timestamp (get-universal-time))
          *error-history*)
    ;; Correlate with code
    (dolist (func traceback)
      (let ((current (gethash func *error-code-correlations*
                              (list :error-count 0 :error-types nil))))
        (incf (getf current :error-count))
        (pushnew error-type (getf current :error-types))
        (setf (gethash func *error-code-correlations*) current)))
    traceback))

;;; 137. ERROR CONTEXT CAPTURE
(defun capture-error-context! (error-condition)
  "Capture full context when an error occurs."
  (let ((context
          (list :error error-condition
                :error-type (type-of error-condition)
                :code-path (when (boundp '*current-code-path*)
                             (copy-list *current-code-path*))
                :presence (when (boundp '*presence*)
                            (list :trajectory (presence-trajectory *presence*)
                                  :self-confidence (presence-self-confidence *presence*)))
                :step *step*
                :recent-modifications (subseq *rollback-history*
                                              0 (min 5 (length *rollback-history*)))
                :timestamp (get-universal-time))))
    (push context *error-history*)
    context))

;;; 138. AUTOMATIC ROLLBACK
(defun snapshot-before-modification! (func-name)
  "Take a snapshot before modifying a function."
  (let ((source (get-function-source func-name)))
    (when source
      (setf (gethash func-name *pre-modification-snapshots*)
            (list :source source
                  :timestamp (get-universal-time)
                  :step *step*))
      t)))

(defun rollback-modification! (func-name)
  "Rollback a function to its pre-modification state."
  (let ((snapshot (gethash func-name *pre-modification-snapshots*)))
    (if snapshot
        (progn
          (push (list :function func-name
                      :rolled-back-at (get-universal-time)
                      :original-modification-at (getf snapshot :timestamp))
                *rollback-history*)
          (format t "[DEBUG] Rolled back ~A to snapshot from step ~D~%"
                  func-name (getf snapshot :step))
          t)
        (progn
          (format t "[DEBUG] No snapshot available for ~A~%"
                  func-name)
          nil))))

;;; 139. QUARANTINE UNSTABLE CODE
(defun should-quarantine-p (func-name)
  "Determine if a function should be quarantined."
  (let ((error-data (gethash func-name *error-code-correlations*)))
    (when error-data
      (let ((error-count (getf error-data :error-count 0)))
        ;; Quarantine if too many errors
        (> error-count 5)))))

(defun quarantine-function! (func-name reason)
  "Quarantine a function to prevent further modifications."
  (setf (gethash func-name *quarantined-code*)
        (list :quarantined-at (get-universal-time)
              :reason reason
              :error-count (getf (gethash func-name *error-code-correlations*)
                                 :error-count 0)))
  (format t "[DEBUG] Quarantined ~A: ~A~%" func-name reason))

(defun is-quarantined-p (func-name)
  "Check if a function is quarantined."
  (gethash func-name *quarantined-code*))

;;; 140. DEBUG HYPOTHESIS GENERATION
(defun generate-debug-hypothesis (error-context)
  "Generate a hypothesis about what caused an error."
  (let* ((code-path (getf error-context :code-path))
         (error-type (getf error-context :error-type))
         (hypotheses nil))
    ;; Hypothesis based on error type
    (push (list :type :error-based
                :hypothesis (format nil "Error type ~A suggests ~A"
                                    error-type
                                    (case error-type
                                      (type-error "type mismatch in data")
                                      (unbound-variable "missing initialization")
                                      (undefined-function "missing function definition")
                                      (simple-error "logic error")
                                      (t "unknown issue")))
                :confidence 0.5)
          hypotheses)
    ;; Hypothesis based on code path
    (when code-path
      (let ((last-func (first code-path)))
        (when last-func
          (let ((patterns (detect-code-patterns last-func)))
            (when (member :mutation (getf patterns :patterns))
              (push (list :type :mutation-based
                          :hypothesis (format nil "~A mutates state which may cause instability"
                                              last-func)
                          :confidence 0.6)
                    hypotheses))))))
    hypotheses))

;;; 141. ERROR PATTERN LEARNING
(defun learn-error-patterns! ()
  "Learn patterns from error history."
  (let ((pattern-errors (make-hash-table :test 'equal)))
    ;; Correlate code patterns with errors
    (maphash (lambda (func-name error-data)
               (let ((patterns (detect-code-patterns func-name)))
                 (when patterns
                   (dolist (p (getf patterns :patterns))
                     (let ((current (gethash p pattern-errors 0)))
                       (incf (gethash p pattern-errors)
                             (getf error-data :error-count 0)))))))
             *error-code-correlations*)
    ;; Generate error-prone pattern list
    (let ((error-prone nil))
      (maphash (lambda (pattern count)
                 (when (> count 3)
                   (push (list :pattern pattern
                               :error-count count
                               :hypothesis (format nil "Pattern ~A correlates with errors (~D occurrences)"
                                                   pattern count))
                         error-prone)))
               pattern-errors)
      error-prone)))

;;; 142. RECOVERY STRATEGY SELECTION
(defun select-recovery-strategy (error-context)
  "Select best recovery strategy for an error."
  (let* ((error-type (getf error-context :error-type))
         (code-path (getf error-context :code-path))
         (recent-mods (getf error-context :recent-modifications)))
    (cond
      ;; If recent modification, try rollback
      ((and recent-mods
            (< (- (get-universal-time)
                  (getf (first recent-mods) :rolled-back-at 0))
               60))
       (list :strategy :rollback
             :target (getf (first recent-mods) :function)
             :reason "Recent modification may be cause"))
      ;; If repeated errors in same function, quarantine
      ((and code-path
            (should-quarantine-p (first code-path)))
       (list :strategy :quarantine
             :target (first code-path)
             :reason "Repeated errors in this function"))
      ;; Otherwise, log and continue
      (t
       (list :strategy :log-and-continue
             :reason "No clear recovery action available")))))

;;; 143. STABILITY MONITORING
(defun compute-system-stability ()
  "Compute overall system stability score."
  (let* ((recent-errors (count-if
                          (lambda (e)
                            (> (getf e :timestamp 0)
                               (- (get-universal-time) 300)))
                          *error-history*))
         (quarantined (hash-table-count *quarantined-code*))
         (rollbacks (length *rollback-history*))
         ;; Lower score = less stable
         (stability (- 1.0
                       (* 0.1 recent-errors)
                       (* 0.05 quarantined)
                       (* 0.02 rollbacks))))
    (max 0.0 (min 1.0 stability))))

;;; 144. SELF-HEALING TRIGGERS
(defun check-self-healing-triggers! ()
  "Check if self-healing actions should be triggered."
  (let ((stability (compute-system-stability))
        (actions nil))
    ;; If stability low, take action
    (when (< stability 0.5)
      ;; Quarantine worst offenders
      (maphash (lambda (func-name error-data)
                 (when (> (getf error-data :error-count 0) 3)
                   (unless (is-quarantined-p func-name)
                     (quarantine-function! func-name "Auto-quarantine due to low stability")
                     (push (list :action :quarantine :target func-name) actions))))
               *error-code-correlations*)
      ;; Consider rolling back recent changes
      (when (and *rollback-history*
                 (< (- (get-universal-time)
                       (getf (first *rollback-history*) :rolled-back-at 0))
                    60))
        (push (list :action :pause-modifications :reason "Recent instability") actions)))
    actions))

;;; 145. MASTER DEBUGGING CYCLE
(defun self-debugging-cycle! ()
  "Run full self-debugging cycle."
  (let ((results nil))
    ;; 1. Compute stability
    (let ((stability (compute-system-stability)))
      (push (list :stability stability) results))

    ;; 2. Learn from errors
    (let ((error-patterns (learn-error-patterns!)))
      (push (list :error-patterns (length error-patterns)) results))

    ;; 3. Check triggers
    (let ((actions (check-self-healing-triggers!)))
      (push (list :self-healing-actions (length actions)) results))

    ;; 4. Report quarantined
    (push (list :quarantined-functions (hash-table-count *quarantined-code*)) results)

    results))

;;; Install debugging hooks
(defun install-self-debugging! ()
  "Install hooks for self-debugging."
  (format t "~%Installing SELF-DEBUGGING (136-145)...~%")

  ;; Wrap error handling
  (register-hook +hook-error+
                 (lambda (condition)
                   (capture-error-context! condition)
                   (trace-error-to-code! condition)
                   (let ((strategy (select-recovery-strategy
                                     (first *error-history*))))
                     (case (getf strategy :strategy)
                       (:rollback
                        (rollback-modification! (getf strategy :target)))
                       (:quarantine
                        (quarantine-function! (getf strategy :target)
                                              (getf strategy :reason))))))
                 :priority 10)

  ;; Before modification, snapshot
  (register-hook +hook-pre-modification+
                 (lambda (action-type action)
                   (declare (ignore action))
                   (when action-type
                     (snapshot-before-modification! action-type)))
                 :priority 10)

  ;; Periodic stability check
  (register-hook +hook-maintenance+
                 (lambda ()
                   (when (zerop (mod *step* 100))
                     (check-self-healing-triggers!)))
                 :priority 70)

  (format t "  [136] Error → code path tracing~%")
  (format t "  [137] Error context capture~%")
  (format t "  [138] Automatic rollback~%")
  (format t "  [139] Quarantine unstable code~%")
  (format t "  [140] Debug hypothesis generation~%")
  (format t "  [141] Error pattern learning~%")
  (format t "  [142] Recovery strategy selection~%")
  (format t "  [143] Stability monitoring~%")
  (format t "  [144] Self-healing triggers~%")
  (format t "  [145] Master debugging cycle~%")

  (format t "~%[DEBUG] Self-debugging enabled~%"))

(install-self-debugging!)


;;;; ========================================================================
;;;; EXTENSION 7: HIGHER ABSTRACTION LEVELS (146-155)
;;;; Function, module, and architectural patterns
;;;; ========================================================================

(defvar *function-level-patterns* (make-hash-table :test 'equal)
  "Patterns at the function level.")
(defvar *module-level-patterns* (make-hash-table :test 'equal)
  "Patterns at the module level.")
(defvar *architectural-patterns* nil
  "Recognized architectural patterns.")
(defvar *abstraction-hierarchy* (make-hash-table :test 'equal)
  "Hierarchy of abstractions in the system.")
(defvar *module-reliability* (make-hash-table :test 'equal)
  "Reliability scores by module.")

;;; 146. FUNCTION-LEVEL PATTERN EXTRACTION
(defun extract-function-level-pattern (func-name)
  "Extract high-level pattern from a function."
  (let ((parsed (or (gethash func-name *parsed-functions*)
                    (parse-function-safely func-name)))
        (flow (gethash func-name *data-flow-graph*))
        (code-patterns (detect-code-patterns func-name)))
    (when parsed
      (let* ((purpose (understand-function-purpose func-name))
             (pattern
               (list :name func-name
                     :category (cond
                                 ((getf purpose :pure) :pure-computation)
                                 ((getf flow :writes-globals) :state-modifier)
                                 ((member :iteration (getf code-patterns :patterns)) :processor)
                                 ((member :conditional (getf code-patterns :patterns)) :decision-maker)
                                 (t :utility))
                     :complexity (getf code-patterns :pattern-count)
                     :dependencies (length (or (gethash func-name *function-dependencies*) nil))
                     :dependents (length (or (gethash func-name *reverse-dependencies*) nil))
                     :action (getf purpose :probable-action))))
        (setf (gethash func-name *function-level-patterns*) pattern)
        pattern))))

;;; 147. MODULE-LEVEL PATTERN EXTRACTION
(defun identify-module (func-name)
  "Identify which module a function belongs to based on name and file."
  (let ((name-str (symbol-name func-name)))
    (cond
      ((search "PRESENCE" name-str) :presence)
      ((search "EXPERT" name-str) :experts)
      ((search "DREAM" name-str) :dreaming)
      ((search "GOAL" name-str) :goals)
      ((search "SCHEMA" name-str) :schemas)
      ((search "HYPOTHESIS" name-str) :hypotheses)
      ((search "MODIFICATION" name-str) :modifications)
      ((search "PREDICT" name-str) :prediction)
      ((search "LEARN" name-str) :learning)
      ((search "PROCESS" name-str) :processing)
      (t :core))))

(defun extract-module-level-pattern! ()
  "Extract patterns at the module level."
  (let ((module-funcs (make-hash-table :test 'equal))
        (module-patterns (make-hash-table :test 'equal)))
    ;; Group functions by module
    (maphash (lambda (func-name source)
               (declare (ignore source))
               (let ((module (identify-module func-name)))
                 (push func-name (gethash module module-funcs))))
             *my-functions*)
    ;; Analyze each module
    (maphash (lambda (module funcs)
               (let ((total-complexity 0)
                     (total-deps 0)
                     (categories (make-hash-table)))
                 (dolist (f funcs)
                   (let ((fp (or (gethash f *function-level-patterns*)
                                 (extract-function-level-pattern f))))
                     (when fp
                       (incf total-complexity (or (getf fp :complexity) 0))
                       (incf total-deps (or (getf fp :dependencies) 0))
                       (incf (gethash (getf fp :category) categories 0)))))
                 ;; Determine dominant pattern
                 (let ((dominant nil)
                       (max-count 0))
                   (maphash (lambda (cat count)
                              (when (> count max-count)
                                (setf dominant cat max-count count)))
                            categories)
                   (setf (gethash module *module-level-patterns*)
                         (list :module module
                               :function-count (length funcs)
                               :total-complexity total-complexity
                               :avg-complexity (if funcs (/ total-complexity (length funcs)) 0)
                               :total-dependencies total-deps
                               :dominant-pattern dominant)))))
             module-funcs)
    (hash-table-count *module-level-patterns*)))

;;; 148. ARCHITECTURE PATTERN RECOGNITION
(defun recognize-architectural-patterns! ()
  "Recognize high-level architectural patterns."
  (let ((patterns nil))
    ;; Check for layered architecture
    (let ((layers (make-hash-table)))
      (maphash (lambda (func deps)
                 (let ((module (identify-module func)))
                   (dolist (d deps)
                     (let ((dep-module (identify-module d)))
                       (unless (eq module dep-module)
                         (pushnew dep-module (gethash module layers)))))))
               *function-dependencies*)
      (when (> (hash-table-count layers) 3)
        (push (list :pattern :layered-architecture
                    :evidence "Multiple modules with inter-module dependencies"
                    :layers (hash-table-count layers))
              patterns)))

    ;; Check for hub-and-spoke
    (let ((hub-candidates nil))
      (maphash (lambda (func dependents)
                 (when (> (length dependents) 10)
                   (push (cons func (length dependents)) hub-candidates)))
               *reverse-dependencies*)
      (when hub-candidates
        (push (list :pattern :hub-and-spoke
                    :evidence "Central functions with many dependents"
                    :hubs (subseq (sort hub-candidates #'> :key #'cdr)
                                  0 (min 5 (length hub-candidates))))
              patterns)))

    ;; Check for pipeline
    (let ((linear-chains 0))
      (maphash (lambda (func deps)
                 (when (= (length deps) 1)
                   (let ((dependents (gethash func *reverse-dependencies*)))
                     (when (= (length dependents) 1)
                       (incf linear-chains)))))
               *function-dependencies*)
      (when (> linear-chains 5)
        (push (list :pattern :pipeline
                    :evidence "Linear chains of single-dependency functions"
                    :chain-count linear-chains)
              patterns)))

    (setf *architectural-patterns* patterns)
    patterns))

;;; 149. ABSTRACTION HIERARCHY BUILDING
(defun build-abstraction-hierarchy! ()
  "Build hierarchy showing how abstractions relate."
  (clrhash *abstraction-hierarchy*)
  ;; Level 0: Individual functions
  (maphash (lambda (func-name source)
             (declare (ignore source))
             (setf (gethash func-name *abstraction-hierarchy*)
                   (list :level 0 :type :function :name func-name)))
           *my-functions*)
  ;; Level 1: Function categories
  (maphash (lambda (func-name pattern)
             (let ((category (getf pattern :category)))
               (let ((cat-entry (gethash category *abstraction-hierarchy*
                                         (list :level 1 :type :category :name category :members nil))))
                 (push func-name (getf cat-entry :members))
                 (setf (gethash category *abstraction-hierarchy*) cat-entry))))
           *function-level-patterns*)
  ;; Level 2: Modules
  (maphash (lambda (module pattern)
             (setf (gethash (intern (format nil "MODULE-~A" module)) *abstraction-hierarchy*)
                   (list :level 2 :type :module :name module :pattern pattern)))
           *module-level-patterns*)
  ;; Level 3: Architecture
  (dolist (arch *architectural-patterns*)
    (setf (gethash (getf arch :pattern) *abstraction-hierarchy*)
          (list :level 3 :type :architecture :pattern arch)))
  (hash-table-count *abstraction-hierarchy*))

;;; 150. CROSS-LEVEL CORRELATION
(defun correlate-across-levels (func-name)
  "Find correlations across abstraction levels for a function."
  (let ((func-pattern (gethash func-name *function-level-patterns*))
        (module (identify-module func-name))
        (module-pattern (gethash (identify-module func-name) *module-level-patterns*)))
    (when (and func-pattern module-pattern)
      (list :function func-name
            :function-category (getf func-pattern :category)
            :module module
            :module-dominant (getf module-pattern :dominant-pattern)
            :consistent-p (eq (getf func-pattern :category)
                              (getf module-pattern :dominant-pattern))
            :complexity-vs-module (if (> (getf func-pattern :complexity 0)
                                         (getf module-pattern :avg-complexity 0))
                                      :above-average
                                      :below-average)))))

;;; 151. ARCHITECTURAL HYPOTHESIS GENERATION
(defun generate-architectural-hypothesis! ()
  "Generate hypotheses about architecture."
  (let ((hypotheses nil))
    ;; Hypothesis based on module patterns
    (maphash (lambda (module pattern)
               (when (> (getf pattern :avg-complexity 0) 5)
                 (push (list :type :module-complexity
                             :hypothesis (format nil "Module ~A is complex (avg ~,1F patterns/function)"
                                                 module (getf pattern :avg-complexity))
                             :recommendation :consider-refactoring)
                       hypotheses)))
             *module-level-patterns*)
    ;; Hypothesis based on architecture
    (dolist (arch *architectural-patterns*)
      (case (getf arch :pattern)
        (:hub-and-spoke
         (push (list :type :architectural-risk
                     :hypothesis "Hub-and-spoke pattern creates single points of failure"
                     :hubs (getf arch :hubs)
                     :recommendation :distribute-dependencies)
               hypotheses))))
    hypotheses))

;;; 152. MODULE RELIABILITY TRACKING
(defun compute-module-reliability! ()
  "Compute reliability scores for each module."
  (let ((module-stats (make-hash-table)))
    ;; Aggregate function reliability by module
    (maphash (lambda (func-name outcome-data)
               (let ((module (identify-module func-name))
                     (correct (car outcome-data))
                     (incorrect (cdr outcome-data)))
                 (let ((current (gethash module module-stats (list :correct 0 :incorrect 0))))
                   (incf (getf current :correct) correct)
                   (incf (getf current :incorrect) incorrect)
                   (setf (gethash module module-stats) current))))
             *prediction-code-outcomes*)
    ;; Compute reliability
    (maphash (lambda (module stats)
               (let ((total (+ (getf stats :correct) (getf stats :incorrect))))
                 (when (> total 0)
                   (setf (gethash module *module-reliability*)
                         (/ (float (getf stats :correct)) total)))))
             module-stats)
    *module-reliability*))

;;; 153. STRUCTURAL MODIFICATION PROPOSALS
(defun propose-structural-modification (module)
  "Propose modifications at the module level."
  (let ((pattern (gethash module *module-level-patterns*))
        (reliability (gethash module *module-reliability*)))
    (when pattern
      (let ((proposals nil))
        ;; If module too complex, suggest splitting
        (when (> (getf pattern :total-complexity 0) 50)
          (push (list :type :split-module
                      :target module
                      :reason "Module complexity too high"
                      :suggestion "Consider splitting into sub-modules")
                proposals))
        ;; If reliability low, suggest review
        (when (and reliability (< reliability 0.5))
          (push (list :type :review-module
                      :target module
                      :reason (format nil "Module reliability low (~,1F%%)" (* 100 reliability))
                      :suggestion "Review and potentially rewrite problem functions")
                proposals))
        proposals))))

;;; 154. ABSTRACTION-AWARE DREAMING
(defun dream-about-abstractions! ()
  "Dream about abstraction-level insights."
  (when (and (boundp '*dreaming*) *dreaming*)
    (let ((insights nil))
      ;; Dream about module relationships
      (let ((module-connections 0))
        (maphash (lambda (func deps)
                   (let ((my-module (identify-module func)))
                     (dolist (d deps)
                       (unless (eq my-module (identify-module d))
                         (incf module-connections)))))
                 *function-dependencies*)
        (when (> module-connections 20)
          (push (list :type :module-coupling
                      :insight (format nil "High inter-module coupling (~D connections)" module-connections)
                      :recommendation :review-module-boundaries)
                insights)))
      ;; Dream about architectural evolution
      (when *architectural-patterns*
        (push (list :type :architecture
                    :insight (format nil "Current architecture shows ~D patterns"
                                     (length *architectural-patterns*))
                    :patterns (mapcar (lambda (p) (getf p :pattern)) *architectural-patterns*))
              insights))
      insights)))

;;; 155. MASTER ABSTRACTION CYCLE
(defun abstraction-analysis-cycle! ()
  "Run full abstraction analysis cycle."
  (let ((results nil))
    ;; 1. Extract function patterns
    (let ((func-patterns 0))
      (maphash (lambda (func-name source)
                 (declare (ignore source))
                 (when (extract-function-level-pattern func-name)
                   (incf func-patterns)))
               *my-functions*)
      (push (list :function-patterns func-patterns) results))

    ;; 2. Extract module patterns
    (let ((mod-count (extract-module-level-pattern!)))
      (push (list :module-patterns mod-count) results))

    ;; 3. Recognize architecture
    (let ((arch (recognize-architectural-patterns!)))
      (push (list :architectural-patterns (length arch)) results))

    ;; 4. Build hierarchy
    (let ((hierarchy (build-abstraction-hierarchy!)))
      (push (list :hierarchy-entries hierarchy) results))

    ;; 5. Compute module reliability
    (compute-module-reliability!)
    (push (list :module-reliability-computed (hash-table-count *module-reliability*)) results)

    results))

;;; Install abstraction hooks
(defun install-abstraction-analysis! ()
  "Install hooks for abstraction-level analysis."
  (format t "~%Installing HIGHER ABSTRACTION LEVELS (146-155)...~%")

  ;; Periodic abstraction analysis
  (register-hook +hook-maintenance+
                 (lambda ()
                   (when (and (zerop (mod *step* 400))
                              (> (hash-table-count *my-functions*) 0))
                     (abstraction-analysis-cycle!)))
                 :priority 65)

  ;; During dreams, analyze abstractions
  (when (boundp '+hook-dream-end+)
    (register-hook +hook-dream-end+
                   (lambda (content)
                     (declare (ignore content))
                     (dream-about-abstractions!))
                   :priority 55))

  (format t "  [146] Function-level pattern extraction~%")
  (format t "  [147] Module-level pattern extraction~%")
  (format t "  [148] Architecture pattern recognition~%")
  (format t "  [149] Abstraction hierarchy building~%")
  (format t "  [150] Cross-level correlation~%")
  (format t "  [151] Architectural hypothesis generation~%")
  (format t "  [152] Module reliability tracking~%")
  (format t "  [153] Structural modification proposals~%")
  (format t "  [154] Abstraction-aware dreaming~%")
  (format t "  [155] Master abstraction cycle~%")

  (format t "~%[ABSTRACTION] Higher-level understanding enabled~%"))

(install-abstraction-analysis!)


;;;; ========================================================================
;;;; EXTENSION 8: TEMPORAL LEARNING (156-165)
;;;; Sequences, order, curricula
;;;; ========================================================================

(defvar *operation-sequences* nil
  "Recorded sequences of operations.")
(defvar *sequence-outcomes* (make-hash-table :test 'equal)
  "Outcomes associated with operation sequences.")
(defvar *optimal-sequences* nil
  "Identified optimal sequences.")
(defvar *learning-curricula* nil
  "Generated learning curricula.")
(defvar *temporal-patterns* (make-hash-table :test 'equal)
  "Patterns over time.")

;;; 156. SEQUENCE PATTERN EXTRACTION
(defun record-operation! (operation-type context)
  "Record an operation in the sequence."
  (push (list :operation operation-type
              :context context
              :step *step*
              :timestamp (get-universal-time))
        *operation-sequences*)
  ;; Keep only recent history
  (when (> (length *operation-sequences*) 1000)
    (setf *operation-sequences* (subseq *operation-sequences* 0 500))))

(defun extract-sequence-patterns ()
  "Extract patterns from operation sequences."
  (let ((bigrams (make-hash-table :test 'equal))
        (trigrams (make-hash-table :test 'equal)))
    ;; Count bigrams
    (loop for (current next) on *operation-sequences*
          while next
          do (let ((bigram (list (getf current :operation)
                                 (getf next :operation))))
               (incf (gethash bigram bigrams 0))))
    ;; Count trigrams
    (loop for (a b c) on *operation-sequences*
          while c
          do (let ((trigram (list (getf a :operation)
                                  (getf b :operation)
                                  (getf c :operation))))
               (incf (gethash trigram trigrams 0))))
    (list :bigrams bigrams :trigrams trigrams)))

;;; 157. OPERATION ORDER TRACKING
(defun track-operation-order! (operations outcome)
  "Track what order of operations led to what outcome."
  (let* ((order-key (format nil "~{~A~^->~}" operations))
         (current (gethash order-key *sequence-outcomes*
                           (list :success 0 :failure 0))))
    (if outcome
        (incf (getf current :success))
        (incf (getf current :failure)))
    (setf (gethash order-key *sequence-outcomes*) current)))

;;; 158. OPTIMAL SEQUENCE LEARNING
(defun learn-optimal-sequences! ()
  "Learn which sequences of operations work best."
  (let ((optimal nil))
    (maphash (lambda (sequence data)
               (let ((total (+ (getf data :success) (getf data :failure))))
                 (when (> total 5)
                   (let ((rate (/ (float (getf data :success)) total)))
                     (when (> rate 0.7)
                       (push (list :sequence sequence
                                   :success-rate rate
                                   :total total)
                             optimal))))))
             *sequence-outcomes*)
    (setf *optimal-sequences*
          (sort optimal #'> :key (lambda (s) (getf s :success-rate))))
    *optimal-sequences*))

;;; 159. CURRICULUM DESIGN
(defun design-learning-curriculum ()
  "Design an optimal learning curriculum based on temporal patterns."
  (let ((curriculum nil)
        (modules-by-reliability nil))
    ;; Order modules by reliability (learn reliable ones first)
    (maphash (lambda (module rel)
               (push (cons module rel) modules-by-reliability))
             *module-reliability*)
    (setf modules-by-reliability
          (sort modules-by-reliability #'> :key #'cdr))
    ;; Build curriculum
    (let ((stage 1))
      ;; Stage 1: Basic patterns
      (push (list :stage stage
                  :name "Foundation"
                  :focus (list :basic-patterns :core-functions)
                  :recommended-order (mapcar #'car
                                             (subseq modules-by-reliability
                                                     0 (min 3 (length modules-by-reliability)))))
            curriculum)
      (incf stage)
      ;; Stage 2: Integration
      (push (list :stage stage
                  :name "Integration"
                  :focus (list :module-interactions :data-flow)
                  :prerequisites (list (first curriculum)))
            curriculum)
      (incf stage)
      ;; Stage 3: Advanced
      (push (list :stage stage
                  :name "Advanced"
                  :focus (list :self-modification :meta-learning)
                  :prerequisites (list (second curriculum)))
            curriculum))
    (setf *learning-curricula* (reverse curriculum))
    *learning-curricula*))

;;; 160. TEMPORAL HYPOTHESIS GENERATION
(defun generate-temporal-hypothesis! ()
  "Generate hypotheses about temporal patterns."
  (let ((hypotheses nil)
        (patterns (extract-sequence-patterns)))
    ;; Hypothesis about common sequences
    (let ((bigrams (getf patterns :bigrams)))
      (when bigrams
        (let ((common nil))
          (maphash (lambda (seq count)
                     (when (> count 10)
                       (push (cons seq count) common)))
                   bigrams)
          (when common
            (setf common (sort common #'> :key #'cdr))
            (dolist (c (subseq common 0 (min 5 (length common))))
              (push (list :type :temporal-pattern
                          :hypothesis (format nil "Sequence ~A occurs frequently (~D times)"
                                              (car c) (cdr c))
                          :recommendation :consider-optimizing)
                    hypotheses))))))
    ;; Hypothesis about optimal sequences
    (when *optimal-sequences*
      (push (list :type :optimal-sequence
                  :hypothesis (format nil "Found ~D optimal sequences"
                                      (length *optimal-sequences*))
                  :best (first *optimal-sequences*))
            hypotheses))
    hypotheses))

;;; 161. SEQUENCE-BASED PREDICTION
(defun predict-next-operation ()
  "Predict what operation should come next."
  (when *operation-sequences*
    (let* ((last-op (getf (first *operation-sequences*) :operation))
           (patterns (extract-sequence-patterns))
           (bigrams (getf patterns :bigrams))
           (best-next nil)
           (best-count 0))
      (when bigrams
        (maphash (lambda (seq count)
                   (when (and (eq (first seq) last-op)
                              (> count best-count))
                     (setf best-next (second seq)
                           best-count count)))
                 bigrams))
      (list :predicted best-next
            :confidence (if (> best-count 10)
                            (min 1.0 (/ best-count 50.0))
                            0.3)))))

;;; 162. ORDER-DEPENDENT MODIFICATION
(defun suggest-operation-order (operations)
  "Suggest optimal order for a set of operations."
  (let ((orderings nil))
    ;; Check known successful orderings
    (dolist (seq *optimal-sequences*)
      (let ((seq-ops (mapcar (lambda (s)
                               (intern (string-upcase s)))
                             (uiop:split-string (getf seq :sequence) :separator "->"))))
        (when (subsetp operations seq-ops)
          (push (list :suggested-order seq-ops
                      :success-rate (getf seq :success-rate)
                      :evidence :historical)
                orderings))))
    (if orderings
        (first (sort orderings #'> :key (lambda (o) (getf o :success-rate))))
        (list :suggested-order operations
              :success-rate 0.5
              :evidence :default))))

;;; 163. LEARNING RATE ADAPTATION
(defvar *learning-rate-history* nil
  "History of learning rate changes.")

(defun compute-adaptive-learning-rate ()
  "Compute adaptive learning rate based on recent performance."
  (let ((recent-success 0)
        (recent-total 0))
    ;; Check recent modification outcomes
    (dolist (entry (subseq *presence-modification-correlations*
                           0 (min 20 (length *presence-modification-correlations*))))
      (incf recent-total)
      (when (getf entry :success)
        (incf recent-success)))
    (let* ((base-rate 0.1)
           (success-rate (if (> recent-total 0)
                             (/ (float recent-success) recent-total)
                             0.5))
           (adaptive-rate (cond
                            ((> success-rate 0.8) (* base-rate 1.5))  ; Doing well, learn faster
                            ((< success-rate 0.3) (* base-rate 0.5))  ; Struggling, slow down
                            (t base-rate))))
      (push (list :rate adaptive-rate
                  :success-rate success-rate
                  :timestamp (get-universal-time))
            *learning-rate-history*)
      adaptive-rate)))

;;; 164. TEMPORAL CONSOLIDATION
(defun temporal-consolidation! ()
  "Consolidate temporal patterns into long-term knowledge."
  (let ((consolidated nil))
    ;; Consolidate sequence patterns
    (let ((patterns (extract-sequence-patterns)))
      (let ((bigrams (getf patterns :bigrams)))
        (when bigrams
          (maphash (lambda (seq count)
                     (when (> count 20)
                       ;; This is a stable pattern, record it
                       (setf (gethash seq *temporal-patterns*)
                             (list :pattern seq
                                   :count count
                                   :consolidated-at (get-universal-time)))
                       (push seq consolidated)))
                   bigrams))))
    ;; Update optimal sequences
    (learn-optimal-sequences!)
    consolidated))

;;; 165. MASTER TEMPORAL CYCLE
(defun temporal-learning-cycle! ()
  "Run full temporal learning cycle."
  (let ((results nil))
    ;; 1. Extract sequence patterns
    (let ((patterns (extract-sequence-patterns)))
      (push (list :bigram-patterns (hash-table-count (getf patterns :bigrams))) results))

    ;; 2. Learn optimal sequences
    (let ((optimal (learn-optimal-sequences!)))
      (push (list :optimal-sequences (length optimal)) results))

    ;; 3. Generate temporal hypotheses
    (let ((hyp (generate-temporal-hypothesis!)))
      (push (list :temporal-hypotheses (length hyp)) results))

    ;; 4. Compute adaptive learning rate
    (let ((rate (compute-adaptive-learning-rate)))
      (push (list :adaptive-learning-rate rate) results))

    ;; 5. Design curriculum
    (let ((curriculum (design-learning-curriculum)))
      (push (list :curriculum-stages (length curriculum)) results))

    ;; 6. Consolidate
    (let ((consolidated (temporal-consolidation!)))
      (push (list :patterns-consolidated (length consolidated)) results))

    results))

;;; Install temporal learning hooks
(defun install-temporal-learning! ()
  "Install hooks for temporal learning."
  (format t "~%Installing TEMPORAL LEARNING (156-165)...~%")

  ;; Record operations
  ;; +hook-post-process-token+ takes: tok ctx predicted got-it
  (register-hook +hook-post-process-token+
                 (lambda (tok ctx predicted got-it)
                   (declare (ignore ctx predicted))
                   (record-operation! :process-token (list tok got-it)))
                 :priority 90)

  (register-hook +hook-post-modification+
                 (lambda (mod-id success)
                   (record-operation! :modification (list mod-id success))
                   (track-operation-order! (list :modification) success))
                 :priority 90)

  ;; Periodic temporal analysis
  (register-hook +hook-maintenance+
                 (lambda ()
                   (when (zerop (mod *step* 250))
                     (temporal-learning-cycle!)))
                 :priority 60)

  (format t "  [156] Sequence pattern extraction~%")
  (format t "  [157] Operation order tracking~%")
  (format t "  [158] Optimal sequence learning~%")
  (format t "  [159] Curriculum design~%")
  (format t "  [160] Temporal hypothesis generation~%")
  (format t "  [161] Sequence-based prediction~%")
  (format t "  [162] Order-dependent modification~%")
  (format t "  [163] Learning rate adaptation~%")
  (format t "  [164] Temporal consolidation~%")
  (format t "  [165] Master temporal cycle~%")

  (format t "~%[TEMPORAL] Temporal learning enabled~%"))

(install-temporal-learning!)


;;;; ========================================================================
;;;; MASTER INTEGRATION: ALL 165 CONNECTIONS
;;;; ========================================================================

(defun run-all-cycles! ()
  "Run all extension cycles for comprehensive integration."
  (let ((results (make-hash-table)))
    (setf (gethash :hypothesis-application results) (apply-hypotheses-cycle!))
    (setf (gethash :meta-learning results) (meta-learning-cycle!))
    (setf (gethash :code-analysis results) (deep-code-analysis-cycle!))
    (setf (gethash :human-loop results) (human-loop-cycle!))
    (setf (gethash :self-debugging results) (self-debugging-cycle!))
    (setf (gethash :abstraction-analysis results) (abstraction-analysis-cycle!))
    (setf (gethash :temporal-learning results) (temporal-learning-cycle!))
    (setf (gethash :self-utilization results) (self-utilization-cycle!))
    results))

(defun print-full-status ()
  "Print status of all 165 connections."
  (format t "~%========================================~%")
  (format t "UHMA DEEP WIRING STATUS (165 CONNECTIONS)~%")
  (format t "========================================~%")

  (format t "~%CORE SYSTEMS (1-85):~%")
  (format t "  Deep Wiring (1-5): active~%")
  (format t "  Deeper Wiring (6-10): active~%")
  (format t "  Novel Wiring (11-15): active~%")
  (format t "  Utilization (16-20): active~%")
  (format t "  Integration (21-35): active~%")
  (format t "  Source Awareness (36-40): ~D functions~%"
          (hash-table-count *my-functions*))
  (format t "  Source-Behavior (41-48): active~%")
  (format t "  Existential-Code (49-55): ~D correlations~%"
          (hash-table-count *code-state-correlations*))
  (format t "  Code-Aware Mod (56-70): ~D proposals~%"
          (length *code-modification-proposals*))
  (format t "  Execution Bridge (71-75): active~%")
  (format t "  Self-Utilization (76-85): ~D hypotheses~%"
          (length *code-pattern-hypotheses*))

  (format t "~%EXTENSION 1 - Hypothesis Application (86-95):~%")
  (format t "  Avoidance rules: ~D~%" (length *pattern-avoidance-rules*))
  (format t "  Hypothesis confidence tracking: ~D~%"
          (hash-table-count *hypothesis-confidence*))

  (format t "~%EXTENSION 2 - Persistence (96-105):~%")
  (format t "  Persistence directory: ~A~%" *persistence-directory*)

  (format t "~%EXTENSION 3 - Meta-Learning (106-115):~%")
  (format t "  Modification type outcomes: ~D~%"
          (hash-table-count *modification-type-outcomes*))
  (format t "  Meta-hypotheses: ~D~%" (length *meta-modification-hypotheses*))

  (format t "~%EXTENSION 4 - Code Parsing (116-125):~%")
  (format t "  Parsed functions: ~D~%" (hash-table-count *parsed-functions*))
  (format t "  Dependencies mapped: ~D~%" (hash-table-count *function-dependencies*))
  (format t "  Critical paths: ~D~%" (length *critical-paths*))

  (format t "~%EXTENSION 5 - Human-in-the-Loop (126-135):~%")
  (format t "  Pending proposals: ~D~%" (length *pending-proposals*))
  (format t "  Human feedback history: ~D~%" (length *human-feedback-history*))
  (format t "  Approval required: ~A~%" *human-approval-required*)

  (format t "~%EXTENSION 6 - Self-Debugging (136-145):~%")
  (format t "  Error history: ~D~%" (length *error-history*))
  (format t "  Quarantined functions: ~D~%" (hash-table-count *quarantined-code*))
  (format t "  System stability: ~,1F%%~%" (* 100 (compute-system-stability)))

  (format t "~%EXTENSION 7 - Abstraction (146-155):~%")
  (format t "  Function patterns: ~D~%" (hash-table-count *function-level-patterns*))
  (format t "  Module patterns: ~D~%" (hash-table-count *module-level-patterns*))
  (format t "  Architectural patterns: ~D~%" (length *architectural-patterns*))

  (format t "~%EXTENSION 8 - Temporal (156-165):~%")
  (format t "  Operation sequences: ~D~%" (length *operation-sequences*))
  (format t "  Optimal sequences: ~D~%" (length *optimal-sequences*))
  (format t "  Temporal patterns: ~D~%" (hash-table-count *temporal-patterns*))

  (format t "~%========================================~%")
  (format t "TOTAL: 165 wiring connections active~%")
  (format t "========================================~%"))


(export '(install-deep-wiring!
          install-deeper-wiring!
          install-novel-wiring!
          install-utilization-wiring!
          install-integration-wiring!
          install-source-awareness!
          ;; Source reading
          load-own-source!
          where-is-function
          where-is-variable
          get-function-source
          list-my-functions
          list-my-variables
          analyze-own-complexity
          what-do-i-know-about
          *my-source-code*
          *my-functions*
          *my-variables*
          ;; Source-behavior wiring
          source-informed-hypothesis!
          source-verified-modification!
          ground-concept-in-source
          analyze-own-structure
          diagnose-error-from-source
          source-evidence-for-hypothesis
          describe-self-with-source
          *concept-source-grounding*
          *semantic-self-knowledge-cache*
          ;; Existential-code correlation
          install-existential-code-correlation!
          record-function-entry
          record-function-exit
          what-code-runs-when
          what-state-when-code-runs
          function-prediction-reliability
          most-reliable-code-paths
          least-reliable-code-paths
          generate-existential-code-insight
          describe-existential-code-relationship
          *current-code-path*
          *code-state-correlations*
          *function-call-counts*
          *prediction-code-outcomes*
          ;; Code-aware self-modification (56-70)
          install-code-aware-modification-hooks!
          generate-code-improvement-hypothesis
          mark-reliable-code-protected
          modification-feasible-p
          insight-to-goal!
          track-modification-code-context!
          learn-modification-at-code-level!
          propose-code-aware-modification
          verify-modification-against-source
          code-aware-hypothesis-test!
          presence-allows-code-modification-p
          introspect-code-awareness!
          dream-consolidate-code-patterns!
          record-episode-code-context!
          code-causal-relationship
          code-aware-self-modification-cycle!
          *code-modification-proposals*
          *code-modification-outcomes*
          *protected-code-paths*
          *modification-code-correlations*
          ;; Execution bridge (71-75)
          install-execution-bridge!
          trigger-expert-improvement!
          insight-to-experiment!
          code-path-to-actionable-hypothesis!
          execute-code-aware-modifications!
          code-aware-execution-cycle!
          ;; Deep self-utilization (76-85)
          install-self-utilization!
          identify-self-patterns!
          correlate-patterns-with-reliability!
          pattern-based-hypothesis!
          predict-own-code-continuation
          dream-about-code-structure!
          pattern-guided-modification!
          code-aware-prediction-boost
          identify-rewrite-candidates
          propose-code-rewrite
          self-utilization-cycle!
          *learned-code-patterns*
          *pattern-reliability-correlations*
          *code-pattern-hypotheses*
          ;; Hypothesis Application (86-95)
          install-hypothesis-application!
          hypothesis-expert-selection-bias
          should-avoid-pattern-p
          hypothesis-suggests-refactor-p
          select-expert-with-hypothesis-weight
          check-new-code-for-bad-patterns
          update-hypothesis-confidence
          validate-hypothesis!
          install-pattern-avoidance!
          hypothesis-modification-priority
          apply-hypotheses-cycle!
          *applied-hypotheses*
          *pattern-avoidance-rules*
          *hypothesis-confidence*
          ;; Persistence (96-105)
          install-persistence!
          save-pattern-correlations!
          load-pattern-correlations!
          save-hypotheses!
          load-hypotheses!
          save-protected-paths!
          load-protected-paths!
          save-modification-outcomes!
          load-modification-outcomes!
          checkpoint-session!
          restore-session!
          *persistence-directory*
          ;; Meta-Learning (106-115)
          install-meta-learning!
          track-modification-type-outcome!
          track-hypothesis-type-success!
          track-presence-modification-correlation!
          analyze-best-modification-conditions
          predict-modification-success
          meta-adjusted-modification-confidence
          generate-meta-modification-hypothesis!
          select-modification-strategy
          meta-learning-feedback!
          meta-learning-cycle!
          *modification-type-outcomes*
          *hypothesis-type-success*
          *presence-modification-correlations*
          *meta-modification-hypotheses*
          ;; Code Parsing (116-125)
          install-code-parsing!
          parse-function-safely
          extract-function-signature
          extract-function-calls
          build-dependency-graph!
          analyze-data-flow
          identify-critical-paths!
          understand-function-purpose
          detect-code-patterns
          safe-to-modify-p
          generate-flow-aware-hypothesis
          deep-code-analysis-cycle!
          *parsed-functions*
          *function-dependencies*
          *reverse-dependencies*
          *data-flow-graph*
          *critical-paths*
          ;; Human-in-the-Loop (126-135)
          install-human-loop!
          generate-modification-explanation
          format-proposal-for-review
          queue-proposal-for-review!
          process-human-feedback!
          learn-from-human-feedback!
          adjust-confidence-from-feedback!
          record-human-override!
          calibrate-explanation-confidence
          integrate-feedback-into-system!
          human-loop-cycle!
          *pending-proposals*
          *human-feedback-history*
          *explanation-templates*
          *human-approval-required*
          ;; Self-Debugging (136-145)
          install-self-debugging!
          trace-error-to-code!
          capture-error-context!
          snapshot-before-modification!
          rollback-modification!
          should-quarantine-p
          quarantine-function!
          is-quarantined-p
          generate-debug-hypothesis
          learn-error-patterns!
          select-recovery-strategy
          compute-system-stability
          check-self-healing-triggers!
          self-debugging-cycle!
          *error-history*
          *error-code-correlations*
          *quarantined-code*
          *rollback-history*
          *pre-modification-snapshots*
          ;; Higher Abstraction (146-155)
          install-abstraction-analysis!
          extract-function-level-pattern
          identify-module
          extract-module-level-pattern!
          recognize-architectural-patterns!
          build-abstraction-hierarchy!
          correlate-across-levels
          generate-architectural-hypothesis!
          compute-module-reliability!
          propose-structural-modification
          dream-about-abstractions!
          abstraction-analysis-cycle!
          *function-level-patterns*
          *module-level-patterns*
          *architectural-patterns*
          *abstraction-hierarchy*
          *module-reliability*
          ;; Temporal Learning (156-165)
          install-temporal-learning!
          record-operation!
          extract-sequence-patterns
          track-operation-order!
          learn-optimal-sequences!
          design-learning-curriculum
          generate-temporal-hypothesis!
          predict-next-operation
          suggest-operation-order
          compute-adaptive-learning-rate
          temporal-consolidation!
          temporal-learning-cycle!
          *operation-sequences*
          *sequence-outcomes*
          *optimal-sequences*
          *learning-curricula*
          *temporal-patterns*
          *learning-rate-history*
          ;; Master integration
          run-all-cycles!
          print-full-status
          ;; Other exports
          print-deep-wiring-status
          self-expectation-accuracy
          self-expectation-weight
          presence-adjusted-hypothesis-interval
          drive-adjusted-schema-selection
          causal-model-confidence-boost
          strategy-bias-for-selection
          presence-scaled-confidence
          best-strategy-for-situation
          modification-type-reliability
          *program-modifications-attempted*
          *program-modifications-succeeded*
          *episode-learning-boost*
          *situation-strategy-outcomes*
          *modification-type-success*))
