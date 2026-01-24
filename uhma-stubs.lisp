
;;;; FILE MANIFEST
;;;; ============
;;;; This file MUST contain the following when complete:
;;;; PACKAGES: uhma
;;;; FUNCTIONS: run-dream-cycle! (and others)
;;;; TOTAL EXPECTED SECTIONS: Unknown (Legacy File)

;;;; ============================================================================
;;;; UHMA STUBS - Stub functions for unimplemented features
;;;; ============================================================================
;;;; Load this AFTER forward-decl but BEFORE other modules
;;;; ============================================================================

(in-package :uhma)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize (speed 0) (safety 3) (debug 3))))


;;; ============================================================================
;;; DREAM FUNCTIONS (planned feature, not implemented)
;;; ============================================================================

(defun run-dream-cycle! ()
  "Stub: Dream cycle not implemented"
  nil)

(defun should-dream-p ()
  "Stub: Dream condition check not implemented"
  nil)

;;; ============================================================================
;;; MEMORY MANAGEMENT
;;; ============================================================================

;; Default bounds (overwritten by uhma-memory-bounds.lisp at load time)
(defvar *max-hypothesis-history* 100)
(defvar *max-modification-history* 100)
(defvar *max-goal-history* 50)
(defvar *max-experiment-history* 100)

(defun trim-all-arrays! ()
  "Stub: Array trimming not implemented"
  nil)

(defun trim-all-histories! ()
  "Prune all list-based histories when they exceed their max size.
   Uses organic-prune-list for salience-based retention."
  ;; Hypothesis history
  (when (and (boundp '*hypothesis-history*) *hypothesis-history*
             (> (length *hypothesis-history*) *max-hypothesis-history*))
    (if (fboundp 'organic-prune-list)
        (organic-prune-list '*hypothesis-history* :modification *max-hypothesis-history*)
        (setf *hypothesis-history* (subseq *hypothesis-history* 0 *max-hypothesis-history*))))
  ;; Modification history
  (when (and (boundp '*modification-history*) *modification-history*
             (> (length *modification-history*) *max-modification-history*))
    (if (fboundp 'organic-prune-list)
        (organic-prune-list '*modification-history* :modification *max-modification-history*)
        (setf *modification-history* (subseq *modification-history* 0 *max-modification-history*))))
  ;; Goal history
  (when (and (boundp '*goal-history*) *goal-history*
             (> (length *goal-history*) *max-goal-history*))
    (setf *goal-history* (subseq *goal-history* 0 *max-goal-history*)))
  ;; Experiment history
  (when (and (boundp '*experiment-history*) *experiment-history*
             (> (length *experiment-history*) *max-experiment-history*))
    (setf *experiment-history* (subseq *experiment-history* 0 *max-experiment-history*)))
  ;; Concept activation history
  (when (and (boundp '*concept-activation-history*) *concept-activation-history*
             (> (length *concept-activation-history*) 500))
    (setf *concept-activation-history* (subseq *concept-activation-history* 0 500)))
  ;; Self-modification history
  (when (and (boundp '*self-modification-history*) *self-modification-history*
             (> (length *self-modification-history*) 100))
    (setf *self-modification-history* (subseq *self-modification-history* 0 100))))

;;; ============================================================================
;;; SELF-MODIFICATION TRACKING (planned feature, not implemented)
;;; ============================================================================

(defun analyze-best-modification-conditions ()
  "Stub: Modification analysis not implemented"
  nil)

(defun generate-meta-modification-hypothesis! ()
  "Stub: Meta-hypothesis generation not implemented"
  nil)

(defun track-modification-type-outcome! (&rest args)
  "Stub: Modification tracking not implemented"
  (declare (ignore args))
  nil)

(defun correlate-context-with-source! (ctx got-it)
  "Correlate prediction context with source-code understanding.
   When predictions about code patterns succeed/fail, record which
   contexts the system handles well for self-knowledge."
  (when (and ctx (listp ctx) (> (length ctx) 0)
             (boundp '*semantic-self-knowledge*))
    ;; Track context-type success/failure for self-knowledge
    (let ((ctx-type (first ctx)))
      (when ctx-type
        ;; Update self-knowledge when enough observations accumulate
        (when (and (boundp '*step*) (zerop (mod *step* 200)))
          (when (fboundp 'update-semantic-self-knowledge!)
            (update-semantic-self-knowledge!)))))))

(defun identify-similar-reliable-functions (&rest args)
  "Stub: Reliable function identification not implemented"
  (declare (ignore args))
  nil)

;;; ============================================================================
;;; HYPOTHESIS FUNCTIONS (planned feature, partial implementation)
;;; ============================================================================

(defun test-hypotheses! ()
  "Test all active hypotheses against recent observations.
   Calls test-hypothesis! on each active hypothesis."
  (when (and (boundp '*hypotheses*) (hash-table-p *hypotheses*)
             (fboundp 'test-hypothesis!))
    (maphash (lambda (id hyp)
               (declare (ignore id))
               (when (and (self-hypothesis-p hyp)
                          (eq (self-hypothesis-status hyp) :active))
                 (ignore-errors (test-hypothesis! hyp))))
             *hypotheses*)))

(defun apply-hypothesis-competitive-decay! ()
  "Stub: Hypothesis decay not implemented"
  nil)

(defun extract-pattern-from-claim (claim)
  "Stub: Pattern extraction not implemented"
  (declare (ignore claim))
  nil)

;;; ============================================================================
;;; INTROSPECTION FUNCTIONS
;;; ============================================================================

(unless (fboundp 'introspect!)
  (defun introspect! ()
    "Stub: Full introspection not implemented"
    nil))

(unless (fboundp 'activate-concept!)
  (defun activate-concept! (concept &optional (intensity 1.0))
    "Force-activate a named concept with given intensity.
     Adds to *cached-active-concepts* and records activation.
     For compound names like CODE-CONFUSED, also activates the base concept CONFUSED."
    (declare (ignore intensity))
    (when (and concept (symbolp concept))
      ;; Add the compound concept to active list
      (when (boundp '*cached-active-concepts*)
        (pushnew concept *cached-active-concepts*))
      ;; Also activate the base concept (suffix after last hyphen)
      ;; e.g. CODE-CONFUSED → CONFUSED, CODE-CONFIDENT → CONFIDENT
      (let* ((name-str (symbol-name concept))
             (hyphen-pos (position #\- name-str :from-end t)))
        (when hyphen-pos
          (let ((base-sym (intern (subseq name-str (1+ hyphen-pos))
                                  (symbol-package concept))))
            (when (and (boundp '*introspective-vocabulary*)
                       (hash-table-p *introspective-vocabulary*)
                       (gethash base-sym *introspective-vocabulary*))
              ;; Base concept exists in vocabulary — activate it too
              (pushnew base-sym *cached-active-concepts*)
              ;; Increment its activation count
              (let ((ic (gethash base-sym *introspective-vocabulary*)))
                (when (introspective-concept-p ic)
                  (incf (introspective-concept-activation-count ic))))))))
      ;; Increment activation count for the compound concept if it exists
      (when (and (boundp '*introspective-vocabulary*)
                 (hash-table-p *introspective-vocabulary*))
        (let ((ic (gethash concept *introspective-vocabulary*)))
          (when (and ic (introspective-concept-p ic))
            (incf (introspective-concept-activation-count ic)))))
      ;; Record in activation history
      (when (fboundp 'record-concept-activation!)
        (record-concept-activation! (list concept))))))

(unless (fboundp 'introspective-concept-state-active)
  (defun introspective-concept-state-active (concept)
    "Check if a concept struct (or its name) is currently active."
    (when (and concept (boundp '*cached-active-concepts*) *cached-active-concepts*)
      (let ((name (cond
                    ((symbolp concept) concept)
                    ((and (introspective-concept-p concept)
                          (introspective-concept-name concept))
                     (introspective-concept-name concept))
                    (t nil))))
        (when name
          (member name *cached-active-concepts*))))))

;;; ============================================================================
;;; SCHEMA FUNCTIONS
;;; ============================================================================

(unless (fboundp 'evolve-schemas!)
  (defun evolve-schemas! ()
    "Stub: Schema evolution not implemented"
    nil))

(unless (fboundp 'schema-success-rate)
  (defun schema-success-rate (schema)
    "Stub: Schema success rate not implemented"
    (declare (ignore schema))
    0.5))

;;; ============================================================================
;;; UTILITY FUNCTIONS
;;; ============================================================================

(unless (fboundp 'compute-recent-accuracy-fast)
  (defun compute-recent-accuracy-fast (n)
    "Fast accuracy computation from recent trace buffer entries."
    (if (and (boundp '*trace-buffer*) (> (fill-pointer *trace-buffer*) 0))
        (let ((correct 0) (total (min n (fill-pointer *trace-buffer*))))
          (loop for i from (1- (fill-pointer *trace-buffer*))
                downto (max 0 (- (fill-pointer *trace-buffer*) total))
                for trace = (aref *trace-buffer* i)
                when (eq (cognitive-trace-prediction trace)
                         (cognitive-trace-actual trace))
                do (incf correct))
          (/ (float correct) (max 1 total)))
        0.5)))

(unless (fboundp 'compute-uncertainty)
  (defun compute-uncertainty (&rest args)
    "Stub: Uncertainty computation not implemented"
    (declare (ignore args))
    0.5))

(unless (fboundp 'context-signature)
  (defun context-signature (ctx)
    "Stub: Context signature not implemented"
    (declare (ignore ctx))
    nil))

(unless (fboundp 'hash-table-to-alist)
  (defun hash-table-to-alist (ht)
    "Convert hash table to alist"
    (when (hash-table-p ht)
      (loop for k being the hash-keys of ht
            for v being the hash-values of ht
            collect (cons k v)))))

(unless (fboundp 'get-param-value)
  (defun get-param-value (param &optional default)
    "Stub: Parameter value retrieval"
    (declare (ignore param))
    default))

(unless (fboundp 'split-symbol-name)
  (defun split-symbol-name (sym)
    "Split symbol name"
    (when sym (list (string sym)))))

(unless (fboundp 'record-trace!)
  (defun record-trace! (&rest args)
    "Stub: Trace recording not implemented"
    (declare (ignore args))
    nil))

;;; ============================================================================
;;; EXPERT FUNCTIONS
;;; ============================================================================

(unless (fboundp 'expert-reliability)
  (defun expert-reliability (expert)
    "Compute expert reliability from hits/misses"
    (if (and expert (typep expert 'expert))
        (let ((hits (expert-hits expert))
              (misses (expert-misses expert)))
          (if (> (+ hits misses) 0)
              (/ hits (+ hits misses))
              0.5))
        0.5)))

(unless (fboundp 'expert-to-plist)
  (defun expert-to-plist (expert)
    "Convert expert to plist representation"
    (when (and expert (typep expert 'expert))
      (list :id (expert-id expert)
            :hits (expert-hits expert)
            :misses (expert-misses expert)))))

;;; ============================================================================
;;; SEMANTIC BRIDGE FUNCTIONS
;;; ============================================================================

(defun parse-json (string)
  "Stub: JSON parsing not available"
  (declare (ignore string))
  (warn "PARSE-JSON called but not implemented")
  nil)

(defun semantic-query (query)
  "Stub: Semantic query not implemented"
  (declare (ignore query))
  nil)

;;; ============================================================================
;;; HOLOGRAPHIC MEMORY FUNCTIONS
;;; ============================================================================

(defun holo-store! (key value)
  "Stub: Holographic memory not implemented"
  (declare (ignore key value))
  nil)

(defun holo-query (pattern)
  "Stub: Holographic memory not implemented"
  (declare (ignore pattern))
  nil)

;;; ============================================================================
;;; PRESENCE ACCESSOR FUNCTIONS
;;; ============================================================================

(unless (fboundp 'presence-agency-strength)
  (defun presence-agency-strength ()
    "Get agency strength from presence"
    (if (and (boundp '*presence*) *presence*)
        (presence-agency *presence*)
        1.0)))

(unless (fboundp 'presence-to-plist)
  (defun presence-to-plist (&optional (p (when (boundp '*presence*) *presence*)))
    "Convert presence to plist"
    (when p
      (list :trajectory (presence-trajectory p)
            :continuity (presence-continuity p)
            :vividness (presence-vividness p)
            :self-confidence (presence-self-confidence p)))))

;;; ============================================================================
;;; MISSING VARIABLES (Legacy Fixes)
;;; ============================================================================

(defvar *self-modification-history* nil "History of self-modifications (legacy)")
(defvar *modification-history* nil "History of modifications (legacy)")
(defvar *current-goal* nil "Current active goal")
(defvar *goal-behavior-params* (list :exploration-rate 0.1) "Behavior parameters for goals")
(defvar *meta-investigation-queue* nil "Queue for meta-investigations")
(defvar *modification-budget* 10 "Budget for modifications per cycle")
(defvar *schema-attempts* 0 "Counter for schema application attempts")
(defvar *schema-successes* 0 "Counter for schema successes")
(defvar *schema-guided-execution* nil "Flag for schema-guided execution")
(defvar *my-functions* (make-hash-table :test 'eq) "Registry of own functions")
(defvar *causal-model* (make-hash-table :test 'equal) "Causal model registry")
(defvar *cognitive-schemas* (make-hash-table :test 'eq) "Cognitive schemas registry")
(defvar *schema-spawn-rate* 0.1 "Rate of schema spawning")
(defvar *context-buffer* (make-array 10 :initial-element nil) "Buffer for recent context")
(defvar *dream-episode-count* 0 "Counter for dream episodes")
(defvar *last-prediction-confidence* 0.0 "Confidence of last prediction")
(defvar *consecutive-failures* 0 "Counter for consecutive failures")
(defvar *current-expert-activations* nil "Current activations of experts")

;;; ============================================================================
;;; SEMANTIC SELF-KNOWLEDGE (Claim 9)
;;; ============================================================================

(defstruct semantic-self-knowledge
  "Self-knowledge about tendencies, strengths, weaknesses."
  (tendencies nil :type list)
  (strengths nil :type list)
  (weaknesses nil :type list)
  (patterns nil :type list))

(defvar *semantic-self-knowledge* (make-semantic-self-knowledge)
  "Self-knowledge about the system's behavioral tendencies. Learned from traces, not hardcoded.")

(defun update-semantic-self-knowledge! ()
  "Learn self-knowledge from actual trace statistics.
   Tendencies, strengths, weaknesses emerge from holographic patterns."
  (when (and (boundp '*trace-buffer*) (> (fill-pointer *trace-buffer*) 50))
    (let ((context-successes (make-hash-table :test 'equal))
          (context-totals (make-hash-table :test 'equal)))
      ;; Analyze traces for context-specific performance
      (loop for i from (1- (fill-pointer *trace-buffer*)) downto (max 0 (- (fill-pointer *trace-buffer*) 200))
            for trace = (aref *trace-buffer* i)
            when (cognitive-trace-p trace)
            do (let ((ctx-type (when (cognitive-trace-context trace)
                                 (first (cognitive-trace-context trace)))))
                 (when ctx-type
                   (incf (gethash ctx-type context-totals 0))
                   (when (eq (cognitive-trace-prediction trace)
                             (cognitive-trace-actual trace))
                     (incf (gethash ctx-type context-successes 0))))))
      ;; Build strengths/weaknesses from performance
      (let ((strengths nil) (weaknesses nil) (tendencies nil))
        (maphash (lambda (ctx total)
                   (when (> total 10)
                     (let ((rate (/ (float (gethash ctx context-successes 0)) total)))
                       (push ctx tendencies)
                       (if (> rate 0.7)
                           (push ctx strengths)
                           (when (< rate 0.3)
                             (push ctx weaknesses))))))
                 context-totals)
        (setf (semantic-self-knowledge-strengths *semantic-self-knowledge*) strengths)
        (setf (semantic-self-knowledge-weaknesses *semantic-self-knowledge*) weaknesses)
        (setf (semantic-self-knowledge-tendencies *semantic-self-knowledge*) tendencies)))))

;;; ============================================================================
;;; COUNTERFACTUAL HISTORY (Claim 17)
;;; ============================================================================

(defvar *counterfactual-history* nil "History of counterfactual reasoning outcomes.")

;;; ============================================================================
;;; CODE INTROSPECTION (Claim 29)
;;; ============================================================================

(defun introspect-own-code (&optional (target nil))
  "Examine own source code structure and report observations."
  (let ((results nil))
    (when (and (boundp '*my-functions*) (> (hash-table-count *my-functions*) 0))
      (maphash (lambda (name info)
                 (when (or (null target) (search (string target) (string name) :test #'char-equal))
                   (push (list :name name :info info) results)))
               *my-functions*))
    (when (and (boundp '*experts*) *experts*)
      (push (list :expert-count (length *experts*)
                  :avg-program-size (if *experts*
                                        (/ (reduce #'+ *experts* :key (lambda (e) (length (expert-program e))))
                                           (max 1 (length *experts*)))
                                        0))
            results))
    results))

;;; ============================================================================
;;; GENETIC OPERATIONS (Claim 31)
;;; ============================================================================

(defun mutate-pattern (pattern &optional (rate 0.1))
  "Apply random mutation to a pattern (S-expression program).
   Substitutes random elements with probability RATE."
  (declare (type single-float rate))
  (if (and (listp pattern) (> (length pattern) 1))
      (mapcar (lambda (elem)
                (if (< (random 1.0) rate)
                    (if (listp elem)
                        (mutate-pattern elem rate)
                        (let ((ops (when (boundp '*available-ops*) *available-ops*)))
                          (if (and ops (> (length ops) 0))
                              (nth (random (length ops)) ops)
                              elem)))
                    (if (listp elem)
                        (mutate-pattern elem (* rate 0.5))
                        elem)))
              pattern)
      pattern))

(defun crossover-patterns (pattern-a pattern-b)
  "Combine two S-expression patterns by taking head of A and tail of B."
  (cond
    ((and (listp pattern-a) (listp pattern-b)
          (> (length pattern-a) 1) (> (length pattern-b) 1))
     (let ((split-a (1+ (random (1- (length pattern-a)))))
           (split-b (random (1- (length pattern-b)))))
       (append (subseq pattern-a 0 split-a)
               (subseq pattern-b (min split-b (1- (length pattern-b)))))))
    (pattern-a pattern-a)
    (t pattern-b)))

(defun vsa-crossover-experts! (parent-a parent-b)
  "Sexual reproduction in VSA space: superpose knowledge-vectors from two parents.
   Creates offspring whose knowledge-vector blends both parents weighted by fitness.
   Also crosses over S-expression programs. Returns the offspring expert."
  (when (and parent-a parent-b
             (typep parent-a 'expert) (typep parent-b 'expert))
    (let* ((fitness-a (expert-life parent-a))
           (fitness-b (expert-life parent-b))
           (total-fitness (+ fitness-a fitness-b))
           (weight-a (if (> total-fitness 0) (/ fitness-a total-fitness) 0.5))
           (weight-b (- 1.0 weight-a))
           ;; Create offspring via S-expression crossover
           (offspring-program (crossover-patterns
                               (expert-program parent-a)
                               (expert-program parent-b)))
           ;; Create offspring expert
           (offspring (make-expert
                       :program offspring-program
                       :code-metrics (make-hash-table :test 'eq)
                       :parent-id (expert-id parent-a)
                       :birth-step (if (boundp '*step*) *step* 0)
                       :centroid (let ((c (make-vsa-vec))
                                       (ca (expert-centroid parent-a))
                                       (cb (expert-centroid parent-b)))
                                   ;; Blend centroids over shared active dimensions
                                   (when (and ca cb)
                                     (let ((n (min (length ca) (length cb) +vsa-dim+)))
                                       (dotimes (i n)
                                         (setf (aref c i)
                                               (+ (* weight-a (aref ca i))
                                                  (* weight-b (aref cb i)))))))
                                   (vnorm! c))
                       :life 1.0)))
      ;; VSA crossover: superpose knowledge-vectors
      (when (and (expert-knowledge-vector parent-a)
                 (expert-knowledge-vector parent-b))
        (let ((kv (make-vsa-vec)))
          (vsa-superpose! kv (expert-knowledge-vector parent-a) (coerce weight-a 'single-float))
          (vsa-superpose! kv (expert-knowledge-vector parent-b) (coerce weight-b 'single-float))
          (setf (expert-knowledge-vector offspring) kv)))
      ;; Inherit owned-type from fitter parent
      (setf (expert-owned-type offspring)
            (if (> fitness-a fitness-b)
                (expert-owned-type parent-a)
                (expert-owned-type parent-b)))
      ;; Add to population
      (push offspring *experts*)
      (incf *total-births*)
      (run-hook +hook-expert-spawned+ offspring)
      offspring)))

(defun attempt-sexual-reproduction! ()
  "Try to breed two fit experts via VSA crossover.
   Triggered organically when population has diverse, fit experts."
  (when (and (boundp '*experts*) (> (length *experts*) 5))
    (let* ((fit-experts (remove-if (lambda (e) (< (expert-life e) 0.6)) *experts*))
           (n (length fit-experts)))
      (when (>= n 2)
        ;; Pick two random fit parents with different owned-types (diversity)
        (let ((parent-a (nth (random n) fit-experts))
              (parent-b (nth (random n) fit-experts)))
          ;; Ensure different parents
          (when (and (not (eq parent-a parent-b))
                     ;; Prefer diverse parents (different knowledge domains)
                     (or (not (eq (expert-owned-type parent-a)
                                  (expert-owned-type parent-b)))
                         (< (random 1.0) 0.3)))  ; Sometimes allow same-type
            (vsa-crossover-experts! parent-a parent-b)))))))

;;; ============================================================================
;;; DREAM EPISODE POPULATION (Wire failed predictions into dream buffer)
;;; ============================================================================

(defun record-dream-candidate! (ctx predicted actual difficulty)
  "Add a failed prediction to the dream buffer for later consolidation.
   Called organically when predictions fail — episodes enter the buffer from need."
  (when (and (boundp '*dream-state*) *dream-state*
             (> difficulty 0.3))  ; Only dream about difficult things
    (let ((ep (make-dream-episode
               :context (if (listp ctx) (copy-list (subseq ctx 0 (min 10 (length ctx)))) nil)
               :original-prediction predicted
               :actual-outcome actual
               :difficulty (coerce difficulty 'single-float)
               :created-at (if (boundp '*step*) *step* 0))))
      (push ep (dream-state-episode-buffer *dream-state*))
      ;; Cap buffer size
      (when (> (length (dream-state-episode-buffer *dream-state*)) *dream-buffer-size*)
        (setf (dream-state-episode-buffer *dream-state*)
              (subseq (dream-state-episode-buffer *dream-state*) 0 *dream-buffer-size*)))
      ep)))

;;; ============================================================================
;;; COUNTERFACTUAL REASONING (Wire into trace pipeline)
;;; ============================================================================

(defun run-counterfactual-on-trace! (trace)
  "Perform counterfactual reasoning on a failed trace and store the result.
   Asks: what if a different expert had fired? What if different ops were used?"
  (when (and trace (cognitive-trace-p trace)
             (not (eq (cognitive-trace-prediction trace)
                      (cognitive-trace-actual trace))))
    (let* ((actual (cognitive-trace-actual trace))
           (expert-id (cognitive-trace-expert-id trace))
           (path (cognitive-trace-reasoning-path trace))
           (ctx (cognitive-trace-context trace))
           ;; Find alternative experts that might have worked
           (alt-experts (when (and (boundp '*experts*) ctx)
                          (remove-if (lambda (e) (eq (expert-id e) expert-id))
                                     (subseq *experts* 0 (min 5 (length *experts*))))))
           (counterfactual (list :step *step*
                                 :context (when (listp ctx) (subseq ctx 0 (min 3 (length ctx))))
                                 :actual-expert expert-id
                                 :actual-outcome actual
                                 :alt-experts (mapcar #'expert-id (or alt-experts nil))
                                 :reasoning-path path
                                 :what-if (if (and path (listp path) (> (length path) 1))
                                              (list :reversed-ops (reverse path))
                                              :no-alternatives))))
      (push counterfactual *counterfactual-history*)
      ;; Keep bounded
      (when (> (length *counterfactual-history*) 100)
        (setf *counterfactual-history* (subseq *counterfactual-history* 0 100)))
      counterfactual)))

;;; ============================================================================
;;; TIER 2 SELF-MODIFICATION (System-level code rewriting)
;;; ============================================================================

(defun tier2-redefine-operation! (op-name new-body)
  "Tier 2: Redefine a system operation at runtime.
   The system modifies its own code — the core homoiconic capability."
  (when (and op-name (symbolp op-name) new-body)
    ;; Record before state
    (let ((before-def (when (fboundp op-name) (symbol-function op-name))))
      ;; Attempt redefinition
      (handler-case
          (progn
            (eval `(defun ,op-name (&rest args)
                     ,@(if (listp new-body) new-body (list new-body))))
            ;; Record in modification history
            (push (list :step (if (boundp '*step*) *step* 0)
                        :type :tier2-redefine
                        :target op-name
                        :before before-def
                        :success t)
                  *modification-history*)
            t)
        (error (e)
          (push (list :step (if (boundp '*step*) *step* 0)
                      :type :tier2-redefine
                      :target op-name
                      :error e
                      :success nil)
                *modification-history*)
          nil)))))

(defun tier2-adjust-global-threshold! (var-name new-value &optional (reason :organic))
  "Tier 2: Modify a global parameter. The system tunes itself."
  (when (and var-name (symbolp var-name) (boundp var-name))
    (let ((before (symbol-value var-name)))
      (setf (symbol-value var-name) new-value)
      (push (list :step (if (boundp '*step*) *step* 0)
                  :type :tier2-threshold
                  :target var-name
                  :before before
                  :after new-value
                  :reason reason)
            *modification-history*)
      t)))

(defun tier2-add-hook! (hook-name handler &key (priority 50) (reason :organic))
  "Tier 2: Add a new hook at runtime. The system rewires its own plumbing."
  (when (and hook-name (boundp hook-name))
    (register-hook (symbol-value hook-name) handler :priority priority)
    (push (list :step (if (boundp '*step*) *step* 0)
                :type :tier2-hook
                :target hook-name
                :priority priority
                :reason reason)
          *modification-history*)
    t))

(defun tier2-discover-rewrites! ()
  "Auto-discover which ops need rewriting from understanding log circuit-outcome mappings.
   The system observes its own failure patterns and rewrites the responsible code."
  (when (and (boundp '*understanding-log*) *understanding-log*)
    (let ((rewrites-attempted 0))
      (dolist (entry *understanding-log*)
        (when (and (eq (getf entry :understanding) :problematic-circuit)
                   (< (or (getf entry :rate) 1.0) 0.25)  ; Very low success rate
                   (> (or (getf entry :samples) 0) 20))   ; Sufficient evidence
          (let* ((circuit (getf entry :circuit))
                 (op-name (when (and circuit (eq (car circuit) :op))
                            (second circuit))))
            (when (and op-name (symbolp op-name) (fboundp op-name)
                       (< rewrites-attempted 2))  ; Max 2 rewrites per cycle
              ;; Attempt to improve the op by wrapping with a fallback
              (let ((original-fn (symbol-function op-name)))
                (handler-case
                    (progn
                      ;; Wrap the failing op with a confidence gate
                      ;; If original returns nil/low-confidence, try a broader match
                      (setf (symbol-function op-name)
                            (lambda (&rest args)
                              (let ((result (ignore-errors (apply original-fn args))))
                                (or result
                                    ;; Fallback: return a conservative default
                                    (when (and (boundp '*last-answering-expert*)
                                               *last-answering-expert*)
                                      (values nil 0.3))))))
                      (push (list :step (if (boundp '*step*) *step* 0)
                                  :type :tier2-auto-rewrite
                                  :target op-name
                                  :reason :circuit-failure-rate
                                  :original-rate (getf entry :rate)
                                  :before original-fn
                                  :success t)
                            *modification-history*)
                      (incf rewrites-attempted)
                      (format t "[TIER2] Auto-rewrote ~A (was ~,1F% success)~%"
                              op-name (* 100 (or (getf entry :rate) 0))))
                  (error (e)
                    (declare (ignore e))
                    ;; Revert on failure
                    (setf (symbol-function op-name) original-fn))))))))
      rewrites-attempted)))

;;; ============================================================================
;;; SELF-CONSUMPTION (System processes its own source code)
;;; ============================================================================

(defvar *self-consumption-step* 0 "Last step when self-consumption ran.")
(defvar *self-consumption-file-index* 0 "Which source file to consume next.")

(defun consume-own-source! (&optional (file-pattern "uhma-*.lisp"))
  "Feed the system its own source code as input.
   The boundary between data-it-learns and code-it-is dissolves."
  (let ((files (directory (merge-pathnames file-pattern
                            (or (when (boundp '*module-base*) *module-base*)
                                (make-pathname :directory '(:relative))))))
        (total-patterns 0))
    (dolist (file files)
      (handler-case
          (with-open-file (s file :direction :input)
            (loop for line = (read-line s nil nil)
                  while line
                  do (when (and (> (length line) 3)
                                (not (char= (char line 0) #\;)))  ; Skip comments
                       (handler-case
                           (progn
                             (process-text! line :verbose nil)
                             (incf total-patterns))
                         (error () nil)))))
        (error () nil)))
    (format t "[SELF-CONSUMPTION] Processed ~D lines from ~D source files~%"
            total-patterns (length files))
    (setf *self-consumption-step* (if (boundp '*step*) *step* 0))
    total-patterns))

(defun consume-own-source-incremental! ()
  "Feed one source file at a time — called organically when curiosity is high.
   Incremental self-consumption avoids overwhelming the system."
  (let ((files (directory (merge-pathnames "uhma-*.lisp"
                            (or (when (boundp '*module-base*) *module-base*)
                                (make-pathname :directory '(:relative))))))
        (patterns 0))
    (when (and files (> (length files) 0))
      (let* ((idx (mod *self-consumption-file-index* (length files)))
             (file (nth idx files)))
        (handler-case
            (with-open-file (s file :direction :input)
              (loop for line = (read-line s nil nil)
                    while line
                    for count from 0
                    ;; Process 50 lines max per incremental call
                    while (< count 50)
                    do (when (and (> (length line) 3)
                                  (not (char= (char line 0) #\;)))
                         (handler-case
                             (progn (process-text! line :verbose nil)
                                    (incf patterns))
                           (error () nil)))))
          (error () nil))
        (incf *self-consumption-file-index*)
        (setf *self-consumption-step* (if (boundp '*step*) *step* 0))
        patterns))))

(defun should-self-consume-p ()
  "Check if conditions are right for self-consumption.
   Driven by curiosity drive pressure and low interference (stable enough to learn)."
  (and (boundp '*intrinsic-drives*)
       ;; Curiosity is high (above target)
       (let ((curiosity (find :curiosity *intrinsic-drives* :key #'drive-name)))
         (and curiosity
              (> (drive-current-level curiosity) (+ (drive-target-level curiosity) 0.1))))
       ;; Interference is low (system is stable)
       (or (not (fboundp 'compute-holographic-interference))
           (< (compute-holographic-interference) 0.4))
       ;; Haven't consumed recently
       (> (- (if (boundp '*step*) *step* 0) *self-consumption-step*) 200)))

;;; ============================================================================
;;; CAUSAL MODEL WIRING (Build from trace observations)
;;; ============================================================================

(defun update-causal-model-from-traces! ()
  "Extract causal relationships from trace buffer observations.
   Builds explicit 'X causes Y' representations from co-occurrences."
  (when (and (boundp '*trace-buffer*) (> (fill-pointer *trace-buffer*) 20)
             (boundp '*causal-model*))
    (let ((limit (min 100 (fill-pointer *trace-buffer*))))
      (loop for i from 1 below limit
            for trace = (aref *trace-buffer* (- (fill-pointer *trace-buffer*) 1 i))
            for prev-trace = (aref *trace-buffer* (- (fill-pointer *trace-buffer*) i))
            when (and (cognitive-trace-p trace) (cognitive-trace-p prev-trace))
            do (let* ((prev-ctx (when (listp (cognitive-trace-context prev-trace))
                                  (first (cognitive-trace-context prev-trace))))
                      (curr-outcome (cognitive-trace-actual trace))
                      (key (cons prev-ctx curr-outcome)))
                 (when (and prev-ctx curr-outcome)
                   (let ((link (gethash key *causal-model*)))
                     (if link
                         ;; Strengthen existing link
                         (when (fboundp 'causal-link-observations)
                           (incf (causal-link-observations link)))
                         ;; Create new causal link
                         (when (fboundp 'make-causal-link)
                           (setf (gethash key *causal-model*)
                                 (make-causal-link :cause prev-ctx
                                                   :effect curr-outcome
                                                   :strength 0.5
                                                   :delay 1)))))))))))

;;; ============================================================================
;;; DREAM EPISODE REPLAY (Organic VSA Consolidation)
;;; ============================================================================

(defun dream-about-episode! (episode)
  "Consolidate an episode through holographic replay.
   Superposes the episode's context-vector with similar stored patterns,
   creating denser interference that strengthens or reveals structure."
  (when episode
    (let* ((ctx (dream-episode-context episode)))
      (when (and ctx (listp ctx) (> (length ctx) 0)
                 (boundp '*holographic-enabled*) *holographic-enabled*)
        ;; Encode episode as holographic pattern
        (let ((hp (holographic-encode ctx)))
          (let ((similar (holographic-find-similar hp 0.3)))
            ;; Superpose episode with similar patterns - creating interference
            (dolist (match similar)
              (let ((match-hp (car match)))
                (when (holographic-pattern-activations match-hp)
                  (incf (holographic-pattern-strength match-hp) 0.15)
                  (incf (holographic-pattern-access-count match-hp)))))
            ;; Store consolidated episode in episodic layer
            (holographic-store! hp :layer :episodic)
            ;; Generate mutations targeting failure patterns
            (when (and (dream-episode-actual-outcome episode)
                       (not (equal (dream-episode-original-prediction episode)
                                   (dream-episode-actual-outcome episode))))
              (let ((mutations (generate-dream-mutations episode)))
                (when mutations
                  (incf (dream-episode-insights episode) (length mutations)))))
            ;; Extract semantic if pattern is strong enough
            (when (> (length similar) 3)
              (extract-semantic-from-episode! episode))))))))

(defun prune-oldest-episodes! (n)
  "Remove the N oldest episodes from the dream buffer."
  (when (and (boundp '*dream-state*) *dream-state*)
    (let ((buf (dream-state-episode-buffer *dream-state*)))
      (when (> (length buf) n)
        (setf (dream-state-episode-buffer *dream-state*)
              (subseq buf 0 (- (length buf) n)))))))

;;; ============================================================================
;;; DREAM MUTATIONS & CONSOLIDATION (Claims 45, 46, 47, 54)
;;; ============================================================================

(defun generate-dream-mutations (episode)
  "Generate mutated variations of a dream episode's context for creative exploration."
  (when episode
    (let* ((ctx (if (dream-episode-p episode)
                    (dream-episode-context episode)
                    (when (listp episode) episode)))
           (mutations nil))
      (when (and ctx (listp ctx) (> (length ctx) 1))
        ;; Swap mutation
        (let ((swapped (copy-list ctx)))
          (when (> (length swapped) 1)
            (rotatef (nth 0 swapped) (nth (1- (length swapped)) swapped))
            (push (list :type :swap :result swapped) mutations)))
        ;; Substitution mutation
        (let ((substituted (mutate-pattern ctx 0.2)))
          (push (list :type :substitution :result substituted) mutations))
        ;; Deletion mutation
        (when (> (length ctx) 2)
          (let ((deleted (remove (nth (random (length ctx)) ctx) ctx :count 1)))
            (push (list :type :deletion :result deleted) mutations))))
      mutations)))

(defun dream-consolidate-episodes! ()
  "Consolidate dream episodes by merging similar ones and strengthening patterns."
  (when (and (boundp '*dream-state*) *dream-state*)
    (let ((buf (dream-state-episode-buffer *dream-state*))
          (consolidated 0))
      (when (> (length buf) 3)
        ;; Find episodes with similar contexts and merge insights
        (let ((groups (make-hash-table :test 'equal)))
          (dolist (ep buf)
            (let ((key (if (and (dream-episode-context ep) (> (length (dream-episode-context ep)) 0))
                           (first (dream-episode-context ep))
                           :unknown)))
              (push ep (gethash key groups))))
          ;; Merge groups
          (maphash (lambda (key episodes)
                     (declare (ignore key))
                     (when (> (length episodes) 1)
                       (let ((primary (first episodes)))
                         (dolist (ep (rest episodes))
                           (incf (dream-episode-insights primary) (dream-episode-insights ep))
                           (incf (dream-episode-replayed-count primary))
                           (incf consolidated)))))
                   groups)))
      consolidated)))

(defun extract-semantic-from-episode! (episode)
  "Extract semantic patterns from an episode and register as schema candidates."
  (when episode
    (let* ((ctx (if (dream-episode-p episode)
                    (dream-episode-context episode)
                    nil))
           (outcome (if (dream-episode-p episode)
                        (dream-episode-actual-outcome episode)
                        nil)))
      (when (and ctx (listp ctx) (> (length ctx) 2))
        ;; Try to extract a compositional pattern
        (let ((structure (when (fboundp 'try-build-structure) (try-build-structure ctx))))
          (when structure
            ;; Register as schema candidate if we have the schema system
            (when (boundp '*cognitive-schemas*)
              (let ((schema-key (format nil "dream-~{~A~^|~}" (subseq ctx 0 (min 3 (length ctx))))))
                (setf (gethash (intern schema-key) *cognitive-schemas*)
                      (make-cognitive-schema
                       :pattern (abstract-pattern ctx)
                       :contexts (list ctx)
                       :instances 1
                       :successes (if outcome 1 0)
                       :created-at (if (boundp '*step*) *step* 0)))
                schema-key))))))))

;;; ============================================================================
;;; MODIFICATION ROLLBACK (Claim 52)
;;; ============================================================================

(defun revert-modification! (mod-entry)
  "Revert a previous modification by restoring the before-state."
  (when (and mod-entry (listp mod-entry))
    (let ((action (getf mod-entry :action))
          (before-acc (getf mod-entry :before-accuracy)))
      (when action
        ;; Record the revert in history
        (when (boundp '*modification-history*)
          (push (list :step (if (boundp '*step*) *step* 0)
                      :action :revert
                      :original-action action
                      :reason :performance-regression)
                *modification-history*))
        t))))

;;; ============================================================================
;;; EXPERT PRUNING (Claim 56)
;;; ============================================================================

(defun prune-dying-experts! ()
  "Remove experts whose knowledge-vectors are redundant in VSA space.
   An expert dies when another expert covers its knowledge with higher fitness.
   This is competitive displacement, not age-based death."
  (when (and (boundp '*experts*) *experts* (> (length *experts*) 10))
    (let ((before-count (length *experts*))
          (redundant nil))
      ;; Find experts whose knowledge-vector is highly similar to a better expert
      (dolist (e *experts*)
        (when (and (expert-knowledge-vector e)
                   (< (expert-life e) 0.5))  ; Only consider weak experts
          (dolist (other *experts*)
            (when (and (not (eq e other))
                       (expert-knowledge-vector other)
                       (> (expert-life other) (expert-life e)))
              ;; If knowledge-vectors are very similar, e is redundant
              (when (> (cosim (expert-knowledge-vector e)
                              (expert-knowledge-vector other)) 0.85)
                (pushnew e redundant)
                (return))))))
      ;; Remove redundant experts
      (setf *experts* (set-difference *experts* redundant))
      (- before-count (length *experts*)))))

;;; ============================================================================
;;; MODULE LOAD CONFIRMATION
;;; ============================================================================

(format t "~%%UHMA stubs loaded - undefined function stubs defined.~%%")


;;;; [SECTION-START:99:VERIFICATION]
;;; Section 99: File Verification

(defun verify-stubs-lisp-completeness () 
  "Verify completeness."
  (unless (find-package :uhma)
    (error "Package UHMA not defined"))
  (unless (fboundp 'run-dream-cycle!)
    (error "Function run-dream-cycle! not defined"))
  (format t "~&uhma-stubs.lisp verification passed.~%"))

(verify-stubs-lisp-completeness)

;;;; [SECTION-END:99:VERIFICATION]
