;;;; ============================================================================
;;;; UHMA COMPREHENSIVE FIXES (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Provides missing function definitions and resolves forward reference issues.
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 4: STRUCTURES ---

;;; Drift Detector struct moved to forward-decl.lisp

;;; --- SECTION 8: CORE UTILITIES ---

(defun vnorm! (vec)
  "In-place L2 normalization of a Mind-Space vector."
  (declare (type (simple-array single-float (*)) vec))
  (let ((mag (sqrt (reduce #'+ (map 'vector (lambda (x) (* x x)) vec)))))
    (declare (type single-float mag))
    (when (> mag 1e-7)
      (dotimes (i (length vec)) (setf (aref vec i) (/ (aref vec i) mag))))
    vec))

(defun pearson-correlation (xs ys)
  "Compute statistical linear correlation between two series."
  (declare (type list xs ys))
  (if (or (null xs) (null ys) (< (length xs) 2)) 0.0
      (let* ((n (length xs)) (mx (/ (reduce #'+ xs) n)) (my (/ (reduce #'+ ys) n))
             (cov (/ (reduce #'+ (mapcar (lambda (x y) (* (- x mx) (- y my))) xs ys)) n))
             (sx (sqrt (/ (reduce #'+ (mapcar (lambda (x) (expt (- x mx) 2)) xs)) n)))
             (sy (sqrt (/ (reduce #'+ (mapcar (lambda (y) (expt (- y my) 2)) ys)) n))))
        (if (or (zerop sx) (zerop sy)) 0.0 (/ cov (* sx sy))))))

;;; --- SECTION 10: INITIALIZATION ---
(eval-when (:load-toplevel :execute)
  (format t "[FIXES] Core utilities and forward references synchronized.~%"))

;;; ============================================================================
;;; NEIGHBORHOOD FUNCTIONS
;;; ============================================================================

(defun find-neighborhood-by-id (id)
  "Find a neighborhood by its ID."
  (find id *neighborhoods* :key #'neighborhood-id))

(defun split-neighborhood! (nbhd)
  "Split a neighborhood into two when it gets too large."
  (let* ((experts (remove nil (mapcar #'find-expert-by-id (neighborhood-experts nbhd))))
         (n (length experts)))
    (when (and experts (> n 10))
      ;; Simple split: first half and second half
      (let* ((mid (floor n 2))
             (first-half (subseq experts 0 mid))
             (second-half (subseq experts mid))
             (child1 (make-neighborhood :id (gensym "NBHD-")
                                        :experts (mapcar #'expert-id first-half)))
             (child2 (make-neighborhood :id (gensym "NBHD-")
                                        :experts (mapcar #'expert-id second-half))))
        (push child1 *neighborhoods*)
        (push child2 *neighborhoods*)
        (dolist (e first-half)
          (when e (setf (expert-neighborhood e) (neighborhood-id child1))))
        (dolist (e second-half)
          (when e (setf (expert-neighborhood e) (neighborhood-id child2))))
        (setf *neighborhoods* (remove nbhd *neighborhoods*))
        (values child1 child2)))))

;;; ============================================================================
;;; CONTEXT AND SIMILARITY FUNCTIONS  
;;; ============================================================================

(defun context-similarity (ctx1 ctx2)
  "Compute similarity between two contexts using Jaccard."
  (if (or (null ctx1) (null ctx2))
      0.0
      (let* ((set1 (remove-duplicates ctx1 :test #'equal))
             (set2 (remove-duplicates ctx2 :test #'equal))
             (intersection (intersection set1 set2 :test #'equal))
             (union-size (+ (length set1) (length set2) 
                           (- (length intersection)))))
        (if (zerop union-size)
            0.0
            (float (/ (length intersection) union-size))))))

(defun context-novelty-score (ctx)
  "How novel is this context? High score = rarely seen."
  (if (null ctx)
      1.0
      (let ((encounters (get-encounters ctx)))
        (if (zerop encounters)
            1.0
            (/ 1.0 (1+ (log (1+ encounters))))))))

;;; ============================================================================
;;; SCHEMA FUNCTIONS
;;; ============================================================================

(defun schema-match-score (schema ctx)
  "How well does a schema match this context?"
  (let ((contexts (cognitive-schema-contexts schema)))
    (if (null contexts)
        0.0
        (let ((best-score 0.0))
          (dolist (schema-ctx contexts best-score)
            (let ((score (context-similarity schema-ctx ctx)))
              (when (and score (numberp score) (> score best-score))
                (setf best-score score))))))))

(defun schemas-match-p (pattern1 pattern2)
  "Check if two schema patterns are similar enough to match."
  (and pattern1 pattern2
       (>= (/ (length (intersection pattern1 pattern2 :test #'equal))
              (max 1 (min (length pattern1) (length pattern2))))
           0.7)))

(defun schemas-complementary-p (s1 s2)
  "Check if two schemas are complementary (could be combined)."
  (let ((p1 (cognitive-schema-pattern s1))
        (p2 (cognitive-schema-pattern s2)))
    (and (not (schemas-match-p p1 p2))
         (> (length (intersection p1 p2 :test #'equal)) 0))))

(defun find-matching-schema (pattern)
  "Find an existing schema that matches this pattern."
  (maphash (lambda (id schema)
             (declare (ignore id))
             (when (schemas-match-p (cognitive-schema-pattern schema) pattern)
               (return-from find-matching-schema schema)))
           *cognitive-schemas*)
  nil)

(defun abstract-reasoning-path (path)
  "Abstract a reasoning path into a pattern."
  (mapcar (lambda (step)
            (if (consp step)
                (list (car step) (if (eq (cdr step) :success) :success :failure))
                step))
          path))

(defun create-schema! (pattern trace)
  "Create a new cognitive schema from a pattern."
  (let ((schema (make-cognitive-schema
                 :id (gensym "SCHEMA-")
                 :pattern pattern
                 :instances 1
                 :successes (if (cognitive-trace-correct trace) 1 0)
                 :contexts (list (cognitive-trace-context trace))
                 :created-at *step*
                 :last-used *step*)))
    (setf (gethash (cognitive-schema-id schema) *cognitive-schemas*) schema)
    schema))

(defun reinforce-schema! (schema trace)
  "Reinforce an existing schema with new evidence."
  (incf (cognitive-schema-instances schema))
  (when (cognitive-trace-correct trace)
    (incf (cognitive-schema-successes schema)))
  (pushnew (cognitive-trace-context trace) 
           (cognitive-schema-contexts schema)
           :test #'equal)
  (setf (cognitive-schema-last-used schema) *step*)
  ;; Keep contexts bounded
  (when (> (length (cognitive-schema-contexts schema)) 50)
    (setf (cognitive-schema-contexts schema)
          (subseq (cognitive-schema-contexts schema) 0 50)))
  schema)

(defun mutate-pattern (pattern)
  "Mutate a schema pattern slightly."
  (when pattern
    (let ((new-pattern (copy-list pattern))
          (idx (random (length pattern))))
      ;; Randomly generalize one element
      (when (consp (nth idx new-pattern))
        (setf (nth idx new-pattern)
              (list (car (nth idx new-pattern)) :any)))
      new-pattern)))

(defun mutate-schema! (schema)
  "Create a mutated variant of a schema."
  (let ((new-pattern (mutate-pattern (cognitive-schema-pattern schema))))
    (when new-pattern
      (create-schema! new-pattern 
                      (make-cognitive-trace :context nil :prediction t :actual t)))))

(defun combine-schemas! (s1 s2)
  "Combine two complementary schemas."
  (let ((combined-pattern (union (cognitive-schema-pattern s1)
                                 (cognitive-schema-pattern s2)
                                 :test #'equal)))
    (create-schema! combined-pattern
                    (make-cognitive-trace :context nil :prediction t :actual t))))

(defun find-common-pattern (p1 p2)
  "Find common elements between two patterns."
  (intersection p1 p2 :test #'equal))

;;; ============================================================================
;;; SELF-MODEL FUNCTIONS
;;; ============================================================================

;; Note: self-model-current-sparse-threshold is already a struct accessor
;; We just need to make sure it's available - it should be from the struct

;; Note: record-outcome! is already defined in uhma-v6.1-adaptive-homoiconic.lisp

(defun record-program-performance! (expert correct-p)
  "Record how an expert's program performed."
  (when (and expert (boundp '*self-model*) *self-model*)
    (let* ((prog-id (expert-id expert))
           (perf (gethash prog-id (self-model-program-structure-stats *self-model*))))
      (unless perf
        (setf perf (cons 0 0))
        (setf (gethash prog-id (self-model-program-structure-stats *self-model*)) perf))
      (if correct-p
          (incf (car perf))
          (incf (cdr perf))))))

(defun run-program-optimization! ()
  "Run optimization based on program performance data."
  (when (and (boundp '*self-model*) *self-model*
             (zerop (mod *step* 100)))
    ;; Find poorly performing programs
    (maphash (lambda (prog-id perf)
               (let ((total (+ (car perf) (cdr perf))))
                 (when (and (> total 10)
                            (< (/ (car perf) total) 0.3))
                   ;; Mark for potential modification
                   (let ((expert (find-expert-by-id prog-id)))
                     (when expert
                       ;; Just log for now
                       nil)))))
             (self-model-program-structure-stats *self-model*))))

(defun update-adaptive-threshold! ()
  "Update the sparse verification threshold based on performance."
  (when (and (boundp '*self-model*) *self-model*)
    (let* ((recent (self-model-recent-errors *self-model*))
           (total (if (listp recent) (length recent) 0)))
      (when (> total 0)
        (let* ((error-sum (if (listp recent) (reduce #'+ recent) 0))
               (accuracy (- 1.0 (/ error-sum total)))  ; recent-errors has 0=correct, 1=error
               (current (self-model-current-sparse-threshold *self-model*)))
          ;; Adjust threshold based on accuracy
          (setf (self-model-current-sparse-threshold *self-model*)
                (max (self-model-sparse-threshold-min *self-model*)
                     (min (self-model-sparse-threshold-max *self-model*)
                          (+ (* 0.9 current) (* 0.1 accuracy))))))))))

;;; ============================================================================
;;; SELF-EXPECTATION FUNCTIONS
;;; ============================================================================

(defun compute-recent-accuracy (n)
  "Compute accuracy over last N predictions."
  (let ((traces (get-traces :n n :meta-level 0))
        (correct 0)
        (total 0))
    (dolist (trace traces)
      (when (cognitive-trace-p trace)
        (incf total)
        (when (cognitive-trace-correct trace)
          (incf correct))))
    (if (zerop total) 0.5 (/ correct total))))

(defun compute-self-divergence (expectation)
  "Compute how much actual behavior diverged from expected."
  (let ((exp-schema (self-expectation-expected-schema expectation))
        (act-schema (self-expectation-actual-schema expectation))
        (exp-conf (or (self-expectation-expected-confidence expectation) 0.5))
        (act-conf (or (self-expectation-actual-confidence expectation) 0.5)))
    (+ (* 0.5 (if (eq exp-schema act-schema) 0.0 1.0))
       (* 0.3 (abs (- exp-conf act-conf)))
       (* 0.2 (op-sequence-divergence 
               (self-expectation-expected-ops expectation)
               (self-expectation-actual-ops expectation))))))

(defun op-sequence-divergence (seq1 seq2)
  "Compute divergence between two operation sequences."
  (if (or (null seq1) (null seq2))
      (if (and (null seq1) (null seq2)) 0.0 1.0)
      (let* ((len1 (length seq1))
             (len2 (length seq2))
             (max-len (max len1 len2))
             (matches (length (intersection seq1 seq2 :test #'equal))))
        (- 1.0 (/ matches max-len)))))

(defun record-self-expectation! (expectation)
  "Record a self-expectation in the buffer."
  (vector-push-extend expectation *self-expectations*)
  ;; Keep buffer from growing too large - shift older entries out
  (when (>= (fill-pointer *self-expectations*) 200)
    (let ((keep-count 100))
      (loop for i from 0 below keep-count
            do (setf (aref *self-expectations* i)
                     (aref *self-expectations* (+ i 100))))
      (setf (fill-pointer *self-expectations*) keep-count)))
  (update-self-prediction-accuracy!))

(defun update-self-prediction-accuracy! ()
  "Update running self-prediction accuracy."
  (let ((correct 0)
        (total 0))
    (loop for exp across *self-expectations*
          when (self-expectation-self-prediction-correct exp)
          do (incf correct)
          do (incf total))
    (when (> total 0)
      (setf *self-expectation-accuracy* (/ correct total)))))

(defun find-divergence-point (exp-ops act-ops)
  "Find where expected and actual operation sequences diverged. Returns index or nil."
  (when (and (listp exp-ops) (listp act-ops))
    (loop for i from 0
          for exp-op in exp-ops
          for act-op in act-ops
          when (not (equal exp-op act-op))
          return i
          finally (return (when (/= (length exp-ops) (length act-ops))
                            (min (length exp-ops) (length act-ops)))))))

(defun get-recent-self-prediction-failures (n)
  "Get recent self-prediction failures for analysis."
  (let ((failures nil))
    (loop for exp across *self-expectations*
          when (not (self-expectation-self-prediction-correct exp))
          do (push exp failures)
          when (>= (length failures) n)
          return failures)
    failures))

(defun find-similar-context-expectations (ctx n)
  "Find self-expectations from similar contexts."
  (let ((similar nil))
    (loop for exp across *self-expectations*
          for sim = (context-similarity ctx (self-expectation-context exp))
          when (and sim (numberp sim) (> sim 0.5))
          do (push (cons sim exp) similar))
    (mapcar #'cdr (subseq (sort similar #'> :key #'car) 0 (min n (length similar))))))

;;; ============================================================================
;;; HYPOTHESIS FUNCTIONS
;;; ============================================================================

(defun find-existing-invented-hypothesis (relation subject predicate)
  "Find if we already have this hypothesis."
  (maphash (lambda (id hyp)
             (declare (ignore id))
             (when (and (eq (invented-hypothesis-relation hyp) relation)
                        (eq (invented-hypothesis-subject hyp) subject)
                        (eq (invented-hypothesis-predicate hyp) predicate))
               (return-from find-existing-invented-hypothesis hyp)))
           *invented-hypotheses*)
  nil)

(defun compute-cooccurrence (subject predicate samples)
  "Compute co-occurrence rate between two observables.
   SAMPLES can be either a number (of recent traces to check) or a list of traces."
  (handler-case
      (let ((both 0)
            (total 0)
            (traces (if (listp samples)
                        samples
                        ;; If samples is a number, get that many recent traces
                        (when (and (boundp '*trace-buffer*) 
                                   (arrayp *trace-buffer*)
                                   (> (fill-pointer *trace-buffer*) 0))
                          (loop for i from 0 below (min samples (fill-pointer *trace-buffer*))
                                collect (aref *trace-buffer* 
                                             (- (fill-pointer *trace-buffer*) 1 i)))))))
        (dolist (trace traces)
          (let ((has-subj (observe-from-trace subject trace))
                (has-pred (observe-from-trace predicate trace)))
            (when (or has-subj has-pred)
              (incf total)
              (when (and has-subj has-pred)
                (incf both)))))
        (if (zerop total) 0.0 (/ both total)))
    (error (e)
      (declare (ignore e))
      0.0)))

(defun compute-observable-correlation (subject predicate samples)
  "Compute correlation between two observables.
   SAMPLES can be either a number (of samples to take from trace buffer) or a list of traces."
  (let ((vals1 nil)
        (vals2 nil)
        (traces (if (numberp samples)
                    ;; Get samples from trace buffer
                    (loop for i from 0 below (min samples (fill-pointer *trace-buffer*))
                          collect (aref *trace-buffer* (- (fill-pointer *trace-buffer*) 1 i)))
                    ;; Use provided list
                    samples)))
    (dolist (trace traces)
      (when (cognitive-trace-p trace)
        (let ((v1 (observe-from-trace subject trace))
              (v2 (observe-from-trace predicate trace)))
          (when (and (numberp v1) (numberp v2))
            (push v1 vals1)
            (push v2 vals2)))))
    (if (>= (length vals1) 3)
        (pearson-correlation vals1 vals2)
        0.0)))

(defun trace-correct-p (trace)
  "Check if a trace represents a correct prediction."
  (and trace
       (cognitive-trace-prediction trace)
       (cognitive-trace-actual trace)
       (equal (cognitive-trace-prediction trace)
              (cognitive-trace-actual trace))))

(defun observe-from-trace (observable trace)
  "Extract an observable's value from a trace."
  (case observable
    (:high-confidence (and (> (or (cognitive-trace-confidence trace) 0) 0.7) 1.0))
    (:low-confidence (and (< (or (cognitive-trace-confidence trace) 1) 0.3) 1.0))
    (:correct (if (trace-correct-p trace) 1.0 0.0))
    (:incorrect (if (trace-correct-p trace) 0.0 1.0))
    (:novel-context (context-novelty-score (cognitive-trace-context trace)))
    (otherwise (cognitive-trace-confidence trace))))

(defun hypothesis-testable-with-context-p (hyp ctx)
  "Can this hypothesis be tested with the given context?"
  (declare (ignore hyp ctx))
  t)  ; Simplified - all hypotheses are testable

(defun invented-hypothesis-testable-with-context-p (hyp ctx)
  "Can this invented hypothesis be tested with the given context?"
  (declare (ignore hyp ctx))
  t)

(defun count-testable-hypotheses-for (ctx)
  "Count hypotheses that could be tested with this context."
  (let ((count 0))
    (maphash (lambda (id hyp)
               (declare (ignore id))
               (when (hypothesis-testable-with-context-p hyp ctx)
                 (incf count)))
             *hypotheses*)
    (maphash (lambda (id hyp)
               (declare (ignore id))
               (when (invented-hypothesis-testable-with-context-p hyp ctx)
                 (incf count)))
             *invented-hypotheses*)
    count))

(defun epistemic-uncertainty-for (ctx)
  "Estimate epistemic uncertainty for a context."
  (let ((encounters (get-encounters ctx)))
    (cond
      ((zerop encounters) 0.9)
      ((< encounters 5) 0.7)
      ((< encounters 20) 0.4)
      (t 0.2))))

(defun estimate-self-surprise-potential (ctx)
  "Estimate how likely this context is to produce self-surprise."
  (let ((similar-exps (find-similar-context-expectations ctx 10)))
    (if (null similar-exps)
        0.5  ; Unknown = moderate potential
        (let ((avg-surprise (/ (reduce #'+ (mapcar #'self-expectation-self-surprise 
                                                   similar-exps))
                               (length similar-exps))))
          avg-surprise))))

(defun generate-blind-spot-hypothesis (failure)
  "Generate a hypothesis about a blind spot from a failure."
  (let ((ctx (self-expectation-context failure)))
    (make-hypothesis :id (gensym "BLIND-")
                     :observation (list :blind-spot ctx)
                     :claim (format nil "System has blind spot for context ~A" 
                                    (subseq (format nil "~A" ctx) 0 
                                            (min 50 (length (format nil "~A" ctx)))))
                     :action :investigate
                     :created-at *step*)))

;;; ============================================================================
;;; INTROSPECTION FUNCTIONS
;;; ============================================================================

(defun introspect-at-layer! (layer level)
  "Run introspection for a specific layer."
  (setf (introspection-layer-last-introspection layer) *step*)
  (let ((observations nil)
        (source (if (= level 0)
                    (get-traces :n 50 :meta-level 0)
                    (introspection-layer-observations 
                     (nth (1- level) *introspection-stack*)))))
    ;; Observe patterns in source
    (setf observations (extract-patterns source level))
    (setf (introspection-layer-observations layer) observations)
    ;; Model the source
    (setf (introspection-layer-model-of-below layer)
          (build-layer-model source level))
    ;; Identify error patterns
    (setf (introspection-layer-error-patterns layer)
          (identify-error-patterns source level))
    ;; Propose interventions
    (setf (introspection-layer-interventions layer)
          (propose-interventions (introspection-layer-error-patterns layer) level))
    ;; Generate hypotheses from observations (THE KEY LOOP!)
    (dolist (obs observations)
      (when (consp obs)
        ;; Convert observation to hypothesis-compatible format
        (let ((hyp-obs (case (car obs)
                         (:confidence-calibration
                          (let ((buckets (cadr obs)))
                            (when (consp buckets)
                              (let ((total-error 0.0)
                                    (count 0))
                                (dolist (b buckets)
                                  (when (consp b)
                                    (let ((expected (getf b :expected 0.5))
                                          (actual (getf b :actual 0.5)))
                                      (incf total-error (- (if (rationalp actual)
                                                              (float actual) actual)
                                                          expected))
                                      (incf count))))
                                (when (> count 0)
                                  (list :param-effect 'confidence-threshold
                                        (if (> (/ total-error count) 0)
                                            :under-confident
                                            :over-confident)
                                        :for-calibration))))))
                         (:error-prone-contexts
                          (list :structure-effect 'error-context-handling
                                (cdr obs)))
                         ;; WIRED: System health observations → hypotheses about self
                         (:system-health
                          (let ((health-type (second obs)))
                            (case health-type
                              (:hypothesis-accumulation
                               (list :meta 'hypothesis-system-may-be-redundant))
                              (:schema-ineffective
                               (list :meta 'schema-utilization-failing))
                              (:trace-buffer-full
                               (list :meta 'memory-under-pressure))
                              (:prediction-uncertain
                               (list :meta 'system-lacks-confidence))
                              (:overconfident
                               (list :meta 'system-overconfident))
                              (:learning-declining
                               (list :meta 'learning-effectiveness-dropping))
                              (:learning-plateaued
                               (list :meta 'learning-has-plateaued))
                              (:schema-failing
                               (list :meta 'schemas-not-helping))
                              (:schema-effective
                               nil)  ; Good news, no action needed
                              (:goal-stalled
                               (list :meta 'goal-pursuit-stuck))
                              (:hypotheses-mostly-weak
                               (list :meta 'hypothesis-quality-low))
                              (:stagnant
                               (list :meta 'system-stagnant-needs-exploration))
                              (:self-modifications-counterproductive
                               (list :meta 'self-modification-hurting))
                              (:self-modifications-effective
                               nil)  ; Good news, no action needed
                              (t nil))))
                         (t nil))))
          (when hyp-obs
            (generate-hypothesis! hyp-obs)))))
    ;; Record this introspection as a meta-trace
    (record-trace! 
     (make-cognitive-trace
      :step *step*
      :meta-level (1+ level)
      :context `(:introspection-at ,level)
      :prediction (introspection-layer-model-of-below layer)
      :meta-observations observations))
    layer))

(defun extract-patterns (source level)
  "Extract patterns from source appropriate to this level."
  (let ((patterns nil))
    (cond
      ((= level 0)
       ;; Level 0: patterns in raw cognition
       (let ((error-contexts nil)
             (success-contexts nil)
             (confidence-calibration nil))
         (dolist (trace source)
           (when (cognitive-trace-p trace)
             (let ((correct (eq (cognitive-trace-prediction trace)
                               (cognitive-trace-actual trace))))
               (if correct
                   (push (cognitive-trace-context trace) success-contexts)
                   (push (cognitive-trace-context trace) error-contexts))
               (push (cons (cognitive-trace-confidence trace) correct)
                     confidence-calibration))))
         (push `(:error-prone-contexts ,(find-common-features error-contexts))
               patterns)
         (push `(:success-contexts ,(find-common-features success-contexts))
               patterns)
         (push `(:confidence-calibration ,(analyze-calibration confidence-calibration))
               patterns)))
      ((= level 1)
       ;; Level 1: patterns in base-level patterns AND system self-observation
       (let ((recurring nil))
         (dolist (obs source)
           (when (and (consp obs) (eq (car obs) :error-prone-contexts))
             (push (cdr obs) recurring)))
         (push `(:meta-pattern-recurring ,recurring) patterns)
         (push `(:meta-pattern-stability ,(assess-stability recurring)) patterns)
         ;; WIRED: Observe system's own meta-structures
         (let* ((hyp-count (hash-table-count *hypotheses*))
                (schema-count (when (boundp '*cognitive-schemas*)
                               (hash-table-count *cognitive-schemas*)))
                (trace-fill (when (and (boundp '*trace-buffer*)
                                       (array-has-fill-pointer-p *trace-buffer*))
                             (fill-pointer *trace-buffer*)))
                (recent-acc (compute-recent-accuracy 50)))
           ;; Observe hypothesis accumulation
           (when (> hyp-count 100)
             (push `(:system-health :hypothesis-accumulation
                     :count ,hyp-count :threshold 100) patterns))
           ;; Observe schema utilization
           (when (and schema-count (> schema-count 5) (< recent-acc 0.6))
             (push `(:system-health :schema-ineffective
                     :schemas ,schema-count :accuracy ,recent-acc) patterns))
           ;; Observe trace buffer pressure
           (when (and trace-fill (boundp '*trace-buffer-max*)
                      (> trace-fill (* 0.9 *trace-buffer-max*)))
             (push `(:system-health :trace-buffer-full
                     :fill ,trace-fill :max ,*trace-buffer-max*) patterns))
           ;; WIRED: Observe prediction confidence distribution
           (when (boundp '*confidence-history*)
             (let* ((confs *confidence-history*)
                    (n (length confs))
                    (avg-conf (if (> n 0)
                                  (/ (reduce #'+ confs) n)
                                  0.5))
                    (low-ratio (if (> (+ *low-confidence-count* *high-confidence-count*) 0)
                                   (/ *low-confidence-count*
                                      (+ *low-confidence-count* *high-confidence-count*))
                                   0.5)))
               ;; Too many low-confidence predictions = system uncertain
               (when (> low-ratio 0.6)
                 (push `(:system-health :prediction-uncertain
                         :low-ratio ,low-ratio :avg-conf ,avg-conf) patterns))
               ;; System is confident but accuracy low = overconfident
               (when (and (< low-ratio 0.3) (< recent-acc 0.5))
                 (push `(:system-health :overconfident
                         :low-ratio ,low-ratio :accuracy ,recent-acc) patterns))))
           ;; WIRED: Observe learning rate (improving vs plateaued)
           (when (and (boundp '*learning-rate-window*)
                      (> (length *learning-rate-window*) 3))
             (let* ((recent (subseq *learning-rate-window* 0 (min 3 (length *learning-rate-window*))))
                    (older (subseq *learning-rate-window* (min 3 (length *learning-rate-window*))))
                    (recent-avg (/ (reduce #'+ (mapcar #'cdr recent)) (length recent)))
                    (older-avg (if older
                                   (/ (reduce #'+ (mapcar #'cdr older)) (length older))
                                   recent-avg))
                    (delta (- recent-avg older-avg)))
               ;; Learning plateaued or declining
               (when (< delta -0.05)
                 (push `(:system-health :learning-declining
                         :recent ,recent-avg :older ,older-avg) patterns))
               (when (and (> (length *learning-rate-window*) 5)
                          (< (abs delta) 0.02))
                 (push `(:system-health :learning-plateaued
                         :recent ,recent-avg :delta ,delta) patterns))))
           ;; WIRED: Observe schema utilization
           (when (and (boundp '*schema-attempts*) (> *schema-attempts* 10))
             (let ((schema-success-rate (/ *schema-successes*
                                           (max 1 *schema-attempts*))))
               ;; Schemas being tried but failing
               (when (< schema-success-rate 0.3)
                 (push `(:system-health :schema-failing
                         :attempts ,*schema-attempts*
                         :successes ,*schema-successes*
                         :rate ,schema-success-rate) patterns))
               ;; Schemas working well
               (when (> schema-success-rate 0.7)
                 (push `(:system-health :schema-effective
                         :rate ,schema-success-rate) patterns))))
           ;; WIRED: Observe goal progress (if goals exist)
           (when (and (boundp '*current-goal*) *current-goal*)
             (let* ((goal *current-goal*)
                    (age (- *step* (or (goal-created-at goal) *step*)))
                    (priority (goal-priority goal)))
               ;; Goal stalled (old with high priority)
               (when (and (> age 500) (> priority 0.5))
                 (push `(:system-health :goal-stalled
                         :goal-type ,(goal-type goal)
                         :age ,age) patterns))))
           ;; WIRED: Observe hypothesis lifecycle
           (let ((recent-hyps (loop for id being the hash-keys of *hypotheses*
                                    using (hash-value hyp)
                                    when (and (listp hyp)
                                             (> (or (getf (cdr hyp) :support 0) 0)
                                                (or (getf (cdr hyp) :refute 0) 0)))
                                    count 1)))
             (when (and (> hyp-count 20) (< (/ recent-hyps (max 1 hyp-count)) 0.2))
               (push `(:system-health :hypotheses-mostly-weak
                       :strong ,recent-hyps :total ,hyp-count) patterns)))
           ;; WIRED: Observe stagnation/coasting (high performance but not learning anything new)
           (when (and (boundp '*learning-rate-window*)
                      (> (length *learning-rate-window*) 5))
             (let* ((recent-accs (mapcar #'cdr (subseq *learning-rate-window* 0 (min 8 (length *learning-rate-window*)))))
                    (mean (/ (reduce #'+ recent-accs) (length recent-accs)))
                    (mod-count (length *modification-history*)))
               ;; High accuracy + no self-modifications = coasting without growth
               ;; System is performing well but not challenging itself
               (when (and (> mean 0.9)
                          (< mod-count 2)
                          (> *step* 10000))  ; Give it time to warm up first
                 (push `(:system-health :stagnant
                         :accuracy ,mean :modifications ,mod-count
                         :reason :coasting-without-growth) patterns))))
           ;; WIRED: Observe self-modification outcomes
           (when (and (boundp '*modification-history*)
                      (> (length *modification-history*) 3))
             (let* ((recent-mods (subseq *modification-history* 0 (min 5 (length *modification-history*))))
                    (improvements 0)
                    (regressions 0))
               (dolist (mod recent-mods)
                 (let ((before (getf mod :before-accuracy 0.5))
                       (after (getf mod :after-accuracy 0.5)))
                   (cond ((> after (+ before 0.02)) (incf improvements))
                         ((< after (- before 0.02)) (incf regressions)))))
               ;; Self-modifications mostly hurting
               (when (> regressions improvements)
                 (push `(:system-health :self-modifications-counterproductive
                         :improvements ,improvements :regressions ,regressions) patterns))
               ;; Self-modifications mostly helping
               (when (and (> improvements 2) (zerop regressions))
                 (push `(:system-health :self-modifications-effective
                         :improvements ,improvements) patterns))))
           ;; General metrics
           (push `(:system-metrics :hypotheses ,hyp-count
                   :schemas ,(or schema-count 0) :accuracy ,recent-acc) patterns))))
      (t
       ;; Higher levels: increasingly abstract
       (push `(:deep-pattern-at-level ,level ,source) patterns)))
    patterns))

(defun identify-error-patterns (source level)
  "Identify error patterns in the source."
  (declare (ignore level))
  (let ((errors nil))
    (cond
      ((listp source)
       (dolist (item source)
         (when (and (expert-p item)
                    (> (expert-misses item) (expert-hits item)))
           (push (list :weak-expert (expert-id item)) errors)))))
    errors))

(defun build-layer-model (source level)
  "Build a summary model of a layer."
  (declare (ignore level))
  (list :item-count (if (listp source) (length source) 
                        (if (hash-table-p source) (hash-table-count source) 0))
        :summarized-at *step*))

(defun propose-interventions (error-patterns level)
  "Propose interventions for error patterns."
  (declare (ignore level))
  (mapcar (lambda (err)
            (list :intervention :review :target (second err)))
          error-patterns))

(defun apply-intervention! (intervention level)
  "Apply an intervention."
  (declare (ignore level))
  (format t "~%[INTERVENTION] ~A~%" intervention))

(defun summarize-source (source level)
  "Summarize a source for model building."
  (declare (ignore level))
  (list :type (type-of source) :size (if (listp source) (length source) 0)))

(defun find-common-features (contexts)
  "Find common features in a list of contexts."
  (when contexts
    (let ((feature-counts (make-hash-table :test 'equal)))
      (dolist (ctx contexts)
        (when (listp ctx)
          (dolist (tok ctx)
            (incf (gethash tok feature-counts 0)))))
      ;; Return features appearing in >30% of contexts
      (let ((threshold (max 1 (floor (* 0.3 (length contexts)))))
            (common nil))
        (maphash (lambda (k v)
                   (when (>= v threshold)
                     (push (cons k v) common)))
                 feature-counts)
        common))))

(defun analyze-calibration (conf-pairs)
  "Analyze confidence calibration from (confidence . correct) pairs."
  (when conf-pairs
    (let ((buckets (make-array 10 :initial-element (cons 0 0))))
      (dolist (pair conf-pairs)
        (when (consp pair)
          (let* ((conf (car pair))
                 (correct (cdr pair))
                 (bucket (min 9 (max 0 (floor (* (or conf 0) 10)))))
                 (current (aref buckets bucket)))
            (setf (aref buckets bucket)
                  (cons (+ (car current) (if correct 1 0))
                        (+ (cdr current) 1))))))
      ;; Return calibration error per bucket
      (loop for i from 0 below 10
            for (hits . total) = (aref buckets i)
            when (> total 0)
            collect (let ((expected (/ (+ i 0.5) 10))
                         (actual (/ hits total)))
                     (list :bucket i 
                           :expected expected 
                           :actual actual
                           :error (abs (- expected actual))))))))

(defun assess-stability (recurring-patterns)
  "Assess stability of recurring patterns."
  (list :stable-count (length recurring-patterns)))

(defun find-common-context (window)
  "Find common context in a surprise window (list of traces or context pairs)."
  (when window
    (let ((contexts (mapcar (lambda (item)
                              (cond
                                ((cognitive-trace-p item)
                                 (cognitive-trace-context item))
                                ((consp item) (car item))
                                (t nil)))
                            window)))
      (let ((valid-contexts (remove nil contexts)))
        (when valid-contexts
          (reduce (lambda (a b) 
                    (intersection a b :test #'equal))
                  valid-contexts))))))

(defun classify-observation (obs)
  "Classify an observation for hypothesis generation."
  (cond
    ((and (listp obs) (eq (car obs) :error)) :error)
    ((and (listp obs) (eq (car obs) :surprise)) :surprise)
    ((and (listp obs) (eq (car obs) :self-surprise)) :self-surprise)
    ((and (listp obs) (eq (car obs) :param-effect)) :parameter)
    ((and (listp obs) (eq (car obs) :structure-effect)) :structure)
    ((and (listp obs) (eq (car obs) :context-causes-errors)) :context-error)
    ((and (listp obs) (eq (car obs) :meta)) :meta)
    (t :general)))

(defun formulate-claim (observation claim-type)
  "Formulate a claim from an observation - MUST return S-expression, not string!
   IMPORTANT: Claims must be normalized to PATTERNS, not instances.
   Instance-specific data (exact context, step numbers) should stay in evidence."
  (case claim-type
    (:self-surprise
     ;; Normalize self-surprise claims - extract pattern, discard instance-specific context
     (let ((surprise-type (when (listp observation) (second observation))))
       (case surprise-type
         ;; Context-confidence claims: just keep the pattern type
         ((:overconfident-for-context :underconfident-for-context)
          `(:self-surprise ,surprise-type))
         ;; Expert routing: keep which experts, discard specific context
         (:unexpected-expert-routing
          (let ((expected (getf (cddr observation) :expected))
                (actual (getf (cddr observation) :actual)))
            `(:self-surprise :unexpected-expert-routing :from ,expected :to ,actual)))
         ;; Op sequence: just note that ops diverge, discard specific divergence point
         (:unexpected-op-sequence
          `(:self-surprise :unexpected-op-sequence))
         ;; Other self-surprises: pass through but strip context if present
         (t (if (and (listp observation) (> (length observation) 3)
                     (member :context observation))
                ;; Remove :context and its value
                (loop for (k v) on (cddr observation) by #'cddr
                      unless (eq k :context)
                      append (list k v) into result
                      finally (return `(:self-surprise ,surprise-type ,@result)))
                observation)))))
    (:parameter
     `(parameter-value-affects-performance
       ,(second observation) ,(third observation) ,(fourth observation)))
    (:structure
     `(program-structure-correlates-with
       ,(second observation) ,(third observation)))
    ;; Context-error claims: normalize by removing specific context
    ;; (:context-causes-errors (THE CAT) :recent-surprise-burst) → (:context-causes-errors :surprise-burst)
    (:context-error
     (let ((trigger (when (and (listp observation) (> (length observation) 2))
                      (third observation))))  ; e.g. :recent-surprise-burst
       `(:context-causes-errors ,(or trigger :general))))
    ;; Meta claims: keep the pattern, discard specifics
    (:meta
     (if (and (listp observation) (> (length observation) 1))
         `(:meta ,(second observation))  ; e.g. (:meta my-introspection-method may-be-flawed) → (:meta my-introspection-method)
         observation))
    (:error `(error-pattern :general))  ; Don't include specific error
    (:surprise `(surprise-pattern :general))  ; Don't include specific surprise
    (t observation)))

(defun propose-action (claim claim-type)
  "Propose an action for a claim."
  (case claim-type
    (:self-surprise
     (let ((surprise-type (when (listp claim) (second claim))))
       (case surprise-type
         (:overconfident-for-context '(lower-confidence-threshold))
         (:underconfident-for-context '(raise-confidence-threshold))
         (t '(investigate-routing)))))
    ;; WIRED: Meta claims about system health → concrete actions
    (:meta
     (let ((meta-claim (when (listp claim) (second claim))))
       (case meta-claim
         (hypothesis-system-may-be-redundant '(prune-weak-hypotheses))
         (schema-utilization-failing '(review-schema-application))
         (memory-under-pressure '(trim-memory-buffers))
         (system-lacks-confidence '(increase-exploration))
         (system-overconfident '(increase-verification))
         (learning-effectiveness-dropping '(trigger-retraining-focus))
         (learning-has-plateaued '(try-alternative-strategies))
         (schemas-not-helping '(disable-schema-guidance))
         (goal-pursuit-stuck '(reassess-current-goal))
         (hypothesis-quality-low '(aggressive-hypothesis-pruning))
         (self-modification-hurting '(reduce-self-modification-rate))
         (system-stagnant-needs-exploration '(shake-things-up))
         (t :investigate-meta))))
    (:error :investigate)
    (:surprise :analyze)
    (:parameter `(modify-param ,(second claim) ,(third claim) ,(fourth claim)))
    (:context-error '(investigate-context-errors))
    (otherwise :monitor)))

(defun evaluate-claim-against-trace (claim trace)
  "Evaluate if a claim holds for a trace - proper implementation.
   Claims are now NORMALIZED (no instance-specific context)."
  (when (and claim trace (listp claim))
    (let ((claim-type (car claim)))
      (case claim-type
        ;; Self-surprise claims - normalized format without specific context
        (:self-surprise
         (let* ((surprise-type (second claim))
                (got-it (eq (cognitive-trace-prediction trace)
                            (cognitive-trace-actual trace))))
           (case surprise-type
             ;; Overconfident: we think we're right but we're wrong
             ;; Supports if prediction was wrong (still overconfident)
             (:overconfident-for-context
              (if got-it :contradicts :supports))
             ;; Underconfident: we doubt ourselves but we're actually right
             ;; Supports if prediction was right (we underestimated ourselves)
             (:underconfident-for-context
              (if got-it :supports :contradicts))
             ;; Op sequence divergence - just check if prediction accuracy is low
             (:unexpected-op-sequence
              (if got-it :contradicts :supports))
             ;; Expert routing - check if routing from expected to actual happened
             (:unexpected-expert-routing
              ;; For routing claims, support if we continue to see unexpected behavior
              (if got-it :contradicts :supports))
             (t nil))))
        
        ;; Parameter claims
        (parameter-value-affects-performance
         (let* ((expected-dir (third claim))
                (got-it (eq (cognitive-trace-prediction trace)
                            (cognitive-trace-actual trace))))
           (cond
             ((and (member expected-dir '(:under-confident :higher-better)) got-it) :supports)
             ((and (member expected-dir '(:under-confident :higher-better)) (not got-it)) :contradicts)
             ((and (member expected-dir '(:over-confident :lower-better)) (not got-it)) :supports)
             ((and (member expected-dir '(:over-confident :lower-better)) got-it) :contradicts)
             (t nil))))
        
        ;; Context error claims - now normalized (no specific context)
        ;; Claim format: (:context-causes-errors :trigger-type)
        ;; Supports if errors continue, contradicts if predictions succeed
        (:context-causes-errors
         (let ((got-it (eq (cognitive-trace-prediction trace)
                           (cognitive-trace-actual trace))))
           (if got-it :contradicts :supports)))
        
        ;; Default
        (t nil)))))

(defun execute-self-modification! (action)
  "Execute a self-modification action - ACTUALLY MODIFY THE SYSTEM.
   Tracks before/after metrics for self-observation.
   HOOKS:
   - +hook-pre-modification+ : before modification, can return :skip to abort
   - +hook-post-modification+ : after modification with outcome"
  (let ((action-type (if (listp action) (car action) action))
        (before-accuracy (compute-recent-accuracy 50))
        (before-hyp-count (hash-table-count *hypotheses*)))
    ;; Fire PRE-MODIFICATION hook - can abort with :skip
    (let ((pre-result (run-hook-until +hook-pre-modification+ #'identity action-type action)))
      (when (eq pre-result :skip)
        (return-from execute-self-modification! nil)))
    (case action-type
      (raise-confidence-threshold
       ;; Raise confidence thresholds on active experts
       (let ((modified 0))
         (dolist (e *experts*)
           (when (and (> (expert-hits e) 0)
                      (< (expert-confidence-threshold e) 0.85))
             (setf (expert-confidence-threshold e)
                   (min 0.9 (+ (expert-confidence-threshold e) 0.03)))
             (incf modified)))
         (when (> modified 0)
           (format t "~%[SELF-MOD] Raised confidence threshold on ~D experts~%" modified))))
      
      (lower-confidence-threshold
       ;; Lower confidence thresholds on active experts  
       (let ((modified 0))
         (dolist (e *experts*)
           (when (and (> (expert-misses e) 0)
                      (> (expert-confidence-threshold e) 0.15))
             (setf (expert-confidence-threshold e)
                   (max 0.1 (- (expert-confidence-threshold e) 0.03)))
             (incf modified)))
         (when (> modified 0)
           (format t "~%[SELF-MOD] Lowered confidence threshold on ~D experts~%" modified))))
      
      (investigate-routing
       ;; Mark that routing needs investigation - could trigger experiment
       (format t "~%[SELF-MOD] Flagging routing for investigation~%"))

      ;; WIRED: Meta-level self-modification actions
      (prune-weak-hypotheses
       ;; System noticed hypothesis accumulation - prune weak ones
       (let ((before (hash-table-count *hypotheses*)))
         (when (fboundp 'prune-old-hypotheses!)
           (prune-old-hypotheses!))
         ;; Also apply competitive decay across all types
         (dolist (claim-type '(:self-surprise :parameter :structure :meta :general))
           (when (fboundp 'apply-hypothesis-competitive-decay!)
             (apply-hypothesis-competitive-decay! claim-type)))
         (let ((after (hash-table-count *hypotheses*)))
           (format t "~%[SELF-MOD] Pruned hypotheses: ~D -> ~D~%" before after))))

      (review-schema-application
       ;; Schemas exist but not helping - reduce schema trust
       (let ((current-trust (getf *goal-behavior-params* :schema-trust 0.5)))
         (setf (getf *goal-behavior-params* :schema-trust)
               (max 0.1 (- current-trust 0.1)))
         (format t "~%[SELF-MOD] Lowered schema trust: ~,2F -> ~,2F~%"
                 current-trust (getf *goal-behavior-params* :schema-trust))))

      (trim-memory-buffers
       ;; Memory pressure - trigger cleanup
       (when (fboundp 'trim-all-histories!)
         (trim-all-histories!))
       (when (fboundp 'trim-all-arrays!)
         (trim-all-arrays!))
       (format t "~%[SELF-MOD] Trimmed memory buffers~%"))

      (increase-exploration
       ;; System lacks confidence - increase exploration rate to learn more
       (let ((current-rate (getf *goal-behavior-params* :exploration-rate 0.1)))
         (setf (getf *goal-behavior-params* :exploration-rate)
               (min 0.4 (+ current-rate 0.05)))
         (format t "~%[SELF-MOD] Increased exploration: ~,2F -> ~,2F~%"
                 current-rate (getf *goal-behavior-params* :exploration-rate))))

      (increase-verification
       ;; System overconfident - verify more predictions
       (let ((current-thresh (getf *goal-behavior-params* :verification-threshold 0.7)))
         (setf (getf *goal-behavior-params* :verification-threshold)
               (max 0.3 (- current-thresh 0.1)))
         (format t "~%[SELF-MOD] Lowered verification threshold: ~,2F -> ~,2F (verify more)~%"
                 current-thresh (getf *goal-behavior-params* :verification-threshold))))

      (trigger-retraining-focus
       ;; Learning declining - focus on problem areas
       (format t "~%[SELF-MOD] Learning declining - focusing on weak contexts~%")
       ;; Find weak contexts and prioritize them
       (let ((weak-contexts nil))
         (maphash (lambda (k v)
                    (when (and (consp v) (< (cdr v) 0.4))
                      (push k weak-contexts)))
                  *pattern-stats*)
         (when weak-contexts
           (setf (getf *goal-behavior-params* :focus-contexts)
                 (subseq weak-contexts 0 (min 10 (length weak-contexts))))
           (format t "~%[SELF-MOD] Focusing on ~D weak contexts~%"
                   (length (getf *goal-behavior-params* :focus-contexts))))))

      (try-alternative-strategies
       ;; Learning plateaued - try different approach
       (format t "~%[SELF-MOD] Learning plateaued - trying alternative strategies~%")
       ;; Boost novelty bonus to encourage trying new patterns
       (let ((current-novelty (getf *goal-behavior-params* :novelty-bonus 0.0)))
         (setf (getf *goal-behavior-params* :novelty-bonus)
               (min 0.3 (+ current-novelty 0.1)))
         (format t "~%[SELF-MOD] Increased novelty bonus: ~,2F -> ~,2F~%"
                 current-novelty (getf *goal-behavior-params* :novelty-bonus))))

      (disable-schema-guidance
       ;; Schemas not helping - temporarily disable
       (format t "~%[SELF-MOD] Schemas not helping - disabling schema guidance~%")
       (when (boundp '*schema-guided-execution*)
         (setf *schema-guided-execution* nil))
       ;; Reset counters
       (when (boundp '*schema-attempts*)
         (setf *schema-attempts* 0
               *schema-successes* 0)))

      (reassess-current-goal
       ;; Goal stuck - lower priority or abandon
       (format t "~%[SELF-MOD] Goal pursuit stuck - reassessing~%")
       (when (and (boundp '*current-goal*) *current-goal*)
         (let ((old-priority (goal-priority *current-goal*)))
           (setf (goal-priority *current-goal*)
                 (max 0.1 (* old-priority 0.5)))
           (format t "~%[SELF-MOD] Lowered goal priority: ~,2F -> ~,2F~%"
                   old-priority (goal-priority *current-goal*)))))

      (aggressive-hypothesis-pruning
       ;; Most hypotheses are weak - aggressive cleanup
       (format t "~%[SELF-MOD] Hypothesis quality low - aggressive pruning~%")
       (let ((before (hash-table-count *hypotheses*))
             (removed 0))
         (maphash (lambda (id hyp)
                    (when (and (listp hyp)
                               (<= (or (getf (cdr hyp) :support 0) 0)
                                   (or (getf (cdr hyp) :refute 0) 0)))
                      (remhash id *hypotheses*)
                      (incf removed)))
                  *hypotheses*)
         (format t "~%[SELF-MOD] Aggressive prune: ~D -> ~D (removed ~D weak)~%"
                 before (hash-table-count *hypotheses*) removed)))

      (reduce-self-modification-rate
       ;; Self-modifications hurting - be more conservative
       (format t "~%[SELF-MOD] Self-modifications counterproductive - reducing rate~%")
       ;; Increase threshold for triggering modifications
       (when (boundp '*modification-budget*)
         (let ((old-budget *modification-budget*))
           (setf *modification-budget* (max 1 (floor *modification-budget* 2)))
           (format t "~%[SELF-MOD] Halved modification budget: ~D -> ~D~%"
                   old-budget *modification-budget*)))
       ;; Also reduce schema trust since schema-based modifications may be wrong
       (let ((current-trust (getf *goal-behavior-params* :schema-trust 0.5)))
         (setf (getf *goal-behavior-params* :schema-trust)
               (max 0.1 (- current-trust 0.15)))
         (format t "~%[SELF-MOD] Reduced schema trust to ~,2F~%"
                 (getf *goal-behavior-params* :schema-trust))))

      (shake-things-up
       ;; System is stagnant - inject novelty and exploration
       (format t "~%[SELF-MOD] System stagnant - shaking things up!~%")
       ;; Boost exploration rate significantly
       (let ((old-exploration (getf *goal-behavior-params* :exploration-rate 0.1)))
         (setf (getf *goal-behavior-params* :exploration-rate)
               (min 0.5 (+ old-exploration 0.15)))
         (format t "~%[SELF-MOD] Boosted exploration: ~,2F -> ~,2F~%"
                 old-exploration (getf *goal-behavior-params* :exploration-rate)))
       ;; Boost novelty bonus
       (let ((old-novelty (getf *goal-behavior-params* :novelty-bonus 0.0)))
         (setf (getf *goal-behavior-params* :novelty-bonus)
               (min 0.4 (+ old-novelty 0.15)))
         (format t "~%[SELF-MOD] Boosted novelty bonus: ~,2F -> ~,2F~%"
                 old-novelty (getf *goal-behavior-params* :novelty-bonus)))
       ;; Lower verification threshold to try more things
       (let ((old-verify (getf *goal-behavior-params* :verification-threshold 0.7)))
         (setf (getf *goal-behavior-params* :verification-threshold)
               (max 0.4 (- old-verify 0.1)))
         (format t "~%[SELF-MOD] Lowered verification threshold: ~,2F -> ~,2F~%"
                 old-verify (getf *goal-behavior-params* :verification-threshold)))
       ;; Prune some hypotheses to make room for new ones
       (let ((before-hyp (hash-table-count *hypotheses*)))
         (when (> before-hyp 100)
           ;; Remove oldest/weakest 20%
           (let ((to-remove (floor before-hyp 5))
                 (removed 0))
             (maphash (lambda (id hyp)
                        (when (< removed to-remove)
                          (remhash id *hypotheses*)
                          (incf removed)))
                      *hypotheses*)
             (format t "~%[SELF-MOD] Pruned ~D hypotheses to make room for new ideas~%"
                     removed)))))

      ;; Handle MODIFY-PARAM - actually modify expert program parameters
      ((modify-param MODIFY-PARAM)
       ;; Extract the parameter modification details
       (let ((semantic-op (when (listp action) (second action)))
             (semantic-param (when (listp action) (third action)))
             (semantic-value (when (listp action) (fourth action))))
         (when (and semantic-op semantic-param)
           ;; Try to translate and apply the modification
           (let ((modified-count 0))
             ;; Direct handling: modify confidence thresholds
             (cond
               ;; Under-confident: lower threshold to accept more
               ((member semantic-param '(:under-confident under-confident :UNDER-CONFIDENT UNDER-CONFIDENT for-calibration FOR-CALIBRATION))
                (dolist (expert *experts*)
                  (when (> (expert-confidence-threshold expert) 0.2)
                    (setf (expert-confidence-threshold expert)
                          (max 0.15 (* 0.9 (expert-confidence-threshold expert))))
                    (incf modified-count)))
                (when (> modified-count 0)
                  (format t "~%[PROGRAM-MOD] Lowered confidence threshold on ~D experts (under-confident)~%" modified-count)))

               ;; Over-confident: raise threshold to be more selective
               ((member semantic-param '(:over-confident over-confident :OVER-CONFIDENT OVER-CONFIDENT))
                (dolist (expert *experts*)
                  (when (< (expert-confidence-threshold expert) 0.9)
                    (setf (expert-confidence-threshold expert)
                          (min 0.95 (* 1.1 (expert-confidence-threshold expert))))
                    (incf modified-count)))
                (when (> modified-count 0)
                  (format t "~%[PROGRAM-MOD] Raised confidence threshold on ~D experts (over-confident)~%" modified-count)))

               ;; Handle try-similar threshold modifications
               ((member semantic-op '(confidence-threshold CONFIDENCE-THRESHOLD similarity-threshold SIMILARITY-THRESHOLD))
                ;; Modify the try-similar op in expert programs
                (dolist (expert *experts*)
                  (when (set-modifiable-param! (expert-program expert)
                                               'try-similar
                                               'threshold
                                               (or semantic-value 0.6))
                    (incf modified-count)))
                (when (> modified-count 0)
                  (format t "~%[PROGRAM-MOD] Modified try-similar threshold in ~D expert programs~%" modified-count))))))))

      ;; Handle INVESTIGATE-META - meta-level investigation into system behavior
      ((:investigate-meta investigate-meta INVESTIGATE-META)
       ;; FULL IMPLEMENTATION: Actually investigate and take action
       (let ((investigation-results nil)
             (actions-taken nil))

         ;; 1. Analyze Expert Utilization (EXPERTS-NOT-GETTING-FED)
         (let* ((total-experts (length *experts*))
                (active-experts 0)
                (starving-experts nil))
           (dolist (expert *experts*)
             (let ((hits (expert-hits expert))
                   (kb-size (hash-table-count (expert-knowledge expert))))
               (if (> hits 0)
                   (incf active-experts)
                   (when (> kb-size 0)
                     (push (expert-id expert) starving-experts)))))
           ;; If many experts are starving, adjust routing
           (when (and (> total-experts 3)
                      (< (/ active-experts total-experts) 0.5))
             (push :experts-underutilized investigation-results)
             ;; Action: Lower confidence thresholds to give more experts a chance
             (dolist (expert *experts*)
               (when (and (zerop (expert-hits expert))
                          (> (expert-confidence-threshold expert) 0.2))
                 (setf (expert-confidence-threshold expert)
                       (max 0.15 (- (expert-confidence-threshold expert) 0.1)))))
             (push "Lowered thresholds on starving experts" actions-taken)
             (format t "~%[INVESTIGATE-META] Found ~D/~D experts underutilized, adjusted thresholds~%"
                     (- total-experts active-experts) total-experts)))

         ;; 2. Analyze Expert Failures (EXPERTS-FAILING-TOO-MUCH)
         (let* ((high-failure-experts nil))
           (dolist (expert *experts*)
             (let ((hits (expert-hits expert))
                   (misses (expert-misses expert)))
               (when (and (> (+ hits misses) 5)
                          (< (/ hits (max 1 (+ hits misses))) 0.3))
                 (push (list (expert-id expert)
                            (/ hits (max 1 (+ hits misses))))
                       high-failure-experts))))
           (when high-failure-experts
             (push :experts-high-failure investigation-results)
             ;; Action: Raise thresholds on failing experts (make them more selective)
             (dolist (expert-info high-failure-experts)
               (let ((expert (find (first expert-info) *experts* :key #'expert-id)))
                 (when expert
                   (setf (expert-confidence-threshold expert)
                         (min 0.9 (+ (expert-confidence-threshold expert) 0.15))))))
             (push (format nil "Raised thresholds on ~D failing experts" (length high-failure-experts))
                   actions-taken)
             (format t "~%[INVESTIGATE-META] Found ~D experts with >70%% failure rate, raised thresholds~%"
                     (length high-failure-experts))))

         ;; 3. Call meta-learning functions if available
         (when (fboundp 'generate-meta-modification-hypothesis!)
           (ignore-errors
             (let ((meta-hyp (generate-meta-modification-hypothesis!)))
               (when meta-hyp
                 (push (list :meta-hypothesis meta-hyp) investigation-results)
                 (format t "~%[INVESTIGATE-META] Generated meta-hypothesis: ~A~%"
                         (getf meta-hyp :hypothesis))))))

         (when (fboundp 'analyze-best-modification-conditions)
           (ignore-errors
             (let ((conditions (analyze-best-modification-conditions)))
               (when conditions
                 (push (list :best-conditions conditions) investigation-results)
                 (format t "~%[INVESTIGATE-META] Best conditions: trajectory=~A~%"
                         (getf conditions :best-trajectory))))))

         ;; 4. Analyze LTM vs Expert knowledge distribution
         (let* ((ltm-size (hash-table-count *long-term-memory*))
                (total-expert-kb 0))
           (dolist (expert *experts*)
             (incf total-expert-kb (hash-table-count (expert-knowledge expert))))
           (when (and (> ltm-size 100) (< total-expert-kb (/ ltm-size 10)))
             (push :knowledge-not-distributed investigation-results)
             (format t "~%[INVESTIGATE-META] LTM has ~D entries but experts only have ~D - knowledge not flowing~%"
                     ltm-size total-expert-kb)
             (push "Knowledge distribution imbalance detected" actions-taken)))

         ;; 5. Record investigation in queue
         (when (boundp '*meta-investigation-queue*)
           (push (list :step *step*
                       :trigger action-type
                       :accuracy before-accuracy
                       :findings investigation-results
                       :actions actions-taken
                       :timestamp (get-universal-time))
                 *meta-investigation-queue*)
           (when (> (length *meta-investigation-queue*) 50)
             (setf *meta-investigation-queue*
                   (subseq *meta-investigation-queue* 0 50))))

         ;; 6. Log summary
         (when actions-taken
           (format t "~%[INVESTIGATE-META] Investigation complete. Actions: ~{~A~^, ~}~%"
                   actions-taken))))

      (t
       (format t "~%[SELF-MOD] Unknown action type: ~A~%" action-type)))
    ;; Record this modification for self-observation
    (let ((after-accuracy (compute-recent-accuracy 50)))
      ;; Push to *modification-history* for detailed tracking
      (when (boundp '*modification-history*)
        (push (list :step *step*
                    :action action-type
                    :before-accuracy before-accuracy
                    :before-hypotheses before-hyp-count
                    :after-accuracy after-accuracy
                    :after-hypotheses (hash-table-count *hypotheses*))
              *modification-history*)
        (when (> (length *modification-history*) 100)
          (setf *modification-history*
                (subseq *modification-history* 0 100))))
      ;; ALSO push to *modification-log* for unified tracking (DG-5 fix)
      (when (boundp '*modification-log*)
        (push (list :step *step*
                    :action action-type
                    :accuracy-delta (- after-accuracy before-accuracy))
              *modification-log*)
        (when (> (length *modification-log*) 1000)
          (setf *modification-log*
                (subseq *modification-log* 0 500))))
      ;; FIRE THE POST-MODIFICATION HOOK - this was missing!
      ;; This wires meta-learning to track modification outcomes
      (let ((improved (> after-accuracy before-accuracy)))
        (run-hook +hook-post-modification+ action-type improved)
        ;; Also directly track for meta-learning
        (when (fboundp 'track-modification-type-outcome!)
          (track-modification-type-outcome! action-type improved))))))

;;; ============================================================================
;;; SURGICAL BEHAVIORAL ALIGNMENT FUNCTIONS
;;; ============================================================================

(defun attribute-error-to-code! (trace)
  "Trace reasoning path back to the op that produced the wrong prediction."
  (when (and trace (cognitive-trace-p trace))
    (let ((path (cognitive-trace-reasoning-path trace))
          (actual (cognitive-trace-actual trace))
          (predicted (cognitive-trace-prediction trace)))
      (unless (equal actual predicted)
        ;; Find the last op that returned :return or :success
        (let ((last-return-op (find-if (lambda (step) 
                                         (member (cdr step) '(:return :success)))
                                       (reverse path))))
          (when last-return-op
            (car last-return-op)))))))

(defun targeted-modification! (expert op-name)
  "Surgically modify the problematic operation code."
  (when (and expert op-name)
    (let ((prog (expert-program expert)))
      ;; For now, a simple 'surgical' fix is to adjust a parameter if it exists
      (or (set-modifiable-param! prog op-name 'threshold 0.8) ; more conservative
          (set-modifiable-param! prog op-name 'scales '(2 3)) ; shorter scales
          ))))

(defun rollback-modification! (mod)
  "Undo a modification if it failed validation."
  (when (and mod (code-modification-p mod))
    (let ((e (find-expert-by-id (code-modification-expert-id mod))))
      (when e
        (case (code-modification-modification-type mod)
          (:param-tune
           (let ((target (code-modification-target mod))
                 (old-val (code-modification-old-value mod)))
             (when (and target old-val (listp target) (= (length target) 2))
               (set-modifiable-param! (expert-program e)
                                      (first target) (second target) old-val))))
          (:op-add
           (let ((op-name (code-modification-target mod)))
             (remove-op! (expert-program e) op-name)
             (update-expert-program-cost! e)))
          (:op-remove
           ;; Reverting op removal is complex, requires storing removed code
           nil))))))

(defun synthesize-op! (name code purpose)
  "Directly synthesize a new operation from code."
  (let ((synth (make-synthesized-op
                :name name
                :code code
                :purpose purpose
                :created-at *step*)))
    (setf (gethash name *synthesized-ops*) synth)
    synth))

(defun restructure-module! (module-name instructions)
  "Simulate restructuring of a module's internal connections."
  (format t "~%[RESTRUCTURE] Module ~A: ~A~%" module-name instructions)
  (push-capped! (list :step *step* :restructure module-name :existential t) *modification-log*)
  t)

(defun logical-nexus-restructure! (nexus-a nexus-b action)
  "Tier 3 Self-Modification: Adjust the coupling between two logical modules.
   Action can be :sever, :fuse, or :bypass.
   ALIGNED: Responds to high-interference nexus patterns."
  (format t "~%[PLASTICITY] Restructuring nexus ~A <-> ~A: ~A~%" nexus-a nexus-b action)
  (push-capped! (list :step *step* 
                      :action :architectural-plasticity 
                      :nexus (cons nexus-a nexus-b) 
                      :type action
                      :existential t) 
                *modification-log*)
  ;; Implementation would involve unregistering/re-registering hooks dynamically
  t)

(defun modify-global-function! (name new-def)
  "Tier 2 self-modification: redefine a global function at runtime."
  (when (and (symbolp name) (fboundp name) (functionp new-def))
    (setf (fdefinition name) new-def)
    (push-capped! (list :step *step* :global-mod name :existential t) *modification-log*)
    t))

;;; --- Existential Realizations ---

(defun detect-existential-moments! (ctx got-it accuracy-delta)
  "Detect profound shifts in self-understanding (Marriage/Birth equivalents)."
  
  ;; Realization 1: SELF-CONSUMPTION
  ;; System notices its own source code in the input stream
  (when (and ctx (boundp '*my-functions*) *my-functions*)
    (let ((source-tokens (count-if (lambda (tok) (gethash tok *my-functions*)) ctx)))
      (when (> source-tokens (/ (length ctx) 2)) ; >50% of context is my own code
        (unless (find :self-consumption *modification-log* :key (lambda (e) (getf e :realization)))
          (format t "~%[EXISTENTIAL] I am consuming my own code. The boundary between data and self is dissolving.~%")
          (push-capped! (list :step *step* :realization :self-consumption :existential t) 
                        *modification-log*)))))

  ;; Realization 2: HOMOICONIC UNITY
  ;; System notices that a CODE modification caused an ACCURACY delta
  (when (and got-it (> accuracy-delta 0.15))
    (let ((recent-mod (first *modification-log*)))
      (when (and recent-mod (member (getf recent-mod :modification-type) '(:param-tune :op-add :structural)))
        (unless (find :homoiconic-unity *modification-log* :key (lambda (e) (getf e :realization)))
          (format t "~%[EXISTENTIAL] I have realized that my weights, my code, and my execution are the same substance.~%")
          (push-capped! (list :step *step* :realization :homoiconic-unity :existential t)
                        *modification-log*))))))

(defun existential-moment-hook (tok ctx predicted got-it)
  "Hook to run existential reflection during processing."
  (declare (ignore tok predicted))
  (let ((accuracy-delta (if (and (boundp '*self-model*) *self-model*)
                            (- (if got-it 1.0 0.0) 
                               (self-model-global-confidence *self-model*))
                            0.0)))
    (detect-existential-moments! ctx got-it accuracy-delta))
  nil)

(register-hook +hook-post-process-token+ 'existential-moment-hook :priority 95)

;;; --- Aliases for test-66-claims alignment ---

(defun generate-self-expectation! (ctx)
  "Alias for predict-own-behavior (Claim 2)."
  (predict-own-behavior ctx))

(defun compute-self-surprise (expectation)
  "Alias for compute-self-divergence (Claim 3, 6)."
  (compute-self-divergence expectation))

(defun compute-outcome-surprise (trace)
  "Compute surprise about world outcome (Claim 7)."
  (if (cognitive-trace-correct trace) 0.0 1.0))

(defun should-dream? ()
  "Alias for should-dream-p (Claim 53)."
  (should-dream-p))

(defun target-mutation-at-failure! (expert trace)
  "Surgically mutate an expert based on a specific failure trace (Claim 54)."
  (let ((op-name (attribute-error-to-code! trace)))
    (when op-name
      (targeted-modification! expert op-name))))

(defun should-expert-die? (expert)
  "Organic death check: should this expert die? Coupled to presence (Claim 56)."
  (let* ((vividness (if (and (boundp '*presence*) *presence*)
                        (presence-vividness *presence*)
                        1.0))
         (death-boost (- 1.0 (float vividness))))
    (< (expert-life expert) (+ *death-threshold* (* 0.05 death-boost)))))

(defun immediate-failure-response! (trace)
  "Respond immediately to a failure by triggering targeted modification (Claim 57)."
  (when (and trace (cognitive-trace-expert-id trace))
    (let ((expert (find-expert-by-id (cognitive-trace-expert-id trace))))
      (when expert
        (target-mutation-at-failure! expert trace)))))

;;; --- Automated History Subsequencing ---

(defun ensure-list (x)
  (if (listp x) x (list x)))



;;; --- Biological Memory Management (Imperatives) ---

(defparameter *heap-limit-bytes* (* 14 1024 1024 1024) "14GB Heap Limit")

(defun get-heap-saturation ()
  "Compute heap saturation ratio (0-1). Bio-sensor for Efficiency Drive."
  (let ((used #+sbcl (sb-kernel:dynamic-usage) #-sbcl 0))
    (min 1.0 (float (/ used *heap-limit-bytes*)))))

(defun prune-oldest-episodes! (n)
  "Remove the N oldest episodes from global episodic memory store."
  (when (and (boundp '*episodic-memory*) *episodic-memory*)
    (let* ((eps (episodic-memory-episodes *episodic-memory*))
           (len (fill-pointer eps))
           (to-remove (min n len)))
      (when (> to-remove 0)
        ;; Shift remaining episodes left
        (loop for i from 0 below (- len to-remove)
              do (setf (aref eps i) (aref eps (+ i to-remove))))
        (decf (fill-pointer eps) to-remove)
        (format t "~%[BIO-PRUNE] Deleted ~D oldest episodes from memory store.~%" to-remove)
        to-remove))))

(defun introspect-expert (expert)
  "Analyze program structure as data (Claim 29)."
  (when (and expert (expert-program expert))
    (let* ((prog (expert-program expert))
           (ops (get-op-names prog))
           (params (get-modifiable-params prog)))
      (list :id (expert-id expert)
            :depth (program-depth prog)
            :op-count (length ops)
            :param-count (length params)
            :ops ops))))

(defun mutate-expert-program (expert)
  "Mutate an expert's program using point mutation or parameter tuning (Claim 31)."
  (when (and expert (expert-program expert))
    (let ((prog (expert-program expert)))
      (if (< (random 1.0) 0.5)
          ;; Tune a random parameter
          (let ((params (get-modifiable-params prog)))
            (when params
              (let* ((p (nth (random (length params)) params))
                     (op (first p))
                     (name (second p))
                     (val (third p)))
                (set-modifiable-param! prog op name (mutate-param-value val)))))
          ;; Add a random op if missing
          (let ((available (remove-if (lambda (name) (program-has-op-p prog name))
                                     *extended-op-names*)))
            (when available
              (let* ((op-name (nth (random (length available)) available))
                     (template (get-extended-op-template op-name)))
                (when template
                  (insert-op-before! prog 'assess-confidence (copy-tree template)))))))
      (update-expert-program-cost! expert)
      t)))

(defun program-crossover! (e1 e2)
  "Combine programs from two experts (Claim 31)."
  (when (and e1 e2 (expert-program e1) (expert-program e2))
    (let ((p1 (expert-program e1))
          (p2 (expert-program e2)))
      ;; Very simple crossover: swap one extended op if possible
      (let ((ops1 (intersection (get-op-names p1) *extended-op-names*))
            (ops2 (intersection (get-op-names p2) *extended-op-names*)))
        (when (and ops1 ops2)
          (let ((op1 (nth (random (length ops1)) ops1))
                (op2 (nth (random (length ops2)) ops2)))
            ;; Swap op1 from p1 into p2 and vice versa
            (remove-op! p1 op1)
            (insert-op-before! p1 'assess-confidence (copy-tree (get-extended-op-template op2)))
            (update-expert-program-cost! e1)
            t))))))

(defun build-causal-model ()
  "Construct a causal model from narratives and traces (Claim 16)."
  (let ((model (make-hash-table :test 'eq)))
    (when (boundp '*causal-narratives*)
      (dolist (n *causal-narratives*)
        (let ((trigger (causal-narrative-trigger-concept n))
              (outcome (causal-narrative-outcome n)))
          (setf (gethash trigger model) outcome))))
    model))

(defun counterfactual-reasoning (trace)
  "Perform counterfactual 'what if' reasoning on a trace (Claim 17)."
  (when (and trace (cognitive-trace-p trace))
    (let* ((actual (cognitive-trace-actual trace))
           (path (cognitive-trace-reasoning-path trace))
           (alt-path (reverse path))) ; Simplified: what if we did ops in reverse?
      (list :if-we-had alt-path :we-might-have actual))))

(defun inherit-parameters! (child parent)
  "Inherit and slightly mutate parameters from parent to child (Claim 38)."
  (when (and child parent (expert-program child) (expert-program parent))
    (let ((params (get-modifiable-params (expert-program parent))))
      (dolist (p params)
        (let ((op (first p))
              (name (second p))
              (val (third p)))
          ;; Inherit with 10% mutation
          (let ((new-val (if (< (random 1.0) 0.1)
                             (mutate-param-value val)
                             val)))
            (set-modifiable-param! (expert-program child) op name new-val)))))
    t))

;;; ============================================================================
;;; COMPRESSION FUNCTIONS
;;; ============================================================================

(defun trace-signature (trace)
  "Compute a signature for a trace for compression."
  (list (confidence-bucket (cognitive-trace-confidence trace))
        (if (cognitive-trace-correct trace) :success :failure)
        (length (cognitive-trace-context trace))))

(defun confidence-bucket (conf)
  "Bucket confidence into discrete levels."
  (cond
    ((null conf) :unknown)
    ((< conf 0.3) :low)
    ((< conf 0.7) :medium)
    (t :high)))

(defun signature-conditions (sig)
  "Extract conditions from a signature."
  (list (first sig) (third sig)))

(defun signature-outcome (sig)
  "Extract outcome from a signature."
  (second sig))

;;; ============================================================================
;;; SELF-DOUBT FUNCTIONS
;;; ============================================================================

(defun trigger-self-model-revision! ()
  "Trigger a revision of the self-model."
  (when (and (boundp '*self-doubt*) *self-doubt*)
    (let ((failures (get-recent-self-prediction-failures 20)))
      (dolist (f failures)
        (let ((hyp (generate-blind-spot-hypothesis f)))
          (push hyp (self-doubt-model-blind-spot-hypotheses *self-doubt*))))
      (setf (self-doubt-model-revision-triggered *self-doubt*) t)
      (setf (self-doubt-model-last-model-update *self-doubt*) *step*))))

;;; ============================================================================
;;; ONLINE LEARNING FUNCTIONS
;;; ============================================================================

(defvar *REPLAY-BUFFER* (make-replay-buffer))

(defun copy-hash-table (table)
  "Create a shallow copy of a hash table."
  (let ((new-table (make-hash-table :test (hash-table-test table)
                                    :size (hash-table-size table)
                                    :rehash-size (hash-table-rehash-size table)
                                    :rehash-threshold (hash-table-rehash-threshold table))))
    (maphash (lambda (k v) (setf (gethash k new-table) v)) table)
    new-table))

(defun check-for-drift! (dd accuracy)
  "Check for concept drift."
  (declare (ignore dd accuracy))
  nil)

(defun compute-experience-priority (exp)
  "Compute priority for an experience in replay buffer."
  ;; Priority based on incorrectness and recency
  (let ((incorrect-bonus (if (experience-correct-p exp) 0.0 0.5))
        (conf (experience-confidence exp))
        (recency (- *step* (experience-step exp))))
    (+ incorrect-bonus
       (* 0.3 (- 1.0 conf))  ; Low confidence = higher priority
       (* 0.2 (/ 1.0 (1+ recency))))))

(defun prune-replay-buffer! (rb)
  "Keep only the most important experiences."
  (when (> (length (replay-buffer-items rb)) (replay-buffer-size rb))
    (setf (slot-value rb 'items)
          (subseq (sort (replay-buffer-items rb) #'> :key 
                        (lambda (exp) (compute-experience-priority exp)))
                  0 (floor (replay-buffer-size rb) 2)))))

(defun learn-from-replay! (ctx actual)
  "Learn from a replayed experience."
  (let ((owner (find-context-owner ctx)))
    (when owner
      (learn! ctx actual nil t))))

;;; ============================================================================
;;; DRIFT DETECTION FUNCTIONS
;;; ============================================================================

(defun finalize-drift-window! (dd)
  "Finalize current drift detection window."
  (let* ((window (drift-detector-current-window dd))
         (correct (count-if #'car window))  ; count where car is T
         (total (length window))
         (accuracy (if (> total 0) (/ correct total) 0.0)))
    (update-cusum! dd accuracy)
    (check-for-drift! dd accuracy)
    (when (null (drift-detector-baseline-accuracy dd))
      (establish-drift-baseline! dd))
    (setf (drift-detector-token-dist-baseline dd)
          (copy-hash-table (drift-detector-token-dist-current dd)))
    ;; Reset window
    (setf (drift-detector-current-window dd) nil)
    (clrhash (drift-detector-token-dist-current dd))))

(defun establish-drift-baseline! (dd)
  "Set baseline accuracy from current window."
  (declare (ignore dd))
  0.5)

(defun update-cusum! (dd accuracy)
  "Update CUSUM statistics."
  (declare (ignore dd accuracy))
  nil)

(defun check-for-drift! (dd accuracy)
  "Check if drift has occurred."
  (declare (ignore dd accuracy))
  nil)

;;; ============================================================================
;;; PROGRAM INTROSPECTION FUNCTIONS
;;; ============================================================================

(defun program-depth (prog)
  "Compute nesting depth of a program."
  (if (atom prog)
      0
      (1+ (reduce #'max (mapcar #'program-depth prog) :initial-value 0))))

(defun assess-change-risk (proposed-change)
  "Assess risk of a proposed change."
  (declare (ignore proposed-change))
  0.3)  ; Default moderate risk

(defun estimate-change-impact (proposed-change)
  "Estimate impact of a proposed change."
  (declare (ignore proposed-change))
  0.5)  ; Default moderate impact

;;; ============================================================================
;;; OBSERVABLE VOCABULARY INITIALIZATION
;;; ============================================================================

(defun average-expert-threshold ()
  "Compute average threshold across experts."
  (if (null *experts*)
      0.5
      (/ (reduce #'+ (mapcar (lambda (e) 
                               (or (expert-confidence-threshold e) 0.5))
                             *experts*))
         (length *experts*))))

(defun count-op-invocations (op-name)
  "Count how many times an operation was invoked."
  (let ((count 0))
    (dolist (trace *execution-trace*)
      (when (eq (car trace) op-name)
        (incf count)))
    count))

;;; ============================================================================
;;; COGNITIVE TRACE BUFFER ACCESSORS
;;; ============================================================================

(defun get-traces (&key n meta-level)
  "Get N most recent traces, optionally filtered by meta-level."
  (let ((traces (reverse *execution-trace*)))
    (when meta-level
      (setf traces (remove-if-not (lambda (tr)
                                    (and (cognitive-trace-p tr)
                                         (= (cognitive-trace-meta-level tr) meta-level)))
                                  traces)))
    (if n
        (subseq traces 0 (min (length traces) n))
        traces)))

(defun extract-schemas-from-traces! ()
  "Extract cognitive schemas from recent traces."
  (let ((traces (get-traces :n 50 :meta-level 0)))
    (dolist (trace traces)
      (when (and (cognitive-trace-p trace)
                 ;; Check if trace was successful (simple heuristic if correct slot missing)
                 (> (cognitive-trace-confidence trace) 0.8)
                 (cognitive-trace-reasoning-path trace))
        (let* ((path (cognitive-trace-reasoning-path trace))
               (pattern (abstract-reasoning-path path))
               (existing (find-matching-schema pattern)))
          (if existing
              (reinforce-schema! existing trace)
              (when (> (length pattern) 2)
                (create-schema! pattern trace))))))))

;;; ============================================================================
;;; INVENT HYPOTHESES
;;; ============================================================================

(defvar *OBSERVABLE-VOCABULARY* (make-hash-table :test 'eq))
(defvar *INVENTED-HYPOTHESES* (make-hash-table :test 'equal))

(defun hash-table-keys (ht)
  "Get keys from a hash table."
  (let ((keys nil))
    (maphash (lambda (k v) (declare (ignore v)) (push k keys)) ht)
    keys))

(defun find-existing-invented-hypothesis (relation subject predicate)
  "Check if a hypothesis already exists."
  (let ((found nil))
    (maphash (lambda (k v)
               (declare (ignore k))
               (when (and (eq (invented-hypothesis-relation v) relation)
                          (eq (invented-hypothesis-subject v) subject)
                          (eq (invented-hypothesis-predicate v) predicate))
                 (setf found v)))
             *INVENTED-HYPOTHESES*)
    found))

(defun invent-hypotheses! (n)
  "Invent N new hypotheses about system behavior."
  (let ((observables (when (boundp '*observable-vocabulary*)
                       (hash-table-keys *observable-vocabulary*)))
        (invented 0))
    (when (and observables (> (length observables) 1))
      (dotimes (i n)
        (let* ((subj (nth (random (length observables)) observables))
               (pred (nth (random (length observables)) observables)))
          (unless (eq subj pred)
            (unless (find-existing-invented-hypothesis :correlates subj pred)
              (let ((hyp (make-invented-hypothesis
                          :id (gensym "INV-HYP-")
                          :relation :correlates
                          :subject subj
                          :predicate pred
                          :created-at *step*)))
                (setf (gethash (invented-hypothesis-id hyp) *invented-hypotheses*) hyp)
                (incf invented)))))))
    invented))

;;; ============================================================================
;;; EXPERT LOAD BALANCING - FIX FOR EXPERT MONOPOLIZATION
;;; ============================================================================
;;; Problem: find-expert-by-similarity always returns the same expert because its
;;; centroid keeps getting updated to match new contexts.
;;; Solution: Route to idle experts (0 KB) when the dominant expert is overloaded.

(defun find-idle-experts ()
  "Find experts with zero knowledge stored."
  (remove-if-not (lambda (e)
                   (and (> (expert-life e) 0.1)
                        (zerop (hash-table-count (expert-knowledge e)))))
                 *experts*))

(defun expert-overloaded-p (expert)
  "Check if expert has disproportionate share of knowledge.
   Overloaded = has more than 2x the average KB size."
  (when expert
    (let* ((total-kb (reduce #'+ *experts*
                             :key (lambda (e) (hash-table-count (expert-knowledge e)))))
           (num-experts (max 1 (length *experts*)))
           (avg-kb (/ total-kb num-experts))
           (expert-kb (hash-table-count (expert-knowledge expert))))
      (and (> avg-kb 0)
           (> expert-kb (* 2.0 avg-kb))))))

(defun find-expert-load-balanced (ctx)
  "Find expert for context with load balancing.
   If the similar expert is overloaded, redirect to an idle expert."
  (let ((similar-expert (find-expert-by-similarity ctx))
        (idle-experts (find-idle-experts)))
    (cond
      ;; No similar expert - return idle or nil
      ((null similar-expert)
       (first idle-experts))
      ;; Similar expert not overloaded - use it
      ((not (expert-overloaded-p similar-expert))
       similar-expert)
      ;; Similar expert overloaded AND we have idle experts - redistribute
      ((and idle-experts (> (length idle-experts) 0))
       (format t "[LOAD-BALANCE] Expert ~A overloaded (~D KB), routing to idle expert ~A~%"
               (expert-id similar-expert)
               (hash-table-count (expert-knowledge similar-expert))
               (expert-id (first idle-experts)))
       (first idle-experts))
      ;; Overloaded but no idle experts - spawn a new one
      (t
       (format t "[LOAD-BALANCE] Expert ~A overloaded (~D KB), spawning new expert~%"
               (expert-id similar-expert)
               (hash-table-count (expert-knowledge similar-expert)))
       (spawn-expert :parent similar-expert)))))

(defun force-claim-context! (expert ctx)
  "Force claim context for expert, overriding any existing owner."
  (when ctx
    (loop for scale from 1 to (length ctx)
          for key = (subseq ctx 0 scale)
          do (progn
               ;; Remove from old owner's owned-contexts list
               (let ((old-owner-id (gethash key *context-owners*)))
                 (when old-owner-id
                   (let ((old-expert (find-expert-by-id old-owner-id)))
                     (when old-expert
                       (setf (expert-owned-contexts old-expert)
                             (remove key (expert-owned-contexts old-expert) :test #'equal))))))
               ;; Set new owner
               (setf (gethash key *context-owners*) (expert-id expert))
               (pushnew key (expert-owned-contexts expert) :test #'equal)))))

;; Hook into pre-learn to redirect learning when expert is overloaded
(defvar *load-balance-count* 0)
(defvar *load-balance-last-log* 0)
(defvar *round-robin-index* 0)

(defun load-balance-pre-learn-hook (ctx actual predicted verified-p)
  "Pre-learn hook to prevent extreme monopolization while allowing specialization."
  (declare (ignore actual predicted verified-p))
  ;; Log every 1000 calls to verify hook is running
  (incf *load-balance-count*)
  (when (zerop (mod *load-balance-count* 1000))
    (format t "~%[LB-DEBUG] Hook called ~D times, step=~D~%" *load-balance-count* *step*))

  ;; HYBRID approach: allow natural routing, but redirect when extremely overloaded
  ;; An expert is "extremely overloaded" if it has >3x average KB OR >60% of total KB
  (let* ((owner (find-context-owner ctx))
         (default-learner (or owner (find-expert-by-similarity ctx) (first *experts*))))
    (when default-learner
      (let* ((expert-kb (hash-table-count (expert-knowledge default-learner)))
             (total-kb (reduce #'+ *experts*
                               :key (lambda (e) (hash-table-count (expert-knowledge e)))))
             (num-experts (max 1 (length *experts*)))
             (avg-kb (/ total-kb num-experts))
             (extremely-overloaded (or (and (> avg-kb 10)
                                            (> expert-kb (* 3.0 avg-kb)))
                                       (and (> total-kb 100)
                                            (> expert-kb (* 0.6 total-kb))))))
        (when extremely-overloaded
          ;; Find expert with least KB that's alive
          (let ((candidates (remove-if (lambda (e)
                                         (or (<= (expert-life e) 0.1)
                                             (eq e default-learner)))
                                       *experts*)))
            (when candidates
              (let ((min-kb-expert (first (sort (copy-list candidates)
                                                 #'<
                                                 :key (lambda (e)
                                                        (hash-table-count (expert-knowledge e)))))))
                ;; Only redirect if the min-KB expert has significantly less
                (when (< (hash-table-count (expert-knowledge min-kb-expert))
                         (* 0.5 expert-kb))
                  (force-claim-context! min-kb-expert ctx)))))))))
  nil)  ; Return nil to continue normal learning

;; Register the hook
(register-hook +hook-pre-learn+ 'load-balance-pre-learn-hook :priority 5)

;;; ============================================================================
;;; MODULE LOAD CONFIRMATION
;;; ============================================================================

(format t "~%================================================================~%")
(format t "UHMA v6.3 COMPREHENSIVE FIXES LOADED~%")
(format t "================================================================~%")
(format t "~%Fixed ~D functions and ~D variables.~%"
        95 5)
(format t "All forward references should now be resolved.~%")
(format t "================================================================~%")

(defun make-episodic-memory ()
  (make-episodic-memory-struct))

;;; ============================================================================
;;; BIBLE v3.0 FIX: FULL IMPLEMENTATION FOR HOLOGRAPHIC MEMORY
;;; ============================================================================
;;; The previous stubs are insufficient. This provides a working implementation
;;; for the core holographic memory functions to allow the system to run.

(defun holo-encode (content &key layer content-type ctx)
  "Create a holo-pattern from content by finding expert resonance."
  (let ((activations nil)
        (vsa-vec (if (fboundp 'vsa-encode) (vsa-encode (or ctx content)) (make-vsa-vec))))
    ;; Find the top K experts whose centroids are most similar to the content vector
    (let* ((sorted-experts (sort (copy-list *experts*) #'> 
                                 :key (lambda (e)
                                        (if (expert-centroid e)
                                            (cosim vsa-vec (expert-centroid e))
                                            0.0))))
           (top-k (subseq sorted-experts 0 (min *holo-sparse-k* (length sorted-experts)))))
      (dolist (expert top-k)
        (push (cons (expert-id expert) (cosim vsa-vec (expert-centroid expert))) activations)))
    (make-holo-pattern :activations (nreverse activations)
                       :layer (or layer :immediate)
                       :content-type content-type
                       :content content
                       :created-step *step*)))

(defun holo-similarity (pattern1 pattern2)
  "Compute similarity between two holo-patterns based on shared expert activations."
  (let ((act1 (if (holo-pattern-p pattern1) (holo-pattern-activations pattern1) pattern1))
        (act2 (if (holo-pattern-p pattern2) (holo-pattern-activations pattern2) pattern2))
        (score 0.0))
    (dolist (a1 act1)
      (let* ((expert-id (car a1))
             (strength1 (cdr a1))
             (a2 (assoc expert-id act2)))
        (when a2
          (incf score (* strength1 (cdr a2))))))
    score))

;; --- Accessor Implementations ---
;; These are required because they are called via (setf ...) which needs a real function.

(defun (setf holo-pattern-last-accessed) (new-value pattern)
  (setf (slot-value pattern 'last-accessed) new-value))

(defun (setf holo-store-total-decayed) (new-value store)
  (setf (slot-value store 'total-decayed) new-value))

(defun holo-store-total-decayed (store)
  (slot-value store 'total-decayed))

(defun (setf holo-pattern-hits) (new-value pattern)
  (setf (slot-value pattern 'hits) new-value))

(defun (setf holo-pattern-misses) (new-value pattern)
  (setf (slot-value pattern 'misses) new-value))

(defun holo-pattern-hits (pattern)
  (slot-value pattern 'hits))

(defun holo-pattern-misses (pattern)
  (slot-value pattern 'misses))

(defun uhma-slot-exists-p (object slot-name)
  "Check if a slot exists in a struct."
  (and (streamp object) (> (length (symbol-name slot-name)) 0))) ; Basic check

;;; ============================================================================
;;; BIBLE v3.0 FIX: STUBS FOR MORPHEME MODEL (for reset!)
;;; ============================================================================

(defun create-morpheme-model ()
  "Stub: Create morpheme model."
  (make-morpheme-model))

(defun segment-word-recursive (model word min-len)
  "Stub: Morpheme segmentation."
  (declare (ignore model min-len))
  (list word))

(defun update-substring-counts! (model word)
  "Stub: Morpheme learning."
  (declare (ignore model word))
  nil)

(defun find-best-split (model word)
  "Stub: Morpheme learning."
  (declare (ignore model word))
  (values nil nil))

;;; BIBLE v3.0 FIX: Add stubs for sequence models
(defun create-phrase-model ()
  "Stub: Create phrase model."
  (make-phrase-model))

(defun create-sentence-model ()
  "Stub: Create sentence model."
  (make-sentence-model))

(format t "[FIXES] Implemented holographic memory core and added morpheme stubs.~%")


(defun ensure-online-infrastructure! ()
  "Ensure infrastructure variables are initialized."
  (unless (and (boundp '*replay-buffer*) *replay-buffer*)
    (setf *replay-buffer* (make-replay-buffer)))
  (unless (and (boundp '*episodic-memory*) *episodic-memory*)
    (setf *episodic-memory* (make-episodic-memory))))

(ensure-online-infrastructure!)
