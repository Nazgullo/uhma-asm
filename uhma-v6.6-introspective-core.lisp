;;;; ============================================================================
;;;; UHMA INTROSPECTIVE GROUNDING - CORE (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Provides named concepts, causal narratives, and grounded hypotheses.
;;;; DEPENDS ON: uhma-v6.1-core-homoiconic.lisp, uhma-v6.2-deep-mind.lisp
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 4: STRUCTURES ---

(defstruct introspective-concept
  "A named symbolic label for a qualitative mental state."
  (name nil :type symbol)
  (description "" :type string)
  (detection-fn nil :type (or null function))
  (activation-count 0 :type fixnum)
  (usefulness 0.5 :type single-float)
  related-concepts typical-causes typical-effects
  (state-signatures nil :type list)
  (learned-thresholds nil :type list))

(defstruct causal-narrative
  "A sequence of concepts explaining a behavioral outcome."
  trigger-concept sequence outcome
  (occurrences 1 :type fixnum)
  (confidence 0.5 :type single-float))

(defstruct grounded-hypothesis
  "A verifiable theory linking introspective concepts."
  (id (gensym "GHYP-") :type symbol)
  (statement "" :type string)
  (if-concepts nil :type list)
  (then-concepts nil :type list)
  (support 0 :type fixnum)
  (opposition 0 :type fixnum)
  (tests 0 :type fixnum)
  (created-step 0 :type fixnum)
  (status :active :type symbol))

;;; --- SECTION 6: SPECIAL VARIABLES ---

(defvar *introspective-vocabulary* (make-hash-table :test 'eq))
(defvar *concept-activation-history* nil)
(defvar *grounded-hypotheses* nil)
(defvar *causal-narratives* nil)
(defvar *concept-detection-sample-rate* 10 )
(defvar *concept-detection-counter* 0 )
(defvar *last-concept-detection-step* -1 )
(defvar *cached-active-concepts* nil)

;;; --- SECTION 8: CORE FUNCTIONS ---

(defun define-introspective-concept! (&key name description detection-fn typical-causes typical-effects related-concepts examples)
  "Register a new concept in the system's ego vocabulary."
  (setf (gethash name *introspective-vocabulary*)
        (make-introspective-concept
         :name name :description description :detection-fn detection-fn
         :typical-causes typical-causes :typical-effects typical-effects
         :related-concepts related-concepts)))

(defun record-concept-activation! (concepts)
  "Add active concepts to the rolling history."
  (when concepts
    (push (list :step *step* :concepts concepts :timestamp (get-internal-real-time))
          *concept-activation-history*)
    (when (> (length *concept-activation-history*) 500)
      (setf *concept-activation-history* (subseq *concept-activation-history* 0 500)))))

(defun gather-current-state ()
  "Collect actual NN metrics for concept detection.
   Reads from trace buffer, expert activations, holographic substrate, and presence."
  (let ((accuracy 0.5)
        (sparsity 5)
        (accuracy-trend 0.0)
        (interference 0.0)
        (pattern-promotions 0))
    ;; Prediction accuracy from recent traces
    (when (and (boundp '*trace-buffer*) (> (fill-pointer *trace-buffer*) 0))
      (let ((correct 0) (total (min 20 (fill-pointer *trace-buffer*))))
        (loop for i from (1- (fill-pointer *trace-buffer*))
              downto (max 0 (- (fill-pointer *trace-buffer*) total))
              for trace = (aref *trace-buffer* i)
              when (eq (cognitive-trace-prediction trace) (cognitive-trace-actual trace))
              do (incf correct))
        (setf accuracy (/ (float correct) (max 1 total))))
      ;; Accuracy trend (compare last 20 vs previous 20)
      (when (> (fill-pointer *trace-buffer*) 40)
        (let ((recent 0) (older 0))
          (loop for i from (1- (fill-pointer *trace-buffer*))
                downto (max 0 (- (fill-pointer *trace-buffer*) 20))
                for trace = (aref *trace-buffer* i)
                when (eq (cognitive-trace-prediction trace) (cognitive-trace-actual trace))
                do (incf recent))
          (loop for i from (max 0 (- (fill-pointer *trace-buffer*) 21))
                downto (max 0 (- (fill-pointer *trace-buffer*) 40))
                for trace = (aref *trace-buffer* i)
                when (eq (cognitive-trace-prediction trace) (cognitive-trace-actual trace))
                do (incf older))
          (setf accuracy-trend (- (/ (float recent) 20.0) (/ (float older) 20.0))))))
    ;; Expert activation sparsity
    (when (and (boundp '*current-expert-activations*) *current-expert-activations*)
      (setf sparsity (length *current-expert-activations*)))
    ;; Holographic interference
    (when (fboundp 'compute-holographic-interference)
      (setf interference (compute-holographic-interference)))
    ;; Pattern promotions (consolidation activity)
    (when (and (boundp '*holographic-memory*) *holographic-memory*)
      (let ((working (gethash :working (holographic-memory-by-layer *holographic-memory*))))
        (setf pattern-promotions
              (count-if (lambda (hp) (> (holographic-pattern-strength hp) 0.8))
                        (or working nil)))))
    (list :step *step*
          :accuracy accuracy
          :sparsity sparsity
          :accuracy-trend accuracy-trend
          :interference interference
          :pattern-promotions pattern-promotions
          :trajectory (when (and (boundp '*presence*) *presence*)
                       (presence-trajectory *presence*))
          :continuity (when (and (boundp '*presence*) *presence*)
                       (presence-continuity *presence*)))))

(defun concept-applies-via-learned-p (concept state)
  "Check if concept applies by comparing current state vector against learned signatures.
   Each concept accumulates state-signatures when it activates; if current state
   has high cosine similarity to a learned signature, concept applies."
  (when (and concept (introspective-concept-p concept)
             (introspective-concept-state-signatures concept)
             state)
    (let ((state-vec (state-to-vsa-vector state)))
      (when state-vec
        (dolist (sig (introspective-concept-state-signatures concept))
          (when (> (cosim state-vec sig) 0.6)
            (return-from concept-applies-via-learned-p t))))))
  nil)

(defun state-to-vsa-vector (state)
  "Encode system state as a VSA vector for concept signature matching."
  (when (and state (listp state))
    (let ((vec (make-vsa-vec)))
      ;; Encode accuracy component
      (let ((acc (or (getf state :accuracy) 0.5)))
        (vsa-superpose! vec (get-vsa-vec (cond ((> acc 0.7) :high-accuracy)
                                               ((< acc 0.3) :low-accuracy)
                                               (t :mid-accuracy)))
                        (coerce acc 'single-float)))
      ;; Encode sparsity component
      (let ((sp (or (getf state :sparsity) 5)))
        (vsa-superpose! vec (get-vsa-vec (cond ((> sp 10) :high-sparsity)
                                               ((< sp 3) :low-sparsity)
                                               (t :mid-sparsity)))
                        (min 1.0 (/ (float sp) 15.0))))
      ;; Encode trajectory
      (let ((traj (getf state :trajectory)))
        (when traj
          (vsa-superpose! vec (get-vsa-vec traj) 0.3)))
      vec)))

(defun record-concept-state-signature! (concept state)
  "Record the current state as a learned signature for this concept."
  (when (and concept (introspective-concept-p concept) state)
    (let ((vec (state-to-vsa-vector state)))
      (when vec
        (push vec (introspective-concept-state-signatures concept))
        ;; Keep only last 10 signatures
        (when (> (length (introspective-concept-state-signatures concept)) 10)
          (setf (introspective-concept-state-signatures concept)
                (subseq (introspective-concept-state-signatures concept) 0 10)))))))

(defun initialize-introspective-vocabulary! ()
  "Populate the standard introspective concepts with organic detection functions.
   Each concept detects itself from actual NN state metrics."
  (define-introspective-concept!
    :name 'CONFUSED
    :description "Many experts firing but none winning - high sparsity, low accuracy."
    :detection-fn (lambda (state)
                    (and (< (or (getf state :accuracy) 0.5) 0.3)
                         (> (or (getf state :sparsity) 5) 10))))
  (define-introspective-concept!
    :name 'CONFIDENT
    :description "Few experts firing with high agreement - low sparsity, high accuracy."
    :detection-fn (lambda (state)
                    (and (> (or (getf state :accuracy) 0.5) 0.7)
                         (< (or (getf state :sparsity) 5) 5))))
  (define-introspective-concept!
    :name 'LEARNING
    :description "Accuracy increasing - positive trend in prediction quality."
    :detection-fn (lambda (state)
                    (> (or (getf state :accuracy-trend) 0.0) 0.05)))
  (define-introspective-concept!
    :name 'STUCK
    :description "Low accuracy with no improvement - flat or negative trend.
                  Requires sufficient data (50+ traces) to distinguish from warmup."
    :detection-fn (lambda (state)
                    (and ;; Maturity guard: need enough data to judge stagnation vs warmup
                         (boundp '*trace-buffer*)
                         (> (fill-pointer *trace-buffer*) 50)
                         ;; Actual stagnation detection
                         (< (or (getf state :accuracy) 0.5) 0.4)
                         (<= (or (getf state :accuracy-trend) 0.0) 0.0))))
  (define-introspective-concept!
    :name 'EXPLORING
    :description "High sparsity with searching trajectory - trying many experts."
    :detection-fn (lambda (state)
                    (and (> (or (getf state :sparsity) 5) 8)
                         (eq (getf state :trajectory) :searching))))
  (define-introspective-concept!
    :name 'CONSOLIDATING
    :description "Holographic patterns gaining strength - knowledge crystallizing."
    :detection-fn (lambda (state)
                    (> (or (getf state :pattern-promotions) 0) 2))))

;;; --- SECTION 11: VERIFICATION ---
(eval-when (:load-toplevel :execute)
  (format t "[INTROSPECTIVE-CORE] Vocabulary foundation loaded.~%"))
