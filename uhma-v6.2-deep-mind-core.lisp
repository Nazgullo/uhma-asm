;;;; ============================================================================
;;;; UHMA DEEP MIND - CORE (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Provides cognitive tracing, epistemic uncertainty, and hypothesis storage.
;;;; DEPENDS ON: uhma-v6.1-core-homoiconic.lisp, uhma-vsa-substrate.lisp
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 4: STRUCTURES ---

;; (cognitive-trace struct moved to forward-decl.lisp)

(defstruct epistemic-state
  "Decomposed uncertainty estimates."
  (aleatoric 0.5 :type single-float)
  (epistemic 0.5 :type single-float)
  (model-uncertainty 0.5 :type single-float)
  (meta-uncertainty 0.5 :type single-float)
  (known-unknowns nil :type list)
  (suspected-unknowns nil :type list))

(defstruct self-hypothesis
  "An explicit theory about system behavior."
  (id (gensym "HYP-") :type symbol)
  claim claim-type
  (evidence-for nil :type list)
  (evidence-against nil :type list)
  (confidence 0.5 :type single-float)
  (times-tested 0 :type fixnum)
  (times-confirmed 0 :type fixnum)
  proposed-action
  (created-at 0 :type fixnum)
  (last-tested 0 :type fixnum)
  (status :active :type symbol))

;;; --- SECTION 6: SPECIAL VARIABLES ---

(defvar *trace-buffer* (make-array 500 :fill-pointer 0 :adjustable t))
(defvar *trace-buffer-max* 500)
(defvar *hypotheses* (make-hash-table :test 'equal))
(defvar *hypothesis-history* nil)
(defvar *max-evidence-per-hypothesis* 20)

;;; --- SECTION 8: CORE FUNCTIONS ---

(defun record-trace! (trace)
  "Add trace to rolling buffer with automatic cleanup."
  (declare (type cognitive-trace trace))
  (when (>= (fill-pointer *trace-buffer*) *trace-buffer-max*)
    (let ((keep-count (floor *trace-buffer-max* 2)))
      (loop for i from 0 below keep-count
            do (setf (aref *trace-buffer* i) (aref *trace-buffer* (+ i keep-count))))
      (setf (fill-pointer *trace-buffer*) keep-count)))
  (vector-push-extend trace *trace-buffer*))

(defun get-traces (&key (n 100) (min-step 0) (meta-level nil))
  "Retrieve traces matching criteria."
  (declare (type fixnum n min-step))
  (let ((result nil))
    (loop for i from (1- (fill-pointer *trace-buffer*)) downto 0
          for trace = (aref *trace-buffer* i)
          while (and (< (length result) n) (>= (cognitive-trace-step trace) min-step))
          when (or (null meta-level) (= (cognitive-trace-meta-level trace) meta-level))
          do (push trace result))
    (nreverse result)))

(defun compute-epistemic-state (ctx prediction confidence)
  "Compute structured uncertainty."
  (declare (type list ctx) (type single-float confidence))
  (let ((encounters (get-encounters ctx)))
    (make-epistemic-state
     :aleatoric (if (> encounters 50) (- 1.0 confidence) 0.3)
     :epistemic (if (< encounters 10) (- 1.0 (* confidence (/ encounters 10.0))) (* 0.3 (- 1.0 confidence)))
     :model-uncertainty (if *self-model* (* 0.5 (self-model-expected-error *self-model*)) 0.5)
     :meta-uncertainty (if (< encounters 5) 0.8 0.3)
     :known-unknowns (when (< confidence 0.5) (list (list :low-confidence-context ctx))))))

(defun claims-semantically-similar-p (claim1 claim2)
  "Check if two claims are semantically similar."
  (cond ((equal claim1 claim2) t)
        ((and (listp claim1) (listp claim2)
              (eq (first claim1) (first claim2)))
         (or (equal (second claim1) (second claim2))
             (> (length (intersection claim1 claim2 :test #'equal)) 1)))
        (t nil)))

(defun find-existing-hypothesis-for-claim (claim)
  "Check for semantically similar active hypotheses."
  (maphash (lambda (k v)
             (declare (ignore k))
             (when (and (claims-semantically-similar-p (self-hypothesis-claim v) claim)
                        (member (self-hypothesis-status v) '(:active :confirmed :executed)))
               (return-from find-existing-hypothesis-for-claim v)))
           *hypotheses*)
  nil)

(defun generate-hypothesis! (observation)
  "Generate new theory from observation."
  (let* ((claim-type (classify-observation observation))
         (claim (formulate-claim observation claim-type))
         (existing (find-existing-hypothesis-for-claim claim)))
    (if existing
        (push observation (self-hypothesis-evidence-for existing))
        (let ((hyp (make-self-hypothesis
                    :claim claim :claim-type claim-type
                    :evidence-for (list observation)
                    :proposed-action (propose-action claim claim-type)
                    :created-at *step*)))
          (when (eq claim-type :meta)
            (setf (self-hypothesis-status hyp) :confirmed
                  (self-hypothesis-confidence hyp) 0.9))
          (setf (gethash (self-hypothesis-id hyp) *hypotheses*) hyp)
          (apply-hypothesis-competitive-decay! claim-type)
          hyp))))

;;; --- SECTION 9: HYPOTHESIS TESTING ---

(defun test-hypothesis! (hyp)
  "Test a hypothesis against recent trace evidence. Updates confidence and status."
  (declare (type self-hypothesis hyp))
  (let* ((claim (self-hypothesis-claim hyp))
         (recent (get-traces :n 50))
         (supporting 0)
         (contradicting 0))
    ;; Evaluate traces for evidence (skip non-cognitive-trace entries)
    (dolist (trace recent)
      (when (cognitive-trace-p trace)
        (let ((correct-p (equal (cognitive-trace-prediction trace)
                                (cognitive-trace-actual trace)))
              (relevant-p (hypothesis-relevant-to-trace-p claim trace)))
          (when relevant-p
            (if correct-p
                (incf supporting)
                (incf contradicting))))))
    ;; Update hypothesis
    (incf (self-hypothesis-times-tested hyp))
    (setf (self-hypothesis-last-tested hyp) (if (boundp '*step*) *step* 0))
    (when (> (+ supporting contradicting) 0)
      (let ((ratio (/ (float supporting) (+ supporting contradicting))))
        (when (> supporting 0)
          (incf (self-hypothesis-times-confirmed hyp))
          (push (list :step *step* :support supporting) (self-hypothesis-evidence-for hyp)))
        (when (> contradicting 0)
          (push (list :step *step* :against contradicting) (self-hypothesis-evidence-against hyp)))
        ;; Bayesian-ish confidence update
        (setf (self-hypothesis-confidence hyp)
              (min 1.0 (max 0.0
                (+ (* 0.7 (self-hypothesis-confidence hyp))
                   (* 0.3 ratio)))))))
    ;; Status transitions
    (cond
      ((and (>= (self-hypothesis-times-tested hyp) 3)
            (> (self-hypothesis-confidence hyp) 0.75))
       (setf (self-hypothesis-status hyp) :confirmed))
      ((and (>= (self-hypothesis-times-tested hyp) 3)
            (< (self-hypothesis-confidence hyp) 0.15))
       (setf (self-hypothesis-status hyp) :refuted)
       (remhash (self-hypothesis-id hyp) *hypotheses*)))
    hyp))

(defun hypothesis-relevant-to-trace-p (claim trace)
  "Check if a hypothesis claim is relevant to a given trace."
  (declare (type cognitive-trace trace))
  (cond
    ;; Parameter claims: relevant if context matches
    ((and (listp claim) (eq (first claim) 'parameter-value-affects-performance))
     (let ((ctx (cognitive-trace-context trace)))
       (and (listp ctx) (member (third claim) ctx :test #'equal))))
    ;; Structure claims: broadly relevant
    ((and (listp claim) (eq (first claim) 'program-structure-correlates-with))
     t)
    ;; Meta claims: relevant if meta-level > 0
    ((and (listp claim) (eq (first claim) 'meta-pattern-observed))
     (> (cognitive-trace-meta-level trace) 0))
    ;; Unknown/other: sample 20% of traces
    (t (< (random 1.0) 0.2))))

(defun act-on-confirmed-hypotheses! ()
  "Act on hypotheses that have been confirmed with sufficient evidence."
  (let ((to-act nil))
    (maphash (lambda (id hyp)
               (declare (ignore id))
               (when (and (eq (self-hypothesis-status hyp) :confirmed)
                          (self-hypothesis-proposed-action hyp)
                          (> (self-hypothesis-confidence hyp) 0.7))
                 (push hyp to-act)))
             *hypotheses*)
    (dolist (hyp to-act)
      (let ((action (self-hypothesis-proposed-action hyp)))
        (when (and (listp action) (eq (first action) 'modify-param))
          ;; Parameter modification: adjust learning rates etc.
          (let ((param (second action)))
            (when (and param (boundp param))
              (let ((current (symbol-value param)))
                (when (numberp current)
                  (setf (symbol-value param)
                        (* current (+ 0.9 (random 0.2)))))))))
        (setf (self-hypothesis-status hyp) :executed)))))

;;; --- SECTION 11: VERIFICATION ---
(eval-when (:load-toplevel :execute)
  (format t "[DEEP-MIND-CORE] Foundation loaded.~%"))
