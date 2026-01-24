;;;; ============================================================================
;;;; UHMA DEEPER MIND - CORE (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Provides self-expectation tracking and meta-confidence models.
;;;; DEPENDS ON: uhma-v6.2-deep-mind.lisp
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 4: STRUCTURES ---

(defstruct self-expectation
  "A prediction about the system's own future behavior."
  (step 0 :type fixnum)
  (context nil :type list)
  (expected-schema nil :type (or null symbol))
  (expected-ops nil :type list)
  (expected-expert nil :type (or null symbol))
  (expected-confidence 0.5 :type single-float)
  (expected-outcome :uncertain :type symbol)
  (actual-schema nil :type (or null symbol))
  (actual-ops nil :type list)
  (actual-expert nil :type (or null symbol))
  (actual-confidence 0.5 :type single-float)
  (actual-outcome nil :type (or null symbol))
  (self-surprise 0.0 :type single-float)
  (outcome-surprise 0.0 :type single-float)
  (self-prediction-correct nil :type boolean))

(defstruct self-doubt-model
  "Meta-confidence tracking for introspection reliability."
  (introspection-confidence 0.5 :type single-float)
  (track-record nil :type list)
  (blind-spot-hypotheses nil :type list)
  (model-staleness 0.0 :type single-float)
  (last-model-update 0 :type fixnum)
  (revision-triggered nil :type boolean))

;;; --- SECTION 6: SPECIAL VARIABLES ---

(defvar *self-expectations* (make-array 200 :fill-pointer 0 :adjustable t))
(defvar *self-expectation-accuracy* 0.5)
(defvar *current-self-expectation* nil)
(defvar *self-doubt* (make-self-doubt-model))

;;; --- SECTION 8: CORE FUNCTIONS ---

(defun predict-own-behavior (ctx)
  "Generate a pre-execution model of internal behavior."
  (declare (type list ctx))
  (let ((exp (make-self-expectation :step *step* :context ctx :expected-confidence (predict-confidence ctx))))
    (run-hook +hook-self-expectation-created+ exp)
    exp))

(defun compute-self-divergence (exp)
  "Calculate qualitative delta between expected and actual internal state."
  (declare (type self-expectation exp))
  (let ((divergence 0.0) (factors 0))
    (incf factors)
    (incf divergence (abs (- (self-expectation-expected-confidence exp) (self-expectation-actual-confidence exp))))
    (if (zerop factors) 0.0 (/ divergence factors))))

(defun predict-confidence (ctx)
  "Predict confidence level for a given context based on history."
  (declare (type list ctx))
  (if (and (boundp '*trace-buffer*) (> (fill-pointer *trace-buffer*) 0))
      (let ((relevant 0) (correct 0))
        (loop for i from (1- (fill-pointer *trace-buffer*)) downto (max 0 (- (fill-pointer *trace-buffer*) 30))
              for trace = (aref *trace-buffer* i)
              when (and (listp (cognitive-trace-context trace))
                        (intersection ctx (cognitive-trace-context trace) :test #'equal))
              do (incf relevant)
                 (when (equal (cognitive-trace-prediction trace) (cognitive-trace-actual trace))
                   (incf correct)))
        (if (> relevant 0) (/ (float correct) relevant) 0.5))
      0.5))

(defun update-self-doubt! ()
  "Update the self-doubt model based on recent self-prediction accuracy."
  (let ((recent-accuracy (if (and (boundp '*trace-buffer*) (> (fill-pointer *trace-buffer*) 10))
                             (let ((correct 0) (total (min 20 (fill-pointer *trace-buffer*))))
                               (loop for i from (1- (fill-pointer *trace-buffer*))
                                     downto (max 0 (- (fill-pointer *trace-buffer*) total))
                                     for trace = (aref *trace-buffer* i)
                                     when (equal (cognitive-trace-prediction trace)
                                                 (cognitive-trace-actual trace))
                                     do (incf correct))
                               (/ (float correct) total))
                             0.5)))
    ;; Update introspection confidence: if we're predicting well, doubt decreases
    (setf (self-doubt-model-introspection-confidence *self-doubt*)
          (+ (* 0.8 (self-doubt-model-introspection-confidence *self-doubt*))
             (* 0.2 recent-accuracy)))
    ;; Track staleness
    (setf (self-doubt-model-model-staleness *self-doubt*)
          (min 1.0 (/ (float (- *step* (self-doubt-model-last-model-update *self-doubt*))) 500.0)))
    ;; Push to track record
    (push (list :step *step* :accuracy recent-accuracy)
          (self-doubt-model-track-record *self-doubt*))
    (when (> (length (self-doubt-model-track-record *self-doubt*)) 20)
      (setf (self-doubt-model-track-record *self-doubt*)
            (subseq (self-doubt-model-track-record *self-doubt*) 0 20)))
    (setf (self-doubt-model-last-model-update *self-doubt*) *step*)))

(defun get-observable-names ()
  "Get names of observable system variables for hypothesis invention."
  (let ((names nil))
    (when (boundp '*intrinsic-drives*)
      (dolist (d *intrinsic-drives*)
        (push (drive-name d) names)))
    (when (and (boundp '*trace-buffer*) (> (fill-pointer *trace-buffer*) 0))
      (push :prediction-accuracy names)
      (push :surprise-level names))
    (when (boundp '*cognitive-schemas*)
      (push :schema-count names))
    names))

;;; --- SECTION 11: VERIFICATION ---
(eval-when (:load-toplevel :execute)
  (format t "[DEEPER-MIND-CORE] Self-expectation foundation loaded.~%"))
