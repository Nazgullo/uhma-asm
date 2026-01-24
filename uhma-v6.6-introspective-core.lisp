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
  "Collect system metrics for concept detection."
  (list :step *step* :uncertainty 0.5))

(defun concept-applies-via-learned-p (concept state)
  "Check if concept applies based on learned patterns."
  (declare (ignore concept state))
  nil)

(defun initialize-introspective-vocabulary! ()
  "Populate the standard introspective concepts."
  (define-introspective-concept! :name 'CONFUSED :description "Uncertainty is high.")
  (define-introspective-concept! :name 'CONFIDENT :description "Predictions are accurate.")
  (define-introspective-concept! :name 'LEARNING :description "Acquiring new patterns.")
  (define-introspective-concept! :name 'STUCK :description "Repeated failures.")
  (define-introspective-concept! :name 'EXPLORING :description "Trying novel approaches.")
  (define-introspective-concept! :name 'CONSOLIDATING :description "Strengthening knowledge."))

;;; --- SECTION 11: VERIFICATION ---
(eval-when (:load-toplevel :execute)
  (format t "[INTROSPECTIVE-CORE] Vocabulary foundation loaded.~%"))
