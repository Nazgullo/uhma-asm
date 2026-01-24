;;;; ============================================================================
;;;; UHMA INTROSPECTIVE GROUNDING - LOADER & INTEGRATION (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Orchestrates the vocabulary and provides narrative self-description.
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 1: MODULE LOADING ---
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((dir (make-pathname :directory (pathname-directory *load-truename*))))
    (load (merge-pathnames "uhma-v6.6-introspective-core.lisp" dir))))

;;; --- SECTION 8: INTEGRATION LOGIC ---

(defun detect-active-concepts ()
  "Detect current qualitative state using seeds and learned thresholds."
  (when (and (boundp '*step*) (= *last-concept-detection-step* *step*))
    (return-from detect-active-concepts *cached-active-concepts*))
  (let ((state (gather-current-state)) (active nil))
    (maphash (lambda (name concept)
               (when (or (and (introspective-concept-detection-fn concept)
                              (ignore-errors (funcall (introspective-concept-detection-fn concept) state)))
                         (concept-applies-via-learned-p concept state))
                 (push name active)
                 (incf (introspective-concept-activation-count concept))))
             *introspective-vocabulary*)
    (setf *last-concept-detection-step* *step*
          *cached-active-concepts* active)
    (run-hook +hook-post-concept-detection+ active state)
    active))

(defun describe-current-state ()
  "High-resolution qualitative self-report."
  (let* ((concepts (detect-active-concepts))
         (state (gather-current-state))
         (expert *last-answering-expert*)
         (spectrum (when (and expert (fboundp 'vsa-explain-vector))
                     (vsa-explain-vector (expert-knowledge-vector expert)))))
    (record-concept-activation! concepts)
    (with-output-to-string (s)
      (format s "I am currently: ~{~A~^, ~}. " (mapcar (lambda (c) (string-downcase (symbol-name c))) (or concepts '(:neutral))))
      (when spectrum
        (format s "~%Internal resonance: ")
        (dolist (pair spectrum) (format s "~A (~,0F%), " (car pair) (* 100 (cdr pair))))))))

(defun test-grounded-hypotheses! ()
  "Test active grounded hypotheses against recent concept activations."
  (dolist (hyp *grounded-hypotheses*)
    (when (eq (grounded-hypothesis-status hyp) :active)
      (let ((if-active (every (lambda (c) (member c *cached-active-concepts*))
                              (grounded-hypothesis-if-concepts hyp))))
        (incf (grounded-hypothesis-tests hyp))
        (if (and if-active (some (lambda (c) (member c *cached-active-concepts*))
                                 (grounded-hypothesis-then-concepts hyp)))
            (incf (grounded-hypothesis-support hyp))
            (when if-active (incf (grounded-hypothesis-opposition hyp))))))))

;;; --- SECTION 9: INTEGRATION HOOKS ---

(defun introspective-grounding-hook (actual ctx predicted got-it)
  "Periodic qualitative assessment."
  (declare (ignore actual ctx predicted got-it))
  (incf *concept-detection-counter*)
  (when (zerop (mod *concept-detection-counter* *concept-detection-sample-rate*))
    (record-concept-activation! (detect-active-concepts))
    (test-grounded-hypotheses!)))

(defun initialize-introspective-grounding! ()
  "Initialize the introspective grounding module."
  (initialize-introspective-vocabulary!)
  (register-hook +hook-post-process-token+ 'introspective-grounding-hook)
  (format t "[INTROSPECTIVE] Grounding hooks active.~%"))

;;; --- SECTION 10: INITIALIZATION ---
(eval-when (:load-toplevel :execute)
  (initialize-introspective-vocabulary!)
  (register-hook +hook-post-process-token+ 'introspective-grounding-hook)
  (format t "[INTROSPECTIVE] Grounding hooks active.~%"))