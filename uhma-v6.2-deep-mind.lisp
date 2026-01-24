;;;; ============================================================================
;;;; UHMA DEEP MIND - INTEGRATION LOADER (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Orchestrates Deep Mind sub-modules and provides integration hooks.
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 1: MODULE LOADING ---
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((dir (make-pathname :directory (pathname-directory *load-truename*))))
    (dolist (m '("uhma-v6.2-deep-mind-core.lisp"
                 "uhma-v6.2-deep-mind-introspection.lisp"
                 "uhma-v6.2-deep-mind-schemas.lisp"))
      (load (merge-pathnames m dir)))))

;;; --- SECTION 2: INTEGRATION HELPERS ---

(defun classify-observation (obs)
  "Map raw observation to theoretical claim type."
  (cond ((and (consp obs) (eq (car obs) :param-effect)) :parameter)
        ((and (consp obs) (eq (car obs) :structure-effect)) :structure)
        ((and (consp obs) (eq (car obs) :meta-pattern)) :meta)
        (t :unknown)))

(defun formulate-claim (obs type)
  "Convert observation to S-expression claim."
  (case type
    (:parameter `(parameter-value-affects-performance ,(second obs) ,(third obs) ,(fourth obs)))
    (:structure `(program-structure-correlates-with ,(second obs) ,(third obs)))
    (:meta `(meta-pattern-observed ,(cdr obs)))
    (t `(unknown-pattern ,obs))))

(defun propose-action (claim type)
  "Propose behavioral change based on claim."
  (case type
    (:parameter `(modify-param ,(second claim) ,(third claim) ,(fourth claim)))
    (:structure `(prefer-structure ,(second claim)))
    (:meta `(adjust-meta-parameters ,claim))
    (t nil)))

(defun apply-hypothesis-competitive-decay! (type)
  "Enforce competitive pressure on hypothesis types."
  (let ((hyps nil))
    (maphash (lambda (k v) (declare (ignore k))
               (when (and (eq (self-hypothesis-claim-type v) type) (eq (self-hypothesis-status v) :active))
                 (push v hyps)))
             *hypotheses*)
    (when (> (length hyps) 10)
      (setf hyps (sort hyps #'> :key (lambda (h) (self-hypothesis-confidence h))))
      (dolist (h (nthcdr 3 hyps))
        (decf (self-hypothesis-confidence h) 0.05)
        (when (< (self-hypothesis-confidence h) 0.1)
          (setf (self-hypothesis-status h) :superseded)
          (remhash (self-hypothesis-id h) *hypotheses*))))))

;;; --- SECTION 3: MAIN INTEGRATION HOOKS ---

(defun deep-mind-post-process-hook (tok ctx predicted got-it)
  "Unified hook for tracing, schemas, and surprises."
  (let* ((expert *last-answering-expert*)
         (expert-id (when (and expert (typep expert 'expert)) (expert-id expert)))
         (confidence (if (and expert (typep expert 'expert)) 
                         (/ (float (expert-hits expert)) (max 1 (+ (expert-hits expert) (expert-misses expert))))
                         0.5))
         (reasoning-path (when (boundp '*execution-trace*) (reverse *execution-trace*)))
         (trace (make-cognitive-trace
                 :step *step* :context ctx :prediction predicted :actual tok
                 :confidence confidence :surprise (if got-it 0.0 1.0)
                 :expert-id expert-id :reasoning-path reasoning-path
                 :epistemic-state (compute-epistemic-state ctx predicted confidence))))
    (record-trace! trace)
    (when (and got-it reasoning-path) (extract-schema-from-trace trace))
    (record-surprise! trace)
    (correlate-context-with-source! ctx got-it)
    ;; Wire failed predictions into dream buffer (organic — dreams from need)
    (unless got-it
      (when (fboundp 'record-dream-candidate!)
        (record-dream-candidate! ctx predicted tok (cognitive-trace-surprise trace)))
      ;; Wire counterfactual reasoning on failures
      (when (fboundp 'run-counterfactual-on-trace!)
        (run-counterfactual-on-trace! trace)))))

(defun deep-mind-maintenance-hook ()
  "Interference-driven self-examination. No timers — the NN's own noise triggers action."
  (let ((interference (if (fboundp 'compute-holographic-interference)
                          (compute-holographic-interference)
                          0.0)))
    ;; Introspect when holographic patterns are noisy (interference high)
    (when (> interference 0.6) (introspect! 1))
    ;; Deep introspect + schema evolution when very noisy
    (when (> interference 0.85) (introspect! 2) (evolve-schemas!)))
  ;; Test hypotheses when concept detection signals problems
  (when (intersection *cached-active-concepts* '(STUCK CONFUSED))
    (maphash (lambda (id hyp) (declare (ignore id))
               (when (eq (self-hypothesis-status hyp) :active) (test-hypothesis! hyp)))
             *hypotheses*)
    (act-on-confirmed-hypotheses!)))

;;; --- SECTION 10: INITIALIZATION ---
(eval-when (:load-toplevel :execute)
  (register-hook +hook-post-process-token+ 'deep-mind-post-process-hook :priority 60)
  (register-hook +hook-maintenance+ 'deep-mind-maintenance-hook :priority 60)
  (format t "[DEEP-MIND] Fully integrated and active.~%"))
