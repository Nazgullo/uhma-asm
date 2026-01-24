;;;; ============================================================================
;;;; UHMA MASTER ORCHESTRATOR (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Single entry point for the entire 1024-dimensional Software Mind.
;;;; ============================================================================

(in-package :cl-user)

;;; --- SECTION 1: MODULE LOADING ---

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :uiop))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *uhma-manifest*
    '("uhma-forward-decl.lisp"
      "uhma-vsa-substrate.lisp"
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
      "uhma-presence-substrate.lisp"
      "uhma-presence-integration.lisp"
      "uhma-active-self-modification.lisp"
      "uhma-goal-driven-generation.lisp"
      "uhma-deep-wiring.lisp"
      "uhma-complete-wiring.lisp"
      "uhma-self-awareness-loop.lisp"
      "uhma-holographic-substrate-v2.lisp"
      "uhma-holographic-convergence.lisp"
      "uhma-state-persistence.lisp"
      "uhma-memory-bounds.lisp"
      "uhma-diagnostic.lisp"
      "uhma-continuous.lisp"))
  (dolist (file *uhma-manifest*)
    (let ((path (merge-pathnames file (uiop:getcwd))))
      (if (probe-file path)
          (load path)
          (format t "⚠ Missing module: ~A~%" file)))))

(in-package :uhma)

;;; --- SECTION 8: INITIALIZATION ---

(defun initialize-system! ()
  "Orchestrate initialization of all cognitive layers in strict order."
  (format t "~%[MASTER] Synchronizing Mind-Space...~%")
  (reset!)
  (initialize-presence-integration!)
  (initialize-cognitive-control!)
  (initialize-v65-agentic!)
  (initialize-introspective-grounding!)
  (initialize-compositional-reasoning!)
  (initialize-pattern-utilization!)
  (initialize-episodic-memory!)
  (initialize-deeper-mind!)
  (initialize-v64-enhancements!)
  (initialize-episodic-memory!)
  (initialize-deeper-mind!)
  (initialize-v64-enhancements!)
  (install-deep-wiring!)
  (install-complete-wiring!)
  (install-self-awareness-loop!)
  (format t "═══ UHMA v3.0 MIND-SPACE FULLY SYNCHRONIZED ═══~%"))

;;; --- SECTION 10: ENTRY POINT ---
(eval-when (:load-toplevel :execute)
  (initialize-system!)
  (format t "Type (live!) to begin the 14GB autonomous session.~%"))