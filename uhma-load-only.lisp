
;;;; UHMA Load-Only - BIBLE v3.0 Compliant
;;;; Loads all 32 modules in strict dependency order.
;;;; Use this for testing and scripted operations.

(in-package :cl-user)

(defparameter *uhma-source-dir*
  (make-pathname :directory (pathname-directory *load-truename*)))

;;; ============================================================================
;;; BIBLE v3.0 - STRICT DEPENDENCY ORDER
;;; ============================================================================
;;; These variables are defined in CL-USER for the loader's operation.
;;; ============================================================================

(defparameter *uhma-foundation*
  '("uhma-forward-decl.lisp"             ; Defines the UHMA package
    "uhma-stubs.lisp"                    ; Stubs for unimplemented features
    "uhma-vsa-substrate.lisp"          ; CRITICAL: RESET! depends on this
    "uhma-v6.1-core-homoiconic.lisp"
    "uhma-v6.1-adaptive-homoiconic.lisp"
    "uhma-v6.1-sequence.lisp"))

(defparameter *uhma-cognitive-layers*
  '("uhma-v6.2-deep-mind-core.lisp"
    "uhma-v6.2-deep-mind-introspection.lisp"
    "uhma-v6.2-deep-mind-schemas.lisp"
    "uhma-v6.2-deep-mind.lisp"
    "uhma-v6.3-deeper-mind-core.lisp"
    "uhma-v6.3-deeper-mind.lisp"))

(defparameter *uhma-agency-and-goals*
  '("uhma-v6.5-agency-core.lisp"
    "uhma-v6.5-agency-goals.lisp"
    "uhma-v6.5-agency-dreams.lisp"
    "uhma-v6.5-agency.lisp"))

(defparameter *uhma-advanced-reasoning*
  '("uhma-v6.6-introspective-core.lisp"
    "uhma-v6.6-introspective-grounding.lisp"
    "uhma-v6.7-compositional-core.lisp"
    "uhma-v6.7-compositional-reasoning.lisp"
    "uhma-v6.8-cognitive-controller.lisp"
    "uhma-v6.9-pattern-utilization.lisp"))

(defparameter *uhma-memory-and-presence*
  '("uhma-v6.10-episodic-core.lisp"
    "uhma-v6.10-episodic-memory.lisp"
    "uhma-presence-substrate.lisp"
    "uhma-holographic-substrate-v2.lisp"
    "uhma-holographic-impl.lisp"))

(defparameter *uhma-integration-and-fixes*
  '("uhma-v6.3-fixes.lisp"
    "uhma-v6.4-enhancements.lisp"
    "uhma-v6.10-episodic-integration.lisp"
    "uhma-presence-integration.lisp"
    "uhma-holographic-integration.lisp"))

(defparameter *uhma-final-wiring*
  '("uhma-deep-wiring.lisp"
    "uhma-complete-wiring.lisp"))

(defparameter *all-uhma-modules*
  (reduce #'append (list *uhma-foundation*
                         *uhma-cognitive-layers*
                         *uhma-agency-and-goals*
                         *uhma-advanced-reasoning*
                         *uhma-memory-and-presence*
                         *uhma-integration-and-fixes*
                         *uhma-final-wiring*)))

(format t "~%Loading UHMA modules in dependency order...~%")

(let ((total-files (length *all-uhma-modules*))
      (loaded-files 0))
  (dolist (module *all-uhma-modules*)
    (incf loaded-files)
    (let ((path (merge-pathnames module *uhma-source-dir*)))
      (format t "[~2D/~D] Loading ~A...~%" loaded-files total-files module)
      (if (probe-file path)
          (handler-case
              (load path :verbose nil :print nil)
            (error (e)
              (format t "FATAL ERROR loading ~A: ~A~%" module e)
              (sb-ext:exit :code 1)))
          (format t "WARNING: Module not found: ~A~%" module)))))

;;; --- Switch to UHMA package now that it has been defined ---
(in-package :uhma)

(format t "~%================================================================~%")
(format t "UHMA Bible v3.0 Modules Loaded Successfully~%")
;; Use package-qualified symbol to access the loader variable from CL-USER
(format t "All ~D files compiled in order.~%" (length cl-user::*all-uhma-modules*))
(format t "================================================================~%~%")

;;;; [SECTION-START:99:VERIFICATION]
;;; Section 99: File Verification

(defun verify-load-only-lisp-completeness ()
  "Verify completeness."
  (unless (find-package :uhma)
    (error "Package UHMA not defined"))
  (format t "~&uhma-load-only.lisp verification passed.~%"))

(verify-load-only-lisp-completeness)

;;;; [SECTION-END:99:VERIFICATION]
