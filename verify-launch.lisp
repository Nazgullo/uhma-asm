;;;; ============================================================================
;;;; UHMA GENESIS SCRIPT: VERIFY-LAUNCH
;;;; ============================================================================
;;;; This script provides a clean, minimal test to verify that the core
;;;; learning loop is functional after the recent architectural refactor.
;;;;
;;;; It performs the following steps:
;;;;   1. Loads the complete UHMA system.
;;;;   2. Resets the system to a clean state.
;;;;   3. Feeds the system its own most fundamental source file.
;;;;   4. Prints a status report to verify that learning occurred.
;;;;
;;;; A successful run of this script is the definitive gate for passing the
;;;; "Bible v3.0" refactoring milestone.
;;;; ============================================================================

(format t "--- VERIFY-LAUNCH: Starting Genesis Test ---~%")

;;; 1. Load the system
(handler-case
    (load "uhma-load-only.lisp")
  (error (e)
    (format t "FATAL: Failed to load UHMA system: ~A~%" e)
    (sb-ext:exit :code 1)))

(in-package :uhma)

;;; 2. Reset to a clean slate
(format t "--- VERIFY-LAUNCH: Resetting system state... ---~%")
(handler-case
    (reset!)
  (error (e)
    (format t "FATAL: Failed during reset!: ~A~%" e)
    (sb-ext:exit :code 1)))

;;; 3. Feed the system its own core source code
(let ((core-file "uhma-v6.1-core-homoiconic.lisp"))
  (format t "--- VERIFY-LAUNCH: Feeding core file '~A'... ---~%" core-file)
  (unless (probe-file core-file)
    (format t "FATAL: Core file not found: ~A~%" core-file)
    (sb-ext:exit :code 1))
  (handler-case
      (process-file! core-file :verbose nil)
    (error (e)
      (format t "FATAL: Error during file processing: ~A~%" e)
      (sb-ext:exit :code 1))))

;;; 4. Print status report to verify learning
(format t "--- VERIFY-LAUNCH: Genesis complete. Verifying state... ---~%")

(let ((expert-count (length *experts*))
      (holo-patterns (if (and (boundp '*holo*) *holo*)
                         (hash-table-count (holo-store-patterns *holo*))
                         0)))
  (format t "  - Experts: ~D~%" expert-count)
  (format t "  - Holographic Patterns: ~D~%" holo-patterns)

  (if (and (> expert-count *initial-experts*)
           (> holo-patterns 0))
      (progn
        (format t "SUCCESS: System appears to be learning.~%")
        (sb-ext:exit :code 0))
      (progn
        (format t "FAILURE: System did not demonstrate learning.~%")
        (sb-ext:exit :code 1))))
