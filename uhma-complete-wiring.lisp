;;;; ============================================================================
;;;; UHMA COMPLETE WIRING (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Connects ALL systems via need-based triggers and qualitative hooks.
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 6: SPECIAL VARIABLES ---

(defvar *complete-wiring-installed* nil )
(defvar *schema-pattern-count* 0 )
(defvar *schema-compile-threshold* 500 )
(defvar *introspection-pressure* 0.0 )

;;; --- SECTION 8: WIRING LOGIC ---

(defun schema-compilation-needed-p ()
  "Check if accumulated patterns warrant structural compilation."
  (and (> *schema-pattern-count* *schema-compile-threshold*) (> (hash-table-count *pattern-stats*) 50)))

(defun introspection-pressure-hook (tok ctx predicted got-it)
  "Update introspection need based on prediction accuracy."
  (declare (ignore tok ctx predicted))
  (if got-it (setf *introspection-pressure* (max 0.0 (- *introspection-pressure* 0.1))) (incf *introspection-pressure* 0.3))
  (when (> *introspection-pressure* 1.0) (introspect! 2) (setf *introspection-pressure* 0.0)))

(defun install-complete-wiring! ()
  "Initialize final orchestrations and hooks."
  (register-hook +hook-post-process-token+ 'introspection-pressure-hook :priority 75)
  (setf *complete-wiring-installed* t))

;;; --- SECTION 10: INITIALIZATION ---
(eval-when (:load-toplevel :execute)
  (install-complete-wiring!)
  (format t "[COMPLETE-WIRING] Final system orchestrations active.~%"))

(defun uninstall-complete-wiring! ()
  "Remove complete wiring."
  (format t "~%Removing complete wiring...~%")
  ;; Note: Would need to track registered hooks to properly unregister
  (setf *complete-wiring-installed* nil)
  (format t "Done. (Some hooks may still be registered)~%"))

;;; ============================================================================
;;; AUTO-INSTALL ON LOAD
;;; ============================================================================

(install-complete-wiring!)
