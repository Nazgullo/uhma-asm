;;;;; ============================================================================
;;;; DEBUG CONFIDENCE SCRIPT
;;;; ============================================================================
;;;; This script is for debugging the SINGLE-FLOAT type error.
;;;; It loads the core system, resets it, and then calls a simplified
;;;; process-token function with extensive debugging output.
;;;; ============================================================================

(format t "--- DEBUG-CONFIDENCE: Starting Debug Test ---~%")

;;; 1. Load the system
(handler-case
    (load "uhma-load-only.lisp")
  (error (e)
    (format t "FATAL: Failed to load UHMA system: ~A~%" e)
    (sb-ext:exit :code 1)))

(in-package :uhma)

;;; 2. Reset to a clean slate
(format t "--- DEBUG-CONFIDENCE: Resetting system state... ---~%")
(handler-case
    (reset!)
  (error (e)
    (format t "FATAL: Failed during reset!: ~A~%" e)
    (sb-ext:exit :code 1)))

;;; 3. Simplified process-token function with debug prints
(defun debug-process-token! (token context)
  (let* ((var-ctx (cons token context))
         (predicted-val nil)
         (confidence-val 0.0) ;; Ensure this is float from the start
         (got-it nil))
    (format t "DEBUG: Entering debug-process-token!\n")
    (format t "DEBUG: Token: ~A, Context: ~A\n" token context)

    (multiple-value-bind (predicted new-confidence)
        (generate-prediction var-ctx)
      (setf predicted-val predicted)
      (setf confidence-val (or new-confidence 0.0)) ;; Still ensure float here
      
      (format t "DEBUG: After generate-prediction: predicted=~A, new-confidence=~A (~A), confidence-val=~A (~A)\n"
              predicted predicted (type-of predicted) confidence-val (type-of confidence-val)))

    (when (and predicted-val (eql predicted-val token))
      (setf got-it t))

    (format t "DEBUG: Final confidence-val before binding: ~A (~A)\n" confidence-val (type-of confidence-val))
    (list :token token :predicted predicted-val :confidence confidence-val :correct got-it)))

;;; 4. Call the debug function with sample input
(format t "--- DEBUG-CONFIDENCE: Calling debug-process-token! ---~%")
(let ((result (debug-process-token! 'CAT '('THE 'MAT))))
  (format t "--- DEBUG-CONFIDENCE: Debug result: ~A ---~%" result))

(format t "--- DEBUG-CONFIDENCE: Test complete. ---~%")

(sb-ext:exit :code 0)
