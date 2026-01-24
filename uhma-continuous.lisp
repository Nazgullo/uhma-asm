;;;; ============================================================================
;;;; UHMA CONTINUOUS OPERATION (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; The system doesn't stop when input stops. Presence continues, drives build.
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 6: SPECIAL VARIABLES ---

(defvar *internal-step-count* 0 )
(defvar *continuous-running* nil )
(defvar *continuous-stop-requested* nil )
(defvar *phase* :settling )
(defvar *phase-step* 0 )
(defvar *equilibrium-threshold* 0.02 )
(defvar *state-history* nil )

;;; --- SECTION 8: CORE FUNCTIONS ---

(defun snapshot-state ()
  "Gather system metrics for phase transition analysis."
  (list :step *step*
        :experts (length *experts*)
        :hypotheses (hash-table-count *hypotheses*)
        :schemas (hash-table-count *cognitive-schemas*)
        :goals (length *goal-stack*)))

(defun internal-step! ()
  "Run one cycle of autonomous cognition."
  (declare (optimize (speed 2) (safety 2)))
  (incf *step*) (incf *internal-step-count*) (incf *phase-step*)
  
  ;; Presence substrate continues
  (presence-fade-textures! 0.98)
  (presence-heal-continuity! 0.01)
  
  ;; Cognitive maintenance based on phase
  (case *phase*
    (:settling 
     (run-consolidation!) 
     (when (should-dream-p) (run-dream-cycle!)))
    (:exploring 
     (introspect! 2) 
     (run-pending-experiments!)))
  
  (update-drives!) 
  (pursue-goals!)
  (run-hook +hook-maintenance+))

(defun live! (&key (report-interval 100))
  "Enter autonomous mode loop. Safe interrupt handling via handler-case."
  (declare (type fixnum report-interval))
  (setf *continuous-running* t *continuous-stop-requested* nil *phase* :settling)
  (format t "~%═══ SYSTEM IS ALIVE (Bible v3.0 Mode) ═══~%")
  (unwind-protect
      (handler-case
          (loop until *continuous-stop-requested*
                do (internal-step!)
                   (when (zerop (mod *internal-step-count* report-interval)) 
                     (report-state)))
        (#+sbcl sb-sys:interactive-interrupt () (format t "~%Interrupted by user.~%")))
    (setf *continuous-running* nil)))

(defun stop! ()
  "Gracefully signal the continuous loop to terminate."
  (setf *continuous-stop-requested* t))

(defun report-state ()
  "Print a summary of the system's current mental state."
  (format t "~%[Step ~D | Phase: ~A | Internal: ~D]~%" *step* *phase* *internal-step-count*)
  (format t "  Experts: ~D  Purpose: ~A~%" (length *experts*) *system-purpose*)
  (format t "  Presence: ~A~%" (presence-summary)))

;;; --- SECTION 10: INITIALIZATION ---
(eval-when (:load-toplevel :execute)
  (format t "[CONTINUOUS] Autonomous Cognition loop ready.~%"))