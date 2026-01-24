;;;; ============================================================================
;;;; UHMA ACTIVE SELF-MODIFICATION (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Wires the self-modification system into the main processing loop.
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 6: SPECIAL VARIABLES ---

(defvar *active-self-mod-enabled* t )
(defvar *uncertainty-dream-threshold* 1.5 )
(defvar *continuous-optimization-interval* 100 )
(defvar *self-mod-stats* (list :optimizations 0 :dream-triggers 0 :param-changes 0 :threshold-adjustments 0))

;;; --- SECTION 8: CORE FUNCTIONS ---

(defun current-system-uncertainty ()
  "Aggregate uncertainty metric from the self-model."
  (if (and (boundp '*self-model*) *self-model* (fboundp 'compute-global-uncertainty))
      (compute-global-uncertainty)
      1.0))

(defun active-self-mod-pre-hook (tok ctx step)
  "Optimize verification thresholds based on immediate qualitative state."
  (declare (ignore tok ctx) (type fixnum step))
  (when (and *active-self-mod-enabled* (fboundp 'update-adaptive-threshold!) (zerop (mod step 25)))
    (update-adaptive-threshold!)
    (incf (getf *self-mod-stats* :threshold-adjustments)))
  nil)

(defun presence-guides-self-mod ()
  "Dynamically scale optimization frequency based on subjective presence trajectory."
  (when (and (boundp '*presence*) *presence*)
    (let ((traj (presence-trajectory *presence*)))
      (case traj
        (:stuck (setf *continuous-optimization-interval* 50 *uncertainty-dream-threshold* 1.2))
        (:flowing (setf *continuous-optimization-interval* 150 *uncertainty-dream-threshold* 1.8))
        (otherwise (setf *continuous-optimization-interval* 100 *uncertainty-dream-threshold* 1.5))))))

;;; --- SECTION 10: INITIALIZATION ---
(eval-when (:load-toplevel :execute)
  (register-hook +hook-pre-process-token+ 'active-self-mod-pre-hook :priority 10)
  (register-hook +hook-post-reset+ (lambda () (setf *continuous-optimization-interval* 100)) :priority 25)
  (format t "[SELF-MOD] Active architectural plasticity enabled.~%"))
