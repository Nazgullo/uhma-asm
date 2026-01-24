;;;; ============================================================================
;;;; UHMA AGENCY - GOALS & DRIVES (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Implements intrinsic drives and the goal stack architecture.
;;;; DEPENDS ON: uhma-v6.5-agency-core.lisp
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 4: STRUCTURES ---

;;; Structs moved to forward-decl.lisp

;;; --- SECTION 6: SPECIAL VARIABLES ---

(defvar *goal-stack* nil)
(defvar *intrinsic-drives* nil)
(defvar *current-goal* nil)
(defvar *system-purpose* "Self-observation and pattern emergence.")
(defvar *vocation-fidelity* 1.0 )

;;; --- SECTION 8: CORE FUNCTIONS ---

(defun update-drives! ()
  "Recalculate drive pressures based on system state."
  (update-system-purpose!)
  (dolist (d *intrinsic-drives*)
    (let ((lvl (compute-drive-level (drive-name d))))
      (when lvl
        (setf (drive-current-level d) (+ (* 0.8 (drive-current-level d)) (* 0.2 lvl))))
      (when (> (drive-current-level d) (drive-target-level d))
        (maybe-generate-goal-from-drive! d))))
  (run-hook +hook-post-drive-update+ *intrinsic-drives*))

(defun initialize-drives! ()
  "Initialize intrinsic drives."
  (setf *intrinsic-drives*
        (list (make-drive :name :curiosity 
                          :target-level 0.5 
                          :current-level 0.5 
                          :increase-rate 0.05
                          :decrease-rate 0.1)
              (make-drive :name :competence 
                          :target-level 0.8 
                          :current-level 0.5 
                          :increase-rate 0.01
                          :decrease-rate 0.1)
              (make-drive :name :efficiency 
                          :target-level 0.9 
                          :current-level 0.5 
                          :increase-rate 0.02
                          :decrease-rate 0.1)
              (make-drive :name :teleology 
                          :target-level 1.0 
                          :current-level 0.5 
                          :increase-rate 0.005
                          :decrease-rate 0.05))))

(defun pursue-goals! ()
  "Execute strategies to satisfy active goals."
  (let ((g (first *goal-stack*)))
    (when g
      (setf *current-goal* g)
      (let ((strategy (select-strategy-for-goal g)))
        (when strategy
          (let ((res (apply-goal-strategy strategy g)))
            (push (list :step *step* :strategy strategy :result res) (goal-attempts g))))))))

(defun maybe-generate-goal-from-drive! (drive)
  "Generate a goal if drive pressure exceeds threshold."
  (let ((pressure (- (drive-current-level drive) (drive-target-level drive))))
    (when (> pressure 0.2)
      (let ((goal (make-goal
                    :type :drive-generated
                    :description (format nil "Satisfy ~A drive" (drive-name drive))
                    :priority (min 1.0 pressure)
                    :drive-source (drive-name drive)
                    :created-at (if (boundp '*step*) *step* 0))))
        (push goal *goal-stack*)))))

(defun select-strategy-for-goal (goal)
  "Select an execution strategy appropriate for a goal."
  (case (goal-drive-source goal)
    (:curiosity :exploratory)
    (:competence :deliberative)
    (:efficiency :fast-default)
    (t :multi-expert)))

(defun apply-goal-strategy (strategy goal)
  "Apply a strategy to pursue a goal. Returns outcome keyword."
  (declare (ignore strategy goal))
  :in-progress)

(defun satisfy-drive! (drive-name amount)
  "Decrease a drive's current level by amount (satisfaction reduces pressure)."
  (declare (type symbol drive-name) (type single-float amount))
  (dolist (d *intrinsic-drives*)
    (when (eq (drive-name d) drive-name)
      (setf (drive-current-level d)
            (max 0.0 (- (drive-current-level d) amount)))
      (return d))))

(defun push-goal! (goal)
  "Push a goal onto the goal stack, maintaining priority order."
  (push goal *goal-stack*)
  (setf *goal-stack* (sort *goal-stack* #'> :key #'goal-priority))
  goal)

;;; --- SECTION 11: VERIFICATION ---
(eval-when (:load-toplevel :execute)
  (format t "[AGENCY-GOALS] Drives and Stack active.~%"))
