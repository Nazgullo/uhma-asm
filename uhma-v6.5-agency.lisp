;;;; ============================================================================
;;;; UHMA AGENCY - INTEGRATION LOADER (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Orchestrates Agency sub-modules and provides global integration hooks.
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 1: MODULE LOADING ---
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((dir (make-pathname :directory (pathname-directory *load-truename*))))
    (dolist (m '("uhma-v6.5-agency-core.lisp"
                 "uhma-v6.5-agency-goals.lisp"
                 "uhma-v6.5-agency-dreams.lisp"))
      (load (merge-pathnames m dir)))))

;;; --- SECTION 8: INTEGRATION LOGIC ---

(defun execute-vibe (domain)
  "Compute alignment between current behavior and a domain's ideal.
   Returns 0.0-1.0 representing felt resonance."
  (case domain
    (:teleology
     ;; How aligned are we with our purpose?
     (let ((acc (if (fboundp 'compute-recent-accuracy) (compute-recent-accuracy 50) 0.5))
           (hyp-count (if (boundp '*hypotheses*) (hash-table-count *hypotheses*) 0)))
       (min 1.0 (+ (* 0.6 acc) (* 0.4 (min 1.0 (/ hyp-count 10.0)))))))
    (t 0.5)))

(defun update-system-purpose! ()
  "Refine system mission based on history and fidelity."
  (let* ((recent-acc (compute-recent-accuracy 100))
         (vibe-match (execute-vibe :teleology)))
    (setf *vocation-fidelity* (+ (* 0.7 *vocation-fidelity*) (* 0.3 (if (> recent-acc 0.4) vibe-match (* vibe-match 0.5)))))
    (when (< *vocation-fidelity* 0.2)
      (setf *system-purpose* "Searching for new meaning..."
            *vocation-fidelity* 0.5))))

(defun compute-drive-level (drive-name)
  "Calculate specific drive pressure modulated by presence."
  (let ((base (case drive-name
                (:curiosity 0.3)
                (:competence (if (boundp '*external-outcomes*) 0.5 0.3))
                (:efficiency (max (get-heap-saturation) 0.2))
                (:teleology (- 1.0 *vocation-fidelity*))
                (otherwise 0.5))))
    (min 1.0 (max 0.0 base))))

(defun initialize-v65-agentic! ()
  "Initialize the v6.5 Agency module."
  (initialize-drives!))

;;; --- SECTION 10: INITIALIZATION ---
(eval-when (:load-toplevel :execute)
  (initialize-drives!)
  (format t "[AGENCY] Fully integrated and active.~%"))