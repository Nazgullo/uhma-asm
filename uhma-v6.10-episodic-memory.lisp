;;;; ============================================================================
;;;; UHMA EPISODIC MEMORY - LOADER & INTEGRATION (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Orchestrates episodic storage and provides autobiographical self-continuity.
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 1: MODULE LOADING ---
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((dir (make-pathname :directory (pathname-directory *load-truename*))))
    (load (merge-pathnames "uhma-v6.10-episodic-core.lisp" dir))))

;;; --- SECTION 8: CORE LOGIC ---

(defun detect-episode-boundary-p (ctx confidence surprise)
  "Determine if a new experience should begin."
  (declare (type single-float confidence surprise))
  (let ((bd (or (episodic-memory-boundary-detector-state *episodic-memory*)
                (setf (episodic-memory-boundary-detector-state *episodic-memory*)
                      (make-boundary-detector)))))
    (incf (boundary-detector-steps-since-boundary bd))
    (incf (boundary-detector-surprise-accumulator bd) surprise)
    (let ((reason (cond ((>= (boundary-detector-steps-since-boundary bd) (boundary-detector-max-episode-length bd)) :max-length)
                        ((> (boundary-detector-surprise-accumulator bd) 3.0) :surprise-spike)
                        ((and (> (boundary-detector-last-confidence bd) 0.7) (< confidence 0.3)) :confidence-collapse)
                        (t nil))))
      (setf (boundary-detector-last-confidence bd) confidence)
      (if reason
          (progn (setf (boundary-detector-steps-since-boundary bd) 0
                       (boundary-detector-surprise-accumulator bd) 0.0)
                 (values t reason))
          (values nil nil)))))

(defun compute-episode-significance (ep)
  "Score the importance of an experience chunk."
  (declare (type episode ep))
  (let ((sig 0.0))
    (incf sig (* 0.4 (min 1.0 (/ (episode-self-surprise-total ep) 3.0))))
    (incf sig (* 0.3 (min 1.0 (/ (episode-outcome-surprise-total ep) 3.0))))
    (min 1.0 sig)))

;;; --- SECTION 9: INTEGRATION HOOKS ---

(defun episodic-memory-post-process-hook (tok ctx predicted got-it)
  "Main integration point for experience capture."
  (declare (ignore tok predicted))
  (let* ((expert *last-answering-expert*)
         (confidence (if (and expert (typep expert 'expert))
                         (/ (float (expert-hits expert)) (max 1 (+ (expert-hits expert) (expert-misses expert))))
                         0.5))
         (surprise (if got-it 0.0 1.0)))
    (multiple-value-bind (boundary-p reason) (detect-episode-boundary-p ctx confidence surprise)
      (when boundary-p
        (when (episodic-memory-current-episode *episodic-memory*)
          (end-episode! (if got-it :success :failure)))
        (start-episode! reason)))
    (unless (episodic-memory-current-episode *episodic-memory*)
      (start-episode! :initial))))

(defun initialize-episodic-memory! ()
  "Initialize the episodic memory module."
  (setf *episodic-memory* (make-episodic-memory))
  (format t "[EPISODIC] Unified memory hooks active.~%"))

;;; --- SECTION 10: INITIALIZATION ---
(eval-when (:load-toplevel :execute)
  (register-hook +hook-post-process-token+ 'episodic-memory-post-process-hook :priority 70)
  (format t "[EPISODIC] Unified memory hooks active.~%"))