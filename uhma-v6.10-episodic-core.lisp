;;;; ============================================================================
;;;; UHMA EPISODIC MEMORY - CORE (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Implements bounded experiences and temporal storage.
;;;; DEPENDS ON: uhma-v6.1-core-homoiconic.lisp, uhma-v6.2-deep-mind.lisp
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 4: STRUCTURES ---

;;; Structs moved to uhma-forward-decl.lisp

;;; --- SECTION 6: SPECIAL VARIABLES ---

(defvar *episodic-memory* (make-episodic-memory))

;;; --- SECTION 8: CORE FUNCTIONS ---

(defun start-episode! (trigger)
  "Initialize a new experience boundary."
  (let ((ep (make-episode :start-step *step* :trigger trigger)))
    (setf (episodic-memory-current-episode *episodic-memory*) ep)
    ep))

(defun end-episode! (outcome)
  "Close and commit the current experience."
  (let ((ep (episodic-memory-current-episode *episodic-memory*)))
    (when ep
      (setf (episode-end-step ep) *step*
            (episode-outcome ep) outcome
            (episode-events ep) (nreverse (episode-events ep))
            (episode-significance ep) (compute-episode-significance ep))
      (store-episode! ep)
      (setf (episodic-memory-current-episode *episodic-memory*) nil)
      ep)))

(defun store-episode! (ep)
  "Persist episode in the rolling store."
  (declare (type episode ep))
  (let ((mem *episodic-memory*))
    (when (>= (fill-pointer (episodic-memory-episodes mem)) (episodic-memory-max-episodes mem))
      (consolidate-episodes! mem))
    (vector-push-extend ep (episodic-memory-episodes mem))
    (incf (episodic-memory-episode-count mem))
    (when (episode-self-relevant-p ep)
      (push ep (episodic-memory-autobiographical mem)))))

;;; --- SECTION 11: VERIFICATION ---
(eval-when (:load-toplevel :execute)
  (format t "[EPISODIC-CORE] Temporal storage initialized.~%"))
