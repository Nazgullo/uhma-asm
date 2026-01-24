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

(defun consolidate-episodes! (mem)
  "Remove least significant episodes when buffer is full.
   Keeps the top half by significance score."
  (let* ((eps (episodic-memory-episodes mem))
         (n (fill-pointer eps))
         (half (max 1 (floor n 2))))
    ;; Sort indices by significance (ascending), remove bottom half
    (let ((indexed (loop for i below n
                        collect (cons i (or (episode-significance (aref eps i)) 0.0)))))
      (setf indexed (sort indexed #'> :key #'cdr))
      ;; Keep only top half
      (let ((keepers (mapcar #'car (subseq indexed 0 half)))
            (new-eps (make-array (episodic-memory-max-episodes mem)
                                 :element-type t :fill-pointer 0 :adjustable t)))
        (dolist (idx (sort keepers #'<))
          (vector-push-extend (aref eps idx) new-eps))
        (setf (episodic-memory-episodes mem) new-eps)))))

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
