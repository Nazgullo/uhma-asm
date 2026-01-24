;;;; ============================================================================
;;;; UHMA AGENCY - DREAMS (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Implements offline consolidation and creative recombination.
;;;; DEPENDS ON: uhma-v6.5-agency-core.lisp
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 4: STRUCTURES ---

(defstruct dream-episode
  "An experience replayed during consolidation."
  (context nil :type list)
  (original-prediction nil)
  (actual-outcome nil)
  (difficulty 0.0 :type single-float)
  (replayed-count 0 :type fixnum)
  (insights 0 :type fixnum)
  (created-at 0 :type fixnum))

(defstruct dream-state
  "Global dreaming dynamics."
  (episode-buffer nil :type list)
  (dream-cycle 0 :type fixnum)
  (total-dreams 0 :type fixnum)
  (insights-discovered 0 :type fixnum))

;;; --- SECTION 6: SPECIAL VARIABLES ---

(defvar *dream-state* (make-dream-state))
(defvar *dreaming* nil )
(defvar *dream-buffer-size* 100 )

;;; --- SECTION 8: CORE FUNCTIONS ---

(defun should-dream-p ()
  "Biological imperative: sleep when memory is saturated or difficulty high."
  (let ((sat (get-heap-saturation))
        (buf (dream-state-episode-buffer *dream-state*)))
    (or (> sat 0.8)
        (and (> (length buf) 5)
             (> (reduce #'+ buf :key #'dream-episode-difficulty) 3.0)))))

(defun run-dream-cycle! ()
  "Execute a consolidation pass."
  (let ((buf (dream-state-episode-buffer *dream-state*)))
    (when buf
      (let ((*dreaming* t))
        (run-hook +hook-dream-start+ (length buf))
        (dolist (ep (subseq buf 0 (min 10 (length buf))))
          (dream-about-episode! ep)
          (incf (dream-episode-replayed-count ep)))
        (when (> (get-heap-saturation) 0.5) (prune-oldest-episodes! 10))
        (setf (dream-state-episode-buffer *dream-state*)
              (remove-if (lambda (e) (> (dream-episode-replayed-count e) 5)) buf))
        (run-hook +hook-dream-end+ nil)))))

;;; --- SECTION 11: VERIFICATION ---
(eval-when (:load-toplevel :execute)
  (format t "[AGENCY-DREAMS] Consolidation engine active.~%"))
