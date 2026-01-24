;;;; ============================================================================
;;;; UHMA DEEPER MIND - LOADER & HYPOTHESES (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Orchestrates the deeper mind sub-modules and implements hypothesis invention.
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 1: MODULE LOADING ---
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((dir (make-pathname :directory (pathname-directory *load-truename*))))
    (load (merge-pathnames "uhma-v6.3-deeper-mind-core.lisp" dir))))

;;; --- SECTION 4: STRUCTURES ---

;;; Structs moved to forward-decl.lisp

(defstruct attention-state
  "Allocation map for cognitive resources based on interestingness."
  (interesting-contexts nil :type list)
  (boring-contexts nil :type list)
  (mysterious-contexts nil :type list))

;;; --- SECTION 6: SPECIAL VARIABLES ---

(defvar *invented-hypotheses* (make-hash-table :test 'eq))
(defvar *attention-state* (make-attention-state))
(defvar *deeper-mind-enabled* t )

;;; --- SECTION 8: CORE LOGIC ---

(defun invent-hypothesis ()
  "Compose a novel internal theory using the generative vocabulary."
  (let ((observables (get-observable-names)))
    (when (and observables (> (length observables) 1))
      (let ((hyp (make-invented-hypothesis :created-at *step*)))
        (setf (gethash (invented-hypothesis-id hyp) *invented-hypotheses*) hyp)
        hyp))))

;;; --- SECTION 9: INTEGRATION HOOKS ---

(defun deeper-mind-maintenance-hook-v63 ()
  "Periodic update cycle for self-model and hypothesis testing."
  (when *deeper-mind-enabled*
    (when (zerop (mod *step* 50)) (update-self-doubt!))
    (when (zerop (mod *step* 100)) (invent-hypothesis))))

(defun initialize-deeper-mind! ()
  "Initialize the deeper mind module."
  (setf *invented-hypotheses* (make-hash-table :test 'eq))
  (setf *attention-state* (make-attention-state))
  (setf *deeper-mind-enabled* t))

;;; --- SECTION 10: INITIALIZATION ---
(eval-when (:load-toplevel :execute)
  (register-hook +hook-maintenance+ 'deeper-mind-maintenance-hook-v63 :priority 65)
  (format t "[DEEPER-MIND] Inventive engine active.~%"))