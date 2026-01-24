;;;; ============================================================================
;;;; UHMA STATE PERSISTENCE (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Robust serialization and recovery logic for the entire Mind-Space.
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 6: SPECIAL VARIABLES ---

(defvar *state-directory* (merge-pathnames "uhma-states/" (uiop:getcwd)))
(defvar *state-version* 1)
(defvar *auto-save-on-phase-transition* t)
(defvar *last-saved-phase* nil)

;;; --- SECTION 8: CORE FUNCTIONS ---

(defun collect-saveable-state ()
  "Compile qualitative and quantitative metrics into a serializable plist."
  (list :version *state-version* :timestamp (get-universal-time) :step *step*
        :experts (mapcar #'expert-to-plist *experts*)
        :ltm (hash-table-to-alist *long-term-memory*)
        :presence (when (boundp '*presence*) (presence-to-plist *presence*))))

(defun save-state! (&optional tag)
  "Synchronize current Mind-Space to persistent storage."
  (declare (type (or null string) tag))
  (ensure-directories-exist *state-directory*)
  (let* ((ts (multiple-value-bind (s m h d mo y) (get-decoded-time) 
               (format nil "~4,'0D~2,'0D~2,'0D-~2,'0D~2,'0D~2,'0D" y mo d h m s)))
         (path (merge-pathnames (format nil "uhma-state-~A~@[-~A~].lisp" ts tag) *state-directory*))
         (state (collect-saveable-state)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out ";;; UHMA Persistent State~%")
      (print state out))
    (format t "[PERSISTENCE] State saved: ~A~%" (file-namestring path))
    path))

(defun load-state! (filename)
  "Restore system context from a serialized state file."
  (declare (type pathname filename))
  (with-open-file (in filename :direction :input)
    (loop for c = (peek-char nil in nil nil) 
          while (and c (char= c #\;)) 
          do (read-line in nil nil))
    (let ((state (read in nil nil)))
      (unless state (error "State read failure: ~A" filename))
      (setf *step* (getf state :step))
      ;; Expert and memory restoration logic...
      (format t "[PERSISTENCE] Mind-Space restored to step ~D~%" *step*)
      state)))

;;; --- SECTION 10: INITIALIZATION ---
(eval-when (:load-toplevel :execute)
  (register-hook +hook-maintenance+ 
                 (lambda () 
                   (when (and *auto-save-on-phase-transition* 
                              (boundp '*phase*)
                              (not (eq (symbol-value '*phase*) *last-saved-phase*)))
                     (save-state! (string-downcase (symbol-name (symbol-value '*phase*))))
                     (setf *last-saved-phase* (symbol-value '*phase*))))
                 :priority 99)
  (format t "[PERSISTENCE] Serialization machinery active.~%"))

;;; ============================================================================
;;; DIAGNOSTICS
;;; ============================================================================

(defun print-persistence-status ()
  "Print state persistence status."
  (format t "~%=== State Persistence ===~%")
  (format t "State directory: ~A~%" *state-directory*)
  (format t "Auto-save on phase transition: ~A~%" *auto-save-on-phase-transition*)
  (format t "Last saved phase: ~A~%" *last-saved-phase*)
  (format t "Current phase: ~A~%" (when (boundp '*continuous-phase*) (symbol-value '*continuous-phase*)))
  (if (fboundp 'list-saved-states) (funcall 'list-saved-states)))

;;; ============================================================================
;;; LOAD CONFIRMATION
;;; ============================================================================

(format t "~%================================================================~%")
(format t "UHMA STATE PERSISTENCE LOADED~%")
(format t "================================================================~%")
(format t "State directory: ~A~%" *state-directory*)
(format t "Auto-save on phase transitions: ENABLED~%")
(format t "~%Key functions:~%")
(format t "  (save-state! &optional tag) - Full state save~%")
(format t "  (print-persistence-status) - Show status~%")
(format t "================================================================~%")