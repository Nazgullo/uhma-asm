
;;;; FILE MANIFEST
;;;; ============
;;;; This file MUST contain the following when complete:
;;;; PACKAGES: uhma
;;;; FUNCTIONS: ensure-state-dir (and others)
;;;; TOTAL EXPECTED SECTIONS: Unknown (Legacy File)

;;;; UHMA State Persistence
;;;; Save and restore the entire learned state
(in-package :uhma)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize (speed 0) (safety 3) (debug 3))))


(defvar *state-save-path* "/home/peter/Desktop/MINION/uhma/saved-states/")

(defun ensure-state-dir ()
  (ensure-directories-exist *state-save-path*))

(defun save-hash-table-to-list (ht)
  "Convert hash table to alist for serialization."
  (let ((result nil))
    (maphash (lambda (k v) (push (cons k v) result)) ht)
    result))

(defun restore-hash-table-from-list (alist &key (test 'equal))
  "Restore hash table from alist."
  (let ((ht (make-hash-table :test test)))
    (dolist (pair alist)
      (setf (gethash (car pair) ht) (cdr pair)))
    ht))

(defun save-system-state! (&optional (name nil))
  "Save entire system state to disk."
  (ensure-state-dir)
  (let* ((timestamp (get-universal-time))
         (state-name (or name (format nil "state-~A" timestamp)))
         (state-path (format nil "~A~A.lisp" *state-save-path* state-name)))

    (format t "~%Saving system state to ~A...~%" state-path)

    (with-open-file (f state-path :direction :output :if-exists :supersede)
      ;; Header
      (format f ";;;; UHMA Saved State~%")
      (format f ";;;; Saved at: ~A~%" timestamp)
      (format f ";;;; Step: ~A~%" *step*)
      (format f "(in-package :uhma)~%~%")

      ;; Step counter
      (format f ";;; Core state~%")
      (format f "(setf *step* ~D)~%" *step*)

      ;; Global thresholds and parameters (these get modified by self-modification)
      (format f "~%;;; Thresholds and parameters~%")
      (format f "(setf *sparse-threshold* ~A)~%" *sparse-threshold*)
      (format f "(setf *attention-focus-threshold* ~A)~%" *attention-focus-threshold*)
      (format f "(setf *neighborhood-diversity-threshold* ~A)~%" *neighborhood-diversity-threshold*)
      (format f "(setf *death-threshold* ~A)~%" *death-threshold*)
      (format f "(setf *traveler-bridge-threshold* ~A)~%" *traveler-bridge-threshold*)
      (format f "(setf *traveler-knowledge-overlap-threshold* ~A)~%" *traveler-knowledge-overlap-threshold*)
      (format f "(setf *merge-similarity-threshold* ~A)~%" *merge-similarity-threshold*)
      (format f "(setf *pattern-prune-threshold* ~A)~%" *pattern-prune-threshold*)
      (format f "(setf *type-cluster-threshold* ~A)~%" *type-cluster-threshold*)
      (format f "(setf *type-context-weight* ~A)~%" *type-context-weight*)
      (format f "(setf *hidden-state-threshold* ~A)~%" *hidden-state-threshold*)
      (format f "(setf *chunk-similarity-threshold* ~A)~%" *chunk-similarity-threshold*)
      (format f "(setf *traveler-link-threshold* ~A)~%" *traveler-link-threshold*)
      (format f "(setf *improvement-threshold* ~A)~%" *improvement-threshold*)
      ;; Deep-mind parameters
      (when (boundp '*hypothesis-decay-rate*)
        (format f "(setf *hypothesis-decay-rate* ~A)~%" *hypothesis-decay-rate*))
      (when (boundp '*hypothesis-extinction-threshold*)
        (format f "(setf *hypothesis-extinction-threshold* ~A)~%" *hypothesis-extinction-threshold*))
      (when (boundp '*schema-spawn-rate*)
        (format f "(setf *schema-spawn-rate* ~A)~%" *schema-spawn-rate*))
      (when (boundp '*surprise-threshold*)
        (format f "(setf *surprise-threshold* ~A)~%" *surprise-threshold*))
      ;; Agency parameters
      (when (boundp '*goal-behavior-params*)
        (format f "(setf *goal-behavior-params* '~S)~%" *goal-behavior-params*))
      (when (boundp '*sustained-urgency-threshold*)
        (format f "(setf *sustained-urgency-threshold* ~A)~%" *sustained-urgency-threshold*))

      ;; Experts - save each expert's knowledge
      (format f "~%;;; Experts (~D)~%" (length *experts*))
      (format f "(setf *experts* '())~%")
      (dolist (expert (reverse *experts*))
        (format f "(let ((e (make-expert :id ~A~%" (expert-id expert))
        (format f "                      :confidence-threshold ~A~%" (expert-confidence-threshold expert))
        (format f "                      :life ~A~%" (expert-life expert))
        (format f "                      :hits ~A~%" (expert-hits expert))
        (format f "                      :misses ~A~%" (expert-misses expert))
        (format f "                      :birth-step ~A~%" (expert-birth-step expert))
        (format f "                      :last-used ~A)))~%" (expert-last-used expert))
        ;; Save program (the actual self-modifying code)
        (format f "  (setf (expert-program e) '~S)~%" (expert-program expert))
        ;; Save knowledge
        (format f "  (setf (expert-knowledge e) (restore-hash-table-from-list '~S))~%"
                (save-hash-table-to-list (expert-knowledge expert)))
        (format f "  (push e *experts*))~%"))

      ;; Long-term memory
      (format f "~%;;; Long-term memory (~D entries)~%" (hash-table-count *long-term-memory*))
      (format f "(setf *long-term-memory* (restore-hash-table-from-list '~S))~%"
              (save-hash-table-to-list *long-term-memory*))

      ;; Holographic store
      (format f "~%;;; Holographic store~%")
      (when *holo*
        (format f "(setf *holo* (make-holo-store))~%")
        (format f "(setf (holo-store-total-stored *holo*) ~D)~%"
                (holo-store-total-stored *holo*))
        (format f "(setf (holo-store-total-retrieved *holo*) ~D)~%"
                (holo-store-total-retrieved *holo*))
        (format f "(setf (holo-store-patterns *holo*) (restore-hash-table-from-list '~S))~%"
                (save-hash-table-to-list (holo-store-patterns *holo*)))
        (format f "(setf (holo-store-by-layer *holo*) (restore-hash-table-from-list '~S))~%"
                (save-hash-table-to-list (holo-store-by-layer *holo*))))

      ;; Presence state
      (format f "~%;;; Presence state~%")
      (when (and (boundp '*presence*) *presence*)
        (format f "(setf *presence* (make-presence~%")
        (format f "                  :trajectory ~S~%" (presence-trajectory *presence*))
        (format f "                  :continuity ~A~%" (presence-continuity *presence*))
        (format f "                  :self-confidence ~A~%"
                (if (slot-boundp *presence* 'self-confidence)
                    (presence-self-confidence *presence*) 0.5))
        (format f "                  :felt-direction ~S))~%"
                (if (slot-boundp *presence* 'felt-direction)
                    (presence-felt-direction *presence*) :neutral)))

      ;; Dream state
      (format f "~%;;; Dream state~%")
      (when (and (boundp '*dream-state*) *dream-state*)
        (format f "(setf *dream-state* (make-dream-state~%")
        (format f "                     :total-dreams ~D))~%"
                (dream-state-total-dreams *dream-state*)))

      ;; Episodic memory count
      (format f "~%;;; Episodic memory~%")
      (when (and (boundp '*episodic-memory*) *episodic-memory*)
        (format f ";; Episode count: ~D~%" (episodic-memory-episode-count *episodic-memory*)))

      ;; Hypotheses (save top ones)
      (format f "~%;;; Hypotheses (~D)~%"
              (if (and (boundp '*hypotheses*) (hash-table-p *hypotheses*))
                  (hash-table-count *hypotheses*) 0))
      (when (and (boundp '*hypotheses*) (hash-table-p *hypotheses*))
        (format f "(setf *hypotheses* (restore-hash-table-from-list '~S))~%"
                (let ((top-hyps nil)
                      (count 0))
                  (maphash (lambda (k v)
                             (when (< count 100)
                               (push (cons k v) top-hyps)
                               (incf count)))
                           *hypotheses*)
                  top-hyps)))

      ;; Executable schemas
      (format f "~%;;; Schemas~%")
      (when (and (boundp '*executable-schemas*) *executable-schemas*)
        (let ((schema-count (if (hash-table-p *executable-schemas*)
                                (hash-table-count *executable-schemas*)
                                (length *executable-schemas*))))
          (format f ";; Executable schemas: ~D~%" schema-count)))

      ;; Recent outcomes
      (format f "~%;;; Recent outcomes~%")
      (when (boundp '*recent-outcomes*)
        (format f "(setf *recent-outcomes* '~S)~%"
                (subseq *recent-outcomes* 0 (min 100 (length *recent-outcomes*)))))

      ;; Footer with stats
      (format f "~%;;; State Summary~%")
      (format f ";;; Step: ~D~%" *step*)
      (format f ";;; Experts: ~D~%" (length *experts*))
      (format f ";;; LTM entries: ~D~%" (hash-table-count *long-term-memory*))
      (format f ";;; Holo patterns: ~D~%"
              (if *holo* (hash-table-count (holo-store-patterns *holo*)) 0))
      (format f "~%;;; Load with: (load ~S)~%" state-path)
      (format f "(format t \"~%State loaded: ~A~%\")~%" state-name))

    (format t "State saved to ~A~%" state-path)
    (format t "  Step: ~D~%" *step*)
    (format t "  Experts: ~D~%" (length *experts*))
    (format t "  LTM: ~D~%" (hash-table-count *long-term-memory*))
    (format t "  Holo patterns: ~D~%"
            (if *holo* (hash-table-count (holo-store-patterns *holo*)) 0))
    state-path))

(defun list-saved-states ()
  "List all saved states."
  (ensure-state-dir)
  (let ((files (directory (format nil "~A*.lisp" *state-save-path*))))
    (format t "~%Saved states:~%")
    (dolist (f files)
      (format t "  ~A~%" (file-namestring f)))
    files))

(defun load-system-state! (name)
  "Load a saved system state."
  (let ((state-path (format nil "~A~A.lisp" *state-save-path* name)))
    (if (probe-file state-path)
        (progn
          (format t "~%Loading state from ~A...~%" state-path)
          (load state-path)
          (format t "State loaded successfully.~%")
          t)
        (progn
          (format t "State file not found: ~A~%" state-path)
          nil))))

(defun save-state-binary! (&optional (name nil))
  "Save state in a more compact binary-ish format using print/read."
  (ensure-state-dir)
  (let* ((timestamp (get-universal-time))
         (state-name (or name (format nil "state-~A" timestamp)))
         (state-path (format nil "~A~A.state" *state-save-path* state-name)))

    (format t "~%Saving binary state to ~A...~%" state-path)

    (with-open-file (f state-path :direction :output :if-exists :supersede)
      (let ((*print-readably* t)
            (*print-circle* t))
        ;; Save as a single plist
        (print (list
                :version 1
                :timestamp timestamp
                :step *step*
                :experts-count (length *experts*)
                :ltm-count (hash-table-count *long-term-memory*)
                :ltm-data (save-hash-table-to-list *long-term-memory*)
                :holo-stored (if *holo* (holo-store-total-stored *holo*) 0)
                :holo-retrieved (if *holo* (holo-store-total-retrieved *holo*) 0)
                :holo-patterns (if *holo* (save-hash-table-to-list (holo-store-patterns *holo*)) nil)
                :presence-trajectory (if (and (boundp '*presence*) *presence*)
                                         (presence-trajectory *presence*) nil)
                :presence-continuity (if (and (boundp '*presence*) *presence*)
                                         (presence-continuity *presence*) 0)
                :recent-outcomes (when (boundp '*recent-outcomes*)
                                   (subseq *recent-outcomes* 0
                                           (min 100 (length *recent-outcomes*)))))
               f)))

    (format t "Binary state saved: ~A~%" state-path)
    state-path))

;;; Auto-save hook - call this at the end of feed
(defun auto-save-after-feed! ()
  "Automatically save state after feed completes."
  (let ((name (format nil "post-feed-~A" (get-universal-time))))
    (save-system-state! name)
    (format t "~%*** STATE AUTOMATICALLY SAVED ***~%")
    (format t "To restore later: (load-system-state! ~S)~%" name)))

(format t "~%Persistence module loaded.~%")
(format t "  (save-system-state!)      - Save current state~%")
(format t "  (load-system-state! name) - Load a saved state~%")
(format t "  (list-saved-states)       - List all saved states~%")


;;;; [SECTION-START:99:VERIFICATION]
;;; Section 99: File Verification

(defun verify-persistence-lisp-completeness () 
  "Verify completeness."
  (unless (find-package :uhma)
    (error "Package UHMA not defined"))
  (unless (fboundp 'ensure-state-dir)
    (error "Function ensure-state-dir not defined"))
  (format t "~&uhma-persistence.lisp verification passed.~%"))

(verify-persistence-lisp-completeness)

;;;; [SECTION-END:99:VERIFICATION]
