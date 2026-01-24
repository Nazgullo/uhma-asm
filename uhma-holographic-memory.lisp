
;;;; FILE MANIFEST
;;;; ============
;;;; This file MUST contain the following when complete:
;;;; PACKAGES: uhma
;;;; FUNCTIONS: holo-compute-activations (and others)
;;;; TOTAL EXPECTED SECTIONS: Unknown (Legacy File)

;;;; ============================================================================
;;;; UHMA HOLOGRAPHIC MEMORY - Generated 2026-01-15T12:42:11.423486
;;;; ============================================================================
;;;;
;;;; Holographic distributed memory layer - hooks-only integration
;;;;
;;;; INTEGRATION STRATEGY:
;;;; - Uses hooks ONLY, never modifies core files
;;;; - Registers with LOW priority (20) to run AFTER core processing
;;;; - Maintains OWN state, never touches *presence*
;;;; - Read-only access to system state for decisions
;;;;
;;;; FORBIDDEN (will break dreams/learning if called):
;;;; ;;;;   - presence-shift-trajectory!
;;;;   - presence-feel-discontinuity!
;;;;   - (setf (presence-vividness
;;;;   - (setf (presence-continuity
;;;;   - (setf (presence-self-confidence
;;;;   - (incf (presence-continuity
;;;;
;;;; ============================================================================

(in-package :uhma)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize (speed 0) (safety 3) (debug 3))))


;;; ============================================================================
;;; CONFIGURATION
;;; ============================================================================

(defparameter *holographic-memory-enabled* t
  "Master switch for holographic memory layer.")

(defparameter *holographic-decay-rate* 0.01
  "Per-step decay rate for pattern strength.")

(defparameter *holographic-min-strength* 0.1
  "Minimum strength before pattern is pruned.")

(defparameter *holographic-max-patterns* 10000
  "Maximum patterns to store.")

;;; ============================================================================
;;; DATA STRUCTURES
;;; ============================================================================

(defstruct holographic-pattern
  "A memory encoded as sparse expert activations."
  (id (gensym "HP-"))
  (activations nil :type list)       ; alist of (expert-id . activation)
  (content nil)                       ; what this pattern represents
  (strength 1.0 :type single-float)   ; decays over time
  (created-step 0 :type fixnum)
  (access-count 0 :type fixnum)
  (last-accessed 0 :type fixnum))

(defstruct holographic-store
  "The holographic memory store."
  (patterns (make-hash-table :test 'equal))  ; id -> pattern
  (by-expert (make-hash-table :test 'eql))   ; expert-id -> list of pattern-ids
  (recent nil :type list)                     ; recently accessed pattern-ids
  (total-stored 0 :type fixnum)
  (total-retrieved 0 :type fixnum))

(defvar *holo-store* (make-holographic-store)
  "The global holographic memory store.")

;;; ============================================================================
;;; CORE OPERATIONS
;;; ============================================================================

(defun holo-compute-activations (ctx)
  "Compute expert activations for context WITHOUT modifying anything.
   Returns alist of (expert-id . activation-strength)."
  (when (and *experts* ctx)
    (let ((ctx-emb (when (fboundp 'context-embedding)
                     (ignore-errors (context-embedding ctx))))
          (activations nil))
      (dolist (expert *experts*)
        (when (and (> (expert-life expert) 0.1)
                   (expert-centroid expert)
                   ctx-emb)
          (let* ((sim (ignore-errors (cosim ctx-emb (expert-centroid expert))))
                 (reliability (/ (expert-hits expert)
                                (max 1 (+ (expert-hits expert)
                                         (expert-misses expert)))))
                 (activation (when sim (* sim (+ 0.5 (* 0.5 reliability))))))
            (when (and activation (> activation 0.1))
              (push (cons (expert-id expert) activation) activations)))))
      ;; Return top activations sorted
      (subseq (sort activations #'> :key #'cdr)
              0 (min 10 (length activations))))))

(defun holo-pattern-similarity (p1-activations p2-activations)
  "Compute similarity between two activation patterns."
  (let ((dot 0.0) (mag1 0.0) (mag2 0.0))
    (dolist (a1 p1-activations)
      (incf mag1 (* (cdr a1) (cdr a1)))
      (let ((a2 (assoc (car a1) p2-activations)))
        (when a2
          (incf dot (* (cdr a1) (cdr a2))))))
    (dolist (a2 p2-activations)
      (incf mag2 (* (cdr a2) (cdr a2))))
    (if (or (zerop mag1) (zerop mag2))
        0.0
        (/ dot (sqrt (* mag1 mag2))))))

(defun holo-store! (content activations)
  "Store a new holographic pattern. Returns the pattern."
  (when (and *holographic-memory-enabled* activations)
    (let ((pattern (make-holographic-pattern
                    :activations activations
                    :content content
                    :created-step *step*)))
      ;; Store in main table
      (setf (gethash (holographic-pattern-id pattern)
                     (holographic-store-patterns *holo-store*))
            pattern)
      ;; Index by expert
      (dolist (act activations)
        (push (holographic-pattern-id pattern)
              (gethash (car act) (holographic-store-by-expert *holo-store*))))
      ;; Track
      (incf (holographic-store-total-stored *holo-store*))
      ;; Prune if over limit
      (when (> (hash-table-count (holographic-store-patterns *holo-store*))
               *holographic-max-patterns*)
        (holo-prune-weakest!))
      pattern)))

(defun holo-retrieve (query-activations &optional (threshold 0.5) (limit 10))
  "Retrieve patterns similar to query activations."
  (when (and *holographic-memory-enabled* query-activations)
    (let ((results nil))
      (maphash
       (lambda (id pattern)
         (declare (ignore id))
         (let ((sim (holo-pattern-similarity
                     query-activations
                     (holographic-pattern-activations pattern))))
           (when (> sim threshold)
             ;; Update access stats (okay - it's OUR data structure)
             (incf (holographic-pattern-access-count pattern))
             (setf (holographic-pattern-last-accessed pattern) *step*)
             (incf (holographic-pattern-strength pattern) 0.05)
             (push (cons sim pattern) results))))
       (holographic-store-patterns *holo-store*))
      ;; Return top results
      (incf (holographic-store-total-retrieved *holo-store*))
      (mapcar #'cdr
              (subseq (sort results #'> :key #'car)
                      0 (min limit (length results)))))))

(defun holo-decay! ()
  "Apply decay to all patterns."
  (maphash
   (lambda (id pattern)
     (declare (ignore id))
     (decf (holographic-pattern-strength pattern) *holographic-decay-rate*))
   (holographic-store-patterns *holo-store*)))

(defun holo-prune-weakest! ()
  "Remove weakest patterns to stay under limit."
  (let ((to-remove nil))
    ;; Find weak patterns
    (maphash
     (lambda (id pattern)
       (when (< (holographic-pattern-strength pattern) *holographic-min-strength*)
         (push id to-remove)))
     (holographic-store-patterns *holo-store*))
    ;; Remove them
    (dolist (id to-remove)
      (let ((pattern (gethash id (holographic-store-patterns *holo-store*))))
        (when pattern
          ;; Remove from expert index
          (dolist (act (holographic-pattern-activations pattern))
            (setf (gethash (car act) (holographic-store-by-expert *holo-store*))
                  (remove id (gethash (car act) (holographic-store-by-expert *holo-store*)))))
          ;; Remove from main store
          (remhash id (holographic-store-patterns *holo-store*)))))))

;;; ============================================================================
;;; HOOK HANDLERS - Low priority (20) to not interfere
;;; ============================================================================

(defun holographic-observe-token (tok ctx prediction correct-p)
  "Observe token processing - store interesting patterns."
  (declare (ignore prediction))
  (when *holographic-memory-enabled*
    ;; Compute activations for this context
    (let ((activations (holo-compute-activations ctx)))
      (when activations
        ;; Store errors (interesting) and sample successes
        (when (or (not correct-p)
                  (< (random 1.0) 0.1))  ; 10% of successes
          (holo-store! (list :token tok :ctx ctx :correct correct-p :step *step*)
                       activations))))))

(defun holographic-observe-learning (ctx actual predicted correct-p learner)
  "Observe learning events - strengthen related patterns."
  (declare (ignore predicted))
  (when (and *holographic-memory-enabled* learner)
    (let ((activations (holo-compute-activations ctx)))
      (when activations
        ;; Find and strengthen similar patterns
        (dolist (pattern (holo-retrieve activations 0.6 5))
          (incf (holographic-pattern-strength pattern)
                (if correct-p 0.1 0.05)))
        ;; Store learning event
        (holo-store! (list :learned actual :by (expert-id learner) :correct correct-p)
                     activations)))))

(defun holographic-maintenance ()
  "Maintenance: decay patterns, prune weak ones."
  (when *holographic-memory-enabled*
    (holo-decay!)
    (holo-prune-weakest!)))

;;; ============================================================================
;;; REGISTER HOOKS - Priority 20 (low, runs after core)
;;; ============================================================================

(register-hook +hook-post-process-token+
               'holographic-observe-token
               :priority 20)

(register-hook +hook-post-learn+
               'holographic-observe-learning
               :priority 20)

(register-hook +hook-maintenance+
               'holographic-maintenance
               :priority 20)

;;; ============================================================================
;;; API FUNCTIONS
;;; ============================================================================

(defun holographic-query (ctx &optional (threshold 0.5) (limit 10))
  "Query holographic memory with a context."
  (let ((activations (holo-compute-activations ctx)))
    (when activations
      (holo-retrieve activations threshold limit))))

(defun holographic-stats ()
  "Return holographic memory statistics."
  (list :enabled *holographic-memory-enabled*
        :pattern-count (hash-table-count (holographic-store-patterns *holo-store*))
        :total-stored (holographic-store-total-stored *holo-store*)
        :total-retrieved (holographic-store-total-retrieved *holo-store*)
        :experts-indexed (hash-table-count (holographic-store-by-expert *holo-store*))))

(defun holographic-reset! ()
  "Reset holographic memory store."
  (setf *holo-store* (make-holographic-store)))

;; Reset on system reset
(register-hook +hook-post-reset+
               (lambda () (holographic-reset!))
               :priority 20)

;;; ============================================================================
;;; INITIALIZATION
;;; ============================================================================

(format t "~%================================================================~%")
(format t "UHMA HOLOGRAPHIC MEMORY LAYER~%")
(format t "================================================================~%")
(format t "Status: ~A~%" (if *holographic-memory-enabled* "ENABLED" "DISABLED"))
(format t "Integration: hooks-only (priority 20)~%")
(format t "Hooks registered:~%")
(format t "  - POST-PROCESS-TOKEN -> holographic-observe-token~%")
(format t "  - POST-LEARN -> holographic-observe-learning~%")
(format t "  - MAINTENANCE -> holographic-maintenance~%")
(format t "================================================================~%")


;;;; [SECTION-START:99:VERIFICATION]
;;; Section 99: File Verification

(defun verify-holographic-memory-lisp-completeness () 
  "Verify completeness."
  (unless (find-package :uhma)
    (error "Package UHMA not defined"))
  (unless (fboundp 'holo-compute-activations)
    (error "Function holo-compute-activations not defined"))
  (format t "~&uhma-holographic-memory.lisp verification passed.~%"))

(verify-holographic-memory-lisp-completeness)

;;;; [SECTION-END:99:VERIFICATION]
