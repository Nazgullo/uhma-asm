;;;; ============================================================================
;;;; UHMA MEMORY BOUNDS (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Prevents memory exhaustion via organic pruning and heap sensing.
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 6: SPECIAL VARIABLES ---

(defvar *max-hypothesis-history* 100 )
(defvar *max-modification-history* 100 )
(defvar *max-schema-evolution-log* 200 )
(defvar *max-goal-history* 50 )
(defvar *max-experiment-history* 100 )

;;; --- SECTION 8: CORE FUNCTIONS ---

(defun compute-entry-salience (entry type)
  "Calculate qualitative priority for retention."
  (declare (type symbol type))
  (let ((age-factor (if (and (listp entry) (getf entry :step)) (exp (- (/ (- *step* (getf entry :step)) 1000.0))) 0.5)))
    (declare (type single-float age-factor))
    (case type
      (:modification (+ (* 0.7 (abs (or (getf entry :accuracy-delta) 0.0))) (* 0.3 age-factor)))
      (otherwise age-factor))))

(defun organic-prune-list (list-var type max-size)
  "Intelligently discard low-salience data while protecting :existential realizations."
  (declare (type symbol list-var type) (type fixnum max-size))
  (let ((current (symbol-value list-var)))
    (when (and current (> (length current) max-size))
      (let* ((existential (remove-if-not (lambda (e) (and (listp e) (getf e :existential))) current))
             (mortal (remove-if (lambda (e) (and (listp e) (getf e :existential))) current))
             (scored (sort (mapcar (lambda (e) (cons e (compute-entry-salience e type))) mortal) #'> :key #'cdr))
             (budget (max 0 (- max-size (length existential))))
             (survivors (mapcar #'car (subseq scored 0 (min (length scored) budget)))))
        (setf (symbol-value list-var) (append existential survivors))))))

;;; --- SECTION 10: INITIALIZATION ---
(eval-when (:load-toplevel :execute)
  (register-hook +hook-maintenance+ (lambda () (when (zerop (mod *step* 500)) (trim-all-histories!) (trim-all-arrays!))) :priority 99)
  (format t "[MEMORY-BOUNDS] Synaptic pruning active.~%"))

;;; ============================================================================
;;; ARRAY TRIMMING
;;; ============================================================================

(defun trim-adjustable-array (arr max-size)
  "Trim an adjustable array to max-size by keeping recent entries."
  (when (and arr (arrayp arr) (array-has-fill-pointer-p arr))
    (let ((current (fill-pointer arr)))
      (when (> current max-size)
        ;; Keep the most recent half
        (let ((keep-from (floor current 2))
              (keep-count (- current (floor current 2))))
          (when (> keep-count max-size)
            (setf keep-from (- current max-size)
                  keep-count max-size))
          (loop for i from 0 below keep-count
                do (setf (aref arr i) (aref arr (+ i keep-from))))
          (setf (fill-pointer arr) keep-count))))))

(defun trim-all-arrays! ()
  "Trim all adjustable arrays that might grow unboundedly."
  ;; Trace buffers
  (when (boundp '*trace-buffer*)
    (trim-adjustable-array *trace-buffer* 500))
  (when (boundp '*cognitive-trace-buffer*)
    (trim-adjustable-array *cognitive-trace-buffer* 1000))
  ;; Situation history
  (when (boundp '*situation-history*)
    (trim-adjustable-array *situation-history* 100))
  ;; Self-expectations
  (when (boundp '*self-expectations*)
    (trim-adjustable-array *self-expectations* 200)))

;;; ============================================================================
;;; HASH TABLE TRIMMING
;;; ============================================================================

(defun trim-hash-table-by-age (ht max-size age-key)
  "Trim hash table by removing oldest entries based on age-key accessor."
  (when (and ht (hash-table-p ht) (> (hash-table-count ht) max-size))
    (let ((entries nil))
      (maphash (lambda (k v) (push (cons k v) entries)) ht)
      ;; Sort by age (oldest first)
      (setf entries (sort entries #'<
                          :key (lambda (e) (or (funcall age-key (cdr e)) 0))))
      ;; Remove oldest until we're under limit
      (let ((to-remove (- (length entries) max-size)))
        (dolist (e (subseq entries 0 to-remove))
          (remhash (car e) ht))))))

(defun trim-all-hash-tables! ()
  "Trim hash tables that might grow unboundedly."
  ;; Causal model - keep strongest links
  (when (and (boundp '*causal-model*) (hash-table-p *causal-model*))
    (when (> (hash-table-count *causal-model*) 1000)
      (let ((entries nil))
        (maphash (lambda (k v) (push (cons k v) entries)) *causal-model*)
        (setf entries (sort entries #'<
                            :key (lambda (e)
                                   (if (causal-link-p (cdr e))
                                       (causal-link-strength (cdr e))
                                       0))))
        (let ((to-remove (- (length entries) 800)))
          (dolist (e (subseq entries 0 (max 0 to-remove)))
            (remhash (car e) *causal-model*)))))))

;;; ============================================================================
;;; MAINTENANCE HOOK
;;; ============================================================================

(defun memory-bounds-maintenance-hook ()
  "Periodic memory trimming - runs every 500 steps."
  (when (and (boundp '*step*) (zerop (mod *step* 500)))
    (trim-all-histories!)
    (trim-all-arrays!)
    (trim-all-hash-tables!))
  nil)

(register-hook +hook-maintenance+
               'memory-bounds-maintenance-hook
               :priority 99)  ; Run late, after other maintenance

;;; ============================================================================
;;; LOAD CONFIRMATION
;;; ============================================================================

(format t "~%================================================================~%")
(format t "UHMA MEMORY BOUNDS LOADED~%")
(format t "================================================================~%")
(format t "Capped histories:~%")
(format t "  - hypothesis-history: ~D~%" *max-hypothesis-history*)
(format t "  - modification-history: ~D~%" *max-modification-history*)
(format t "  - schema-evolution-log: ~D~%" *max-schema-evolution-log*)
(format t "  - goal-history: ~D~%" *max-goal-history*)
(format t "  - experiment-history: ~D~%" *max-experiment-history*)
(format t "Trimming runs every 500 steps.~%")
