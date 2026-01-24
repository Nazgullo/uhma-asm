;;;; ============================================================================
;;;; UHMA ADAPTIVE SELF-TUNING (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; The self-model adaptively controls verification and program optimization.
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 1: PACKAGE EXPORTS (Added to uhma-forward-decl) ---

;;; --- SECTION 4: STRUCTURES ---

;;; Self-Model struct moved to forward-decl.lisp


;;; --- SECTION 6: SPECIAL VARIABLES ---
(defvar *self-model* nil "The active self-model instance.")

;;; --- SECTION 8: CORE FUNCTIONS ---

(defun create-self-model ()
  "Initialize the self-model."
  (make-self-model))

(defun record-outcome! (correct-p)
  "Record a prediction outcome in the self-model."
  (declare (type boolean correct-p))
  (let ((sm (or *self-model* (setf *self-model* (create-self-model)))))
    (push (if correct-p 0.0 1.0) (self-model-recent-errors sm))
    (when (> (length (self-model-recent-errors sm)) (self-model-error-window sm))
      (setf (self-model-recent-errors sm) (subseq (self-model-recent-errors sm) 0 (self-model-error-window sm))))
    (let ((error-val (if correct-p 0.0 1.0)))
      (setf (self-model-expected-error sm) (+ (* 0.95 (self-model-expected-error sm)) (* 0.05 error-val))))
    (setf (self-model-global-confidence sm) (- 1.0 (self-model-expected-error sm)))))

(defun compute-global-uncertainty ()
  "Compute current global uncertainty from self-model."
  (if (null *self-model*) 1.0
      (let* ((sm *self-model*)
             (expected (self-model-expected-error sm))
             (baseline (self-model-error-baseline sm))
             (norm-error (/ expected (+ baseline 0.1)))
             (recent (self-model-recent-errors sm))
             (variance (if (> (length recent) 5)
                           (let* ((mean (/ (reduce #'+ recent) (length recent)))
                                  (sq-diffs (mapcar (lambda (x) (expt (- x mean) 2)) recent)))
                             (/ (reduce #'+ sq-diffs) (length sq-diffs)))
                           0.25))
             (norm-variance (/ variance (+ (expt expected 2) 0.1))))
        (+ (* 0.7 norm-error) (* 0.3 (+ 1.0 norm-variance))))))

(defun reset-self-model! ()
  "Reset the self-model to its initial state."
  (setf *self-model* (create-self-model))
  (format t "[ADAPTIVE] Self-model reset.~%"))

(defun update-adaptive-threshold! ()
  "Update sparse threshold based on current uncertainty."
  (if (null *self-model*) *sparse-threshold*
      (let* ((sm *self-model*)
             (uncertainty (compute-global-uncertainty))
             (u-low (self-model-uncertainty-low sm))
             (u-high (self-model-uncertainty-high sm))
             (t-min (self-model-sparse-threshold-min sm))
             (t-max (self-model-sparse-threshold-max sm))
             (new-threshold (cond ((< uncertainty u-low) t-max)
                                  ((> uncertainty u-high) t-min)
                                  (t (let ((t-val (/ (- uncertainty u-low) (- u-high u-low))))
                                       (+ t-max (* (- t-min t-max) t-val)))))))
        (setf (self-model-current-sparse-threshold sm)
              (+ (* 0.9 (self-model-current-sparse-threshold sm)) (* 0.1 new-threshold)))
        (when (zerop (mod *step* 100))
          (push (list *step* uncertainty (self-model-current-sparse-threshold sm)) (self-model-threshold-history sm))
          (when (> (length (self-model-threshold-history sm)) 100)
            (setf (self-model-threshold-history sm) (subseq (self-model-threshold-history sm) 0 100))))
        (self-model-current-sparse-threshold sm))))

;;; --- SECTION 10: INITIALIZATION ---
(eval-when (:load-toplevel :execute)
  (register-hook +hook-post-reset+ (lambda () (setf *self-model* (create-self-model))))
  (unless *self-model* (setf *self-model* (create-self-model)))
  (format t "[ADAPTIVE] Self-model initialized and registered.~%"))

;;; ============================================================================
;;; HOMOICONIC: PROGRAM STRUCTURE ANALYSIS
;;; ============================================================================

(defun program-signature (program)
  "Create a signature representing program structure for comparison.
   Returns a canonical representation of what ops the program uses."
  (sort (copy-list (get-op-names program)) #'string< :key #'symbol-name))

(defun record-program-performance! (expert correct-p)
  "Record performance of expert's program structure for self-model analysis."
  (unless *self-model*
    (return-from record-program-performance!))
  (let* ((sm *self-model*)
         (program (expert-program expert))
         (sig (program-signature program))
         (stats-table (self-model-program-structure-stats sm))
         (current (gethash sig stats-table (cons 0 0))))
    ;; Update hits/total
    (setf (gethash sig stats-table)
          (cons (+ (car current) (if correct-p 1 0))
                (+ (cdr current) 1)))
    ;; Also track parameter values
    (dolist (param-info (get-modifiable-params program))
      (let* ((op-name (first param-info))
             (param-name (second param-info))
             (value (third param-info))
             (key (list op-name param-name value))
             (param-stats (gethash key (self-model-param-performance sm) (cons 0 0))))
        (setf (gethash key (self-model-param-performance sm))
              (cons (+ (car param-stats) (if correct-p 1 0))
                    (+ (cdr param-stats) 1)))))))

(defun analyze-program-structures! ()
  "Analyze which program structures perform best.
   Called periodically to update best-program-structures."
  (unless *self-model*
    (return-from analyze-program-structures!))
  (let* ((sm *self-model*)
         (stats-table (self-model-program-structure-stats sm))
         (structures nil))
    ;; Collect structures with sufficient data
    (maphash (lambda (sig stats)
               (when (> (cdr stats) 10)  ; Require at least 10 samples
                 (push (list sig 
                            (/ (float (car stats)) (cdr stats))
                            (cdr stats))
                       structures)))
             stats-table)
    ;; Sort by accuracy
    (setf structures (sort structures #'> :key #'second))
    ;; Keep top performers
    (setf (self-model-best-program-structures sm)
          (subseq structures 0 (min 5 (length structures))))
    (setf (self-model-last-program-analysis sm) *step*)))

(defun get-best-param-value (op-name param-name)
  "Get the best-performing value for a parameter based on self-model tracking."
  (unless *self-model*
    (return-from get-best-param-value nil))
  (let* ((param-perf (self-model-param-performance *self-model*))
         (best-value nil)
         (best-score 0.0))
    (maphash (lambda (key stats)
               (when (and (eq (first key) op-name)
                          (eq (second key) param-name)
                          (> (cdr stats) 5))
                 (let ((score (/ (float (car stats)) (cdr stats))))
                   (when (> score best-score)
                     (setf best-score score
                           best-value (third key))))))
             param-perf)
    (values best-value best-score)))

;;; ============================================================================
;;; HOMOICONIC: PROGRAM MODIFICATION
;;; ============================================================================

(defun maybe-improve-expert-program! (expert)
  "Self-model attempts to improve an expert's program based on analysis.
   Only modifies if we have good evidence for improvement."
  (unless *self-model*
    (return-from maybe-improve-expert-program! nil))
  (let ((program (expert-program expert))
        (modified nil))
    ;; Check each modifiable parameter
    (dolist (param-info (get-modifiable-params program))
      (let* ((op-name (first param-info))
             (param-name (second param-info))
             (current-value (third param-info)))
        (multiple-value-bind (best-value best-score)
            (get-best-param-value op-name param-name)
          ;; Only modify if we have strong evidence (>70% accuracy) and it's different
          (when (and best-value 
                     (> best-score 0.7)
                     (not (equal best-value current-value)))
            (set-modifiable-param! program op-name param-name best-value)
            (setf modified t)
            ;; Record the change
            (push (list *step* op-name param-name current-value best-value)
                  (expert-change-history expert))))))
    ;; Update program cost if modified
    (when modified
      (update-expert-program-cost! expert)
      ;; Fire hook for program modification
      (run-hook +hook-program-modified+ expert))
    modified))

(defun run-program-optimization! ()
  "Run a pass of program optimization across all experts.
   Called periodically to improve underperforming programs."
  (analyze-program-structures!)
  (let ((optimized 0))
    (dolist (expert *experts*)
      (when (and (> (expert-life expert) 0.5)
                 (> (+ (expert-hits expert) (expert-misses expert)) 20)
                 ;; Only optimize experts with below-average performance
                 (< (/ (float (expert-hits expert))
                       (max 1 (+ (expert-hits expert) (expert-misses expert))))
                    (self-model-global-confidence *self-model*)))
        (when (maybe-improve-expert-program! expert)
          (incf optimized))))
    optimized))

;;; ============================================================================
;;; ADAPTIVE VERIFICATION
;;; ============================================================================

(defun adaptive-should-verify-p (ctx confidence)
  "Self-model guided verification decision.
   Replaces the fixed-threshold should-verify-p.
   
   Decision factors:
   1. Global uncertainty (self-model) -> adjusts threshold
   2. Context encounters (need initial data)
   3. Per-context attention (focus on trouble spots)
   4. Prediction confidence (low = verify)"
  (incf-encounters ctx)
  (unless *sparse-enabled* 
    (when *self-model* (incf (self-model-total-verified *self-model*)))
    (return-from adaptive-should-verify-p t))
  
  (let* ((sm (or *self-model* (setf *self-model* (create-self-model))))
         (encounters (get-encounters ctx))
         (attention (get-attention ctx))
         (adaptive-threshold (update-adaptive-threshold!)))
    
    (cond
      ;; Always verify until minimum encounters
      ((< encounters *min-encounters-before-sparse*)
       (incf (self-model-total-verified sm))
       (incf *verifications-performed*)
       t)
      
      ;; Always verify if attention is focused here
      ((> attention *attention-focus-threshold*)
       (incf (self-model-total-verified sm))
       (incf *attention-forced-verifications*)
       (incf *verifications-performed*)
       t)
      
      ;; Use adaptive threshold
      ((< confidence adaptive-threshold)
       (incf (self-model-total-verified sm))
       (incf *verifications-performed*)
       t)
      
      ;; Skip verification
      (t
       (incf (self-model-total-skipped sm))
       (incf *verifications-skipped*)
       nil))))

;;; ============================================================================
;;; INTEGRATION: MODIFIED PROCESS-CHUNK WITH ADAPTIVE VERIFICATION
;;; ============================================================================

(defun process-chunk-adaptive! (text &key verbose carry-over)
  "Process text with adaptive self-tuning.
   Uses self-model to guide verification decisions."
  (unless *self-model*
    (setf *self-model* (create-self-model)))
  (let ((tokens (if carry-over 
                    (append carry-over (tokenize text))
                    (tokenize text)))
        (context nil) (correct 0) (total 0) (verified 0) (skipped 0))
    (when *morpheme-model*
      (learn-morphemes! text *morpheme-model*))
    ;; Build co-occurrence for embeddings
    (loop for i from 0 below (length tokens)
          do (loop for j from (max 0 (- i 2)) to (min (1- (length tokens)) (+ i 2))
                   when (/= i j) do (attract! (nth i tokens) (nth j tokens) 0.15)))
    ;; Process tokens
    (loop for tok in tokens
          do (incf *step*)
             (when (>= (length context) 1)
               (incf total)
               (setf *call-stack* nil)
               (let* ((ctx (subseq context 0 (min (apply #'max *context-scales*) 
                                                  (length context))))
                      (result (execute-with-routing ctx))
                      (predicted (op-result-value result))
                      (confidence (op-result-confidence result))
                      (source-expert (op-result-source result))
                      ;; USE ADAPTIVE VERIFICATION
                      (verify (adaptive-should-verify-p ctx confidence))
                      (got-it (eq predicted tok)))
                 ;; Record outcome in self-model
                 (record-outcome! got-it)
                 
                 ;; HOMOICONIC: Record program performance
                 (when *last-answering-expert*
                   (record-program-performance! *last-answering-expert* got-it))
                 
                 (if verify
                     (progn
                       (incf verified)
                       (when got-it (incf correct))
                       (learn! ctx tok predicted t))
                     (progn
                       (incf skipped)
                       (update-attention! ctx got-it)))
                 (when verbose
                   (format t "~4D: ~12A -> ~8A/~8A ~A ~A conf:~,2F thr:~,2F~%"
                           *step* 
                           (format nil "(~{~A~^ ~})" 
                                   (reverse (subseq ctx 0 (min 2 (length ctx)))))
                           (or predicted '_) tok
                           (if got-it "[OK]" "[X]")
                           (if verify "V" "S")
                           confidence
                           (self-model-current-sparse-threshold *self-model*)))))
             (push tok context)
             (when (> (length context) (apply #'max *context-scales*))
               (setf context (subseq context 0 (apply #'max *context-scales*))))
             ;; Update global context state
             (update-global-context-state! tok))
    ;; Run maintenance
    (run-consolidation!)
    (detect-and-create-travelers!)
    (recluster-types!)
    (run-compression!)
    (lifecycle-step!)
    
    ;; HOMOICONIC: Periodically run program optimization
    (when (and (> *step* 500) (zerop (mod *step* 200)))
      (run-program-optimization!))
    
    (values (if (> verified 0) (float (/ correct verified)) 0.0)
            total verified skipped
            (subseq context 0 (min (apply #'max *context-scales*) (length context))))))

(defun process-text-adaptive! (text &key (verbose t))
  "Process text string with adaptive self-tuning"
  (multiple-value-bind (acc total verified skipped)
      (process-chunk-adaptive! text :verbose verbose)
    (format t "~%Accuracy: ~,1F% (verified ~A/~A, skipped ~A)~%"
            (* 100 acc) (round (* acc verified)) total skipped)
    acc))

(defun process-file-adaptive! (path &key (chunk-size 10000) (verbose nil))
  "Process file with adaptive self-tuning"
  (with-open-file (s path :direction :input)
    (let ((total-acc 0.0) (n-chunks 0) (carry nil))
      (loop for chunk = (make-string chunk-size)
            for pos = (read-sequence chunk s)
            while (> pos 0)
            do (let ((text (subseq chunk 0 pos)))
                 (multiple-value-bind (acc total verified skipped new-carry)
                     (process-chunk-adaptive! text :verbose verbose :carry-over carry)
                   (declare (ignore total verified skipped))
                   (incf total-acc acc)
                   (incf n-chunks)
                   (setf carry new-carry))))
      (when (> n-chunks 0)
        (format t "=== Complete === Chunks: ~A, Avg Acc: ~,1F%~%"
                n-chunks (* 100 (/ total-acc n-chunks))))
      (/ total-acc (max 1 n-chunks)))))

;;; ============================================================================
;;; DIAGNOSTICS
;;; ============================================================================

(defun print-self-model ()
  "Print self-model state and statistics"
  (format t "~%========== Self-Model Status ==========~%")
  (if *self-model*
      (let ((sm *self-model*))
        (format t "Expected error: ~,3F~%" (self-model-expected-error sm))
        (format t "Global confidence: ~,3F~%" (self-model-global-confidence sm))
        (format t "Current uncertainty: ~,3F~%" (compute-global-uncertainty))
        (format t "Adaptive sparse threshold: ~,3F~%" 
                (self-model-current-sparse-threshold sm))
        (format t "Total verified: ~A, Total skipped: ~A~%"
                (self-model-total-verified sm)
                (self-model-total-skipped sm))
        (let ((total (+ (self-model-total-verified sm) 
                       (self-model-total-skipped sm))))
          (when (> total 0)
            (format t "Adaptive sparse ratio: ~,1F%~%"
                    (* 100 (/ (self-model-total-skipped sm) total)))))
        (format t "Recent errors (last 10): ~A~%"
                (subseq (self-model-recent-errors sm) 
                        0 (min 10 (length (self-model-recent-errors sm)))))
        
        ;; HOMOICONIC: Program structure analysis
        (format t "~%--- Program Structure Analysis ---~%")
        (format t "Unique program structures tracked: ~A~%"
                (hash-table-count (self-model-program-structure-stats sm)))
        (format t "Parameter combinations tracked: ~A~%"
                (hash-table-count (self-model-param-performance sm)))
        (when (self-model-best-program-structures sm)
          (format t "Best performing structures:~%")
          (dolist (s (self-model-best-program-structures sm))
            (format t "  ~{~A~^,~}: ~,1F% (~A samples)~%"
                    (first s) (* 100 (second s)) (third s))))
        
        (format t "~%Threshold history (recent):~%")
        (dolist (h (subseq (self-model-threshold-history sm) 
                          0 (min 5 (length (self-model-threshold-history sm)))))
          (format t "  Step ~A: uncertainty=~,2F threshold=~,3F~%"
                  (first h) (second h) (third h))))
      (format t "Self-model not initialized~%"))
  (format t "========================================~%"))

;;; ============================================================================
;;; DEMO
;;; ============================================================================

(defun demo-adaptive ()
  "Demonstrate adaptive self-tuning with homoiconic capabilities"
  (format t "~%================================================================~%")
  (format t "UHMA v6.1 - PRIORITY 13: ADAPTIVE SELF-TUNING (HOMOICONIC)~%")
  (format t "================================================================~%")
  (format t "Self-model guided verification + PROGRAM INTROSPECTION~%")
  (format t "The self-model can now READ and MODIFY expert programs!~%")
  (format t "================================================================~%")
  
  ;; Reset everything including self-model
  (reset!)
  (reset-self-model!)
  
  (format t "~%--- Phase 1: Initial Learning (high uncertainty) ---~%")
  (format t "Expect: low threshold (verify more)~%~%")
  (process-text-adaptive! "the cat sat on the mat" :verbose t)
  (print-self-model)
  
  (format t "~%--- Phase 2: Pattern Reinforcement ---~%")
  (dotimes (i 10)
    (process-text-adaptive! "the cat sat on the mat" :verbose nil))
  (format t "After 10 iterations:~%")
  (print-self-model)
  
  (format t "~%--- Phase 3: Show Expert Program Introspection ---~%")
  (when *experts*
    (let ((e (first *experts*)))
      (format t "Expert ~A program ops: ~{~A~^, ~}~%"
              (expert-id e) (get-op-names (expert-program e)))
      (format t "Modifiable params: ~S~%"
              (get-modifiable-params (expert-program e)))))
  
  (format t "~%--- Phase 4: Heavy Training (convergence) ---~%")
  (dotimes (i 30)
    (process-text-adaptive! "the cat sat on the mat" :verbose nil)
    (process-text-adaptive! "the dog ran in the park" :verbose nil)
    (process-text-adaptive! "the bird flew over the tree" :verbose nil))
  (format t "After 30x3 iterations:~%")
  (print-self-model)
  
  (format t "~%--- Phase 5: Check Program Structure Stats ---~%")
  (analyze-program-structures!)
  (format t "Best program structures identified.~%")
  (print-self-model)
  
  (format t "~%--- Phase 6: Generation Test ---~%")
  (format t "Greedy: ~A~%" (generate "the cat" :length 8 :method :greedy))
  (format t "Greedy: ~A~%" (generate "the dog" :length 8 :method :greedy))
  (format t "Greedy: ~A~%" (generate "the bird" :length 8 :method :greedy))
  
  (format t "~%--- Final Status ---~%")
  (print-status)
  (print-self-model)
  
  (format t "~%Demo complete.~%"))

;;; ============================================================================
;;; MODULE INITIALIZATION
;;; ============================================================================

;; Register hook to reset self-model whenever system resets
(register-hook +hook-post-reset+ 'reset-self-model!)

;; Initialize self-model now
(reset-self-model!)

;; Don't auto-run - let user call (demo-adaptive)
(format t "~%Priority 13: Adaptive Self-Tuning (Homoiconic) loaded.~%")
(format t "The self-model can now READ and MODIFY expert programs.~%")
(format t "Run (demo-adaptive) to test.~%")
