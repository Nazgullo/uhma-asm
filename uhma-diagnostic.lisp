;;;; ============================================================================
;;;; UHMA DIAGNOSTIC SYSTEM (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Provides comprehensive system-wide health checks and visualization.
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 6: SPECIAL VARIABLES ---

(defvar *diag-log* nil "History of diagnostic entries.")
(defvar *diag-file* nil "Diagnostic output stream.")
(defvar *diag-on* nil )

;;; --- SECTION 8: CORE FUNCTIONS ---

(defun diag (category &rest args)
  "Record a qualitative diagnostic snapshot."
  (declare (type symbol category))
  (when *diag-on*
    (let ((msg (apply #'format nil args)))
      (push (list :step *step* :cat category :msg msg) *diag-log*)
      (when *diag-file* (format *diag-file* "[~D] ~A: ~A~%" *step* category msg)))))

(defun dump-experts ()
  "Quantitative audit of the expert population."
  (diag :SYSTEM "Expert Audit: ~D active units." (length *experts*))
  (dolist (e *experts*)
    (diag :EXPERT "ID: ~A Life: ~,2F Acc: ~,2F" (expert-id e) (expert-life e) (expert-hits e))))

;;; --- SECTION 10: INITIALIZATION ---
(eval-when (:load-toplevel :execute)
  (format t "[DIAGNOSTIC] Health monitoring systems ready.~%"))

;;; ============================================================================
;;; HYPOTHESIS TRACKING  
;;; ============================================================================

(defun dump-hypotheses ()
  "Dump current state of all hypotheses"
  (diag "HYPOTHESES" "=== Hypothesis State (total: ~D) ===" (hash-table-count *hypotheses*))
  (let ((by-status (make-hash-table)))
    (maphash (lambda (k v)
               (declare (ignore k))
               (push v (gethash (self-hypothesis-status v) by-status)))
             *hypotheses*)
    (maphash (lambda (status hyps)
               (diag "HYPOTHESES" "  ~A: ~D" status (length hyps)))
             by-status))
  ;; Show unique claims
  (let ((claims (make-hash-table :test 'equal)))
    (maphash (lambda (k v)
               (declare (ignore k))
               (incf (gethash (self-hypothesis-claim v) claims 0)))
             *hypotheses*)
    (diag "HYPOTHESES" "  Unique claims: ~D" (hash-table-count claims))
    (maphash (lambda (claim count)
               (when (> count 1)
                 (diag "HYPOTHESES" "    DUPLICATE: ~A (x~D)" claim count)))
             claims)))

(defun dump-hypothesis-detail (hyp)
  "Dump full detail of one hypothesis"
  (diag "HYP-DETAIL" "ID: ~A" (self-hypothesis-id hyp))
  (diag "HYP-DETAIL" "  Claim: ~S" (self-hypothesis-claim hyp))
  (diag "HYP-DETAIL" "  Type: ~A" (type-of (self-hypothesis-claim hyp)))
  (diag "HYP-DETAIL" "  Status: ~A" (self-hypothesis-status hyp))
  (diag "HYP-DETAIL" "  Confidence: ~,2F" (self-hypothesis-confidence hyp))
  (diag "HYP-DETAIL" "  Tested: ~D, Confirmed: ~D" 
        (self-hypothesis-times-tested hyp)
        (self-hypothesis-times-confirmed hyp))
  (diag "HYP-DETAIL" "  Evidence for: ~D, against: ~D"
        (length (self-hypothesis-evidence-for hyp))
        (length (self-hypothesis-evidence-against hyp)))
  (diag "HYP-DETAIL" "  Proposed action: ~S" (self-hypothesis-proposed-action hyp)))

;;; ============================================================================
;;; SELF-MODIFICATION TRACKING
;;; ============================================================================

(defvar *orig-execute-self-mod* nil)

(defun wrap-execute-self-modification! ()
  "Wrap execute-self-modification! to trace what happens"
  (unless *orig-execute-self-mod*
    (when (fboundp 'execute-self-modification!)
      (setf *orig-execute-self-mod* (symbol-function 'execute-self-modification!))))
  (setf (symbol-function 'execute-self-modification!)
        (lambda (action)
          (diag "SELF-MOD" ">>> execute-self-modification! ~S" action)
          (let ((experts-before (track-expert-changes)))
            (let ((result (funcall *orig-execute-self-mod* action)))
              (let ((experts-after (track-expert-changes)))
                (compare-expert-states experts-before experts-after)
                (when (equal experts-before experts-after)
                  (diag "SELF-MOD" "!!! NO CHANGES MADE !!!")))
              result)))))

;;; ============================================================================
;;; HYPOTHESIS GENERATION TRACKING
;;; ============================================================================

(defvar *orig-generate-hypothesis* nil)

(defun wrap-generate-hypothesis! ()
  "Wrap generate-hypothesis! to trace claim creation"
  (unless *orig-generate-hypothesis*
    (when (fboundp 'generate-hypothesis!)
      (setf *orig-generate-hypothesis* (symbol-function 'generate-hypothesis!))))
  (setf (symbol-function 'generate-hypothesis!)
        (lambda (observation)
          (diag "GEN-HYP" ">>> observation: ~S" observation)
          (diag "GEN-HYP" "    obs type: ~A" (type-of observation))
          (let ((result (funcall *orig-generate-hypothesis* observation)))
            (when result
              (diag "GEN-HYP" "<<< created: ~A" (self-hypothesis-id result))
              (diag "GEN-HYP" "    claim: ~S" (self-hypothesis-claim result))
              (diag "GEN-HYP" "    claim type: ~A" (type-of (self-hypothesis-claim result))))
            result))))

;;; ============================================================================
;;; HYPOTHESIS TESTING TRACKING
;;; ============================================================================

(defvar *orig-test-hypothesis* nil)

(defun wrap-test-hypothesis! ()
  "Wrap test-hypothesis! to trace evaluation"
  (unless *orig-test-hypothesis*
    (when (fboundp 'test-hypothesis!)
      (setf *orig-test-hypothesis* (symbol-function 'test-hypothesis!))))
  (setf (symbol-function 'test-hypothesis!)
        (lambda (hyp)
          (diag "TEST-HYP" ">>> ~A (conf=~,2F tested=~D)"
                (self-hypothesis-id hyp)
                (self-hypothesis-confidence hyp)
                (self-hypothesis-times-tested hyp))
          (let ((result (funcall *orig-test-hypothesis* hyp)))
            (diag "TEST-HYP" "<<< ~A now conf=~,2F tested=~D status=~A"
                  (self-hypothesis-id hyp)
                  (self-hypothesis-confidence hyp)
                  (self-hypothesis-times-tested hyp)
                  (self-hypothesis-status hyp))
            result))))

;;; ============================================================================
;;; CLAIM EVALUATION TRACKING
;;; ============================================================================

(defvar *orig-evaluate-claim* nil)

(defun wrap-evaluate-claim! ()
  "Wrap evaluate-claim-against-trace to see verdicts"
  (unless *orig-evaluate-claim*
    (when (fboundp 'evaluate-claim-against-trace)
      (setf *orig-evaluate-claim* (symbol-function 'evaluate-claim-against-trace))))
  (setf (symbol-function 'evaluate-claim-against-trace)
        (lambda (claim trace)
          (let ((result (funcall *orig-evaluate-claim* claim trace)))
            (when (and *diag-on* result)
              (diag "EVAL-CLAIM" "claim=~S trace-step=~D -> ~A"
                    (if (> (length (format nil "~S" claim)) 50)
                        (subseq (format nil "~S" claim) 0 50)
                        claim)
                    (cognitive-trace-step trace)
                    result))
            result))))

;;; ============================================================================
;;; MAINTENANCE HOOK TRACKING
;;; ============================================================================

(defvar *orig-deep-mind-maint* nil)

(defun wrap-deep-mind-maintenance! ()
  "Wrap deep-mind-maintenance-hook"
  (unless *orig-deep-mind-maint*
    (when (fboundp 'deep-mind-maintenance-hook)
      (setf *orig-deep-mind-maint* (symbol-function 'deep-mind-maintenance-hook))))
  (setf (symbol-function 'deep-mind-maintenance-hook)
        (lambda ()
          (diag "MAINT" ">>> deep-mind-maintenance step=~D" *step*)
          (diag "MAINT" "    *last-hyp-test-step*=~A" 
                (if (boundp '*last-hyp-test-step*) *last-hyp-test-step* 'UNBOUND))
          (diag "MAINT" "    hypotheses=~D" (hash-table-count *hypotheses*))
          (let ((result (funcall *orig-deep-mind-maint*)))
            (diag "MAINT" "<<< deep-mind-maintenance done")
            result))))

;;; ============================================================================
;;; DRIVE AND GOAL TRACKING
;;; ============================================================================

(defun dump-drives ()
  "Dump current drive states"
  (diag "DRIVES" "=== Drive State ===")
  (when (boundp '*intrinsic-drives*)
    (dolist (d *intrinsic-drives*)
      (diag "DRIVES" "  ~A: ~,2F (target ~,2F) ~A"
            (drive-name d)
            (drive-current-level d)
            (drive-target-level d)
            (if (> (drive-current-level d) (drive-target-level d))
                "UNSATISFIED" "ok")))))

(defun dump-goals ()
  "Dump current goal state"
  (diag "GOALS" "=== Goal State ===")
  (diag "GOALS" "  Active: ~D" (length *goal-stack*))
  (diag "GOALS" "  History: ~D" (length *goal-history*))
  (dolist (g *goal-stack*)
    (diag "GOALS" "    ~A: ~A" (goal-type g) (goal-description g))))

;;; ============================================================================
;;; PRESENCE TRACKING
;;; ============================================================================

(defun dump-presence ()
  "Dump current presence state"
  (diag "PRESENCE" "=== Presence State ===")
  (when (boundp '*presence*)
    (diag "PRESENCE" "  Trajectory: ~A" (presence-trajectory *presence*))
    (diag "PRESENCE" "  Agency: ~,2F" (presence-agency *presence*))
    (diag "PRESENCE" "  Continuity: ~,2F" (presence-continuity *presence*))
    (diag "PRESENCE" "  Self-confidence: ~,2F" (presence-self-confidence *presence*))
    (diag "PRESENCE" "  Textures: ~A" (mapcar #'car (presence-textures *presence*)))))

;;; ============================================================================
;;; EXPERIMENT TRACKING
;;; ============================================================================

(defun dump-experiments ()
  "Dump experiment state"
  (diag "EXPERIMENTS" "=== Experiment State ===")
  (diag "EXPERIMENTS" "  *experimentation-enabled*: ~A" 
        (if (boundp '*experimentation-enabled*) *experimentation-enabled* 'UNBOUND))
  (diag "EXPERIMENTS" "  Active: ~D" (length *experiments*))
  (diag "EXPERIMENTS" "  History: ~D" (length *experiment-history*)))

;;; ============================================================================
;;; TRACE BUFFER AND LTM TRACKING
;;; ============================================================================

(defun dump-memory ()
  "Dump memory state"
  (diag "MEMORY" "=== Memory State ===")
  (diag "MEMORY" "  Traces: ~D" (if (boundp '*trace-buffer*) (fill-pointer *trace-buffer*) 0))
  (diag "MEMORY" "  LTM: ~D" (hash-table-count *long-term-memory*))
  (diag "MEMORY" "  Schemas: ~D" (hash-table-count *cognitive-schemas*))
  (when (boundp '*dream-state*)
    (diag "MEMORY" "  Dreams: ~D" (dream-state-total-dreams *dream-state*))))

;;; ============================================================================
;;; INSTALL ALL WRAPPERS
;;; ============================================================================

(defun install-all-wrappers ()
  "Install all diagnostic wrappers"
  (diag "SYSTEM" "Installing diagnostic wrappers...")
  (wrap-generate-hypothesis!)
  (wrap-test-hypothesis!)
  (wrap-evaluate-claim!)
  (wrap-execute-self-modification!)
  (wrap-deep-mind-maintenance!)
  (diag "SYSTEM" "Wrappers installed"))

;;; ============================================================================
;;; FULL DIAGNOSTIC RUN
;;; ============================================================================

(defun run-full-diagnostic (&key (iterations 30) 
                                 (text "The cat sat on the mat. The dog ran in the park.")
                                 (filename "full-diagnostic.txt"))
  "Run a complete diagnostic capturing everything"
  (format t "~%~%")
  (format t "╔══════════════════════════════════════════════════════════════════╗~%")
  (format t "║           UHMA FULL DIAGNOSTIC RUN                               ║~%")
  (format t "╚══════════════════════════════════════════════════════════════════╝~%~%")
  
  ;; Initialize
  (reset!)
  (initialize-v65-agentic!)
  (initialize-presence-integration!)
  
  ;; Start diagnostics
  (start-diag filename)
  (install-all-wrappers)
  
  ;; Dump initial state
  (diag "SYSTEM" "========== INITIAL STATE ==========")
  (dump-experts)
  (dump-hypotheses)
  (dump-drives)
  (dump-goals)
  (dump-presence)
  (dump-experiments)
  (dump-memory)
  
  ;; Run training
  (diag "SYSTEM" "========== TRAINING (~D iterations) ==========" iterations)
  (let ((experts-before (track-expert-changes)))
    (dotimes (i iterations)
      (when (zerop (mod i 10))
        (diag "PROGRESS" "--- Iteration ~D/~D ---" i iterations))
      (process-text! text))
    
    ;; Check for any expert changes during entire run
    (let ((experts-after (track-expert-changes)))
      (diag "SYSTEM" "========== EXPERT CHANGES DURING RUN ==========")
      (compare-expert-states experts-before experts-after)))
  
  ;; Dump final state
  (diag "SYSTEM" "========== FINAL STATE ==========")
  (dump-experts)
  (dump-hypotheses)
  (dump-drives)
  (dump-goals)
  (dump-presence)
  (dump-experiments)
  (dump-memory)
  
  ;; Dump detail on executed hypotheses
  (diag "SYSTEM" "========== EXECUTED HYPOTHESIS DETAILS ==========")
  (maphash (lambda (k v)
             (declare (ignore k))
             (when (eq (self-hypothesis-status v) :executed)
               (dump-hypothesis-detail v)))
           *hypotheses*)
  
  ;; Summary
  (diag "SYSTEM" "========== SUMMARY ==========")
  (diag "SYSTEM" "Step: ~D" *step*)
  (diag "SYSTEM" "Experts: ~D" (length *experts*))
  (let ((tested 0) (confirmed 0) (executed 0))
    (maphash (lambda (k v)
               (declare (ignore k))
               (when (> (self-hypothesis-times-tested v) 0) (incf tested))
               (when (eq (self-hypothesis-status v) :confirmed) (incf confirmed))
               (when (eq (self-hypothesis-status v) :executed) (incf executed)))
             *hypotheses*)
    (diag "SYSTEM" "Hypotheses: ~D total, ~D tested, ~D confirmed, ~D executed"
          (hash-table-count *hypotheses*) tested confirmed executed))
  (diag "SYSTEM" "Experiments: ~D" (length *experiments*))
  (diag "SYSTEM" "Goals completed: ~D" (length *goal-history*))
  
  ;; Check for common problems
  (diag "SYSTEM" "========== PROBLEM DETECTION ==========")
  
  ;; Check if hypotheses are duplicated
  (let ((claims (make-hash-table :test 'equal))
        (duplicates 0))
    (maphash (lambda (k v)
               (declare (ignore k))
               (incf (gethash (self-hypothesis-claim v) claims 0)))
             *hypotheses*)
    (maphash (lambda (claim count)
               (declare (ignore claim))
               (when (> count 1) (incf duplicates)))
             claims)
    (if (> duplicates 0)
        (diag "PROBLEM" "!!! ~D duplicate hypothesis claims - deduplication not working" duplicates)
        (diag "OK" "No duplicate hypothesis claims")))
  
  ;; Check if self-modifications had effect
  (let ((any-threshold-changed nil))
    (dolist (e *experts*)
      (unless (= (expert-confidence-threshold e) 0.3)
        (setf any-threshold-changed t)))
    (if any-threshold-changed
        (diag "OK" "Expert thresholds were modified")
        (diag "PROBLEM" "!!! No expert thresholds changed - self-modification not working")))
  
  ;; Check presence
  (when (boundp '*presence*)
    (when (= (presence-continuity *presence*) 0.0)
      (diag "PROBLEM" "!!! Presence continuity is 0 - something resetting it"))
    (when (= (presence-self-confidence *presence*) 0.0)
      (diag "PROBLEM" "!!! Presence self-confidence is 0")))
  
  ;; Check experiments
  (when (= (length *experiments*) 0)
    (diag "PROBLEM" "!!! No experiments generated - experiment system not working"))
  
  ;; Stop diagnostics
  (stop-diag)
  
  (format t "~%Diagnostic complete. Output written to ~A~%" filename)
  (format t "Total entries: ~D~%" (length *diag-log*)))

;;; ============================================================================
;;; QUICK CHECKS
;;; ============================================================================

(defun check-claim-format ()
  "Quick check that claims are lists not strings"
  (let ((strings 0) (lists 0))
    (maphash (lambda (k v)
               (declare (ignore k))
               (if (stringp (self-hypothesis-claim v))
                   (incf strings)
                   (incf lists)))
             *hypotheses*)
    (format t "Claims: ~D strings (BAD), ~D lists (GOOD)~%" strings lists)))

(defun check-executed-actions ()
  "Show what actions were executed"
  (let ((actions (make-hash-table :test 'equal)))
    (maphash (lambda (k v)
               (declare (ignore k))
               (when (eq (self-hypothesis-status v) :executed)
                 (incf (gethash (self-hypothesis-proposed-action v) actions 0))))
             *hypotheses*)
    (format t "Executed actions:~%")
    (maphash (lambda (action count)
               (format t "  ~S: ~D times~%" action count))
             actions)))

(defun check-why-no-threshold-change ()
  "Debug why thresholds aren't changing"
  (format t "~%=== Debugging threshold changes ===~%")
  (format t "Experts with hits > 0:~%")
  (let ((any nil))
    (dolist (e *experts*)
      (when (> (expert-hits e) 0)
        (setf any t)
        (format t "  E~A: hits=~D thresh=~,2F~%" 
                (expert-id e) (expert-hits e) (expert-confidence-threshold e))))
    (unless any
      (format t "  NONE - this is the problem!~%")
      (format t "  Experts exist but none have hits > 0~%")
      (format t "  The routing system may be using different expert tracking~%"))))

;;; ============================================================================
;;; LOADED MESSAGE
;;; ============================================================================

(format t "~%")
(format t "════════════════════════════════════════════════════════════════════~%")
(format t "UHMA Diagnostic System v2 Loaded~%")
(format t "════════════════════════════════════════════════════════════════════~%")
(format t "~%Main command:~%")
(format t "  (run-full-diagnostic)    ; Full trace to file~%")
(format t "  (run-full-diagnostic :iterations 10)  ; Quick test~%")
(format t "~%Quick checks:~%")
(format t "  (check-claim-format)     ; Are claims lists or strings?~%")
(format t "  (check-executed-actions) ; What actions were executed?~%")
(format t "  (check-why-no-threshold-change)  ; Debug threshold issue~%")
(format t "~%State dumps:~%")
(format t "  (dump-experts)           ; Expert state~%")
(format t "  (dump-hypotheses)        ; Hypothesis state~%")
(format t "  (dump-drives)            ; Drive state~%")
(format t "  (dump-presence)          ; Presence state~%")
(format t "════════════════════════════════════════════════════════════════════~%")
