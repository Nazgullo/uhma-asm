;;;; FILE MANIFEST
;;;; =============
;;;; This file MUST contain the following when complete:
;;;; PACKAGES: uhma
;;;; FUNCTIONS: start!, status, learning-progress
;;;; TOTAL EXPECTED SECTIONS: Unknown (Legacy File)

;;;; UHMA Quick Start
;;;; Convenience functions for interactive use

(in-package :uhma)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize (speed 0) (safety 3) (debug 3))))

(defun start! (&key (demo t))
  "Initialize the full UHMA system and optionally run a demo."
  (format t "~%Initializing UHMA...~%")
  (reset!)
  (initialize-cognitive-control!)
  (initialize-v65-agentic!)
  (initialize-pattern-utilization!)
  ;; Initialize presence substrate
  (when (fboundp 'initialize-presence-integration!)
    (initialize-presence-integration!))
  ;; Initialize self-modification system
  (when (fboundp 'initialize-v64-enhancements!)
    (initialize-v64-enhancements!))
  ;; Initialize deeper mind (hypotheses, self-theories)
  (when (fboundp 'initialize-deeper-mind!)
    (initialize-deeper-mind!))
  ;; Initialize introspective grounding
  (when (fboundp 'initialize-introspective-grounding!)
    (initialize-introspective-grounding!))
  ;; Final step: Install complete system wiring
  (when (fboundp 'install-complete-wiring!)
    (install-complete-wiring!))
  (format t "~%System ready.~%")
  (when demo
    (format t "~%Running demo with sample corpus...~%~%")
    (demo-run))
  (values))

(defun status ()
  "Print comprehensive system status."
  (format t "~%========== UHMA SYSTEM STATUS ==========~%~%")
  
  ;; Core stats
  (format t "CORE:~%")
  (format t "  Experts: ~D~%" (length *experts*))
  (format t "  Travelers: ~D~%" (length *travelers*))
  (format t "  Neighborhoods: ~D~%" (length *neighborhoods*))
  (format t "  LTM entries: ~D~%" (hash-table-count *long-term-memory*))
  (format t "  Total births: ~D~%" *total-births*)
  (format t "  Total deaths: ~D~%" *total-deaths*)
  (format t "  Merges: ~D~%" *experts-merged*)
  
  ;; Cognitive state
  (format t "~%COGNITIVE:~%")
  (format t "  Traces: ~D~%" (fill-pointer *trace-buffer*))
  (format t "  Hypotheses: ~D~%" (hash-table-count *hypotheses*))
  (format t "  Schemas: ~D~%" (hash-table-count *cognitive-schemas*))
  
  ;; Drives
  (format t "~%DRIVES:~%")
  (dolist (d *intrinsic-drives*)
    (format t "  ~A: ~,1F% (target ~,0F%)~%"
            (drive-name d) 
            (* 100 (drive-current-level d))
            (* 100 (drive-target-level d))))
  
  ;; Goals
  (format t "~%GOALS:~%")
  (format t "  Active: ~D~%" (length *goal-stack*))
  (format t "  Completed: ~D~%" (length *goal-history*))
  
  (format t "~%=========================================~%")
  (values))

(defun learning-progress ()
  "Show metrics indicating whether the system is getting smarter.
   These metrics track improvement over time, not just current state."
  (format t "~%======== LEARNING PROGRESS ========~%~%")

  ;; 1. External Prediction Accuracy
  (format t "PREDICTION ACCURACY (external inputs only):~%")
  (if (and (boundp '*external-outcomes*) *external-outcomes*)
      (let* ((total (length *external-outcomes*))
             (correct (count-if #'cdr *external-outcomes*))
             (accuracy (if (> total 0) (/ (* 100.0 correct) total) 0)))
        (format t "  Last ~D predictions: ~,1F% accurate~%" total accuracy)
        (format t "  ~:[STRUGGLING~;COMPETENT~] at current inputs~%" (> accuracy 60)))
      (format t "  No external data yet~%"))

  ;; 2. Knowledge Efficiency (more coverage with less storage)
  (format t "~%KNOWLEDGE EFFICIENCY:~%")
  (let* ((ltm-count (hash-table-count *long-term-memory*))
         (pattern-count (hash-table-count *pattern-stats*))
         (efficiency (if (> ltm-count 0) (/ pattern-count ltm-count 1.0) 0)))
    (format t "  LTM entries: ~D~%" ltm-count)
    (format t "  Patterns tracked: ~D~%" pattern-count)
    (format t "  Coverage ratio: ~,1F patterns/entry~%" efficiency)
    (format t "  ~:[INEFFICIENT~;EFFICIENT~] storage~%" (< ltm-count (* pattern-count 0.5))))

  ;; 3. Expert Specialization
  (format t "~%EXPERT SPECIALIZATION:~%")
  (when *experts*
    (let* ((expert-count (length *experts*))
           (total-knowledge 0)
           (total-scope 0))
      (dolist (e *experts*)
        (incf total-knowledge (hash-table-count (expert-knowledge e)))
        (incf total-scope (length (expert-owned-contexts e))))
      (format t "  Experts: ~D~%" expert-count)
      (format t "  Avg knowledge/expert: ~,1F~%" (if (> expert-count 0) (/ total-knowledge expert-count 1.0) 0))
      (format t "  Avg contexts/expert: ~,1F~%" (if (> expert-count 0) (/ total-scope expert-count 1.0) 0))
      (format t "  Birth/death ratio: ~,1F~%"
              (if (> *total-deaths* 0) (/ *total-births* *total-deaths* 1.0) *total-births*))))

  ;; 4. Self-Modification Activity
  (format t "~%SELF-MODIFICATION:~%")
  (when (and (boundp '*self-modification-history*) *self-modification-history*)
    (let* ((mods *self-modification-history*)
           (total (length mods))
           (executed (count :executed mods :key (lambda (m) (getf m :status))))
           (success-rate (if (> total 0) (/ (* 100.0 executed) total) 0)))
      (format t "  Modifications attempted: ~D~%" total)
      (format t "  Successfully executed: ~D~%" executed)
      (format t "  Execution rate: ~,1F%~%" success-rate)))

  ;; 5. Hypothesis Formation & Testing
  (format t "~%HYPOTHESIS ACTIVITY:~%")
  (when (and (boundp '*hypotheses*) (hash-table-p *hypotheses*))
    (let ((total 0) (confirmed 0) (tested 0))
      (maphash (lambda (k h)
                 (declare (ignore k))
                 (incf total)
                 (when (and (self-hypothesis-p h)
                            (> (self-hypothesis-times-tested h) 0))
                   (incf tested)
                   (when (> (self-hypothesis-confidence h) 0.7)
                     (incf confirmed))))
               *hypotheses*)
      (format t "  Total hypotheses: ~D~%" total)
      (format t "  Tested: ~D, Confirmed: ~D~%" tested confirmed)
      (when (> tested 0)
        (format t "  Confirmation rate: ~,1F%~%"
                (/ (* 100.0 confirmed) tested)))))

  ;; 6. Schema Formation (higher-level abstractions)
  (format t "~%SCHEMA FORMATION:~%")
  (when (and (boundp '*cognitive-schemas*) (hash-table-p *cognitive-schemas*))
    (let ((compiled 0) (total 0))
      (maphash (lambda (k s)
                 (declare (ignore k))
                 (incf total)
                 (when (and (listp s) (getf s :compiled))
                   (incf compiled)))
               *cognitive-schemas*)
      (format t "  Schemas discovered: ~D~%" total)
      (format t "  Compiled (actionable): ~D~%" compiled)))

  ;; Overall Assessment
  (format t "~%======== ASSESSMENT ========~%")
  (let ((smart-indicators 0))
    ;; Check each indicator
    (when (and (boundp '*external-outcomes*) *external-outcomes*
               (> (count-if #'cdr *external-outcomes*)
                  (* 0.6 (length *external-outcomes*))))
      (incf smart-indicators))
    (when (and *experts* (> (length *experts*) 3))
      (incf smart-indicators))
    (when (> (hash-table-count *long-term-memory*) 0)
      (incf smart-indicators))
    (when (and (boundp '*hypotheses*) (> (hash-table-count *hypotheses*) 0))
      (incf smart-indicators))
    (format t "~%  Smart indicators: ~D/4~%" smart-indicators)
    (format t "  System is ~:[still learning~;showing intelligence~]~%"
            (>= smart-indicators 3)))

  (format t "~%===================================~%")
  (values))

(defun demo-run ()
  "Run a demonstration with mixed content."
  (let ((corpus '(
    ;; Mind/consciousness
    "consciousness emerges from integrated information"
    "attention selects relevant signals from noise"
    "working memory holds items for manipulation"
    "the brain constructs models of reality"
    ;; Code patterns
    "defun creates a new function"
    "lambda makes anonymous functions"
    "mapcar applies function to list"
    ;; Poetry
    "the fog comes on little cat feet"
    "two roads diverged in yellow wood"
    "i wandered lonely as a cloud"
    ;; Science
    "neurons transmit signals through synapses"
    "mitochondria generate cellular energy"
    "dna encodes genetic information")))
    
    (format t "Training on ~D sentences...~%~%" (length corpus))
    (dotimes (i 200)
      (process-text! (nth (mod i (length corpus)) corpus))
      (when (zerop (mod (1+ i) 50))
        (format t "  Step ~D: ~D experts, ~D LTM~%"
                (1+ i) (length *experts*) 
                (hash-table-count *long-term-memory*))))
    
    (format t "~%Generation samples:~%")
    (dolist (seed '((the) (neurons) (defun) (consciousness)))
      (format t "  ~A -> ~A~%" seed (generate seed :length 5)))
    
    (format t "~%")
    (status)))

(defun train (text &optional (iterations 1))
  "Train the system on text for given iterations."
  (dotimes (i iterations)
    (process-text! text))
  (format t "Processed ~D iteration~:P~%" iterations))

(defun think-about (context)
  "Show the system's thinking about a context."
  (let ((result (think context)))
    (format t "~%Context: ~A~%" context)
    (format t "Prediction: ~A~%" (thinking-result-prediction result))
    (format t "Confidence: ~,1F%~%" (* 100 (thinking-result-confidence result)))
    (format t "Strategy: ~A~%" (thinking-result-strategy-used result))
    (format t "Reasoning: ~A~%" (thinking-result-reasoning-trace result))
    result))

(defun introspect ()
  "Have the system describe itself."
  (format t "~%")
  (describe-self)
  (format t "~%Current state: ")
  (describe-current-state)
  (format t "~%"))

;; Export convenience functions
(export '(start! status learning-progress demo-run train think-about introspect))

;;;; [SECTION-START:99:VERIFICATION]
;;; Section 99: File Verification

(defun verify-start-lisp-completeness ()
  "Verify completeness."
  (unless (find-package :uhma)
    (error "Package UHMA not defined"))
  (unless (fboundp 'start!)
    (error "Function start! not defined"))
  (format t "~&start.lisp verification passed.~%"))

(verify-start-lisp-completeness)

;;;; [SECTION-END:99:VERIFICATION]
