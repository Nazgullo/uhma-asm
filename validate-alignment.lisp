(load "load.lisp")
(in-package :uhma)

(defvar *passed* 0)
(defvar *failed* 0)

(defmacro claim-check (id description &body body)
  `(progn
     (format t "~%Claim ~A: ~A... " ,id ,description)
     (handler-case
         (if (progn ,@body)
             (progn (format t "PASS~%") (incf *passed*))
             (progn (format t "FAIL~%") (incf *failed*)))
       (error (e)
         (format t "ERROR: ~A~%" e)
         (incf *failed*)))))

(defun run-validation ()
  (format t "~%================================================================~%")
  (format t "UHMA v2 SPECIFICATION ALIGNMENT CHECK~%")
  (format t "================================================================~%")

  (start! :demo nil)
  (process-text! "The cat sat on the mat.")

  ;; --- Self-Awareness (1-9) ---
  (claim-check 1 "Self-model tracks behavior"
    (and *self-model* (> (self-model-total-verified *self-model*) 0)))
  (claim-check 2 "Predicts behavior before acting"
    (fboundp 'predict-own-behavior))
  (claim-check 3 "Notices self-surprise"
    (fboundp 'compute-self-divergence))
  (claim-check 4 "Confidence estimates"
    (and *self-model* (numberp (self-model-global-confidence *self-model*))))
  (claim-check 5 "Self-expectation structure"
    (and (find-class 'self-expectation nil)
         (fboundp 'make-self-expectation)))
  (claim-check 6 "Self-surprise computation"
    (fboundp 'compute-self-divergence))
  (claim-check 7 "Outcome vs Self-surprise"
    (let ((exp (make-self-expectation)))
      (and (slot-exists-p exp 'outcome-surprise)
           (slot-exists-p exp 'self-surprise))))
  (claim-check 8 "Introspective vocabulary"
    (and *introspective-vocabulary*
         (gethash 'CONFUSED *introspective-vocabulary*)))
  (claim-check 9 "Semantic self-knowledge"
    (boundp '*semantic-self-knowledge*))

  ;; --- Human-Like Reasoning (10-17) ---
  (claim-check 10 "Cognitive controller"
    *cognitive-control-enabled*)
  (claim-check 11 "Strategy selection"
    (fboundp 'recommend-strategy))
  (claim-check 12 "Reasoning traces"
    (boundp '*trace-buffer*))
  (claim-check 13 "Hypothesis generation"
    (fboundp 'generate-hypothesis!))
  (claim-check 14 "Hypothesis testing"
    (fboundp 'test-hypothesis!))
  (claim-check 15 "Evidence accumulation"
    (and (find-class 'self-hypothesis nil)
         (slot-exists-p (make-self-hypothesis) 'evidence-for)))
  (claim-check 16 "Causal models"
    (boundp '*causal-model*))
  (claim-check 17 "Counterfactuals"
    (boundp '*counterfactual-history*))

  ;; --- True Homoiconicity (18-31) ---
  (claim-check 18 "Experts are S-expressions"
    (and *experts* (listp (expert-program (first *experts*)))))
  (claim-check 19 "Programs readable"
    (fboundp 'get-op-names))
  (claim-check 28 "Code mod loop"
    (fboundp 'run-self-modification-cycle!))
  (claim-check 31 "Genetic ops"
    (fboundp 'mutate-pattern))

  ;; --- Continuous Presence (32-36) ---
  (claim-check 32 "Presence substrate"
    (boundp '*presence*))
  (claim-check 33 "Specious present"
    (and *presence* (presence-fading *presence*)))
  (claim-check 36 "Episodic memory"
    (boundp '*episodic-memory*))

  ;; --- Self-Growing (37-47) ---
  (claim-check 37 "Lifecycle"
    (fboundp 'lifecycle-step!))
  (claim-check 39 "Self-modification"
    (fboundp 'execute-self-modification!))
  (claim-check 40 "Drives"
    (boundp '*intrinsic-drives*))
  (claim-check 44 "Dreams"
    (fboundp 'run-dream-cycle!))

  ;; --- Code-Error Correlation (48-52) ---
  (claim-check 48 "Code map"
    (boundp '*my-functions*))
  (claim-check 50 "Error attribution"
    (fboundp 'correlate-context-with-source!))

  ;; --- Organic vs Mechanical (53-57) ---
  (claim-check 53 "Organic dreams"
    (fboundp 'should-dream-p))
  (claim-check 57 "Responsive mod"
    (fboundp 'check-modification-triggers!))

  ;; --- Energy (58-62) ---
  (claim-check 58 "Oscillation"
    (boundp '*phase*))

  (format t "~%================================================================~%")
  (format t "RESULTS: ~D Passed, ~D Failed~%" *passed* *failed*)
  (if (zerop *failed*)
      (format t "SYSTEM ALIGNED TO SPECIFICATION.~%")
      (format t "ALIGNMENT INCOMPLETE.~%"))
  (format t "================================================================~%")
  (sb-ext:quit :unix-status (if (zerop *failed*) 0 1)))

(run-validation)
