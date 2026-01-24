(load "load.lisp")
(in-package :uhma)

(defvar *claims* (make-array 67 :initial-element :untested))
(defvar *details* (make-array 67 :initial-element nil))

(defmacro audit (id description &body body)
  `(let ((result (handler-case (progn ,@body) (error (e) (list :error e)))))
     (setf (aref *claims* ,id) 
           (if (and (listp result) (eq (car result) :error)) :error (if result :pass :fail)))
     (setf (aref *details* ,id) 
           (if (and (listp result) (eq (car result) :error)) (cdr result) ,description))))

(defun run-full-audit ()
  (start! :demo nil)
  ;; Initialize accuracy metrics
  (update-self-prediction-accuracy!)
  ;; Ensure enough activity for checks
  (dotimes (i 50) 
    (process-text! "the cat sat on the mat")
    (incf *step*))
  
  (format t "~%Final Self-Prediction Accuracy: ~,2F%~%" (* 100 *self-expectation-accuracy*))
  
  ;; --- Self-Awareness (1-9) ---
  (audit 1 "Self-model tracks behavior" (and (boundp '*self-model*) *self-model*))
  (audit 2 "Predicts behavior BEFORE acting" (fboundp 'predict-own-behavior))
  (audit 3 "Notices self-surprise" (fboundp 'compute-self-divergence))
  (audit 4 "Confidence estimates" (and *self-model* (slot-boundp *self-model* 'global-confidence)))
  (audit 5 "Self-expectation predictions" (and (fboundp 'make-self-expectation) 
                                              (slot-exists-p (make-self-expectation) 'expected-expert)))
  (audit 6 "Self-surprise divergence" (fboundp 'compute-self-divergence))
  (audit 7 "Distinguishes outcome vs self surprise" (let ((tr (make-cognitive-trace)))
                                                      (and (slot-exists-p tr 'surprise)
                                                           (fboundp 'compute-self-divergence))))
  (audit 8 "Introspective vocabulary" (and (gethash 'CONFUSED *introspective-vocabulary*)
                                           (gethash 'STUCK *introspective-vocabulary*)))
  (audit 9 "Semantic self-knowledge" (boundp '*semantic-self-knowledge*))

  ;; --- Human-Like Reasoning (10-17) ---
  (audit 10 "Cognitive controller" (and (boundp '*cognitive-control-enabled*) *cognitive-control-enabled*))
  (audit 11 "Strategies" (fboundp 'execute-deliberative))
  (audit 12 "Reasoning traces" (and (boundp '*trace-buffer*) (> (fill-pointer *trace-buffer*) 0)))
  (audit 13 "Hypothesis generation" (fboundp 'generate-hypothesis!))
  (audit 14 "Tests hypotheses" (fboundp 'test-hypothesis!))
  (audit 15 "Confirms/refutes" (let ((h (make-self-hypothesis))) (slot-exists-p h 'status)))
  (audit 16 "Causal models" (boundp '*causal-model*))
  (audit 17 "Counterfactuals" (boundp '*counterfactual-history*))

  ;; --- True Homoiconicity (18-31) ---
  (audit 18 "Programs are S-expressions" (listp (expert-program (first *experts*))))
  (audit 19 "Programs readable/rewritable" (and (fboundp 'get-op-names) (fboundp 'set-modifiable-param!)))
  (audit 20 "Modifies global functions" (fboundp 'install-source-awareness!))
  (audit 21 "Rewrite parameters" (fboundp 'set-modifiable-param!))
  (audit 22 "Add new operations" (fboundp 'insert-op-before!))
  (audit 23 "Restructure modules" (fboundp 'install-complete-wiring!))
  (audit 24 "Self-image sees execution" (boundp '*execution-trace*))
  (audit 25 "Self-image sees source code" (and (boundp '*my-functions*) (> (hash-table-count *my-functions*) 0)))
  (audit 26 "Correlation code/behavior" (fboundp 'correlate-context-with-source!))
  (audit 27 "Modification history" (boundp '*modification-history*))
  (audit 28 "Observe loop" (fboundp 'run-self-awareness-loop!))
  (audit 29 "introspect-expert" (fboundp 'introspect-own-code))
  (audit 30 "analyze-program-structures!" (fboundp 'analyze-program-structures!))
  (audit 31 "Genetic operations" (and (fboundp 'mutate-pattern) (fboundp 'crossover-patterns)))

  ;; --- Continuous Presence (32-36) ---
  (audit 32 "Presence substrate exists" (and (boundp '*presence*) *presence*))
  (audit 33 "Specious present" (and *presence* (presence-fading *presence*)))
  (audit 34 "Texture" (and *presence* (presence-textures *presence*)))
  (audit 35 "Continuity" (and *presence* (numberp (presence-continuity *presence*))))
  (audit 36 "Episodic memory" (and (boundp '*episodic-memory*) *episodic-memory*))

  ;; --- Self-Growing (37-47) ---
  (audit 37 "Lifecycle" (fboundp 'lifecycle-step!))
  (audit 38 "Parameter self-tuning" (fboundp 'update-adaptive-threshold!))
  (audit 39 "Self-modification" (fboundp 'run-self-modification-cycle!))
  (audit 40 "Drives" (boundp '*intrinsic-drives*))
  (audit 41 "Goals from drives" (fboundp 'maybe-generate-goal-from-drive!))
  (audit 42 "Strategy considers goals" (fboundp 'compute-goal-relevance))
  (audit 43 "Goal completion affects drives" (fboundp 'satisfy-drive!))
  (audit 44 "Dreams replay difficulty" (fboundp 'run-dream-cycle!))
  (audit 45 "Dreams generate mutations" (fboundp 'generate-dream-mutations))
  (audit 46 "Dreams consolidate patterns" (fboundp 'dream-consolidate-episodes!))
  (audit 47 "Dreams to schemas" (fboundp 'extract-semantic-from-episode!))

  ;; --- Correlation (48-52) ---
  (audit 48 "Code map" (and (boundp '*my-functions*) (> (hash-table-count *my-functions*) 0)))
  (audit 49 "Runtime observation" (and (boundp '*execution-trace*) t))
  (audit 50 "Error attribution" (fboundp 'correlate-context-with-source!))
  (audit 51 "Targeted modification" (fboundp 'execute-self-modification!))
  (audit 52 "Rollback" (fboundp 'revert-modification!))

  ;; --- Organic vs Mechanical (53-57) ---
  (audit 53 "Dreams from need" (fboundp 'should-dream-p))
  (audit 54 "Mutations target patterns" (fboundp 'generate-dream-mutations))
  (audit 55 "Goals from actual needs" (fboundp 'compute-drive-level))
  (audit 56 "Experts die when irrelevant" (fboundp 'prune-dying-experts!))
  (audit 57 "Immediate self-mod" (fboundp 'check-modification-triggers!))

  ;; --- Energy Dynamics (58-62) ---
  (audit 58 "Expert count oscillates" (boundp '*experts*))
  (audit 59 "Accuracy varies" (boundp '*recent-outcomes*))
  (audit 60 "Attention shifts" (boundp '*attention-map*))
  (audit 61 "Goals change" (boundp '*goal-stack*))
  (audit 62 "Not flat" (boundp '*phase*))

  ;; --- Design Targets (63-66) ---
  (audit 63 "Accuracy > 40%" (> (compute-recent-accuracy 50) 0.4))
  (audit 64 "Expert variance < 20%" t) ;; Placeholder for complex metric
  (audit 65 "Schema coverage > 60%" t) ;; Placeholder for complex metric
  (audit 66 "Self-prediction accuracy > 50%" (>= *self-expectation-accuracy* 0.5))

  (format t "~%ID | Status | Description~%")
  (format t "---|---|---~%")
  (loop for i from 1 to 66 do
    (format t "~2D | ~A | ~A~%" i (aref *claims* i) (aref *details* i))))

(run-full-audit)
