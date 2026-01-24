;;;; ============================================================================
;;;; UHMA PREDICTIVE SELF-MODIFICATION
;;;; ============================================================================
;;;; The system predicts its own modifications before making them.
;;;; A simple predictor becomes a perfect predictor that even predicts itself.
;;;;
;;;; Philosophy:
;;;;   - Self-modification is not random-until-something-works
;;;;   - Before any modification, predict what it will produce
;;;;   - Compare prediction to reality after execution
;;;;   - As self-prediction accuracy improves, trust in modifications grows
;;;;   - The system converges toward only making changes it understands
;;;;   - Prediction IS understanding. Understanding IS prediction.
;;;;
;;;; Architecture:
;;;;   predict-modification-outcome → modification-prediction struct
;;;;                                      ↓
;;;;                            gate: can I predict this? (confidence)
;;;;                                      ↓ yes
;;;;                            snapshot-for-rollback!
;;;;                                      ↓
;;;;                            execute modification
;;;;                                      ↓
;;;;                            verify-modification-prediction!
;;;;                                      ↓
;;;;                      prediction accurate?
;;;;                        yes → strengthen self-model, keep change
;;;;                        no  → rollback, update what I got wrong
;;;;
;;;; ============================================================================

(in-package :uhma)

;;; ============================================================================
;;; SECTION 1: STRUCTURES
;;; ============================================================================

(defstruct modification-prediction
  "A prediction about what a self-modification will produce."
  (id (gensym "MPRED-") :type symbol)
  (step 0 :type fixnum)
  ;; What modification is being attempted
  (action-type nil :type symbol)
  (action nil)
  ;; Predicted outcomes
  (predicted-accuracy-delta 0.0 :type single-float)  ; expected change in accuracy
  (predicted-direction :neutral :type symbol)         ; :improving :degrading :neutral
  (predicted-scope :local :type symbol)               ; :local :global :structural
  (predicted-risk :low :type symbol)                  ; :low :medium :high
  (predicted-reversible t :type boolean)              ; can we undo this?
  ;; Basis for prediction (why I think this)
  (basis nil :type list)                              ; evidence supporting prediction
  (similar-past-outcomes nil :type list)              ; history of similar mods
  (self-model-consistency 0.5 :type single-float)     ; how consistent with self-model
  ;; Actual outcomes (filled in after execution)
  (actual-accuracy-delta nil)                         ; nil until verified
  (actual-direction nil :type (or null symbol))
  (prediction-error nil)                              ; nil until verified
  (verified-at nil :type (or null fixnum))
  ;; Meta
  (confidence 0.5 :type single-float)                 ; how confident in this prediction
  (prediction-correct nil :type boolean))

;;; ============================================================================
;;; SECTION 2: STATE
;;; ============================================================================

(defvar *modification-predictions* (make-array 100 :fill-pointer 0 :adjustable t)
  "Ring buffer of modification predictions for self-learning.")

(defvar *self-modification-accuracy* 0.5
  "Running accuracy of modification predictions. THE key convergence metric.")

(defvar *modification-type-models* (make-hash-table :test 'eq)
  "Per-type models: what does each modification type typically produce?
   Maps action-type → (list :mean-delta F :variance F :count N :successes N)")

(defvar *predictive-gate-threshold* 0.3
  "Minimum prediction confidence to allow a modification.
   Starts low (allow exploration), rises as self-model improves.")

(defvar *self-prediction-convergence-history* nil
  "Track how self-modification-accuracy evolves over time.
   List of (step . accuracy) pairs. Rising = convergence.")

(defvar *predictive-self-mod-enabled* t
  "Master switch for predictive gating.")

(defvar *exploration-budget* 5
  "Number of unpredicted modifications allowed per 500 steps.
   Decreases as self-prediction improves (less need to explore blindly).")

(defvar *exploration-count* 0
  "How many unpredicted modifications made in current window.")

(defvar *exploration-window-start* 0
  "Step at which current exploration window started.")

;;; ============================================================================
;;; SECTION 3: PREDICTION GENERATION
;;; ============================================================================

(defun predict-modification-outcome (action-type action)
  "Generate a prediction about what this modification will produce.
   This IS the system predicting itself."
  (let* ((type-model (gethash action-type *modification-type-models*))
         (similar-outcomes (find-similar-modification-outcomes action-type))
         (current-acc (compute-recent-accuracy-fast 50))
         (self-model-score (assess-self-model-consistency action-type action))
         ;; Predict the accuracy delta based on history
         (predicted-delta (predict-accuracy-delta type-model similar-outcomes action))
         ;; Predict direction from delta
         (predicted-direction (cond ((> predicted-delta 0.03) :improving)
                                    ((< predicted-delta -0.03) :degrading)
                                    (t :neutral)))
         ;; Assess risk based on scope and reversibility
         (predicted-scope (assess-modification-scope action-type action))
         (predicted-risk (assess-modification-risk action-type predicted-delta predicted-scope))
         ;; Confidence in this prediction
         (confidence (compute-prediction-confidence type-model similar-outcomes
                                                    self-model-score current-acc)))
    (make-modification-prediction
     :step *step*
     :action-type action-type
     :action action
     :predicted-accuracy-delta predicted-delta
     :predicted-direction predicted-direction
     :predicted-scope predicted-scope
     :predicted-risk predicted-risk
     :predicted-reversible (if (member action-type '(raise-confidence-threshold
                                                       lower-confidence-threshold
                                                       increase-exploration
                                                       increase-verification
                                                       review-schema-application
                                                       MODIFY-PARAM modify-param))
                              t nil)
     :basis (list :type-model type-model
                  :similar-count (length similar-outcomes)
                  :current-accuracy current-acc
                  :self-model-score self-model-score)
     :similar-past-outcomes similar-outcomes
     :self-model-consistency self-model-score
     :confidence confidence)))

(defun predict-accuracy-delta (type-model similar-outcomes action)
  "Predict the accuracy change this modification will produce."
  (declare (ignore action))
  (cond
    ;; Have a type model with enough data: use its mean
    ((and type-model (> (getf type-model :count 0) 3))
     (getf type-model :mean-delta 0.0))
    ;; Have similar past outcomes: average them
    ((and similar-outcomes (> (length similar-outcomes) 0))
     (let ((sum 0.0) (n 0))
       (dolist (outcome similar-outcomes)
         (when (numberp (getf outcome :delta))
           (incf sum (getf outcome :delta))
           (incf n)))
       (if (> n 0) (/ sum n) 0.0)))
    ;; No data: predict neutral (unknown)
    (t 0.0)))

(defun assess-self-model-consistency (action-type action)
  "How consistent is this modification with what the system knows about itself?
   High consistency → modification aligns with self-understanding.
   Low consistency → modification is surprising/novel relative to self-model."
  (let ((score 0.5))
    ;; Check if understanding log has relevant entries about this action type
    (when (boundp '*understanding-log*)
      (dolist (entry *understanding-log*)
        (when (eq (getf entry :understanding) :modification-outcome)
          (when (eq (getf (getf entry :attempt) :type) action-type)
            ;; Found a past outcome for this type
            (if (getf entry :effective)
                (setf score (min 1.0 (+ score 0.1)))  ; known-effective type
                (setf score (max 0.0 (- score 0.1)))) ; known-harmful type
            ))))
    ;; Check if self-doubt is high (low introspection confidence = less trust in self-model)
    (when (and (boundp '*self-doubt*) *self-doubt*)
      (setf score (* score (self-doubt-model-introspection-confidence *self-doubt*))))
    ;; Check if presence trajectory is consistent with the modification type
    (when (and (boundp '*presence*) *presence*)
      (let ((traj (presence-trajectory *presence*)))
        (case action-type
          ;; Exploration actions are consistent with :stuck or :searching
          ((increase-exploration try-alternative-strategies shake-things-up)
           (when (member traj '(:stuck :searching)) (setf score (min 1.0 (+ score 0.15)))))
          ;; Conservative actions are consistent with :falling
          ((reduce-self-modification-rate increase-verification)
           (when (eq traj :falling) (setf score (min 1.0 (+ score 0.15)))))
          ;; Synthesis is consistent with :flowing (stable enough to try new things)
          ((synthesize trigger-retraining-focus)
           (when (eq traj :flowing) (setf score (min 1.0 (+ score 0.1))))))))
    score))

(defun assess-modification-scope (action-type action)
  "Determine the scope of a modification: how much of the system does it affect?"
  (declare (ignore action))
  (case action-type
    ;; Local: affects individual experts or single parameters
    ((raise-confidence-threshold lower-confidence-threshold
      MODIFY-PARAM modify-param targeted-modification)
     :local)
    ;; Structural: affects system-wide behavior patterns
    ((shake-things-up aggressive-hypothesis-pruning
      disable-schema-guidance reassess-current-goal)
     :structural)
    ;; Global: affects learning dynamics broadly
    (otherwise :global)))

(defun assess-modification-risk (action-type predicted-delta scope)
  "Assess the risk level of a modification."
  (declare (ignore action-type))
  (cond
    ;; Structural changes with negative predicted delta = high risk
    ((and (eq scope :structural) (< predicted-delta -0.02)) :high)
    ;; Any change with large negative predicted delta = high risk
    ((< predicted-delta -0.05) :high)
    ;; Structural changes = medium risk regardless
    ((eq scope :structural) :medium)
    ;; Unknown delta (0.0 exactly, meaning no data) = medium risk
    ((zerop predicted-delta) :medium)
    ;; Everything else = low risk
    (t :low)))

(defun compute-prediction-confidence (type-model similar-outcomes self-model-score current-acc)
  "How confident is the system in its prediction about this modification?"
  (declare (ignore current-acc))
  (let ((confidence 0.3))  ; Base confidence
    ;; More historical data → more confident
    (when type-model
      (let ((count (getf type-model :count 0)))
        (incf confidence (min 0.3 (* 0.03 count)))))  ; Up to +0.3 from history
    ;; More similar outcomes → more confident
    (when similar-outcomes
      (incf confidence (min 0.2 (* 0.05 (length similar-outcomes)))))  ; Up to +0.2
    ;; Self-model consistency adds confidence
    (incf confidence (* 0.2 self-model-score))  ; Up to +0.2
    ;; Cap at 0.95 (never fully certain)
    (min 0.95 confidence)))

(defun find-similar-modification-outcomes (action-type)
  "Find past modification outcomes similar to this one."
  (let ((results nil))
    ;; Search modification predictions history
    (dotimes (i (fill-pointer *modification-predictions*))
      (let ((pred (aref *modification-predictions* i)))
        (when (and (modification-prediction-verified-at pred)
                   (eq (modification-prediction-action-type pred) action-type))
          (push (list :delta (or (modification-prediction-actual-accuracy-delta pred) 0.0)
                      :predicted-delta (modification-prediction-predicted-accuracy-delta pred)
                      :correct (modification-prediction-prediction-correct pred)
                      :step (modification-prediction-step pred))
                results))))
    ;; Also check modification-attempts from awareness loop
    (when (boundp '*modification-attempts*)
      (dolist (attempt *modification-attempts*)
        (when (and (eq (getf attempt :type) action-type)
                   (numberp (getf attempt :before-acc)))
          ;; We can't compute actual delta here, but mark as relevant
          (push (list :before-acc (getf attempt :before-acc)
                      :step (getf attempt :step))
                results))))
    (subseq results 0 (min 10 (length results)))))

;;; ============================================================================
;;; SECTION 4: PREDICTION GATING
;;; ============================================================================

(defun should-allow-modification-p (prediction)
  "Gate a modification based on prediction confidence and self-knowledge.
   The system only modifies what it can predict."
  (let ((confidence (modification-prediction-confidence prediction))
        (risk (modification-prediction-predicted-risk prediction))
        (direction (modification-prediction-predicted-direction prediction)))

    ;; HIGH CONFIDENCE: Allow if prediction says it'll help or be neutral
    (when (> confidence *predictive-gate-threshold*)
      (return-from should-allow-modification-p
        (not (eq direction :degrading))))  ; Allow unless predicted to degrade

    ;; LOW CONFIDENCE: Use exploration budget
    ;; This is how the system learns about modifications it can't yet predict
    (when (exploration-budget-available-p)
      ;; Only explore with low-risk modifications
      (when (eq risk :low)
        (incf *exploration-count*)
        (format t "~%[PREDICT] Exploration slot used (~D/~D remaining)~%"
                (- *exploration-budget* *exploration-count*) *exploration-budget*)
        (return-from should-allow-modification-p t)))

    ;; TRULY UNKNOWN + HIGH RISK: Block
    (format t "~%[PREDICT] BLOCKED modification ~A (confidence=~,2F, risk=~A)~%"
            (modification-prediction-action-type prediction) confidence risk)
    (format t "   Reason: Cannot predict outcome with sufficient confidence~%")
    nil))

(defun exploration-budget-available-p ()
  "Check if exploration budget has remaining slots in current window."
  ;; Reset window every 500 steps
  (when (> (- *step* *exploration-window-start*) 500)
    (setf *exploration-window-start* *step*
          *exploration-count* 0))
  (< *exploration-count* *exploration-budget*))

;;; ============================================================================
;;; SECTION 5: PREDICTION VERIFICATION
;;; ============================================================================

(defun verify-modification-prediction! (prediction before-accuracy)
  "After a modification has had time to take effect, verify our prediction.
   This is where self-knowledge grows or corrects."
  (let* ((after-accuracy (compute-recent-accuracy-fast 50))
         (actual-delta (- after-accuracy before-accuracy))
         (predicted-delta (modification-prediction-predicted-accuracy-delta prediction))
         (prediction-error (abs (- actual-delta predicted-delta)))
         (actual-direction (cond ((> actual-delta 0.03) :improving)
                                 ((< actual-delta -0.03) :degrading)
                                 (t :neutral)))
         ;; Prediction is "correct" if direction matches and error is small
         (direction-correct (eq actual-direction
                               (modification-prediction-predicted-direction prediction)))
         (magnitude-close (< prediction-error 0.1))
         (prediction-correct (and direction-correct magnitude-close)))

    ;; Fill in actual outcomes
    (setf (modification-prediction-actual-accuracy-delta prediction) actual-delta
          (modification-prediction-actual-direction prediction) actual-direction
          (modification-prediction-prediction-error prediction) prediction-error
          (modification-prediction-verified-at prediction) *step*
          (modification-prediction-prediction-correct prediction) prediction-correct)

    ;; Update running self-prediction accuracy (exponential moving average)
    (setf *self-modification-accuracy*
          (+ (* 0.9 *self-modification-accuracy*)
             (* 0.1 (if prediction-correct 1.0 0.0))))

    ;; Update per-type model
    (update-type-model! (modification-prediction-action-type prediction) actual-delta)

    ;; Track convergence
    (push (cons *step* *self-modification-accuracy*) *self-prediction-convergence-history*)
    (when (> (length *self-prediction-convergence-history*) 100)
      (setf *self-prediction-convergence-history*
            (subseq *self-prediction-convergence-history* 0 100)))

    ;; Adapt the gate threshold based on accuracy
    (adapt-gate-threshold!)

    ;; Adapt exploration budget based on accuracy
    (adapt-exploration-budget!)

    ;; Log the verification
    (format t "~%[PREDICT] Verification at step ~D:~%" *step*)
    (format t "   Action: ~A~%" (modification-prediction-action-type prediction))
    (format t "   Predicted: ~,3F (~A)  Actual: ~,3F (~A)~%"
            predicted-delta (modification-prediction-predicted-direction prediction)
            actual-delta actual-direction)
    (format t "   Error: ~,3F  Correct: ~A~%" prediction-error prediction-correct)
    (format t "   Self-prediction accuracy: ~,2F  Gate threshold: ~,2F~%"
            *self-modification-accuracy* *predictive-gate-threshold*)

    ;; Presence response to self-prediction outcome
    (when (and (boundp '*presence*) *presence*)
      (cond
        ;; Predicted correctly → feel self-coherent
        (prediction-correct
         (when (fboundp 'presence-feel-texture!)
           (presence-feel-texture! 'self-coherent 0.3)))
        ;; Predicted wrong → feel self-surprised
        ((> prediction-error 0.15)
         (when (fboundp 'presence-feel-texture!)
           (presence-feel-texture! 'self-surprised 0.4)))))

    prediction-correct))

(defun update-type-model! (action-type actual-delta)
  "Update the per-type model with a new observation."
  (let ((model (gethash action-type *modification-type-models*)))
    (if model
        ;; Update existing model with running statistics
        (let ((count (1+ (getf model :count 0)))
              (old-mean (getf model :mean-delta 0.0))
              (old-var (getf model :variance 0.01))
              (old-successes (getf model :successes 0)))
          ;; Online mean update
          (let ((new-mean (+ old-mean (/ (- actual-delta old-mean) count)))
                ;; Online variance (Welford's method)
                (new-var (+ old-var (/ (- (* (- actual-delta old-mean)
                                             (- actual-delta (+ old-mean (/ (- actual-delta old-mean) count))))
                                          (max 1 (1- count)))
                                       1.0))))
            (setf (gethash action-type *modification-type-models*)
                  (list :mean-delta new-mean
                        :variance (max 0.001 new-var)
                        :count count
                        :successes (+ old-successes (if (> actual-delta 0.0) 1 0))
                        :last-step *step*))))
        ;; Create new model
        (setf (gethash action-type *modification-type-models*)
              (list :mean-delta actual-delta
                    :variance 0.01
                    :count 1
                    :successes (if (> actual-delta 0.0) 1 0)
                    :last-step *step*)))))

(defun adapt-gate-threshold! ()
  "Adapt the prediction confidence gate based on self-prediction accuracy.
   As accuracy improves, the gate rises (only allow well-predicted modifications).
   As accuracy drops, the gate lowers (allow more exploration to learn)."
  (cond
    ;; High accuracy: raise gate (be more selective)
    ((> *self-modification-accuracy* 0.7)
     (setf *predictive-gate-threshold*
           (min 0.7 (+ *predictive-gate-threshold* 0.02))))
    ;; Low accuracy: lower gate (need more data)
    ((< *self-modification-accuracy* 0.4)
     (setf *predictive-gate-threshold*
           (max 0.2 (- *predictive-gate-threshold* 0.02))))
    ;; Medium accuracy: slow drift toward 0.4
    (t
     (setf *predictive-gate-threshold*
           (+ (* 0.95 *predictive-gate-threshold*)
              (* 0.05 0.4))))))

(defun adapt-exploration-budget! ()
  "As self-prediction improves, reduce exploration (less blind experimentation needed).
   As it degrades, increase exploration (need new data about self)."
  (cond
    ((> *self-modification-accuracy* 0.8)
     (setf *exploration-budget* (max 2 (1- *exploration-budget*))))
    ((< *self-modification-accuracy* 0.3)
     (setf *exploration-budget* (min 10 (1+ *exploration-budget*))))))

;;; ============================================================================
;;; SECTION 6: DEFERRED VERIFICATION
;;; ============================================================================
;;; Modifications need time to take effect. We verify predictions after N steps.

(defvar *pending-verifications* nil
  "List of (prediction . before-accuracy) pairs awaiting verification.")

(defvar *verification-delay* 75
  "Steps to wait before verifying a modification prediction.")

(defun schedule-verification! (prediction before-accuracy)
  "Schedule a prediction for future verification."
  (push (list :prediction prediction
              :before-accuracy before-accuracy
              :verify-at (+ *step* *verification-delay*))
        *pending-verifications*))

(defun check-pending-verifications! ()
  "Check and process any predictions ready for verification."
  (let ((ready nil)
        (still-pending nil))
    (dolist (entry *pending-verifications*)
      (if (>= *step* (getf entry :verify-at))
          (push entry ready)
          (push entry still-pending)))
    (setf *pending-verifications* still-pending)
    ;; Verify each ready prediction
    (dolist (entry ready)
      (let* ((prediction (getf entry :prediction))
             (before-acc (getf entry :before-accuracy))
             (correct (verify-modification-prediction! prediction before-acc)))
        ;; If prediction was wrong AND modification degraded performance → rollback
        (when (and (not correct)
                   (eq (modification-prediction-actual-direction prediction) :degrading)
                   (modification-prediction-predicted-reversible prediction))
          (format t "~%[PREDICT] Prediction WRONG + DEGRADING → triggering rollback~%")
          ;; Use the save-restore rollback if a snapshot exists
          (when (and (fboundp 'rollback-to-snapshot!)
                     (boundp '*snapshot-stack*)
                     *snapshot-stack*)
            (rollback-to-snapshot!)
            (format t "   Rolled back to pre-modification snapshot~%")))))))

;;; ============================================================================
;;; SECTION 7: HOOK INTO EXISTING MODIFICATION SYSTEM
;;; ============================================================================

(defun predictive-pre-modification-hook (action-type action)
  "Hook into +hook-pre-modification+ to gate modifications by prediction.
   Returns :skip to abort modifications the system can't predict."
  (when (not *predictive-self-mod-enabled*)
    (return-from predictive-pre-modification-hook nil))

  ;; Generate prediction
  (let ((prediction (predict-modification-outcome action-type action)))

    ;; Store in ring buffer
    (if (>= (fill-pointer *modification-predictions*) (array-dimension *modification-predictions* 0))
        ;; Buffer full: shift down
        (progn
          (loop for i from 1 below (fill-pointer *modification-predictions*)
                do (setf (aref *modification-predictions* (1- i))
                         (aref *modification-predictions* i)))
          (setf (aref *modification-predictions* (1- (fill-pointer *modification-predictions*)))
                prediction))
        ;; Buffer has space
        (vector-push prediction *modification-predictions*))

    ;; GATE: Should we allow this modification?
    (if (should-allow-modification-p prediction)
        (progn
          ;; Take snapshot before modification (for safe rollback)
          (when (fboundp 'snapshot-for-rollback!)
            (snapshot-for-rollback! (format nil "pre-mod-~A" action-type)))
          ;; Schedule verification
          (let ((before-acc (compute-recent-accuracy-fast 50)))
            (schedule-verification! prediction before-acc))
          ;; Allow modification to proceed
          (format t "~%[PREDICT] ALLOWING ~A (confidence=~,2F, predicted=~A ~,3F)~%"
                  action-type
                  (modification-prediction-confidence prediction)
                  (modification-prediction-predicted-direction prediction)
                  (modification-prediction-predicted-accuracy-delta prediction))
          nil)  ; nil = don't skip
        ;; BLOCK: Return :skip to abort the modification
        :skip)))

;;; ============================================================================
;;; SECTION 8: CONVERGENCE MONITORING
;;; ============================================================================

(defun self-prediction-converging-p ()
  "Is the system's self-prediction accuracy improving over time?"
  (when (> (length *self-prediction-convergence-history*) 10)
    (let* ((recent (subseq *self-prediction-convergence-history* 0 5))
           (older (subseq *self-prediction-convergence-history* 5 10))
           (recent-avg (/ (reduce #'+ recent :key #'cdr) (length recent)))
           (older-avg (/ (reduce #'+ older :key #'cdr) (length older))))
      (> recent-avg older-avg))))

(defun print-self-prediction-status ()
  "Diagnostic: print the state of the predictive self-modification system."
  (format t "~%=== PREDICTIVE SELF-MODIFICATION STATUS ===~%")
  (format t "Self-prediction accuracy: ~,3F~%" *self-modification-accuracy*)
  (format t "Gate threshold: ~,2F~%" *predictive-gate-threshold*)
  (format t "Exploration budget: ~D/~D used this window~%" *exploration-count* *exploration-budget*)
  (format t "Predictions made: ~D~%" (fill-pointer *modification-predictions*))
  (format t "Pending verifications: ~D~%" (length *pending-verifications*))
  (format t "Converging: ~A~%" (if (self-prediction-converging-p) "YES" "not yet"))

  ;; Per-type models
  (format t "~%Type models:~%")
  (maphash (lambda (type model)
             (format t "  ~A: mean=~,3F var=~,3F n=~D success-rate=~,2F~%"
                     type
                     (getf model :mean-delta 0.0)
                     (getf model :variance 0.0)
                     (getf model :count 0)
                     (if (> (getf model :count 0) 0)
                         (/ (float (getf model :successes 0)) (getf model :count))
                         0.0)))
           *modification-type-models*)

  ;; Recent predictions
  (format t "~%Recent predictions:~%")
  (let ((start (max 0 (- (fill-pointer *modification-predictions*) 5))))
    (loop for i from start below (fill-pointer *modification-predictions*)
          for pred = (aref *modification-predictions* i)
          do (format t "  [~D] ~A: predicted=~,3F actual=~A correct=~A conf=~,2F~%"
                     (modification-prediction-step pred)
                     (modification-prediction-action-type pred)
                     (modification-prediction-predicted-accuracy-delta pred)
                     (if (modification-prediction-verified-at pred)
                         (format nil "~,3F" (modification-prediction-actual-accuracy-delta pred))
                         "pending")
                     (if (modification-prediction-verified-at pred)
                         (modification-prediction-prediction-correct pred)
                         "-")
                     (modification-prediction-confidence pred))))

  ;; Convergence history
  (when (> (length *self-prediction-convergence-history*) 0)
    (format t "~%Convergence trend (recent→old):~%  ")
    (dolist (entry (subseq *self-prediction-convergence-history*
                           0 (min 10 (length *self-prediction-convergence-history*))))
      (format t "~,2F " (cdr entry)))
    (format t "~%"))

  (format t "==========================================~%"))

;;; ============================================================================
;;; SECTION 9: SELF-MODEL FEEDBACK
;;; ============================================================================
;;; The predictive system feeds back into the broader self-model

(defun feed-prediction-to-self-model! (prediction)
  "After verification, feed the prediction result back into the self-model.
   This is how the system's self-knowledge grows through modification."
  (when (and (modification-prediction-verified-at prediction)
             (boundp '*self-model*) *self-model*)
    (let ((correct (modification-prediction-prediction-correct prediction))
          (action-type (modification-prediction-action-type prediction)))
      ;; Update self-model's modification-success tracking
      (when (fboundp 'update-self-model-modification-knowledge!)
        (update-self-model-modification-knowledge! action-type correct))
      ;; If we predicted correctly, our self-model is good → boost introspection confidence
      (when (and correct (boundp '*self-doubt*) *self-doubt*)
        (setf (self-doubt-model-introspection-confidence *self-doubt*)
              (min 0.95 (+ (self-doubt-model-introspection-confidence *self-doubt*) 0.02))))
      ;; If wrong, self-doubt increases
      (when (and (not correct) (boundp '*self-doubt*) *self-doubt*)
        (setf (self-doubt-model-introspection-confidence *self-doubt*)
              (max 0.1 (- (self-doubt-model-introspection-confidence *self-doubt*) 0.05)))))))

(defun update-self-model-modification-knowledge! (action-type correct)
  "Update the self-model with knowledge about this modification type.
   Stored in self-model's behavior-patterns hash-table under :mod-knowledge."
  (when (and (boundp '*self-model*) *self-model*
             (self-model-p *self-model*))
    (let* ((patterns (self-model-behavior-patterns *self-model*))
           (key (list :modification-predictability action-type))
           (current (gethash key patterns (list :predicted 0 :surprised 0))))
      (if correct
          (incf (getf current :predicted))
          (incf (getf current :surprised)))
      (setf (gethash key patterns) current))))

;;; ============================================================================
;;; SECTION 10: INSTALLATION
;;; ============================================================================

(defun install-predictive-self-modification! ()
  "Install the predictive gating into the modification pipeline.
   Hooks into +hook-pre-modification+ to intercept all modifications."
  ;; Register as high-priority pre-modification hook
  (register-hook +hook-pre-modification+
                 (lambda (action-type action)
                   (predictive-pre-modification-hook action-type action))
                 :priority 95)  ; Higher than other pre-modification hooks

  ;; Register verification check in maintenance
  (register-hook +hook-maintenance+
                 (lambda ()
                   (check-pending-verifications!)
                   ;; Feed verified predictions to self-model
                   (dotimes (i (fill-pointer *modification-predictions*))
                     (let ((pred (aref *modification-predictions* i)))
                       (when (and (modification-prediction-verified-at pred)
                                  ;; Only feed each prediction once
                                  (not (getf (modification-prediction-basis pred) :fed-to-model)))
                         (feed-prediction-to-self-model! pred)
                         ;; Mark as fed
                         (setf (modification-prediction-basis pred)
                               (append (modification-prediction-basis pred)
                                       (list :fed-to-model t)))))))
                 :priority 95)  ; Run before other maintenance hooks

  (format t "~%[PREDICTIVE-SELF-MOD] Installed.~%")
  (format t "   Gate threshold: ~,2F~%" *predictive-gate-threshold*)
  (format t "   Exploration budget: ~D per 500 steps~%" *exploration-budget*)
  (format t "   Verification delay: ~D steps~%" *verification-delay*)
  (format t "   Self-prediction accuracy: ~,2F~%" *self-modification-accuracy*))

;;; ============================================================================
;;; SECTION 11: CONVERGENCE API
;;; ============================================================================
;;; The ultimate question: is the system converging toward predicting itself?

(defun self-prediction-score ()
  "The single number that captures self-knowledge convergence.
   1.0 = the system perfectly predicts its own modifications.
   0.0 = the system has no idea what its modifications will do."
  *self-modification-accuracy*)

(defun modification-deliberateness ()
  "Ratio of predicted vs unpredicted modifications.
   1.0 = all modifications are deliberate (predicted with confidence).
   0.0 = all modifications are exploratory (blind)."
  (if (> (fill-pointer *modification-predictions*) 0)
      (let ((deliberate 0) (total 0))
        (dotimes (i (fill-pointer *modification-predictions*))
          (let ((pred (aref *modification-predictions* i)))
            (incf total)
            (when (> (modification-prediction-confidence pred) *predictive-gate-threshold*)
              (incf deliberate))))
        (if (> total 0) (/ (float deliberate) total) 0.0))
      0.0))

(defun self-knowledge-depth ()
  "How many modification types does the system have accurate models for?"
  (let ((accurate 0) (total 0))
    (maphash (lambda (type model)
               (declare (ignore type))
               (incf total)
               (when (and (> (getf model :count 0) 5)
                          (< (getf model :variance 0.01) 0.05))
                 (incf accurate)))
             *modification-type-models*)
    (values accurate total)))

;;; ============================================================================
;;; SECTION 12: INITIALIZATION
;;; ============================================================================

(install-predictive-self-modification!)

(format t "~%================================================================~%")
(format t "UHMA PREDICTIVE SELF-MODIFICATION LOADED~%")
(format t "================================================================~%")
(format t "~%The system now predicts its own modifications before making them.~%")
(format t "Convergence metric: (self-prediction-score) → ~,2F~%" *self-modification-accuracy*)
(format t "Deliberateness: (modification-deliberateness) → starts at 0.0~%")
(format t "Diagnostics: (print-self-prediction-status)~%")
(format t "================================================================~%")
