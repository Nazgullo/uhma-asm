;;;; ============================================================================
;;;; UHMA v6.8 - COGNITIVE CONTROLLER
;;;; ============================================================================
;;;;
;;;; The machinery exists. This module WIRES IT TOGETHER.
;;;;
;;;; Before: input → find expert → output (everything else is bookkeeping)
;;;; After:  input → assess situation → select strategy → execute with 
;;;;         the right tools → monitor → adjust → learn from process
;;;;
;;;; The cognitive controller sits between input and execution, deciding
;;;; HOW to think, not just WHAT to predict.
;;;;
;;;; ============================================================================

(in-package :uhma)

;;; ============================================================================
;;; PART 1: SITUATION ASSESSMENT
;;; ============================================================================
;;; Before doing anything, understand what kind of problem this is.

(defstruct situation
  "Assessment of the current cognitive situation."
  (context nil)                        ; The input context
  (novelty 0.0)                        ; How novel is this? 0-1
  (uncertainty 0.0)                    ; How uncertain are we? 0-1
  (complexity 0.0)                     ; How complex? 0-1  
  (goal-relevance 0.0)                 ; How relevant to active goals? 0-1
  (causal-density 0.0)                 ; How many causal links involved? 0-1
  (structure-present nil)              ; Did we find compositional structure?
  (similar-past nil)                   ; Similar situations from history
  (recommended-strategy nil)           ; What strategy to use
  (outcome nil)                        ; What actually happened (filled in later)
  (assessed-at 0))

(defvar *current-situation* nil
  "Current situation assessment")
(defvar *situation-history* (make-array 100 :fill-pointer 0 :adjustable t)
  "Recent situation assessments for learning")

(defun assess-situation (ctx)
  "Assess the current situation to decide how to think."
  (let ((situation (make-situation
                    :context ctx
                    :assessed-at (if (boundp '*step*) *step* 0))))
    
    ;; Novelty: Have we seen this context before?
    (setf (situation-novelty situation)
          (compute-novelty ctx))
    
    ;; Uncertainty: What do experts say?
    (setf (situation-uncertainty situation)
          (compute-uncertainty ctx))
    
    ;; Complexity: How much structure is here?
    (setf (situation-complexity situation)
          (compute-complexity ctx))
    
    ;; Goal relevance: Does this matter for active goals?
    (setf (situation-goal-relevance situation)
          (compute-goal-relevance ctx))
    
    ;; Causal density: How many causal relationships touch this?
    (setf (situation-causal-density situation)
          (compute-causal-density ctx))
    
    ;; Try to build structure
    (when (and (listp ctx) (> (length ctx) 1))
      (let ((struct (try-build-structure ctx)))
        (setf (situation-structure-present situation) struct)))
    
    ;; Find similar past situations
    (setf (situation-similar-past situation)
          (find-similar-situations ctx))
    
    ;; Recommend strategy based on assessment
    (setf (situation-recommended-strategy situation)
          (recommend-strategy situation))
    
    ;; Store in history
    (vector-push-extend situation *situation-history*)
    (setf *current-situation* situation)
    
    situation))

(defun compute-novelty (ctx)
  "How novel is this context?"
  (if (null ctx)
      1.0
      (let ((encounters 0))
        ;; Check how many experts have seen this
        (when (boundp '*experts*)
          (let ((key (make-context-key ctx (min 3 (length ctx)))))
            (dolist (e *experts*)
              (when (gethash key (expert-knowledge e))
                (incf encounters)))))
        ;; Check pattern stats
        (when (boundp '*pattern-stats*)
          (let ((stats (gethash (make-context-key ctx (min 3 (length ctx))) *pattern-stats*)))
            (when stats
              (incf encounters (car stats)))))
        ;; Convert to 0-1 (more encounters = less novel)
        (/ 1.0 (1+ (* 0.1 encounters))))))

(defun compute-uncertainty (ctx)
  "How uncertain are we about this context?"
  (let ((predictions nil)
        (confidences nil))
    ;; Gather predictions from multiple experts
    (when (boundp '*experts*)
      (let ((key (make-context-key ctx (min 3 (length ctx)))))
        (dolist (e (subseq *experts* 0 (min 5 (length *experts*))))
          (let ((pred (gethash key (expert-knowledge e))))
            (when pred
              (push pred predictions)
              (let ((hits (expert-hits e))
                    (misses (expert-misses e)))
                (push (if (> (+ hits misses) 0)
                          (/ hits (+ hits misses))
                          0.5)
                      confidences)))))))
    (cond
      ;; No predictions = high uncertainty
      ((null predictions) 0.9)
      ;; All agree = low uncertainty  
      ((= 1 (length (remove-duplicates predictions :test #'equal)))
       (* 0.3 (- 1.0 (if confidences (apply #'max confidences) 0.5))))
      ;; Disagreement = uncertainty proportional to spread
      (t (let ((unique (length (remove-duplicates predictions :test #'equal))))
           (min 1.0 (* 0.3 unique)))))))

(defun compute-complexity (ctx)
  "How complex is this context?"
  (if (null ctx)
      0.0
      (let ((score 0.0))
        ;; Length contributes
        (incf score (min 0.3 (* 0.05 (length ctx))))
        ;; Type diversity contributes
        (let ((types (remove-duplicates (mapcar #'get-type ctx))))
          (incf score (min 0.3 (* 0.1 (length types)))))
        ;; Structural features contribute
        (when (try-build-structure ctx)
          (incf score 0.2))
        ;; Causal links contribute
        (when (boundp '*causal-model*)
          (let ((links 0))
            (dolist (tok ctx)
              (incf links (length (get-effects tok)))
              (incf links (length (get-causes tok))))
            (incf score (min 0.2 (* 0.02 links)))))
        (min 1.0 score))))

(defun compute-goal-relevance (ctx)
  "How relevant is this to active goals?"
  (if (or (not (boundp '*goal-stack*)) (null *goal-stack*))
      0.0
      (let ((relevance 0.0))
        (dolist (goal *goal-stack*)
          ;; Check if context overlaps with goal description
          (let ((desc (goal-description goal)))
            (when (and desc (listp ctx))
              (dolist (tok ctx)
                (when (and (stringp desc)
                           (search (string-downcase (format nil "~A" tok))
                                  (string-downcase desc)))
                  (incf relevance (* 0.2 (goal-priority goal))))))))
        (min 1.0 relevance))))

(defun compute-causal-density (ctx)
  "How many causal relationships involve this context?"
  (if (or (not (boundp '*causal-model*)) (null ctx))
      0.0
      (let ((total-links 0))
        (dolist (tok ctx)
          (incf total-links (length (get-effects tok)))
          (incf total-links (length (get-causes tok))))
        (min 1.0 (* 0.1 total-links)))))

(defun find-similar-situations (ctx)
  "Find similar situations from history."
  (let ((similar nil))
    (loop for i from (1- (fill-pointer *situation-history*)) downto 0
          for sit = (aref *situation-history* i)
          for sim = (context-similarity ctx (situation-context sit))
          when (> sim 0.5)
          do (push (cons sit sim) similar)
          when (>= (length similar) 3)
          do (return))
    similar))

;;; ============================================================================
;;; PART 2: THINKING STRATEGIES
;;; ============================================================================
;;; Different situations call for different approaches.

(deftype thinking-strategy ()
  '(member :fast-default      ; Just use expert lookup (low uncertainty, familiar)
           :multi-expert      ; Consult multiple experts, vote
           :structural        ; Use compositional structure  
           :causal            ; Reason through causal model
           :planning          ; Engage planning for multi-step
           :analogical        ; Use similar past situations
           :exploratory       ; Try something new (high novelty)
           :deliberative))    ; Full deliberation (high stakes/complexity)

(defun recommend-strategy (situation)
  "Recommend a thinking strategy based on situation assessment."
  (let ((novelty (situation-novelty situation))
        (uncertainty (situation-uncertainty situation))
        (complexity (situation-complexity situation))
        (goal-rel (situation-goal-relevance situation))
        (causal (situation-causal-density situation))
        (struct (situation-structure-present situation))
        (similar (situation-similar-past situation)))
    
    (cond
      ;; High goal relevance + complexity = full deliberation
      ((and (> goal-rel 0.5) (> complexity 0.5))
       :deliberative)
      
      ;; High causal density = use causal reasoning
      ((> causal 0.5)
       :causal)
      
      ;; Structure present = use structural approach
      (struct
       :structural)
      
      ;; Similar past situations = use analogy
      ((and similar (> (cdar similar) 0.7))
       :analogical)
      
      ;; High novelty = explore
      ((> novelty 0.7)
       :exploratory)
      
      ;; High uncertainty = consult multiple experts
      ((> uncertainty 0.5)
       :multi-expert)
      
      ;; Default: fast lookup
      (t :fast-default))))

;;; ============================================================================
;;; PART 3: STRATEGY EXECUTION
;;; ============================================================================
;;; Each strategy actually uses the machinery.

(defstruct thinking-result
  "Result of applying a thinking strategy."
  (prediction nil)
  (confidence 0.0)
  (strategy-used nil)
  (reasoning-trace nil)                ; What happened during thinking
  (alternatives nil)                   ; Other candidates considered
  (time-spent 0)                       ; Steps of deliberation
  (working-memory-used nil))           ; WM state after thinking

(defun execute-strategy (strategy ctx situation)
  "Execute a thinking strategy and return result."
  (let ((start-time (get-internal-real-time))
        (result (make-thinking-result :strategy-used strategy)))
    
    ;; Load context to working memory
    (wm-load! ctx :type :token :priority 0.6 :source :input)
    
    (case strategy
      (:fast-default
       (execute-fast-default ctx result))
      
      (:multi-expert
       (execute-multi-expert ctx result))
      
      (:structural
       (execute-structural ctx situation result))
      
      (:causal
       (execute-causal ctx situation result))
      
      (:planning
       (execute-planning ctx situation result))
      
      (:analogical
       (execute-analogical ctx situation result))
      
      (:exploratory
       (execute-exploratory ctx result))
      
      (:deliberative
       (execute-deliberative ctx situation result))
      
      (t (execute-fast-default ctx result)))
    
    ;; Record what's in working memory
    (setf (thinking-result-working-memory-used result) (wm-contents))
    
    ;; Record time spent
    (setf (thinking-result-time-spent result)
          (- (get-internal-real-time) start-time))
    
    result))

(defun execute-fast-default (ctx result)
  "Fast default: just use standard routing."
  (let ((op-result (execute-with-routing ctx)))
    (setf (thinking-result-prediction result) (op-result-value op-result))
    (setf (thinking-result-confidence result) (op-result-confidence op-result))
    (push :used-standard-routing (thinking-result-reasoning-trace result))))

(defun execute-multi-expert (ctx result)
  "Consult multiple experts and aggregate."
  (let ((votes (make-hash-table :test 'equal))
        (total-weight 0.0)
        (experts-consulted 0))
    ;; Gather votes from multiple experts
    (when (boundp '*experts*)
      (let ((key (make-context-key ctx (min 3 (length ctx)))))
        (dolist (e (subseq *experts* 0 (min 10 (length *experts*))))
          (let ((pred (gethash key (expert-knowledge e))))
            (when pred
              (incf experts-consulted)
              (let* ((hits (expert-hits e))
                     (misses (expert-misses e))
                     (weight (if (> (+ hits misses) 0)
                                (/ hits (+ hits misses))
                                0.5)))
                (incf (gethash pred votes 0.0) weight)
                (incf total-weight weight)
                (push (list :expert (expert-id e) :pred pred :weight weight)
                      (thinking-result-alternatives result))))))))
    ;; Find best
    (let ((best-pred nil) (best-weight 0.0))
      (maphash (lambda (pred weight)
                 (when (> weight best-weight)
                   (setf best-pred pred best-weight weight)))
               votes)
      (setf (thinking-result-prediction result) best-pred)
      (setf (thinking-result-confidence result)
            (if (> total-weight 0) (/ best-weight total-weight) 0.0)))
    (push (list :consulted-experts experts-consulted)
          (thinking-result-reasoning-trace result))))

(defun execute-structural (ctx situation result)
  "Use compositional structure for reasoning."
  (let ((struct (situation-structure-present situation)))
    (if struct
        (progn
          ;; Load structure to WM
          (wm-load! struct :type :structure :priority 0.7)
          ;; Use structure to guide prediction
          (let ((head (comp-structure-head struct))
                (slots (comp-structure-slots struct)))
            ;; Look for patterns with similar structure
            (let ((pred (predict-from-structure struct ctx)))
              (setf (thinking-result-prediction result) pred)
              (setf (thinking-result-confidence result) 
                    (if pred 0.6 0.3))))
          (push (list :used-structure (comp-structure-type struct))
                (thinking-result-reasoning-trace result)))
        ;; Fallback to multi-expert
        (execute-multi-expert ctx result))))

(defun predict-from-structure (struct ctx)
  "Use structure to make prediction."
  ;; Find experts that have seen similar structures
  (let ((struct-type (comp-structure-type struct))
        (candidates nil))
    (when (boundp '*built-structures*)
      (dolist (past *built-structures*)
        (when (eq (comp-structure-type past) struct-type)
          ;; Check what followed this structure
          (let ((tokens (comp-structure-source-tokens past)))
            (when (and tokens (> (length tokens) 0))
              (push (first (last tokens)) candidates))))))
    ;; Return most common candidate
    (when candidates
      (let ((counts (make-hash-table :test 'equal)))
        (dolist (c candidates)
          (incf (gethash c counts 0)))
        (let ((best nil) (best-count 0))
          (maphash (lambda (k v)
                     (when (> v best-count)
                       (setf best k best-count v)))
                   counts)
          best)))))

(defun execute-causal (ctx situation result)
  "Use causal model for reasoning."
  ;; What caused the current context?
  (let ((causes nil)
        (predicted-effects nil))
    (when (and (boundp '*causal-model*) (listp ctx))
      ;; Get causes of recent tokens
      (dolist (tok (subseq ctx 0 (min 2 (length ctx))))
        (let ((tok-causes (get-causes tok)))
          (dolist (c tok-causes)
            (push c causes))))
      ;; Get effects of recent tokens
      (dolist (tok (subseq ctx 0 (min 2 (length ctx))))
        (let ((effects (get-effects tok)))
          (dolist (e effects)
            (push e predicted-effects))))
      ;; Load causal info to WM
      (when causes
        (wm-load! causes :type :hypothesis :priority 0.5))
      (when predicted-effects
        (wm-load! predicted-effects :type :hypothesis :priority 0.6)))
    ;; Predict most likely effect
    (if predicted-effects
        (let ((best (first (sort (copy-list predicted-effects) #'> :key #'cdr))))
          (setf (thinking-result-prediction result) (car best))
          (setf (thinking-result-confidence result) (cdr best))
          (push (list :causal-prediction (car best) :strength (cdr best))
                (thinking-result-reasoning-trace result)))
        ;; Fallback
        (execute-multi-expert ctx result))))

(defun execute-planning (ctx situation result)
  "Use planning for multi-step reasoning."
  (declare (ignore situation))
  ;; Check if we have an active goal
  (let ((goal (when (boundp '*goal-stack*) (first *goal-stack*))))
    (if goal
        (progn
          ;; Try to make a plan
          (let* ((current-state (list ctx))
                 (goal-state (goal-success-criteria goal))
                 (plan (when (and goal-state (listp goal-state))
                        (plan-for-goal goal-state current-state))))
            (if plan
                (progn
                  ;; Use first step of plan to guide prediction
                  (let ((first-step (first (plan-steps plan))))
                    (when first-step
                      (setf (thinking-result-prediction result)
                            (plan-step-action first-step))
                      (setf (thinking-result-confidence result) 0.5)))
                  (push (list :planned-steps (length (plan-steps plan)))
                        (thinking-result-reasoning-trace result)))
                ;; No plan possible
                (execute-multi-expert ctx result))))
        ;; No goal
        (execute-multi-expert ctx result))))

(defun execute-analogical (ctx situation result)
  "Use similar past situations."
  (let ((similar (situation-similar-past situation)))
    (if similar
        (let* ((best-match (first similar))
               (past-sit (car best-match))
               (similarity (cdr best-match)))
          ;; What happened in that situation?
          (let ((past-ctx (situation-context past-sit)))
            ;; Find what followed that context
            (let ((idx (position past-sit 
                                (coerce *situation-history* 'list)
                                :test #'eq)))
              (when (and idx (< (1+ idx) (fill-pointer *situation-history*)))
                (let ((next-sit (aref *situation-history* (1+ idx))))
                  (let ((next-ctx (situation-context next-sit)))
                    (when (and next-ctx (listp next-ctx))
                      (setf (thinking-result-prediction result)
                            (first next-ctx))
                      (setf (thinking-result-confidence result)
                            (* 0.7 similarity))))))))
          (push (list :used-analogy :similarity similarity)
                (thinking-result-reasoning-trace result)))
        ;; No similar situations
        (execute-multi-expert ctx result))))

(defun execute-exploratory (ctx result)
  "Try something new for novel situations."
  ;; Generate candidates
  (let ((candidates nil))
    ;; Random sample from expert knowledge
    (when (boundp '*experts*)
      (dolist (e (subseq *experts* 0 (min 5 (length *experts*))))
        (maphash (lambda (k v)
                   (declare (ignore k))
                   (push v candidates))
                 (expert-knowledge e))))
    ;; Pick randomly with some randomness
    (if candidates
        (let* ((unique (remove-duplicates candidates :test #'equal))
               (pick (nth (random (length unique)) unique)))
          (setf (thinking-result-prediction result) pick)
          (setf (thinking-result-confidence result) 0.3)
          (push :exploratory-random-pick
                (thinking-result-reasoning-trace result)))
        ;; True fallback - vote
        (execute-multi-expert ctx result)))
  ;; Record that we explored
  (when (boundp '*intrinsic-drives*)
    (satisfy-drive! :curiosity 0.1)))

(defun execute-deliberative (ctx situation result)
  "Full deliberation: use everything available."
  (let ((candidates nil)
        (trace nil))
    
    ;; 1. Check causal model
    (when (boundp '*causal-model*)
      (dolist (tok ctx)
        (dolist (effect-pair (get-effects tok))
          (push (list :source :causal 
                     :prediction (car effect-pair)
                     :confidence (cdr effect-pair))
                candidates)))
      (push :checked-causal-model trace))
    
    ;; 2. Check structure
    (when (situation-structure-present situation)
      (let ((struct-pred (predict-from-structure 
                          (situation-structure-present situation) ctx)))
        (when struct-pred
          (push (list :source :structural
                     :prediction struct-pred
                     :confidence 0.5)
                candidates)))
      (push :checked-structure trace))
    
    ;; 3. Check analogies
    (let ((similar (situation-similar-past situation)))
      (when similar
        (let ((past-sit (caar similar)))
          (let ((past-strategy (situation-recommended-strategy past-sit)))
            (push (list :past-strategy-was past-strategy) trace)))))
    
    ;; 4. Multi-expert vote
    (let ((vote-result (make-thinking-result)))
      (execute-multi-expert ctx vote-result)
      (when (thinking-result-prediction vote-result)
        (push (list :source :expert-vote
                   :prediction (thinking-result-prediction vote-result)
                   :confidence (thinking-result-confidence vote-result))
              candidates))
      (push :checked-expert-vote trace))
    
    ;; 5. Check projections
    (let ((projection (project-future ctx 3)))
      (when projection
        (let ((seq (projection-sequence projection)))
          (when (first seq)
            (push (list :source :temporal-projection
                       :prediction (first seq)
                       :confidence 0.4)
                  candidates)))
        (push :checked-temporal-projection trace)))
    
    ;; 6. Aggregate candidates
    (if candidates
        (let ((votes (make-hash-table :test 'equal)))
          (dolist (c candidates)
            (let ((pred (getf c :prediction))
                  (conf (getf c :confidence)))
              (incf (gethash pred votes 0.0) conf)))
          ;; Pick highest
          (let ((best nil) (best-score 0.0))
            (maphash (lambda (pred score)
                       (when (> score best-score)
                         (setf best pred best-score score)))
                     votes)
            (setf (thinking-result-prediction result) best)
            (setf (thinking-result-confidence result)
                  (min 1.0 (/ best-score (length candidates))))
            (setf (thinking-result-alternatives result) candidates)))
        ;; Fallback
        (execute-fast-default ctx result))
    
    (setf (thinking-result-reasoning-trace result) (nreverse trace))))

;;; ============================================================================
;;; PART 4: OUTCOME MONITORING AND ADAPTATION
;;; ============================================================================
;;; Learn which strategies work in which situations.

(defstruct strategy-stats
  "Statistics for a thinking strategy."
  (strategy nil)
  (uses 0)
  (successes 0)
  (total-confidence 0.0)
  (avg-time 0.0)
  (situation-profiles nil))            ; What situations use this strategy

(defvar *strategy-stats* (make-hash-table)
  "Performance stats for each strategy")

(defun record-strategy-outcome! (result actual)
  "Record outcome for strategy learning."
  (let* ((strategy (thinking-result-strategy-used result))
         (predicted (thinking-result-prediction result))
         (correct (equal predicted actual))
         (stats (or (gethash strategy *strategy-stats*)
                    (setf (gethash strategy *strategy-stats*)
                          (make-strategy-stats :strategy strategy)))))
    (incf (strategy-stats-uses stats))
    (when correct
      (incf (strategy-stats-successes stats)))
    (incf (strategy-stats-total-confidence stats)
          (thinking-result-confidence result))
    ;; Update average time
    (setf (strategy-stats-avg-time stats)
          (/ (+ (* (strategy-stats-avg-time stats) 
                   (1- (strategy-stats-uses stats)))
                (thinking-result-time-spent result))
             (strategy-stats-uses stats)))
    ;; Store situation profile
    (when *current-situation*
      (push (list :novelty (situation-novelty *current-situation*)
                 :uncertainty (situation-uncertainty *current-situation*)
                 :complexity (situation-complexity *current-situation*)
                 :correct correct)
            (strategy-stats-situation-profiles stats)))))

(defun best-strategy-for-situation (situation)
  "Learn which strategy works best for this type of situation."
  (let ((novelty (situation-novelty situation))
        (uncertainty (situation-uncertainty situation))
        (complexity (situation-complexity situation))
        (best-strategy nil)
        (best-score -1.0))
    (maphash 
     (lambda (strategy stats)
       (when (> (strategy-stats-uses stats) 5)
         ;; Calculate expected success rate for similar situations
         (let ((similar-outcomes nil))
           (dolist (profile (strategy-stats-situation-profiles stats))
             (let ((sim (- 1.0 (+ (abs (- novelty (getf profile :novelty)))
                                 (abs (- uncertainty (getf profile :uncertainty)))
                                 (abs (- complexity (getf profile :complexity)))))))
               (when (> sim 0.5)
                 (push (getf profile :correct) similar-outcomes))))
           (when similar-outcomes
             (let ((success-rate (/ (count t similar-outcomes) 
                                   (length similar-outcomes))))
               (when (> success-rate best-score)
                 (setf best-score success-rate
                       best-strategy strategy)))))))
     *strategy-stats*)
    (or best-strategy (recommend-strategy situation))))

;;; ============================================================================
;;; PART 5: MAIN COGNITIVE CONTROL LOOP
;;; ============================================================================
;;; This replaces/wraps execute-with-routing.

(defvar *cognitive-control-enabled* t
  "Whether to use cognitive control or bypass to fast default")

(defvar *thinking-history* nil
  "Recent thinking results for inspection")

(defun think (ctx)
  "Main cognitive control entry point. Replaces direct execute-with-routing."
  (if (not *cognitive-control-enabled*)
      ;; Bypass: just use original routing
      (let ((result (execute-with-routing ctx)))
        (values (op-result-value result) (op-result-confidence result)))
      
      ;; Organic cognitive control - try all resources, aggregate
      (let ((result (think-organic ctx)))
        
        ;; Store for inspection
        (push result *thinking-history*)
        (when (> (length *thinking-history*) 100)
          (setf *thinking-history* (subseq *thinking-history* 0 100)))
        
        ;; Return prediction and confidence
        (values (thinking-result-prediction result)
                (thinking-result-confidence result)
                result))))

(defun think-organic (ctx)
  "Organic thinking: try all available resources, weight by evidence quality.
   Strategy emerges from what contributed, not from pre-classification."
  (let ((result (make-thinking-result))
        (contributions nil)  ; list of (prediction confidence weight source)
        (start-time (get-internal-real-time)))
    
    ;; Load context to working memory
    (wm-load! ctx :type :token :priority 0.6 :source :input)
    
    ;; === TRY ALL AVAILABLE RESOURCES ===
    
    ;; 1. Fast expert lookup (always available, baseline)
    (let ((fast-result (execute-with-routing ctx)))
      (when (op-result-value fast-result)
        (push (list (op-result-value fast-result)
                    (or (op-result-confidence fast-result) 0.5)
                    1.0  ; base weight
                    :expert-routing)
              contributions)))
    
    ;; 2. Multi-expert voting (if multiple experts know this context)
    (when (boundp '*experts*)
      (let* ((key (make-context-key ctx (min 3 (length ctx))))
             (votes (make-hash-table :test 'equal))
             (total-weight 0.0)
             (n-experts 0))
        (dolist (e (subseq *experts* 0 (min 10 (length *experts*))))
          (let ((pred (gethash key (expert-knowledge e))))
            (when pred
              (incf n-experts)
              (let* ((hits (expert-hits e))
                     (misses (expert-misses e))
                     (w (if (> (+ hits misses) 0) (/ hits (+ hits misses)) 0.5)))
                (incf (gethash pred votes 0.0) w)
                (incf total-weight w)))))
        ;; If multiple experts contributed, add vote consensus
        (when (> n-experts 1)
          (let ((best-pred nil) (best-weight 0.0))
            (maphash (lambda (pred weight)
                       (when (> weight best-weight)
                         (setf best-pred pred best-weight weight)))
                     votes)
            (when best-pred
              (push (list best-pred
                          (/ best-weight (max 0.01 total-weight))
                          (* 0.8 (/ n-experts 10.0))  ; weight by expert agreement
                          :multi-expert)
                    contributions))))))
    
    ;; 3. Structure-based prediction (if structure found)
    (when (and (listp ctx) (> (length ctx) 1))
      (let ((struct (try-build-structure ctx)))
        (when struct
          (wm-load! struct :type :structure :priority 0.7)
          (let ((pred (predict-from-structure struct ctx)))
            (when pred
              (push (list pred
                          0.6
                          0.7  ; structural evidence is good
                          :structural)
                    contributions))))))
    
    ;; 4. Causal model prediction (if causal links exist)
    (when (and (boundp '*causal-model*) (listp ctx))
      (let ((predicted-effects nil))
        (dolist (tok (subseq ctx 0 (min 3 (length ctx))))
          (let ((effects (get-effects tok)))
            (when effects
              (setf predicted-effects (append effects predicted-effects)))))
        (when predicted-effects
          ;; Most common predicted effect
          (let ((counts (make-hash-table :test 'equal)))
            (dolist (e predicted-effects)
              (incf (gethash e counts 0)))
            (let ((best nil) (best-count 0))
              (maphash (lambda (e c) 
                         (when (> c best-count)
                           (setf best e best-count c)))
                       counts)
              (when best
                (push (list best
                            (min 1.0 (* 0.2 best-count))
                            0.6  ; causal evidence
                            :causal)
                      contributions)))))))
    
    ;; 5. Similar past situations (if history available)
    (let ((similar (find-similar-situations ctx)))
      (when (and similar (> (cdr (first similar)) 0.7))
        (let ((past-outcome (situation-outcome (car (first similar)))))
          (when past-outcome
            (push (list past-outcome
                        (cdr (first similar))  ; similarity as confidence
                        0.5  ; analogical evidence
                        :analogical)
                  contributions)))))
    
    ;; === AGGREGATE CONTRIBUTIONS ===
    (if (null contributions)
        ;; No evidence - return nil with low confidence
        (progn
          (setf (thinking-result-prediction result) nil)
          (setf (thinking-result-confidence result) 0.0)
          (setf (thinking-result-strategy-used result) :no-evidence))
        
        ;; Weighted vote across all contributions
        (let ((votes (make-hash-table :test 'equal))
              (total-weight 0.0)
              (sources-used nil))
          (dolist (contrib contributions)
            (destructuring-bind (pred conf weight source) contrib
              (let ((effective-weight (* conf weight)))
                (incf (gethash pred votes 0.0) effective-weight)
                (incf total-weight effective-weight)
                (pushnew source sources-used))))
          
          ;; Find winner
          (let ((best-pred nil) (best-weight 0.0))
            (maphash (lambda (pred weight)
                       (when (> weight best-weight)
                         (setf best-pred pred best-weight weight)))
                     votes)
            (setf (thinking-result-prediction result) best-pred)
            (setf (thinking-result-confidence result)
                  (if (> total-weight 0)
                      (/ best-weight total-weight)
                      0.0))
            
            ;; Strategy is EMERGENT - describe what contributed
            (setf (thinking-result-strategy-used result)
                  (cond
                    ((= (length sources-used) 1) (first sources-used))
                    ((member :structural sources-used) :structural-dominant)
                    ((member :causal sources-used) :causal-integrated)
                    ((member :multi-expert sources-used) :expert-consensus)
                    (t :multi-source)))
            
            (setf (thinking-result-alternatives result)
                  (mapcar (lambda (c) 
                            (list :pred (first c) :conf (second c) 
                                  :weight (third c) :source (fourth c)))
                          contributions))
            (setf (thinking-result-reasoning-trace result) sources-used))))
    
    ;; Record time
    (setf (thinking-result-time-spent result)
          (- (get-internal-real-time) start-time))
    
    result))

(defun think-and-learn! (ctx actual)
  "Think about context, then learn from actual outcome."
  (multiple-value-bind (predicted confidence result)
      (think ctx)
    (declare (ignore confidence))
    ;; Record strategy outcome
    (record-strategy-outcome! result actual)
    ;; Update causal model
    (when (and (listp ctx) (first ctx))
      (observe-causal! (first ctx) actual (equal predicted actual)))
    ;; Generate counterfactuals on mistake
    (unless (equal predicted actual)
      (generate-counterfactuals-for-mistake ctx predicted actual))
    ;; Return
    (values predicted (equal predicted actual) result)))

(defun generate-counterfactuals-for-mistake (ctx predicted actual)
  "Generate alternative explanations when prediction fails."
  (let ((counterfactuals nil))
    ;; What if a different strategy had been used?
    (when *current-situation*
      (let ((used (situation-recommended-strategy *current-situation*)))
        (dolist (alt '(:multi-expert :structural :causal :analogical))
          (unless (eq alt used)
            (push (list :alternative-strategy alt
                       :context ctx :would-predict :unknown)
                  counterfactuals)))))
    ;; What if context was different?
    (when (and (listp ctx) (> (length ctx) 1))
      (push (list :shorter-context (butlast ctx)
                  :might-change :prediction-basis)
            counterfactuals))
    ;; Record the counterfactual for learning
    (when (and counterfactuals (boundp '*causal-model*))
      (add-causal-link! predicted actual :strength 0.3))
    counterfactuals))

;;; ============================================================================
;;; PART 6: INTEGRATION - REPLACE CORE PREDICTION
;;; ============================================================================

(defun cognitive-predict! (ctx)
  "Drop-in replacement for core prediction that uses cognitive control."
  (multiple-value-bind (pred conf result)
      (think ctx)
    (declare (ignore result))
    (make-op-result :type :return :value pred :confidence (or conf 0.0))))

;; Hook to wire cognitive control into processing
(defun cognitive-control-hook (tok ctx step)
  "Pre-process hook to enable cognitive control."
  (declare (ignore tok step))
  ;; Tick working memory
  (when (fboundp 'wm-tick!)
    (wm-tick!))
  ;; Update drives
  (when (and (fboundp 'update-drives!) (boundp '*intrinsic-drives*))
    (update-drives!))
  ;; Assess the current situation (populates situation history)
  (when (and ctx (listp ctx) (>= (length ctx) 1))
    (assess-situation ctx))
  ;; Use cognitive control if we have context
  (when (and ctx (listp ctx) (>= (length ctx) 1))
    (let ((short-ctx (subseq ctx 0 (min (if (boundp '*context-scales*)
                                            (apply #'max *context-scales*)
                                            8)
                                        (length ctx)))))
      (multiple-value-bind (pred conf result)
          (think short-ctx)
        (declare (ignore result))
        (when pred
          (make-op-result :type :return :value pred :confidence (or conf 0.0)))))))

(defun cognitive-post-hook (actual ctx predicted correct-p)
  "Post-process hook for cognitive learning."
  (declare (ignore predicted))
  ;; Record strategy outcome if we have a recent result
  (when (and *thinking-history* (first *thinking-history*))
    (record-strategy-outcome! (first *thinking-history*) actual))
  ;; Update causal model
  (when (and (listp ctx) (first ctx) (boundp '*causal-model*))
    (observe-causal! (first ctx) actual correct-p)))

;;; ============================================================================
;;; PART 7: INITIALIZATION AND REPORTING
;;; ============================================================================

(defun initialize-cognitive-control! ()
  "Initialize the cognitive controller."
  ;; Initialize dependencies if not already done
  (unless (boundp '*working-memory*)
    (when (fboundp 'initialize-compositional-reasoning!)
      (initialize-compositional-reasoning!)))
  
  ;; Clear state
  (setf *current-situation* nil)
  (setf (fill-pointer *situation-history*) 0)
  (setf *thinking-history* nil)
  (clrhash *strategy-stats*)
  
  ;; Initialize strategy stats
  (dolist (s '(:fast-default :multi-expert :structural :causal 
               :planning :analogical :exploratory :deliberative))
    (setf (gethash s *strategy-stats*) (make-strategy-stats :strategy s)))
  
  ;; Enable cognitive control (hooks are registered via post-reset hook)
  (setf *cognitive-control-enabled* t)
  
  (format t "~%Cognitive Controller initialized.~%")
  (format t "Use (think ctx) for single predictions.~%")
  (format t "Use (think-and-learn! ctx actual) for learning.~%")
  (format t "Set *cognitive-control-enabled* to nil to bypass.~%"))

(defun print-cognitive-status ()
  "Print cognitive controller status."
  (format t "~%=== COGNITIVE CONTROLLER STATUS ===~%")
  (format t "Enabled: ~A~%" *cognitive-control-enabled*)
  
  (format t "~%CURRENT SITUATION:~%")
  (when *current-situation*
    (format t "  Novelty:      ~,2F~%" (situation-novelty *current-situation*))
    (format t "  Uncertainty:  ~,2F~%" (situation-uncertainty *current-situation*))
    (format t "  Complexity:   ~,2F~%" (situation-complexity *current-situation*))
    (format t "  Goal-relevant:~,2F~%" (situation-goal-relevance *current-situation*))
    (format t "  Causal density:~,2F~%" (situation-causal-density *current-situation*))
    (format t "  Strategy:     ~A~%" (situation-recommended-strategy *current-situation*)))
  
  (format t "~%STRATEGY PERFORMANCE:~%")
  (let ((sorted nil))
    (maphash (lambda (strategy stats)
               (when (> (strategy-stats-uses stats) 0)
                 (push (list strategy
                            (strategy-stats-uses stats)
                            (if (> (strategy-stats-uses stats) 0)
                                (/ (strategy-stats-successes stats)
                                   (strategy-stats-uses stats))
                                0.0))
                       sorted)))
             *strategy-stats*)
    (setf sorted (sort sorted #'> :key #'second))
    (dolist (entry sorted)
      (format t "  ~15A: ~4D uses, ~,1F% success~%"
              (first entry) (second entry) (* 100 (third entry)))))
  
  (format t "~%RECENT THINKING:~%")
  (dolist (result (subseq *thinking-history* 0 (min 3 (length *thinking-history*))))
    (format t "  ~A -> ~A (conf:~,2F)~%"
            (thinking-result-strategy-used result)
            (thinking-result-prediction result)
            (thinking-result-confidence result)))
  
  (format t "~%SITUATION HISTORY: ~A assessments~%" (fill-pointer *situation-history*)))

(defun explain-last-thought ()
  "Explain the most recent thinking process."
  (let ((result (first *thinking-history*)))
    (if result
        (progn
          (format t "~%=== LAST THINKING PROCESS ===~%")
          (format t "Strategy: ~A~%" (thinking-result-strategy-used result))
          (format t "Prediction: ~A~%" (thinking-result-prediction result))
          (format t "Confidence: ~,2F~%" (thinking-result-confidence result))
          (format t "~%Reasoning trace:~%")
          (dolist (step (thinking-result-reasoning-trace result))
            (format t "  ~A~%" step))
          (format t "~%Alternatives considered:~%")
          (dolist (alt (thinking-result-alternatives result))
            (format t "  ~A~%" alt))
          (format t "~%Working memory after:~%")
          (dolist (item (thinking-result-working-memory-used result))
            (format t "  ~A~%" item)))
        (format t "No thinking history yet.~%"))))

;;; ============================================================================
;;; EXPORTS
;;; ============================================================================

(export '(;; Main entry points
          think
          think-and-learn!
          cognitive-predict!
          
          ;; Situation assessment
          assess-situation
          *current-situation*
          
          ;; Strategy control
          *cognitive-control-enabled*
          best-strategy-for-situation
          
          ;; Inspection
          *thinking-history*
          *strategy-stats*
          explain-last-thought
          
          ;; Initialization
          initialize-cognitive-control!
          print-cognitive-status))

;;; Register hooks at load time via post-reset hook
;;; This ensures hooks are registered after every reset! call
(register-hook +hook-post-reset+ 
               (lambda () 
                 (register-hook +hook-pre-process-token+ 'cognitive-control-hook :priority 10)
                 (register-hook +hook-post-process-token+ 'cognitive-post-hook :priority 90)
                 (setf *cognitive-control-enabled* t))
               :priority 50)

(format t "~%")
(format t "================================================================~%")
(format t "UHMA v6.8 COGNITIVE CONTROLLER LOADED~%")
(format t "================================================================~%")
(format t "~%")
(format t "This module WIRES THE MACHINERY TOGETHER.~%")
(format t "~%")
(format t "Before: input -> find expert -> output~%")
(format t "After:  input -> assess -> select strategy -> execute -> learn~%")
(format t "~%")
(format t "Strategies available:~%")
(format t "  :fast-default   - Quick expert lookup (familiar cases)~%")
(format t "  :multi-expert   - Vote across experts (uncertain cases)~%")
(format t "  :structural     - Use compositional structure~%")
(format t "  :causal         - Reason through causal model~%")
(format t "  :planning       - Multi-step lookahead~%")
(format t "  :analogical     - Use similar past situations~%")
(format t "  :exploratory    - Try something new (novel cases)~%")
(format t "  :deliberative   - Full deliberation (complex/important)~%")
(format t "~%")
(format t "Usage:~%")
(format t "  (initialize-cognitive-control!)~%")
(format t "  (think '(the cat sat))          ; Single prediction~%")
(format t "  (think-and-learn! ctx actual)   ; Predict and learn~%")
(format t "  (explain-last-thought)          ; See reasoning~%")
(format t "  (print-cognitive-status)        ; Full status~%")
(format t "================================================================~%")
