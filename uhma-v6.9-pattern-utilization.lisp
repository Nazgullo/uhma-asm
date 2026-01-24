;;;; ============================================================================
;;;; UHMA PATTERN UTILIZATION (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Makes learned patterns actionable and domain-agnostic.
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 4: STRUCTURES ---

(defstruct pattern-signature
  "A domain-agnostic representation of structural patterns."
  (id (gensym "PAT-") :type symbol)
  (template nil :type list)
  (instances nil :type list)
  (frequency 0 :type fixnum)
  (contexts nil :type list)
  (outcomes nil :type list)
  (confidence 0.0 :type single-float)
  (created-step 0 :type fixnum)
  (last-seen-step 0 :type fixnum))

(defstruct familiarity-assessment
  "Quantitative familiarization score."
  (score 0.0 :type single-float)
  (matching-patterns nil :type list)
  (coverage 0.0 :type single-float)
  (confidence-boost 0.0 :type single-float))

(defstruct meta-pattern
  "Higher-order patterns regarding system performance."
  (condition nil :type symbol)
  (observation nil :type symbol)
  (action nil :type list)
  (effectiveness 0.0 :type single-float)
  (applications 0 :type fixnum))

;;; --- SECTION 6: SPECIAL VARIABLES ---

(defvar *pattern-signatures* (make-hash-table :test 'equal))
(defvar *meta-patterns* (make-hash-table :test 'equal))
(defvar *pattern-signature-threshold* 3 )

;;; --- SECTION 8: CORE FUNCTIONS ---

(defun abstract-pattern (tokens)
  "Map concrete tokens to structural variables."
  (declare (type list tokens))
  (let ((seen (make-hash-table :test 'equal)) (var-count 0))
    (mapcar (lambda (tok)
              (let ((ts (string tok)))
                (cond ((member tok '(|.| |,| |(| |)| |[| |]| DEFUN LET SETF)) tok)
                      ((every #'digit-char-p ts) :NUM)
                      ((gethash tok seen))
                      (t (let ((v (intern (format nil "?~D" (incf var-count)) :keyword)))
                           (setf (gethash tok seen) v) v)))))
            tokens)))

(defun register-pattern-at-length! (tokens context outcome)
  "Commit structural pattern to long-term signature store."
  (declare (type list tokens))
  (let* ((abstract (abstract-pattern tokens))
         (key (format nil "~{~A~^|~}" abstract))
         (sig (or (gethash key *pattern-signatures*)
                  (setf (gethash key *pattern-signatures*)
                        (make-pattern-signature :template abstract :created-step *step*)))))
    (push tokens (pattern-signature-instances sig))
    (incf (pattern-signature-frequency sig))
    (setf (pattern-signature-last-seen-step sig) *step*)
    (when context (pushnew context (pattern-signature-contexts sig) :test #'equal))
    (when outcome (push outcome (pattern-signature-outcomes sig)))
    ;; Confidence calculation
    (let ((f (pattern-signature-frequency sig)) (o (pattern-signature-outcomes sig)))
      (setf (pattern-signature-confidence sig)
            (if (< f *pattern-signature-threshold*) 0.0
                (* (min 1.0 (/ f 20.0)) (if o (/ (count (first o) o :test #'equal) (float (length o))) 0.5)))))
    sig))

(defun find-matching-patterns (tokens)
  "Find pattern signatures matching subsequences of tokens."
  (declare (type list tokens))
  (let ((results nil)
        (len (length tokens)))
    (loop for size from 2 to (min len 10)
          do (loop for start from 0 to (- len size)
                   for sub = (subseq tokens start (+ start size))
                   for key = (format nil "~{~A~^|~}" (abstract-pattern sub))
                   for sig = (gethash key *pattern-signatures*)
                   when (and sig (>= (pattern-signature-frequency sig) *pattern-signature-threshold*))
                   do (pushnew sig results :test #'eq)))
    results))

(defun assess-familiarity (tokens)
  "Compute familiarity score and confidence boost."
  (declare (type list tokens))
  (let* ((matches (find-matching-patterns tokens))
         (count (length matches))
         (score (if (zerop count) 0.0 (min 1.0 (/ (reduce #'+ matches :key #'pattern-signature-confidence) count)))))
    (make-familiarity-assessment :score score :matching-patterns matches
                                 :coverage (if (zerop (length tokens)) 0.0 (min 1.0 (/ count (float (length tokens)))))
                                 :confidence-boost (* score 0.3))))

;;; --- SECTION 10: INITIALIZATION ---
(eval-when (:load-toplevel :execute)
  (register-hook +hook-post-process-token+ 'pattern-post-token-hook :priority 60)
  (format t "[PATTERN-UTIL] Recognition and Transfer active.~%"))

;;; ============================================================================
;;; STRUCTURE TRANSFER - "This is like something I know"
;;; ============================================================================

(defun find-analogous-patterns (tokens)
  "Find patterns from other contexts that have similar structure."
  (let* ((abstract (abstract-pattern tokens))
         (structure-length (length abstract))
         (analogies nil))
    (maphash (lambda (k sig)
               (let ((template (pattern-signature-template sig)))
                 (when (and (= (length template) structure-length)
                            (> (structural-similarity abstract template) 0.5)
                            (not (equal abstract template)))
                   (push (cons sig (structural-similarity abstract template))
                         analogies))))
             *pattern-signatures*)
    (sort analogies #'> :key #'cdr)))

(defun structural-similarity (pat1 pat2)
  "Compute structural similarity between two abstract patterns."
  (if (or (null pat1) (null pat2) (/= (length pat1) (length pat2)))
      0.0
      (let ((matches 0)
            (total (length pat1)))
        (loop for a in pat1
              for b in pat2
              do (when (or (equal a b)
                           (and (keywordp a) (keywordp b)))  ; Both are variables
                   (incf matches)))
        (/ matches total 1.0))))

(defun transfer-prediction (tokens)
  "Use analogous patterns to make predictions for unfamiliar input."
  (let ((analogies (find-analogous-patterns tokens)))
    (when analogies
      (let* ((best-analogy (car analogies))
             (sig (car best-analogy))
             (outcomes (pattern-signature-outcomes sig)))
        (when outcomes
          ;; Return most common outcome, scaled by analogy strength
          (values (first outcomes)
                  (* (cdr best-analogy) 
                     (pattern-signature-confidence sig))))))))

;;; ============================================================================
;;; SELF-MODIFICATION FROM PATTERNS - "What works should shape how I work"
;;; ============================================================================

(defun record-meta-observation! (condition observation outcome)
  "Record an observation about system behavior."
  (let* ((key (list condition observation))
         (mp (or (gethash key *meta-patterns*)
                 (setf (gethash key *meta-patterns*)
                       (make-meta-pattern :condition condition
                                          :observation observation)))))
    (incf (meta-pattern-applications mp))
    ;; Update effectiveness based on outcome
    (let ((current (meta-pattern-effectiveness mp))
          (n (meta-pattern-applications mp)))
      (setf (meta-pattern-effectiveness mp)
            (+ (* current (/ (1- n) n))
               (* (if outcome 1.0 0.0) (/ 1.0 n)))))
    mp))

(defun derive-self-modifications ()
  "Analyze meta-patterns to derive potential self-modifications."
  (let ((modifications nil))
    (maphash (lambda (k mp)
               (when (and (> (meta-pattern-applications mp) 10)
                          (> (meta-pattern-effectiveness mp) 0.7))
                 (push (list :condition (meta-pattern-condition mp)
                             :action (suggest-action-for-pattern mp)
                             :confidence (meta-pattern-effectiveness mp))
                       modifications)))
             *meta-patterns*)
    modifications))

(defun suggest-action-for-pattern (mp)
  "Suggest a self-modification action based on meta-pattern."
  (let ((obs (meta-pattern-observation mp)))
    (cond
      ((eq obs :high-confidence-correct)
       '(:increase-boldness :trust-pattern-more))
      ((eq obs :high-confidence-wrong)
       '(:decrease-confidence :seek-alternatives))
      ((eq obs :low-confidence-correct)
       '(:this-pattern-is-learnable :increase-attention))
      ((eq obs :familiar-context-success)
       '(:familiarity-is-good-signal :weight-known-patterns))
      ((eq obs :novel-context-failure)
       '(:be-cautious-with-novelty :explore-more))
      ((eq obs :analogy-helped)
       '(:structure-transfer-works :use-analogies))
      (t nil))))

;;; ============================================================================
;;; INTEGRATION WITH EXISTING SYSTEM
;;; ============================================================================

(defun utilize-patterns! (context)
  "Main entry point: utilize learned patterns for a given context.
   Returns enhancement info that can modify prediction behavior."
  (let* ((familiarity (assess-familiarity context))
         (analogies (find-analogous-patterns context))
         (confidence-modifier (familiarity-assessment-confidence-boost familiarity)))
    (values
     ;; Primary: familiarity-based confidence adjustment
     confidence-modifier
     ;; Secondary: analogy-based prediction if available
     (when analogies
       (transfer-prediction context))
     ;; Tertiary: full familiarity assessment
     familiarity)))

(defun context-familiarity (context)
  "STUB: Computes the familiarity of a context."
  (declare (ignore context))
  0.5)

(defun pattern-enhanced-predict (context)
  "Make a prediction with pattern utilization."
  (multiple-value-bind (conf-boost analogy-pred familiarity)
      (utilize-patterns! context)
    (let ((base-prediction (generate-prediction context)))
      (if base-prediction
          ;; Enhance existing prediction with familiarity
          (values base-prediction
                  (+ (or (second base-prediction) 0.5) conf-boost)
                  familiarity)
          ;; Fall back to analogy if no direct prediction
          (when analogy-pred
            (values analogy-pred
                    (* 0.5 conf-boost)  ; Lower confidence for analogy
                    familiarity))))))

;;; Hook into learning process - NOW DONE VIA PROPER HOOK REGISTRATION
;;; See initialize-pattern-utilization! which registers pattern-post-token-hook

;;; ============================================================================
;;; STATUS AND INTROSPECTION
;;; ============================================================================

(defun print-pattern-utilization-status ()
  "Print status of pattern utilization system."
  (format t "~%=== PATTERN UTILIZATION STATUS ===~%")
  (format t "Learned patterns: ~D~%" (hash-table-count *pattern-signatures*))
  (format t "Meta-patterns: ~D~%" (hash-table-count *meta-patterns*))
  
  ;; Top patterns by confidence
  (format t "~%Top patterns by confidence:~%")
  (let ((sorted nil))
    (maphash (lambda (k sig)
               (push sig sorted))
             *pattern-signatures*)
    (setf sorted (sort sorted #'> :key #'pattern-signature-confidence))
    (loop for sig in (subseq sorted 0 (min 5 (length sorted)))
          do (format t "  ~,2F: ~A (seen ~D times)~%"
                     (pattern-signature-confidence sig)
                     (pattern-signature-template sig)
                     (pattern-signature-frequency sig))))
  
  ;; Effective meta-patterns
  (format t "~%Effective meta-patterns:~%")
  (let ((effective nil))
    (maphash (lambda (k mp)
               (when (> (meta-pattern-effectiveness mp) 0.5)
                 (push mp effective)))
             *meta-patterns*)
    (dolist (mp (subseq (sort effective #'> :key #'meta-pattern-effectiveness)
                        0 (min 5 (length effective))))
      (format t "  ~,2F: ~A -> ~A~%"
              (meta-pattern-effectiveness mp)
              (meta-pattern-condition mp)
              (meta-pattern-observation mp))))
  
  ;; Suggested modifications
  (format t "~%Suggested self-modifications:~%")
  (let ((mods (derive-self-modifications)))
    (if mods
        (dolist (m mods)
          (format t "  When ~A: ~A (conf: ~,2F)~%"
                  (getf m :condition)
                  (getf m :action)
                  (getf m :confidence)))
        (format t "  (none yet - need more experience)~%"))))

(defun reset-pattern-utilization! ()
  "Reset pattern utilization state."
  (setf *pattern-signatures* (make-hash-table :test 'equal))
  (setf *meta-patterns* (make-hash-table :test 'equal)))

;;; ============================================================================
;;; HOOK INTEGRATION - O(n) automatic registration
;;; ============================================================================

(defun pattern-post-token-hook (token ctx predicted correct)
  "Hook called after each token - registers patterns automatically.
   This is O(n) per token vs O(n²) manual registration."
  (declare (ignore token))
  (when (and ctx (>= (length ctx) 2))
    ;; Register pattern at current context length only (O(1) per call)
    (register-pattern-at-length! (reverse ctx) 
                                  (when (> (length ctx) 2)
                                    (subseq ctx 0 2))
                                  predicted)
    ;; Record meta-observation for learning
    (let ((familiarity (assess-familiarity (reverse ctx))))
      (cond
        ((and (> (familiarity-assessment-score familiarity) 0.7) correct)
         (record-meta-observation! :familiar :high-confidence-correct t))
        ((and (> (familiarity-assessment-score familiarity) 0.7) (not correct))
         (record-meta-observation! :familiar :high-confidence-wrong nil))
        ((and (< (familiarity-assessment-score familiarity) 0.3) correct)
         (record-meta-observation! :novel :low-confidence-correct t))
        ((and (< (familiarity-assessment-score familiarity) 0.3) (not correct))
         (record-meta-observation! :novel :novel-context-failure nil))))))

;;; ============================================================================
;;; INITIALIZATION
;;; ============================================================================

(defun initialize-pattern-utilization! ()
  "Initialize the pattern utilization system and register hooks."
  (reset-pattern-utilization!)
  ;; Register the hook for automatic O(n) pattern learning
  (register-hook +hook-post-process-token+ 'pattern-post-token-hook :priority 60)

  ;; Also register for +hook-post-learn+ so pattern utilization works during rehearsal/live mode
  (register-hook +hook-post-learn+
                 (lambda (ctx actual predicted correct learner)
                   (declare (ignore learner))
                   ;; pattern-post-token-hook signature: (tok ctx predicted got-it)
                   (pattern-post-token-hook actual ctx predicted correct))
                 :priority 60)

  (format t "Pattern Utilization initialized with hook registration.~%")
  (format t "Patterns become actionable after ~D instances.~%"
          *pattern-signature-threshold*))

(format t "~%================================================================~%")
(format t "UHMA v6.9 PATTERN UTILIZATION MODULE LOADED~%")
(format t "================================================================~%")
(format t "The system can now USE what it learns:~%")
(format t "  - Pattern recognition (I've seen this before)~%")
(format t "  - Familiarity assessment (I know this well)~%")
(format t "  - Structure transfer (this is like something I know)~%")
(format t "  - Self-modification from patterns (what works shapes me)~%")
(format t "~%All domain-agnostic. Works on any sequential input.~%")
(format t "================================================================~%")
