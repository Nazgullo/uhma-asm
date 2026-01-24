;;;; ============================================================================
;;;; UHMA v6.10 EPISODIC MEMORY INTEGRATION PATCHES
;;;; ============================================================================
;;;;
;;;; This file patches existing modules to integrate with episodic memory:
;;;;   - Self-expectation now consults episodic memory
;;;;   - Dreams now consolidate episodes
;;;;   - Describe-self now includes history
;;;;
;;;; Load AFTER uhma-v6.10-episodic-memory.lisp
;;;; ============================================================================

(in-package :uhma)

;;; ============================================================================
;;; PATCH 1: ENHANCE SELF-EXPECTATION WITH EPISODIC MEMORY
;;; ============================================================================
;;; predict-own-behavior should ask "what did I do last time in similar situations?"

;; Store the original function before redefining
(defvar *original-predict-own-behavior* #'predict-own-behavior
  "Original predict-own-behavior before episodic enhancement")

(defun predict-own-behavior (ctx)
  "Before executing, predict what I'll do - NOW CONSULTING EPISODIC MEMORY."
  ;; First, consult episodic memory for similar situations
  (let* ((episodic-hints (when (boundp '*episodic-memory*)
                          (consult-episodic-for-self-prediction ctx)))
         (base-expectation (funcall *original-predict-own-behavior* ctx)))
    
    ;; Adjust predictions based on episodic memory
    (when (and episodic-hints base-expectation)
      (let ((typical-outcome (getf episodic-hints :typical-outcome))
            (typical-surprise (getf episodic-hints :typical-self-surprise)))
        ;; If we have historical data about similar contexts, weight toward it
        (when typical-outcome
          (case typical-outcome
            (:success 
             (when (eq (self-expectation-expected-outcome base-expectation) :uncertain)
               (setf (self-expectation-expected-outcome base-expectation) :success)))
            (:failure
             (when (eq (self-expectation-expected-outcome base-expectation) :uncertain)
               (setf (self-expectation-expected-outcome base-expectation) :failure)))))
        
        ;; If we typically surprise ourselves in this context, note it
        (when (and typical-surprise (> typical-surprise 0.5))
          ;; Reduce confidence in prediction
          (let ((current-conf (self-expectation-expected-confidence base-expectation)))
            (setf (self-expectation-expected-confidence base-expectation)
                  (max 0.1 (- current-conf (* 0.2 typical-surprise))))))))
    
    base-expectation))

;;; ============================================================================
;;; PATCH 2: ENHANCE DREAM CYCLE WITH EPISODE CONSOLIDATION
;;; ============================================================================
;;; Dreams should consolidate episodic memory into semantic knowledge

(defvar *original-run-dream-cycle* #'run-dream-cycle!
  "Original run-dream-cycle! before episodic enhancement")

(defun run-dream-cycle! ()
  "Run a dream consolidation cycle - NOW INCLUDING EPISODE CONSOLIDATION."
  ;; Maintain *dreaming* guard to prevent recursion (DG fix)
  (let ((*dreaming* t))
    ;; Run original dream cycle
    (let ((result (funcall *original-run-dream-cycle*)))

      ;; Additionally consolidate episodic memory
      (when (boundp '*episodic-memory*)
        (let ((consolidated (dream-consolidate-episodes!)))
          (when result
            (setf (getf result :episodes-consolidated) consolidated))))

      result)))

;;; ============================================================================
;;; PATCH 3: ENHANCE DESCRIBE-SELF WITH HISTORY
;;; ============================================================================
;;; Override describe-self to include autobiographical information

(defun describe-self ()
  "Generate a high-level self-description WITH TEMPORAL HISTORY."
  (if (boundp '*episodic-memory*)
      (describe-self-with-history)
      ;; Fallback to original if episodic memory not loaded
      (with-output-to-string (s)
        (format s "I am a cognitive system with ~A experts organized into ~A neighborhoods.~%"
                (length *experts*) (length *neighborhoods*))
        (format s "I have consolidated ~A patterns into long-term memory.~%"
                (hash-table-count *long-term-memory*))
        (when (and (boundp '*self-model*) *self-model*)
          (format s "I estimate my error rate at ~,1F% with ~,0F% confidence in myself.~%"
                  (* 100 (self-model-expected-error *self-model*))
                  (* 100 (self-model-global-confidence *self-model*)))))))

;;; ============================================================================
;;; PATCH 4: RECORD SELF-EXPECTATIONS TO EPISODIC MEMORY
;;; ============================================================================
;;; When self-expectation is recorded, also note it in the current episode

(defvar *original-record-self-expectation* #'record-self-expectation!
  "Original record-self-expectation! before episodic enhancement")

(defun record-self-expectation! (exp)
  "Record self-expectation to buffer AND to episodic memory."
  ;; Original recording
  (funcall *original-record-self-expectation* exp)
  ;; Also record to episodic memory
  (when (and (boundp '*episodic-memory*) exp)
    (episodic-memory-self-expectation-hook exp)))

;;; ============================================================================
;;; PATCH 5: TRIGGER EPISODE EVENTS FROM INTROSPECTION
;;; ============================================================================
;;; When introspection runs, record it as an episode event

(defun episodic-memory-introspection-hook (level observations)
  "Record introspection event into the current episode."
  (when (and (boundp '*episodic-memory*) *episodic-memory*
             (episodic-memory-current-episode *episodic-memory*))
    (push (make-episode-event :step *step* :type :introspection
                              :data (list :level level :observations observations))
          (episode-events (episodic-memory-current-episode *episodic-memory*)))))

(defvar *original-introspect-at-layer* #'introspect-at-layer!
  "Original introspect-at-layer! before episodic enhancement")

(defun introspect-at-layer! (layer level)
  "Run introspection for a specific layer AND record to episodic memory."
  ;; Run original
  (funcall *original-introspect-at-layer* layer level)
  ;; Record to episodic memory
  (when (boundp '*episodic-memory*)
    (episodic-memory-introspection-hook 
     level 
     (introspection-layer-observations layer))))

;;; ============================================================================
;;; PATCH 6: INTEGRATE CONCEPT ACTIVATION WITH EPISODES
;;; ============================================================================
;;; When concepts are activated, note them in the current episode

(defvar *original-record-concept-activation* #'record-concept-activation!
  "Original record-concept-activation! before episodic enhancement")

(defun record-concept-activation! (concepts)
  "Record concept activation AND note in current episode."
  ;; Original recording
  (funcall *original-record-concept-activation* concepts)
  ;; Also note in current episode
  (when (and (boundp '*episodic-memory*)
             (episodic-memory-current-episode *episodic-memory*))
    (let ((ep (episodic-memory-current-episode *episodic-memory*)))
      (setf (episode-concepts-activated ep)
            (union concepts (episode-concepts-activated ep) :test #'eq)))))

;;; ============================================================================
;;; PATCH 7: CONNECT DRIFT DETECTION TO EPISODES
;;; ============================================================================
;;; Significant drift should trigger episode boundary

(defvar *original-check-for-drift* #'check-for-drift!
  "Original check-for-drift! before episodic enhancement")

(defun check-for-drift! (dd accuracy)
  "Check if drift has occurred AND maybe trigger episode boundary."
  ;; Original check
  (funcall *original-check-for-drift* dd accuracy)
  ;; If drift detected, force episode boundary
  (let ((baseline (or (drift-detector-baseline-accuracy dd) 0.5)))
    (when (and (> (abs (- accuracy baseline)) 0.2)
               (boundp '*episodic-memory*)
               (episodic-memory-current-episode *episodic-memory*))
      ;; End current episode due to drift
      (end-episode! :drift-detected)
      (start-episode! :distribution-drift))))

;;; ============================================================================
;;; EXPORT NEW FUNCTIONS
;;; ============================================================================

(export '(what-have-i-learned
          how-have-i-changed
          describe-self-with-history
          describe-semantic-self
          recall-autobiographical
          recall-similar-episodes
          recall-episodes-by-tag
          recall-significant-episodes
          print-recent-episodes
          print-episodic-memory-status
          print-autobiographical-summary
          consult-episodic-for-self-prediction))

;;; ============================================================================
;;; LOAD CONFIRMATION
;;; ============================================================================

(format t "~%================================================================~%")
(format t "UHMA v6.10 EPISODIC MEMORY INTEGRATION PATCHES LOADED~%")
(format t "================================================================~%")
(format t "~%Integrations:~%")
(format t "  - Self-expectation now consults episodic memory~%")
(format t "  - Dreams now consolidate episodes -> semantic knowledge~%")
(format t "  - Describe-self now includes temporal history~%")
(format t "  - Self-expectations recorded to episodes~%")
(format t "  - Introspection events recorded to episodes~%")
(format t "  - Concept activations tracked in episodes~%")
(format t "  - Drift detection can trigger episode boundaries~%")
(format t "================================================================~%")
