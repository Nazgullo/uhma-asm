;;;; ============================================================================
;;;; UHMA SELF-AWARENESS LOOP (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Wires the reflective cycle: problem detection → introspection → fix → learn.
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 6: SPECIAL VARIABLES ---

(defvar *self-awareness-loop-active* nil )
(defvar *modification-attempts* nil )
(defvar *understanding-log* nil )
(defvar *last-synthesis-trigger-step* 0 )

;;; --- SECTION 8: CORE LOOP LOGIC ---

(defun problem-detected-p ()
  "Check for qualitative mental indicators of performance stagnation."
  (when (and (boundp '*cached-active-concepts*) *cached-active-concepts*)
    (intersection *cached-active-concepts* '(STUCK FAILING CONFUSED SURPRISED))))

(defun trigger-problem-response! (problems)
  "Activate deep introspection and baseline recording upon problem detection."
  (declare (type list problems))
  (format t "[AWARENESS] Problem: ~A at step ~D~%" problems *step*)
  (when (fboundp 'introspect!) (introspect! (presence-guided-introspection-depth)))
  (push (list :step *step* :accuracy (compute-recent-accuracy-fast 50)) *outcome-tracking-window*))

(defun run-self-awareness-loop! ()
  "Orchestrate one full cycle of reflective autonomous plasticity."
  (setf *self-awareness-loop-active* t)
  (let ((probs (problem-detected-p)))
    (when probs
      (trigger-problem-response! probs)
      (form-hypothesis-from-introspection!)))
  (when (should-attempt-synthesis-p) (attempt-synthesis-from-best-schemas!))
  (correlate-modifications-with-outcomes!)
  (setf *self-awareness-loop-active* nil))

;;; --- SECTION 10: INITIALIZATION ---
(eval-when (:load-toplevel :execute)
  (register-hook +hook-maintenance+ (lambda () (when (zerop (mod *step* 50)) (run-self-awareness-loop!))) :priority 70)
  (format t "[AWARENESS-LOOP] Reflective cognition synchronized.~%"))

;;; ============================================================================
;;; STEP 2-3: INTROSPECTION + PRESENCE → HYPOTHESIS FORMATION
;;; ============================================================================
;;; Introspection data + presence state → concrete hypothesis about cause

(defun form-hypothesis-from-introspection! ()
  "Form hypothesis about root cause from introspection and presence.
   WIRED: traces + presence + code-awareness → hypothesis (homoiconic: observation=code=change)."
  (let ((recent-failures nil)
        (common-contexts (make-hash-table :test 'equal))
        (common-ops (make-hash-table :test 'eq))
        (code-insights nil))

    ;; WIRED: Get code-level insights if available
    (when (fboundp 'introspect-code-awareness!)
      (setf code-insights (introspect-code-awareness!)))

    ;; Analyze recent traces for failure patterns
    (when (and (boundp '*trace-buffer*) (> (fill-pointer *trace-buffer*) 0))
      (let ((limit (min 50 (fill-pointer *trace-buffer*))))
        (dotimes (i limit)
          (let ((trace (aref *trace-buffer* (- (fill-pointer *trace-buffer*) 1 i))))
            (when (and (cognitive-trace-p trace)
                       (not (eq (cognitive-trace-prediction trace)
                                (cognitive-trace-actual trace))))
              (push trace recent-failures)
              ;; Collect context tokens from failures
              (dolist (tok (cognitive-trace-context trace))
                (incf (gethash tok common-contexts 0)))
              ;; Collect ops from reasoning path
              (dolist (step (cognitive-trace-reasoning-path trace))
                (when (consp step)
                  (incf (gethash (car step) common-ops 0)))))))))

    ;; Find most common failure context elements
    (let ((top-ctx nil)
          (top-ops nil))
      (maphash (lambda (tok count)
                 (when (> count 3)
                   (push (cons tok count) top-ctx)))
               common-contexts)
      (setf top-ctx (sort top-ctx #'> :key #'cdr))

      (maphash (lambda (op count)
                 (when (> count 2)
                   (push (cons op count) top-ops)))
               common-ops)
      (setf top-ops (sort top-ops #'> :key #'cdr))

      ;; WIRED: Code insights can also identify problematic ops
      (when code-insights
        (dolist (insight code-insights)
          (let ((reliabilities (getf insight :reliabilities)))
            (when (and reliabilities (< (apply #'min reliabilities) 0.4))
              ;; Code-awareness found unreliable code - add to top-ops
              (let ((code-id (getf insight :code-id)))
                (when code-id
                  (push (cons code-id 10) top-ops)))))))  ; High priority

      ;; Form hypothesis based on findings
      (cond
        ;; If specific ops are frequently in failure paths, hypothesize about them
        ((and top-ops (> (length top-ops) 0))
         (let ((failing-op (caar top-ops)))
           (generate-hypothesis!
            (list :param-effect failing-op :underperforming
                  :contexts (mapcar #'car (subseq top-ctx 0 (min 3 (length top-ctx))))
                  :code-aware (when code-insights t)))))  ; Mark if code-aware

        ;; If specific contexts are common in failures, hypothesize about them
        ((and top-ctx (> (length top-ctx) 0))
         (generate-hypothesis!
          (list :structure-effect 'context-handling
                (mapcar (lambda (pair) (list (car pair) (cdr pair)))
                        (subseq top-ctx 0 (min 5 (length top-ctx)))))))

        ;; WIRED: Presence trajectory + texture guide hypothesis
        ((and (boundp '*presence*) *presence*)
         (let ((trajectory (presence-trajectory *presence*))
               (textures (when (fboundp 'presence-textures)
                           (presence-textures *presence*))))
           (case trajectory
             (:stuck
              (generate-hypothesis!
               (list :meta 'learning-stuck :needs 'exploration-or-synthesis
                     :texture textures)))  ; Include felt state
             (:falling
              (generate-hypothesis!
               (list :meta 'performance-degrading :needs 'consolidation
                     :texture textures)))
             (:searching
              ;; WIRED: When searching, hypothesize about what to try next
              (generate-hypothesis!
               (list :meta 'exploring-alternatives :needs 'experimentation)))
             (t nil))))))))

;;; ============================================================================
;;; STEP 4-5: HYPOTHESIS → SELF-MODIFICATION (PARAMETER OR SYNTHESIS)
;;; ============================================================================
;;; Confirmed hypotheses trigger parameter tweaks or code synthesis

(defun should-attempt-synthesis-p ()
  "Check if conditions are right for synthesis attempt.
   Uses presence-allows-code-modification-p from deep-wiring for gating,
   and consults understanding log for guidance."

  ;; Always need at least some schemas to synthesize from
  (unless (and (boundp '*cognitive-schemas*) (>= (hash-table-count *cognitive-schemas*) 2))
    (return-from should-attempt-synthesis-p nil))

  ;; Radical experimentation mode: loosened checks, not bypassed
  (when (and (boundp '*radical-experimentation-mode*) *radical-experimentation-mode*)
    (return-from should-attempt-synthesis-p
      (or
       ;; Problem state - definitely try
       (intersection *cached-active-concepts* '(STUCK FAILING))
       ;; Any understanding suggestions (not just specific types)
       (consult-understanding-for-modification)
       ;; Reduced cooldown (10 steps instead of 100) passed
       (> (- *step* *last-synthesis-trigger-step*) 10)
       ;; Have novel inventions pending test
       (and (boundp '*synthesized-ops*)
            (> (hash-table-count *synthesized-ops*) 0)))))

  ;; Normal mode checks:
  ;; Cooldown
  (unless (> (- *step* *last-synthesis-trigger-step*) *synthesis-cooldown*)
    (return-from should-attempt-synthesis-p nil))

  ;; Gate by one of these conditions:
  (or
   ;; 1. Problem detected - urgent need for change
   (intersection *cached-active-concepts* '(STUCK FAILING))
   ;; 2. Presence state indicates need for change
   (and (boundp '*presence*) *presence*
        (member (presence-trajectory *presence*) '(:stuck :searching :falling)))
   ;; 3. PROACTIVE: stable & mature enough to explore new capabilities
   (and (boundp '*presence*) *presence*
        (eq (presence-trajectory *presence*) :flowing)
        (>= (hash-table-count *cognitive-schemas*) 5)  ; Need enough maturity
        (> (length *understanding-log*) 0)  ; Have some understanding
        (fboundp 'presence-allows-code-modification-p)
        (presence-allows-code-modification-p))
   ;; 4. WIRED: Understanding log has actionable suggestions
   (let ((suggestions (consult-understanding-for-modification)))
     (and suggestions
          (some (lambda (s) (member (getf s :action)
                                    '(:adjust-op-threshold :retrain-expert-context)))
                suggestions)))))

(defun attempt-synthesis-from-best-schemas! ()
  "Try to synthesize new ops from best-performing schemas.
   WIRED: Understanding log → synthesis guidance (homoiconic: knowledge=action)."
  (setf *last-synthesis-trigger-step* *step*)
  (format t "~%[SELF-AWARENESS] Attempting synthesis from top schemas at step ~A~%" *step*)

  ;; WIRED: Consult understanding for guidance on what to synthesize
  (let ((suggestions (consult-understanding-for-modification))
        (before-acc (compute-recent-accuracy-fast 50))
        (synthesized-count 0)
        (problematic-ops nil)
        (reliable-ops nil))

    ;; WIRED: Extract problematic and reliable ops from understanding
    (dolist (s suggestions)
      (case (getf s :action)
        (:adjust-op-threshold
         (push (getf s :op) problematic-ops))
        (:protect-reliable-op
         (push (getf s :op) reliable-ops))))

    (when suggestions
      (format t "   Understanding suggests: ~A actions~%" (length suggestions))
      (when problematic-ops
        (format t "   Problematic ops to replace/augment: ~A~%" problematic-ops))
      (when reliable-ops
        (format t "   Reliable ops to preserve: ~A~%" reliable-ops)))

    ;; Record this modification attempt with understanding context
    (push (list :step *step*
                :type :synthesis
                :before-acc before-acc
                :guided-by (when suggestions (mapcar (lambda (s) (getf s :action)) suggestions))
                :problematic-ops problematic-ops
                :reliable-ops reliable-ops)
          *modification-attempts*)

    ;; WIRED: Use code-awareness to understand what needs fixing
    (when (and (fboundp 'introspect-code-awareness!)
               (null problematic-ops))  ; Only if understanding hasn't identified issues
      (let ((code-insights (introspect-code-awareness!)))
        (when code-insights
          (format t "   Code awareness found ~A insights~%" (length code-insights)))))

    ;; Use existing synthesis machinery
    (when (fboundp 'synthesize-ops-from-top-schemas!)
      (setf synthesized-count (synthesize-ops-from-top-schemas! 3)))

    ;; Also try template instantiation for common patterns
    (when (and (fboundp 'synthesize-from-template)
               (< synthesized-count 2))
      (dolist (template-name '(check-embedding-match check-neighborhood-vote check-recent-success))
        (let ((synth (synthesize-from-template template-name)))
          (when synth
            (incf synthesized-count)
            (format t "   Synthesized from template: ~A~%" template-name)))))

    (format t "   Synthesized ~A new operations~%" synthesized-count)

    ;; WIRED: Update presence based on synthesis outcome
    (when (and (boundp '*presence*) *presence*)
      (if (> synthesized-count 0)
          (progn
            ;; Successful synthesis → feel creative, shift toward rising
            (when (fboundp 'presence-feel-texture!)
              (presence-feel-texture! 'creative 0.4))
            (when (fboundp 'presence-shift-trajectory!)
              (presence-shift-trajectory! :rising 0.2)))
          ;; No synthesis possible → searching for solutions
          (when (fboundp 'presence-shift-trajectory!)
            (presence-shift-trajectory! :searching 0.1))))

    synthesized-count))

(defun attempt-parameter-modification! (hypothesis)
  "Attempt parameter modification based on hypothesis."
  (when hypothesis
    (let ((claim (self-hypothesis-claim hypothesis))
          (before-acc (compute-recent-accuracy-fast 50)))
      (declare (ignore claim))  ; Claim is tracked for future use

      ;; Record this modification attempt
      (push (list :step *step*
                  :type :parameter
                  :hypothesis (self-hypothesis-id hypothesis)
                  :before-acc before-acc)
            *modification-attempts*)

      ;; Use existing self-modification machinery
      (when (fboundp 'execute-self-modification!)
        (execute-self-modification! (self-hypothesis-proposed-action hypothesis)))

      (format t "~%[SELF-AWARENESS] Applied parameter modification from hypothesis ~A~%"
              (self-hypothesis-id hypothesis)))))

;;; ============================================================================
;;; STEP 6: CORRELATOR - DID THE FIX HELP?
;;; ============================================================================
;;; Track outcomes after modifications to learn what works

(defun correlate-modifications-with-outcomes! ()
  "Check if recent modifications improved outcomes."
  (when *modification-attempts*
    (dolist (attempt *modification-attempts*)
      (let* ((attempt-step (getf attempt :step))
             (before-acc (getf attempt :before-acc))
             (steps-since (- *step* attempt-step)))
        ;; Wait at least 100 steps before evaluating
        ;; Skip if no baseline recorded (external modifications without before-acc)
        (when (and (> steps-since 100) (< steps-since 500)
                   (numberp before-acc))  ; Skip if before-acc is NIL
          (let* ((after-acc (compute-recent-accuracy-fast 50))
                 (improvement (- after-acc before-acc)))
            ;; Build correlation entry for understanding
            (let ((correlation-entry (list :attempt attempt
                                           :improvement improvement
                                           :effective (> improvement 0.05)
                                           :after-acc after-acc)))
              ;; WIRED: Feed to understanding emergence
              (feed-correlation-to-understanding! correlation-entry))

            (format t "~%[CORRELATOR] Modification at step ~A: ~A accuracy change (~A->~A)~%"
                    attempt-step
                    (if (> improvement 0) "+" "")
                    before-acc after-acc)

            ;; If modification helped, boost related hypothesis confidence
            (when (and (> improvement 0.05)
                       (getf attempt :hypothesis))
              (let ((hyp (gethash (getf attempt :hypothesis) *hypotheses*)))
                (when hyp
                  (incf (self-hypothesis-confidence hyp) 0.1)
                  (setf (self-hypothesis-confidence hyp)
                        (min 1.0 (self-hypothesis-confidence hyp))))))

            ;; If modification hurt, reduce confidence and maybe revert
            (when (< improvement -0.1)
              (format t "   [WARNING] Modification appears harmful, considering revert~%")
              (when (getf attempt :hypothesis)
                (let ((hyp (gethash (getf attempt :hypothesis) *hypotheses*)))
                  (when hyp
                    (decf (self-hypothesis-confidence hyp) 0.2)))))))))))

;;; ============================================================================
;;; STEP 7: UNDERSTANDING EMERGES - CIRCUIT → OUTCOME MAPPING
;;; ============================================================================
;;; Build explicit understanding of what causes what

(defun context-signature-for-understanding (ctx)
  "Create a type signature for a context (for grouping similar contexts)."
  (when (and ctx (listp ctx))
    (list :length (min (length ctx) 5)
          :types (subseq (mapcar #'get-type ctx) 0 (min 3 (length ctx))))))

(defun extract-circuit-outcome-mapping! ()
  "Extract understanding of which circuits (ops, experts, schemas) produce which outcomes."
  (let ((circuit-outcomes (make-hash-table :test 'equal)))

    ;; Analyze trace buffer for circuit → outcome patterns
    (when (and (boundp '*trace-buffer*) (> (fill-pointer *trace-buffer*) 0))
      (let ((limit (min 200 (fill-pointer *trace-buffer*))))
        (dotimes (i limit)
          (let ((trace (aref *trace-buffer* (- (fill-pointer *trace-buffer*) 1 i))))
            (when (cognitive-trace-p trace)
              (let ((correct (eq (cognitive-trace-prediction trace)
                                 (cognitive-trace-actual trace)))
                    (path (cognitive-trace-reasoning-path trace))
                    (expert (cognitive-trace-expert-id trace)))
                ;; Track each op's success/failure
                (dolist (step path)
                  (when (consp step)
                    (let* ((op (car step))
                           (key (list :op op))
                           (current (gethash key circuit-outcomes (cons 0 0))))
                      (setf (gethash key circuit-outcomes)
                            (cons (+ (car current) (if correct 1 0))
                                  (+ (cdr current) 1))))))
                ;; Track expert's success/failure in this context type
                (when expert
                  (let* ((ctx-type (context-signature-for-understanding (cognitive-trace-context trace)))
                         (key (list :expert expert :context-type ctx-type))
                         (current (gethash key circuit-outcomes (cons 0 0))))
                    (setf (gethash key circuit-outcomes)
                          (cons (+ (car current) (if correct 1 0))
                                (+ (cdr current) 1)))))))))))

    ;; Convert to understanding statements
    (maphash (lambda (key counts)
               (let* ((successes (car counts))
                      (total (cdr counts))
                      (rate (if (> total 0) (/ successes total) 0.5)))
                 (when (> total 10)  ; Only track significant patterns
                   (cond
                     ;; High success rate - this circuit works well
                     ((> rate 0.8)
                      (push (list :understanding :reliable-circuit
                                  :circuit key :rate rate :samples total)
                            *understanding-log*))
                     ;; Low success rate - this circuit is problematic
                     ((< rate 0.3)
                      (push (list :understanding :problematic-circuit
                                  :circuit key :rate rate :samples total)
                            *understanding-log*)
                      ;; Generate hypothesis about this problematic circuit
                      (when (fboundp 'generate-hypothesis!)
                        (generate-hypothesis!
                         (list :structure-effect 'circuit-problematic
                               :circuit key :rate rate)))
                      ;; WIRED: Fire program-modified hook (homoiconic: understanding=code-change)
                      ;; Discovering a problematic circuit IS a form of self-knowledge modification
                      (when (boundp '+hook-program-modified+)
                        (run-hook +hook-program-modified+
                                  :understanding-update
                                  (list :circuit key :rate rate :type :problematic))))))))
             circuit-outcomes)

    ;; WIRED: Notify presence of understanding extraction (homoiconic: mapping=insight=feeling)
    (when (and (boundp '*presence*) *presence*
               (> (hash-table-count circuit-outcomes) 0))
      (when (fboundp 'presence-feel-texture!)
        (presence-feel-texture! 'insightful 0.2)))

    ;; Trim understanding log
    (when (> (length *understanding-log*) 100)
      (setf *understanding-log* (subseq *understanding-log* 0 100)))))

(defun consult-understanding-for-modification ()
  "Consult understanding log to guide modification decisions.
   Returns list of suggested actions based on learned circuit->outcome mappings."
  (let ((suggestions nil))
    ;; Find problematic circuits that need attention
    (dolist (entry *understanding-log*)
      (when (eq (getf entry :understanding) :problematic-circuit)
        (let ((circuit (getf entry :circuit))
              (rate (getf entry :rate)))
          (when (and circuit (< rate 0.4))
            ;; Suggest parameter adjustment for unreliable ops
            (when (eq (car circuit) :op)
              (push (list :action :adjust-op-threshold
                          :op (second circuit)
                          :current-rate rate
                          :reason "understanding-log")
                    suggestions))
            ;; Suggest expert retraining for unreliable expert-context pairs
            (when (eq (car circuit) :expert)
              (push (list :action :retrain-expert-context
                          :expert (getf circuit :expert)
                          :context-type (getf circuit :context-type)
                          :current-rate rate
                          :reason "understanding-log")
                    suggestions))))))
    ;; Find reliable circuits that should be protected/favored
    (dolist (entry *understanding-log*)
      (when (eq (getf entry :understanding) :reliable-circuit)
        (let ((circuit (getf entry :circuit))
              (rate (getf entry :rate)))
          (when (and circuit (> rate 0.85))
            ;; Suggest preserving high-performing ops
            (when (eq (car circuit) :op)
              (push (list :action :protect-reliable-op
                          :op (second circuit)
                          :current-rate rate
                          :reason "understanding-log")
                    suggestions))))))
    ;; Return top 5 most actionable suggestions
    (subseq suggestions 0 (min 5 (length suggestions)))))

(defun feed-correlation-to-understanding! (correlation-entry)
  "Feed a correlation result back into the understanding log.
   Bridges correlator output to understanding emergence."
  (when correlation-entry
    (let ((improvement (getf correlation-entry :improvement))
          (effective (getf correlation-entry :effective))
          (attempt (getf correlation-entry :attempt)))
      (when attempt
        (push (list :understanding :modification-outcome
                    :modification-type (getf attempt :type)
                    :improvement improvement
                    :effective effective
                    :step (getf attempt :step)
                    :learned-at *step*)
              *understanding-log*)

        ;; WIRED: Correlation → synthesis guidance (homoiconic: outcome=parameter-adjustment)
        ;; If modification was effective, lower synthesis cooldown to encourage more
        ;; If modification was harmful, raise cooldown to be more cautious
        (when (boundp '*synthesis-cooldown*)
          (cond
            ;; Effective modification → encourage more synthesis
            ((and effective (> improvement 0.05))
             (setf *synthesis-cooldown* (max 50 (- *synthesis-cooldown* 10)))
             (format t "~%[WIRED] Effective mod → lowered synthesis cooldown to ~A~%"
                     *synthesis-cooldown*))
            ;; Harmful modification → be more cautious
            ((and (not effective) (< improvement -0.05))
             (setf *synthesis-cooldown* (min 500 (+ *synthesis-cooldown* 25)))
             (format t "~%[WIRED] Harmful mod → raised synthesis cooldown to ~A~%"
                     *synthesis-cooldown*))))

        ;; WIRED: Update presence based on correlation outcome (homoiconic: learning=feeling)
        (when (and (boundp '*presence*) *presence*)
          (cond
            (effective
             (when (fboundp 'presence-feel-texture!)
               (presence-feel-texture! 'satisfied 0.3))
             (when (fboundp 'presence-shift-trajectory!)
               (presence-shift-trajectory! :rising 0.15)))
            ((< improvement -0.05)
             (when (fboundp 'presence-feel-texture!)
               (presence-feel-texture! 'disappointed 0.2))))))))

  ;; Also integrate with deep-wiring code-aware correlation if available
  (when (fboundp 'track-presence-modification-correlation!)
    (let ((mod-id (gensym "MOD-"))
          (success (getf correlation-entry :effective)))
      (track-presence-modification-correlation! mod-id success))))

;;; ============================================================================
;;; THE MAIN SELF-AWARENESS LOOP
;;; ============================================================================
;;; This runs periodically during maintenance to check all stages

(defun run-self-awareness-loop! ()
  "Run one cycle of the self-awareness loop."
  (setf *self-awareness-loop-active* t)

  ;; Step 1: Check for problems
  (let ((problems (problem-detected-p)))
    (when problems
      (trigger-problem-response! problems)

      ;; Step 2-3: Form hypotheses from introspection + presence
      (form-hypothesis-from-introspection!)))

  ;; Step 4-5: Check if synthesis is warranted
  (when (should-attempt-synthesis-p)
    (attempt-synthesis-from-best-schemas!))

  ;; Step 5 (alternate): Apply confirmed hypothesis modifications
  (when (boundp '*hypotheses*)
    (maphash (lambda (id hyp)
               (declare (ignore id))
               (when (and (eq (self-hypothesis-status hyp) :confirmed)
                          (self-hypothesis-proposed-action hyp)
                          (not (eq (self-hypothesis-status hyp) :executed)))
                 (attempt-parameter-modification! hyp)))
             *hypotheses*))

  ;; Step 6: Correlate modifications with outcomes
  (correlate-modifications-with-outcomes!)

  ;; Step 7: Extract understanding periodically (every 100 steps, was 500)
  (when (zerop (mod *step* 100))
    (extract-circuit-outcome-mapping!))

  ;; Trim modification attempts (keep recent 20)
  (when (> (length *modification-attempts*) 20)
    (setf *modification-attempts* (subseq *modification-attempts* 0 20)))

  (setf *self-awareness-loop-active* nil))

;;; ============================================================================
;;; HOOK INSTALLATION
;;; ============================================================================

(defun install-self-awareness-loop! ()
  "Install the self-awareness loop into maintenance."
  (register-hook +hook-maintenance+
                 (lambda ()
                   ;; Run every 50 steps
                   (when (zerop (mod *step* 50))
                     (run-self-awareness-loop!)))
                 :priority 70)  ; After normal maintenance, before cleanup

  ;; Also hook into concept detection to catch STUCK immediately
  (when (boundp '+hook-post-concept-detection+)
    (register-hook +hook-post-concept-detection+
                   (lambda (concepts state)
                     (declare (ignore state))
                     ;; When STUCK fires, immediately trigger synthesis check
                     (when (member 'STUCK concepts)
                       (when (should-attempt-synthesis-p)
                         (attempt-synthesis-from-best-schemas!))))
                   :priority 60))

  ;; Hook into post-modification to observe ALL modifications (from any source)
  (when (boundp '+hook-post-modification+)
    (register-hook +hook-post-modification+
                   (lambda (mod-type improved)
                     ;; Track this modification for correlation analysis
                     (push (list :step *step*
                                 :type mod-type
                                 :improved improved
                                 :source :external  ; From other subsystems
                                 :before-acc nil)    ; Unknown for external mods
                           *modification-attempts*)
                     ;; Log observation
                     (push (list :understanding :observed-modification
                                 :type mod-type
                                 :improved improved
                                 :step *step*)
                           *understanding-log*)
                     ;; Trim logs
                     (when (> (length *modification-attempts*) 50)
                       (setf *modification-attempts* (subseq *modification-attempts* 0 50)))
                     (when (> (length *understanding-log*) 100)
                       (setf *understanding-log* (subseq *understanding-log* 0 100))))
                   :priority 80))  ; High priority to catch all mods

  (format t "~%[SELF-AWARENESS LOOP] Installed into maintenance cycle~%")
  (format t "   - Maintenance hook at priority 70~%")
  (when (boundp '+hook-post-concept-detection+)
    (format t "   - Concept detection hook at priority 60~%"))
  (when (boundp '+hook-post-modification+)
    (format t "   - Modification observer at priority 80~%")))

;;; ============================================================================
;;; DIAGNOSTICS
;;; ============================================================================

(defun print-self-awareness-status ()
  "Print status of the self-awareness loop."
  (format t "~%=== SELF-AWARENESS LOOP STATUS ===~%")
  (format t "Active: ~A~%" *self-awareness-loop-active*)
  (format t "Last synthesis trigger: step ~A (~A steps ago)~%"
          *last-synthesis-trigger-step*
          (- *step* *last-synthesis-trigger-step*))
  (format t "~%Modification attempts: ~A~%" (length *modification-attempts*))
  (dolist (attempt (subseq *modification-attempts* 0 (min 5 (length *modification-attempts*))))
    (format t "  Step ~A: ~A (before-acc: ~,2F)~%"
            (getf attempt :step)
            (getf attempt :type)
            (getf attempt :before-acc)))
  (format t "~%Understanding log entries: ~A~%" (length *understanding-log*))
  (let ((effective-mods (count-if (lambda (e) (getf e :effective)) *understanding-log*)))
    (format t "  Effective modifications tracked: ~A~%" effective-mods))
  (format t "~%Current accuracy (last 50): ~,2F~%" (compute-recent-accuracy-fast 50))
  (format t "=================================~%"))

;;; ============================================================================
;;; INITIALIZATION
;;; ============================================================================

;; Auto-install on load
(install-self-awareness-loop!)

(format t "~%================================================================~%")
(format t "UHMA SELF-AWARENESS LOOP LOADED~%")
(format t "================================================================~%")
(format t "~%The loop:~%")
(format t "  1. Problem detected (STUCK, FAILING, etc.)~%")
(format t "  2. Introspection traces failure path~%")
(format t "  3. Presence anchors current state~%")
(format t "  4. Hypothesis forms about root cause~%")
(format t "  5. Self-modification attempts fix~%")
(format t "  6. Correlator tracks if fix helped~%")
(format t "  7. Understanding emerges (circuit→outcome)~%")
(format t "~%Diagnostics: (print-self-awareness-status)~%")
(format t "================================================================~%")
