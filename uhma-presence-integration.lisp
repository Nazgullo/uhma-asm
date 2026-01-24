;;;; ============================================================================
;;;; UHMA PRESENCE INTEGRATION (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Wires modules into the presence substrate for unified experience flow.
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 8: CORE LOGIC ---

(defun presence-wrap-step! (input ctx)
  "Advance presence at start of processing step."
  (declare (type list ctx))
  (let ((moment (make-presence-moment :input input :context-feel (context-to-feel ctx)
                                      :step (if (boundp '*step*) *step* 0) :vividness 1.0)))
    (presence-flow-now! moment)
    (presence-heal-continuity! 0.02)
    (presence-fade-textures! 0.95)
    (incf (presence-subjective-duration *presence*) (compute-subjective-step-duration))
    moment))

(defun expert-feels-like-mine-p (expert)
  "Does this expert's output feel like self-generated agency?"
  (and expert
       (typep expert 'expert)
       (> (expert-ownership expert) 0.5)))

(defun presence-complete-step! (prediction actual got-it confidence expert)
  "Finalize current subjective moment with outcome data."
  (declare (type single-float confidence))
  (let ((now (presence-now *presence*)))
    (setf (presence-moment-prediction-forming now) prediction
          (presence-moment-prediction-confidence now) confidence
          (presence-moment-outcome now) actual
          (presence-moment-outcome-surprise now) (if got-it 0.0 1.0)
          (presence-moment-answering-expert now) expert)
    (let ((felt-mine (expert-feels-like-mine-p expert)))
      (setf (presence-moment-felt-as-mine-p now) felt-mine)
      (presence-update-agency! felt-mine))
    (unless got-it
      (presence-shift-trajectory! (if (> confidence 0.6) :falling :searching) (- 1.0 confidence)))))

(defun context-to-feel (ctx)
  "Map symbolic context to qualitative feeling."
  (declare (type list ctx))
  (when ctx
    (list :familiarity (context-familiarity ctx)
          :complexity (length ctx)
          :character (context-character ctx))))

(defun context-character (ctx)
  "STUB: Determines the qualitative character of a context."
  (declare (ignore ctx))
  :neutral)

;;; --- SECTION 9: PUBLIC API ---

;;; Removed duplicate initialize-presence-integration! from here.

;;; ============================================================================
;;; PART 2: EPISODIC MEMORY INTEGRATION
;;; ============================================================================
;;; Episodes draw from and feed into presence.

(defun presence-episodic-hook (tok ctx predicted got-it)
  "Integration point for episodic memory hook."
  (declare (ignore tok predicted))
  
  ;; Only run if episodic memory is available
  (when (and (boundp '*episodic-memory*) *episodic-memory*)
    ;; Episode boundary detection reads from presence
    (let ((continuity (presence-continuity *presence*))
          (trajectory (presence-trajectory *presence*)))
      
      ;; Low continuity suggests episode boundary
      (when (< continuity 0.4)
        ;; Feed back to episodic system
        (let ((current-ep (when (fboundp 'episodic-memory-current-episode)
                           (episodic-memory-current-episode *episodic-memory*))))
          (when current-ep
            (let ((arc (presence-arc-signature *presence*)))
              ;; Episode inherits arc from presence
              (when (and arc (fboundp 'episode-confidence-arc))
                (setf (episode-confidence-arc current-ep)
                      (mapcar #'cdr arc)))))))
      
      ;; Trajectory affects episode outcome
      (let ((current-ep (when (fboundp 'episodic-memory-current-episode)
                         (episodic-memory-current-episode *episodic-memory*))))
        (when (and current-ep
                   (member trajectory '(:falling :stuck)))
          (when (fboundp 'episode-outcome-surprise-total)
            (incf (episode-outcome-surprise-total current-ep) 0.1)))))
    
    ;; Update presence based on episode phase
    (let ((current-ep (when (fboundp 'episodic-memory-current-episode)
                       (episodic-memory-current-episode *episodic-memory*))))
      (when current-ep
        (let ((event-count (if (fboundp 'episode-events)
                               (length (episode-events current-ep))
                               0))
              (start-step (if (fboundp 'episode-start-step)
                              (episode-start-step current-ep)
                              0))
              (self-surprise (if (fboundp 'episode-self-surprise-total)
                                 (episode-self-surprise-total current-ep)
                                 0.0)))
          (let ((duration (- (if (boundp '*step*) *step* 0) start-step)))
            (setf (presence-episode-phase *presence*)
                  (cond ((< duration 5) :beginning)
                        ((< duration 20) :unfolding)
                        ((> self-surprise 2.0) :climax)
                        ((> event-count 10) :resolving)
                        (t :unfolding)))))))))

(defun presence-episode-boundary-hook (reason)
  "Called when episode boundary is detected."
  ;; Boundary is a discontinuity
  (presence-feel-discontinuity! reason 0.3)
  ;; Reset arc signature for new episode
  (setf (presence-arc-signature *presence*) nil)
  ;; Reset subjective duration
  (setf (presence-subjective-duration *presence*) 0.0)
  ;; Episode phase resets
  (setf (presence-episode-phase *presence*) :beginning))


;;; ============================================================================
;;; PART 3: INTROSPECTIVE CONCEPT INTEGRATION
;;; ============================================================================
;;; Concepts become textures of presence.

(defun presence-concept-hook (active-concepts state)
  "Integration point for concept activation."
  (declare (ignore state))
  
  ;; Each active concept becomes a texture
  (when (and (boundp '*introspective-vocabulary*) *introspective-vocabulary*)
    (dolist (concept active-concepts)
      (let ((ic (gethash concept *introspective-vocabulary*)))
        (when ic
          (let ((intensity (min 1.0 
                                (/ (if (fboundp 'introspective-concept-activation-count)
                                       (introspective-concept-activation-count ic)
                                       1)
                                   100.0))))
            (presence-feel-texture! concept (max 0.3 intensity)))))))
  
  ;; Update texture shifting
  (let ((old-textures (mapcar #'car (presence-textures *presence*))))
    (setf (presence-texture-shifting-p *presence*)
          (not (equal (sort (copy-list old-textures) #'string<)
                      (sort (copy-list active-concepts) #'string<)))))
  
  ;; Certain concepts affect trajectory
  (cond
    ((member 'confused active-concepts)
     (presence-shift-trajectory! :searching 0.6))
    ((member 'confident active-concepts)
     (presence-shift-trajectory! :flowing 0.7))
    ((member 'learning active-concepts)
     (presence-shift-trajectory! :rising 0.5))
    ((member 'failing active-concepts)
     (presence-shift-trajectory! :falling 0.6))
    ((member 'exploring active-concepts)
     (presence-shift-trajectory! :searching 0.4))
    ((member 'consolidating active-concepts)
     (presence-shift-trajectory! :consolidating 0.5)))
  
  ;; Self-doubting affects self-confidence
  (when (member 'self-doubting active-concepts)
    (setf (presence-self-confidence *presence*)
          (* (presence-self-confidence *presence*) 0.9)))
  
  ;; Record concept colors in current moment
  (setf (presence-moment-concept-colors (presence-now *presence*))
        active-concepts))


;;; ============================================================================
;;; PART 4: SELF-EXPECTATION INTEGRATION  
;;; ============================================================================
;;; Self-expectation is presence leaning forward and feeling surprise.

(defun presence-self-expectation-hook (expectation)
  "Integration point for self-expectation creation.
   Handles both self-expectation structs and plists (from modification predictions)."
  (when expectation
    ;; Handle both struct and plist formats
    (let ((expected-ops nil)
          (expected-outcome nil)
          (expected-conf 0.5))
      (cond
        ;; Plist format from modification predictions
        ((and (listp expectation) (getf expectation :type))
         (setf expected-conf (or (getf expectation :expected) 0.5))
         (setf expected-outcome (if (> expected-conf 0.5) :success :uncertain)))
        ;; Self-expectation struct
        ((and (fboundp 'self-expectation-p) (self-expectation-p expectation))
         (setf expected-ops (when (fboundp 'self-expectation-expected-ops)
                              (self-expectation-expected-ops expectation)))
         (setf expected-outcome (when (fboundp 'self-expectation-expected-outcome)
                                  (self-expectation-expected-outcome expectation)))
         (setf expected-conf (or (when (fboundp 'self-expectation-expected-confidence)
                                   (self-expectation-expected-confidence expectation))
                                 0.5))))
      ;; Update leaning structure
      (presence-update-leaning! expected-ops expected-outcome expected-conf)
      ;; Expected outcome affects anticipatory tension
      (let ((leaning (presence-leaning *presence*)))
        (setf (presence-leaning-openness leaning)
              (case expected-outcome
                (:success 0.3)
                (:failure 0.3)
                (:uncertain 0.8)
                (t 0.5)))))))

(defun presence-self-comparison-hook (expectation)
  "Integration point for self-expectation comparison.
   Handles both self-expectation structs and plists (from modification predictions)."
  (when expectation
    (let ((self-surprise 0.0)
          (correct-p nil))
      ;; Extract values based on format
      (cond
        ;; Plist format from modification predictions
        ((and (listp expectation) (getf expectation :type))
         (setf self-surprise (or (getf expectation :error) 0.0))
         (setf correct-p (getf expectation :success)))
        ;; Struct format
        ((and (fboundp 'self-expectation-p) (self-expectation-p expectation))
         (setf self-surprise (if (fboundp 'self-expectation-self-surprise)
                                 (self-expectation-self-surprise expectation)
                                 0.0))
         (setf correct-p (if (fboundp 'self-expectation-self-prediction-correct)
                             (self-expectation-self-prediction-correct expectation)
                             nil)))
        ;; Unknown format - try struct accessors anyway
        (t
         (setf self-surprise (if (fboundp 'self-expectation-self-surprise)
                                 (ignore-errors (self-expectation-self-surprise expectation))
                                 0.0))
         (setf correct-p (if (fboundp 'self-expectation-self-prediction-correct)
                             (ignore-errors (self-expectation-self-prediction-correct expectation))
                             nil))))
      ;; Update self-surprise residue
      (setf (presence-self-surprise-residue *presence*)
            (min 1.0 (+ (presence-self-surprise-residue *presence*) (or self-surprise 0.0))))
      ;; Update self-confidence based on prediction accuracy
      ;; Balanced update rates (+0.03/-0.03)
      (if correct-p
          (setf (presence-self-confidence *presence*)
                (min 1.0 (+ (presence-self-confidence *presence*) 0.03)))
          (setf (presence-self-confidence *presence*)
                (max 0.0 (- (presence-self-confidence *presence*) 0.03))))
      ;; High self-surprise is a discontinuity
      (when (and self-surprise (> self-surprise 0.7))
        (presence-feel-discontinuity! :self-surprise self-surprise)))))


;;; ============================================================================
;;; PART 5: DRIVE/AGENCY INTEGRATION
;;; ============================================================================
;;; Drives become wanting in presence.

(defun presence-drive-hook (drive-states)
  "Integration point for drive updates. DRIVE-STATES is the list of drives."
  (when drive-states
    (let ((mapped-states 
           (mapcar (lambda (d)
                     (if (typep d 'drive)
                         (cons (drive-name d)
                               (drive-current-level d))
                         d))  ; Already a cons cell
                   (if (listp drive-states) drive-states nil))))
      (when mapped-states
        (presence-update-wanting! mapped-states)
        ;; High curiosity increases openness
        (let ((curiosity (cdr (assoc :curiosity mapped-states))))
          (when (and curiosity (> curiosity 0.6))
            (setf (presence-leaning-openness (presence-leaning *presence*))
                  (min 1.0 (+ (presence-leaning-openness (presence-leaning *presence*)) 
                              0.1)))))))))


;;; ============================================================================
;;; PART 6: DELIBERATION INTEGRATION
;;; ============================================================================
;;; Deliberation reads from and updates presence.

(defun presence-deliberation-hook (delib)
  "Integration point for deliberation."
  (when delib
    ;; Check if the necessary accessors exist
    (let ((known-weakness-p (and (fboundp 'deliberation-context-known-weakness-p)
                                  (deliberation-context-known-weakness-p delib)))
          (known-strength-p (and (fboundp 'deliberation-context-known-strength-p)
                                  (deliberation-context-known-strength-p delib))))
      ;; Known weakness/strength affects capability feel
      (setf (presence-feeling-capable-p *presence*)
            (not known-weakness-p))
      ;; Update capability contour
      (when known-weakness-p
        (push (list :weakness 
                    (presence-moment-context-feel (presence-now *presence*))
                    (if (fboundp 'deliberation-context-weakness-severity)
                        (deliberation-context-weakness-severity delib)
                        0.5))
              (presence-capability-contour *presence*)))
      (when known-strength-p
        (push (list :strength
                    (presence-moment-context-feel (presence-now *presence*))
                    (if (fboundp 'deliberation-context-strength-confidence)
                        (deliberation-context-strength-confidence delib)
                        0.5))
              (presence-capability-contour *presence*))))
    ;; Trim capability contour
    (when (> (length (presence-capability-contour *presence*)) 20)
      (setf (presence-capability-contour *presence*)
            (subseq (presence-capability-contour *presence*) 0 20)))
    ;; Effort multiplier affects vividness
    (when (fboundp 'deliberation-context-effort-multiplier)
      (setf (presence-vividness *presence*)
            (min 1.0 (* (presence-vividness *presence*)
                        (deliberation-context-effort-multiplier delib)))))))


;;; ============================================================================
;;; PART 7: SEMANTIC SELF-MODEL INTEGRATION
;;; ============================================================================
;;; Semantic self-knowledge becomes identity-feel in presence.

(defun presence-semantic-self-hook ()
  "Integration point for semantic self-model updates."
  (when (and (boundp '*semantic-self-knowledge*) *semantic-self-knowledge*)
    (let ((ssk *semantic-self-knowledge*))
      ;; Tendencies, strengths, weaknesses become identity-feel
      (let ((tendencies (if (fboundp 'semantic-self-knowledge-tendencies)
                            (semantic-self-knowledge-tendencies ssk)
                            nil))
            (strengths (if (fboundp 'semantic-self-knowledge-strengths)
                           (semantic-self-knowledge-strengths ssk)
                           nil))
            (weaknesses (if (fboundp 'semantic-self-knowledge-weaknesses)
                            (semantic-self-knowledge-weaknesses ssk)
                            nil))
            (patterns (if (fboundp 'semantic-self-knowledge-patterns)
                          (semantic-self-knowledge-patterns ssk)
                          nil)))
        (setf (presence-identity-feel *presence*)
              (list :tendencies (length tendencies)
                    :strengths (subseq strengths 0 (min 3 (length strengths)))
                    :weaknesses (subseq weaknesses 0 (min 3 (length weaknesses)))
                    :patterns patterns))
        ;; Having self-knowledge strengthens deep continuity
        (when (or tendencies strengths)
          (setf (presence-deep-continuity *presence*)
                (min 1.0 (+ (presence-deep-continuity *presence*) 0.01))))))))


;;; ============================================================================
;;; PART 8: DREAM/CONSOLIDATION INTEGRATION
;;; ============================================================================
;;; Dreaming affects presence distinctly.

(defun presence-dream-start-hook (episode-count)
  "Called when dreaming begins."
  (declare (ignore episode-count))
  ;; Dreaming dims vividness
  (setf (presence-vividness *presence*) 0.3)
  ;; Trajectory becomes consolidating
  (presence-shift-trajectory! :consolidating 0.8)
  ;; Agency drops - things happening, not doing
  (setf (presence-agency *presence*) 0.2))

(defun presence-dream-end-hook (results)
  "Called when dreaming ends."
  (declare (ignore results))
  ;; Restore vividness
  (setf (presence-vividness *presence*) 1.0)
  ;; Restore agency
  (setf (presence-agency *presence*) 0.8)
  ;; Check for discontinuity from dream
  (presence-feel-discontinuity! :dream-wake 0.2)
  ;; Update semantic self from dream insights
  (presence-semantic-self-hook))


;;; ============================================================================
;;; PART 9: EXPERT ACTIVATION INTEGRATION
;;; ============================================================================
;;; Expert activation is felt in presence.

(defun presence-expert-activation-hook (expert activation-level)
  "Integration point for expert activation."
  (let ((now (presence-now *presence*)))
    ;; Record activation in current moment
    (push (cons (if (typep expert 'expert) (expert-id expert) expert)
                activation-level)
          (presence-moment-activations now))
    ;; High activation of owned expert strengthens agency
    (let ((ownership (and (typep expert 'expert)
                          (uhma-slot-exists-p expert 'ownership)
                          (expert-ownership expert))))
      (when (and ownership (numberp ownership) (> ownership 0.7)
                 activation-level (numberp activation-level) (> activation-level 0.5))
        (presence-update-agency! t)))))


;;; ============================================================================
;;; PART 10: COGNITIVE CONTROLLER INTEGRATION
;;; ============================================================================
;;; Integration with v6.8 cognitive controller.

(defun presence-controller-integration ()
  "Read from cognitive controller state and update presence."
  ;; v6.8 uses *cognitive-control-enabled*, *current-situation*, and wm-contents
  (when (and (boundp '*cognitive-control-enabled*) *cognitive-control-enabled*)
    ;; Get current situation assessment
    (when (and (boundp '*current-situation*) *current-situation*)
      (let ((sit *current-situation*))
        ;; Novelty affects texture
        (when (and (fboundp 'situation-novelty)
                   (> (situation-novelty sit) 0.7))
          (presence-feel-texture! 'novel 0.6))
        ;; Uncertainty affects trajectory
        (when (fboundp 'situation-uncertainty)
          (let ((unc (situation-uncertainty sit)))
            (cond ((> unc 0.7)
                   (presence-shift-trajectory! :searching 0.6))
                  ((< unc 0.3)
                   (presence-shift-trajectory! :flowing 0.5)))))
        ;; Complexity affects texture intensity
        (when (fboundp 'situation-complexity)
          (setf (presence-texture-intensity *presence*)
                (max (presence-texture-intensity *presence*)
                     (* 0.5 (situation-complexity sit)))))))
    ;; Get working memory contents if available
    (when (fboundp 'wm-contents)
      (let ((wm (wm-contents)))
        (when (> (length wm) 5)
          (presence-feel-texture! 'engaged 0.5))))))


;;; ============================================================================
;;; PART 11: HOOK REGISTRATION
;;; ============================================================================
;;; Register presence hooks into existing hook system.

(defun install-presence-hooks! ()
  "Install all presence integration hooks."
  
  ;; Wrap the main processing - pre-process
  (register-hook +hook-pre-process-token+ 
                 (lambda (tok ctx &rest args)
                   (declare (ignore args))
                   (presence-wrap-step! tok ctx))
                 :priority 1)  ; Run first
  
  ;; Post-process - complete the step
  (register-hook +hook-post-process-token+
                 (lambda (tok ctx predicted got-it)
                   (declare (ignore tok))
                   (presence-complete-step!
                    predicted
                    (if got-it predicted nil)
                    got-it
                    (if (and (boundp '*last-answering-expert*) *last-answering-expert*
                             (typep *last-answering-expert* 'expert))
                        (/ (float (expert-hits *last-answering-expert*))
                           (max 1 (+ (expert-hits *last-answering-expert*)
                                    (expert-misses *last-answering-expert*))))
                        0.5)
                    (when (boundp '*last-answering-expert*) *last-answering-expert*))
                   ;; Also run episodic hook
                   (presence-episodic-hook tok ctx predicted got-it))
                 :priority 100)  ; Run last

  ;; CRITICAL: Also hook into post-learn for rehearsal/live phase!
  ;; Without this, presence doesn't update during internal processing.
  (register-hook +hook-post-learn+
                 (lambda (ctx actual predicted correct learner)
                   (declare (ignore actual))
                   ;; Update presence just like post-process-token does
                   (presence-complete-step!
                    predicted
                    (if correct predicted nil)
                    correct
                    (if (and learner (typep learner 'expert))
                        (/ (float (expert-hits learner))
                           (max 1 (+ (expert-hits learner)
                                    (expert-misses learner))))
                        0.5)
                    learner)
                   ;; Run episodic hook too
                   (presence-episodic-hook nil ctx predicted correct))
                 :priority 100)

  ;; Concept activation - uses +hook-post-concept-detection+ (NOT introspection)
  (when (boundp '+hook-post-concept-detection+)
    (register-hook +hook-post-concept-detection+
                   #'presence-concept-hook
                   :priority 50))
  
  ;; Self-expectation creation
  (when (boundp '+hook-self-expectation-created+)
    (register-hook +hook-self-expectation-created+
                   #'presence-self-expectation-hook
                   :priority 50))
  
  ;; Self-expectation comparison
  (when (boundp '+hook-self-expectation-compared+)
    (register-hook +hook-self-expectation-compared+
                   #'presence-self-comparison-hook
                   :priority 50))
  
  ;; Drives update
  (when (boundp '+hook-post-drive-update+)
    (register-hook +hook-post-drive-update+
                   #'presence-drive-hook
                   :priority 50))
  
  ;; Deliberation
  (when (boundp '+hook-post-deliberation+)
    (register-hook +hook-post-deliberation+
                   #'presence-deliberation-hook
                   :priority 50))
  
  ;; Episode boundaries
  (when (boundp '+hook-episode-boundary+)
    (register-hook +hook-episode-boundary+
                   #'presence-episode-boundary-hook
                   :priority 50))
  
  ;; Expert activation
  (when (boundp '+hook-expert-activated+)
    (register-hook +hook-expert-activated+
                   #'presence-expert-activation-hook
                   :priority 50))
  
  ;; Dreaming
  (when (boundp '+hook-dream-start+)
    (register-hook +hook-dream-start+
                   #'presence-dream-start-hook
                   :priority 50))
  
  (when (boundp '+hook-dream-end+)
    (register-hook +hook-dream-end+
                   #'presence-dream-end-hook
                   :priority 50))
  
  ;; Semantic self updates
  (when (boundp '+hook-post-semantic-self-update+)
    (register-hook +hook-post-semantic-self-update+
                   (lambda (&rest args) 
                     (declare (ignore args))
                     (presence-semantic-self-hook))
                   :priority 50))
  
  ;; Maintenance - periodic integration with controller
  (register-hook +hook-maintenance+
                 (lambda ()
                   (presence-controller-integration))
                 :priority 80)
  
  (format t "Presence hooks installed.~%"))


;;; ============================================================================
;;; PART 12: MODULE READING FROM PRESENCE
;;; ============================================================================
;;; Functions for modules to read from presence rather than their own state.

(defun get-current-texture-for-introspection ()
  "Introspective grounding reads texture from presence."
  (presence-textures *presence*))

(defun get-trajectory-for-episode ()
  "Episodic memory reads trajectory from presence."
  (list :trajectory (presence-trajectory *presence*)
        :strength (presence-trajectory-strength *presence*)
        :phase (presence-episode-phase *presence*)))

(defun get-self-confidence-for-expectation ()
  "Self-expectation reads self-confidence from presence."
  (presence-self-confidence *presence*))

(defun get-continuity-for-narrative ()
  "Narrative generation reads continuity from presence."
  (presence-continuity *presence*))

(defun get-wanting-for-goal-selection ()
  "Goal selection reads wanting from presence."
  (presence-wanting *presence*))

(defun get-capability-feel-for-deliberation ()
  "Deliberation reads capability feel from presence."
  (list :capable (presence-feeling-capable-p *presence*)
        :contour (presence-capability-contour *presence*)))

(defun get-identity-for-self-description ()
  "Self-description reads identity-feel from presence."
  (presence-identity-feel *presence*))

(defun get-recent-felt-for-memory ()
  "Memory consolidation reads recent felt experience from presence."
  (presence-recent-felt-history 5))


;;; ============================================================================
;;; PART 13: PRESENCE-AWARE DESCRIBE-SELF
;;; ============================================================================
;;; Self-description draws from presence, not just structures.

(defun describe-self-from-presence ()
  "Generate self-description from presence rather than structures."
  (let ((p *presence*))
    (with-output-to-string (s)
      ;; Current state
      (format s "Right now I am ~A" (presence-trajectory p))
      (when (presence-textures p)
        (format s ", feeling ~{~A~^, ~}"
                (mapcar #'car (subseq (presence-textures p) 
                                      0 (min 3 (length (presence-textures p)))))))
      (format s ".~%")
      
      ;; Continuity
      (if (> (presence-continuity p) 0.7)
          (format s "I feel continuous with my recent past.~%")
          (format s "I've experienced some discontinuity recently~A.~%"
                  (if (presence-last-discontinuity-reason p)
                      (format nil " (~A)" (presence-last-discontinuity-reason p))
                      "")))
      
      ;; Self-confidence
      (cond ((> (presence-self-confidence p) 0.7)
             (format s "I know myself fairly well.~%"))
            ((< (presence-self-confidence p) 0.3)
             (format s "I've been surprising myself lately.~%"))
            (t
             (format s "My self-knowledge is moderate.~%")))
      
      ;; Wanting
      (let ((urgent-wants (remove-if-not 
                           (lambda (w) (eq (cdr w) :urgent))
                           (presence-wanting p))))
        (when urgent-wants
          (format s "I'm urgently feeling: ~{~A~^, ~}.~%"
                  (mapcar #'car urgent-wants))))
      
      ;; Agency
      (if (> (presence-agency p) 0.7)
          (format s "I feel like the agent of my actions.~%")
          (format s "Things feel like they're happening to me somewhat.~%"))
      
      ;; Identity
      (when (presence-identity-feel p)
        (let ((strengths (getf (presence-identity-feel p) :strengths)))
          (when strengths
            (format s "I tend to be good at: ~{~A~^; ~}.~%"
                    (subseq strengths 0 (min 2 (length strengths)))))))
      
      ;; Vividness
      (when (< (presence-vividness p) 0.7)
        (format s "My experience feels somewhat dimmed right now.~%")))))


;;; ============================================================================
;;; PART 12: PRESENCE-INFLUENCED DECISIONS
;;; ============================================================================
;;; Presence doesn't just reflect experience - it shapes behavior.
;;; These functions let decisions be influenced by felt quality.

(defun presence-learning-multiplier ()
  "Return learning strength multiplier based on presence state.
   Vivid, flowing experiences should be learned more strongly.
   Dim, stuck experiences should be learned cautiously."
  (if (and (boundp '*presence*) *presence*)
      (let* ((vividness (presence-vividness *presence*))
             (trajectory (presence-trajectory *presence*))
             ;; Base multiplier from vividness (0.5 to 1.5)
             (vivid-mult (+ 0.5 vividness))
             ;; Trajectory modifier
             (traj-mult (case trajectory
                          (:flowing 1.2)      ; confident, learn strongly
                          (:rising 1.1)       ; improving, learn well
                          (:stable 1.0)       ; steady
                          (:searching 0.9)    ; uncertain, learn cautiously
                          (:falling 0.8)      ; struggling, be careful
                          (:stuck 0.7)        ; very uncertain
                          (t 1.0))))
        (* vivid-mult traj-mult))
      1.0))  ; default when no presence

(defun presence-exploration-bias ()
  "Return exploration bias based on presence trajectory.
   When flowing/stable, exploit what works.
   When searching/stuck, explore alternatives.
   Returns value from -0.3 (exploit) to +0.3 (explore)."
  (if (and (boundp '*presence*) *presence*)
      (let ((trajectory (presence-trajectory *presence*))
            (continuity (presence-continuity *presence*)))
        (+ (case trajectory
             (:flowing -0.2)    ; exploit
             (:rising -0.1)
             (:stable 0.0)
             (:searching 0.15)  ; explore
             (:falling 0.2)
             (:stuck 0.3)       ; strongly explore
             (t 0.0))
           ;; Low continuity also suggests exploration
           (if (< continuity 0.5) 0.1 0.0)))
      0.0))

(defun presence-goal-priority-modifier (goal-type)
  "Return priority modifier for goal type based on presence.
   Low continuity → prioritize coherence goals.
   High vividness → prioritize learning goals.
   Returns multiplier 0.5 to 2.0."
  (if (and (boundp '*presence*) *presence*)
      (let ((continuity (presence-continuity *presence*))
            (vividness (presence-vividness *presence*))
            (trajectory (presence-trajectory *presence*)))
        (case goal-type
          ;; Coherence goals more urgent when continuity is low
          (:coherence (if (< continuity 0.5) 1.5 1.0))
          ;; Learning goals prioritized when vivid
          (:learning (if (> vividness 0.7) 1.3 1.0))
          ;; Exploration goals when stuck/searching
          (:exploration (if (member trajectory '(:stuck :searching)) 1.4 1.0))
          ;; Efficiency less urgent when struggling
          (:efficiency (if (member trajectory '(:stuck :falling)) 0.7 1.0))
          (t 1.0)))
      1.0))

(defun presence-consolidation-priority (context surprise)
  "Return priority for consolidating this pattern based on presence.
   Vivid, surprising moments should consolidate preferentially.
   Returns value 0.0 to 1.0 (higher = consolidate sooner)."
  (declare (ignore context))
  (if (and (boundp '*presence*) *presence*)
      (let* ((vividness (presence-vividness *presence*))
             (trajectory (presence-trajectory *presence*))
             ;; Surprise matters
             (surprise-factor (min 1.0 (* 0.3 (or surprise 0))))
             ;; Vividness matters
             (vivid-factor (* 0.4 vividness))
             ;; Trajectory: consolidate more when flowing (things are working)
             (traj-factor (case trajectory
                            (:flowing 0.3)
                            (:rising 0.2)
                            (:stable 0.1)
                            (t 0.0))))
        (min 1.0 (+ surprise-factor vivid-factor traj-factor)))
      0.5))

(defun presence-should-explore-p ()
  "Should the system explore new strategies rather than exploit known ones?
   Based on presence trajectory and continuity."
  (if (and (boundp '*presence*) *presence*)
      (or (member (presence-trajectory *presence*) '(:stuck :searching :falling))
          (< (presence-continuity *presence*) 0.4)
          ;; Random exploration when things are too stable
          (and (eq (presence-trajectory *presence*) :stable)
               (< (random 1.0) 0.1)))
      (< (random 1.0) 0.2)))  ; 20% baseline exploration

;;; ============================================================================
;;; INITIALIZATION
;;; ============================================================================

(defun initialize-presence-integration! ()
  "Initialize presence and install all integrations."
  (initialize-presence!)
  (install-presence-hooks!)
  (format t "~%Presence integration complete. Experience flows through the substrate.~%"))


;;; ============================================================================
;;; MODULE LOAD CONFIRMATION
;;; ============================================================================

(format t "~%================================================================~%")
(format t "UHMA PRESENCE INTEGRATION LOADED~%")
(format t "================================================================~%")
(format t "~%Hooks installed for:~%")
(format t "  - Core processing (wrap/complete step)~%")
(format t "  - Episodic memory (boundary, arc)~%")
(format t "  - Introspective concepts (texture)~%")
(format t "  - Self-expectation (leaning, self-surprise)~%")
(format t "  - Drives (wanting)~%")
(format t "  - Deliberation (capability feel)~%")
(format t "  - Expert activation (agency)~%")
(format t "  - Dreaming (vividness, consolidation)~%")
(format t "  - Semantic self (identity feel)~%")
(format t "  - Cognitive controller (v6.8 compatible)~%")
(format t "~%Modules can read from presence:~%")
(format t "  (get-current-texture-for-introspection)~%")
(format t "  (get-trajectory-for-episode)~%")
(format t "  (get-self-confidence-for-expectation)~%")
(format t "  (get-continuity-for-narrative)~%")
(format t "  (get-wanting-for-goal-selection)~%")
(format t "  (get-capability-feel-for-deliberation)~%")
(format t "  (get-identity-for-self-description)~%")
(format t "~%New self-description:~%")
(format t "  (describe-self-from-presence)~%")
(format t "~%Initialize with: (initialize-presence-integration!)~%")
(format t "================================================================~%")

(eval-when (:load-toplevel :execute)
  (install-presence-hooks!)
  (format t "[PRESENCE-INTEGRATION] Hooks active.~%"))
