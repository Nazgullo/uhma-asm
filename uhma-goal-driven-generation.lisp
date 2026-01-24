;;;; ============================================================================
;;;; UHMA GOAL-DRIVEN GENERATION (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Biases sequence generation based on active cognitive goals and drives.
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 6: SPECIAL VARIABLES ---

(defvar *goal-driven-generation-enabled* t )
(defvar *generation-goal-influence* 0.3 )
(defvar *generation-stats* (list :goal-guided 0 :drive-guided 0 :exploration-guided 0 :default 0))

;;; --- SECTION 8: CORE FUNCTIONS ---

(defun goal-generation-params (goal)
  "Map goal types to generation hyper-parameters."
  (let ((type (if (goal-p goal) (goal-type goal) goal)))
    (case type
      (:exploration (list :temperature 1.2 :top-p 0.95 :method :sample :rep-penalty 1.3))
      (:competence (list :temperature 0.7 :top-p 0.8 :method :greedy :rep-penalty 1.1))
      (:efficiency (list :temperature 0.5 :top-p 0.7 :method :greedy :rep-penalty 1.0))
      (otherwise (list :temperature 1.0 :top-p 0.9 :method :sample :rep-penalty 1.1)))))

(defun drive-modulates-generation ()
  "Compute base parameter offsets from current drive activations."
  (let ((temp-mod 0.0) (top-p-mod 0.0))
    (declare (type single-float temp-mod top-p-mod))
    (when (and (boundp '*intrinsic-drives*) *intrinsic-drives*)
      (dolist (d *intrinsic-drives*)
        (let ((act (drive-current-level d)))
          (case (drive-name d)
            (:curiosity (when (> act 0.7) (incf temp-mod 0.2)))
            (:competence (when (> act 0.7) (decf temp-mod 0.15)))))))
    (list :temp-mod temp-mod :top-p-mod top-p-mod)))

;;; --- SECTION 10: INITIALIZATION ---
(eval-when (:load-toplevel :execute)
  (register-hook +hook-post-reset+ (lambda () (setf *generation-stats* (list :goal-guided 0 :drive-guided 0 :exploration-guided 0 :default 0))) :priority 26)
  (format t "[GOAL-GEN] Biased generation active.~%"))

;;; ============================================================================
;;; GOAL-DIRECTED GENERATION
;;; ============================================================================

(defun generate-for-goal (seed-text &key (length 20) goal)
  "Generate text guided by a specific goal.
   If no goal provided, uses current goal or drive-based defaults."
  (unless (fboundp 'generate)
    (error "Generation functions not loaded"))

  (let* ((active-goal (or goal
                          (when (boundp '*current-goal*) *current-goal*)))
         (params (goal-generation-params active-goal))
         (drive-mods (drive-modulates-generation))
         ;; Apply goal parameters with drive modulation
         (temperature (max 0.1 (min 2.0
                                    (+ (or (getf params :temperature) 1.0)
                                       (* *generation-goal-influence*
                                          (getf drive-mods :temp-mod))))))
         (top-p (max 0.1 (min 1.0
                              (+ (or (getf params :top-p) 0.9)
                                 (* *generation-goal-influence*
                                    (getf drive-mods :top-p-mod))))))
         (method (or (getf params :method) :sample))
         (rep-penalty (or (getf params :rep-penalty) 1.1)))

    ;; Track statistics
    (cond
      (active-goal (incf (getf *generation-stats* :goal-guided)))
      ((getf drive-mods :explore) (incf (getf *generation-stats* :exploration-guided)))
      ((not (zerop (getf drive-mods :temp-mod))) (incf (getf *generation-stats* :drive-guided)))
      (t (incf (getf *generation-stats* :default))))

    ;; Generate
    (generate seed-text
              :length length
              :method method
              :temperature temperature
              :top-p top-p
              :rep-penalty rep-penalty)))

;;; ============================================================================
;;; EXPLORATORY GENERATION
;;; ============================================================================

(defun generate-exploratory (seed-text &key (length 20) (variants 3))
  "Generate multiple exploratory variants, useful for curiosity-driven exploration."
  (unless (fboundp 'generate)
    (error "Generation functions not loaded"))

  (incf (getf *generation-stats* :exploration-guided) variants)

  (let ((results nil))
    (dotimes (i variants)
      (let* ((temp (+ 1.0 (* 0.3 (random 1.0))))  ; Random 1.0-1.3
             (top-p (+ 0.85 (* 0.1 (random 1.0)))) ; Random 0.85-0.95
             (result (generate seed-text
                              :length length
                              :method :sample
                              :temperature temp
                              :top-p top-p
                              :rep-penalty 1.3)))
        (push (list :text result :temp temp :top-p top-p) results)))
    (nreverse results)))

;;; ============================================================================
;;; COMPETENCE-FOCUSED GENERATION
;;; ============================================================================

(defun generate-accurate (seed-text &key (length 20) (beam-width 3))
  "Generate text optimizing for accuracy, useful for competence goals."
  (unless (fboundp 'generate)
    (error "Generation functions not loaded"))

  (incf (getf *generation-stats* :goal-guided))

  (generate seed-text
            :length length
            :method :beam
            :beam-width beam-width
            :temperature 0.6
            :top-p 0.75
            :rep-penalty 1.0))

;;; ============================================================================
;;; GOAL-GUIDED CONTINUATION PREDICTION
;;; ============================================================================

(defun goal-biased-prediction (ctx)
  "Get a prediction biased by current goal.
   Can be used as an alternative to standard expert routing."
  (when (and *goal-driven-generation-enabled*
             (fboundp 'collect-vote-distribution)
             (fboundp 'normalize-votes))
    (let* ((votes (collect-vote-distribution ctx))
           (normalized (normalize-votes votes)))
      (when normalized
        (let* ((goal (when (boundp '*current-goal*) *current-goal*))
               (params (goal-generation-params goal))
               (temp (or (getf params :temperature) 1.0)))
          ;; Apply temperature
          (when (and (fboundp 'apply-temperature) (/= temp 1.0))
            (setf normalized (apply-temperature normalized temp)))
          ;; Return best prediction with confidence
          (let ((best (first (sort (copy-list normalized) #'> :key #'cdr))))
            (when best
              (make-op-result :type :return
                              :value (car best)
                              :confidence (cdr best)
                              :source :goal-generation))))))))

;;; ============================================================================
;;; INTEGRATION WITH GOAL PURSUIT
;;; ============================================================================

(defun pursue-generation-goal! (goal)
  "Actively pursue a goal through generation.
   This can be used to generate hypotheses, explore possibilities, etc."
  (when (and goal (goal-p goal))
    (let ((goal-type (goal-type goal))
          (ctx (when (goal-attempts goal)
                 (getf (first (goal-attempts goal)) :context))))
      (case goal-type
        ;; For exploration goals: generate diverse continuations
        (:exploration
         (when ctx
           (let ((variants (generate-exploratory (reverse ctx) :length 5 :variants 2)))
             ;; Record what we explored
             (push (list :action :generation
                         :variants (mapcar (lambda (v) (getf v :text)) variants)
                         :at-step *step*)
                   (goal-attempts goal)))))
        ;; For competence goals: generate accurate prediction
        (:competence
         (when ctx
           (let ((prediction (generate-accurate (reverse ctx) :length 3)))
             (push (list :action :generation
                         :prediction prediction
                         :at-step *step*)
                   (goal-attempts goal)))))
        ;; For coherence goals: generate and compare
                 (:coherence
                 (when ctx
                   (let ((v1 (generate-for-goal (reverse ctx) :length 5 :goal goal))
                         (v2 (generate (reverse ctx) :length 5 :method :greedy)))
                     (push (list :action :generation                         :goal-guided v1
                         :default v2
                         :at-step *step*)
                   (goal-attempts goal)))))))))

;;; ============================================================================
;;; DIAGNOSTICS
;;; ============================================================================

(defun print-generation-status ()
  "Print status of goal-driven generation system."
  (format t "~%=== Goal-Driven Generation Status ===~%")
  (format t "Enabled: ~A~%" *goal-driven-generation-enabled*)
  (format t "Goal influence: ~,2F~%" *generation-goal-influence*)
  (format t "~%Statistics:~%")
  (format t "  Goal-guided: ~D~%" (getf *generation-stats* :goal-guided))
  (format t "  Drive-guided: ~D~%" (getf *generation-stats* :drive-guided))
  (format t "  Exploration-guided: ~D~%" (getf *generation-stats* :exploration-guided))
  (format t "  Default: ~D~%" (getf *generation-stats* :default))
  (when (boundp '*current-goal*)
    (format t "~%Current goal: ~A~%"
            (when *current-goal* (goal-type *current-goal*)))
    (when *current-goal*
      (format t "  Params: ~A~%" (goal-generation-params *current-goal*))))
  (format t "~%Drive modulation: ~A~%" (drive-modulates-generation)))

;;; ============================================================================
;;; RESET HANDLER
;;; ============================================================================

(defun reset-generation-stats! ()
  "Reset generation statistics."
  (setf *generation-stats* (list :goal-guided 0 :drive-guided 0
                                 :exploration-guided 0 :default 0)))

(register-hook +hook-post-reset+
               'reset-generation-stats!
               :priority 26)

;;; ============================================================================
;;; LOAD CONFIRMATION
;;; ============================================================================

(format t "~%================================================================~%")
(format t "UHMA GOAL-DRIVEN GENERATION LOADED~%")
(format t "================================================================~%")
(format t "Generation now responds to goals and drives.~%")
(format t "Key functions:~%")
(format t "  (generate-for-goal seed :length N :goal G)~%")
(format t "  (generate-exploratory seed :variants N)~%")
(format t "  (generate-accurate seed :beam-width N)~%")
(format t "Call (print-generation-status) for diagnostics.~%")
