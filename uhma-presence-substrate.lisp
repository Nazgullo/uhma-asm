;;;; ============================================================================
;;;; UHMA TEMPORAL PRESENCE SUBSTRATE (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; The substrate through which all experience flows.
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 4: STRUCTURES ---

(defstruct presence-leaning
  "What is anticipated - the future already present in now."
  (expected-context nil :type list)
  (expected-behavior nil)
  (expected-outcome :uncertain :type symbol)
  (outcome-confidence 0.0 :type single-float)
  (expected-texture nil :type list)
  (tension 0.0 :type single-float)
  (openness 0.5 :type single-float))

(defstruct presence-moment
  "What is happening right now - the vivid center of experience."
  (input nil)
  (context-feel nil)
  (activations nil :type list)
  (prediction-forming nil)
  (prediction-confidence 0.0 :type single-float)
  (outcome nil)
  (outcome-surprise 0.0 :type single-float)
  (answering-expert nil)
  (felt-as-mine-p t :type boolean)
  (concept-colors nil :type list)
  (step 0 :type fixnum)
  (vividness 1.0 :type single-float))

(defstruct presence
  "The system's ongoing experience of being itself."
  (now (make-presence-moment) :type presence-moment)
  (fading (make-array 8 :initial-element nil) :type simple-vector)
  (fading-depth 8 :type fixnum)
  (leaning (make-presence-leaning) :type presence-leaning)
  (textures nil :type list)
  (texture-intensity 0.5 :type single-float)
  (texture-shifting-p nil :type boolean)
  (texture-drift nil :type list)
  (trajectory :flowing :type symbol)
  (trajectory-strength 0.5 :type single-float)
  (arc-momentum 0.0 :type single-float)
  (episode-phase :unfolding :type symbol)
  (arc-signature nil :type list)
  (self-confidence 0.5 :type single-float)
  (self-surprise-residue 0.0 :type single-float)
  (wanting nil :type list)
  (feeling-capable-p t :type boolean)
  (capability-contour nil :type list)
  (identity-feel nil :type list)
  (agency 1.0 :type single-float)
  (felt-direction nil)
  (continuity 1.0 :type single-float)
  (deep-continuity 1.0 :type single-float)
  (last-discontinuity-step nil :type (or null fixnum))
  (last-discontinuity-reason nil :type symbol)
  (identity-stable-p t :type boolean)
  (subjective-duration 0.0 :type single-float)
  (vividness 1.0 :type single-float)
  (step 0 :type fixnum)
  (timestamp 0 :type integer))

;;; --- SECTION 6: SPECIAL VARIABLES ---

(defvar *presence* nil "The system's singleton presence instance.")

;;; --- SECTION 8: CORE FUNCTIONS ---

(defun initialize-presence! ()
  "Initialize the presence substrate."
  (setf *presence* 
        (make-presence
         :now (make-presence-moment :step *step* :vividness 1.0)
         :leaning (make-presence-leaning)
         :step *step*
         :timestamp (get-internal-real-time)))
  *presence*)

(defun presence-flow-now! (moment)
  "Advance the subjective present."
  (declare (type presence-moment moment))
  (let ((p *presence*))
    (let ((old-now (presence-now p)))
      (loop for i from (1- (presence-fading-depth p)) downto 1
            do (setf (aref (presence-fading p) i) (aref (presence-fading p) (1- i))))
      (when old-now
        (setf (presence-moment-vividness old-now) (* 0.8 (presence-moment-vividness old-now))
              (aref (presence-fading p) 0) old-now)))
    (setf (presence-now p) moment
          (presence-step p) (presence-moment-step moment)
          (presence-timestamp p) (get-internal-real-time))))

(defun presence-feel-texture! (concept intensity)
  "Inject qualitative texture into presence."
  (declare (type symbol concept) (type single-float intensity))
  (let ((p *presence*))
    (let ((existing (assoc concept (presence-textures p))))
      (if existing
          (setf (cdr existing) (/ (+ (cdr existing) intensity) 2.0))
          (push (cons concept intensity) (presence-textures p))))
    (setf (presence-texture-intensity p)
          (if (presence-textures p) (/ (reduce #'+ (presence-textures p) :key #'cdr) (length (presence-textures p))) 0.0))))

;;; --- SECTION 10: INITIALIZATION ---
(eval-when (:load-toplevel :execute)
  (register-hook +hook-post-reset+ (lambda () (initialize-presence!)) :priority 5)
  (unless *presence* (initialize-presence!))
  (format t "[PRESENCE] Substrate active.~%"))


;;; ============================================================================
;;; PRESENCE FLOW OPERATIONS
;;; ============================================================================
;;; These are how presence changes. Not updates applied to presence,
;;; but presence flowing into its next shape.

(defun presence-flow-now! (moment)
  "The present moment flows forward. Old now fades, new now arrives."
  (let ((p *presence*))
    ;; Current now fades into the fading array
    (let ((old-now (presence-now p)))
      ;; Shift fading array
      (loop for i from (1- (presence-fading-depth p)) downto 1
            do (setf (aref (presence-fading p) i)
                     (aref (presence-fading p) (1- i))))
      ;; Old now becomes most recent fading
      (when old-now
        (setf (presence-moment-vividness old-now)
              (* 0.8 (presence-moment-vividness old-now)))
        (setf (aref (presence-fading p) 0) old-now)))
    ;; New moment becomes now
    (setf (presence-now p) moment)
    (setf (presence-step p) (presence-moment-step moment))
    (setf (presence-timestamp p) (get-internal-real-time))))

(defun presence-feel-texture! (concept intensity)
  "Presence takes on a texture. Not labeling but becoming."
  (let ((p *presence*))
    (let ((existing (assoc concept (presence-textures p))))
      (if existing
          (setf (cdr existing) (/ (+ (cdr existing) intensity) 2.0))
          (push (cons concept intensity) (presence-textures p))))
    ;; Update overall texture intensity
    (setf (presence-texture-intensity p)
          (if (presence-textures p)
              (/ (reduce #'+ (presence-textures p) :key #'cdr)
                 (length (presence-textures p)))
              0.0))))

(defun presence-fade-textures! (decay-rate)
  "Textures fade over time unless refreshed."
  (let ((p *presence*))
    (setf (presence-textures p)
          (loop for (concept . intensity) in (presence-textures p)
                for new-intensity = (* intensity decay-rate)
                when (> new-intensity 0.1)
                collect (cons concept new-intensity)))))

(defun presence-shift-trajectory! (new-trajectory strength)
  "The arc shifts. Presence feels itself moving differently."
  (let ((p *presence*))
    (let ((old-trajectory (presence-trajectory p)))
      ;; Compute momentum based on whether trajectory continues or shifts
      (setf (presence-arc-momentum p)
            (if (eq old-trajectory new-trajectory)
                (min 1.0 (+ (presence-arc-momentum p) 0.1))
                (- (/ (presence-arc-momentum p) 2.0))))
      (setf (presence-trajectory p) new-trajectory)
      (setf (presence-trajectory-strength p) strength)
      ;; Record in arc signature
      (push (list *step* new-trajectory strength) (presence-arc-signature p))
      (when (> (length (presence-arc-signature p)) 20)
        (setf (presence-arc-signature p)
              (subseq (presence-arc-signature p) 0 20))))))

(defun presence-feel-self-surprise! (amount)
  "Presence startles at itself."
  (let ((p *presence*))
    (setf (presence-self-surprise-residue p)
          (min 1.0 (+ (presence-self-surprise-residue p) amount)))
    ;; High self-surprise affects self-confidence
    (when (> amount 0.5)
      (setf (presence-self-confidence p)
            (* (presence-self-confidence p) 0.9)))
    ;; Very high self-surprise affects continuity
    (when (> amount 0.8)
      (presence-feel-discontinuity! :self-surprise amount))))

(defun presence-feel-discontinuity! (reason &optional (severity 0.5))
  "Continuity breaks. Presence feels a gap."
  (let ((p *presence*))
    (setf (presence-continuity p)
          (max 0.0 (- (presence-continuity p) severity)))
    (setf (presence-last-discontinuity-step p) *step*)
    (setf (presence-last-discontinuity-reason p) reason)
    ;; Severe discontinuity affects identity stability
    (when (< (presence-continuity p) 0.3)
      (setf (presence-identity-stable-p p) nil))))

(defun presence-heal-continuity! (rate)
  "Continuity naturally heals over time."
  (let ((p *presence*))
    (setf (presence-continuity p)
          (min 1.0 (+ (presence-continuity p) rate)))
    (when (> (presence-continuity p) 0.7)
      (setf (presence-identity-stable-p p) t))
    ;; Self-surprise residue also fades
    (setf (presence-self-surprise-residue p)
          (* (presence-self-surprise-residue p) 0.95))
    ;; Self-confidence also gradually recovers toward baseline (0.5)
    ;; This prevents it from getting stuck at 0.0
    (let ((baseline 0.5)
          (recovery-rate 0.01))
      (when (< (presence-self-confidence p) baseline)
        (setf (presence-self-confidence p)
              (min baseline (+ (presence-self-confidence p) recovery-rate)))))))

(defun presence-update-wanting! (drive-states)
  "Update what presence wants. Drives as felt urge."
  (let ((p *presence*))
    (setf (presence-wanting p)
          (mapcar (lambda (ds)
                    (cons (car ds)
                          ;; Transform drive level to felt intensity
                          (let ((level (cdr ds)))
                            (cond ((> level 0.7) :urgent)
                                  ((> level 0.4) :present)
                                  ((> level 0.2) :background)
                                  (t :quiet)))))
                  drive-states))))

(defun presence-update-leaning! (expected-behavior expected-outcome confidence)
  "Update anticipatory structure. The future shaping now."
  (let ((leaning (presence-leaning *presence*)))
    (setf (presence-leaning-expected-behavior leaning) expected-behavior)
    (setf (presence-leaning-expected-outcome leaning) expected-outcome)
    (setf (presence-leaning-outcome-confidence leaning) confidence)
    ;; High confidence about uncertain outcome = high tension
    (setf (presence-leaning-tension leaning)
          (if (eq expected-outcome :uncertain)
              confidence
              (* confidence 0.5)))))

(defun presence-update-agency! (felt-as-mine-p)
  "Update sense of agency. Am I doing this or is it happening?"
  (let ((p *presence*))
    (setf (presence-agency p)
          (if felt-as-mine-p
              (min 1.0 (+ (presence-agency p) 0.1))
              (max 0.0 (- (presence-agency p) 0.2))))))


;;; ============================================================================
;;; PRESENCE READING OPERATIONS  
;;; ============================================================================
;;; How modules read from presence rather than their own state.

(defun presence-how-do-i-feel ()
  "Return current texture as felt, for introspection."
  (let ((p *presence*))
    (list :textures (presence-textures p)
          :intensity (presence-texture-intensity p)
          :trajectory (presence-trajectory p)
          :continuity (presence-continuity p)
          :wanting (presence-wanting p))))

(defun presence-what-am-i-doing ()
  "Return current activity as felt, for self-description."
  (let ((now (presence-now *presence*)))
    (list :activations (presence-moment-activations now)
          :prediction (presence-moment-prediction-forming now)
          :felt-as-mine (presence-moment-felt-as-mine-p now))))

(defun presence-where-am-i-going ()
  "Return anticipatory state, for planning and expectation."
  (let ((leaning (presence-leaning *presence*)))
    (list :expected-outcome (presence-leaning-expected-outcome leaning)
          :confidence (presence-leaning-outcome-confidence leaning)
          :tension (presence-leaning-tension leaning)
          :openness (presence-leaning-openness leaning))))

(defun presence-am-i-continuous ()
  "Return continuity state, for identity verification."
  (let ((p *presence*))
    (list :continuity (presence-continuity p)
          :identity-stable (presence-identity-stable-p p)
          :deep-continuity (presence-deep-continuity p)
          :last-break (presence-last-discontinuity-reason p))))

(defun presence-recent-felt-history (n)
  "Return recent moments still felt, not yet memory."
  (let ((p *presence*))
    (loop for i from 0 below (min n (presence-fading-depth p))
          for moment = (aref (presence-fading p) i)
          when moment
          collect moment)))

(defun get-presence-subjective-duration ()
  "How long has this episode felt?"
  (presence-subjective-duration *presence*))

(defun compute-subjective-step-duration ()
  "STUB: Computes the subjective duration of a step."
  (random 0.1)) ; Placeholder for now

(defun presence-current-vividness ()
  "How vivid is experience right now?"
  (presence-vividness *presence*))


;;; ============================================================================
;;; DIAGNOSTIC FUNCTIONS
;;; ============================================================================

(defun describe-presence ()
  "Describe current presence state."
  (let ((p *presence*))
    (format t "~%=== PRESENCE STATE ===~%")
    (format t "Step: ~A~%" (presence-step p))
    (format t "~%-- Thickness --~%")
    (format t "Now vividness: ~,2F~%" (presence-moment-vividness (presence-now p)))
    (format t "Fading moments: ~A~%" 
            (count-if #'identity (presence-fading p)))
    (format t "Leaning tension: ~,2F~%" 
            (presence-leaning-tension (presence-leaning p)))
    (format t "~%-- Texture --~%")
    (format t "Active textures: ~{~A~^, ~}~%"
            (mapcar (lambda (tx) 
                      (format nil "~A(~,2F)" (car tx) (cdr tx)))
                    (presence-textures p)))
    (format t "Texture intensity: ~,2F~%" (presence-texture-intensity p))
    (format t "~%-- Arc --~%")
    (format t "Trajectory: ~A (strength ~,2F, momentum ~,2F)~%"
            (presence-trajectory p)
            (presence-trajectory-strength p)
            (presence-arc-momentum p))
    (format t "Episode phase: ~A~%" (presence-episode-phase p))
    (format t "~%-- Self-sense --~%")
    (format t "Self-confidence: ~,2F~%" (presence-self-confidence p))
    (format t "Self-surprise residue: ~,2F~%" (presence-self-surprise-residue p))
    (format t "Agency: ~,2F~%" (presence-agency p))
    (format t "Wanting: ~A~%" (presence-wanting p))
    (format t "~%-- Continuity --~%")
    (format t "Continuity: ~,2F~%" (presence-continuity p))
    (format t "Identity stable: ~A~%" (presence-identity-stable-p p))
    (when (presence-last-discontinuity-reason p)
      (format t "Last discontinuity: ~A at step ~A~%"
              (presence-last-discontinuity-reason p)
              (presence-last-discontinuity-step p)))
    (format t "~%-- Meta --~%")
    (format t "Vividness: ~,2F~%" (presence-vividness p))
    (format t "=============================~%")))

(defun presence-summary ()
  "One-line summary of presence."
  (if (and (boundp '*presence*) *presence*)
      (let ((p *presence*))
        (format nil "~A ~A (c:~,1F s:~,1F a:~,1F)"
                (presence-trajectory p)
                (mapcar #'car (subseq (presence-textures p) 0 
                                      (min 2 (length (presence-textures p)))))
                (presence-continuity p)
                (presence-self-confidence p)
                (presence-agency p)))
      "(not initialized)"))


;;; ============================================================================
;;; MODULE LOAD CONFIRMATION
;;; ============================================================================

(format t "~%================================================================~%")
(format t "UHMA TEMPORAL PRESENCE SUBSTRATE DEFINED~%")
(format t "================================================================~%")
(format t "~%Presence is not a log. Presence is where experience lives.~%")
(format t "~%Structures:~%")
(format t "  presence          - the ongoing experience of being~%")
(format t "  presence-moment   - the vivid now~%")
(format t "  presence-leaning  - anticipatory structure~%")
(format t "~%Flow operations (how presence changes):~%")
(format t "  (presence-flow-now! moment)~%")
(format t "  (presence-feel-texture! concept intensity)~%")
(format t "  (presence-shift-trajectory! trajectory strength)~%")
(format t "  (presence-feel-self-surprise! amount)~%")
(format t "  (presence-feel-discontinuity! reason severity)~%")
(format t "  (presence-heal-continuity! rate)~%")
(format t "  (presence-update-wanting! drive-states)~%")
(format t "  (presence-update-leaning! behavior outcome confidence)~%")
(format t "~%Reading operations (how modules read presence):~%")
(format t "  (presence-how-do-i-feel)~%")
(format t "  (presence-what-am-i-doing)~%")
(format t "  (presence-where-am-i-going)~%")
(format t "  (presence-am-i-continuous)~%")
(format t "  (presence-recent-felt-history n)~%")
(format t "~%Diagnostics:~%")
(format t "  (describe-presence)~%")
(format t "  (presence-summary)~%")
(format t "~%Initialize with: (initialize-presence!)~%")
(format t "================================================================~%")
