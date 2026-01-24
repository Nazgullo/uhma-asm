;;;; ============================================================================
;;;; UHMA SAVE/RESTORE SYSTEM
;;;; ============================================================================
;;;; Provides:
;;;;   1. In-memory snapshots for fast rollback (self-modification safety)
;;;;   2. Disk persistence for cross-session continuity
;;;;   3. Verification to confirm state integrity after restore
;;;; ============================================================================

(in-package :uhma)

;;;; [SECTION-START:1:CONFIG]
;;; Section 1: Configuration

(defconstant +save-restore-version+ 2)

(defparameter *state-save-directory*
  (merge-pathnames "uhma-states/" cl-user::*module-base*)
  "Directory for saved state files.")

(defparameter *max-snapshots-in-memory* 5
  "Maximum number of in-memory snapshots kept on the stack.")

(defvar *snapshot-stack* nil
  "Stack of system-snapshot structs for rollback.")

;;;; [SECTION-END:1:CONFIG]

;;;; [SECTION-START:2:SNAPSHOT-STRUCT]
;;; Section 2: system-snapshot struct

(defstruct system-snapshot
  "Complete system state captured as serializable plists/alists."
  ;; Metadata
  (version +save-restore-version+ :type fixnum)
  (timestamp 0 :type integer)
  (reason nil)
  ;; Counters
  (step 0 :type fixnum)
  (expert-counter 0 :type fixnum)
  (traveler-counter 0 :type fixnum)
  (type-counter 0 :type fixnum)
  (total-births 0 :type fixnum)
  (total-deaths 0 :type fixnum)
  ;; Core data (serialized as plists/alists)
  (experts nil)
  (neighborhoods nil)
  (root-neighborhood-id nil)
  (travelers nil)
  (long-term-memory nil)
  (hypotheses nil)
  (cognitive-schemas nil)
  (executable-schemas nil)
  (holo nil)
  (holographic-memory nil)
  (presence nil)
  (self-model nil)
  ;; Cognitive state
  (dream-state nil)
  (dreaming nil)
  (episodic-memory nil)
  (causal-model nil)
  (trace-buffer nil)
  (cognitive-trace-buffer nil)
  (goal-stack nil)
  (current-goal nil)
  (goal-history nil)
  (hypothesis-history nil)
  (modification-history nil)
  (self-expectations nil)
  (self-expectation-accuracy 0.5 :type single-float)
  (self-doubt nil)
  (attention-map nil)
  (attention-state nil)
  (introspective-concepts nil)
  (cached-active-concepts nil)
  (working-memory nil)
  (intrinsic-drives nil)
  ;; History
  (recent-outcomes nil)
  (external-outcomes nil)
  (self-modification-history nil)
  ;; Parameters
  (parameters nil))

;;;; [SECTION-END:2:SNAPSHOT-STRUCT]

;;;; [SECTION-START:3:GENERIC-SERIALIZATION]
;;; Section 3: Generic serialization helpers

(defun copy-float-array (arr)
  "Safely copy a float array, returning nil if input is nil."
  (when (and arr (arrayp arr))
    (let* ((len (length arr))
           (new (make-array len :element-type 'single-float)))
      (dotimes (i len new)
        (setf (aref new i) (aref arr i))))))

(defun float-array-to-list (arr)
  "Convert a float array to a list for serialization."
  (when (and arr (arrayp arr))
    (coerce arr 'list)))

(defun list-to-float-array (lst)
  "Convert a list back to a float array."
  (when (and lst (listp lst))
    (make-array (length lst)
                :element-type 'single-float
                :initial-contents (mapcar (lambda (x) (coerce x 'single-float)) lst))))

(defun hash-table-to-serializable (ht)
  "Convert a hash-table to a serializable plist form."
  (when (and ht (hash-table-p ht))
    (let ((entries nil)
          (test (hash-table-test ht)))
      (maphash (lambda (k v) (push (cons k v) entries)) ht)
      (list :hash-table :test test :entries entries))))

(defun serializable-to-hash-table (form)
  "Rebuild a hash-table from its serialized form."
  (when (and form (listp form) (eq (first form) :hash-table))
    (let* ((test (getf (rest form) :test))
           (entries (getf (rest form) :entries))
           (ht (make-hash-table :test (or test 'equal))))
      (dolist (pair entries ht)
        (setf (gethash (car pair) ht) (cdr pair))))))

(defun serialize-fill-pointer-array (arr &optional max-entries)
  "Serialize a fill-pointer array to a plist form."
  (when (and arr (arrayp arr) (array-has-fill-pointer-p arr))
    (let* ((fp (fill-pointer arr))
           (limit (if max-entries (min fp max-entries) fp))
           (start (if (and max-entries (> fp max-entries)) (- fp max-entries) 0))
           (contents nil))
      (loop for i from start below (+ start limit)
            do (push (aref arr i) contents))
      (list :fill-pointer limit :contents (nreverse contents)))))

(defun deserialize-fill-pointer-array (form &optional (capacity 1000))
  "Rebuild a fill-pointer array from its serialized form."
  (when (and form (listp form) (eq (first form) :fill-pointer))
    (let* ((fp (getf form :fill-pointer))
           (contents (getf form :contents))
           (arr (make-array (max capacity (or fp 0))
                            :fill-pointer 0 :adjustable t)))
      (dolist (item contents arr)
        (vector-push-extend item arr)))))

;;;; [SECTION-END:3:GENERIC-SERIALIZATION]

;;;; [SECTION-START:4:STRUCT-SERIALIZERS]
;;; Section 4: Per-struct serializers (to-plist / from-plist)

;;; --- Expert ---

(defun expert-to-plist (e)
  "Serialize an expert struct to a plist."
  (when (expert-p e)
    (list :id (expert-id e)
          :program (expert-program e)
          :code-metrics (hash-table-to-serializable (expert-code-metrics e))
          :previous-program (expert-previous-program e)
          :previous-metrics (expert-previous-metrics e)
          :change-history (expert-change-history e)
          :change-step (expert-change-step e)
          :knowledge-vector (float-array-to-list (expert-knowledge-vector e))
          :knowledge (hash-table-to-serializable (expert-knowledge e))
          :owned-type (expert-owned-type e)
          :owned-contexts (expert-owned-contexts e)
          :confidence-threshold (expert-confidence-threshold e)
          :life (expert-life e)
          :hits (expert-hits e)
          :misses (expert-misses e)
          :parent-id (expert-parent-id e)
          :birth-step (expert-birth-step e)
          :centroid (float-array-to-list (expert-centroid e))
          :neighborhood (expert-neighborhood e)
          :hidden-state (expert-hidden-state e)
          :hidden-state-strength (expert-hidden-state-strength e)
          :type-pattern-memory (expert-type-pattern-memory e)
          :traveler-links (expert-traveler-links e)
          :program-cost (expert-program-cost e)
          :last-used (expert-last-used e)
          :ownership (expert-ownership e))))

(defun plist-to-expert (plist)
  "Rebuild an expert struct from a plist."
  (when plist
    (let ((e (make-expert :id (getf plist :id)
                          :program (getf plist :program)
                          :previous-program (getf plist :previous-program)
                          :previous-metrics (getf plist :previous-metrics)
                          :change-history (getf plist :change-history)
                          :change-step (or (getf plist :change-step) 0)
                          :owned-type (getf plist :owned-type)
                          :owned-contexts (getf plist :owned-contexts)
                          :confidence-threshold (or (getf plist :confidence-threshold) 0.3)
                          :life (or (getf plist :life) 1.0)
                          :hits (or (getf plist :hits) 0)
                          :misses (or (getf plist :misses) 0)
                          :parent-id (getf plist :parent-id)
                          :birth-step (or (getf plist :birth-step) 0)
                          :neighborhood (getf plist :neighborhood)
                          :hidden-state (getf plist :hidden-state)
                          :hidden-state-strength (or (getf plist :hidden-state-strength) 0.0)
                          :type-pattern-memory (getf plist :type-pattern-memory)
                          :traveler-links (getf plist :traveler-links)
                          :program-cost (or (getf plist :program-cost) 0.0)
                          :last-used (or (getf plist :last-used) 0)
                          :ownership (or (getf plist :ownership) 1.0))))
      ;; Restore hash-tables
      (let ((cm (serializable-to-hash-table (getf plist :code-metrics))))
        (when cm (setf (expert-code-metrics e) cm)))
      (let ((k (serializable-to-hash-table (getf plist :knowledge))))
        (when k (setf (expert-knowledge e) k)))
      ;; Restore float arrays
      (let ((kv (getf plist :knowledge-vector)))
        (when kv (setf (expert-knowledge-vector e) (list-to-float-array kv))))
      (let ((c (getf plist :centroid)))
        (when c (setf (expert-centroid e) (list-to-float-array c))))
      e)))

;;; --- Neighborhood ---

(defun neighborhood-to-plist (n)
  "Serialize a neighborhood. Parent/children stored as IDs to break circularity."
  (when (neighborhood-p n)
    (list :id (neighborhood-id n)
          :parent-id (when (neighborhood-parent n)
                       (neighborhood-id (neighborhood-parent n)))
          :children-ids (mapcar #'neighborhood-id (neighborhood-children n))
          :experts (neighborhood-experts n)
          :centroid (float-array-to-list (neighborhood-centroid n))
          :type-signature (neighborhood-type-signature n)
          :depth (neighborhood-depth n))))

(defun plist-to-neighborhood (plist)
  "Create a neighborhood from plist (parent/children reconnected in 2nd pass)."
  (when plist
    (let ((n (make-neighborhood :id (getf plist :id)
                                :experts (getf plist :experts)
                                :type-signature (getf plist :type-signature)
                                :depth (or (getf plist :depth) 0))))
      (let ((c (getf plist :centroid)))
        (when c (setf (neighborhood-centroid n) (list-to-float-array c))))
      n)))

(defun reconnect-neighborhoods! (nbhd-plists nbhd-map)
  "Second pass: reconnect parent/children references using the ID map."
  (dolist (pl nbhd-plists)
    (let* ((id (getf pl :id))
           (nbhd (gethash id nbhd-map)))
      (when nbhd
        (let ((parent-id (getf pl :parent-id)))
          (when parent-id
            (setf (neighborhood-parent nbhd)
                  (gethash parent-id nbhd-map))))
        (setf (neighborhood-children nbhd)
              (remove nil
                      (mapcar (lambda (cid) (gethash cid nbhd-map))
                              (getf pl :children-ids))))))))

;;; --- Traveler ---

(defun traveler-to-plist (tr)
  "Serialize a traveler struct."
  (when (traveler-p tr)
    (list :id (traveler-id tr)
          :expert-id (traveler-expert-id tr)
          :home-neighborhood (traveler-home-neighborhood tr)
          :visited-neighborhoods (traveler-visited-neighborhoods tr)
          :bridge-patterns (traveler-bridge-patterns tr)
          :created-at (traveler-created-at tr)
          :bridge-count (traveler-bridge-count tr))))

(defun plist-to-traveler (plist)
  "Rebuild a traveler from plist."
  (when plist
    (make-traveler :id (getf plist :id)
                   :expert-id (getf plist :expert-id)
                   :home-neighborhood (getf plist :home-neighborhood)
                   :visited-neighborhoods (getf plist :visited-neighborhoods)
                   :bridge-patterns (getf plist :bridge-patterns)
                   :created-at (or (getf plist :created-at) 0)
                   :bridge-count (or (getf plist :bridge-count) 0))))

;;; --- Self-Model ---

(defun self-model-to-plist (sm)
  "Serialize a self-model struct."
  (when (and sm (self-model-p sm))
    (list :expected-error (self-model-expected-error sm)
          :error-baseline (self-model-error-baseline sm)
          :recent-errors (self-model-recent-errors sm)
          :error-window (self-model-error-window sm)
          :global-confidence (self-model-global-confidence sm)
          :sparse-threshold-min (self-model-sparse-threshold-min sm)
          :sparse-threshold-max (self-model-sparse-threshold-max sm)
          :current-sparse-threshold (self-model-current-sparse-threshold sm)
          :uncertainty-low (self-model-uncertainty-low sm)
          :uncertainty-high (self-model-uncertainty-high sm)
          :total-verified (self-model-total-verified sm)
          :total-skipped (self-model-total-skipped sm)
          :threshold-history (self-model-threshold-history sm)
          :behavior-patterns (hash-table-to-serializable (self-model-behavior-patterns sm))
          :program-structure-stats (hash-table-to-serializable (self-model-program-structure-stats sm))
          :best-program-structures (self-model-best-program-structures sm)
          :param-performance (hash-table-to-serializable (self-model-param-performance sm)))))

(defun plist-to-self-model (plist)
  "Rebuild a self-model from plist."
  (when plist
    (let ((sm (make-self-model
               :expected-error (or (getf plist :expected-error) 0.5)
               :error-baseline (or (getf plist :error-baseline) 0.5)
               :recent-errors (getf plist :recent-errors)
               :error-window (or (getf plist :error-window) 50)
               :global-confidence (or (getf plist :global-confidence) 0.5)
               :sparse-threshold-min (or (getf plist :sparse-threshold-min) 0.65)
               :sparse-threshold-max (or (getf plist :sparse-threshold-max) 0.95)
               :current-sparse-threshold (or (getf plist :current-sparse-threshold) 0.85)
               :uncertainty-low (or (getf plist :uncertainty-low) 0.3)
               :uncertainty-high (or (getf plist :uncertainty-high) 1.5)
               :total-verified (or (getf plist :total-verified) 0)
               :total-skipped (or (getf plist :total-skipped) 0)
               :threshold-history (getf plist :threshold-history)
               :best-program-structures (getf plist :best-program-structures))))
      (let ((bp (serializable-to-hash-table (getf plist :behavior-patterns))))
        (when bp (setf (self-model-behavior-patterns sm) bp)))
      (let ((ps (serializable-to-hash-table (getf plist :program-structure-stats))))
        (when ps (setf (self-model-program-structure-stats sm) ps)))
      (let ((pp (serializable-to-hash-table (getf plist :param-performance))))
        (when pp (setf (self-model-param-performance sm) pp)))
      sm)))

;;; --- Presence ---

(defun presence-leaning-to-plist (pl)
  "Serialize a presence-leaning sub-struct."
  (when (and pl (presence-leaning-p pl))
    (list :expected-context (presence-leaning-expected-context pl)
          :expected-behavior (presence-leaning-expected-behavior pl)
          :expected-outcome (presence-leaning-expected-outcome pl)
          :outcome-confidence (presence-leaning-outcome-confidence pl)
          :expected-texture (presence-leaning-expected-texture pl)
          :tension (presence-leaning-tension pl)
          :openness (presence-leaning-openness pl))))

(defun plist-to-presence-leaning (plist)
  "Rebuild presence-leaning from plist."
  (when plist
    (make-presence-leaning
     :expected-context (getf plist :expected-context)
     :expected-behavior (getf plist :expected-behavior)
     :expected-outcome (or (getf plist :expected-outcome) :uncertain)
     :outcome-confidence (or (getf plist :outcome-confidence) 0.0)
     :expected-texture (getf plist :expected-texture)
     :tension (or (getf plist :tension) 0.0)
     :openness (or (getf plist :openness) 0.5))))

(defun presence-moment-to-plist (pm)
  "Serialize a presence-moment sub-struct."
  (when (and pm (presence-moment-p pm))
    (list :input (presence-moment-input pm)
          :context-feel (presence-moment-context-feel pm)
          :activations (presence-moment-activations pm)
          :prediction-forming (presence-moment-prediction-forming pm)
          :prediction-confidence (presence-moment-prediction-confidence pm)
          :outcome (presence-moment-outcome pm)
          :outcome-surprise (presence-moment-outcome-surprise pm)
          :answering-expert (presence-moment-answering-expert pm)
          :felt-as-mine-p (presence-moment-felt-as-mine-p pm)
          :concept-colors (presence-moment-concept-colors pm)
          :step (presence-moment-step pm)
          :vividness (presence-moment-vividness pm))))

(defun plist-to-presence-moment (plist)
  "Rebuild presence-moment from plist."
  (when plist
    (make-presence-moment
     :input (getf plist :input)
     :context-feel (getf plist :context-feel)
     :activations (getf plist :activations)
     :prediction-forming (getf plist :prediction-forming)
     :prediction-confidence (or (getf plist :prediction-confidence) 0.0)
     :outcome (getf plist :outcome)
     :outcome-surprise (or (getf plist :outcome-surprise) 0.0)
     :answering-expert (getf plist :answering-expert)
     :felt-as-mine-p (if (member :felt-as-mine-p plist) (getf plist :felt-as-mine-p) t)
     :concept-colors (getf plist :concept-colors)
     :step (or (getf plist :step) 0)
     :vividness (or (getf plist :vividness) 1.0))))

(defun presence-to-plist (p)
  "Serialize presence struct."
  (when (and p (presence-p p))
    (list :now (presence-moment-to-plist (presence-now p))
          :fading (coerce (presence-fading p) 'list)
          :fading-depth (presence-fading-depth p)
          :leaning (presence-leaning-to-plist (presence-leaning p))
          :textures (presence-textures p)
          :texture-intensity (presence-texture-intensity p)
          :texture-shifting-p (presence-texture-shifting-p p)
          :texture-drift (presence-texture-drift p)
          :trajectory (presence-trajectory p)
          :trajectory-strength (presence-trajectory-strength p)
          :arc-momentum (presence-arc-momentum p)
          :episode-phase (presence-episode-phase p)
          :arc-signature (presence-arc-signature p)
          :self-confidence (presence-self-confidence p)
          :self-surprise-residue (presence-self-surprise-residue p)
          :wanting (presence-wanting p)
          :feeling-capable-p (presence-feeling-capable-p p)
          :capability-contour (presence-capability-contour p)
          :identity-feel (presence-identity-feel p)
          :agency (presence-agency p)
          :felt-direction (presence-felt-direction p)
          :continuity (presence-continuity p)
          :deep-continuity (presence-deep-continuity p)
          :last-discontinuity-step (presence-last-discontinuity-step p)
          :last-discontinuity-reason (presence-last-discontinuity-reason p)
          :identity-stable-p (presence-identity-stable-p p)
          :subjective-duration (presence-subjective-duration p)
          :vividness (presence-vividness p)
          :step (presence-step p)
          :timestamp (presence-timestamp p))))

(defun plist-to-presence (plist)
  "Rebuild presence from plist."
  (when plist
    (let ((p (make-presence
              :fading-depth (or (getf plist :fading-depth) 8)
              :textures (getf plist :textures)
              :texture-intensity (or (getf plist :texture-intensity) 0.5)
              :texture-shifting-p (getf plist :texture-shifting-p)
              :texture-drift (getf plist :texture-drift)
              :trajectory (or (getf plist :trajectory) :flowing)
              :trajectory-strength (or (getf plist :trajectory-strength) 0.5)
              :arc-momentum (or (getf plist :arc-momentum) 0.0)
              :episode-phase (or (getf plist :episode-phase) :unfolding)
              :arc-signature (getf plist :arc-signature)
              :self-confidence (or (getf plist :self-confidence) 0.5)
              :self-surprise-residue (or (getf plist :self-surprise-residue) 0.0)
              :wanting (getf plist :wanting)
              :feeling-capable-p (if (member :feeling-capable-p plist) (getf plist :feeling-capable-p) t)
              :capability-contour (getf plist :capability-contour)
              :identity-feel (getf plist :identity-feel)
              :agency (or (getf plist :agency) 1.0)
              :felt-direction (getf plist :felt-direction)
              :continuity (or (getf plist :continuity) 1.0)
              :deep-continuity (or (getf plist :deep-continuity) 1.0)
              :last-discontinuity-step (getf plist :last-discontinuity-step)
              :last-discontinuity-reason (getf plist :last-discontinuity-reason)
              :identity-stable-p (if (member :identity-stable-p plist) (getf plist :identity-stable-p) t)
              :subjective-duration (or (getf plist :subjective-duration) 0.0)
              :vividness (or (getf plist :vividness) 1.0)
              :step (or (getf plist :step) 0)
              :timestamp (or (getf plist :timestamp) 0))))
      ;; Restore sub-structs
      (setf (presence-now p)
            (or (plist-to-presence-moment (getf plist :now))
                (make-presence-moment)))
      (setf (presence-leaning p)
            (or (plist-to-presence-leaning (getf plist :leaning))
                (make-presence-leaning)))
      ;; Restore fading array
      (let ((fading-data (getf plist :fading)))
        (when fading-data
          (let ((arr (make-array (presence-fading-depth p) :initial-element nil)))
            (loop for item in fading-data
                  for i from 0 below (length arr)
                  do (setf (aref arr i) item))
            (setf (presence-fading p) arr))))
      p)))

;;; --- Cognitive-Trace ---

(defun cognitive-trace-to-plist (ct)
  "Serialize a cognitive-trace struct."
  (when (and ct (cognitive-trace-p ct))
    (list :step (cognitive-trace-step ct)
          :timestamp (cognitive-trace-timestamp ct)
          :context (cognitive-trace-context ct)
          :prediction (cognitive-trace-prediction ct)
          :actual (cognitive-trace-actual ct)
          :confidence (cognitive-trace-confidence ct)
          :surprise (cognitive-trace-surprise ct)
          :expert-id (cognitive-trace-expert-id ct)
          :meta-level (cognitive-trace-meta-level ct)
          :reasoning-path (cognitive-trace-reasoning-path ct)
          :meta-observations (cognitive-trace-meta-observations ct)
          :alternatives (cognitive-trace-alternatives ct)
          :epistemic-state (cognitive-trace-epistemic-state ct))))

(defun plist-to-cognitive-trace (plist)
  "Rebuild a cognitive-trace from plist."
  (when plist
    (make-cognitive-trace
     :step (or (getf plist :step) 0)
     :timestamp (or (getf plist :timestamp) 0)
     :context (getf plist :context)
     :prediction (getf plist :prediction)
     :actual (getf plist :actual)
     :confidence (or (getf plist :confidence) 0.5)
     :surprise (or (getf plist :surprise) 0.0)
     :expert-id (getf plist :expert-id)
     :meta-level (or (getf plist :meta-level) 0)
     :reasoning-path (getf plist :reasoning-path)
     :meta-observations (getf plist :meta-observations)
     :alternatives (getf plist :alternatives)
     :epistemic-state (getf plist :epistemic-state))))

;;; --- Self-Hypothesis ---

(defun self-hypothesis-to-plist (h)
  "Serialize a self-hypothesis struct."
  (when (and h (self-hypothesis-p h))
    (list :id (self-hypothesis-id h)
          :claim (self-hypothesis-claim h)
          :claim-type (self-hypothesis-claim-type h)
          :evidence-for (self-hypothesis-evidence-for h)
          :evidence-against (self-hypothesis-evidence-against h)
          :confidence (self-hypothesis-confidence h)
          :times-tested (self-hypothesis-times-tested h)
          :times-confirmed (self-hypothesis-times-confirmed h)
          :proposed-action (self-hypothesis-proposed-action h)
          :created-at (self-hypothesis-created-at h)
          :last-tested (self-hypothesis-last-tested h)
          :status (self-hypothesis-status h))))

(defun plist-to-self-hypothesis (plist)
  "Rebuild a self-hypothesis from plist."
  (when plist
    (make-self-hypothesis
     :id (getf plist :id)
     :claim (getf plist :claim)
     :claim-type (getf plist :claim-type)
     :evidence-for (getf plist :evidence-for)
     :evidence-against (getf plist :evidence-against)
     :confidence (or (getf plist :confidence) 0.5)
     :times-tested (or (getf plist :times-tested) 0)
     :times-confirmed (or (getf plist :times-confirmed) 0)
     :proposed-action (getf plist :proposed-action)
     :created-at (or (getf plist :created-at) 0)
     :last-tested (or (getf plist :last-tested) 0)
     :status (or (getf plist :status) :active))))

;;; --- Cognitive-Schema ---

(defun cognitive-schema-to-plist (cs)
  "Serialize a cognitive-schema struct."
  (when (and cs (cognitive-schema-p cs))
    (list :id (cognitive-schema-id cs)
          :pattern (cognitive-schema-pattern cs)
          :instances (cognitive-schema-instances cs)
          :successes (cognitive-schema-successes cs)
          :contexts (cognitive-schema-contexts cs)
          :sub-schemas (cognitive-schema-sub-schemas cs)
          :created-at (cognitive-schema-created-at cs)
          :last-used (cognitive-schema-last-used cs)
          :evolved-from (cognitive-schema-evolved-from cs))))

(defun plist-to-cognitive-schema (plist)
  "Rebuild a cognitive-schema from plist."
  (when plist
    (make-cognitive-schema
     :id (getf plist :id)
     :pattern (getf plist :pattern)
     :instances (or (getf plist :instances) 0)
     :successes (or (getf plist :successes) 0)
     :contexts (getf plist :contexts)
     :sub-schemas (getf plist :sub-schemas)
     :created-at (or (getf plist :created-at) 0)
     :last-used (or (getf plist :last-used) 0)
     :evolved-from (getf plist :evolved-from))))

;;; --- Executable-Schema ---

(defun executable-schema-to-plist (es)
  "Serialize an executable-schema struct."
  (when (and es (executable-schema-p es))
    (list :id (executable-schema-id es)
          :source-schema-id (executable-schema-source-schema-id es)
          :op-sequence (executable-schema-op-sequence es)
          :entry-conditions (executable-schema-entry-conditions es)
          :parameter-bindings (executable-schema-parameter-bindings es)
          :execution-count (executable-schema-execution-count es)
          :success-count (executable-schema-success-count es)
          :avg-confidence (executable-schema-avg-confidence es)
          :contexts-applied (executable-schema-contexts-applied es)
          :compiled-at (executable-schema-compiled-at es)
          :last-executed (executable-schema-last-executed es))))

(defun plist-to-executable-schema (plist)
  "Rebuild an executable-schema from plist."
  (when plist
    (make-executable-schema
     :id (getf plist :id)
     :source-schema-id (getf plist :source-schema-id)
     :op-sequence (getf plist :op-sequence)
     :entry-conditions (getf plist :entry-conditions)
     :parameter-bindings (getf plist :parameter-bindings)
     :execution-count (or (getf plist :execution-count) 0)
     :success-count (or (getf plist :success-count) 0)
     :avg-confidence (or (getf plist :avg-confidence) 0.0)
     :contexts-applied (getf plist :contexts-applied)
     :compiled-at (or (getf plist :compiled-at) 0)
     :last-executed (or (getf plist :last-executed) 0))))

;;; --- Goal ---

(defun goal-to-plist (g)
  "Serialize a goal struct."
  (when (and g (goal-p g))
    (list :id (goal-id g)
          :type (goal-type g)
          :description (goal-description g)
          :priority (goal-priority g)
          :status (goal-status g)
          :drive-source (goal-drive-source g)
          :strategies (goal-strategies g)
          :attempts (goal-attempts g)
          :created-at (goal-created-at g))))

(defun plist-to-goal (plist)
  "Rebuild a goal from plist."
  (when plist
    (make-goal
     :id (getf plist :id)
     :type (or (getf plist :type) :intrinsic)
     :description (or (getf plist :description) "")
     :priority (or (getf plist :priority) 0.5)
     :status (or (getf plist :status) :active)
     :drive-source (getf plist :drive-source)
     :strategies (getf plist :strategies)
     :attempts (getf plist :attempts)
     :created-at (or (getf plist :created-at) 0))))

;;; --- Dream ---

(defun dream-episode-to-plist (de)
  "Serialize a dream-episode struct."
  (when (and de (dream-episode-p de))
    (list :context (dream-episode-context de)
          :original-prediction (dream-episode-original-prediction de)
          :actual-outcome (dream-episode-actual-outcome de)
          :difficulty (dream-episode-difficulty de)
          :replayed-count (dream-episode-replayed-count de)
          :insights (dream-episode-insights de)
          :created-at (dream-episode-created-at de))))

(defun plist-to-dream-episode (plist)
  "Rebuild a dream-episode from plist."
  (when plist
    (make-dream-episode
     :context (getf plist :context)
     :original-prediction (getf plist :original-prediction)
     :actual-outcome (getf plist :actual-outcome)
     :difficulty (or (getf plist :difficulty) 0.0)
     :replayed-count (or (getf plist :replayed-count) 0)
     :insights (or (getf plist :insights) 0)
     :created-at (or (getf plist :created-at) 0))))

(defun dream-state-to-plist (ds)
  "Serialize dream-state struct."
  (when (and ds (dream-state-p ds))
    (list :episode-buffer (mapcar #'dream-episode-to-plist
                                  (dream-state-episode-buffer ds))
          :dream-cycle (dream-state-dream-cycle ds)
          :total-dreams (dream-state-total-dreams ds)
          :insights-discovered (dream-state-insights-discovered ds))))

(defun plist-to-dream-state (plist)
  "Rebuild dream-state from plist."
  (when plist
    (make-dream-state
     :episode-buffer (mapcar #'plist-to-dream-episode
                             (getf plist :episode-buffer))
     :dream-cycle (or (getf plist :dream-cycle) 0)
     :total-dreams (or (getf plist :total-dreams) 0)
     :insights-discovered (or (getf plist :insights-discovered) 0))))

;;; --- Episodic Memory ---

(defun episode-event-to-plist (ev)
  "Serialize an episode-event struct."
  (when (and ev (episode-event-p ev))
    (list :step (episode-event-step ev)
          :type (episode-event-type ev)
          :description (episode-event-description ev)
          :self-expectation (episode-event-self-expectation ev)
          :self-surprise (episode-event-self-surprise ev)
          :data (episode-event-data ev))))

(defun plist-to-episode-event (plist)
  "Rebuild an episode-event from plist."
  (when plist
    (make-episode-event
     :step (or (getf plist :step) 0)
     :type (getf plist :type)
     :description (getf plist :description)
     :self-expectation (getf plist :self-expectation)
     :self-surprise (or (getf plist :self-surprise) 0.0)
     :data (getf plist :data))))

(defun episode-to-plist (ep)
  "Serialize an episode struct."
  (when (and ep (episode-p ep))
    (list :id (episode-id ep)
          :start-step (episode-start-step ep)
          :end-step (episode-end-step ep)
          :trigger (episode-trigger ep)
          :events (mapcar #'episode-event-to-plist (episode-events ep))
          :outcome (episode-outcome ep)
          :drive-state-start (episode-drive-state-start ep)
          :drive-state-end (episode-drive-state-end ep)
          :confidence-arc (episode-confidence-arc ep)
          :self-relevant-p (episode-self-relevant-p ep)
          :significance (episode-significance ep)
          :tags (episode-tags ep)
          :causal-links (episode-causal-links ep)
          :self-surprise-total (episode-self-surprise-total ep)
          :outcome-surprise-total (episode-outcome-surprise-total ep)
          :concepts-activated (episode-concepts-activated ep)
          :narrative (episode-narrative ep))))

(defun plist-to-episode (plist)
  "Rebuild an episode from plist."
  (when plist
    (make-episode
     :id (getf plist :id)
     :start-step (or (getf plist :start-step) 0)
     :end-step (or (getf plist :end-step) 0)
     :trigger (getf plist :trigger)
     :events (mapcar #'plist-to-episode-event (getf plist :events))
     :outcome (getf plist :outcome)
     :drive-state-start (getf plist :drive-state-start)
     :drive-state-end (getf plist :drive-state-end)
     :confidence-arc (getf plist :confidence-arc)
     :self-relevant-p (getf plist :self-relevant-p)
     :significance (or (getf plist :significance) 0.0)
     :tags (getf plist :tags)
     :causal-links (getf plist :causal-links)
     :self-surprise-total (or (getf plist :self-surprise-total) 0.0)
     :outcome-surprise-total (or (getf plist :outcome-surprise-total) 0.0)
     :concepts-activated (getf plist :concepts-activated)
     :narrative (getf plist :narrative))))

(defun episodic-memory-to-plist (em)
  "Serialize episodic-memory struct."
  (when (and em (episodic-memory-p em))
    (let ((episodes-list nil))
      ;; Serialize episodes from fill-pointer array
      (dotimes (i (fill-pointer (episodic-memory-episodes em)))
        (push (episode-to-plist (aref (episodic-memory-episodes em) i))
              episodes-list))
      (list :episodes (nreverse episodes-list)
            :current-episode (when (episodic-memory-current-episode em)
                               (episode-to-plist (episodic-memory-current-episode em)))
            :episode-count (episodic-memory-episode-count em)
            :max-episodes (episodic-memory-max-episodes em)
            :by-tag (hash-table-to-serializable (episodic-memory-by-tag em))
            :autobiographical (episodic-memory-autobiographical em)
            :schemas (hash-table-to-serializable (episodic-memory-schemas em))
            :capacity (episodic-memory-capacity em)))))

(defun plist-to-episodic-memory (plist)
  "Rebuild episodic-memory from plist."
  (when plist
    (let* ((cap (or (getf plist :capacity) 1000))
           (em (make-episodic-memory-struct
                :episode-count (or (getf plist :episode-count) 0)
                :max-episodes (or (getf plist :max-episodes) 1000)
                :autobiographical (getf plist :autobiographical)
                :capacity cap)))
      ;; Restore episodes into fill-pointer array
      (setf (episodic-memory-episodes em)
            (make-array cap :fill-pointer 0 :adjustable t))
      (dolist (ep-plist (getf plist :episodes))
        (let ((ep (plist-to-episode ep-plist)))
          (when ep (vector-push-extend ep (episodic-memory-episodes em)))))
      ;; Restore current-episode
      (let ((cur (getf plist :current-episode)))
        (when cur
          (setf (episodic-memory-current-episode em) (plist-to-episode cur))))
      ;; Restore by-significance as empty (rebuilt from episodes)
      (setf (episodic-memory-by-significance em)
            (make-array 50 :fill-pointer 0 :adjustable t))
      ;; Rebuild by-significance from episodes
      (dotimes (i (fill-pointer (episodic-memory-episodes em)))
        (let ((ep (aref (episodic-memory-episodes em) i)))
          (when (> (episode-significance ep) 0.5)
            (vector-push-extend ep (episodic-memory-by-significance em)))))
      ;; Restore hash-tables
      (let ((bt (serializable-to-hash-table (getf plist :by-tag))))
        (when bt (setf (episodic-memory-by-tag em) bt)))
      (let ((sc (serializable-to-hash-table (getf plist :schemas))))
        (when sc (setf (episodic-memory-schemas em) sc)))
      em)))

;;; --- Holo-Pattern ---

(defun holo-pattern-to-plist (hp)
  "Serialize a holo-pattern struct."
  (when (and hp (holo-pattern-p hp))
    (list :id (holo-pattern-id hp)
          :activations (holo-pattern-activations hp)
          :layer (holo-pattern-layer hp)
          :content-type (holo-pattern-content-type hp)
          :content (holo-pattern-content hp)
          :strength (holo-pattern-strength hp)
          :created-step (holo-pattern-created-step hp)
          :access-count (holo-pattern-access-count hp)
          :hits (holo-pattern-hits hp)
          :misses (holo-pattern-misses hp))))

(defun plist-to-holo-pattern (plist)
  "Rebuild a holo-pattern from plist."
  (when plist
    (make-holo-pattern
     :id (getf plist :id)
     :activations (getf plist :activations)
     :layer (or (getf plist :layer) :immediate)
     :content-type (getf plist :content-type)
     :content (getf plist :content)
     :strength (or (getf plist :strength) 1.0)
     :created-step (or (getf plist :created-step) 0)
     :access-count (or (getf plist :access-count) 0)
     :hits (or (getf plist :hits) 0)
     :misses (or (getf plist :misses) 0))))

;;; --- Holo-Store ---

(defun serialize-holo-hash (ht)
  "Serialize a hash-table whose values are holo-pattern structs."
  (when (and ht (hash-table-p ht))
    (let ((entries nil)
          (test (hash-table-test ht)))
      (maphash (lambda (k v)
                 (push (cons k (if (holo-pattern-p v)
                                   (holo-pattern-to-plist v)
                                   v))
               entries))
               ht)
      (list :hash-table :test test :entries entries))))

(defun deserialize-holo-hash (form &optional values-are-patterns)
  "Rebuild a hash-table, optionally converting values to holo-pattern structs."
  (when (and form (listp form) (eq (first form) :hash-table))
    (let* ((test (getf (rest form) :test))
           (entries (getf (rest form) :entries))
           (ht (make-hash-table :test (or test 'equal))))
      (dolist (pair entries ht)
        (setf (gethash (car pair) ht)
              (if values-are-patterns
                  (plist-to-holo-pattern (cdr pair))
                  (cdr pair)))))))

(defun serialize-holo-list-hash (ht)
  "Serialize a hash-table whose values are lists of holo-pattern structs."
  (when (and ht (hash-table-p ht))
    (let ((entries nil)
          (test (hash-table-test ht)))
      (maphash (lambda (k v)
                 (push (cons k (mapcar (lambda (hp)
                                         (if (holo-pattern-p hp)
                                             (holo-pattern-to-plist hp)
                                             hp))
                                       v))
                       entries))
               ht)
      (list :hash-table :test test :entries entries))))

(defun deserialize-holo-list-hash (form)
  "Rebuild a hash-table whose values are lists of holo-pattern structs."
  (when (and form (listp form) (eq (first form) :hash-table))
    (let* ((test (getf (rest form) :test))
           (entries (getf (rest form) :entries))
           (ht (make-hash-table :test (or test 'equal))))
      (dolist (pair entries ht)
        (setf (gethash (car pair) ht)
              (mapcar #'plist-to-holo-pattern (cdr pair)))))))

(defun holo-store-to-plist (hs)
  "Serialize a holo-store struct.
Note: by-layer/by-type/by-expert store lists of pattern IDs, not pattern structs."
  (when (and hs (holo-store-p hs))
    (list :patterns (serialize-holo-hash (holo-store-patterns hs))
          :by-layer (hash-table-to-serializable (holo-store-by-layer hs))
          :by-type (hash-table-to-serializable (holo-store-by-type hs))
          :by-expert (hash-table-to-serializable (holo-store-by-expert hs))
          :total-stored (holo-store-total-stored hs)
          :total-retrieved (holo-store-total-retrieved hs))))

(defun plist-to-holo-store (plist)
  "Rebuild a holo-store from plist."
  (when plist
    (let ((hs (make-holo-store
               :total-stored (or (getf plist :total-stored) 0)
               :total-retrieved (or (getf plist :total-retrieved) 0))))
      (let ((p (deserialize-holo-hash (getf plist :patterns) t)))
        (when p (setf (holo-store-patterns hs) p)))
      (let ((bl (serializable-to-hash-table (getf plist :by-layer))))
        (when bl (setf (holo-store-by-layer hs) bl)))
      (let ((bt (serializable-to-hash-table (getf plist :by-type))))
        (when bt (setf (holo-store-by-type hs) bt)))
      (let ((be (serializable-to-hash-table (getf plist :by-expert))))
        (when be (setf (holo-store-by-expert hs) be)))
      hs)))

;;; --- Holographic-Memory ---

(defun holographic-memory-to-plist (hm)
  "Serialize a holographic-memory struct."
  (when (and hm (holographic-memory-p hm))
    (list :patterns (serialize-holo-list-hash (holographic-memory-patterns hm))
          :by-layer (serialize-holo-list-hash (holographic-memory-by-layer hm))
          :count (holographic-memory-count hm))))

(defun plist-to-holographic-memory (plist)
  "Rebuild a holographic-memory from plist."
  (when plist
    (let ((hm (make-holographic-memory
               :count (or (getf plist :count) 0))))
      (let ((p (deserialize-holo-list-hash (getf plist :patterns))))
        (when p (setf (holographic-memory-patterns hm) p)))
      (let ((bl (deserialize-holo-list-hash (getf plist :by-layer))))
        (when bl (setf (holographic-memory-by-layer hm) bl)))
      hm)))

;;; --- Causal-Link ---

(defun causal-link-to-plist (cl)
  "Serialize a causal-link struct."
  (when (and cl (causal-link-p cl))
    (list :id (causal-link-id cl)
          :cause (causal-link-cause cl)
          :effect (causal-link-effect cl)
          :strength (causal-link-strength cl)
          :delay (causal-link-delay cl)
          :status (causal-link-status cl)
          :observations (causal-link-observations cl)
          :confirmations (causal-link-confirmations cl))))

(defun plist-to-causal-link (plist)
  "Rebuild a causal-link from plist."
  (when plist
    (make-causal-link
     :id (getf plist :id)
     :cause (getf plist :cause)
     :effect (getf plist :effect)
     :strength (or (getf plist :strength) 0.5)
     :delay (or (getf plist :delay) 1)
     :status (or (getf plist :status) :hypothesized)
     :observations (or (getf plist :observations) 0)
     :confirmations (or (getf plist :confirmations) 0))))

;;; --- Self-Doubt-Model ---

(defun self-doubt-model-to-plist (sd)
  "Serialize a self-doubt-model struct."
  (when (and sd (self-doubt-model-p sd))
    (list :introspection-confidence (self-doubt-model-introspection-confidence sd)
          :track-record (self-doubt-model-track-record sd)
          :blind-spot-hypotheses (self-doubt-model-blind-spot-hypotheses sd)
          :model-staleness (self-doubt-model-model-staleness sd)
          :last-model-update (self-doubt-model-last-model-update sd)
          :revision-triggered (self-doubt-model-revision-triggered sd))))

(defun plist-to-self-doubt-model (plist)
  "Rebuild a self-doubt-model from plist."
  (when plist
    (make-self-doubt-model
     :introspection-confidence (or (getf plist :introspection-confidence) 0.5)
     :track-record (getf plist :track-record)
     :blind-spot-hypotheses (getf plist :blind-spot-hypotheses)
     :model-staleness (or (getf plist :model-staleness) 0.0)
     :last-model-update (or (getf plist :last-model-update) 0)
     :revision-triggered (getf plist :revision-triggered))))

;;; --- Attention-State ---

(defun attention-state-to-plist (as)
  "Serialize an attention-state struct."
  (when (and as (attention-state-p as))
    (list :interesting-contexts (attention-state-interesting-contexts as)
          :boring-contexts (attention-state-boring-contexts as)
          :mysterious-contexts (attention-state-mysterious-contexts as))))

(defun plist-to-attention-state (plist)
  "Rebuild an attention-state from plist."
  (when plist
    (make-attention-state
     :interesting-contexts (getf plist :interesting-contexts)
     :boring-contexts (getf plist :boring-contexts)
     :mysterious-contexts (getf plist :mysterious-contexts))))

;;; --- Working-Memory ---

(defun working-memory-to-plist (wm)
  "Serialize a working-memory struct."
  (when (and wm (working-memory-p wm))
    (list :capacity (working-memory-capacity wm)
          :slots (coerce (working-memory-slots wm) 'list)
          :slot-ages (coerce (working-memory-slot-ages wm) 'list)
          :slot-priorities (coerce (working-memory-slot-priorities wm) 'list)
          :current-focus (working-memory-current-focus wm)
          :compression-count (working-memory-compression-count wm)
          :overflow-count (working-memory-overflow-count wm)
          :total-bindings (working-memory-total-bindings wm))))

(defun plist-to-working-memory (plist)
  "Rebuild a working-memory from plist."
  (when plist
    (let* ((cap (or (getf plist :capacity) 7))
           (wm (make-working-memory :capacity cap
                                    :current-focus (getf plist :current-focus)
                                    :compression-count (or (getf plist :compression-count) 0)
                                    :overflow-count (or (getf plist :overflow-count) 0)
                                    :total-bindings (or (getf plist :total-bindings) 0))))
      ;; Restore fixed-size arrays
      (let ((slots (getf plist :slots)))
        (when slots
          (setf (working-memory-slots wm)
                (make-array cap :initial-contents
                            (loop for i below cap
                                  collect (nth i slots))))))
      (let ((ages (getf plist :slot-ages)))
        (when ages
          (setf (working-memory-slot-ages wm)
                (make-array cap :element-type 'fixnum
                            :initial-contents
                            (loop for i below cap
                                  collect (or (nth i ages) 0))))))
      (let ((pris (getf plist :slot-priorities)))
        (when pris
          (setf (working-memory-slot-priorities wm)
                (make-array cap :element-type 'single-float
                            :initial-contents
                            (loop for i below cap
                                  collect (coerce (or (nth i pris) 0.0) 'single-float))))))
      wm)))

;;; --- Type-Cluster ---

(defun type-cluster-to-plist (tc)
  "Serialize a type-cluster struct."
  (when (and tc (type-cluster-p tc))
    (list :id (type-cluster-id tc)
          :members (type-cluster-members tc)
          :centroid (float-array-to-list (type-cluster-centroid tc))
          :access-count (type-cluster-access-count tc)
          :context-distribution (hash-table-to-serializable
                                 (type-cluster-context-distribution tc))
          :prediction-contexts (type-cluster-prediction-contexts tc)
          :created-at (type-cluster-created-at tc))))

(defun plist-to-type-cluster (plist)
  "Rebuild a type-cluster from plist."
  (when plist
    (let ((tc (make-type-cluster
               :id (getf plist :id)
               :members (getf plist :members)
               :access-count (or (getf plist :access-count) 0)
               :prediction-contexts (getf plist :prediction-contexts)
               :created-at (or (getf plist :created-at) 0))))
      (let ((c (getf plist :centroid)))
        (when c (setf (type-cluster-centroid tc) (list-to-float-array c))))
      (let ((cd (serializable-to-hash-table (getf plist :context-distribution))))
        (when cd (setf (type-cluster-context-distribution tc) cd)))
      tc)))

;;;; [SECTION-END:4:STRUCT-SERIALIZERS]

;;;; [SECTION-START:5:STATE-COLLECTION]
;;; Section 5: State collection

(defun serialize-hash-of-structs (ht to-plist-fn)
  "Serialize a hash-table whose values are structs, using the given serializer."
  (when (and ht (hash-table-p ht))
    (let ((entries nil)
          (test (hash-table-test ht)))
      (maphash (lambda (k v)
                 (push (cons k (funcall to-plist-fn v)) entries))
               ht)
      (list :hash-table :test test :entries entries))))

(defun deserialize-hash-of-structs (form from-plist-fn)
  "Rebuild a hash-table whose values are structs, using the given deserializer."
  (when (and form (listp form) (eq (first form) :hash-table))
    (let* ((test (getf (rest form) :test))
           (entries (getf (rest form) :entries))
           (ht (make-hash-table :test (or test 'equal))))
      (dolist (pair entries ht)
        (let ((rebuilt (funcall from-plist-fn (cdr pair))))
          (when rebuilt
            (setf (gethash (car pair) ht) rebuilt)))))))

(defun serialize-causal-model (cm)
  "Serialize the causal model hash-table (values are causal-link structs or lists)."
  (when (and cm (hash-table-p cm))
    (let ((entries nil)
          (test (hash-table-test cm)))
      (maphash (lambda (k v)
                 (push (cons k
                             (cond ((causal-link-p v) (causal-link-to-plist v))
                                   ((and (listp v) (causal-link-p (first v)))
                                    (mapcar #'causal-link-to-plist v))
                                   (t v)))
                       entries))
               cm)
      (list :hash-table :test test :entries entries))))

(defun deserialize-causal-model (form)
  "Rebuild causal model from serialized form."
  (when (and form (listp form) (eq (first form) :hash-table))
    (let* ((test (getf (rest form) :test))
           (entries (getf (rest form) :entries))
           (ht (make-hash-table :test (or test 'equal))))
      (dolist (pair entries ht)
        (setf (gethash (car pair) ht)
              (let ((v (cdr pair)))
                (cond ((and (listp v) (eq (first v) :id))
                       ;; Single causal-link plist
                       (plist-to-causal-link v))
                      ((and (listp v) (listp (first v)) (eq (first (first v)) :id))
                       ;; List of causal-link plists
                       (mapcar #'plist-to-causal-link v))
                      (t v))))))))

(defun collect-parameters ()
  "Collect all self-tunable threshold parameters."
  (let ((params nil))
    (flet ((save-param (name)
             (when (boundp name)
               (push (cons name (symbol-value name)) params))))
      (save-param '*sparse-threshold*)
      (save-param '*death-threshold*)
      (save-param '*metabolism*)
      (save-param '*merge-similarity-threshold*)
      (save-param '*pattern-prune-threshold*)
      (save-param '*pattern-decay-rate*)
      (save-param '*improvement-threshold*)
      (save-param '*neighborhood-min-similarity*)
      (save-param '*neighborhood-diversity-threshold*)
      (save-param '*attention-decay*)
      (save-param '*attention-window*)
      (save-param '*attention-focus-threshold*)
      (save-param '*type-cluster-threshold*)
      (save-param '*type-context-weight*)
      (save-param '*traveler-bridge-threshold*)
      (save-param '*traveler-knowledge-overlap-threshold*)
      (save-param '*holo-similarity-threshold*)
      (save-param '*schema-spawn-rate*))
    params))

(defun serialize-trace-buffer (buf &optional (max-entries 200))
  "Serialize a trace buffer (fill-pointer array of cognitive-trace structs)."
  (when (and buf (arrayp buf) (array-has-fill-pointer-p buf))
    (let* ((fp (fill-pointer buf))
           (start (max 0 (- fp max-entries)))
           (entries nil))
      (loop for i from start below fp
            do (let ((item (aref buf i)))
                 (push (if (cognitive-trace-p item)
                           (cognitive-trace-to-plist item)
                           item)
                       entries)))
      (list :fill-pointer (- fp start)
            :contents (nreverse entries)))))

(defun deserialize-trace-buffer (form &optional (capacity 1000))
  "Rebuild a trace buffer from serialized form."
  (when (and form (listp form) (eq (first form) :fill-pointer))
    (let* ((contents (getf form :contents))
           (arr (make-array capacity :fill-pointer 0 :adjustable t)))
      (dolist (item contents arr)
        (vector-push-extend
         (if (and (listp item) (member :step item))
             (plist-to-cognitive-trace item)
             item)
         arr)))))

(defun collect-full-state (&optional reason)
  "Gather ALL mutable global state into a system-snapshot struct."
  (let ((snap (make-system-snapshot
               :timestamp (get-universal-time)
               :reason reason
               ;; Counters
               :step (if (boundp '*step*) *step* 0)
               :expert-counter (if (boundp '*expert-counter*) *expert-counter* 0)
               :traveler-counter (if (boundp '*traveler-counter*) *traveler-counter* 0)
               :type-counter (if (boundp '*type-counter*) *type-counter* 0)
               :total-births (if (boundp '*total-births*) *total-births* 0)
               :total-deaths (if (boundp '*total-deaths*) *total-deaths* 0))))

    ;; Experts
    (when (and (boundp '*experts*) *experts*)
      (setf (system-snapshot-experts snap)
            (mapcar #'expert-to-plist *experts*)))

    ;; Neighborhoods (store plists with ID refs)
    (when (and (boundp '*neighborhoods*) *neighborhoods*)
      (setf (system-snapshot-neighborhoods snap)
            (mapcar #'neighborhood-to-plist *neighborhoods*)))
    (when (and (boundp '*root-neighborhood*) *root-neighborhood*
               (neighborhood-p *root-neighborhood*))
      (setf (system-snapshot-root-neighborhood-id snap)
            (neighborhood-id *root-neighborhood*)))

    ;; Travelers
    (when (and (boundp '*travelers*) *travelers*)
      (setf (system-snapshot-travelers snap)
            (mapcar #'traveler-to-plist *travelers*)))

    ;; Long-term memory
    (when (and (boundp '*long-term-memory*) (hash-table-p *long-term-memory*))
      (setf (system-snapshot-long-term-memory snap)
            (hash-table-to-serializable *long-term-memory*)))

    ;; Hypotheses
    (when (and (boundp '*hypotheses*) (hash-table-p *hypotheses*))
      (setf (system-snapshot-hypotheses snap)
            (serialize-hash-of-structs *hypotheses* #'self-hypothesis-to-plist)))

    ;; Cognitive schemas
    (when (and (boundp '*cognitive-schemas*) (hash-table-p *cognitive-schemas*))
      (setf (system-snapshot-cognitive-schemas snap)
            (serialize-hash-of-structs *cognitive-schemas* #'cognitive-schema-to-plist)))

    ;; Executable schemas
    (when (and (boundp '*executable-schemas*) (hash-table-p *executable-schemas*))
      (setf (system-snapshot-executable-schemas snap)
            (serialize-hash-of-structs *executable-schemas* #'executable-schema-to-plist)))

    ;; Holo-store
    (when (and (boundp '*holo*) *holo* (holo-store-p *holo*))
      (setf (system-snapshot-holo snap) (holo-store-to-plist *holo*)))

    ;; Holographic-memory
    (when (and (boundp '*holographic-memory*) *holographic-memory*
               (holographic-memory-p *holographic-memory*))
      (setf (system-snapshot-holographic-memory snap)
            (holographic-memory-to-plist *holographic-memory*)))

    ;; Presence
    (when (and (boundp '*presence*) *presence* (presence-p *presence*))
      (setf (system-snapshot-presence snap) (presence-to-plist *presence*)))

    ;; Self-model
    (when (and (boundp '*self-model*) *self-model* (self-model-p *self-model*))
      (setf (system-snapshot-self-model snap) (self-model-to-plist *self-model*)))

    ;; Dream state
    (when (and (boundp '*dream-state*) *dream-state* (dream-state-p *dream-state*))
      (setf (system-snapshot-dream-state snap) (dream-state-to-plist *dream-state*)))
    (when (boundp '*dreaming*)
      (setf (system-snapshot-dreaming snap) *dreaming*))

    ;; Episodic memory
    (when (and (boundp '*episodic-memory*) *episodic-memory*
               (episodic-memory-p *episodic-memory*))
      (setf (system-snapshot-episodic-memory snap)
            (episodic-memory-to-plist *episodic-memory*)))

    ;; Causal model
    (when (and (boundp '*causal-model*) (hash-table-p *causal-model*))
      (setf (system-snapshot-causal-model snap)
            (serialize-causal-model *causal-model*)))

    ;; Trace buffers (capped at 200)
    (when (and (boundp '*trace-buffer*) *trace-buffer*
               (arrayp *trace-buffer*) (array-has-fill-pointer-p *trace-buffer*))
      (setf (system-snapshot-trace-buffer snap)
            (serialize-trace-buffer *trace-buffer* 200)))
    (when (and (boundp '*cognitive-trace-buffer*) *cognitive-trace-buffer*
               (arrayp *cognitive-trace-buffer*)
               (array-has-fill-pointer-p *cognitive-trace-buffer*))
      (setf (system-snapshot-cognitive-trace-buffer snap)
            (serialize-trace-buffer *cognitive-trace-buffer* 200)))

    ;; Goals
    (when (and (boundp '*goal-stack*) *goal-stack*)
      (setf (system-snapshot-goal-stack snap)
            (mapcar #'goal-to-plist *goal-stack*)))
    (when (and (boundp '*current-goal*) *current-goal* (goal-p *current-goal*))
      (setf (system-snapshot-current-goal snap)
            (goal-to-plist *current-goal*)))
    (when (boundp '*goal-history*)
      (setf (system-snapshot-goal-history snap) *goal-history*))

    ;; History
    (when (boundp '*hypothesis-history*)
      (setf (system-snapshot-hypothesis-history snap) *hypothesis-history*))
    (when (boundp '*modification-history*)
      (setf (system-snapshot-modification-history snap) *modification-history*))

    ;; Self-expectations (fill-pointer array, cap at 200)
    (when (and (boundp '*self-expectations*) *self-expectations*
               (arrayp *self-expectations*)
               (array-has-fill-pointer-p *self-expectations*))
      (setf (system-snapshot-self-expectations snap)
            (serialize-fill-pointer-array *self-expectations* 200)))
    (when (boundp '*self-expectation-accuracy*)
      (setf (system-snapshot-self-expectation-accuracy snap)
            (coerce *self-expectation-accuracy* 'single-float)))

    ;; Self-doubt
    (when (and (boundp '*self-doubt*) *self-doubt* (self-doubt-model-p *self-doubt*))
      (setf (system-snapshot-self-doubt snap)
            (self-doubt-model-to-plist *self-doubt*)))

    ;; Attention
    (when (and (boundp '*attention-map*) (hash-table-p *attention-map*))
      (setf (system-snapshot-attention-map snap)
            (hash-table-to-serializable *attention-map*)))
    (when (and (boundp '*attention-state*) *attention-state*
               (attention-state-p *attention-state*))
      (setf (system-snapshot-attention-state snap)
            (attention-state-to-plist *attention-state*)))

    ;; Introspective concepts
    (when (and (boundp '*introspective-concepts*) (hash-table-p *introspective-concepts*))
      (setf (system-snapshot-introspective-concepts snap)
            (hash-table-to-serializable *introspective-concepts*)))
    (when (boundp '*cached-active-concepts*)
      (setf (system-snapshot-cached-active-concepts snap) *cached-active-concepts*))

    ;; Working memory
    (when (and (boundp '*working-memory*) *working-memory*
               (working-memory-p *working-memory*))
      (setf (system-snapshot-working-memory snap)
            (working-memory-to-plist *working-memory*)))

    ;; Intrinsic drives
    (when (boundp '*intrinsic-drives*)
      (setf (system-snapshot-intrinsic-drives snap) *intrinsic-drives*))

    ;; Recent/external outcomes
    (when (boundp '*recent-outcomes*)
      (setf (system-snapshot-recent-outcomes snap) *recent-outcomes*))
    (when (boundp '*external-outcomes*)
      (setf (system-snapshot-external-outcomes snap) *external-outcomes*))

    ;; Self-modification history
    (when (boundp '*self-modification-history*)
      (setf (system-snapshot-self-modification-history snap)
            *self-modification-history*))

    ;; Parameters
    (setf (system-snapshot-parameters snap) (collect-parameters))

    snap))

;;;; [SECTION-END:5:STATE-COLLECTION]

;;;; [SECTION-START:6:RESTORE]
;;; Section 6: Restore from snapshot

(defun rebuild-derived-state! ()
  "Rebuild derived/cached state that is not saved. Called after restore."
  ;; Clear caches that will be rebuilt on demand
  (when (boundp '*embeddings*)
    (clrhash *embeddings*))
  (when (boundp '*context-owners*)
    (clrhash *context-owners*))
  (when (boundp '*context-encounters*)
    (clrhash *context-encounters*))
  (when (boundp '*pattern-stats*)
    (clrhash *pattern-stats*))
  ;; Reset cached concept state
  (when (boundp '*cached-active-concepts*)
    (setf *cached-active-concepts* nil))
  ;; Clear hook fire counts (hooks re-register on module load)
  (when (and (boundp '*hook-fire-counts*) (hash-table-p *hook-fire-counts*))
    (clrhash *hook-fire-counts*))
  (format t "[SAVE-RESTORE] Derived state rebuilt.~%"))

(defun restore-parameters! (params)
  "Restore saved parameter values."
  (dolist (pair params)
    (when (and (boundp (car pair)) (cdr pair))
      (setf (symbol-value (car pair)) (cdr pair)))))

(defun restore-from-snapshot! (snap)
  "Install a system-snapshot as current state. Two-pass for neighborhoods."
  (unless (system-snapshot-p snap)
    (error "restore-from-snapshot!: argument is not a system-snapshot"))

  (format t "[SAVE-RESTORE] Restoring state from snapshot (step ~D)...~%"
          (system-snapshot-step snap))

  ;; Counters
  (when (boundp '*step*) (setf *step* (system-snapshot-step snap)))
  (when (boundp '*expert-counter*)
    (setf *expert-counter* (system-snapshot-expert-counter snap)))
  (when (boundp '*traveler-counter*)
    (setf *traveler-counter* (system-snapshot-traveler-counter snap)))
  (when (boundp '*type-counter*)
    (setf *type-counter* (system-snapshot-type-counter snap)))
  (when (boundp '*total-births*)
    (setf *total-births* (system-snapshot-total-births snap)))
  (when (boundp '*total-deaths*)
    (setf *total-deaths* (system-snapshot-total-deaths snap)))

  ;; Experts
  (when (boundp '*experts*)
    (setf *experts*
          (remove nil (mapcar #'plist-to-expert
                              (system-snapshot-experts snap)))))

  ;; Neighborhoods (two-pass)
  (when (boundp '*neighborhoods*)
    (let ((nbhd-plists (system-snapshot-neighborhoods snap))
          (nbhd-map (make-hash-table :test 'eq)))
      ;; Pass 1: create all neighborhoods
      (setf *neighborhoods*
            (remove nil
                    (mapcar (lambda (pl)
                              (let ((n (plist-to-neighborhood pl)))
                                (when n
                                  (setf (gethash (neighborhood-id n) nbhd-map) n))
                                n))
                            nbhd-plists)))
      ;; Pass 2: reconnect parent/children
      (reconnect-neighborhoods! nbhd-plists nbhd-map)
      ;; Restore root
      (when (boundp '*root-neighborhood*)
        (setf *root-neighborhood*
              (gethash (system-snapshot-root-neighborhood-id snap) nbhd-map)))))

  ;; Travelers
  (when (boundp '*travelers*)
    (setf *travelers*
          (remove nil (mapcar #'plist-to-traveler
                              (system-snapshot-travelers snap)))))

  ;; Long-term memory
  (when (boundp '*long-term-memory*)
    (let ((ltm (serializable-to-hash-table (system-snapshot-long-term-memory snap))))
      (if ltm
          (setf *long-term-memory* ltm)
          (clrhash *long-term-memory*))))

  ;; Hypotheses
  (when (boundp '*hypotheses*)
    (let ((h (deserialize-hash-of-structs
              (system-snapshot-hypotheses snap) #'plist-to-self-hypothesis)))
      (if h
          (setf *hypotheses* h)
          (clrhash *hypotheses*))))

  ;; Cognitive schemas
  (when (boundp '*cognitive-schemas*)
    (let ((cs (deserialize-hash-of-structs
               (system-snapshot-cognitive-schemas snap) #'plist-to-cognitive-schema)))
      (if cs
          (setf *cognitive-schemas* cs)
          (when (hash-table-p *cognitive-schemas*) (clrhash *cognitive-schemas*)))))

  ;; Executable schemas
  (when (boundp '*executable-schemas*)
    (let ((es (deserialize-hash-of-structs
               (system-snapshot-executable-schemas snap) #'plist-to-executable-schema)))
      (if es
          (setf *executable-schemas* es)
          (when (hash-table-p *executable-schemas*) (clrhash *executable-schemas*)))))

  ;; Holo-store
  (when (boundp '*holo*)
    (let ((hs (plist-to-holo-store (system-snapshot-holo snap))))
      (setf *holo* (or hs (make-holo-store)))))

  ;; Holographic memory
  (when (boundp '*holographic-memory*)
    (let ((hm (plist-to-holographic-memory (system-snapshot-holographic-memory snap))))
      (setf *holographic-memory* (or hm (make-holographic-memory)))))

  ;; Presence
  (when (boundp '*presence*)
    (setf *presence* (plist-to-presence (system-snapshot-presence snap))))

  ;; Self-model
  (when (boundp '*self-model*)
    (setf *self-model* (plist-to-self-model (system-snapshot-self-model snap))))

  ;; Dream state
  (when (boundp '*dream-state*)
    (let ((ds (plist-to-dream-state (system-snapshot-dream-state snap))))
      (setf *dream-state* (or ds (make-dream-state)))))
  (when (boundp '*dreaming*)
    (setf *dreaming* (system-snapshot-dreaming snap)))

  ;; Episodic memory
  (when (boundp '*episodic-memory*)
    (let ((em (plist-to-episodic-memory (system-snapshot-episodic-memory snap))))
      (setf *episodic-memory* (or em (make-episodic-memory)))))

  ;; Causal model
  (when (boundp '*causal-model*)
    (let ((cm (deserialize-causal-model (system-snapshot-causal-model snap))))
      (if cm
          (setf *causal-model* cm)
          (when (hash-table-p *causal-model*) (clrhash *causal-model*)))))

  ;; Trace buffers
  (when (boundp '*trace-buffer*)
    (let ((tb (deserialize-trace-buffer (system-snapshot-trace-buffer snap) 500)))
      (setf *trace-buffer* (or tb (make-array 500 :fill-pointer 0 :adjustable t)))))
  (when (boundp '*cognitive-trace-buffer*)
    (let ((ctb (deserialize-trace-buffer
                (system-snapshot-cognitive-trace-buffer snap) 1000)))
      (setf *cognitive-trace-buffer*
            (or ctb (make-array 1000 :fill-pointer 0 :adjustable t)))))

  ;; Goals
  (when (boundp '*goal-stack*)
    (setf *goal-stack*
          (remove nil (mapcar #'plist-to-goal (system-snapshot-goal-stack snap)))))
  (when (boundp '*current-goal*)
    (setf *current-goal* (plist-to-goal (system-snapshot-current-goal snap))))
  (when (boundp '*goal-history*)
    (setf *goal-history* (system-snapshot-goal-history snap)))

  ;; History
  (when (boundp '*hypothesis-history*)
    (setf *hypothesis-history* (system-snapshot-hypothesis-history snap)))
  (when (boundp '*modification-history*)
    (setf *modification-history* (system-snapshot-modification-history snap)))

  ;; Self-expectations
  (when (boundp '*self-expectations*)
    (let ((se (deserialize-fill-pointer-array
               (system-snapshot-self-expectations snap) 200)))
      (setf *self-expectations* (or se (make-array 200 :fill-pointer 0 :adjustable t)))))
  (when (boundp '*self-expectation-accuracy*)
    (setf *self-expectation-accuracy*
          (system-snapshot-self-expectation-accuracy snap)))

  ;; Self-doubt
  (when (boundp '*self-doubt*)
    (let ((sd (plist-to-self-doubt-model (system-snapshot-self-doubt snap))))
      (setf *self-doubt* (or sd (make-self-doubt-model)))))

  ;; Attention
  (when (boundp '*attention-map*)
    (let ((am (serializable-to-hash-table (system-snapshot-attention-map snap))))
      (if am
          (setf *attention-map* am)
          (when (hash-table-p *attention-map*) (clrhash *attention-map*)))))
  (when (boundp '*attention-state*)
    (let ((as (plist-to-attention-state (system-snapshot-attention-state snap))))
      (setf *attention-state* (or as (make-attention-state)))))

  ;; Introspective concepts
  (when (boundp '*introspective-concepts*)
    (let ((ic (serializable-to-hash-table
               (system-snapshot-introspective-concepts snap))))
      (if ic
          (setf *introspective-concepts* ic)
          (when (hash-table-p *introspective-concepts*)
            (clrhash *introspective-concepts*)))))
  (when (boundp '*cached-active-concepts*)
    (setf *cached-active-concepts*
          (system-snapshot-cached-active-concepts snap)))

  ;; Working memory
  (when (boundp '*working-memory*)
    (let ((wm (plist-to-working-memory (system-snapshot-working-memory snap))))
      (setf *working-memory* (or wm (make-working-memory)))))

  ;; Intrinsic drives
  (when (boundp '*intrinsic-drives*)
    (setf *intrinsic-drives* (system-snapshot-intrinsic-drives snap)))

  ;; Outcomes
  (when (boundp '*recent-outcomes*)
    (setf *recent-outcomes* (system-snapshot-recent-outcomes snap)))
  (when (boundp '*external-outcomes*)
    (setf *external-outcomes* (system-snapshot-external-outcomes snap)))

  ;; Self-modification history
  (when (boundp '*self-modification-history*)
    (setf *self-modification-history*
          (system-snapshot-self-modification-history snap)))

  ;; Parameters
  (restore-parameters! (system-snapshot-parameters snap))

  ;; Rebuild derived state
  (rebuild-derived-state!)

  (format t "[SAVE-RESTORE] State restored. Step=~D, Experts=~D, LTM=~D~%"
          *step* (length *experts*)
          (if (and (boundp '*long-term-memory*) (hash-table-p *long-term-memory*))
              (hash-table-count *long-term-memory*) 0))
  t)

;;;; [SECTION-END:6:RESTORE]

;;;; [SECTION-START:7:ROLLBACK-API]
;;; Section 7: In-memory rollback API

(defun snapshot-for-rollback! (&optional reason)
  "Push a snapshot onto the rollback stack. Returns the snapshot."
  (let ((snap (collect-full-state (or reason :rollback-point))))
    ;; Enforce max snapshots
    (when (>= (length *snapshot-stack*) *max-snapshots-in-memory*)
      (setf *snapshot-stack* (butlast *snapshot-stack*)))
    (push snap *snapshot-stack*)
    (format t "[SAVE-RESTORE] Snapshot taken (reason: ~A). Stack depth: ~D~%"
            (or reason :rollback-point) (length *snapshot-stack*))
    snap))

(defun rollback-to-snapshot! (&optional which)
  "Pop and restore the most recent snapshot (or the nth one if WHICH is given)."
  (unless *snapshot-stack*
    (format t "[SAVE-RESTORE] No snapshots available for rollback.~%")
    (return-from rollback-to-snapshot! nil))
  (let ((snap (if (and which (integerp which) (< which (length *snapshot-stack*)))
                  (nth which *snapshot-stack*)
                  (first *snapshot-stack*))))
    ;; Remove the snapshot and everything above it from stack
    (setf *snapshot-stack*
          (if (and which (integerp which))
              (nthcdr (1+ which) *snapshot-stack*)
              (rest *snapshot-stack*)))
    (format t "[SAVE-RESTORE] Rolling back to snapshot (step ~D, reason: ~A)~%"
            (system-snapshot-step snap) (system-snapshot-reason snap))
    (restore-from-snapshot! snap)
    t))

(defun peek-snapshot (&optional (n 0))
  "View the nth snapshot on the stack without popping it."
  (when (< n (length *snapshot-stack*))
    (let ((snap (nth n *snapshot-stack*)))
      (format t "[SAVE-RESTORE] Snapshot ~D: step=~D, reason=~A, time=~A~%"
              n (system-snapshot-step snap)
              (system-snapshot-reason snap)
              (system-snapshot-timestamp snap))
      snap)))

(defun clear-snapshots! ()
  "Clear all in-memory snapshots."
  (let ((count (length *snapshot-stack*)))
    (setf *snapshot-stack* nil)
    (format t "[SAVE-RESTORE] Cleared ~D snapshots.~%" count)
    count))

;;;; [SECTION-END:7:ROLLBACK-API]

;;;; [SECTION-START:8:DISK-PERSISTENCE]
;;; Section 8: Disk persistence API

(defun ensure-save-directory ()
  "Ensure the state save directory exists."
  (ensure-directories-exist *state-save-directory*)
  *state-save-directory*)

(defun generate-save-filename (&optional tag)
  "Generate a unique save filename with timestamp and optional tag."
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time))
    (let ((base (format nil "uhma-full-state-~4,'0D~2,'0D~2,'0D-~2,'0D~2,'0D~2,'0D"
                        year month day hour min sec)))
      (when tag
        (setf base (format nil "~A-~A" base tag)))
      (merge-pathnames (format nil "~A.lisp" base)
                       (ensure-save-directory)))))

(defun save-full-state! (&optional tag)
  "Save the complete system state to disk. Returns the path written."
  (let* ((snap (collect-full-state (or tag :disk-save)))
         (path (generate-save-filename tag)))
    (format t "[SAVE-RESTORE] Saving full state to ~A...~%" path)
    (with-open-file (out path :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
      (let ((*print-readably* t)
            (*print-circle* t)
            (*print-length* nil)
            (*print-level* nil)
            (*package* (find-package :uhma)))
        ;; Write header comment (not part of readable form)
        (format out ";;; UHMA Full State Save~%")
        (format out ";;; Version: ~D~%" +save-restore-version+)
        (format out ";;; Step: ~D~%" (system-snapshot-step snap))
        (format out ";;; Timestamp: ~D~%" (system-snapshot-timestamp snap))
        (format out ";;; Experts: ~D~%" (length (system-snapshot-experts snap)))
        (format out ";;; Tag: ~A~%" (or tag "none"))
        (format out "~%")
        ;; Write the snapshot as a single readable form
        (prin1 (list :save-restore-version +save-restore-version+
                     :snapshot snap)
               out)
        (terpri out)))
    (format t "[SAVE-RESTORE] State saved to ~A (~D experts, step ~D)~%"
            path
            (length (system-snapshot-experts snap))
            (system-snapshot-step snap))
    path))

(defun restore-full-state! (path)
  "Restore system state from a disk file."
  (unless (probe-file path)
    (error "restore-full-state!: file not found: ~A" path))
  (format t "[SAVE-RESTORE] Loading state from ~A...~%" path)
  (let ((data (with-open-file (in path :direction :input)
                (let ((*package* (find-package :uhma))
                      (*read-eval* nil))
                  (read in)))))
    (unless (and (listp data) (eq (getf data :save-restore-version) +save-restore-version+))
      (warn "[SAVE-RESTORE] Version mismatch or invalid format. Attempting restore anyway."))
    (let ((snap (getf data :snapshot)))
      (unless (system-snapshot-p snap)
        (error "restore-full-state!: no valid snapshot in file"))
      (restore-from-snapshot! snap)
      (format t "[SAVE-RESTORE] Restore complete from disk.~%")
      t)))

(defun list-saved-states ()
  "List all available saved state files."
  (ensure-save-directory)
  (let ((files (directory (merge-pathnames "uhma-full-state-*.lisp"
                                           *state-save-directory*))))
    (if files
        (progn
          (format t "[SAVE-RESTORE] Available states:~%")
          (dolist (f (sort files #'> :key #'file-write-date))
            (format t "  ~A~%" (namestring f)))
          files)
        (progn
          (format t "[SAVE-RESTORE] No saved states found in ~A~%"
                  *state-save-directory*)
          nil))))

;;;; [SECTION-END:8:DISK-PERSISTENCE]

;;;; [SECTION-START:9:VERIFICATION]
;;; Section 9: Verification

(defun verify-state-integrity (&optional reference-snapshot)
  "Compare current state against a snapshot and report discrepancies.
If no reference is given, takes a fresh snapshot and compares key metrics."
  (let ((ref (or reference-snapshot (collect-full-state :verification)))
        (discrepancies nil))
    (flet ((check (name expected actual)
             (unless (equal expected actual)
               (push (list name :expected expected :actual actual) discrepancies))))

      ;; Check counters
      (check :step (system-snapshot-step ref)
             (if (boundp '*step*) *step* 0))
      (check :expert-counter (system-snapshot-expert-counter ref)
             (if (boundp '*expert-counter*) *expert-counter* 0))
      (check :expert-count (length (system-snapshot-experts ref))
             (if (boundp '*experts*) (length *experts*) 0))
      (check :traveler-count (length (system-snapshot-travelers ref))
             (if (boundp '*travelers*) (length *travelers*) 0))
      (check :neighborhood-count (length (system-snapshot-neighborhoods ref))
             (if (boundp '*neighborhoods*) (length *neighborhoods*) 0))
      (check :total-births (system-snapshot-total-births ref)
             (if (boundp '*total-births*) *total-births* 0))
      (check :total-deaths (system-snapshot-total-deaths ref)
             (if (boundp '*total-deaths*) *total-deaths* 0))

      ;; Check LTM size
      (let ((ref-ltm-size (if (system-snapshot-long-term-memory ref)
                              (length (getf (rest (system-snapshot-long-term-memory ref))
                                            :entries))
                              0))
            (cur-ltm-size (if (and (boundp '*long-term-memory*)
                                   (hash-table-p *long-term-memory*))
                              (hash-table-count *long-term-memory*) 0)))
        (check :ltm-size ref-ltm-size cur-ltm-size))

      ;; Check hypothesis count
      (let ((ref-hyp-count (if (system-snapshot-hypotheses ref)
                               (length (getf (rest (system-snapshot-hypotheses ref))
                                             :entries))
                               0))
            (cur-hyp-count (if (and (boundp '*hypotheses*)
                                    (hash-table-p *hypotheses*))
                               (hash-table-count *hypotheses*) 0)))
        (check :hypothesis-count ref-hyp-count cur-hyp-count)))

    ;; Report
    (if discrepancies
        (progn
          (format t "[SAVE-RESTORE] INTEGRITY CHECK: ~D discrepancies found:~%"
                  (length discrepancies))
          (dolist (d discrepancies)
            (format t "  ~A: expected=~A, actual=~A~%"
                    (first d) (getf (rest d) :expected) (getf (rest d) :actual)))
          (values nil discrepancies))
        (progn
          (format t "[SAVE-RESTORE] INTEGRITY CHECK: All metrics match.~%")
          (values t nil)))))

;;;; [SECTION-END:9:VERIFICATION]

;;;; [SECTION-START:10:INTEGRATION]
;;; Section 10: Integration & Init

;;; Auto-save on heap pressure (hooks into maintenance cycle)
(defun save-restore-maintenance-hook ()
  "Auto-save when heap usage exceeds threshold."
  (when (and (boundp '*heap-limit-bytes*) *heap-limit-bytes*
             (boundp '*step*) (> *step* 0))
    (let* ((usage-fn (or (find-symbol "DYNAMIC-USAGE" "SB-KERNEL")
                         (find-symbol "DYNAMIC-SPACE-USAGE" "SB-EXT")))
           (heap-usage (if (and usage-fn (fboundp usage-fn))
                           (funcall usage-fn)
                           0)))
      (when (> heap-usage (* 0.9 *heap-limit-bytes*))
        (format t "[SAVE-RESTORE] Heap pressure detected (~D bytes). Auto-saving...~%"
                heap-usage)
        (save-full-state! "auto-heap-pressure")))))

;; Register with maintenance hook if available
(when (and (boundp '+hook-maintenance+) (fboundp 'register-hook))
  (register-hook +hook-maintenance+ 'save-restore-maintenance-hook :priority 99))

;;; Compatibility aliases
(defun save-system-state! (&optional tag)
  "Compatibility alias for save-full-state!."
  (save-full-state! tag))

;;; Replace revert-modification! to use snapshot-based rollback
(defun revert-modification! (mod-entry)
  "Revert a previous modification. Uses snapshot rollback if available,
falls back to recording the revert."
  (when (and mod-entry (listp mod-entry))
    (let ((action (getf mod-entry :action)))
      (when action
        ;; Try snapshot rollback first
        (if *snapshot-stack*
            (progn
              (format t "[SAVE-RESTORE] Reverting via snapshot rollback.~%")
              (rollback-to-snapshot!))
            ;; Fallback: just record the revert
            (when (boundp '*modification-history*)
              (push (list :step (if (boundp '*step*) *step* 0)
                          :action :revert
                          :original-action action
                          :reason :performance-regression)
                    *modification-history*)))
        t))))

;;;; [SECTION-END:10:INTEGRATION]

;;; Load confirmation
(format t "~%[SAVE-RESTORE] uhma-save-restore.lisp loaded (v~D).~%"
        +save-restore-version+)
(format t "[SAVE-RESTORE] API:~%")
(format t "  (snapshot-for-rollback! &optional reason) - take snapshot~%")
(format t "  (rollback-to-snapshot! &optional which)   - restore snapshot~%")
(format t "  (save-full-state! &optional tag)          - save to disk~%")
(format t "  (restore-full-state! path)                - restore from disk~%")
(format t "  (verify-state-integrity &optional ref)    - check consistency~%")
(format t "  (list-saved-states)                       - show saved files~%")
