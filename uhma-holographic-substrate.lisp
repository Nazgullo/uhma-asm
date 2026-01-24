
;;;; FILE MANIFEST
;;;; ============
;;;; This file MUST contain the following when complete:
;;;; PACKAGES: uhma
;;;; FUNCTIONS: compute-all-expert-activations (and others)
;;;; TOTAL EXPECTED SECTIONS: Unknown (Legacy File)

;;;; ============================================================================
;;;; UHMA HOLOGRAPHIC SUBSTRATE
;;;; ============================================================================
;;;;
;;;; Unifies memory as holographic distributed patterns with sparse firing.
;;;;
;;;; KEY INSIGHT: Experts ARE the holographic substrate. Every pattern is
;;;; encoded as a sparse activation across ALL experts. Memory is not stored
;;;; IN experts but BETWEEN them - in the pattern of co-activation.
;;;;
;;;; USES EXISTING:
;;;;   - context-embedding (uhma-v6.1-core-homoiconic.lisp:325)
;;;;   - cosim (uhma-v6.1-core-homoiconic.lisp:308)
;;;;   - expert-centroid (expert struct field)
;;;;   - *recent-outcomes*, *situation-history*, *episodic-memory*, *long-term-memory*
;;;;   - presence system (uhma-presence-substrate.lisp)
;;;;
;;;; ADDS:
;;;;   - Sparse top-k expert activation
;;;;   - Holographic pattern encoding
;;;;   - Unified memory view with decay policies
;;;;   - Presence-gated sparsity control
;;;;
;;;; ============================================================================

(in-package :uhma)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize (speed 0) (safety 3) (debug 3))))


;;; ============================================================================
;;; CONFIGURATION
;;; ============================================================================

(defparameter *holographic-enabled* t
  "Master switch for holographic substrate. The VSA interconnection layer IS the neural network.")

(defparameter *sparse-k-default* 5
  "Default number of experts that fire for each input.")

(defparameter *sparse-k-min* 2
  "Minimum experts that fire (even when highly focused).")

(defparameter *sparse-k-max* 15
  "Maximum experts that fire (even when exploring).")

(defparameter *sparse-activation-threshold* 0.1
  "Minimum activation to consider an expert 'firing'.")

(defparameter *holographic-decay-enabled* t
  "Whether to apply time-based decay to holographic patterns.")

;;; ============================================================================
;;; PART 1: SPARSE EXPERT ACTIVATION
;;; ============================================================================
;;; Compute activation for ALL experts, keep only top-k.

(defun compute-all-expert-activations (ctx)
  "Compute activation score for every expert given context.
   Uses existing context-embedding and cosim.
   Returns sorted list of (expert . activation)."
  (when (and ctx *experts*)
    (let* ((ctx-emb (context-embedding ctx))
           (activations nil))
      ;; Score all experts
      (dolist (expert *experts*)
        (when (and (> (expert-life expert) 0.1)
                   (expert-centroid expert))
          (let* ((sim (cosim ctx-emb (expert-centroid expert)))
                 ;; Boost by expert reliability
                 (reliability (/ (expert-hits expert)
                                 (max 1 (+ (expert-hits expert)
                                          (expert-misses expert)))))
                 ;; Combined activation
                 (activation (* sim (+ 0.5 (* 0.5 reliability)))))
            (when (> activation *sparse-activation-threshold*)
              (push (cons expert activation) activations)))))
      ;; Sort by activation descending
      (sort activations #'> :key #'cdr))))

(defun sparse-select-top-k (activations k)
  "Select top-k activations from sorted activation list.
   Returns list of (expert . activation)."
  (subseq activations 0 (min k (length activations))))

(defun compute-sparse-k ()
  "Compute k based on presence state.
   More focused = lower k (fewer experts fire).
   Exploring = higher k (more experts contribute)."
  (if (and (boundp '*presence*) *presence*)
      (let* ((trajectory (presence-trajectory *presence*))
             (self-conf (presence-self-confidence *presence*))
             (base-k *sparse-k-default*))
        (cond
          ;; Flowing confidently - low k, trust few experts
          ((and (eq trajectory :flowing) (> self-conf 0.7))
           (max *sparse-k-min* (floor base-k 2)))
          ;; Stuck or searching - high k, consider more options
          ((member trajectory '(:stuck :searching))
           (min *sparse-k-max* (* base-k 2)))
          ;; Exploring - medium-high k
          ((eq trajectory :exploring)
           (min *sparse-k-max* (ceiling (* base-k 1.5))))
          ;; Default
          (t base-k)))
      *sparse-k-default*))

(defun sparse-expert-selection (ctx)
  "Main entry: select which experts fire for this context.
   Returns list of (expert . activation)."
  (when *holographic-enabled*
    (let* ((all-activations (compute-all-expert-activations ctx))
           (k (compute-sparse-k)))
      (sparse-select-top-k all-activations k))))

;;; ============================================================================
;;; PART 2: HOLOGRAPHIC PATTERN ENCODING
;;; ============================================================================
;;; A pattern is encoded as a sparse activation vector across experts.

(defstruct holographic-pattern
  "A memory encoded as sparse expert activations."
  (id (gensym "HP-"))
  (activations nil)          ; alist of (expert-id . activation)
  (sparsity 0)               ; how many experts active
  (content nil)              ; original content (for debugging)
  (layer :immediate)         ; memory layer this belongs to
  (strength 1.0)             ; current strength (decays over time)
  (access-count 0)           ; times retrieved
  (created-step 0)
  (last-accessed-step 0))

(defun holographic-encode (content &optional ctx)
  "Encode content as holographic pattern.
   Uses sparse expert activations as the encoding."
  (let* ((context (or ctx (if (listp content) content (list content))))
         (firing (sparse-expert-selection context))
         (activation-alist (loop for (expert . act) in firing
                                 collect (cons (expert-id expert) act))))
    (make-holographic-pattern
     :activations activation-alist
     :sparsity (length activation-alist)
     :content content
     :created-step (if (boundp '*step*) *step* 0))))

(defun holographic-similarity (pattern1 pattern2)
  "Compute similarity between two holographic patterns.
   Based on overlap of expert activations."
  (let ((a1 (holographic-pattern-activations pattern1))
        (a2 (holographic-pattern-activations pattern2))
        (dot 0.0) (mag1 0.0) (mag2 0.0))
    ;; Dot product of activation vectors
    (dolist (e1 a1)
      (incf mag1 (* (cdr e1) (cdr e1)))
      (let ((e2-entry (assoc (car e1) a2)))
        (when e2-entry
          (incf dot (* (cdr e1) (cdr e2-entry))))))
    (dolist (e2 a2)
      (incf mag2 (* (cdr e2) (cdr e2))))
    ;; Cosine similarity
    (if (or (zerop mag1) (zerop mag2))
        0.0
        (/ dot (sqrt (* mag1 mag2))))))

;;; ============================================================================
;;; PART 3: HOLOGRAPHIC MEMORY STORE
;;; ============================================================================
;;; Stores holographic patterns with layer-based organization.

(defstruct holographic-memory
  "The holographic memory substrate."
  (patterns (make-hash-table :test 'equal))  ; signature → pattern
  (by-layer (make-hash-table :test 'eq))     ; layer → list of patterns
  (count 0))

(defvar *holographic-memory* (make-holographic-memory)
  "The holographic memory store.")

(defun pattern-signature (pattern)
  "Create lookup signature for pattern (based on top activations)."
  (let ((top-3 (subseq (holographic-pattern-activations pattern)
                       0 (min 3 (length (holographic-pattern-activations pattern))))))
    (format nil "~{~A~^-~}" (mapcar #'car top-3))))

(defun holographic-store! (pattern &key (layer :immediate))
  "Store a holographic pattern."
  (setf (holographic-pattern-layer pattern) layer)
  (let ((sig (pattern-signature pattern)))
    ;; Add to signature index
    (push pattern (gethash sig (holographic-memory-patterns *holographic-memory*)))
    ;; Add to layer index
    (push pattern (gethash layer (holographic-memory-by-layer *holographic-memory*)))
    (incf (holographic-memory-count *holographic-memory*)))
  pattern)

(defun holographic-retrieve (query-pattern &key (threshold 0.5) (limit 10) layer)
  "Retrieve patterns similar to query pattern.
   LAYER: if specified, only search that layer; otherwise search all."
  (let ((results nil)
        (search-source (if layer
                          (list (cons layer (gethash layer
                                                     (holographic-memory-by-layer *holographic-memory*))))
                          (holographic-memory-by-layer *holographic-memory*))))
    (if layer
        ;; Search specific layer
        (dolist (hp (gethash layer (holographic-memory-by-layer *holographic-memory*)))
          (let ((sim (holographic-similarity query-pattern hp)))
            (when (> sim threshold)
              (incf (holographic-pattern-access-count hp))
              (setf (holographic-pattern-last-accessed-step hp)
                    (if (boundp '*step*) *step* 0))
              (incf (holographic-pattern-strength hp) 0.05)
              (push (cons sim hp) results))))
        ;; Search all layers
        (maphash
         (lambda (sig patterns)
           (declare (ignore sig))
           (dolist (hp patterns)
             (let ((sim (holographic-similarity query-pattern hp)))
               (when (> sim threshold)
                 (incf (holographic-pattern-access-count hp))
                 (setf (holographic-pattern-last-accessed-step hp)
                       (if (boundp '*step*) *step* 0))
                 (incf (holographic-pattern-strength hp) 0.05)
                 (push (cons sim hp) results)))))
         (holographic-memory-by-layer *holographic-memory*)))
    ;; Return sorted by similarity
    (mapcar #'cdr
            (subseq (sort results #'> :key #'car)
                    0 (min limit (length results))))))

(defun holographic-find-similar (query-pattern threshold)
  "Find patterns similar to query above threshold.
   Returns list of (pattern . similarity) pairs."
  (let ((results nil))
    (maphash
     (lambda (layer patterns)
       (declare (ignore layer))
       (dolist (hp patterns)
         (let ((sim (holographic-similarity query-pattern hp)))
           (when (> sim threshold)
             (push (cons hp sim) results)))))
     (holographic-memory-by-layer *holographic-memory*))
    (sort results #'> :key #'cdr)))

(defun holographic-retrieve-by-content (content &key (threshold 0.5) (limit 10) layer)
  "Convenience: retrieve by content (encodes first)."
  (holographic-retrieve (holographic-encode content)
                        :threshold threshold :limit limit :layer layer))

;;; ============================================================================
;;; PART 4: UNIFIED MEMORY VIEW
;;; ============================================================================
;;; Wrap existing memory systems as holographic views.

(defstruct memory-view
  "A view into memory with specific decay policy."
  (name nil)
  (decay-rate 0.0)           ; per-step strength decay
  (capacity nil)             ; max patterns (nil = unlimited)
  (source nil)               ; source variable (e.g., '*recent-outcomes*)
  (extractor nil))           ; function to extract content from source items

(defvar *memory-views*
  (list
   (make-memory-view :name :immediate
                     :decay-rate 0.1
                     :capacity 50
                     :source '*recent-outcomes*
                     :extractor #'car)
   (make-memory-view :name :working
                     :decay-rate 0.01
                     :capacity 200
                     :source '*situation-history*
                     :extractor (lambda (s) (when (situation-p s)
                                              (situation-context s))))
   (make-memory-view :name :episodic
                     :decay-rate 0.001
                     :capacity 500
                     :source '*episodic-memory*
                     :extractor nil)  ; handled specially
   (make-memory-view :name :semantic
                     :decay-rate 0.0  ; permanent
                     :capacity nil
                     :source '*long-term-memory*
                     :extractor nil))
  "Memory views with decay policies.")

(defun get-view (name)
  "Get memory view by name."
  (find name *memory-views* :key #'memory-view-name))

(defun holographic-decay-all! ()
  "Apply decay to all holographic patterns based on their layer."
  (when *holographic-decay-enabled*
    (maphash
     (lambda (layer patterns)
       (let* ((view (get-view layer))
              (decay-rate (if view (memory-view-decay-rate view) 0.01)))
         (when (> decay-rate 0)
           (setf (gethash layer (holographic-memory-by-layer *holographic-memory*))
                 (loop for hp in patterns
                       do (decf (holographic-pattern-strength hp) decay-rate)
                       when (> (holographic-pattern-strength hp) 0.1)
                       collect hp)))))
     (holographic-memory-by-layer *holographic-memory*))))

(defun holographic-query-all-layers (content &key (threshold 0.4) (limit 20))
  "Query across all memory layers, return unified results."
  (let* ((query-pattern (holographic-encode content))
         (results nil))
    ;; Query holographic store
    (dolist (match (holographic-retrieve query-pattern
                                         :threshold threshold
                                         :limit limit))
      (push (list :source :holographic
                  :layer (holographic-pattern-layer match)
                  :pattern match
                  :content (holographic-pattern-content match))
            results))
    ;; Also query native stores for completeness
    ;; Immediate: *recent-outcomes*
    (when (and (boundp '*recent-outcomes*) *recent-outcomes*)
      (dolist (outcome (subseq *recent-outcomes*
                               0 (min 10 (length *recent-outcomes*))))
        (push (list :source :immediate
                    :layer :immediate
                    :content (car outcome)
                    :correct (cdr outcome))
              results)))
    ;; Working: *situation-history*
    (when (and (boundp '*situation-history*)
               (> (fill-pointer *situation-history*) 0))
      (loop for i from (1- (fill-pointer *situation-history*)) downto 0
            for sit = (aref *situation-history* i)
            repeat 5
            do (push (list :source :working
                          :layer :working
                          :content (situation-context sit)
                          :novelty (situation-novelty sit))
                    results)))
    ;; Semantic: *long-term-memory*
    (when (and (boundp '*long-term-memory*)
               (> (hash-table-count *long-term-memory*) 0))
      (let ((ctx (if (listp content) content (list content))))
        (dolist (scale '(3 2 1))
          (let* ((key (make-context-key ctx scale))
                 (val (when key (gethash key *long-term-memory*))))
            (when val
              (push (list :source :semantic
                         :layer :semantic
                         :key key
                         :value val)
                   results))))))
    (nreverse results)))

;;; ============================================================================
;;; PART 5: SPARSE ROUTING INTEGRATION
;;; ============================================================================
;;; Wire sparse selection into the main routing path.

(defvar *sparse-routing-enabled* t
  "Whether to use sparse expert selection in routing.")

(defvar *sparse-expert-cache* nil
  "Cache of current step's sparse expert selection.")

(defun sparse-route-prediction (ctx)
  "Get prediction using sparse expert selection.
   Returns (prediction confidence expert-contributions)."
  (let* ((firing (sparse-expert-selection ctx))
         (predictions (make-hash-table :test 'equal))
         (total-weight 0.0))
    ;; Collect weighted votes from firing experts
    (dolist (entry firing)
      (let* ((expert (car entry))
             (activation (cdr entry))
             (pred (expert-best-prediction expert ctx)))
        (when pred
          (let ((weight (* activation
                          (expert-confidence-threshold expert))))
            (incf (gethash pred predictions 0.0) weight)
            (incf total-weight weight)))))
    ;; Find best prediction
    (let ((best-pred nil) (best-weight 0.0))
      (maphash (lambda (pred weight)
                 (when (> weight best-weight)
                   (setf best-pred pred best-weight weight)))
               predictions)
      (values best-pred
              (if (> total-weight 0) (/ best-weight total-weight) 0.0)
              firing))))

(defun expert-best-prediction (expert ctx)
  "Get expert's best prediction for context.
   Uses expert's knowledge hash."
  (when (and expert (expert-knowledge expert))
    (let ((best nil) (best-count 0))
      ;; Check at multiple scales
      (dolist (scale '(3 2 1))
        (when (>= (length ctx) scale)
          (let* ((key (subseq ctx 0 scale))
                 (entry (gethash key (expert-knowledge expert))))
            (when (and entry (listp entry))
              (dolist (pred-entry entry)
                (when (and (listp pred-entry)
                           (> (cdr pred-entry) best-count))
                  (setf best (car pred-entry)
                        best-count (cdr pred-entry))))))))
      best)))

;;; ============================================================================
;;; PART 5.5: HOLOGRAPHIC INTERFERENCE METRIC
;;; ============================================================================
;;; Measures how much current activation patterns conflict with stored patterns.
;;; This is the NN's "noise signal" - high interference triggers organic responses.

(defun compute-holographic-interference ()
  "Measure interference between recent holographic patterns.
   High interference = noisy/conflicting signals in the NN.
   Mid-range similarity between patterns indicates unresolved competition.
   Returns 0.0-1.0 where higher = more interference."
  (if (not *holographic-enabled*) 0.0
    (let ((immediate (gethash :immediate (holographic-memory-by-layer *holographic-memory*)))
          (interference 0.0)
          (comparisons 0))
      (when (and immediate (> (length immediate) 2))
        (loop for i from 0 below (min 10 (length immediate))
              for p1 = (nth i immediate)
              do (loop for j from (1+ i) below (min 10 (length immediate))
                       for p2 = (nth j immediate)
                       do (let ((sim (holographic-similarity p1 p2)))
                            ;; Mid-range similarity = interference (not same, not orthogonal)
                            (when (and (> sim 0.2) (< sim 0.7))
                              (incf interference (- 0.7 (abs (- sim 0.45))))
                              (incf comparisons))))))
      (if (> comparisons 0)
          (min 1.0 (/ interference (float comparisons)))
          0.0))))

;;; ============================================================================
;;; PART 6: PRESENCE INTEGRATION - PRESENCE SEES ALL
;;; ============================================================================
;;; Presence observes holographic memory as its field of awareness.
;;; Holographic patterns ARE what presence experiences.

(defun holographic-presence-update! ()
  "Update presence based on holographic memory state.
   Presence SEES all layers - they form its field of awareness."
  (when (and *holographic-enabled* (boundp '*presence*) *presence*)
    ;; Sparsity level affects presence trajectory
    (let ((k (compute-sparse-k)))
      (when (< k *sparse-k-min*)
        (presence-shift-trajectory! :flowing 0.05))
      (when (> k *sparse-k-max*)
        (presence-shift-trajectory! :exploring 0.05)))

    ;; Pattern count affects continuity
    (let ((pattern-count (holographic-memory-count *holographic-memory*)))
      (when (> pattern-count 100)
        (incf (presence-continuity *presence*) 0.01)))

    ;; Update presence awareness from holographic layers
    (holographic-update-presence-awareness!)))

(defun holographic-update-presence-awareness! ()
  "Update presence's field of awareness from holographic patterns.
   Texture-intensity = how much is held in active awareness.
   Now-vividness = strength of current patterns."
  (when (and (boundp '*presence*) *presence*)
    (let* ((immediate (gethash :immediate (holographic-memory-by-layer *holographic-memory*)))
           (working (gethash :working (holographic-memory-by-layer *holographic-memory*)))
           (immediate-count (length immediate))
           (working-count (length working)))

      ;; Texture intensity from awareness load
      (let ((awareness-load (+ immediate-count (* 0.5 working-count))))
        (setf (presence-texture-intensity *presence*)
              (min 1.0 (max 0.1 (/ awareness-load 50.0)))))

      ;; Now-vividness from average strength of immediate patterns
      (when immediate
        (let* ((now (presence-now *presence*))
               (avg-strength (/ (reduce #'+ immediate
                                        :key #'holographic-pattern-strength)
                               (max 1 immediate-count))))
          (setf (presence-moment-vividness now)
                (min 1.0 (max 0.1 avg-strength))))))))

(defun presence-holographic-view ()
  "What presence currently 'sees' through holographic memory.
   Returns the immediate field of awareness."
  (when (and *holographic-enabled* (boundp '*presence*) *presence*)
    (list :current-focus (holographic-retrieve-layer :immediate 5)
          :working-context (holographic-retrieve-layer :working 3)
          :background-knowledge (holographic-retrieve-layer :semantic 2)
          :texture-intensity (presence-texture-intensity *presence*)
          :now-vividness (presence-moment-vividness (presence-now *presence*))
          :trajectory (presence-trajectory *presence*)
          :continuity (presence-continuity *presence*)
          :sparsity-k (compute-sparse-k))))

(defun presence-query-memory (query &key (scope :all))
  "Presence queries holographic memory.
   SCOPE: :immediate (focus), :working (context), :deep (all), :all"
  (when *holographic-enabled*
    (let ((layers (case scope
                    (:immediate '(:immediate))
                    (:working '(:immediate :working))
                    (:deep '(:episodic :semantic))
                    (:all '(:immediate :working :episodic :semantic))
                    (t '(:immediate :working)))))
      (holographic-unified-query query :layers layers :types :all))))

(defun presence-feel-pattern! (pattern-type content)
  "Presence stores a pattern it 'feels' into holographic memory.
   This is how presence writes to memory."
  (when *holographic-enabled*
    (let* ((ctx (list 'presence-feel pattern-type))
           (hp (holographic-encode ctx)))
      (setf (holographic-pattern-content hp)
            (list :type :presence-feel
                  :pattern-type pattern-type
                  :content content
                  :vividness (when (and (boundp '*presence*) *presence*)
                              (presence-moment-vividness (presence-now *presence*)))
                  :step *step*))
      (holographic-store! hp :layer :immediate))))

(defun presence-reflect ()
  "Presence reflects on its holographic memory state.
   Returns a self-aware summary."
  (when (and *holographic-enabled* (boundp '*presence*) *presence*)
    (let* ((patterns (holographic-memory-count *holographic-memory*))
           (immediate (length (gethash :immediate (holographic-memory-by-layer *holographic-memory*))))
           (working (length (gethash :working (holographic-memory-by-layer *holographic-memory*))))
           (episodic (length (gethash :episodic (holographic-memory-by-layer *holographic-memory*))))
           (semantic (length (gethash :semantic (holographic-memory-by-layer *holographic-memory*)))))
      (list :i-am-aware-of patterns
            :in-focus immediate
            :in-context working
            :in-memory episodic
            :in-knowledge semantic
            :my-texture-intensity (presence-texture-intensity *presence*)
            :my-now-vividness (presence-moment-vividness (presence-now *presence*))
            :my-trajectory (presence-trajectory *presence*)
            :my-continuity (presence-continuity *presence*)
            :my-self-confidence (presence-self-confidence *presence*)))))

;;; ============================================================================
;;; PART 7: HOOKS - WIRE INTO EXISTING SYSTEM
;;; ============================================================================

(defun holographic-post-process-hook (tok ctx predicted got-it)
  "Hook: after each token, optionally store holographic pattern."
  (declare (ignore predicted))
  (when *holographic-enabled*
    ;; Store significant patterns
    (when (or (not got-it)  ; errors are interesting
              (< (random 1.0) 0.1))  ; sample 10% of successes
      (let ((hp (holographic-encode ctx)))
        (setf (holographic-pattern-content hp)
              (list :token tok :correct got-it :step *step*))
        (holographic-store! hp :layer :immediate)))
    ;; Decay driven by memory pressure (organic, not timer-based)
    (when (> (holographic-memory-count *holographic-memory*) 200)
      (holographic-decay-all!))
    ;; Update presence
    (holographic-presence-update!)))

(defun holographic-pattern-subsumed-by-p (hp target-layer &optional (threshold 0.9))
  "Check if pattern HP is subsumed by an existing pattern in TARGET-LAYER.
   Returns the subsuming pattern if found, nil otherwise."
  (dolist (existing (gethash target-layer (holographic-memory-by-layer *holographic-memory*)))
    (when (and (not (eq hp existing))
               (> (holographic-similarity hp existing) threshold)
               (>= (holographic-pattern-strength existing)
                   (holographic-pattern-strength hp)))
      (return-from holographic-pattern-subsumed-by-p existing)))
  nil)

(defun holographic-promote-pattern! (hp from-layer to-layer)
  "Promote pattern HP from FROM-LAYER to TO-LAYER, handling deduplication.
   Removes from source layer. If subsumed by existing in target, merges instead."
  (let ((subsuming (holographic-pattern-subsumed-by-p hp to-layer)))
    (if subsuming
        ;; Pattern is subsumed - merge into existing (boost strength/access)
        (progn
          (incf (holographic-pattern-strength subsuming)
                (* 0.1 (holographic-pattern-strength hp)))
          (incf (holographic-pattern-access-count subsuming)
                (holographic-pattern-access-count hp)))
        ;; Not subsumed - promote to new layer
        (progn
          (setf (holographic-pattern-layer hp) to-layer)
          (push hp (gethash to-layer (holographic-memory-by-layer *holographic-memory*)))))
    ;; Always remove from source layer
    (setf (gethash from-layer (holographic-memory-by-layer *holographic-memory*))
          (remove hp (gethash from-layer (holographic-memory-by-layer *holographic-memory*))
                  :test #'eq))))

(defun holographic-consolidation-hook ()
  "Hook: during consolidation, promote strong patterns (with deduplication)."
  (when *holographic-enabled*
    ;; Collect patterns to promote (don't modify list while iterating)
    (let ((immediate-to-promote nil)
          (working-to-promote nil))
      ;; Find immediate patterns ready for working
      (dolist (hp (gethash :immediate (holographic-memory-by-layer *holographic-memory*)))
        (when (and (> (holographic-pattern-strength hp) 0.8)
                   (> (holographic-pattern-access-count hp) 3))
          (push hp immediate-to-promote)))
      ;; Find working patterns ready for episodic
      (dolist (hp (gethash :working (holographic-memory-by-layer *holographic-memory*)))
        (when (and (> (holographic-pattern-strength hp) 0.9)
                   (> (holographic-pattern-access-count hp) 5))
          (push hp working-to-promote)))
      ;; Now promote (with deduplication)
      (dolist (hp immediate-to-promote)
        (holographic-promote-pattern! hp :immediate :working))
      (dolist (hp working-to-promote)
        (holographic-promote-pattern! hp :working :episodic)))))

;; Register hooks
(register-hook +hook-post-process-token+
               'holographic-post-process-hook
               :priority 85)

(register-hook +hook-maintenance+
               'holographic-consolidation-hook
               :priority 60)

;;; ============================================================================
;;; PART 8: DIAGNOSTIC FUNCTIONS
;;; ============================================================================

(defun print-holographic-status ()
  "Print status of holographic substrate."
  (format t "~%=== HOLOGRAPHIC SUBSTRATE STATUS ===~%")
  (format t "Enabled: ~A~%" *holographic-enabled*)
  (format t "Sparse routing: ~A~%" *sparse-routing-enabled*)
  (format t "Current k: ~D (range ~D-~D)~%"
          (compute-sparse-k) *sparse-k-min* *sparse-k-max*)
  (format t "~%Pattern counts by layer:~%")
  (maphash (lambda (layer patterns)
             (format t "  ~A: ~D patterns~%" layer (length patterns)))
           (holographic-memory-by-layer *holographic-memory*))
  (format t "Total patterns: ~D~%" (holographic-memory-count *holographic-memory*))
  (format t "~%Memory views:~%")
  (dolist (view *memory-views*)
    (format t "  ~A: decay=~,3F capacity=~A~%"
            (memory-view-name view)
            (memory-view-decay-rate view)
            (or (memory-view-capacity view) "unlimited"))))

(defun test-sparse-selection (text)
  "Test sparse expert selection on text."
  (format t "~%=== SPARSE SELECTION TEST ===~%")
  (format t "Input: ~S~%" text)
  (let* ((tokens (if (listp text) text (tokenize text)))
         (ctx (subseq tokens 0 (min 5 (length tokens))))
         (k (compute-sparse-k))
         (firing (sparse-expert-selection ctx)))
    (format t "Context: ~S~%" ctx)
    (format t "Sparse k: ~D~%" k)
    (format t "Firing experts (~D):~%" (length firing))
    (dolist (entry firing)
      (let ((expert (car entry))
            (activation (cdr entry)))
        (format t "  Expert ~D: activation=~,3F reliability=~,3F~%"
                (expert-id expert)
                activation
                (/ (expert-hits expert)
                   (max 1 (+ (expert-hits expert) (expert-misses expert)))))))))

(defun test-holographic-retrieval (text)
  "Test holographic pattern encoding and retrieval."
  (format t "~%=== HOLOGRAPHIC RETRIEVAL TEST ===~%")
  (format t "Query: ~S~%" text)
  (let* ((results (holographic-query-all-layers text)))
    (format t "Found ~D results across layers:~%" (length results))
    (dolist (r results)
      (format t "  [~A/~A] ~S~%"
              (getf r :source)
              (getf r :layer)
              (or (getf r :content) (getf r :key))))))

;;; ============================================================================
;;; PART 9: UNIVERSAL CROSS-WIRING - HOLOGRAPHIC AS THE SUBSTRATE FOR ALL
;;; ============================================================================
;;; Every system reads/writes through holographic patterns.
;;; This makes holographic memory THE memory, not A memory.

;;; --- 9.1: Self-Model Interface ---
;;; Self-model stores its statistics as holographic patterns

(defun holographic-store-self-model! (self-model)
  "Store self-model state as holographic pattern for introspection."
  (when (and *holographic-enabled* self-model)
    (let* ((ctx (list 'self-model (type-of self-model)))
           (hp (holographic-encode ctx)))
      (setf (holographic-pattern-content hp)
            (list :type :self-model
                  :error-rate (if (fboundp 'self-model-error-rate)
                                  (funcall 'self-model-error-rate self-model) 0)
                  :uncertainty (if (fboundp 'self-model-uncertainty)
                                   (funcall 'self-model-uncertainty self-model) 0)
                  :step *step*))
      (setf (holographic-pattern-layer hp) :semantic)  ; Long-term self-knowledge
      (holographic-store! hp :layer :semantic))))

(defun holographic-retrieve-self-model ()
  "Retrieve self-model patterns from holographic memory."
  (holographic-retrieve 'self-model :layer :semantic))

;;; --- 9.2: Introspection Interface ---
;;; Introspection reads holographic patterns to understand state

(defun holographic-retrieve-layer (layer &optional (n 5))
  "Retrieve top N patterns from a layer by strength."
  (let ((patterns (gethash layer (holographic-memory-by-layer *holographic-memory*))))
    (when patterns
      (subseq (sort (copy-list patterns) #'>
                    :key #'holographic-pattern-strength)
              0 (min n (length patterns))))))

(defun holographic-introspect (&optional (depth 3))
  "Introspect by reading holographic patterns across all layers.
   Returns summary of what the system 'knows about itself'."
  (let ((immediate (holographic-retrieve-layer :immediate depth))
        (working (holographic-retrieve-layer :working depth))
        (episodic (holographic-retrieve-layer :episodic depth))
        (semantic (holographic-retrieve-layer :semantic depth)))
    (list :immediate-focus (mapcar #'holographic-pattern-content immediate)
          :working-context (mapcar #'holographic-pattern-content working)
          :episodic-memory (mapcar #'holographic-pattern-content episodic)
          :semantic-knowledge (mapcar #'holographic-pattern-content semantic)
          :total-patterns (holographic-memory-count *holographic-memory*)
          :current-k (compute-sparse-k))))

;;; --- 9.3: Goal System Interface ---
;;; Goals stored as holographic patterns, retrieved by context

(defun holographic-store-goal! (goal)
  "Store a goal as holographic pattern."
  (when (and *holographic-enabled* goal)
    (let* ((ctx (if (listp goal)
                    (list 'goal (getf goal :type) (getf goal :description))
                    (list 'goal goal)))
           (hp (holographic-encode ctx)))
      (setf (holographic-pattern-content hp)
            (list :type :goal
                  :goal goal
                  :priority (if (listp goal) (getf goal :priority 0.5) 0.5)
                  :step *step*))
      (setf (holographic-pattern-layer hp) :working)  ; Goals are working memory
      (holographic-store! hp :layer :working))))

(defun holographic-retrieve-relevant-goals (ctx)
  "Retrieve goals relevant to current context from holographic memory."
  (let* ((hp-query (holographic-encode ctx))
         (goal-patterns (remove-if-not
                         (lambda (hp)
                           (eq (getf (holographic-pattern-content hp) :type) :goal))
                         (gethash :working (holographic-memory-by-layer *holographic-memory*)))))
    (when goal-patterns
      (let ((scored (mapcar (lambda (hp)
                              (cons hp (holographic-similarity hp-query hp)))
                            goal-patterns)))
        (mapcar #'car (sort scored #'> :key #'cdr))))))

;;; --- 9.4: Dream System Interface ---
;;; Dreams consolidate patterns and create new holographic links

(defun holographic-dream-consolidate! (episode)
  "Consolidate a dream episode into holographic memory."
  (when (and *holographic-enabled* episode)
    (let* ((ctx (if (listp episode)
                    (or (getf episode :context) (list 'dream episode))
                    (list 'dream episode)))
           (hp (holographic-encode ctx)))
      (setf (holographic-pattern-content hp)
            (list :type :dream
                  :episode episode
                  :insight (getf episode :insight)
                  :step *step*))
      ;; Dreams go to episodic - they're consolidated memories
      (setf (holographic-pattern-layer hp) :episodic)
      (holographic-store! hp :layer :episodic)
      ;; Strengthen related patterns
      (dolist (related (holographic-find-similar hp 0.5))
        (incf (holographic-pattern-strength (car related)) 0.1)
        (incf (holographic-pattern-access-count (car related)))))))

(defun holographic-dream-retrieve (theme)
  "Retrieve dream-related patterns by theme."
  (let* ((ctx (list 'dream theme))
         (hp-query (holographic-encode ctx)))
    (holographic-find-similar hp-query 0.3)))

;;; --- 9.5: Episodic Memory Interface ---
;;; Episodic memory IS holographic patterns in the :episodic layer

(defun holographic-store-episode! (episode)
  "Store an episode as holographic pattern."
  (when (and *holographic-enabled* episode)
    (let* ((ctx (or (when (listp episode) (getf episode :context))
                    (list 'episode episode)))
           (hp (holographic-encode ctx)))
      (setf (holographic-pattern-content hp)
            (list :type :episode
                  :episode episode
                  :emotional-valence (getf episode :emotional-valence 0)
                  :step *step*))
      (setf (holographic-pattern-layer hp) :episodic)
      (holographic-store! hp :layer :episodic))))

(defun holographic-retrieve-episodes (ctx &optional (n 5))
  "Retrieve similar episodes from holographic memory."
  (let* ((hp-query (holographic-encode ctx))
         (episode-patterns (remove-if-not
                            (lambda (hp)
                              (eq (getf (holographic-pattern-content hp) :type) :episode))
                            (gethash :episodic (holographic-memory-by-layer *holographic-memory*)))))
    (when episode-patterns
      (let ((scored (mapcar (lambda (hp)
                              (cons hp (holographic-similarity hp-query hp)))
                            episode-patterns)))
        (subseq (sort scored #'> :key #'cdr)
                0 (min n (length scored)))))))

;;; --- 9.6: Schema System Interface ---
;;; Schemas are high-level holographic patterns in :semantic layer

(defun holographic-store-schema! (schema)
  "Store a schema as holographic pattern for persistent access."
  (when (and *holographic-enabled* schema)
    (let* ((pattern (if (fboundp 'executable-schema-p)
                        (if (funcall 'executable-schema-p schema)
                            (funcall 'executable-schema-source-pattern schema)
                            schema)
                        schema))
           (ctx (list 'schema pattern))
           (hp (holographic-encode ctx)))
      (setf (holographic-pattern-content hp)
            (list :type :schema
                  :schema schema
                  :success-rate (when (and (fboundp 'executable-schema-p)
                                          (funcall 'executable-schema-p schema))
                                 (let ((exec (funcall 'executable-schema-execution-count schema)))
                                   (if (> exec 0)
                                       (/ (funcall 'executable-schema-success-count schema) exec)
                                       0)))
                  :step *step*))
      (setf (holographic-pattern-layer hp) :semantic)  ; Schemas are semantic knowledge
      (holographic-store! hp :layer :semantic))))

(defun holographic-retrieve-schemas (ctx)
  "Retrieve relevant schemas from holographic memory."
  (let* ((hp-query (holographic-encode ctx))
         (schema-patterns (remove-if-not
                           (lambda (hp)
                             (eq (getf (holographic-pattern-content hp) :type) :schema))
                           (gethash :semantic (holographic-memory-by-layer *holographic-memory*)))))
    (when schema-patterns
      (mapcar (lambda (hp) (getf (holographic-pattern-content hp) :schema))
              (mapcar #'car
                      (sort (mapcar (lambda (hp)
                                      (cons hp (holographic-similarity hp-query hp)))
                                    schema-patterns)
                            #'> :key #'cdr))))))

;;; --- 9.7: Self-Expectation Interface ---
;;; Self-expectations use holographic patterns to predict own behavior

(defun holographic-store-expectation! (expectation)
  "Store a self-expectation as holographic pattern."
  (when (and *holographic-enabled* expectation)
    (let* ((ctx (list 'self-expect (type-of expectation)))
           (hp (holographic-encode ctx)))
      (setf (holographic-pattern-content hp)
            (list :type :self-expectation
                  :expectation expectation
                  :step *step*))
      (setf (holographic-pattern-layer hp) :working)
      (holographic-store! hp :layer :working))))

(defun holographic-predict-own-behavior (ctx)
  "Use holographic patterns to predict own behavior.
   Returns expected outcome based on similar past situations."
  (let* ((hp-query (holographic-encode ctx))
         ;; Look across all layers for relevant patterns
         (all-patterns (append
                        (gethash :immediate (holographic-memory-by-layer *holographic-memory*))
                        (gethash :working (holographic-memory-by-layer *holographic-memory*))
                        (gethash :episodic (holographic-memory-by-layer *holographic-memory*)))))
    (when all-patterns
      (let* ((scored (mapcar (lambda (hp)
                               (cons hp (holographic-similarity hp-query hp)))
                             all-patterns))
             (top-matches (subseq (sort scored #'> :key #'cdr)
                                  0 (min 5 (length scored)))))
        ;; Compute expected outcome from top matches
        (let ((success-count 0) (total 0))
          (dolist (match top-matches)
            (let ((content (holographic-pattern-content (car match))))
              (when (getf content :correct)
                (incf success-count))
              (incf total)))
          (if (> total 0)
              (list :expected-success-rate (/ success-count total)
                    :confidence (reduce #'+ (mapcar #'cdr top-matches))
                    :based-on total)
              nil))))))

;;; --- 9.8: Cognitive Trace Interface ---
;;; Traces stored as holographic patterns for later introspection

(defun holographic-store-trace! (trace)
  "Store a cognitive trace as holographic pattern."
  (when (and *holographic-enabled* trace)
    (let* ((ctx (if (and (fboundp 'cognitive-trace-p)
                        (funcall 'cognitive-trace-p trace))
                   (funcall 'cognitive-trace-context trace)
                   (list 'trace trace)))
           (hp (holographic-encode ctx)))
      (setf (holographic-pattern-content hp)
            (list :type :trace
                  :trace trace
                  :step *step*))
      ;; Traces are immediate/working memory
      (setf (holographic-pattern-layer hp) :immediate)
      (holographic-store! hp :layer :immediate))))

;;; --- 9.9: Unified Query Interface ---
;;; Single interface to query across all pattern types

(defun holographic-unified-query (ctx &key (types :all) (layers :all) (n 10))
  "Query holographic memory across types and layers.
   TYPES: :all, or list like (:goal :episode :schema)
   LAYERS: :all, or list like (:working :episodic)
   Returns unified results sorted by relevance."
  (let* ((hp-query (holographic-encode ctx))
         (search-layers (if (eq layers :all)
                           '(:immediate :working :episodic :semantic)
                           (if (listp layers) layers (list layers))))
         (all-patterns nil))
    ;; Collect patterns from specified layers
    (dolist (layer search-layers)
      (dolist (hp (gethash layer (holographic-memory-by-layer *holographic-memory*)))
        (let ((content-type (getf (holographic-pattern-content hp) :type)))
          (when (or (eq types :all)
                    (member content-type types))
            (push hp all-patterns)))))
    ;; Score and sort
    (when all-patterns
      (let ((scored (mapcar (lambda (hp)
                              (cons hp (holographic-similarity hp-query hp)))
                            all-patterns)))
        (mapcar (lambda (pair)
                  (list :pattern (car pair)
                        :similarity (cdr pair)
                        :type (getf (holographic-pattern-content (car pair)) :type)
                        :layer (holographic-pattern-layer (car pair))
                        :content (holographic-pattern-content (car pair))))
                (subseq (sort scored #'> :key #'cdr)
                        0 (min n (length scored))))))))

;;; --- 9.10: Cross-Wire Hooks ---
;;; Install hooks to wire holographic memory into all systems

(defun install-holographic-cross-wiring! ()
  "Install hooks to wire holographic memory into all systems."

  ;; Wire into self-model updates
  (when (boundp '+hook-post-learn+)
    (register-hook +hook-post-learn+
                   (lambda (ctx actual predicted correct learner)
                     (declare (ignore ctx actual predicted correct))
                     (when (and learner (boundp '*self-model*) *self-model*)
                       (holographic-store-self-model! *self-model*)))
                   :priority 90))

  ;; Wire into dream consolidation
  (when (boundp '+hook-maintenance+)
    (register-hook +hook-maintenance+
                   (lambda ()
                     (when (and (boundp '*dream-state*) *dream-state*
                               (fboundp 'dream-state-current-dream))
                       (let ((dream (funcall 'dream-state-current-dream *dream-state*)))
                         (when dream
                           (holographic-dream-consolidate! dream)))))
                   :priority 65))

  ;; Wire into goal system
  (when (boundp '+hook-post-process-token+)
    (register-hook +hook-post-process-token+
                   (lambda (tok ctx predicted got-it)
                     (declare (ignore tok predicted got-it))
                     (when (and ctx (boundp '*goal-stack*) *goal-stack*)
                       ;; Store active goals in holographic memory
                       (dolist (goal (subseq *goal-stack* 0 (min 3 (length *goal-stack*))))
                         (holographic-store-goal! goal))))
                   :priority 88))

  ;; Wire into cognitive traces
  (when (and (boundp '+hook-post-process-token+)
             (fboundp 'make-cognitive-trace))
    (register-hook +hook-post-process-token+
                   (lambda (tok ctx predicted got-it)
                     (declare (ignore tok predicted))
                     (when ctx
                       (let ((trace (list :context ctx :correct got-it :step *step*)))
                         (holographic-store-trace! trace))))
                   :priority 89))

  (format t "Holographic cross-wiring installed.~%"))

;; Auto-install cross-wiring
(install-holographic-cross-wiring!)

;;; ============================================================================
;;; INITIALIZATION
;;; ============================================================================

(defun initialize-holographic-substrate! ()
  "Initialize the holographic substrate."
  (setf *holographic-memory* (make-holographic-memory))
  (format t "Holographic substrate initialized.~%")
  (format t "  Sparse k: ~D (presence-controlled)~%" *sparse-k-default*)
  (format t "  Decay enabled: ~A~%" *holographic-decay-enabled*))

;; Auto-initialize
(initialize-holographic-substrate!)

;;; ============================================================================
;;; MODULE LOAD CONFIRMATION
;;; ============================================================================

(format t "~%================================================================~%")
(format t "UHMA HOLOGRAPHIC SUBSTRATE - UNIFIED MEMORY LAYER~%")
(format t "================================================================~%")
(format t "~%Core Features:~%")
(format t "  - Sparse expert activation (top-k firing)~%")
(format t "  - Holographic pattern encoding~%")
(format t "  - Unified memory view with decay~%")
(format t "  - Presence-gated sparsity control~%")
(format t "~%Cross-Wired Systems (everything uses holographic memory):~%")
(format t "  - Self-model     -> holographic-store-self-model!~%")
(format t "  - Introspection  -> holographic-introspect~%")
(format t "  - Goals          -> holographic-store-goal!, holographic-retrieve-relevant-goals~%")
(format t "  - Dreams         -> holographic-dream-consolidate!~%")
(format t "  - Episodes       -> holographic-store-episode!, holographic-retrieve-episodes~%")
(format t "  - Schemas        -> holographic-store-schema!, holographic-retrieve-schemas~%")
(format t "  - Self-expects   -> holographic-store-expectation!, holographic-predict-own-behavior~%")
(format t "  - Traces         -> holographic-store-trace!~%")
(format t "~%Unified Query:~%")
(format t "  (holographic-unified-query ctx :types :all :layers :all)~%")
(format t "~%Configuration:~%")
(format t "  *holographic-enabled*: ~A~%" *holographic-enabled*)
(format t "  *sparse-k-default*: ~D (range ~D-~D)~%" *sparse-k-default* *sparse-k-min* *sparse-k-max*)
(format t "================================================================~%")


;;;; [SECTION-START:99:VERIFICATION]
;;; Section 99: File Verification

(defun verify-holographic-substrate-lisp-completeness () 
  "Verify completeness."
  (unless (find-package :uhma)
    (error "Package UHMA not defined"))
  (unless (fboundp 'compute-all-expert-activations)
    (error "Function compute-all-expert-activations not defined"))
  (format t "~&uhma-holographic-substrate.lisp verification passed.~%"))

(verify-holographic-substrate-lisp-completeness)

;;;; [SECTION-END:99:VERIFICATION]
