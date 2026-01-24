;;;; ============================================================================
;;;; UHMA HOLOGRAPHIC SUBSTRATE V2 (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Replaces all 7 memory layers with ONE unified holographic store.
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 4: STRUCTURES ---

(defstruct holo-pattern
  "A memory encoded as sparse expert activations."
  (id (gensym "HP-") :type symbol)
  (activations nil :type list)
  (layer :immediate :type symbol)
  (content-type nil :type symbol)
  (content nil)
  (strength 1.0 :type single-float)
  (created-step 0 :type fixnum)
  (access-count 0 :type fixnum)
  (hits 0 :type fixnum) ; Added for pattern-stats
  (misses 0 :type fixnum)) ; Added for pattern-stats

(defstruct holo-store
  "The unified holographic memory store."
  (patterns (make-hash-table :test 'equal) :type hash-table)
  (by-layer (make-hash-table :test 'eq) :type hash-table)
  (by-type (make-hash-table :test 'eq) :type hash-table)
  (by-expert (make-hash-table :test 'eql) :type hash-table)
  (total-stored 0 :type fixnum)
  (total-retrieved 0 :type fixnum))

;;; --- SECTION 6: SPECIAL VARIABLES ---

(defvar *holo* (make-holo-store) "THE unified holographic memory store.")
(defvar *holo-enabled* t )
(defvar *holo-sparse-k* 7 )
(defvar *holo-similarity-threshold* 0.6 "Minimum similarity to consider a match.")

;;; --- SECTION 8: CORE FUNCTIONS ---

(defun holo-compute-activations (ctx)
  "Map symbolic context to sparse expert resonance."
  (declare (type list ctx))
  (let ((activations nil))
    ;; Resonance logic...
    (let ((sorted (sort activations #'> :key #'cdr)))
      (subseq sorted 0 (min *holo-sparse-k* (length sorted))))))

(defun holo-store! (pattern)
  "Commit a holographic pattern to the unified Mind-Space."
  (declare (type holo-pattern pattern))
  (let ((id (holo-pattern-id pattern)) (layer (holo-pattern-layer pattern)))
    (setf (gethash id (holo-store-patterns *holo*)) pattern)
    (push id (gethash layer (holo-store-by-layer *holo*)))
    (incf (holo-store-total-stored *holo*))
    pattern))

;;; --- SECTION 10: INITIALIZATION ---
(eval-when (:load-toplevel :execute)
  (format t "[HOLO-SUBSTRATE] Unified holographic memory active.~%"))

(defun holo-enforce-capacity! (layer)
  "Enforce capacity limit for layer by removing weakest patterns."
  (let ((capacity (cdr (assoc layer *holo-layer-capacity*))))
    (when capacity
      (let ((ids (gethash layer (holo-store-by-layer *holo*))))
        (when (> (length ids) capacity)
          ;; Sort by strength, remove weakest
          (let* ((patterns (mapcar (lambda (id) (gethash id (holo-store-patterns *holo*))) ids))
                 (sorted (sort (copy-list patterns) #'< :key #'holo-pattern-strength))
                 (to-remove (subseq sorted 0 (- (length ids) capacity))))
            (dolist (p to-remove)
              (holo-remove! p))))))))

(defun holo-remove! (pattern)
  "Remove a pattern from the store."
  (when (holo-pattern-p pattern)
    (let ((id (holo-pattern-id pattern))
          (layer (holo-pattern-layer pattern))
          (ctype (holo-pattern-content-type pattern)))
      ;; Remove from main table
      (remhash id (holo-store-patterns *holo*))
      ;; Remove from layer index
      (setf (gethash layer (holo-store-by-layer *holo*))
            (remove id (gethash layer (holo-store-by-layer *holo*))))
      ;; Remove from type index
      (when ctype
        (setf (gethash ctype (holo-store-by-type *holo*))
              (remove id (gethash ctype (holo-store-by-type *holo*)))))
      ;; Remove from expert index
      (dolist (act (holo-pattern-activations pattern))
        (setf (gethash (car act) (holo-store-by-expert *holo*))
              (remove id (gethash (car act) (holo-store-by-expert *holo*))))))))

;;; ============================================================================
;;; RETRIEVAL - QUERY PATTERNS
;;; ============================================================================

(defun holo-retrieve (query &key layer content-type (threshold *holo-similarity-threshold*) (limit 10))
  "Retrieve patterns similar to query.
   QUERY can be: content (encodes first), activations list, or holo-pattern.
   Returns list of (similarity . pattern) pairs sorted by similarity."
  (when *holo-enabled*
    (let* ((query-activations (cond
                                ((holo-pattern-p query) (holo-pattern-activations query))
                                ((and (listp query) (consp (car query)) (numberp (cdar query))) query)
                                (t (holo-pattern-activations (holo-encode query)))))
           (candidates (holo-get-candidates query-activations layer content-type))
           (results nil))
      (dolist (pattern candidates)
        (let ((sim (holo-similarity query-activations pattern)))
          (when (and sim (> sim threshold))
            ;; Update access stats
            (incf (holo-pattern-access-count pattern))
            (setf (holo-pattern-last-accessed pattern) (if (boundp '*step*) *step* 0))
            ;; Strengthen slightly on access
            (incf (holo-pattern-strength pattern) 0.02)
            (push (cons sim pattern) results))))
      (incf (holo-store-total-retrieved *holo*))
      ;; Return sorted, limited
      (subseq (sort results #'> :key #'car) 0 (min limit (length results))))))

(defun holo-get-candidates (activations layer content-type)
  "Get candidate patterns for retrieval.
   Uses expert index for fast narrowing."
  (let ((candidates nil))
    (if (or layer content-type)
        ;; Filtered search
        (let ((ids (cond
                    ((and layer content-type)
                     (intersection (gethash layer (holo-store-by-layer *holo*))
                                   (gethash content-type (holo-store-by-type *holo*))))
                    (layer (gethash layer (holo-store-by-layer *holo*)))
                    (content-type (gethash content-type (holo-store-by-type *holo*))))))
          (dolist (id ids)
            (let ((p (gethash id (holo-store-patterns *holo*))))
              (when p (push p candidates)))))
        ;; Use expert index for broad search
        (if activations
            (let ((seen (make-hash-table :test 'equal)))
              (dolist (act activations)
                (dolist (id (gethash (car act) (holo-store-by-expert *holo*)))
                  (unless (gethash id seen)
                    (setf (gethash id seen) t)
                    (let ((p (gethash id (holo-store-patterns *holo*))))
                      (when p (push p candidates)))))))
            ;; Fallback: all patterns
            (maphash (lambda (id p) (declare (ignore id)) (push p candidates))
                     (holo-store-patterns *holo*))))
    candidates))

(defun holo-query (content &key layer content-type (limit 10))
  "Convenience: query by content, return just patterns (not similarities)."
  (mapcar #'cdr (holo-retrieve content :layer layer :content-type content-type :limit limit)))

;;; ============================================================================
;;; DECAY & MAINTENANCE
;;; ============================================================================

(defun holo-decay! ()
  "Apply decay to all patterns based on their layer."
  (when *holo-enabled*
    (let ((to-remove nil))
      (maphash
       (lambda (id pattern)
         (declare (ignore id))
         (let* ((layer (holo-pattern-layer pattern))
                (decay-rate (or (cdr (assoc layer *holo-decay-rates*)) 0.01)))
           (when (> decay-rate 0)
             (decf (holo-pattern-strength pattern) decay-rate)
             (when (<= (holo-pattern-strength pattern) 0.0)
               (push pattern to-remove)))))
       (holo-store-patterns *holo*))
      ;; Remove dead patterns
      (dolist (p to-remove)
        (holo-remove! p)
        (incf (holo-store-total-decayed *holo*))))))

(defun holo-consolidate! ()
  "Promote strong patterns to more permanent layers."
  (when *holo-enabled*
    ;; Immediate -> Working (if accessed multiple times)
    (dolist (id (copy-list (gethash :immediate (holo-store-by-layer *holo*))))
      (let ((p (gethash id (holo-store-patterns *holo*))))
        (when (and p (> (holo-pattern-access-count p) 2))
          (holo-promote! p :working))))
    ;; Working -> Situational (if strong)
    (dolist (id (copy-list (gethash :working (holo-store-by-layer *holo*))))
      (let ((p (gethash id (holo-store-patterns *holo*))))
        (when (and p (> (holo-pattern-strength p) 0.8) (> (holo-pattern-access-count p) 5))
          (holo-promote! p :situational))))
    ;; Situational -> Semantic (if very strong and accessed often)
    (dolist (id (copy-list (gethash :situational (holo-store-by-layer *holo*))))
      (let ((p (gethash id (holo-store-patterns *holo*))))
        (when (and p (> (holo-pattern-strength p) 0.9) (> (holo-pattern-access-count p) 10))
          (holo-promote! p :semantic))))))

(defun holo-promote! (pattern new-layer)
  "Promote pattern to new layer."
  (let ((old-layer (holo-pattern-layer pattern))
        (id (holo-pattern-id pattern)))
    ;; Remove from old layer index
    (setf (gethash old-layer (holo-store-by-layer *holo*))
          (remove id (gethash old-layer (holo-store-by-layer *holo*))))
    ;; Update pattern
    (setf (holo-pattern-layer pattern) new-layer)
    (setf (holo-pattern-strength pattern) 1.0)  ; Reset strength
    ;; Add to new layer index
    (push id (gethash new-layer (holo-store-by-layer *holo*)))))

(defun holo-maintenance! ()
  "Full maintenance cycle: decay + consolidate."
  (holo-decay!)
  (holo-consolidate!))

;;; ============================================================================
;;; LAYER-SPECIFIC API (Replaces old memory systems)
;;; ============================================================================

;;; --- IMMEDIATE (replaces *recent-outcomes*) ---

(defun holo-store-outcome! (ctx correct-p &optional prediction)
  "Store an outcome. Replaces pushing to *recent-outcomes*."
  (holo-store! (holo-encode (list ctx correct-p prediction)
                            :layer :immediate
                            :content-type :outcome
                            :ctx ctx)))

(defun holo-recent-outcomes (&optional (n 20))
  "Get recent outcomes. Replaces reading *recent-outcomes*."
  (let ((patterns (holo-query nil :layer :immediate :content-type :outcome :limit n)))
    (mapcar #'holo-pattern-content patterns)))

;;; --- WORKING (replaces *working-memory*) ---

(defun holo-store-working! (content &optional focus)
  "Store in working memory."
  (let ((p (holo-encode content :layer :working :content-type :working)))
    (when focus (setf (holo-pattern-content p) (list :focus focus :data content)))
    (holo-store! p)))

(defun holo-working-contents ()
  "Get working memory contents."
  (mapcar #'holo-pattern-content (holo-query nil :layer :working :limit 50)))

;;; --- SITUATIONAL (replaces *situation-history*) ---

(defun holo-store-situation! (situation)
  "Store a situation assessment."
  (let ((ctx (if (and (fboundp 'situation-p) (situation-p situation))
                 (situation-context situation)
                 situation)))
    (holo-store! (holo-encode ctx :layer :situational :content-type :situation))))

(defun holo-find-similar-situations (ctx &optional (n 5))
  "Find situations similar to context."
  (holo-query ctx :layer :situational :content-type :situation :limit n))

;;; --- EPISODIC (replaces *episodic-memory*) ---

(defun holo-store-episode! (episode)
  "Store an episode."
  (let ((ctx (if (and (fboundp 'episode-p) (episode-p episode))
                 (list 'episode (episode-id episode) (episode-trigger episode))
                 (list 'episode episode))))
    (let ((p (holo-encode ctx :layer :episodic :content-type :episode)))
      (setf (holo-pattern-content p) episode)
      (holo-store! p))))

(defun holo-retrieve-episodes (ctx &optional (n 5))
  "Retrieve episodes similar to context."
  (holo-query ctx :layer :episodic :content-type :episode :limit n))

;;; --- SEMANTIC (replaces *long-term-memory*) ---

(defun holo-store-knowledge! (ctx prediction confidence)
  "Store consolidated knowledge. Replaces storing in *long-term-memory*."
  (let ((p (holo-encode ctx :layer :semantic :content-type :knowledge :ctx ctx)))
    (setf (holo-pattern-content p) (list :ctx ctx :prediction prediction :confidence confidence))
    (holo-store! p)))

(defun holo-lookup (ctx)
  "Lookup knowledge for context. Replaces lookup-consolidated."
  (let ((results (holo-retrieve ctx :layer :semantic :content-type :knowledge :limit 3)))
    (when results
      (let* ((best (cdar results))  ; highest similarity
             (content (holo-pattern-content best)))
        (values (getf content :prediction)
                (getf content :confidence))))))

;;; --- PATTERN (replaces *pattern-stats*, *pattern-signatures*) ---

(defun holo-store-pattern-stats! (ctx hits misses)
  "Store pattern statistics."
  (let ((p (holo-encode ctx :layer :pattern :content-type :pattern-stats :ctx ctx)))
    (setf (holo-pattern-hits p) hits)
    (setf (holo-pattern-misses p) misses)
    (holo-store! p)))

(defun holo-get-pattern-stats (ctx)
  "Get pattern stats. Returns (hits . misses) or nil."
  (let ((results (holo-retrieve ctx :layer :pattern :content-type :pattern-stats :limit 1)))
    (when results
      (let ((p (cdar results)))
        (cons (holo-pattern-hits p) (holo-pattern-misses p))))))

(defun holo-update-pattern-stats! (ctx hit-p)
  "Update pattern stats based on hit/miss."
  (let ((existing (holo-retrieve ctx :layer :pattern :content-type :pattern-stats :limit 1)))
    (if existing
        (let ((p (cdar existing)))
          (if hit-p
              (incf (holo-pattern-hits p))
              (incf (holo-pattern-misses p))))
        ;; Create new
        (holo-store-pattern-stats! ctx (if hit-p 1 0) (if hit-p 0 1)))))

;;; --- TRACE (replaces *trace-buffer*, *cognitive-trace-buffer*) ---

(defun holo-store-trace! (trace)
  "Store a cognitive trace."
  (let ((ctx (if (listp trace) trace (list 'trace trace))))
    (holo-store! (holo-encode ctx :layer :trace :content-type :trace))))

(defun holo-recent-traces (&optional (n 20))
  "Get recent traces."
  (mapcar #'holo-pattern-content (holo-query nil :layer :trace :limit n)))

;;; ============================================================================
;;; UNIFIED QUERY - SEARCH ACROSS ALL LAYERS
;;; ============================================================================

(defun holo-search (query &key layers content-types (limit 20))
  "Search across multiple layers and types.
   Returns unified results sorted by similarity."
  (let ((all-results nil))
    (dolist (layer (or layers '(:immediate :working :situational :episodic :semantic :pattern :trace)))
      (if content-types
          (dolist (ctype content-types)
            (dolist (r (holo-retrieve query :layer layer :content-type ctype :limit limit))
              (push (list :layer layer :type ctype :sim (car r) :pattern (cdr r)) all-results)))
          (dolist (r (holo-retrieve query :layer layer :limit limit))
            (push (list :layer layer :sim (car r) :pattern (cdr r)) all-results))))
    ;; Sort by similarity
    (sort all-results #'> :key (lambda (r) (getf r :sim)))))

;;; ============================================================================
;;; STATISTICS & DIAGNOSTICS
;;; ============================================================================

(defun holo-stats ()
  "Get holographic memory statistics."
  (list :enabled *holo-enabled*
        :total-patterns (hash-table-count (holo-store-patterns *holo*))
        :total-stored (holo-store-total-stored *holo*)
        :total-retrieved (holo-store-total-retrieved *holo*)
        :total-decayed (holo-store-total-decayed *holo*)
        :by-layer (let ((counts nil))
                    (maphash (lambda (layer ids)
                               (push (cons layer (length ids)) counts))
                             (holo-store-by-layer *holo*))
                    counts)
        :by-type (let ((counts nil))
                   (maphash (lambda (ctype ids)
                              (push (cons ctype (length ids)) counts))
                            (holo-store-by-type *holo*))
                   counts)))

(defun holo-print-status ()
  "Print holographic memory status."
  (let ((stats (holo-stats)))
    (format t "~%=== HOLOGRAPHIC SUBSTRATE STATUS ===~%")
    (format t "Enabled: ~A~%" (getf stats :enabled))
    (format t "Total patterns: ~D~%" (getf stats :total-patterns))
    (format t "Stored: ~D  Retrieved: ~D  Decayed: ~D~%"
            (getf stats :total-stored)
            (getf stats :total-retrieved)
            (getf stats :total-decayed))
    (format t "~%By layer:~%")
    (dolist (lc (getf stats :by-layer))
      (format t "  ~A: ~D~%" (car lc) (cdr lc)))
    (format t "~%By content type:~%")
    (dolist (tc (getf stats :by-type))
      (format t "  ~A: ~D~%" (car tc) (cdr tc)))))

;;; ============================================================================
;;; RESET
;;; ============================================================================

(defun holo-reset! ()
  "Reset the holographic store."
  (setf *holo* (make-holo-store))
  (format t "Holographic substrate reset.~%"))

;;; ============================================================================
;;; MODULE INFO
;;; ============================================================================

(format t "~%================================================================~%")
(format t "UHMA HOLOGRAPHIC SUBSTRATE v2~%")
(format t "================================================================~%")
(format t "Replaces 7 memory layers with unified holographic store:~%")
(format t "  1. Immediate  (decay: 0.15) - recent outcomes~%")
(format t "  2. Working    (decay: 0.08) - active reasoning~%")
(format t "  3. Situational(decay: 0.03) - situation history~%")
(format t "  4. Episodic   (decay: 0.005)- autobiographical~%")
(format t "  5. Semantic   (decay: 0.0)  - permanent knowledge~%")
(format t "  6. Pattern    (decay: 0.01) - learned patterns~%")
(format t "  7. Trace      (decay: 0.1)  - cognitive traces~%")
(format t "~%Core principle: Memory stored BETWEEN experts, not IN them~%")
(format t "================================================================~%")
