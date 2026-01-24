(in-package :uhma)

(defun context-embedding (ctx)
  "STUB: Returns a vector embedding for the given context."
  (declare (ignore ctx))
  (make-vsa-vec)) ; Return a zero vector for now

(defun random-vec ()
  (let ((v (make-vsa-vec))) ; Use VSA's vector creation
    (dotimes (i *emb-dim*) 
      (setf (aref v i) (- (random 2.0) 1.0)))
    (vnorm! v)))

(defun get-emb (tok)
  (or (gethash tok *embeddings*)
      (setf (gethash tok *embeddings*) (random-vec))))

(defun vcopy (v)
  (let ((n (make-array (length v) :element-type 'single-float)))
    (dotimes (i (length v)) (setf (aref n i) (aref v i)))
    n))

(defun attract! (tok toward lr)
  (let ((tv (get-emb tok)) (cv (get-emb toward)))
    (dotimes (i *emb-dim*)
      (incf (aref tv i) (* lr (- (aref cv i) (aref tv i)))))
    (vnorm! tv)))

(defun split-string-words (text)
  "Splits a string into words by whitespace and punctuation."
  (unless (stringp text) (return-from split-string-words nil))
  (let ((words nil)
        (current-word nil))
    (loop for char across text do
      (if (alphanumericp char)
          (push char current-word)
          (when current-word
            (push (string-downcase (coerce (nreverse current-word) 'string)) words)
            (setf current-word nil))))
    (when current-word
      (push (string-downcase (coerce (nreverse current-word) 'string)) words))
    (nreverse words)))

(defun tokenize (text)
  (let ((result nil)
        (words (split-string-words text)))
    (dolist (word words)
      (if (and *morpheme-model* (> (length word) 6))
          (dolist (m (segment-word word *morpheme-model*))
            (push m result))
          (push (intern (string-upcase word)) result)))
    (nreverse result)))

(defun learn-morphemes! (text morpheme-model)
  "STUB: Learns morphemes from the given text using the provided model."
  (declare (ignore text morpheme-model))
  nil)

;;; ============================================================================
;;; TYPE EMERGENCE (unchanged)
;;; ============================================================================

(defun update-token-context! (token context)
  (let ((ctx-dist (gethash token *token-contexts*)))
    (unless ctx-dist
      (setf ctx-dist (make-hash-table :test 'eq))
      (setf (gethash token *token-contexts*) ctx-dist))
    (dolist (ctx-tok context)
      (incf (gethash ctx-tok ctx-dist 0)))))

(defun compute-context-similarity (tok1 tok2)
  (let ((ctx1 (gethash tok1 *token-contexts*))
        (ctx2 (gethash tok2 *token-contexts*)))
    (unless (and ctx1 ctx2)
      (return-from compute-context-similarity 0.0))
    (let ((dot 0.0) (mag1 0.0) (mag2 0.0))
      (loop for k being the hash-keys of ctx1
            using (hash-value v1)
            do (let ((v2 (gethash k ctx2 0)))
                 (incf dot (* v1 v2))
                 (incf mag1 (* v1 v1))))
      (loop for v2 being the hash-values of ctx2
            do (incf mag2 (* v2 v2)))
      (if (and (> mag1 0) (> mag2 0))
          (/ dot (sqrt (* mag1 mag2)))
          0.0))))

(defun get-type (prediction)
  (or (gethash prediction *type-registry*)
      (let ((new-type (incf *type-counter*)))
        (setf (gethash prediction *type-registry*) new-type)
        (setf (gethash new-type *type-clusters*)
              (make-type-cluster 
               :id new-type
               :members (list prediction)
               :centroid (vcopy (get-emb prediction))
               :created-at *step*))
        new-type)))

(defun same-type-p (p1 p2)
  (and p1 p2 (eql (get-type p1) (get-type p2))))

(defun update-type-cluster! (prediction context)
  (let* ((type-id (get-type prediction))
         (cluster (gethash type-id *type-clusters*)))
    (when cluster
      (incf (type-cluster-access-count cluster))
      (let ((pred-emb (get-emb prediction))
            (centroid (type-cluster-centroid cluster)))
        (when (and pred-emb centroid)
          (dotimes (i *emb-dim*)
            (setf (aref centroid i) 
                  (+ (* 0.95 (aref centroid i))
                     (* 0.05 (aref pred-emb i)))))
          (vnorm! centroid)))
      (when context
        (dolist (ctx-tok context)
          (incf (gethash ctx-tok (type-cluster-context-distribution cluster) 0)))
        (pushnew (first context) (type-cluster-prediction-contexts cluster))
        (when (> (length (type-cluster-prediction-contexts cluster)) 50)
          (setf (type-cluster-prediction-contexts cluster)
                (subseq (type-cluster-prediction-contexts cluster) 0 50)))))))

(defun compute-type-similarity (type1-id type2-id)
  (let ((c1 (gethash type1-id *type-clusters*))
        (c2 (gethash type2-id *type-clusters*)))
    (unless (and c1 c2)
      (return-from compute-type-similarity 0.0))
    (let ((emb-sim 0.0)
          (ctx-sim 0.0)
          (has-context-data nil))
      (when (and (type-cluster-centroid c1) (type-cluster-centroid c2))
        (setf emb-sim (cosim (type-cluster-centroid c1) (type-cluster-centroid c2))))
      (let ((dist1 (type-cluster-context-distribution c1))
            (dist2 (type-cluster-context-distribution c2)))
        (when (and (> (hash-table-count dist1) 2) (> (hash-table-count dist2) 2))
          (setf has-context-data t)
          (let ((dot 0.0) (mag1 0.0) (mag2 0.0))
            (loop for k being the hash-keys of dist1
                  using (hash-value v1)
                  do (let ((v2 (gethash k dist2 0)))
                       (incf dot (* v1 v2))
                       (incf mag1 (* v1 v1))))
            (loop for v2 being the hash-values of dist2
                  do (incf mag2 (* v2 v2)))
            (when (and (> mag1 0) (> mag2 0))
              (setf ctx-sim (/ dot (sqrt (* mag1 mag2))))))))
      (if has-context-data
          (+ (* (- 1 *type-context-weight*) emb-sim)
             (* *type-context-weight* ctx-sim))
          emb-sim))))

(defun merge-types! (type1-id type2-id)
  (let ((c1 (gethash type1-id *type-clusters*))
        (c2 (gethash type2-id *type-clusters*)))
    (when (and c1 c2)
      (dolist (member (type-cluster-members c2))
        (setf (gethash member *type-registry*) type1-id)
        (pushnew member (type-cluster-members c1)))
      (loop for k being the hash-keys of (type-cluster-context-distribution c2)
            using (hash-value v)
            do (incf (gethash k (type-cluster-context-distribution c1) 0) v))
      (dolist (ctx (type-cluster-prediction-contexts c2))
        (pushnew ctx (type-cluster-prediction-contexts c1)))
      (let ((n1 (length (type-cluster-members c1)))
            (n2 (length (type-cluster-members c2))))
        (when (and (type-cluster-centroid c1) (type-cluster-centroid c2))
          (dotimes (i *emb-dim*)
            (setf (aref (type-cluster-centroid c1) i)
                  (/ (+ (* n1 (aref (type-cluster-centroid c1) i))
                        (* n2 (aref (type-cluster-centroid c2) i)))
                     (+ n1 n2))))
          (vnorm! (type-cluster-centroid c1))))
      (remhash type2-id *type-clusters*))))

(defun recluster-types! ()
  (when (< (- *step* *last-recluster*) *type-recluster-interval*)
    (return-from recluster-types! 0))
  (setf *last-recluster* *step*)
  (let ((type-ids (loop for k being the hash-keys of *type-clusters* collect k))
        (merges 0))
    (loop for i from 0 below (length type-ids)
          do (loop for j from (1+ i) below (length type-ids)
                   for t1 = (nth i type-ids)
                   for t2 = (nth j type-ids)
                   for sim = (compute-type-similarity t1 t2)
                   when (and (> sim *type-cluster-threshold*)
                             (gethash t1 *type-clusters*)
                             (gethash t2 *type-clusters*))
                   do (merge-types! t1 t2)
                      (incf merges)))
    merges))

(defun print-type-clusters ()
  (format t "~%========== Type Clusters ==========~%")
  (format t "Total types: ~A~%" (hash-table-count *type-clusters*))
  (let ((clusters nil))
    (loop for id being the hash-keys of *type-clusters*
          using (hash-value cluster)
          do (push (cons id cluster) clusters))
    (setf clusters (sort clusters #'> 
                         :key #'(lambda (c) (length (type-cluster-members (cdr c))))))
    (loop for (id . cluster) in (subseq clusters 0 (min 10 (length clusters)))
          do (format t "Type ~A: ~A members, ~A accesses, ~A ctx-keys~%"
                     id 
                     (length (type-cluster-members cluster))
                     (type-cluster-access-count cluster)
                     (hash-table-count (type-cluster-context-distribution cluster)))
             (format t "  Members: ~{~A~^, ~}~%"
                     (subseq (type-cluster-members cluster) 
                             0 (min 8 (length (type-cluster-members cluster)))))))
  (format t "====================================~%"))

;;; ============================================================================
;;; MULTI-SCALE CONTEXT (unchanged)
;;; ============================================================================

(defun make-context-key (ctx scale)
  (when (and ctx (>= (length ctx) scale))
    (subseq ctx 0 scale)))

(defun context-owner (ctx)
  "Find owner checking all context sizes from longest to shortest."
  (when ctx
    (loop for scale from (length ctx) downto 1
          for key = (subseq ctx 0 scale)
          for owner = (gethash key *context-owners*)
          when owner return owner)))


;;; ============================================================================
;;; EXPERT STRUCT (moved for forward reference resolution)
;;; ============================================================================


;;; ============================================================================
;;; OP-METRICS STRUCT (moved for forward reference resolution)
;;; ============================================================================



(defun claim-context! (expert ctx)
  "Claim context at all sizes from 1 to context length."
  (when ctx
    (loop for scale from 1 to (length ctx)
          for key = (subseq ctx 0 scale)
          do (unless (gethash key *context-owners*)
               (setf (gethash key *context-owners*) (expert-id expert))
               (pushnew key (expert-owned-contexts expert) :test #'equal)))))

;;; ============================================================================
;;; HOOK SYSTEM
;;; ============================================================================

(defun register-hook (hook-name function &key (priority 50))
  "Register a function to be called when a hook fires."
  (let ((entry (cons priority function))
        (current (gethash hook-name *hooks*)))
    (setf (gethash hook-name *hooks*)
          (sort (cons entry current) #'< :key #'car))
    entry))

(defun unregister-hook (hook-name handle)
  "Remove a previously registered hook handler."
  (setf (gethash hook-name *hooks*)
        (remove handle (gethash hook-name *hooks*) :test #'eq)))

(defun clear-all-hooks ()
  "Remove all hook handlers."
  (clrhash *hooks*))

(defun run-hook (hook-name &rest args)
  "Fire a hook, calling all handlers in priority order."
  (incf (gethash hook-name *hook-fire-counts* 0))
  (let ((result nil))
    (dolist (entry (gethash hook-name *hooks*))
      (setf result (apply (cdr entry) args)))
    result))

(defun run-hook-until (hook-name predicate &rest args)
  "Fire hook until a handler returns value satisfying PREDICATE."
  (dolist (entry (gethash hook-name *hooks*))
    (let ((result (apply (cdr entry) args)))
      (when (funcall predicate result)
        (return-from run-hook-until result))))
  nil)

(defconstant +hook-pre-learn+ 'pre-learn)
(defconstant +hook-post-learn+ 'post-learn)
(defconstant +hook-should-learn+ 'should-learn)
(defconstant +hook-pre-process-token+ 'pre-process-token)
(defconstant +hook-post-process-token+ 'post-process-token)
(defconstant +hook-should-verify+ 'should-verify)
(defconstant +hook-maintenance+ 'maintenance)
(defconstant +hook-expert-spawned+ 'expert-spawned)
(defconstant +hook-expert-dying+ 'expert-dying)
(defconstant +hook-program-inherited+ 'program-inherited)
(defconstant +hook-pre-reset+ 'pre-reset)
(defconstant +hook-post-reset+ 'post-reset)
(defconstant +hook-program-modified+ 'program-modified)
(defconstant +hook-pre-modification+ 'pre-modification)
(defconstant +hook-post-modification+ 'post-modification)
(defconstant +hook-error+ 'error)
(defconstant +hook-expert-activated+ 'expert-activated)

;;; ============================================================================
;;; NEIGHBORHOODS (unchanged)
;;; ============================================================================


(defun create-root-neighborhood ()
  (let ((root (make-neighborhood :centroid (random-vec))))
    (setf *root-neighborhood* root)
    (push root *neighborhoods*)
    root))

(defun compute-neighborhood-diversity (nbhd)
  (let ((experts (remove nil (mapcar #'find-expert-by-id (neighborhood-experts nbhd)))))
    (when (< (length experts) 2)
      (return-from compute-neighborhood-diversity 0.0))
    (let ((sum-sim 0.0) (count 0))
      (loop for i from 0 below (length experts)
            do (loop for j from (1+ i) below (length experts)
                     for e1 = (nth i experts)
                     for e2 = (nth j experts)
                     when (and (expert-centroid e1) (expert-centroid e2))
                     do (incf sum-sim (cosim (expert-centroid e1) (expert-centroid e2)))
                        (incf count)))
      (if (> count 0)
          (- 1.0 (/ sum-sim count))
          0.0))))

(defun find-best-neighborhood (expert)
  (let ((expert-emb (expert-centroid expert))
        (best *root-neighborhood*)
        (best-sim -999.0))
    (when expert-emb
      (labels ((search-tree (nbhd)
                 (when (neighborhood-centroid nbhd)
                   (let ((sim (cosim expert-emb (neighborhood-centroid nbhd))))
                     (when (> sim best-sim)
                       (setf best nbhd best-sim sim))))
                 (dolist (child (neighborhood-children nbhd))
                   (search-tree child))))
        (search-tree *root-neighborhood*)))
    best))

(defun add-expert-to-neighborhood! (expert nbhd)
  (push (expert-id expert) (neighborhood-experts nbhd))
  (setf (expert-neighborhood expert) (neighborhood-id nbhd))
  (when (expert-centroid expert)
    (let ((c (or (neighborhood-centroid nbhd) (random-vec))))
      (dotimes (i *emb-dim*)
        (setf (aref c i) (+ (* 0.95 (aref c i)) 
                            (* 0.05 (aref (expert-centroid expert) i)))))
      (vnorm! c)
      (setf (neighborhood-centroid nbhd) c)))
  (when (or (> (length (neighborhood-experts nbhd)) *neighborhood-max-size*)
            (and (> (length (neighborhood-experts nbhd)) 4)
                 (> (compute-neighborhood-diversity nbhd) *neighborhood-diversity-threshold*)))
    (split-neighborhood! nbhd)))

(defun split-neighborhood! (nbhd)
  (let* ((expert-ids (neighborhood-experts nbhd))
         (experts (remove nil (mapcar #'find-expert-by-id expert-ids)))
         (n (length experts)))
    (when (< n 4) (return-from split-neighborhood! nil))
    (let* ((best-dist 0.0) (seed1 nil) (seed2 nil))
      (loop for i from 0 below n
            for e1 = (nth i experts)
            when (expert-centroid e1)
            do (loop for j from (1+ i) below n
                     for e2 = (nth j experts)
                     when (expert-centroid e2)
                     do (let ((dist (- 1.0 (cosim (expert-centroid e1) (expert-centroid e2)))))
                          (when (> dist best-dist)
                            (setf best-dist dist seed1 e1 seed2 e2)))))
      (unless (and seed1 seed2)
        (return-from split-neighborhood! nil))
      (let ((c1 (vcopy (expert-centroid seed1)))
            (c2 (vcopy (expert-centroid seed2)))
            (group1 nil) (group2 nil))
        (dolist (e experts)
          (let ((emb (or (expert-centroid e) (random-vec))))
            (if (> (cosim emb c1) (cosim emb c2))
                (push e group1)
                (push e group2))))
        (when (and group1 group2 (>= (length group1) 2) (>= (length group2) 2))
          (let ((child1 (make-neighborhood :parent nbhd :centroid c1 
                                           :depth (1+ (neighborhood-depth nbhd))))
                (child2 (make-neighborhood :parent nbhd :centroid c2
                                           :depth (1+ (neighborhood-depth nbhd)))))
            (push child1 *neighborhoods*)
            (push child2 *neighborhoods*)
            (push child1 (neighborhood-children nbhd))
            (push child2 (neighborhood-children nbhd))
            (dolist (e group1)
              (setf (expert-neighborhood e) (neighborhood-id child1))
              (push (expert-id e) (neighborhood-experts child1)))
            (dolist (e group2)
              (setf (expert-neighborhood e) (neighborhood-id child2))
              (push (expert-id e) (neighborhood-experts child2)))
            (setf (neighborhood-experts nbhd) nil)
            t))))))

(defun find-experts-in-neighborhood (nbhd)
  (let ((result nil))
    (labels ((collect (n)
               (dolist (eid (neighborhood-experts n))
                 (let ((e (find-expert-by-id eid)))
                   (when e (push e result))))
               (dolist (child (neighborhood-children n))
                 (collect child))))
      (collect nbhd))
    result))

(defun route-to-neighborhood (ctx)
  (let ((ctx-emb (context-embedding ctx))
        (best *root-neighborhood*)
        (best-sim -999.0))
    (when ctx-emb
      (labels ((search-tree (nbhd)
                 (when (neighborhood-centroid nbhd)
                   (let ((sim (cosim ctx-emb (neighborhood-centroid nbhd))))
                     (when (> sim best-sim)
                       (setf best nbhd best-sim sim))))
                 (dolist (child (neighborhood-children nbhd))
                   (search-tree child))))
        (search-tree *root-neighborhood*)))
    best))

;;; ============================================================================
;;; TRAVELERS (unchanged)
;;; ============================================================================


(defun find-traveler-for-expert (expert-id)
  (find expert-id *travelers* :key #'traveler-expert-id))

(defun expert-is-traveler-p (expert)
  (find-traveler-for-expert (expert-id expert)))

(defun compute-knowledge-overlap (e1 e2)
  (let ((k1 (expert-knowledge e1))
        (k2 (expert-knowledge e2))
        (shared 0) (total 0))
    (maphash (lambda (k v) (declare (ignore v))
               (incf total)
               (when (gethash k k2) (incf shared)))
             k1)
    (maphash (lambda (k v) (declare (ignore k v))
               (incf total))
             k2)
    (if (> total 0)
        (/ shared (max 1 (- total shared)))
        0.0)))

(defun find-bridge-candidates (expert)
  (let ((home-nbhd (find-neighborhood-by-id (expert-neighborhood expert)))
        (candidates nil))
    (when (and home-nbhd (expert-centroid expert))
      (dolist (nbhd *neighborhoods*)
        (when (and (not (eq nbhd home-nbhd))
                   (neighborhood-centroid nbhd))
          (let ((emb-sim (cosim (expert-centroid expert) (neighborhood-centroid nbhd))))
            (when (> emb-sim *traveler-bridge-threshold*)
              (push (cons nbhd emb-sim) candidates))
            (unless (member nbhd (mapcar #'car candidates))
              (dolist (other-eid (neighborhood-experts nbhd))
                (let ((other (find-expert-by-id other-eid)))
                  (when (and other (> (compute-knowledge-overlap expert other)
                                      *traveler-knowledge-overlap-threshold*))
                    (push (cons nbhd 0.5) candidates)
                    (return)))))))))
    (mapcar #'car 
            (subseq (sort (remove-duplicates candidates :key #'car) #'> :key #'cdr)
                    0 (min *traveler-max-neighborhoods* (length candidates))))))

(defun create-traveler! (expert bridge-neighborhoods &optional pattern)
  (let ((existing (find-traveler-for-expert (expert-id expert))))
    (if existing
        (progn
          (dolist (nbhd bridge-neighborhoods)
            (pushnew (neighborhood-id nbhd) (traveler-visited-neighborhoods existing)))
          (when pattern
            (pushnew pattern (traveler-bridge-patterns existing) :test #'equal))
          existing)
        (let ((trav (make-traveler
                     :expert-id (expert-id expert)
                     :home-neighborhood (expert-neighborhood expert)
                     :visited-neighborhoods (mapcar #'neighborhood-id bridge-neighborhoods)
                     :bridge-patterns (when pattern (list pattern))
                     :created-at *step*)))
          (push trav *travelers*)
          (dolist (nbhd bridge-neighborhoods)
            (pushnew (expert-id expert) (neighborhood-experts nbhd)))
          trav))))

(defun detect-and-create-travelers! ()
  (when (< (- *step* *last-traveler-pass*) *traveler-creation-interval*)
    (return-from detect-and-create-travelers! 0))
  (setf *last-traveler-pass* *step*)
  (let ((created 0))
    (dolist (e *experts*)
      (when (and (> (expert-life e) 0.5)
                 (> (hash-table-count (expert-knowledge e)) 3)
                 (not (expert-is-traveler-p e)))
        (let ((bridges (find-bridge-candidates e)))
          (when bridges
            (create-traveler! e bridges)
            (incf created)))))
    (dolist (e1 *experts*)
      (when (and (> (expert-life e1) 0.5)
                 (> (hash-table-count (expert-knowledge e1)) 5)
                 (not (expert-is-traveler-p e1)))
        (dolist (e2 *experts*)
          (when (and (not (eq e1 e2))
                     (> (compute-knowledge-overlap e1 e2) *traveler-knowledge-overlap-threshold*))
            (let ((nbhd (find-neighborhood-by-id (expert-neighborhood e2))))
              (when nbhd
                (create-traveler! e1 (list nbhd))
                (incf created)
                (return)))))))
    created))

(defun find-travelers-for-context (ctx)
  (let ((ctx-emb (context-embedding ctx))
        (matching nil))
    (when ctx-emb
      (dolist (trav *travelers*)
        (let ((expert (find-expert-by-id (traveler-expert-id trav))))
          (when (and expert (expert-centroid expert))
            (let ((sim (cosim ctx-emb (expert-centroid expert))))
              (when (> sim 0.4)
                (push (cons trav sim) matching)))))))
    (mapcar #'car (sort matching #'> :key #'cdr))))

(defun use-traveler-bridge! (traveler)
  (incf (traveler-bridge-count traveler)))

(defun print-travelers ()
  (format t "~%========== Travelers ==========~%")
  (format t "Total travelers: ~A~%" (length *travelers*))
  (dolist (trav (sort (copy-list *travelers*) #'> :key #'traveler-bridge-count))
    (format t "Traveler ~A (E~A): bridges=~A, uses=~A~%"
            (traveler-id trav)
            (traveler-expert-id trav)
            (length (traveler-visited-neighborhoods trav))
            (traveler-bridge-count trav)))
  (format t "================================~%"))

;;; ============================================================================
;;; MEMORY CONSOLIDATION (unchanged)
;;; ============================================================================


(defun consolidate-pattern! (key prediction)
  (let ((pattern (make-consolidated-pattern
                  :key key :prediction prediction
                  :confidence 1.0 :consolidated-at *step* :last-access *step*)))
    (setf (gethash key *long-term-memory*) pattern)))

(defun lookup-consolidated (ctx)
  (dolist (scale (reverse *context-scales*))
    (let* ((key (make-context-key ctx scale))
           (pattern (when key (gethash key *long-term-memory*))))
      (when pattern
        (incf (consolidated-pattern-access-count pattern))
        (setf (consolidated-pattern-last-access pattern) *step*)
        (return-from lookup-consolidated 
          (values (consolidated-pattern-prediction pattern)
                  (consolidated-pattern-confidence pattern)
                  :consolidated)))))
  nil)

(defun update-pattern-stats! (key correct)
  (let ((stats (gethash key *pattern-stats*)))
    (if stats
        (progn (incf (car stats)) (when correct (incf (cdr stats))))
        (setf (gethash key *pattern-stats*) (cons 1 (if correct 1 0))))))

(defun get-pattern-accuracy (key)
  "Get accuracy (hits/total) for a pattern from *pattern-stats*."
  (let ((stats (gethash key *pattern-stats*)))
    (if (and stats (> (car stats) 0))
        (/ (float (cdr stats)) (car stats))
        0.0)))

(defun context-prefix-p (shorter longer)
  "Is SHORTER a prefix of LONGER? (context builds by prepending, so prefix = more general)
   Example: (SAT) is prefix of (SAT CAT THE) - same first elements."
  (and (listp shorter) (listp longer)
       (< (length shorter) (length longer))
       (equal shorter (subseq longer 0 (length shorter)))))

(defun ltm-pattern-subsumes-p (general-key general-pred specific-key specific-pred)
  "Does the GENERAL pattern subsume the SPECIFIC one?
   True if: same prediction AND general-key is prefix of specific-key
   AND general has >= accuracy than specific.
   Example: (SAT)->ON subsumes (SAT CAT THE)->ON because (SAT) is prefix."
  (and (equal general-pred specific-pred)
       (context-prefix-p general-key specific-key)
       (>= (get-pattern-accuracy general-key)
           (get-pattern-accuracy specific-key))))

(defun find-subsuming-pattern (key prediction)
  "Find an existing LTM pattern that subsumes this one (shorter, same prediction, >= accuracy)."
  (maphash (lambda (existing-key existing-pattern)
             (when (and (not (equal existing-key key))
                        (ltm-pattern-subsumes-p existing-key
                                           (consolidated-pattern-prediction existing-pattern)
                                           key prediction))
               (return-from find-subsuming-pattern existing-key)))
           *long-term-memory*)
  nil)

(defun find-subsumed-patterns (key prediction)
  "Find LTM patterns that would be subsumed by this new one (longer, same prediction, <= accuracy)."
  (let ((subsumed nil))
    (maphash (lambda (existing-key existing-pattern)
               (when (and (not (equal existing-key key))
                          (ltm-pattern-subsumes-p key prediction
                                             existing-key
                                             (consolidated-pattern-prediction existing-pattern)))
                 (push existing-key subsumed)))
             *long-term-memory*)
    subsumed))

(defun prune-subsumed-patterns! ()
  "Remove LTM entries that are subsumed by more general patterns with equal or better accuracy.
   'More with less' - keep patterns that predict accurately with minimal context."
  (let ((to-remove nil))
    (maphash (lambda (specific-key specific-pattern)
               (maphash (lambda (general-key general-pattern)
                          (when (ltm-pattern-subsumes-p
                                 general-key (consolidated-pattern-prediction general-pattern)
                                 specific-key (consolidated-pattern-prediction specific-pattern))
                            (push specific-key to-remove)))
                        *long-term-memory*))
             *long-term-memory*)
    (let ((unique-removes (remove-duplicates to-remove :test #'equal)))
      (dolist (k unique-removes)
        (remhash k *long-term-memory*))
      (length unique-removes))))

(defun run-consolidation! ()
  (when (< (- *step* *last-consolidation*) *consolidation-interval*)
    (return-from run-consolidation! nil))
  (setf *last-consolidation* *step*)
  (let* ((presence-pri (if (fboundp 'presence-consolidation-priority) (presence-consolidation-priority nil 0) 0.5))
         (age-threshold (min *consolidation-age* (max 5 (floor *step* 20))))
         (strength-threshold (max 1 (floor (* (min *consolidation-strength* (max 2 (floor *step* 50)))
                                              (- 1.5 presence-pri)))))  ; vivid = easier consolidation
         (added 0)
         (pruned 0))
    (dolist (e *experts*)
      (maphash (lambda (key prediction)
                 (let ((stats (gethash key *pattern-stats*)))
                   (when (and stats
                              (> (car stats) age-threshold)
                              (> (cdr stats) strength-threshold))
                     ;; Check if already subsumed by a more general pattern
                     (let ((subsuming (find-subsuming-pattern key prediction)))
                       (if subsuming
                           ;; Don't add - already covered by more general pattern
                           nil
                           ;; Add new pattern
                           (progn
                             ;; First remove any patterns this one subsumes
                             (let ((subsumed (find-subsumed-patterns key prediction)))
                               (dolist (k subsumed)
                                 (remhash k *long-term-memory*)
                                 (incf pruned)))
                             ;; Then add if not already present
                             (unless (gethash key *long-term-memory*)
                               (consolidate-pattern! key prediction)
                               (incf added))))))))
               (expert-knowledge e)))
    ;; Final pruning pass for any remaining redundancy
    (incf pruned (prune-subsumed-patterns!))
    (values added pruned)))

;;; ============================================================================
;;; COMPRESSION/PRUNING (unchanged)
;;; ============================================================================

(defun compute-expert-similarity (e1 e2)
  (let ((knowledge-sim 0.0)
        (embedding-sim 0.0)
        (k1 (expert-knowledge e1))
        (k2 (expert-knowledge e2)))
    (let ((shared 0) (total 0))
      (maphash (lambda (k v) (declare (ignore v))
                 (incf total)
                 (when (gethash k k2) (incf shared)))
               k1)
      (maphash (lambda (k v) (declare (ignore k v)) (incf total)) k2)
      (when (> total 0)
        (setf knowledge-sim (/ shared (max 1 (- total shared))))))
    (when (and (expert-centroid e1) (expert-centroid e2))
      (setf embedding-sim (cosim (expert-centroid e1) (expert-centroid e2))))
    (+ (* 0.6 knowledge-sim) (* 0.4 embedding-sim))))

(defun merge-experts! (keep discard)
  (maphash (lambda (k v)
             (unless (gethash k (expert-knowledge keep))
               (setf (gethash k (expert-knowledge keep)) v)))
           (expert-knowledge discard))
  (dolist (ctx (expert-owned-contexts discard))
    (setf (gethash ctx *context-owners*) (expert-id keep))
    (pushnew ctx (expert-owned-contexts keep) :test #'equal))
  (incf (expert-hits keep) (expert-hits discard))
  (incf (expert-misses keep) (expert-misses discard))
  (when (and (expert-centroid keep) (expert-centroid discard))
    (dotimes (i *emb-dim*)
      (setf (aref (expert-centroid keep) i)
            (/ (+ (aref (expert-centroid keep) i)
                  (aref (expert-centroid discard) i)) 2.0)))
    (vnorm! (expert-centroid keep)))
  (let ((nbhd (find-neighborhood-by-id (expert-neighborhood discard))))
    (when nbhd
      (setf (neighborhood-experts nbhd)
            (remove (expert-id discard) (neighborhood-experts nbhd)))))
  (let ((trav (find-traveler-for-expert (expert-id discard))))
    (when trav (setf *travelers* (remove trav *travelers*))))
  (setf *experts* (remove discard *experts*))
  (incf *experts-merged*))

(defun merge-similar-experts! (&optional (threshold *merge-similarity-threshold*))
  (let ((merged 0)
        (candidates (remove-if (lambda (e) (<= (expert-life e) *death-threshold*)) *experts*)))
    (loop while t do
      (let ((found-pair nil))
        (loop named outer
              for i from 0 below (length candidates)
              do (loop for j from (1+ i) below (length candidates)
                       for e1 = (nth i candidates)
                       for e2 = (nth j candidates)
                       for sim = (compute-expert-similarity e1 e2)
                       when (> sim threshold)
                       do (setf found-pair (cons e1 e2))
                          (return-from outer)))
        (if found-pair
            (let* ((e1 (car found-pair)) (e2 (cdr found-pair))
                   (keep (if (> (expert-life e1) (expert-life e2)) e1 e2))
                   (discard (if (eq keep e1) e2 e1)))
              (merge-experts! keep discard)
              (setf candidates (remove discard candidates))
              (incf merged))
            (return))))
    merged))

(defun prune-weak-patterns! (&optional (threshold *pattern-prune-threshold*))
  (let ((pruned 0))
    (dolist (e *experts*)
      (let ((to-remove nil))
        (maphash (lambda (key val) (declare (ignore val))
                   (let ((stats (gethash key *pattern-stats*)))
                     (when (and stats (< (cdr stats) threshold))
                       (push key to-remove))))
                 (expert-knowledge e))
        (dolist (k to-remove)
          (remhash k (expert-knowledge e))
          (incf pruned))))
    (incf *patterns-pruned* pruned)
    pruned))

(defun decay-unused-patterns! (&optional (rate *pattern-decay-rate*))
  (when (< (- *step* *last-decay*) *pattern-decay-interval*)
    (return-from decay-unused-patterns! 0))
  (setf *last-decay* *step*)
  (let ((decayed 0) (removed 0) (to-remove nil))
    (maphash (lambda (key pattern)
               (when (> (- *step* (consolidated-pattern-last-access pattern)) 
                        (* 2 *pattern-decay-interval*))
                 (setf (consolidated-pattern-confidence pattern)
                       (* (consolidated-pattern-confidence pattern) rate))
                 (incf decayed)
                 (when (< (consolidated-pattern-confidence pattern) 0.3)
                   (push key to-remove))))
             *long-term-memory*)
    (dolist (k to-remove)
      (remhash k *long-term-memory*)
      (incf removed))
    (values decayed removed)))

(defun compact-neighborhoods! ()
  ;; First pass: remove empty leaf neighborhoods
  (setf *neighborhoods*
        (remove-if (lambda (nbhd)
                     (and (null (neighborhood-experts nbhd))
                          (null (neighborhood-children nbhd))
                          (not (eq nbhd *root-neighborhood*))))
                   *neighborhoods*))
  ;; Second pass: clean up child references
  (dolist (nbhd *neighborhoods*)
    (setf (neighborhood-children nbhd)
          (remove-if-not (lambda (c) (member c *neighborhoods*))
                         (neighborhood-children nbhd))))
  ;; Third pass: merge parent with single child when parent has no experts
  (dolist (nbhd *neighborhoods*)
    (when (and (= (length (neighborhood-children nbhd)) 1)
               (null (neighborhood-experts nbhd))
               (not (eq nbhd *root-neighborhood*)))
      (let ((child (first (neighborhood-children nbhd))))
        ;; Move child's experts and children to parent
        (setf (neighborhood-experts nbhd) (neighborhood-experts child))
        (setf (neighborhood-children nbhd) (neighborhood-children child))
        ;; Update expert references
        (dolist (eid (neighborhood-experts nbhd))
          (let ((e (find-expert-by-id eid)))
            (when e (setf (expert-neighborhood e) (neighborhood-id nbhd)))))
        ;; Remove the merged child
        (setf *neighborhoods* (remove child *neighborhoods*))))))

(defun run-compression! ()
  (when (< (- *step* *last-compression*) *compression-interval*)
    (return-from run-compression! nil))
  (setf *last-compression* *step*)
  (let ((merged (merge-similar-experts!))
        (pruned (prune-weak-patterns!)))
    (decay-unused-patterns!)
    (compact-neighborhoods!)
    (values merged pruned)))

;;; ============================================================================
;;; ATTENTION SYSTEM (unchanged)
;;; ============================================================================

(defun get-attention (ctx)
  (or (gethash (make-context-key ctx 2) *attention-map*) 0.0))

(defun get-encounters (ctx)
  (or (gethash (make-context-key ctx 2) *context-encounters*) 0))

(defun incf-encounters (ctx)
  (let ((key (make-context-key ctx 2)))
    (setf (gethash key *context-encounters*) 
          (1+ (or (gethash key *context-encounters*) 0)))))

(defun update-attention! (ctx correct)
  (let* ((key (make-context-key ctx 2))
         (current (or (gethash key *attention-map*) 0.0)))
    (setf (gethash key *attention-map*)
          (if correct
              (max 0.0 (- (* current *attention-decay*) *attention-hit-decay*))
              (min 1.0 (+ (* current *attention-decay*) *attention-miss-boost*))))
    (push (cons key correct) *recent-outcomes*)
    (when (> (length *recent-outcomes*) (* *attention-window* 3))
      (setf *recent-outcomes* (subseq *recent-outcomes* 0 (* *attention-window* 2))))))

(defun in-focus-p (ctx)
  (> (get-attention ctx) *attention-focus-threshold*))

(defun should-verify-p (ctx confidence)
  (incf-encounters ctx)
  (unless *sparse-enabled* (return-from should-verify-p t))
  (let ((hook-result (run-hook-until +hook-should-verify+ #'identity ctx confidence)))
    (when hook-result
      (ecase hook-result
        (:verify (incf *verifications-performed*) (return-from should-verify-p t))
        (:skip (incf *verifications-skipped*) (return-from should-verify-p nil)))))
  ;; Use adaptive threshold from self-model if available
  (let ((threshold (if (and (boundp '*self-model*) 
                            *self-model* 
                            (self-model-current-sparse-threshold *self-model*))
                       (self-model-current-sparse-threshold *self-model*)
                       *sparse-threshold*)))
    (cond
      ((< (get-encounters ctx) *min-encounters-before-sparse*)
       (incf *verifications-performed*) t)
      ((< confidence threshold)
       (incf *verifications-performed*) t)
      ((in-focus-p ctx)
       (incf *attention-forced-verifications*)
       (incf *verifications-performed*) t)
      (t (incf *verifications-skipped*) nil))))


;;; ============================================================================
;;; EXPERT STRUCTURE - TRUE HOMOICONIC PROGRAMS
;;; ============================================================================
;;;
;;; The expert program is a REAL S-expression that gets EVAL'd.
;;; No hidden lambdas, no registry lookups - the code IS the data.
;;;
;;; Program structure:
;;;   (progn
;;;     (op :name check-ownership
;;;         :body (when-let ((owner (context-owner ctx)))
;;;                 (if (eq owner my-id) (continue) (delegate owner))))
;;;     (op :name assess-confidence
;;;         :modifiable ((scales (4 3 2)))
;;;         :body (do-scales (scale scales)
;;;                 (when-let ((result (gethash (make-key ctx scale) my-knowledge)))
;;;                   (return-op result :confidence (scale-confidence scale)))))
;;;     ...)
;;;
;;; The self-model can:
;;;   - Walk the program tree and read any part
;;;   - Find :modifiable parameters and their values
;;;   - Replace any subexpression
;;;   - Measure which parts execute via :name tags
;;;
;;; ============================================================================
;;; ============================================================================
;;; CODE METRICS
;;; ============================================================================
(defun get-op-metrics (expert op-name)
  "Get or create metrics for an operation"
  (unless (expert-code-metrics expert)
    (setf (expert-code-metrics expert) (make-hash-table :test 'eq)))
  (or (gethash op-name (expert-code-metrics expert))
      (setf (gethash op-name (expert-code-metrics expert))
            (make-op-metrics :name op-name))))

(defun record-op-metric! (expert op-name result-type &optional confidence)
  "Record metrics for an operation execution"
  ;; Add to execution trace for deep-mind
  (push (cons op-name result-type) *execution-trace*)
  ;; Record per-expert metrics
  (let ((m (get-op-metrics expert op-name)))
    (incf (op-metrics-invocations m))
    (setf (op-metrics-last-invoked m) *step*)
    (case result-type
      (:return (incf (op-metrics-returns m))
               (when confidence (incf (op-metrics-total-confidence m) confidence)))
      (:continue (incf (op-metrics-continues m)))
      (:delegate (incf (op-metrics-delegates m)))
      (:spawn (incf (op-metrics-spawns m))))))

;;; ============================================================================
;;; OPERATION RESULT STRUCTURES
;;; ============================================================================


;;; ============================================================================
;;; PROGRAM EVALUATION ENVIRONMENT
;;; ============================================================================
;;;
;;; When we eval a program, we bind these variables in the environment:
;;;   - ctx: the current context (list of recent tokens)
;;;   - my-id: this expert's id
;;;   - my-knowledge: this expert's knowledge hash table
;;;   - my-neighborhood: this expert's neighborhood id
;;;   - my-centroid: this expert's embedding centroid
;;;   - *current-expert*: the expert struct itself (for metrics)
;;;
;;; Special forms available in programs:
;;;   - (op :name NAME :modifiable PARAMS :body BODY) - operation wrapper
;;;   - (return-op VALUE &key confidence scale source) - return a prediction
;;;   - (delegate TARGET-ID) - delegate to another expert
;;;   - (spawn) - spawn a new expert
;;;   - (continue) - continue to next operation
;;;   - (do-scales (VAR SCALES) &body BODY) - iterate over scales
;;;   - (when-let ((VAR EXPR)) &body BODY) - bind if non-nil
;;;
;;; ============================================================================

(defvar *current-expert* nil "The expert currently executing")
(defvar *current-op-name* nil "Name of the currently executing operation")
(defvar *last-answering-expert* nil "Expert that produced the last prediction")
(defvar *execution-trace* nil "List of (op-name . result-type) for current execution")

(defun return-op (value &key (confidence 0.5) scale source)
  (declare (type single-float confidence))
  (setf *last-answering-expert* *current-expert*)
  (when *current-expert*
    ;; Mark expert as used when it answers - not just when it learns
    ;; This prevents inactivity death when verification is skipped
    (setf (expert-last-used *current-expert*) *step*)
    (run-hook +hook-expert-activated+ *current-expert* confidence))
  (throw 'op-result
         (make-op-result :type :return :value value :confidence confidence
                         :scale scale :source source)))

(defun delegate (target-id)
  (throw 'op-result
         (make-op-result :type :delegate :target target-id)))

(defun spawn-new ()
  (throw 'op-result
         (make-op-result :type :spawn)))

(defun continue-op ()
  (throw 'op-result
         (make-op-result :type :continue)))

;;; Scale confidence lookup - dynamic based on scale
(defun scale-confidence (scale)
  "Higher scales = more context = more confidence"
  (min 1.0 (+ 0.5 (* 0.05 scale))))  ; scale 1=0.55, 2=0.6, ... 10=1.0

;;; Helper: make context key
(defun make-key (ctx scale)
  (make-context-key ctx scale))

;;; ============================================================================
;;; SPECIAL FORM MACROS FOR PROGRAMS
;;; ============================================================================

(defmacro op ((&key name modifiable) &body body)
  "Operation wrapper - tracks metrics and allows modification.
   NAME: symbol identifying this operation
   MODIFIABLE: alist of (param-name default-value) that can be tuned
   BODY: the actual operation code"
  (let ((result-var (gensym "RESULT")))
    `(let (,@(mapcar (lambda (m) `(,(first m) ,(second m))) modifiable))
       (let ((*current-op-name* ',name))
         (let ((,result-var (catch 'op-result (progn ,@body nil))))
           (when ,result-var
             (record-op-metric! *current-expert* ',name 
                               (op-result-type ,result-var)
                               (op-result-confidence ,result-var))
             (unless (eq (op-result-type ,result-var) :continue)
               (throw 'program-result ,result-var)))
           ;; If we get here, operation continued
           (record-op-metric! *current-expert* ',name :continue))))))

(defmacro do-scales ((var scales-expr) &body body)
  "Iterate over scales, executing body for each"
  `(dolist (,var ,scales-expr)
     ,@body))

(defmacro when-let (((var expr)) &body body)
  "Bind VAR to EXPR, execute BODY only if non-nil"
  `(let ((,var ,expr))
     (when ,var ,@body)))

;;; ============================================================================
;;; THE DEFAULT PROGRAM - ACTUAL S-EXPRESSION CODE
;;; ============================================================================

(defparameter *default-program*
  '(block expert-main
     (handler-case
         (progn
           ;; Check ownership - if someone owns this context, let them handle it
           (op (:name check-ownership)
             (let ((owner-id (context-owner ctx)))
               (cond 
                 ((null owner-id) nil)  ; no owner, continue
                 ((eql owner-id my-id) nil)  ; I'm owner, continue
                 (t (delegate owner-id)))))  ; delegate to owner
           
           ;; Check consolidated memory
           (op (:name check-consolidated)
             (multiple-value-bind (pred conf src) (lookup-consolidated ctx)
               (when pred
                 (return-op pred :confidence conf :source :consolidated))))
           
           ;; Check travelers for cross-neighborhood knowledge
           (op (:name check-travelers)
             (restart-case
                 (dolist (trav (find-travelers-for-context ctx))
                   (let ((trav-expert (find-expert-by-id (traveler-expert-id trav))))
                     (when (and trav-expert
                                (not (eql (expert-id trav-expert) my-id))
                                (not (member (expert-id trav-expert) *call-stack*)))
                       (dolist (scale (reverse *context-scales*))
                         (let* ((key (make-key ctx scale))
                                (result (when key (gethash key (expert-knowledge trav-expert)))))
                           (when result
                             (use-traveler-bridge! trav)
                             (return-op result :confidence 0.85 :scale scale :source :traveler)))))))
               (skip-travelers () nil)))  ; restart: skip if traveler lookup fails
           
           ;; Multi-scale knowledge lookup - THE CORE PREDICTION
           ;; Scales are DYNAMIC based on actual context length (longest first)
           (op (:name assess-confidence)
             (let ((ctx-len (length ctx)))
               (dolist (scale '(10 9 8 7 6 5 4 3 2 1))
                 (when (<= scale ctx-len)
                   (let* ((key (make-key ctx scale))
                          (result (when key (gethash key my-knowledge))))
                     (when result
                       (return-op result :confidence (scale-confidence scale) 
                                  :scale scale :source :knowledge)))))))
           
           ;; Try similar context matching
           (op (:name try-similar :modifiable ((threshold 0.7)))
             (restart-case
                 (when (and ctx my-centroid)
                   (let ((ctx-emb (context-embedding ctx))
                         (best-val nil) (best-sim 0.0))
                     (loop for k being the hash-keys of my-knowledge
                           using (hash-value v)
                           when k
                           do (let ((k-emb (context-embedding k)))
                                (when k-emb
                                  (let ((sim (cosim ctx-emb k-emb)))
                                    (when (> sim best-sim)
                                      (setf best-sim sim best-val v))))))
                     (when (> best-sim threshold)
                       (return-op best-val :confidence (* 0.5 best-sim) :source :similar))))
               (use-default-threshold ()  ; restart: fall back to 0.5 threshold
                 (when (and ctx my-centroid)
                   (let ((ctx-emb (context-embedding ctx))
                         (best-val nil) (best-sim 0.0))
                     (loop for k being the hash-keys of my-knowledge
                           using (hash-value v)
                           when k
                           do (let ((k-emb (context-embedding k)))
                                (when k-emb
                                  (let ((sim (cosim ctx-emb k-emb)))
                                    (when (> sim best-sim)
                                      (setf best-sim sim best-val v))))))
                     (when (> best-sim 0.5)
                       (return-op best-val :confidence (* 0.4 best-sim) :source :similar-fallback)))))))
           
           ;; Check type routing
           (op (:name check-type)
             (let ((my-type (expert-owned-type *current-expert*)))
               (when my-type
                 (let* ((input (first ctx))
                        (input-type (when input (get-type input))))
                   (when (and input-type (not (eql my-type input-type)))
                     (let ((better (find-expert-for-type input-type)))
                       (when (and better (not (eql (expert-id better) my-id)))
                         (delegate (expert-id better)))))))))
           
           ;; Request help or spawn
           (op (:name request-help)
             (let* ((nbhd (find-neighborhood-by-id my-neighborhood))
                    (helpers (when nbhd (find-experts-in-neighborhood nbhd))))
               (dolist (h helpers)
                 (when (and (not (eql (expert-id h) my-id))
                            (not (member (expert-id h) *call-stack*))
                            (> (expert-life h) 0.1))
                   (delegate (expert-id h)))))
             ;; No helper found, try similarity
             (let ((helper (find-expert-by-similarity ctx)))
               (if (and helper 
                        (not (eql (expert-id helper) my-id))
                        (not (member (expert-id helper) *call-stack*)))
                   (delegate (expert-id helper))
                   (spawn-new)))))
       
       ;; Error handler - recover from failures
       (expert-error (c)
         (expert-warn (format nil "Expert ~A caught error: ~A" my-id c))
         (spawn-new))  ; if we error out, spawn a new expert to handle it
       
       (error (c)
         (expert-warn (format nil "Expert ~A unexpected error: ~A" my-id c))
         nil)))  ; return nil on unexpected errors
  "The default expert program - uses block/handler-case/restarts")

;;; ============================================================================
;;; EXTENDED OPERATIONS (P19) - AS S-EXPRESSIONS
;;; ============================================================================

(defparameter *op-update-hidden-state*
  '(op (:name update-hidden-state)
     (let ((ctx-emb (context-embedding ctx)))
       (when ctx-emb
         (unless (expert-hidden-state *current-expert*)
           (setf (expert-hidden-state *current-expert*)
                 (make-array *emb-dim* :element-type 'single-float :initial-element 0.0)))
         (let ((state (expert-hidden-state *current-expert*))
               (decay *hidden-state-decay*))
           (dotimes (i *emb-dim*)
             (setf (aref state i)
                   (+ (* decay (aref state i))
                      (* (- 1.0 decay) (aref ctx-emb i))))))
         (setf (expert-hidden-state-strength *current-expert*)
               (min 1.0 (+ (* *hidden-state-decay* 
                             (expert-hidden-state-strength *current-expert*))
                          (- 1.0 *hidden-state-decay*))))))))

(defparameter *op-check-hidden-state*
  '(op (:name check-hidden-state)
     (let ((state (expert-hidden-state *current-expert*))
           (strength (expert-hidden-state-strength *current-expert*))
           (ctx-emb (context-embedding ctx)))
       (when (and state ctx-emb (> strength *hidden-state-threshold*))
         (let ((sim (cosim state ctx-emb)))
           (when (> sim 0.5)
             (dolist (scale (reverse *context-scales*))
               (let* ((key (make-key ctx scale))
                      (result (when key (gethash key my-knowledge))))
                 (when result
                   (let* ((base-conf (scale-confidence scale))
                          (boost (* strength sim 0.2)))
                     (return-op result 
                               :confidence (min 1.0 (+ base-conf boost))
                               :scale scale 
                               :source :hidden-state)))))))))))

(defparameter *op-check-chunk-context*
  '(op (:name check-chunk-context)
     (when (and *chunk-embedding* my-centroid)
       (let ((sim (cosim my-centroid *chunk-embedding*)))
         (when (> sim *chunk-similarity-threshold*)
           (dolist (scale (reverse *context-scales*))
             (let* ((key (make-key ctx scale))
                    (result (when key (gethash key my-knowledge))))
               (when result
                 (let* ((base-conf (scale-confidence scale))
                        (boost (* (- sim *chunk-similarity-threshold*) 0.3)))
                   (return-op result
                             :confidence (min 1.0 (+ base-conf boost))
                             :scale scale
                             :source :chunk-context))))))))))

(defparameter *op-check-type-pattern*
  '(op (:name check-type-pattern)
     (let ((memory (expert-type-pattern-memory *current-expert*)))
       (when (and memory *type-buffer* (>= (length *type-buffer*) 3))
         (let* ((current-pattern (subseq *type-buffer* 0 
                                         (min *type-pattern-length* (length *type-buffer*))))
                (match-score (type-pattern-similarity current-pattern memory)))
           (when (> match-score 0.5)
             (dolist (scale (reverse *context-scales*))
               (let* ((key (make-key ctx scale))
                      (result (when key (gethash key my-knowledge))))
                 (when result
                   (let* ((base-conf (scale-confidence scale))
                          (boost (* match-score 0.15)))
                     (return-op result
                               :confidence (min 1.0 (+ base-conf boost))
                               :scale scale
                               :source :type-pattern)))))))))))

(defparameter *op-check-traveler-links*
  '(op (:name check-traveler-links)
     (let ((links (expert-traveler-links *current-expert*))
           (ctx-emb (context-embedding ctx)))
       (when (and links ctx-emb)
         (let ((best-match 0.0) (best-token nil))
           (dolist (link links)
             (let* ((stored-emb (car link))
                    (stored-token (cdr link))
                    (sim (cosim ctx-emb stored-emb)))
               (when (> sim best-match)
                 (setf best-match sim best-token stored-token))))
           (when (and (> best-match *traveler-link-threshold*) best-token)
             (return-op best-token 
                       :confidence (* 0.7 best-match) 
                       :source :traveler-link)))))))

(defparameter *extended-op-templates*
  (list *op-update-hidden-state*
        *op-check-hidden-state*
        *op-check-chunk-context*
        *op-check-type-pattern*
        *op-check-traveler-links*)
  "Extended operations that can be spliced into programs")

;;; Helper for type patterns
(defun type-pattern-similarity (p1 p2)
  (if (or (null p1) (null p2)) 0.0
      (let ((matches 0) (total (min (length p1) (length p2))))
        (dotimes (i total)
          (when (eql (nth i p1) (nth i p2)) (incf matches)))
        (if (> total 0) (/ (float matches) total) 0.0))))

;;; Helper for traveler links
(defun store-traveler-link! (expert ctx token confidence)
  (when (> confidence *traveler-link-confidence*)
    (let ((ctx-emb (context-embedding ctx)))
      (when ctx-emb
        (push (cons (vcopy ctx-emb) token) (expert-traveler-links expert))
        (when (> (length (expert-traveler-links expert)) *traveler-link-max*)
          (setf (expert-traveler-links expert)
                (subseq (expert-traveler-links expert) 0 *traveler-link-max*)))))))

;;; Global context state updates
(defun update-chunk-state! (token)
  (push token *chunk-buffer*)
  (when (> (length *chunk-buffer*) *chunk-window-size*)
    (setf *chunk-buffer* (subseq *chunk-buffer* 0 *chunk-window-size*)))
  (let ((tok-emb (get-emb token)))
    (when tok-emb
      (unless *chunk-embedding*
        (setf *chunk-embedding* (make-array *emb-dim* :element-type 'single-float 
                                            :initial-element 0.0)))
      (let ((alpha (/ 1.0 *chunk-window-size*)))
        (dotimes (i *emb-dim*)
          (setf (aref *chunk-embedding* i)
                (+ (* (- 1.0 alpha) (aref *chunk-embedding* i))
                   (* alpha (aref tok-emb i)))))))))

(defun update-type-state! (token)
  (let ((token-type (get-type token)))
    (push token-type *type-buffer*)
    (when (> (length *type-buffer*) *type-window-size*)
      (setf *type-buffer* (subseq *type-buffer* 0 *type-window-size*)))))

(defun update-global-context-state! (token)
  (update-chunk-state! token)
  (update-type-state! token))

;;; ============================================================================
;;; PROGRAM EXECUTION - S-EXPRESSION INTERPRETER (NO EVAL)
;;; ============================================================================
;;;
;;; Features:
;;;   - Proper lexical shadowing via alist environment
;;;   - Macroexpansion before interpretation  
;;;   - Condition/restart system for error recovery
;;; ============================================================================

(defvar *current-ctx* nil "Current context for program execution")
(defvar *sexp-env* nil "Local bindings alist for interpreter - proper shadowing")

;;; --- Condition system for expert programs ---
(define-condition expert-condition (condition)
  ((message :initarg :message :reader expert-condition-message)
   (context :initarg :context :initform nil :reader expert-condition-context))
  (:report (lambda (c s) (format s "Expert condition: ~A" (expert-condition-message c)))))

(define-condition expert-error (expert-condition error) ())
(define-condition expert-warning (expert-condition warning) ())

(defvar *expert-restarts* nil "Active restarts in expert program execution")

(defmacro with-expert-restart (name (&rest args) &body body)
  "Establish a restart point in expert code"
  `(let ((*expert-restarts* (cons (list ',name (lambda ,args ,@body)) *expert-restarts*)))
     ,@body))

(defun invoke-expert-restart (name &rest args)
  "Invoke a named restart"
  (let ((restart (assoc name *expert-restarts*)))
    (if restart
        (apply (cadr restart) args)
        (error "No restart named ~S" name))))

(defun expert-signal (message &key (context nil))
  "Signal a condition from expert code - can be handled or ignored"
  (signal 'expert-condition :message message :context context))

(defun expert-warn (message &key (context nil))
  "Warn from expert code"
  (warn 'expert-warning :message message :context context))

;;; --- Macroexpansion cache ---
(defvar *macroexpand-cache* (make-hash-table :test 'equal)
  "Cache for macroexpanded forms")

;; Forms we handle directly in the interpreter - don't macroexpand these
(defparameter *interpreter-handled-forms*
  '(quote declare progn let let* if when unless cond and or not null
    setf setq incf decf push pushnew pop
    dolist dotimes loop multiple-value-bind
    catch throw block return-from
    handler-case handler-bind restart-case signal warn
    op return-op delegate spawn-new continue-op function))

(defun expand-form (form)
  "Macroexpand form, but skip forms we handle directly in interpreter."
  (if (or (atom form) 
          (eq (car form) 'quote)
          (member (car form) *interpreter-handled-forms*))
      form  ; don't expand - we handle these
      (let ((cached (gethash form *macroexpand-cache*)))
        (if cached
            cached
            (let ((expanded (macroexpand-1 form)))
              (if (eq expanded form)
                  form  ; no expansion available
                  (progn
                    (setf (gethash form *macroexpand-cache*) expanded)
                    (expand-form expanded))))))))  ; expand again in case of nested macros

(defun clear-macroexpand-cache ()
  "Clear the macroexpansion cache"
  (clrhash *macroexpand-cache*))

(defun sexp-lookup (var)
  "Look up variable with proper lexical shadowing.
   Alist lookup finds FIRST match = innermost binding shadows outer ones.
   Falls back to special vars, then global symbol-value."
  ;; assoc finds first match - this gives us proper shadowing for free
  ;; inner let/let* prepend to *sexp-env*, so they shadow outer bindings
  (let ((binding (assoc var *sexp-env*)))
    (if binding
        (cdr binding)
        (case var
          (ctx *current-ctx*)
          (my-id (expert-id *current-expert*))
          (my-knowledge (expert-knowledge *current-expert*))
          (my-neighborhood (expert-neighborhood *current-expert*))
          (my-centroid (expert-centroid *current-expert*))
          (otherwise 
           (if (boundp var)
               (symbol-value var)
               (error "Unbound variable: ~S" var)))))))

(defun sexp-set! (var val)
  "Set variable: local env if bound there, else symbol-value"
  (let ((binding (assoc var *sexp-env*)))
    (if binding
        (setf (cdr binding) val)
        (case var
          (ctx (setf *current-ctx* val))
          (otherwise (setf (symbol-value var) val))))
    val))

(defun execute-sexp (form)
  "Execute S-expression without eval.
   Features: macroexpansion, proper shadowing, condition/restart support."
  (cond
    ;; Self-evaluating
    ((null form) nil)
    ((eq form t) t)
    ((numberp form) form)
    ((stringp form) form)
    ((keywordp form) form)
    ((arrayp form) form)
    ((hash-table-p form) form)
    
    ;; Variable lookup (with shadowing)
    ((symbolp form) (sexp-lookup form))
    
    ;; Special forms and calls
    ((consp form)
     (let* ((expanded (expand-form form))  ; macroexpand first
            (op (car expanded)))
       ;; If expansion changed the form, execute the expansion
       (unless (eq expanded form)
         (return-from execute-sexp (execute-sexp expanded)))
       (case op
         (quote (cadr form))
         (declare nil)  ; ignore declarations
         (progn (execute-progn (cdr form)))
         (let (execute-let (cadr form) (cddr form)))
         (let* (execute-let* (cadr form) (cddr form)))
         (if (execute-if (cadr form) (caddr form) (cadddr form)))
         (when (execute-when (cadr form) (cddr form)))
         (unless (execute-unless (cadr form) (cddr form)))
         (cond (execute-cond (cdr form)))
         (and (execute-and (cdr form)))
         (or (execute-or (cdr form)))
         (not (not (execute-sexp (cadr form))))
         (null (null (execute-sexp (cadr form))))
         (setf (execute-setf (cdr form)))
         (setq (execute-setf (cdr form)))
         (incf (execute-incf (cadr form) (caddr form)))
         (decf (execute-decf (cadr form) (caddr form)))
         (push (execute-push (cadr form) (caddr form)))
         (pushnew (execute-pushnew (cadr form) (caddr form)))
         (pop (execute-pop (cadr form)))
         (dolist (execute-dolist (cadr form) (cddr form)))
         (dotimes (execute-dotimes (cadr form) (cddr form)))
         (loop (execute-loop (cdr form)))
         (multiple-value-bind (execute-mvb (cadr form) (caddr form) (cdddr form)))
         (catch (execute-catch (cadr form) (cddr form)))
         (throw (execute-throw (cadr form) (caddr form)))
         (block (execute-block (cadr form) (cddr form)))
         (return-from (execute-return-from (cadr form) (caddr form)))
         ;; Condition system
         (handler-case (execute-handler-case (cadr form) (cddr form)))
         (handler-bind (execute-handler-bind (cadr form) (cddr form)))
         (restart-case (execute-restart-case (cadr form) (cddr form)))
         (signal (expert-signal (execute-sexp (cadr form))))
         (warn (expert-warn (execute-sexp (cadr form))))
         ;; UHMA-specific
         (op (execute-op (cadr form) (cddr form)))
         (return-op (execute-return-op (cdr form)))
         (delegate (execute-delegate (cdr form)))
         (spawn-new (spawn-new))
         (continue-op (continue-op))
         (function (execute-function (cadr form)))
         (otherwise (execute-funcall op (cdr form))))))
    
    (t (error "Cannot execute: ~S" form))))

(defun execute-progn (forms)
  (let ((result nil))
    (dolist (f forms result)
      (setf result (execute-sexp f)))))

(defun execute-let (bindings body)
  (let ((new-bindings nil))
    ;; Evaluate all binding values in current env
    (dolist (b bindings)
      (let ((var (if (consp b) (car b) b))
            (val (if (consp b) (execute-sexp (cadr b)) nil)))
        (push (cons var val) new-bindings)))
    ;; Execute body with extended env
    (let ((*sexp-env* (append new-bindings *sexp-env*)))
      (execute-progn body))))

(defun execute-let* (bindings body)
  (if (null bindings)
      (execute-progn body)
      (let* ((b (car bindings))
             (var (if (consp b) (car b) b))
             (val (if (consp b) (execute-sexp (cadr b)) nil)))
        (let ((*sexp-env* (cons (cons var val) *sexp-env*)))
          (execute-let* (cdr bindings) body)))))

(defun execute-if (test then else)
  (if (execute-sexp test)
      (execute-sexp then)
      (execute-sexp else)))

(defun execute-when (test body)
  (when (execute-sexp test)
    (execute-progn body)))

(defun execute-unless (test body)
  (unless (execute-sexp test)
    (execute-progn body)))

(defun execute-cond (clauses)
  (dolist (clause clauses)
    (let ((test (car clause)))
      (when (or (eq test t) (execute-sexp test))
        (return-from execute-cond
          (if (cdr clause)
              (execute-progn (cdr clause))
              (execute-sexp test)))))))

(defun execute-and (forms)
  (let ((result t))
    (dolist (f forms result)
      (setf result (execute-sexp f))
      (unless result (return-from execute-and nil)))))

(defun execute-or (forms)
  (dolist (f forms nil)
    (let ((result (execute-sexp f)))
      (when result (return-from execute-or result)))))

(defun execute-setf (pairs)
  (let ((result nil))
    (loop while pairs do
      (let ((place (pop pairs))
            (value-form (pop pairs)))
        (setf result (execute-sexp value-form))
        (cond
          ;; Simple variable
          ((symbolp place)
           (sexp-set! place result))
          ;; Place form like (aref x i) or (gethash k h)
          ((consp place)
           (let ((place-op (car place)))
             (case place-op
               (aref
                (let ((arr (execute-sexp (cadr place)))
                      (idx (execute-sexp (caddr place))))
                  (setf (aref arr idx) result)))
               (gethash
                (let ((key (execute-sexp (cadr place)))
                      (ht (execute-sexp (caddr place))))
                  (setf (gethash key ht) result)))
               (car
                (let ((lst (execute-sexp (cadr place))))
                  (setf (car lst) result)))
               (cdr
                (let ((lst (execute-sexp (cadr place))))
                  (setf (cdr lst) result)))
               (first (setf (first (execute-sexp (cadr place))) result))
               (second (setf (second (execute-sexp (cadr place))) result))
               (third (setf (third (execute-sexp (cadr place))) result))
               (nth
                (let ((n (execute-sexp (cadr place)))
                      (lst (execute-sexp (caddr place))))
                  (setf (nth n lst) result)))
               (slot-value
                (let ((obj (execute-sexp (cadr place)))
                      (slot (execute-sexp (caddr place))))
                  (setf (slot-value obj slot) result)))
               ;; Struct accessors - use funcall with (setf accessor)
               (otherwise
                (let ((setter (fdefinition (list 'setf place-op)))
                      (args (mapcar #'execute-sexp (cdr place))))
                  (apply setter result args))))))
          (t (error "Unknown setf place: ~S" place)))))
    result))

(defun execute-incf (place &optional delta-form)
  (let* ((delta (if delta-form (execute-sexp delta-form) 1))
         (old-val (execute-sexp place))
         (new-val (+ old-val delta)))
    (execute-setf (list place `(quote ,new-val)))
    new-val))

(defun execute-decf (place &optional delta-form)
  (let* ((delta (if delta-form (execute-sexp delta-form) 1))
         (old-val (execute-sexp place))
         (new-val (- old-val delta)))
    (execute-setf (list place `(quote ,new-val)))
    new-val))

(defun execute-push (item-form place)
  (let ((item (execute-sexp item-form))
        (lst (execute-sexp place)))
    (let ((new-list (cons item lst)))
      (execute-setf (list place `(quote ,new-list)))
      new-list)))

(defun execute-pushnew (item-form place)
  (let ((item (execute-sexp item-form))
        (lst (execute-sexp place)))
    (let ((new-list (adjoin item lst)))
      (execute-setf (list place `(quote ,new-list)))
      new-list)))

(defun execute-pop (place)
  (let ((lst (execute-sexp place)))
    (prog1 (car lst)
      (execute-setf (list place `(quote ,(cdr lst)))))))

(defun execute-dolist (var-list body)
  (let ((var (first var-list))
        (list-form (second var-list))
        (result-form (third var-list)))
    (let ((lst (execute-sexp list-form))
          (result nil))
      (dolist (item lst)
        (let ((*sexp-env* (cons (cons var item) *sexp-env*)))
          (setf result (execute-progn body))))
      (if result-form
          (let ((*sexp-env* (cons (cons var nil) *sexp-env*)))
            (execute-sexp result-form))
          result))))

(defun execute-dotimes (var-count body)
  (let ((var (first var-count))
        (count-form (second var-count))
        (result-form (third var-count)))
    (let ((count (execute-sexp count-form))
          (result nil))
      (dotimes (i count)
        (let ((*sexp-env* (cons (cons var i) *sexp-env*)))
          (setf result (execute-progn body))))
      (if result-form
          (let ((*sexp-env* (cons (cons var count) *sexp-env*)))
            (execute-sexp result-form))
          result))))

(defun execute-loop (clauses)
  "Execute LOOP - delegate to CL's loop for complex forms"
  ;; Check if it's a simple infinite loop or complex loop
  ;; Complex loops start with loop keywords like FOR, WITH, WHILE, etc.
  (let ((loop-keywords '(for as with while until repeat 
                         do doing collect collecting append appending
                         nconc nconcing count counting sum summing
                         maximize maximizing minimize minimizing
                         initially finally return named)))
    (if (and clauses (not (member (car clauses) loop-keywords)))
        ;; Simple loop - just body forms, repeat forever (need explicit return)
        (loop (execute-progn clauses))
        ;; Complex loop with keywords
        (execute-complex-loop clauses))))

(defun execute-complex-loop (clauses)
  "Handle complex LOOP with for/collect/etc"
  (let ((loop-bindings nil)
        (result nil)
        (clauses-copy clauses))
    ;; Parse loop to find iteration variables
    (loop while clauses-copy do
      (let ((kw (pop clauses-copy)))
        (case kw
          ((for as)
           (let ((var (pop clauses-copy)))
             (push (cons var nil) loop-bindings)
             ;; Skip rest of for clause
             (loop while (and clauses-copy 
                             (member (car clauses-copy) 
                                    '(from to below above by in on across being = then)))
                   do (let ((sub-kw (pop clauses-copy)))
                        (case sub-kw
                          ((from to below above by in on across =)
                           (pop clauses-copy))  ; skip value
                          (being
                           ;; Skip: the hash-keys/hash-values of X using (hash-value V)
                           (pop clauses-copy)  ; the
                           (pop clauses-copy)  ; hash-keys or hash-values  
                           (pop clauses-copy)  ; of
                           (pop clauses-copy)  ; hash-table form
                           (when (eq (car clauses-copy) 'using)
                             (pop clauses-copy)  ; using
                             (let ((using-spec (pop clauses-copy)))
                               (when (consp using-spec)
                                 (push (cons (cadr using-spec) nil) loop-bindings)))))
                          (then (pop clauses-copy)))))))
          ((with)
           (let ((var (pop clauses-copy)))
             (push (cons var nil) loop-bindings)
             (when (eq (car clauses-copy) '=)
               (pop clauses-copy)
               (pop clauses-copy))))
          ((collect append nconc sum count maximize minimize)
           (pop clauses-copy)
           (when (eq (car clauses-copy) 'into)
             (pop clauses-copy)
             (let ((var (pop clauses-copy)))
               (push (cons var nil) loop-bindings))))
          (otherwise nil))))
    
    ;; Now actually execute the loop
    ;; We'll parse and execute step by step
    (let ((*sexp-env* (append loop-bindings *sexp-env*)))
      (execute-loop-body clauses))))

(defun execute-loop-body (clauses)
  "Actually execute a complex loop"
  (let ((for-clauses nil)
        (body-clauses nil)
        (collect-var nil)
        (collect-fn nil)
        (initially nil)
        (finally nil)
        (result-form nil))
    ;; Parse into sections
    (let ((c clauses))
      (loop while c do
        (let ((kw (pop c)))
          (case kw
            ((for as)
             (let ((var (pop c))
                   (clause-parts (list kw)))
               (push var clause-parts)
               (loop while (and c (member (car c) 
                                         '(from to below above by in on across 
                                           being = then using)))
                     do (push (pop c) clause-parts)
                        (when (and c (not (keywordp (car c))))
                          (push (pop c) clause-parts)))
               (push (nreverse clause-parts) for-clauses)))
            ((while until when unless)
             (push (list kw (pop c)) body-clauses))
            ((do doing)
             (push (list 'do (pop c)) body-clauses))
            ((collect collecting append appending nconc nconcing
              sum summing count counting maximize maximizing minimize minimizing)
             (let ((form (pop c)))
               (setf collect-fn kw)
               (if (eq (car c) 'into)
                   (progn (pop c) (setf collect-var (pop c)))
                   (setf collect-var (gensym)))
               (push (list 'collect collect-fn form collect-var) body-clauses)))
            (initially (setf initially (pop c)))
            (finally (setf finally (pop c)))
            (return (setf result-form (pop c)))
            (otherwise 
             (push (list 'do kw) body-clauses))))))
    
    ;; Setup for-clause iterators
    (let ((iterators (setup-loop-iterators (nreverse for-clauses)))
          (collected nil)
          (sum-val 0)
          (count-val 0)
          (max-val nil)
          (min-val nil)
          (body-clauses-rev (nreverse body-clauses)))
      ;; Initially
      (when initially (execute-sexp initially))
      
      ;; Main loop - if no iterators, don't loop at all
      (block loop-block
        (when iterators
          (loop
            ;; Advance iterators
            (dolist (iter iterators)
              (unless (funcall (iterator-step iter))
                (return-from loop-block nil)))
            
            ;; Execute body clauses
            (dolist (bc body-clauses-rev)
              (case (car bc)
                (while (unless (execute-sexp (cadr bc))
                         (return-from loop-block nil)))
                (until (when (execute-sexp (cadr bc))
                         (return-from loop-block nil)))
                (when (when (execute-sexp (cadr bc))
                        nil))  ; just condition check
                (unless (unless (execute-sexp (cadr bc))
                          nil))
                (do (execute-sexp (cadr bc)))
                (collect
                 (let ((val (execute-sexp (caddr bc))))
                   (case (cadr bc)
                     ((collect collecting) (push val collected))
                     ((append appending) (setf collected (append collected val)))
                     ((nconc nconcing) (setf collected (nconc collected val)))
                     ((sum summing) (incf sum-val val))
                     ((count counting) (when val (incf count-val)))
                     ((maximize maximizing) 
                      (when (or (null max-val) (> val max-val))
                        (setf max-val val)))
                     ((minimize minimizing)
                      (when (or (null min-val) (< val min-val))
                        (setf min-val val)))))))))))
      
      ;; Finally
      (when finally (execute-sexp finally))
      
      ;; Return appropriate result
      (cond
        (result-form (execute-sexp result-form))
        (collect-fn
         (case collect-fn
           ((collect collecting) (nreverse collected))
           ((append appending nconc nconcing) collected)
           ((sum summing) sum-val)
           ((count counting) count-val)
           ((maximize maximizing) max-val)
           ((minimize minimizing) min-val)
           (t collected)))
        (t nil)))))


(defun setup-loop-iterators (for-clauses)
  "Create iterator objects for each FOR clause"
  (let ((iterators nil))
    (dolist (fc for-clauses)
      (let* ((var (cadr fc))
             (rest (cddr fc)))
        (cond
          ;; FOR var FROM x TO y [BY z]
          ((member 'from rest)
           (let* ((from-val (execute-sexp (cadr (member 'from rest))))
                  (to-val (let ((to-pos (member 'to rest)))
                            (if to-pos (execute-sexp (cadr to-pos)) nil)))
                  (below-val (let ((below-pos (member 'below rest)))
                               (if below-pos (execute-sexp (cadr below-pos)) nil)))
                  (above-val (let ((above-pos (member 'above rest)))
                               (if above-pos (execute-sexp (cadr above-pos)) nil)))
                  (by-val (let ((by-pos (member 'by rest)))
                            (if by-pos (execute-sexp (cadr by-pos)) 1)))
                  (current from-val)
                  (first t))
             (push (make-iterator
                    :step (let ((v var) (c current) (tv to-val) (blv below-val) 
                               (abv above-val) (bv by-val) (fst first))
                            (declare (ignore c fst))
                            #'(lambda ()
                                (if first
                                    (progn 
                                      (setf first nil)
                                      (let ((binding (assoc v *sexp-env*)))
                                        (when binding (setf (cdr binding) current)))
                                      t)
                                    (progn
                                      (incf current bv)
                                      (let ((done (or (and tv (> current tv))
                                                     (and blv (>= current blv))
                                                     (and abv (<= current abv)))))
                                        (unless done
                                          (let ((binding (assoc v *sexp-env*)))
                                            (when binding (setf (cdr binding) current))))
                                        (not done)))))))
                   iterators)))
          
          ;; FOR var IN list
          ((member 'in rest)
           (let* ((list-val (execute-sexp (cadr (member 'in rest))))
                  (remaining list-val))
             (push (make-iterator
                    :step (let ((v var))
                            #'(lambda ()
                                (if remaining
                                    (progn
                                      (let ((binding (assoc v *sexp-env*)))
                                        (when binding (setf (cdr binding) (car remaining))))
                                      (setf remaining (cdr remaining))
                                      t)
                                    nil))))
                   iterators)))
          
          ;; FOR var ON list  
          ((member 'on rest)
           (let* ((list-val (execute-sexp (cadr (member 'on rest))))
                  (remaining list-val)
                  (first t))
             (push (make-iterator
                    :step (let ((v var))
                            #'(lambda ()
                                (if first
                                    (progn
                                      (setf first nil)
                                      (let ((binding (assoc v *sexp-env*)))
                                        (when binding (setf (cdr binding) remaining)))
                                      (not (null remaining)))
                                    (progn
                                      (setf remaining (cdr remaining))
                                      (if remaining
                                          (progn
                                            (let ((binding (assoc v *sexp-env*)))
                                              (when binding (setf (cdr binding) remaining)))
                                            t)
                                          nil))))))
                   iterators)))
          
          ;; FOR var ACROSS vector
          ((member 'across rest)
           (let* ((vec (execute-sexp (cadr (member 'across rest))))
                  (len (length vec))
                  (idx 0))
             (push (make-iterator
                    :step (let ((v var))
                            #'(lambda ()
                                (if (< idx len)
                                    (progn
                                      (let ((binding (assoc v *sexp-env*)))
                                        (when binding (setf (cdr binding) (aref vec idx))))
                                      (incf idx)
                                      t)
                                    nil))))
                   iterators)))
          
          ;; FOR var BEING THE HASH-KEYS OF ht [USING (HASH-VALUE vvar)]
          ((member 'being rest)
           (let* ((being-pos (member 'being rest))
                  (hash-type (caddr being-pos))  ; hash-keys or hash-values
                  (of-pos (member 'of being-pos))
                  (ht (execute-sexp (cadr of-pos)))
                  (using-pos (member 'using being-pos))
                  (value-var (when using-pos 
                               (cadr (cadr using-pos))))  ; extract var from (hash-value var)
                  (keys (when ht (loop for k being the hash-keys of ht collect k)))
                  (remaining keys))
             (push (make-iterator
                    :step (let ((v var) (vv value-var) (tbl ht) (kt hash-type))
                            #'(lambda ()
                                (if remaining
                                    (progn
                                      (let* ((k (car remaining))
                                             (val (gethash k tbl)))
                                        (if (eq kt 'hash-keys)
                                            (progn
                                              (let ((binding (assoc v *sexp-env*)))
                                                (when binding (setf (cdr binding) k)))
                                              (when vv
                                                (let ((vbinding (assoc vv *sexp-env*)))
                                                  (when vbinding (setf (cdr vbinding) val)))))
                                            (progn
                                              (let ((binding (assoc v *sexp-env*)))
                                                (when binding (setf (cdr binding) val)))
                                              (when vv
                                                (let ((vbinding (assoc vv *sexp-env*)))
                                                  (when vbinding (setf (cdr vbinding) k)))))))
                                      (setf remaining (cdr remaining))
                                      t)
                                    nil))))
                   iterators)))
          
          ;; FOR var = init [THEN step]
          ((member '= rest)
           (let* ((init-form (cadr (member '= rest)))
                  (then-pos (member 'then rest))
                  (then-form (when then-pos (cadr then-pos)))
                  (first t))
             (push (make-iterator
                    :step (let ((v var) (init init-form) (then then-form))
                            #'(lambda ()
                                (let ((val (if first
                                              (execute-sexp init)
                                              (if then
                                                  (execute-sexp then)
                                                  (execute-sexp init)))))
                                  (setf first nil)
                                  (let ((binding (assoc v *sexp-env*)))
                                    (when binding (setf (cdr binding) val)))
                                  t))))  ; = loops forever unless terminated by while/until
                   iterators))))))
    (nreverse iterators)))

(defun execute-mvb (vars value-form body)
  "Execute multiple-value-bind"
  (let ((values (multiple-value-list (execute-sexp value-form)))
        (new-bindings nil))
    (loop for var in vars
          for val in values
          do (push (cons var val) new-bindings))
    ;; Fill in remaining vars with nil
    (loop for var in (nthcdr (length values) vars)
          do (push (cons var nil) new-bindings))
    (let ((*sexp-env* (append new-bindings *sexp-env*)))
      (execute-progn body))))

(defun execute-catch (tag-form body)
  (let ((tag (execute-sexp tag-form)))
    (catch tag
      (execute-progn body))))

(defun execute-throw (tag-form value-form)
  (let ((tag (execute-sexp tag-form))
        (value (execute-sexp value-form)))
    (throw tag value)))

;;; --- Block/Return-from for non-local exits ---
(defvar *block-tags* nil "Active block tags for return-from: alist of (name . catch-tag)")

(defun execute-block (name body)
  "Execute (block name body...) - establishes named exit point"
  (let ((tag (gensym (format nil "BLOCK-~A-" name))))
    (catch tag
      (let ((*block-tags* (cons (cons name tag) *block-tags*)))
        (execute-progn body)))))

(defun execute-return-from (name value-form)
  "Execute (return-from name value) - return to enclosing block"
  (let ((entry (assoc name *block-tags*)))
    (unless entry
      (error "No block named ~S is currently visible" name))
    (throw (cdr entry) (execute-sexp value-form))))

;;; --- Condition System ---

(defun execute-handler-case (protected-form handlers)
  "Execute (handler-case form (condition-type (var) body...)*)"
  (handler-case
      (execute-sexp protected-form)
    (expert-condition (c)
      ;; Find matching handler
      (dolist (handler handlers)
        (let ((type (car handler))
              (var-list (cadr handler))
              (body (cddr handler)))
          (when (or (eq type 'expert-condition)
                    (eq type 'expert-error)
                    (eq type 'expert-warning)
                    (eq type t))
            (let ((*sexp-env* (if var-list
                                  (cons (cons (car var-list) c) *sexp-env*)
                                  *sexp-env*)))
              (return-from execute-handler-case (execute-progn body))))))
      ;; No handler matched, re-signal
      (error c))
    (error (c)
      ;; Handle CL errors too if requested
      (dolist (handler handlers)
        (let ((type (car handler))
              (var-list (cadr handler))
              (body (cddr handler)))
          (when (or (eq type 'error) (eq type t))
            (let ((*sexp-env* (if var-list
                                  (cons (cons (car var-list) c) *sexp-env*)
                                  *sexp-env*)))
              (return-from execute-handler-case (execute-progn body))))))
      (error c))))

(defun execute-handler-bind (bindings body)
  "Execute (handler-bind ((type handler-fn)...) body...)
   Bindings establish handlers without unwinding."
  ;; For now, simplified version - just execute body
  ;; Full implementation would establish dynamic handlers
  (execute-progn body))

(defun execute-restart-case (protected-form restarts)
  "Execute (restart-case form (restart-name (args...) body...)*)"
  (let ((restart-alist nil))
    ;; Build restart functions
    (dolist (r restarts)
      (let ((name (car r))
            (args (cadr r))
            (body (cddr r)))
        (push (list name 
                    (lambda (&rest vals)
                      (let ((new-bindings (mapcar #'cons args vals)))
                        (let ((*sexp-env* (append new-bindings *sexp-env*)))
                          (execute-progn body)))))
              restart-alist)))
    ;; Execute with restarts available
    (let ((*expert-restarts* (append restart-alist *expert-restarts*)))
      (execute-sexp protected-form))))

(defun execute-op (plist body)
  "Execute (op (:name X :modifiable ((p v)...)) body...)"
  (let* ((name (getf plist :name))
         (modifiable (getf plist :modifiable))
         (mod-bindings nil))
    ;; Create bindings for modifiable params
    (dolist (m modifiable)
      (push (cons (first m) (execute-sexp (second m))) mod-bindings))
    ;; Execute with bindings
    (let ((*sexp-env* (append mod-bindings *sexp-env*))
          (*current-op-name* name))
      (let ((result (catch 'op-result 
                      (execute-progn body)
                      nil)))
        (when result
          (record-op-metric! *current-expert* name 
                            (op-result-type result)
                            (op-result-confidence result))
          (unless (eq (op-result-type result) :continue)
            (throw 'program-result result)))
        ;; If we get here, operation continued
        (record-op-metric! *current-expert* name :continue)))))

(defun execute-return-op (args)
  "Execute (return-op value :confidence C :scale S :source SRC)"
  (let ((value (execute-sexp (car args)))
        (plist (cdr args)))
    ;; Evaluate keyword args
    (let ((confidence (let ((c (getf plist :confidence)))
                        (if c (float (execute-sexp c) 0.0) 0.5)))
          (scale (let ((s (getf plist :scale)))
                   (if s (execute-sexp s) nil)))
          (source (let ((src (getf plist :source)))
                    (if src (execute-sexp src) nil))))
      (return-op value :confidence confidence :scale scale :source source))))

(defun execute-delegate (args)
  "Execute (delegate target-id)"
  (let ((target-id (execute-sexp (car args))))
    (delegate target-id)))

(defun execute-function (fname)
  "Handle #'fname"
  (if (symbolp fname)
      (fdefinition fname)
      (error "execute-function: expected symbol, got ~S" fname)))

(defun execute-funcall (fn-name args)
  "Execute a function call"
  (let ((fn (cond
              ((symbolp fn-name) 
               (if (fboundp fn-name)
                   (fdefinition fn-name)
                   (error "Unknown function: ~S" fn-name)))
              ((functionp fn-name) fn-name)
              (t (error "Cannot call: ~S" fn-name))))
        (evaled-args (mapcar #'execute-sexp args)))
    (apply fn evaled-args)))

(defun run-program (expert ctx)
  "Execute expert's S-expression program via interpreter (no eval)."
  (let* ((*current-expert* expert)
         (*current-ctx* ctx)
         (*sexp-env* nil))
    (let ((result (catch 'program-result
                    (execute-sexp (expert-program expert))
                    nil)))
      (or result (make-op-result :type :return :value nil :confidence 0.0)))))

;;; ============================================================================
;;; PROGRAM CONSTRUCTION
;;; ============================================================================

(defun make-default-program ()
  "Return a fresh copy of the default program S-expression"
  (copy-tree *default-program*))

(defun copy-program (program)
  "Deep copy a program"
  (copy-tree program))

;;; ============================================================================
;;; PROGRAM INTROSPECTION - READ THE CODE
;;; ============================================================================

(defun find-ops-in-program (program)
  "Walk program tree and find all (op ...) forms, return list of op plists"
  (let ((ops nil))
    (labels ((walk (form)
               (when (consp form)
                 (if (and (eq (car form) 'op)
                          (consp (cadr form))
                          (eq (car (cadr form)) :name))
                     ;; Found an op form
                     (push (cadr form) ops)
                     ;; Recurse
                     (dolist (sub form)
                       (walk sub))))))
      (walk program))
    (nreverse ops)))

(defun get-op-names (program)
  "Get list of operation names in a program"
  (mapcar (lambda (op-plist) (getf op-plist :name))
          (find-ops-in-program program)))

(defun get-modifiable-params (program)
  "Get all modifiable parameters from program: ((op-name param-name value) ...)"
  (let ((result nil))
    (dolist (op-plist (find-ops-in-program program))
      (let ((name (getf op-plist :name))
            (modifiable (getf op-plist :modifiable)))
        (dolist (param modifiable)
          (push (list name (first param) (second param)) result))))
    (nreverse result)))

(defun get-modifiable-param (program param-name)
  "Get a single modifiable parameter value by name (e.g. 'threshold or :threshold).
   Searches all ops for the first match. Handles both symbol and keyword names."
  (let ((target-name (if (keywordp param-name)
                         (intern (symbol-name param-name))
                         param-name)))
    (dolist (op-plist (find-ops-in-program program))
      (let ((modifiable (getf op-plist :modifiable)))
        (dolist (param modifiable)
          (when (or (eq (first param) param-name)
                    (eq (first param) target-name)
                    (string= (symbol-name (first param)) (symbol-name param-name)))
            (return-from get-modifiable-param (second param)))))))
  nil)

(defun find-op-in-program (program op-name)
  "Find the (op ...) form with given name, return the whole form"
  (labels ((walk (form)
             (when (consp form)
               (if (and (eq (car form) 'op)
                        (consp (cadr form))
                        (eq (getf (cadr form) :name) op-name))
                   form
                   (dolist (sub form)
                     (let ((found (walk sub)))
                       (when found (return-from find-op-in-program found))))))))
    (walk program)
    nil))

(defun program-has-op-p (program op-name)
  "Check if program contains an operation with given name"
  (member op-name (get-op-names program)))

;;; ============================================================================
;;; PROGRAM MODIFICATION - CHANGE THE CODE
;;; ============================================================================

(defun set-modifiable-param! (program op-name param-name new-value)
  "Destructively modify a parameter value in the program.
   Returns T if modified, NIL if not found."
  (let ((op-form (find-op-in-program program op-name)))
    (when op-form
      (let* ((plist (cadr op-form))
             (modifiable (getf plist :modifiable)))
        (dolist (param modifiable)
          (when (eq (first param) param-name)
            (setf (second param) new-value)
            (return-from set-modifiable-param! t))))))
  nil)

(defun insert-op-before! (program before-op-name new-op-sexp)
  "Insert new operation before the named operation.
   Modifies program in place. Returns program."
  (labels ((walk (form)
             (when (and (consp form) (eq (car form) 'progn))
               (let ((new-body nil)
                     (body (cdr form)))
                 (dolist (sub body)
                   (when (and (consp sub)
                              (eq (car sub) 'op)
                              (consp (cadr sub))
                              (eq (getf (cadr sub) :name) before-op-name))
                     (push new-op-sexp new-body))
                   (push sub new-body))
                 (setf (cdr form) (nreverse new-body))
                 (return-from insert-op-before! program)))
             (when (consp form)
               (dolist (sub form)
                 (walk sub)))))
    (walk program))
  program)

(defun remove-op! (program op-name)
  "Remove operation with given name from program.
   Modifies program in place. Returns program."
  (labels ((walk (form)
             (when (and (consp form) (eq (car form) 'progn))
               (setf (cdr form)
                     (remove-if (lambda (sub)
                                  (and (consp sub)
                                       (eq (car sub) 'op)
                                       (consp (cadr sub))
                                       (eq (getf (cadr sub) :name) op-name)))
                                (cdr form)))
               (return-from remove-op! program))
             (when (consp form)
               (dolist (sub form)
                 (walk sub)))))
    (walk program))
  program)

;;; ============================================================================
;;; PROGRAM INHERITANCE AND MUTATION
;;; ============================================================================

(defparameter *operation-costs*
  '((check-ownership . 0.01)      ; Was 0.0 - basic ops need cost for EFFICIENCY drive
    (check-consolidated . 0.02)   ; LTM lookup has some cost
    (check-travelers . 0.02)      ; Cross-neighborhood check
    (assess-confidence . 0.01)    ; Multi-scale confidence
    (try-similar . 0.03)          ; Similarity search is expensive
    (check-type . 0.01)           ; Type routing
    (request-help . 0.02)         ; Delegation overhead
    (update-hidden-state . 0.01)
    (check-hidden-state . 0.01)
    (check-chunk-context . 0.02)
    (check-type-pattern . 0.02)
    (check-traveler-links . 0.03)
    (lookup . 0.01)               ; Hash lookup
    (vote-neighbors . 0.03))      ; Voting is expensive
  "Metabolic cost per operation - all ops have cost for EFFICIENCY drive")

(defun compute-program-cost (program)
  "Total metabolic cost of operations in program"
  (let ((total 0.0))
    (dolist (name (get-op-names program))
      (let ((cost (cdr (assoc name *operation-costs*))))
        (when cost (incf total cost))))
    total))

(defun update-expert-program-cost! (expert)
  (setf (expert-program-cost expert)
        (compute-program-cost (expert-program expert))))

(defparameter *extended-op-names*
  '(update-hidden-state check-hidden-state check-chunk-context 
    check-type-pattern check-traveler-links)
  "Names of extended operations")

(defun get-extended-op-template (name)
  "Get the S-expression template for an extended operation"
  (case name
    (update-hidden-state *op-update-hidden-state*)
    (check-hidden-state *op-check-hidden-state*)
    (check-chunk-context *op-check-chunk-context*)
    (check-type-pattern *op-check-type-pattern*)
    (check-traveler-links *op-check-traveler-links*)))

(defun inherit-program (parent-program)
  "Create a new program inheriting from parent with possible mutations.
   Returns the new program S-expression."
  (let ((program (copy-program parent-program)))
    
    ;; Possibly add an extended operation
    (when (< (random 1.0) *program-mutation-rate*)
      (let ((available (remove-if (lambda (name) (program-has-op-p program name))
                                  *extended-op-names*)))
        (when available
          (let* ((new-op-name (nth (random (length available)) available))
                 (new-op (copy-tree (get-extended-op-template new-op-name))))
            (when new-op
              ;; Insert before assess-confidence (or at start of progn)
              (if (eq new-op-name 'update-hidden-state)
                  ;; update-hidden-state goes early
                  (insert-op-before! program 'check-consolidated new-op)
                  ;; Others go before assess-confidence  
                  (insert-op-before! program 'assess-confidence new-op)))))))
    
    ;; Possibly remove an extended operation
    (when (< (random 1.0) *program-mutation-rate*)
      (let ((removable (intersection (get-op-names program) *extended-op-names*)))
        (when removable
          (remove-op! program (nth (random (length removable)) removable)))))
    
    ;; Possibly mutate a modifiable parameter (only numeric ones)
    (when (< (random 1.0) *program-mutation-rate*)
      (let ((params (get-modifiable-params program)))
        (when params
          (let* ((param (nth (random (length params)) params))
                 (op-name (first param))
                 (param-name (second param))
                 (current-value (third param)))
            ;; Only mutate numeric parameters (thresholds etc)
            (when (numberp current-value)
              (let* ((delta (* current-value 0.2 (- (random 2.0) 1.0)))
                     (new-val (max 0.1 (min 0.95 (+ current-value delta)))))
                (set-modifiable-param! program op-name param-name new-val)))))))
    
    program))

;;; ============================================================================
;;; EXPERT CREATION AND ROUTING
;;; ============================================================================

(defun spawn-expert (&key parent)
  "Create a new expert with S-expression program.
   HOOK: +hook-expert-spawned+ fired after creation."
  (let ((program (if parent
                     (inherit-program (expert-program parent))
                     (make-default-program))))
    (let ((e (make-expert
              :program program
              :code-metrics (make-hash-table :test 'eq)
              :parent-id (when parent (expert-id parent))
              :birth-step *step*
              :centroid (if parent (vcopy (expert-centroid parent)) (random-vec))
              :life 1.0)))
      (when parent
        (setf (expert-owned-type e) (expert-owned-type parent))
        (let ((count 0))
          (maphash (lambda (k v)
                     (when (< count 5)
                       (setf (gethash k (expert-knowledge e)) v)
                       (incf count)))
                   (expert-knowledge parent)))
        ;; P19: Inherit type pattern memory if parent has it
        (when (expert-type-pattern-memory parent)
          (setf (expert-type-pattern-memory e)
                (copy-list (expert-type-pattern-memory parent))))
        ;; Fire hook for program inheritance tracking
        (run-hook +hook-program-inherited+ e parent))
      ;; Compute initial program cost
      (update-expert-program-cost! e)
      (push e *experts*)
      (incf *total-births*)
      (let ((nbhd (find-best-neighborhood e)))
        (add-expert-to-neighborhood! e nbhd))
      ;; Fire hook
      (run-hook +hook-expert-spawned+ e parent)
      e)))

(defun find-expert-by-id (id)
  (find id *experts* :key #'expert-id))

(defun find-neighborhood-by-id (id)
  (find id *neighborhoods* :key #'neighborhood-id))

(defun find-expert-for-type (type)
  (find-if (lambda (e)
             (and (> (expert-life e) 0.1)
                  (eql (expert-owned-type e) type)))
           *experts*))

(defun find-expert-by-similarity (ctx)
  (when ctx
    (let* ((target-nbhd (route-to-neighborhood ctx))
           (candidates (find-experts-in-neighborhood target-nbhd))
           (ctx-emb (context-embedding ctx))
           (best nil) (best-sim -999.0))
      (dolist (e candidates)
        (when (and (> (expert-life e) 0.1) (expert-centroid e))
          (let ((sim (cosim ctx-emb (expert-centroid e))))
            (when (> sim best-sim)
              (setf best e best-sim sim)))))
      (when (> best-sim 0.3) best))))

(defun find-context-owner (ctx)
  (let ((owner-id (context-owner ctx)))
    (when owner-id (find-expert-by-id owner-id))))

(defun find-best-parent ()
  (let ((best nil) (best-score 0.0))
    (dolist (e *experts*)
      (let ((score (* (expert-life e) (1+ (hash-table-count (expert-knowledge e))))))
        (when (and (>= (expert-life e) 1.0) (> score best-score))
          (setf best e best-score score))))
    best))

(defun find-expert-by-similarity-excluding (ctx exclude-id)
  (when ctx
    (let ((ctx-emb (context-embedding (list ctx)))
          (best nil) (best-sim -999.0))
      (dolist (e *experts*)
        (when (and (> (expert-life e) 0.1) 
                   (expert-centroid e)
                   (not (eql (expert-id e) exclude-id)))
          (let ((sim (cosim ctx-emb (expert-centroid e))))
            (when (> sim best-sim)
              (setf best e best-sim sim)))))
      (when (> best-sim 0.3) best))))

;;; ============================================================================
;;; MAIN EXECUTION
;;; ============================================================================

(defun execute-with-routing (ctx &optional (depth 0))
  (when (> depth 10)
    (setf *last-answering-expert* nil)
    (return-from execute-with-routing 
      (make-op-result :type :return :value nil :confidence 0.0)))
  
  (multiple-value-bind (pred conf source) (lookup-consolidated ctx)
    (when pred
      (setf *last-answering-expert* nil)  ; Consolidated memory answered, not an expert
      (return-from execute-with-routing
        (make-op-result :type :return :value pred :confidence conf :source source))))
  
  (let ((owner (find-context-owner ctx)))
    (when (and owner (> (expert-life owner) 0.1))
      (push (expert-id owner) *call-stack*)
      (unwind-protect
          (let ((result (run-program owner ctx)))
            (case (op-result-type result)
              (:return (return-from execute-with-routing result))
              (:delegate 
               (let ((target (find-expert-by-id (op-result-target result))))
                 (when (and target (not (member (expert-id target) *call-stack*)))
                   (return-from execute-with-routing 
                     (execute-with-routing ctx (1+ depth))))))
              (:spawn nil)))
        (pop *call-stack*))))
  
  (let* ((target-nbhd (route-to-neighborhood ctx))
         (candidates (find-experts-in-neighborhood target-nbhd)))
    (dolist (e candidates)
      (when (and (not (member (expert-id e) *call-stack*))
                 (> (expert-life e) 0.1))
        (push (expert-id e) *call-stack*)
        (unwind-protect
            (let ((result (run-program e ctx)))
              (case (op-result-type result)
                (:return (return-from execute-with-routing result))
                (:delegate 
                 (let ((target (find-expert-by-id (op-result-target result))))
                   (when (and target (not (member (expert-id target) *call-stack*)))
                     (return-from execute-with-routing 
                       (execute-with-routing ctx (1+ depth))))))
                (:spawn nil)))
          (pop *call-stack*)))))
  
  (spawn-expert :parent (find-best-parent))
  (setf *last-answering-expert* nil)  ; No expert answered
  (make-op-result :type :return :value nil :confidence 0.0))

(defun vote-across-scales (ctx)
  (let ((votes (make-hash-table :test 'eq)) (total-weight 0.0))
    (loop for scale in *context-scales*
          for weight in *scale-weights*
          do (let ((key (make-context-key ctx scale)))
               (when key
                 (dolist (e *experts*)
                   (when (> (expert-life e) 0.1)
                     (let ((pred (gethash key (expert-knowledge e))))
                       (when pred
                         (incf (gethash pred votes 0.0) weight)
                         (incf total-weight weight))))))))
    (dolist (scale *context-scales*)
      (let* ((key (make-context-key ctx scale))
             (pattern (when key (gethash key *long-term-memory*))))
        (when pattern
          (let ((pred (consolidated-pattern-prediction pattern)))
            (incf (gethash pred votes 0.0) 1.0)
            (incf total-weight 1.0)))))
    (when (> total-weight 0)
      (let ((best-pred nil) (best-weight 0.0))
        (maphash (lambda (pred weight)
                   (when (> weight best-weight)
                     (setf best-pred pred best-weight weight)))
                 votes)
        (values best-pred (/ best-weight total-weight))))))

;;; ============================================================================
;;; LEARNING
;;; ============================================================================

(defun learn! (ctx actual predicted verified-p)
  "Core learning function with hook integration.
   HOOKS:
   - +hook-pre-learn+ : before learning, can return :skip
   - +hook-should-learn+ : gating hook, can return :learn or :skip
   - +hook-post-learn+ : after learning with results"
  ;; Pre-learn hook can skip standard learning
  (let ((pre-result (run-hook +hook-pre-learn+ ctx actual predicted verified-p)))
    (when (eq pre-result :skip)
      (return-from learn! nil)))
  ;; Should-learn gating hook (similar to should-verify-p)
  (let ((should-result (run-hook-until +hook-should-learn+ #'identity ctx actual predicted)))
    (when (eq should-result :skip)
      (return-from learn! nil)))

  (let* ((owner (find-context-owner ctx))
         (learner (or owner (find-expert-by-similarity ctx) (first *experts*)))
         (correct (equal predicted actual)))  ; equal not eq - tokens are strings
    (when learner
      ;; Mark expert as used (for organic lifecycle)
      (setf (expert-last-used learner) *step*)
      ;; Presence affects learning strength - vivid experiences learned more strongly
      (let ((learn-mult (if (fboundp 'presence-learning-multiplier)
                            (presence-learning-multiplier)
                            1.0)))
        (if correct
            (progn
              (incf (expert-hits learner))
              (incf (expert-life learner) (* 0.1 learn-mult))  ; presence-modulated
              (dolist (scale *context-scales*)
                (let ((key (make-context-key ctx scale)))
                  (when key (update-pattern-stats! key t)))))
            (progn
              (incf (expert-misses learner))
              (decf (expert-life learner) (* 0.03 learn-mult)))))  ; presence-modulated
      (when verified-p
        (dolist (scale *context-scales*)
          (let ((key (make-context-key ctx scale)))
            (when key
              (setf (gethash key (expert-knowledge learner)) actual)
              (update-pattern-stats! key correct))))
        (let ((c (expert-centroid learner))
              (ctx-emb (context-embedding ctx)))
          (when (and c ctx-emb)
            (dotimes (i *emb-dim*)
              (setf (aref c i) (+ (* 0.9 (aref c i)) (* 0.1 (aref ctx-emb i)))))
            (vnorm! c)))
        (unless (context-owner ctx)
          (claim-context! learner ctx))
        (unless (expert-owned-type learner)
          (setf (expert-owned-type learner) (get-type actual)))
        ;; P19: Update extended state for experts with those capabilities
        (when (program-has-op-p (expert-program learner) 'check-type-pattern)
          (setf (expert-type-pattern-memory learner)
                (when *type-buffer*
                  (subseq *type-buffer* 0 (min *type-pattern-length* 
                                               (length *type-buffer*))))))
        (when (program-has-op-p (expert-program learner) 'check-traveler-links)
          (store-traveler-link! learner ctx actual (if correct 0.9 0.5))))
      ;; Track token contexts for distributional similarity
      (update-token-context! actual ctx)
      (update-type-cluster! actual ctx)
      (update-attention! ctx correct)
      ;; Track outcomes for drive computation (COMPETENCE drive needs this)
      (push (cons ctx correct) *recent-outcomes*)
      (when (> (length *recent-outcomes*) 100)
        (setf *recent-outcomes* (subseq *recent-outcomes* 0 100)))
      ;; External outcomes tracked separately for COMPETENCE (not diluted by rehearsal)
      (push (cons ctx correct) *external-outcomes*)
      (when (> (length *external-outcomes*) 100)
        (setf *external-outcomes* (subseq *external-outcomes* 0 100)))
      ;; Post-learn hook
      (run-hook +hook-post-learn+ ctx actual predicted correct learner))
    correct))

;;; ============================================================================
;;; LIFECYCLE
;;; ============================================================================

(defun lifecycle-step! ()
  "Run lifecycle maintenance. HOOK: +hook-expert-dying+ fired before death.
   Organic: decay reflects relevance, inheritance reflects demonstrated value."
  ;; Organic metabolism: decay based on non-use rather than fixed rate
  ;; NOTE: In live mode, rehearsal only feeds ~1 expert per call, so
  ;; inactivity penalty must be low enough that unfed experts survive
  (dolist (e *experts*)
    (let* ((steps-since-used (- *step* (or (expert-last-used e) 0)))
           ;; Base decay + gentle inactivity + program cost
           ;; Program cost reduced from 0.5 to 0.05 - was causing death in 14 steps!
           ;; With 0.05: decay ~0.007/step, ~130 steps to death, experts can survive
           (decay (+ (* *metabolism* 0.3)  ; base ~0.0006
                     (* 0.00002 (min 200 steps-since-used))  ; inactivity max 0.004
                     (* 0.05 (expert-program-cost e)))))  ; 10% of program cost
      (decf (expert-life e) decay)))
  (let* ((candidates (remove-if (lambda (e) (> (expert-life e) *death-threshold*)) *experts*))
         ;; Limit deaths to 20% of population per step to prevent cliff edges
         (max-deaths (max 1 (floor (length *experts*) 5)))
         (dead (if (> (length candidates) max-deaths)
                   (subseq (sort candidates #'< :key #'expert-life) 0 max-deaths)
                   candidates)))
    (dolist (d dead)
      (let ((heir (find-expert-by-similarity-excluding 
                   (first (expert-owned-contexts d)) (expert-id d))))
        ;; Fire dying hook before processing death
        (run-hook +hook-expert-dying+ d heir)
        (when heir
          (maphash (lambda (k v)
                     (unless (gethash k (expert-knowledge heir))
                       (setf (gethash k (expert-knowledge heir)) v)))
                   (expert-knowledge d))
          (dolist (ctx (expert-owned-contexts d))
            (setf (gethash ctx *context-owners*) (expert-id heir))
            (pushnew ctx (expert-owned-contexts heir) :test #'equal))
          ;; Organic inheritance: based on demonstrated value, not random
          (when (and (> (expert-hits d) (expert-misses d))
                     (> (length (get-op-names (expert-program d))) 
                        (length (get-op-names (expert-program heir)))))
            ;; Inherit if parent's hit rate was better than heir's
            (let ((d-rate (/ (expert-hits d) (max 1 (+ (expert-hits d) (expert-misses d)))))
                  (heir-rate (/ (expert-hits heir) (max 1 (+ (expert-hits heir) (expert-misses heir))))))
              (when (> d-rate heir-rate)  ; inherit if parent performed better
                (setf (expert-program heir) (inherit-program (expert-program d)))
                (update-expert-program-cost! heir)))))
        (unless heir
          ;; No heir - consolidate valuable knowledge to long-term memory
          (maphash (lambda (key prediction)
                     (let ((stats (gethash key *pattern-stats*)))
                       (when (and stats (> (cdr stats) 2))  ; At least 2 correct
                         (unless (gethash key *long-term-memory*)
                           (consolidate-pattern! key prediction)))))
                   (expert-knowledge d))
          (dolist (ctx (expert-owned-contexts d))
            (remhash ctx *context-owners*))))
      (let ((nbhd (find-neighborhood-by-id (expert-neighborhood d))))
        (when nbhd
          (setf (neighborhood-experts nbhd)
                (remove (expert-id d) (neighborhood-experts nbhd)))))
      (let ((trav (find-traveler-for-expert (expert-id d))))
        (when trav (setf *travelers* (remove trav *travelers*))))
      (incf *total-deaths*)
      ;; Remove this expert from the list (was bug: removed ALL below threshold, not just dead list)
      (setf *experts* (remove d *experts*)))))

;;; ============================================================================
;;; PROCESSING
;;; ============================================================================

(defun process-chunk! (text &key verbose carry-over)
  "Process a chunk of text with hook integration.
   HOOKS: +hook-pre-process-token+, +hook-post-process-token+, +hook-maintenance+"
  (let ((tokens (if carry-over 
                    (append carry-over (tokenize text))
                    (tokenize text)))
        (context nil) (correct 0) (total 0) (verified 0) (skipped 0))
    (when *morpheme-model*
      (learn-morphemes! text *morpheme-model*))
    ;; Build co-occurrence for embeddings
    (loop for i from 0 below (length tokens)
          do (loop for j from (max 0 (- i 2)) to (min (1- (length tokens)) (+ i 2))
                   when (/= i j) do (attract! (nth i tokens) (nth j tokens) 0.15)))
    ;; Process tokens
    (loop for tok in tokens
          do (incf *step*)
             ;; Pre-process hook - can return op-result to override prediction
             (let ((hook-result (run-hook +hook-pre-process-token+ tok context *step*)))
               (when (>= (length context) 1)
                 (incf total)
                 (setf *call-stack* nil)
                 (setf *execution-trace* nil)  ; Clear for new execution
                 (let* ((ctx (subseq context 0 (min (apply #'max *context-scales*) (length context))))
                        ;; Use hook result if it's an op-result, otherwise call execute-with-routing
                        (result (if (and hook-result (op-result-p hook-result))
                                    hook-result
                                    (execute-with-routing ctx)))
                        (predicted (op-result-value result))
                        (confidence (op-result-confidence result))
                        (verify (should-verify-p ctx confidence))
                        (got-it (eq predicted tok)))
                 ;; Record cognitive trace for concept detection (if deep-mind loaded)
                 (when (and (fboundp 'record-trace!) (fboundp 'make-cognitive-trace))
                   (record-trace!
                    (funcall 'make-cognitive-trace
                     :step *step*
                     :context ctx
                     :prediction predicted
                     :actual tok
                     :confidence (float (or confidence 0.5) 0.0)
                     :surprise (if got-it 0.0 1.0)
                     :expert-id (when *last-answering-expert*
                                  (expert-id *last-answering-expert*))
                     :meta-level 0)))
                 ;; Record program performance for self-model
                 (when (and *last-answering-expert* (fboundp 'record-program-performance!))
                   (record-program-performance! *last-answering-expert* got-it))
                 ;; Record outcome for self-model error tracking
                 (when (fboundp 'record-outcome!)
                   (record-outcome! got-it))
                 ;; Self-observation: track confidence distribution
                 (let ((conf (or confidence 0.5)))
                   (push conf *confidence-history*)
                   (when (> (length *confidence-history*) 100)
                     (setf *confidence-history* (subseq *confidence-history* 0 100)))
                   (cond ((< conf 0.3) (incf *low-confidence-count*))
                         ((> conf 0.7) (incf *high-confidence-count*))))
                 (if verify
                     (progn
                       (incf verified)
                       (when got-it (incf correct))
                       (learn! ctx tok predicted t))
                     (progn
                       (incf skipped)
                       (update-attention! ctx got-it)))
                 ;; Post-process hook
                 (run-hook +hook-post-process-token+ tok ctx predicted got-it)
                 (when verbose
                   (format t "~4D: ~12A -> ~8A/~8A ~A ~A conf:~,2F~%"
                           *step* 
                           (format nil "(~{~A~^ ~})" (reverse (subseq ctx 0 (min 2 (length ctx)))))
                           (or predicted '_) tok
                           (if got-it "[OK]" "[X]")
                           (if verify "V" "S")
                           confidence)))))
             (push tok context)
             (when (> (length context) (apply #'max *context-scales*))
               (setf context (subseq context 0 (apply #'max *context-scales*))))
             ;; P19: Update global context state
             (update-global-context-state! tok))
    ;; Run maintenance
    (run-consolidation!)
    (detect-and-create-travelers!)
    (recluster-types!)
    (run-compression!)
    (lifecycle-step!)
    ;; Update adaptive threshold when accuracy drifts (organic — not timer)
    (when (and (fboundp 'update-adaptive-threshold!)
               (> *step* *last-accuracy-snapshot*))
      (let ((current-acc (compute-recent-accuracy 50)))
        (when (> (abs (- current-acc (or (cdar *learning-rate-window*) 0.5))) 0.1)
          (update-adaptive-threshold!)
          ;; Self-observation: snapshot when accuracy changes significantly
          (push (cons *step* current-acc) *learning-rate-window*)
          (when (> (length *learning-rate-window*) 20)
            (setf *learning-rate-window* (subseq *learning-rate-window* 0 20)))
          (setf *last-accuracy-snapshot* *step*))))
    ;; Program optimization when experts are struggling (organic — fitness-driven)
    (when (and (fboundp 'run-program-optimization!)
               (boundp '*experts*)
               (> (length *experts*) 3)
               (< (/ (reduce #'+ *experts* :key #'expert-life) (length *experts*)) 0.4))
      (run-program-optimization!))
    ;; Maintenance hook
    (run-hook +hook-maintenance+)
    (values (if (> verified 0) (float (/ correct verified)) 0.0)
            total verified skipped
            (when (> (length context) 0)
              (subseq context 0 (min 3 (length context)))))))

(defun process-text! (text &key verbose)
  (multiple-value-bind (acc total verified skipped)
      (process-chunk! text :verbose verbose)
    (when verbose
      (format t "~%Accuracy: ~,1F% (verified ~D/~D, skipped ~D)~%"
              (* 100 acc) verified total skipped))
    acc))

(defun process-file! (path &key (chunk-size 1000) verbose (report-interval 10))
  (with-open-file (stream path :direction :input :if-does-not-exist nil)
    (unless stream
      (format t "File not found: ~A~%" path)
      (return-from process-file! nil))
    (let ((total-chunks 0) (total-correct 0) (total-verified 0) (total-tokens 0)
          (carry-over nil) (buffer (make-string chunk-size)))
      (loop
        (let ((chars-read (read-sequence buffer stream)))
          (when (zerop chars-read) (return))
          (let ((chunk (subseq buffer 0 chars-read)))
            (incf total-chunks)
            (multiple-value-bind (acc tokens verified skipped new-carry)
                (process-chunk! chunk :verbose nil :carry-over carry-over)
              (incf total-tokens tokens)
              (incf total-verified verified)
              (incf total-correct (round (* acc verified)))
              (setf carry-over new-carry)
              (when (and verbose (zerop (mod total-chunks report-interval)))
                (format t "Chunk ~D: ~D tokens, acc=~,1F%~%"
                        total-chunks total-tokens 
                        (if (> total-verified 0) (* 100 (/ total-correct total-verified)) 0.0)))))))
      (format t "~%=== Complete === Tokens: ~D, Acc: ~,1F%~%"
              total-tokens (if (> total-verified 0) (* 100 (/ total-correct total-verified)) 0.0))
      (if (> total-verified 0) (float (/ total-correct total-verified)) 0.0))))

;;; ============================================================================
;;; GENERATION
;;; ============================================================================

(defun collect-vote-distribution (context &key debug)
  "Collect votes from all sources, returning hash-table of token -> score."
  (let ((votes (make-hash-table :test 'eq))
        (sources-used 0))
    ;; 1. Consolidated memory (high weight)
    (dolist (scale *context-scales*)
      (let* ((key (make-context-key context scale))
             (pattern (when key (gethash key *long-term-memory*))))
        (when pattern
          (let* ((pred (consolidated-pattern-prediction pattern))
                 (conf (consolidated-pattern-confidence pattern))
                 (weight (* 2.0 conf)))
            (incf (gethash pred votes 0.0) weight)
            (incf sources-used)
            (when debug (format t "  [CONS] ~A -> ~A (w=~,2F)~%" key pred weight))))))
    ;; 2. Expert knowledge across scales
    (loop for scale in *context-scales*
          for scale-weight in *scale-weights*
          do (let ((key (make-context-key context scale)))
               (when key
                 (dolist (e *experts*)
                   (when (> (expert-life e) 0.1)
                     (let ((pred (gethash key (expert-knowledge e))))
                       (when pred
                         (let ((weight (* scale-weight (expert-life e))))
                           (incf (gethash pred votes 0.0) weight)
                           (incf sources-used)
                           (when debug 
                             (format t "  [E~A] ~A -> ~A (w=~,2F)~%" 
                                     (expert-id e) key pred weight))))))))))
    ;; 3. Traveler knowledge
    (dolist (trav *travelers*)
      (let ((expert (find-expert-by-id (traveler-expert-id trav))))
        (when (and expert (> (expert-life expert) 0.1))
          (dolist (scale *context-scales*)
            (let* ((key (make-context-key context scale))
                   (pred (when key (gethash key (expert-knowledge expert)))))
              (when pred
                (incf (gethash pred votes 0.0) 0.5)
                (incf sources-used)
                (when debug
                  (format t "  [TRAV] ~A -> ~A (w=0.5)~%" key pred))))))))
    ;; 4. Unigram fallback
    (when (and (= (hash-table-count votes) 0) context)
      (let ((last-tok (first context)))
        (dolist (e *experts*)
          (when (> (expert-life e) 0.1)
            (maphash (lambda (key pred)
                       (when (and (listp key) 
                                  (>= (length key) 1)
                                  (eq (first key) last-tok))
                         (incf (gethash pred votes 0.0) 0.3)
                         (incf sources-used)))
                     (expert-knowledge e))))
        (when debug
          (format t "  [UNIGRAM fallback] last-tok=~A, found ~A predictions~%" 
                  last-tok (hash-table-count votes)))))
    (when debug
      (format t "  Total sources: ~A, unique predictions: ~A~%" 
              sources-used (hash-table-count votes)))
    (values votes sources-used)))

(defun normalize-votes (votes)
  (let ((total 0.0))
    (maphash (lambda (k v) (declare (ignore k)) (incf total v)) votes)
    (when (> total 0)
      (maphash (lambda (k v) (setf (gethash k votes) (/ v total))) votes))
    votes))

(defun apply-temperature (votes temperature)
  (when (and (> temperature 0) (/= temperature 1.0))
    (let ((inv-temp (/ 1.0 temperature)))
      (maphash (lambda (k v) 
                 (setf (gethash k votes) (expt (max v 0.0001) inv-temp)))
               votes)
      (normalize-votes votes)))
  votes)

(defun apply-repetition-penalty (votes generated penalty)
  (when (and generated (> penalty 1.0))
    (let ((recent-set (make-hash-table :test 'eq))
          (window-size (min 10 (length generated))))
      (loop for tok in (subseq generated 0 (min window-size (length generated)))
            for i from 0
            for recency-weight = (/ 1.0 (1+ i))
            do (setf (gethash tok recent-set) 
                     (max (gethash tok recent-set 0) recency-weight)))
      (maphash (lambda (tok recency)
                 (let ((current (gethash tok votes)))
                   (when current
                     (setf (gethash tok votes) 
                           (/ current (expt penalty recency))))))
               recent-set)
      (normalize-votes votes)))
  votes)

(defun nucleus-filter (votes top-p)
  (when (and (< top-p 1.0) (> (hash-table-count votes) 1))
    (let ((items nil))
      (maphash (lambda (k v) (push (cons k v) items)) votes)
      (setf items (sort items #'> :key #'cdr))
      (let ((cumsum 0.0)
            (nucleus nil))
        (dolist (item items)
          (when (> cumsum top-p)
            (return))
          (incf cumsum (cdr item))
          (push item nucleus))
        (clrhash votes)
        (dolist (item nucleus)
          (setf (gethash (car item) votes) (cdr item)))
        (normalize-votes votes))))
  votes)

(defun sample-from-distribution (votes)
  (when (= (hash-table-count votes) 0)
    (return-from sample-from-distribution nil))
  (let ((items nil))
    (maphash (lambda (k v) (push (cons k v) items)) votes)
    (let ((r (random 1.0))
          (cumsum 0.0))
      (dolist (item items)
        (incf cumsum (cdr item))
        (when (>= cumsum r)
          (return-from sample-from-distribution (car item))))
      (caar (sort items #'> :key #'cdr)))))

(defun greedy-from-distribution (votes)
  (let ((best nil) (best-score 0.0))
    (maphash (lambda (k v)
               (when (> v best-score)
                 (setf best k best-score v)))
             votes)
    best))

(defun generate-prediction (context)
  (multiple-value-bind (votes sources) (collect-vote-distribution context)
    (when (> (hash-table-count votes) 0)
      (normalize-votes votes)
      (let ((pred (get-most-likely-token votes)))
        (values pred (or (gethash pred votes) 0.0))))))
;;; Beam search structures and functions
(defstruct beam-state
  (tokens nil)
  (context nil)
  (score 0.0)
  (finished nil))

(defun beam-search-step (states beam-width max-ctx temperature top-p rep-penalty &key debug)
  (let ((candidates nil))
    (dolist (state states)
      (unless (beam-state-finished state)
        (multiple-value-bind (votes sources) 
            (collect-vote-distribution (beam-state-context state) :debug debug)
          (when debug
            (format t "Beam step: context=~A, votes=~A~%" 
                    (beam-state-context state) (hash-table-count votes)))
          (when (> (hash-table-count votes) 0)
            (normalize-votes votes)
            (apply-temperature votes temperature)
            (apply-repetition-penalty votes (beam-state-tokens state) rep-penalty)
            (nucleus-filter votes top-p)
            (maphash (lambda (tok prob)
                       (when (> prob 0.001)
                         (let* ((new-tokens (cons tok (beam-state-tokens state)))
                                (new-context (cons tok (beam-state-context state))))
                           (when (> (length new-context) max-ctx)
                             (setf new-context (subseq new-context 0 max-ctx)))
                           (let ((new-score (+ (beam-state-score state) 
                                              (log (max prob 0.0001)))))
                             (push (make-beam-state
                                    :tokens new-tokens
                                    :context new-context
                                    :score new-score)
                                   candidates)))))
                     votes)))))
    (when (null candidates)
      (return-from beam-search-step 
        (mapcar (lambda (s) 
                  (make-beam-state :tokens (beam-state-tokens s)
                                   :context (beam-state-context s)
                                   :score (beam-state-score s)
                                   :finished t))
                states)))
    (let ((sorted (sort candidates #'> :key #'beam-state-score)))
      (subseq sorted 0 (min beam-width (length sorted))))))

(defun beam-search (seed-tokens &key (length 20) (beam-width 4) 
                                     (temperature 1.0) (top-p 0.9) (rep-penalty 1.2)
                                     debug)
  (let* ((max-ctx (apply #'max *context-scales*))
         (initial-context (reverse seed-tokens)))
    (when (> (length initial-context) max-ctx)
      (setf initial-context (subseq initial-context 0 max-ctx)))
    (when debug
      (format t "Beam search: seed=~A, initial-context=~A~%" seed-tokens initial-context))
    (let ((states (list (make-beam-state :context initial-context :score 0.0))))
      (dotimes (i length)
        (when debug (format t "~%=== Beam iteration ~A ===~%" i))
        (let ((new-states (beam-search-step states beam-width max-ctx 
                                            temperature top-p rep-penalty 
                                            :debug debug)))
          (when (or (null new-states)
                    (every #'beam-state-finished new-states))
            (return))
          (setf states new-states)))
      (when states
        (let ((best (first (sort (copy-list states) #'> :key #'beam-state-score))))
          (nreverse (beam-state-tokens best)))))))

(defun generate (seed-text &key (length 20) (method :greedy)
                               (temperature 1.0) (top-p 1.0) (rep-penalty 1.0)
                               (beam-width 4))
  "Generate text continuation with advanced options."
  (let* ((tokens (if (listp seed-text) 
                     seed-text 
                     (tokenize seed-text)))
         (max-ctx (apply #'max *context-scales*)))
    (case method
      (:beam
       (let ((generated (beam-search tokens 
                                     :length length 
                                     :beam-width beam-width
                                     :temperature temperature
                                     :top-p top-p
                                     :rep-penalty rep-penalty)))
         (format nil "~{~A~^ ~}" (append tokens generated))))
      (otherwise
       (let ((context (reverse tokens))
             (generated nil))
         (when (> (length context) max-ctx)
           (setf context (subseq context 0 max-ctx)))
         (dotimes (i length)
           (multiple-value-bind (votes sources) (collect-vote-distribution context)
             (when (= (hash-table-count votes) 0)
               (return))
             (normalize-votes votes)
             (apply-temperature votes temperature)
             (apply-repetition-penalty votes generated rep-penalty)
             (nucleus-filter votes top-p)
             (let ((pred (if (eq method :sample)
                             (sample-from-distribution votes)
                             (greedy-from-distribution votes))))
               (unless pred (return))
               (push pred generated)
               (push pred context)
               (when (> (length context) max-ctx)
                 (setf context (subseq context 0 max-ctx))))))
         (format nil "~{~A~^ ~}" (append tokens (nreverse generated))))))))

(defun generate-variants (seed-text &key (count 5) (length 15) 
                                         (temperature 0.8) (top-p 0.9) (rep-penalty 1.3))
  (loop for i from 1 to count
        collect (generate seed-text 
                         :length length 
                         :method :sample
                         :temperature temperature
                         :top-p top-p
                         :rep-penalty rep-penalty)))

(defun compare-generation-methods (seed-text &key (length 10))
  (format t "~%=== Generation Method Comparison ===~%")
  (format t "Seed: ~S~%~%" seed-text)
  (format t "Greedy:~%  ~A~%~%" 
          (generate seed-text :length length :method :greedy))
  (format t "Sample (temp=0.7):~%  ~A~%~%"
          (generate seed-text :length length :method :sample :temperature 0.7))
  (format t "Sample (temp=1.2):~%  ~A~%~%"
          (generate seed-text :length length :method :sample :temperature 1.2))
  (format t "Beam (width=4):~%  ~A~%~%"
          (generate seed-text :length length :method :beam :beam-width 4))
  (format t "=====================================~%"))

;;; ============================================================================
;;; INITIALIZATION
;;; ============================================================================

(defun reset! ()
  "Reset the system. HOOKS: +hook-pre-reset+, +hook-post-reset+"
  (format t "--- EXECUTING RESET! ---~%")
  (run-hook +hook-pre-reset+)
  ;; Don't clear hooks - modules register them at load time and expect them to persist
  (setf *experts* nil
        *expert-counter* 0
        *neighborhoods* nil
        *root-neighborhood* nil
        *context-owners* (make-hash-table :test 'equal)
        *embeddings* (make-hash-table :test 'eq)
        *type-registry* (make-hash-table :test 'eq)
        *type-clusters* (make-hash-table :test 'eq)
        *token-contexts* (make-hash-table :test 'eq)
        *type-counter* 0
        *step* 0
        *total-births* 0
        *total-deaths* 0
        *call-stack* nil
        *execution-trace* nil
        *attention-map* (make-hash-table :test 'equal)
        *context-encounters* (make-hash-table :test 'equal)
        *recent-outcomes* nil
        *external-outcomes* nil
        *long-term-memory* (make-hash-table :test 'equal)
        *pattern-stats* (make-hash-table :test 'equal)
        *last-consolidation* 0
        *verifications-skipped* 0
        *verifications-performed* 0
        *attention-forced-verifications* 0
        *travelers* nil
        *traveler-counter* 0
        *last-traveler-pass* 0
        *last-compression* 0
        *last-decay* 0
        *patterns-pruned* 0
        *experts-merged* 0
        *last-recluster* 0
        *morpheme-model* (create-morpheme-model)
        *chunk-buffer* nil
        *chunk-embedding* nil
        *type-buffer* nil
        *phrase-model* nil
        *sentence-model* nil
        *last-answering-expert* nil)
  (when (fboundp 'holo-reset!) (holo-reset!)) ; Initialize holographic store
  (create-root-neighborhood)
  (dotimes (i *initial-experts*)
    (spawn-expert))
  (run-hook +hook-post-reset+)
  (format t "UHMA v6.1 (Homoiconic Core) initialized with ~A experts.~%" *initial-experts*))

;;; ============================================================================
;;; DIAGNOSTICS
;;; ============================================================================

(defun print-status ()
  (format t "~%========== UHMA v6.1 Status (Homoiconic) ==========~%")
  (format t "Step: ~A~%" *step*)
  (format t "Experts: ~A~%" (length *experts*))
  (format t "Neighborhoods: ~A~%" (length *neighborhoods*))
  (format t "Travelers: ~A~%" (length *travelers*))
  (format t "Type clusters: ~A~%" (hash-table-count *type-clusters*))
  (format t "Consolidated: ~A~%" (hash-table-count *long-term-memory*))
  (format t "Births: ~A, Deaths: ~A~%" *total-births* *total-deaths*)
  (let ((total (+ *verifications-performed* *verifications-skipped*)))
    (format t "Sparse ratio: ~,1F% (~A/~A skipped)~%" 
            (if (> total 0) (* 100 (/ *verifications-skipped* total)) 0.0)
            *verifications-skipped* total))
  (format t "Patterns pruned: ~A, Experts merged: ~A~%"
          *patterns-pruned* *experts-merged*)
  (format t "=================================================~%"))

(defun print-hierarchy ()
  (format t "~%========== Hierarchy ==========~%")
  (labels ((print-nbhd (n indent)
             (format t "~A[~A] d=~A e=~A~%"
                     (make-string (* indent 2) :initial-element #\Space)
                     (neighborhood-id n)
                     (neighborhood-depth n)
                     (length (neighborhood-experts n)))
             (dolist (child (neighborhood-children n))
               (print-nbhd child (1+ indent)))))
    (when *root-neighborhood*
      (print-nbhd *root-neighborhood* 0)))
  (format t "================================~%"))

(defun print-experts ()
  (format t "~%========== Experts ==========~%")
  (dolist (e (sort (copy-list *experts*) #'> :key #'expert-life))
    (format t "E~A~A: life=~,2F h=~A m=~A knows=~A ops=~A~%"
            (expert-id e)
            (if (expert-is-traveler-p e) "[T]" "")
            (expert-life e) (expert-hits e) (expert-misses e)
            (hash-table-count (expert-knowledge e))
            (length (expert-program e))))
  (format t "==============================~%"))

(defun print-consolidated ()
  (format t "~%========== Consolidated ==========~%")
  (let ((count 0))
    (maphash (lambda (k v)
               (when (< count 15)
                 (format t "~A -> ~A (conf=~,2F)~%" 
                         k 
                         (consolidated-pattern-prediction v)
                         (consolidated-pattern-confidence v))
                 (incf count)))
             *long-term-memory*))
  (format t "==================================~%"))

(defun print-hooks ()
  (format t "~%========== Registered Hooks ==========~%")
  (let ((count 0))
    (maphash (lambda (name entries)
               (when entries
                 (format t "~A: ~A handler(s)~%" name (length entries))
                 (incf count (length entries))))
             *hooks*)
    (format t "Total: ~A handlers across ~A hooks~%" 
            count (hash-table-count *hooks*)))
  (format t "=======================================~%"))

;;; ============================================================================
;;; HOMOICONIC DIAGNOSTICS - NEW
;;; ============================================================================

(defun print-expert-program (expert-or-id)
  "Print an expert's S-expression program - THE ACTUAL CODE"
  (let ((e (if (numberp expert-or-id) 
               (find-expert-by-id expert-or-id)
               expert-or-id)))
    (unless e
      (format t "Expert not found~%")
      (return-from print-expert-program nil))
    (format t "~%========== Expert ~A Program ==========~%" (expert-id e))
    (format t "Life: ~,2F  Hits: ~A  Misses: ~A~%" 
            (expert-life e) (expert-hits e) (expert-misses e))
    (format t "~%Operations: ~{~A~^, ~}~%" (get-op-names (expert-program e)))
    (format t "~%Modifiable Parameters:~%")
    (dolist (param (get-modifiable-params (expert-program e)))
      (format t "  ~A.~A = ~S~%" (first param) (second param) (third param)))
    (format t "~%Program Cost: ~,4F~%" (expert-program-cost e))
    (format t "~%Full Program S-Expression:~%")
    (let ((*print-pretty* t)
          (*print-right-margin* 80))
      (format t "~S~%" (expert-program e)))
    (format t "=========================================~%")))

(defun print-code-metrics (expert-or-id)
  "Print an expert's per-operation metrics"
  (let ((e (if (numberp expert-or-id) 
               (find-expert-by-id expert-or-id)
               expert-or-id)))
    (unless e
      (format t "Expert not found~%")
      (return-from print-code-metrics nil))
    (format t "~%========== Expert ~A Code Metrics ==========~%" (expert-id e))
    (if (expert-code-metrics e)
        (maphash (lambda (op-name metrics)
                   (format t "~A:~%" op-name)
                   (format t "  invocations: ~A~%" (op-metrics-invocations metrics))
                   (format t "  returns: ~A  continues: ~A~%" 
                           (op-metrics-returns metrics)
                           (op-metrics-continues metrics))
                   (format t "  delegates: ~A  spawns: ~A~%"
                           (op-metrics-delegates metrics)
                           (op-metrics-spawns metrics))
                   (when (> (op-metrics-returns metrics) 0)
                     (format t "  avg-confidence: ~,2F~%"
                             (/ (op-metrics-total-confidence metrics)
                                (op-metrics-returns metrics)))))
                 (expert-code-metrics e))
        (format t "  No metrics collected yet~%"))
    (format t "=============================================~%")))

(defun print-program-stats ()
  "Print statistics about expert program evolution"
  (format t "~%========== Program Evolution Stats ==========~%")
  (let ((total-experts (length *experts*))
        (extended-count 0)
        (op-counts (make-hash-table)))
    (dolist (e *experts*)
      (let ((op-names (get-op-names (expert-program e)))
            (has-extended nil))
        (dolist (op-name op-names)
          (incf (gethash op-name op-counts 0))
          (when (member op-name *extended-op-names*)
            (setf has-extended t)))
        (when has-extended
          (incf extended-count))))
    (format t "Total experts: ~A~%" total-experts)
    (format t "Experts with extended ops: ~A (~,1F%)~%"
            extended-count
            (if (> total-experts 0) (* 100.0 (/ extended-count total-experts)) 0.0))
    (format t "~%Operation frequency:~%")
    (maphash (lambda (name count)
               (format t "  ~A: ~A (~,1F%)~%"
                       name count
                       (if (> total-experts 0) (* 100.0 (/ count total-experts)) 0.0)))
             op-counts)
    (format t "~%Program costs:~%")
    (let ((costs nil))
      (dolist (e *experts*)
        (push (expert-program-cost e) costs))
      (when costs
        (format t "  Min: ~,4F~%" (apply #'min costs))
        (format t "  Max: ~,4F~%" (apply #'max costs))
        (format t "  Avg: ~,4F~%" (/ (apply #'+ costs) (length costs))))))
  (format t "=============================================~%"))

(defun print-expert-programs (&optional (n 10))
  "Print programs of top N experts by life"
  (format t "~%========== Expert Programs (Top ~A) ==========~%" n)
  (let ((sorted (sort (copy-list *experts*) #'> :key #'expert-life)))
    (dolist (e (subseq sorted 0 (min n (length sorted))))
      (format t "E~A (life=~,2F, h=~A, m=~A, cost=~,3F):~%"
              (expert-id e)
              (expert-life e)
              (expert-hits e)
              (expert-misses e)
              (expert-program-cost e))
      (format t "  Operations: ~{~A~^, ~}~%" (get-op-names (expert-program e)))
      (format t "  Modifiable: ~{~S~^, ~}~%" (get-modifiable-params (expert-program e)))
      (when (> (expert-hidden-state-strength e) 0)
        (format t "  Hidden state strength: ~,2F~%" (expert-hidden-state-strength e)))
      (when (expert-type-pattern-memory e)
        (format t "  Type pattern: ~A~%" (expert-type-pattern-memory e)))
      (when (expert-traveler-links e)
        (format t "  Traveler links: ~A~%" (length (expert-traveler-links e))))))
  (format t "==============================================~%"))

;;; ============================================================================
;;; PERSISTENCE
;;; ============================================================================

(defun save-state (path)
  (with-open-file (s path :direction :output :if-exists :supersede)
    (format s ";; UHMA v6.1 Homoiconic State~%")
    (format s "(setf *step* ~A)~%" *step*)
    (format s "(setf *total-births* ~A)~%" *total-births*)
    (format s "(setf *total-deaths* ~A)~%" *total-deaths*)
    (format t "State saved to ~A~%" path)))

(defun load-state (path)
  (load path)
  (format t "State loaded from ~A~%" path))

;;; ============================================================================
;;; DEMO
;;; ============================================================================

(defun demo ()
  (format t "~%================================================================~%")
  (format t "UHMA v6.1 HOMOICONIC - UNIVERSAL HOMOICONIC MEMORY ARCHITECTURE~%")
  (format t "================================================================~%")
  (format t "Phase 2: Expert programs are now S-expressions~%")
  (format t "  - Programs can be executed (via eval)~%")
  (format t "  - Programs can be read (walked as data)~%")
  (format t "  - Programs can be modified (by self-model)~%")
  (format t "================================================================~%")
  
  (reset!)
  
  (format t "~%--- Phase 1: Basic Learning ---~%")
  (dotimes (i 5) (process-text! "the cat sat on the mat" :verbose nil))
  (process-text! "the cat sat on the mat" :verbose t)
  
  (format t "~%--- Phase 2: Multiple Patterns ---~%")
  (dotimes (i 5)
    (process-text! "the dog ran in the park" :verbose nil)
    (process-text! "the bird flew over the tree" :verbose nil))
  
  (format t "~%--- Phase 3: Show Homoiconic Programs ---~%")
  (print-expert-programs 3)
  
  (format t "~%--- Phase 4: Heavy Training ---~%")
  (dotimes (i 50)
    (process-text! "the cat sat on the mat" :verbose nil)
    (process-text! "the dog ran in the park" :verbose nil)
    (process-text! "the bird flew over the tree" :verbose nil))
  
  (format t "~%--- Phase 5: Show Code Metrics ---~%")
  (when *experts*
    (print-code-metrics (first *experts*)))
  
  (format t "~%--- Phase 6: Generation ---~%")
  (format t "Greedy 'the cat': ~A~%" (generate "the cat" :length 8 :method :greedy))
  (format t "Greedy 'the dog': ~A~%" (generate "the dog" :length 8 :method :greedy))
  (format t "Beam 'the bird': ~A~%" (generate "the bird" :length 8 :method :beam))
  
  (format t "~%--- Final Status ---~%")
  (print-status)
  (print-program-stats)
  
  (format t "~%Demo complete.~%"))

;;; ============================================================================
;;; LOAD MESSAGE
;;; ============================================================================

(format t "~%")
(format t "================================================================~%")
(format t "UHMA v6.1 TRUE HOMOICONIC CORE MODULE LOADED~%")
(format t "================================================================~%")
(format t "Expert programs are REAL S-expressions that get EVAL'd:~%")
(format t "  - Executable (via eval with bindings)~%")
(format t "  - Readable (walkable as data - get-op-names, get-modifiable-params)~%")
(format t "  - Modifiable (set-modifiable-param!, insert-op-before!, remove-op!)~%")
(format t "~%")
(format t "NO HIDDEN LAMBDAS - the code IS the data.~%")
(format t "~%")
(format t "Quick Start:~%")
(format t "  (reset!)~%")
(format t "  (process-text! \"the cat sat on the mat\")~%")
(format t "  (generate \"the cat\" :length 6)~%")
(format t "  (print-expert-program (first *experts*))~%")
(format t "~%")
(format t "Introspection:~%")
(format t "  (get-op-names (expert-program e))~%")
(format t "  (get-modifiable-params (expert-program e))~%")
(format t "  (find-op-in-program (expert-program e) 'assess-confidence)~%")
(format t "~%")
(format t "Modification:~%")
(format t "  (set-modifiable-param! prog 'try-similar 'threshold 0.6)~%")
(format t "  (insert-op-before! prog 'assess-confidence *op-check-hidden-state*)~%")
(format t "  (remove-op! prog 'check-type)~%")
(format t "================================================================~%")

(defun segment-word (word &optional (model *morpheme-model*))
  "Segment WORD using MODEL."
  (unless model
    (return-from segment-word (list (intern (string-upcase word)))))
  (let ((w (string-downcase word)))
    (if (< (length w) 6)
        (list (intern (string-upcase w)))
        (loop for m in (segment-word-recursive model w 2)
              collect (intern (string-upcase m))))))

(defun learn-morphemes-from-word! (model word)
  "Update model based on single WORD."
  (let ((w (string-downcase word)))
    (when (>= (length w) *morpheme-min-length*)
      (update-substring-counts! model w)
      (multiple-value-bind (split-pos split-cost) (find-best-split model w)
        (when split-pos
          (let* ((boundary-key (format nil "~A|~A" 
                                       (subseq w (max 0 (- split-pos 2)) split-pos)
                                       (subseq w split-pos (min (length w) (+ split-pos 2)))))
                 (current (gethash boundary-key (morpheme-model-boundaries model) 0.5))
                 (update-strength (min 0.3 (* *morpheme-learning-rate* (- (abs split-cost))))))
            (when (> update-strength 0)
              (setf (gethash boundary-key (morpheme-model-boundaries model))
                    (+ current (* update-strength (- 1.0 current)))))))))))

(defun create-morpheme-model ()
  "Creates and initializes a new morpheme model."
  (make-morpheme-model :boundaries (make-hash-table :test 'equal)
                       :vocabulary (make-hash-table :test 'equal)
                       :frequencies (make-hash-table :test 'equal)
                       :known-affixes (make-hash-table :test 'equal)))

(defun segment-word-recursive (model word min-len)
  "STUB: Recursively segments a word into morphemes using the model."
  (declare (ignore model min-len))
  (list word)) ; Return the word as a single morpheme for now

(defun update-substring-counts! (model word)
  "STUB: Updates substring frequency counts in the morpheme model."
  (declare (ignore model word))
  nil)

(defun find-best-split (model word)
  "STUB: Finds the best split point in a word for morpheme segmentation."
  (declare (ignore model word))
  (values nil nil)) ; No split for now
