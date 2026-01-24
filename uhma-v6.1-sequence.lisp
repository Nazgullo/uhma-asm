;;;; ============================================================================
;;;; UHMA SEQUENCE-LEVEL OPERATIONS (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Implements phrase detection, variable context, and sentence boundaries.
;;;; ============================================================================

(in-package :uhma)
(declaim (ftype function create-sentence-model))

;;; --- SECTION 4: STRUCTURES ---

;;; Sequence models moved to forward-decl.lisp

;;; --- SECTION 6: SPECIAL VARIABLES ---

(defvar *phrase-model* nil)
(defvar *context-scale-model* nil)
(defvar *sentence-model* nil)

;;; --- SECTION 8: CORE FUNCTIONS ---

(defun update-ngram-counts! (tokens)
  "Register token sequences in the atomic frequency map."
  (declare (type list tokens))
  (unless *phrase-model* (setf *phrase-model* (make-phrase-model)))
  (let ((pm *phrase-model*) (n (length tokens)))
    (dolist (tok tokens) (incf (gethash tok (phrase-model-unigram-counts pm) 0)) (incf (phrase-model-total-tokens pm)))
    (loop for i from 0 below (1- n) do (incf (gethash (list (nth i tokens) (nth (1+ i) tokens)) (phrase-model-bigram-counts pm) 0)))))

(defun compute-pmi (tokens)
  "Calculate Pointwise Mutual Information for a candidate phrase."
  (declare (type list tokens))
  (unless *phrase-model* (return-from compute-pmi 0.0))
  (let* ((pm *phrase-model*) (total (float (max 1 (phrase-model-total-tokens pm))))
         (p-phrase (/ (get-ngram-count tokens) total))
         (p-prod (reduce (lambda (acc t1) (* acc (/ (gethash t1 (phrase-model-unigram-counts pm) 1) total))) tokens :initial-value 1.0)))
    (if (> p-prod 0) (log (/ p-phrase p-prod) 2) 0.0)))

;;; --- SECTION 10: INITIALIZATION ---
(eval-when (:load-toplevel :execute)
  (register-hook +hook-post-process-token+ 'sequence-post-token-hook)
  (format t "[SEQUENCE] Multi-gram atoms and clausal boundaries active.~%"))

;;; ============================================================================
;;; PHRASE DETECTION: N-GRAM COUNTING
;;; ============================================================================

(defun update-ngram-counts! (tokens)
  "Update unigram through 4-gram counts from token sequence"
  (unless *phrase-model*
    (setf *phrase-model* (create-phrase-model)))
  (let ((pm *phrase-model*)
        (n (length tokens)))
    ;; Unigrams
    (dolist (tok tokens)
      (incf (gethash tok (phrase-model-unigram-counts pm) 0))
      (incf (phrase-model-total-tokens pm)))
    ;; Bigrams
    (loop for i from 0 below (1- n)
          for bigram = (list (nth i tokens) (nth (1+ i) tokens))
          do (incf (gethash bigram (phrase-model-bigram-counts pm) 0)))
    ;; Trigrams
    (loop for i from 0 below (- n 2)
          for trigram = (list (nth i tokens) (nth (1+ i) tokens) (nth (+ i 2) tokens))
          do (incf (gethash trigram (phrase-model-trigram-counts pm) 0)))
    ;; 4-grams
    (loop for i from 0 below (- n 3)
          for fourgram = (list (nth i tokens) (nth (1+ i) tokens) 
                               (nth (+ i 2) tokens) (nth (+ i 3) tokens))
          do (incf (gethash fourgram (phrase-model-fourgram-counts pm) 0)))))

;;; ============================================================================
;;; PHRASE DETECTION: PMI COMPUTATION
;;; ============================================================================

(defun get-ngram-count (tokens)
  "Get count for any n-gram"
  (unless *phrase-model*
    (return-from get-ngram-count 0))
  (let ((pm *phrase-model*))
    (case (length tokens)
      (1 (gethash (first tokens) (phrase-model-unigram-counts pm) 0))
      (2 (gethash tokens (phrase-model-bigram-counts pm) 0))
      (3 (gethash tokens (phrase-model-trigram-counts pm) 0))
      (4 (gethash tokens (phrase-model-fourgram-counts pm) 0))
      (t 0))))

(defun compute-pmi (tokens)
  "Compute Pointwise Mutual Information for a token sequence.
   PMI = log2(P(phrase) / product(P(each token)))
   High PMI = tokens co-occur much more than expected by chance"
  (unless *phrase-model*
    (return-from compute-pmi 0.0))
  (let* ((pm *phrase-model*)
         (total (max 1.0 (float (phrase-model-total-tokens pm))))
         (phrase-count (get-ngram-count tokens)))
    (when (<= phrase-count 0)
      (return-from compute-pmi 0.0))
    ;; P(phrase) 
    (let ((p-phrase (/ phrase-count total))
          (p-product 1.0))
      ;; Product of individual token probabilities
      (dolist (tok tokens)
        (let ((tok-count (gethash tok (phrase-model-unigram-counts pm) 1)))
          (setf p-product (* p-product (/ tok-count total)))))
      ;; PMI = log2(P(phrase) / P(t1)*P(t2)*...)
      (if (> p-product 0)
          (log (/ p-phrase p-product) 2)
          0.0))))

(defun compute-phrase-score (tokens)
  "Combined score for phrase quality: PMI + frequency bonus"
  (let ((pmi (compute-pmi tokens))
        (freq (get-ngram-count tokens)))
    ;; PMI weighted by log frequency
    (if (>= freq *phrase-min-frequency*)
        (* pmi (1+ (log freq 2)))
        0.0)))

;;; ============================================================================
;;; PHRASE LEARNING AND LOOKUP
;;; ============================================================================

(defstruct phrase-info
  "Metadata for a learned atomic phrase unit."
  (tokens nil :type list)
  (symbol nil :type symbol)
  (frequency 0 :type fixnum)
  (pmi 0.0 :type single-float)
  (created-at 0 :type fixnum)
  (last-seen 0 :type fixnum))

(defun create-phrase-model ()
  "Creates and initializes a new phrase model."
  (make-phrase-model :unigram-counts (make-hash-table :test 'eq)
                     :bigram-counts (make-hash-table :test 'equal)
                     :trigram-counts (make-hash-table :test 'equal)
                     :fourgram-counts (make-hash-table :test 'equal)
                     :total-tokens 0
                     :phrases (make-hash-table :test 'equal)
                     :phrase-index (make-hash-table :test 'eq)
                     :last-detection-step 0
                     :phrases-learned 0))

(defun make-phrase-symbol (tokens)
  "Create a unified symbol for a phrase"
  (intern (format nil "~{~A~^_~}" tokens)))

(defun learn-phrase! (tokens)
  "Register a new phrase in the model"
  (unless *phrase-model*
    (return-from learn-phrase! nil))
  (let* ((pm *phrase-model*)
         (existing (gethash tokens (phrase-model-phrases pm))))
    (if existing
        ;; Update existing phrase
        (progn
          (incf (phrase-info-frequency existing))
          (setf (phrase-info-last-seen existing) *step*)
          existing)
        ;; Create new phrase
        (let ((info (make-phrase-info
                     :tokens tokens
                     :symbol (make-phrase-symbol tokens)
                     :frequency (get-ngram-count tokens)
                     :pmi (compute-pmi tokens)
                     :created-at *step*
                     :last-seen *step*)))
          (setf (gethash tokens (phrase-model-phrases pm)) info)
          ;; Add to reverse index (by first token)
          (push tokens (gethash (first tokens) (phrase-model-phrase-index pm)))
          (incf (phrase-model-phrases-learned pm))
          info))))

(defun detect-phrases! ()
  "Scan n-gram counts and learn high-PMI phrases"
  (unless *phrase-model*
    (return-from detect-phrases! 0))
  (let ((pm *phrase-model*)
        (learned 0))
    ;; Only run periodically
    (when (< (- *step* (phrase-model-last-detection-step pm)) 
             *phrase-detection-interval*)
      (return-from detect-phrases! 0))
    (setf (phrase-model-last-detection-step pm) *step*)
    
    ;; Check bigrams
    (maphash (lambda (tokens count)
               (when (and (>= count *phrase-min-frequency*)
                          (not (gethash tokens (phrase-model-phrases pm))))
                 (let ((pmi (compute-pmi tokens)))
                   (when (> pmi *phrase-pmi-threshold*)
                     (learn-phrase! tokens)
                     (incf learned)))))
             (phrase-model-bigram-counts pm))
    
    ;; Check trigrams
    (maphash (lambda (tokens count)
               (when (and (>= count *phrase-min-frequency*)
                          (not (gethash tokens (phrase-model-phrases pm))))
                 (let ((pmi (compute-pmi tokens)))
                   (when (> pmi *phrase-pmi-threshold*)
                     (learn-phrase! tokens)
                     (incf learned)))))
             (phrase-model-trigram-counts pm))
    
    ;; Check 4-grams (higher threshold)
    (maphash (lambda (tokens count)
               (when (and (>= count (1+ *phrase-min-frequency*))
                          (not (gethash tokens (phrase-model-phrases pm))))
                 (let ((pmi (compute-pmi tokens)))
                   (when (> pmi (* 1.2 *phrase-pmi-threshold*))
                     (learn-phrase! tokens)
                     (incf learned)))))
             (phrase-model-fourgram-counts pm))
    
    learned))

(defun find-phrase-at-position (tokens start)
  "Check if a known phrase starts at position START in TOKENS.
   Returns (phrase-info . length) or nil"
  (unless *phrase-model*
    (return-from find-phrase-at-position nil))
  (let* ((pm *phrase-model*)
         (first-tok (nth start tokens))
         (candidates (gethash first-tok (phrase-model-phrase-index pm))))
    (when candidates
      ;; Check longest phrases first
      (let ((sorted (sort (copy-list candidates) #'> :key #'length)))
        (dolist (phrase-tokens sorted)
          (let ((len (length phrase-tokens)))
            (when (<= (+ start len) (length tokens))
              ;; Check if tokens match
              (let ((match t))
                (loop for i from 0 below len
                      for pt in phrase-tokens
                      for tt = (nth (+ start i) tokens)
                      unless (eq pt tt) do (setf match nil) (return))
                (when match
                  (let ((info (gethash phrase-tokens (phrase-model-phrases pm))))
                    (when info
                      (return-from find-phrase-at-position 
                        (cons info len)))))))))))))

(defun tokenize-with-phrases (tokens)
  "Convert token sequence, replacing known phrases with phrase symbols.
   Returns list mixing regular tokens and phrase symbols."
  (unless *phrase-model*
    (return-from tokenize-with-phrases tokens))
  (let ((result nil)
        (i 0)
        (n (length tokens)))
    (loop while (< i n) do
      (let ((phrase-match (find-phrase-at-position tokens i)))
        (if phrase-match
            (progn
              (push (phrase-info-symbol (car phrase-match)) result)
              (incf i (cdr phrase-match)))
            (progn
              (push (nth i tokens) result)
              (incf i)))))
    (nreverse result)))

;;; ============================================================================
;;; VARIABLE-LENGTH CONTEXT
;;; ============================================================================

(defun context-signature (ctx)
  "Create a signature for context pattern (first 2 tokens)"
  (when (>= (length ctx) 2)
    (list (first ctx) (second ctx))))

(defun get-optimal-scale (ctx)
  "Get the learned optimal scale for this context pattern"
  (unless *context-scale-model*
    (return-from get-optimal-scale 3))  ; Default
  (let* ((csm *context-scale-model*)
         (sig (context-signature ctx))
         (optimal (when sig (gethash sig (context-scale-model-optimal-scales csm)))))
    (or optimal 3)))

(defun record-scale-outcome! (ctx scale correct)
  "Record whether a particular scale worked for this context"
  (unless *context-scale-model*
    (setf *context-scale-model* (create-context-scale-model)))
  (let* ((csm *context-scale-model*)
         (sig (context-signature ctx))
         (key (when sig (cons sig scale))))
    ;; Update per-context stats
    (when key
      (let ((stats (gethash key (context-scale-model-scale-stats csm))))
        (if stats
            (progn
              (incf (cdr stats))
              (when correct (incf (car stats))))
            (setf (gethash key (context-scale-model-scale-stats csm))
                  (cons (if correct 1 0) 1)))))
    ;; Update global stats
    (when (< scale 10)
      (incf (aref (context-scale-model-global-scale-total csm) scale))
      (when correct
        (incf (aref (context-scale-model-global-scale-hits csm) scale))))))

(defun update-optimal-scale! (ctx)
  "Recompute optimal scale for a context based on accumulated stats"
  (unless *context-scale-model*
    (return-from update-optimal-scale! nil))
  (let* ((csm *context-scale-model*)
         (sig (context-signature ctx)))
    (when sig
      (let ((best-scale 3)
            (best-rate 0.0))
        ;; Find scale with best accuracy for this context signature
        (loop for scale from *context-min-scale* to (min *context-max-scale* 8)
              for key = (cons sig scale)
              for stats = (gethash key (context-scale-model-scale-stats csm))
              when (and stats (> (cdr stats) 2))  ; Need minimum data
              do (let ((rate (/ (car stats) (cdr stats))))
                   (when (> rate best-rate)
                     (setf best-rate rate)
                     (setf best-scale scale))))
        (setf (gethash sig (context-scale-model-optimal-scales csm)) best-scale)))))

(defun select-context-scale (ctx)
  "Select context scale: use optimal with probability, else explore"
  (let ((optimal (get-optimal-scale ctx)))
    (if (< (random 1.0) *context-scale-exploration*)
        ;; Explore: try random scale
        (+ *context-min-scale* 
           (random (1+ (- *context-max-scale* *context-min-scale*))))
        ;; Exploit: use learned optimal
        optimal)))

(defun make-variable-context (full-ctx)
  "Create context of adaptive length based on learned optimal scale"
  (let* ((scale (select-context-scale full-ctx))
         (actual-scale (min scale (length full-ctx))))
    (values (subseq full-ctx 0 actual-scale) scale)))

;;; ============================================================================
;;; SENTENCE BOUNDARY HANDLING
;;; ============================================================================

(defun sentence-end-p (token)
  "Check if token marks end of sentence"
  (member token *sentence-end-tokens*))

(defun clause-boundary-p (token)
  "Check if token marks a clause boundary"
  (member token *clause-boundary-tokens*))

(defun update-sentence-model! (token)
  "Update sentence tracking with new token"
  (unless *sentence-model*
    (setf *sentence-model* (create-sentence-model)))
  (let ((sm *sentence-model*))
    (cond
      ;; Sentence end
      ((sentence-end-p token)
       (let ((len (1+ (sentence-model-sentence-position sm))))
         ;; Update average length
         (setf (sentence-model-avg-sentence-length sm)
               (+ (* 0.95 (sentence-model-avg-sentence-length sm))
                  (* 0.05 len)))
         ;; Record sentence end token
         (setf (sentence-model-prev-sentence-end-token sm) token)
         ;; Summarize sentence (keep last few tokens as context)
         (setf (sentence-model-prev-sentence-summary sm)
               (subseq (sentence-model-current-tokens sm) 
                       0 (min 3 (length (sentence-model-current-tokens sm)))))
         ;; Reset for new sentence
         (setf (sentence-model-current-tokens sm) nil)
         (setf (sentence-model-sentence-position sm) 0)
         (incf (sentence-model-total-sentences sm))))
      
      ;; New sentence starting
      ((and (zerop (sentence-model-sentence-position sm))
            (not (sentence-end-p token))
            (not (clause-boundary-p token)))
       (incf (gethash token (sentence-model-sentence-starts sm) 0))
       (push token (sentence-model-current-tokens sm))
       (incf (sentence-model-sentence-position sm)))
      
      ;; Continue sentence
      (t
       (push token (sentence-model-current-tokens sm))
       (incf (sentence-model-sentence-position sm))))))

(defun get-sentence-context-weight ()
  "Get weight for current position in sentence.
   Higher weight at start (more uncertainty), lower in middle."
  (unless *sentence-model*
    (return-from get-sentence-context-weight 1.0))
  (let* ((sm *sentence-model*)
         (pos (sentence-model-sentence-position sm))
         (avg-len (sentence-model-avg-sentence-length sm)))
    ;; U-shaped: high at start, dips in middle, rises near expected end
    (cond
      ((< pos 2) 1.2)                                    ; Sentence start
      ((> pos (* 0.8 avg-len)) 1.1)                      ; Near expected end
      (t 1.0))))                                         ; Middle

(defun apply-sentence-boundary-decay! (context-weights)
  "Apply decay to context weights at sentence/clause boundaries"
  (unless *sentence-model*
    (return-from apply-sentence-boundary-decay! context-weights))
  (let ((sm *sentence-model*))
    (when (and (sentence-model-prev-sentence-end-token sm)
               (< (sentence-model-sentence-position sm) 2))
      ;; Just started new sentence: decay influence of pre-boundary context
      (mapcar (lambda (w) (* w *sentence-boundary-context-decay*)) context-weights))))

;;; ============================================================================
;;; HIERARCHICAL PREDICTION
;;; ============================================================================

;;; Hierarchical votes moved to forward-decl.lisp

(defun collect-phrase-votes (context)
  "Get predictions treating context as potential phrase continuations"
  (let ((votes (make-hash-table :test 'eq)))
    (when *phrase-model*
      (let ((pm *phrase-model*))
        ;; Check if current context is start of known phrase
        (dolist (phrase-tokens (gethash (first context) (phrase-model-phrase-index pm)))
          (let ((phrase-len (length phrase-tokens)))
            ;; If context matches start of phrase, vote for next token in phrase
            (when (>= (length context) 1)
              (let ((match-len (loop for i from 0 below (min (length context) phrase-len)
                                     while (eq (nth i context) (nth i phrase-tokens))
                                     count t)))
                (when (and (> match-len 0) (< match-len phrase-len))
                  ;; Vote for next token in phrase
                  (let ((next-tok (nth match-len phrase-tokens))
                        (info (gethash phrase-tokens (phrase-model-phrases pm))))
                    (when (and next-tok info)
                      (incf (gethash next-tok votes 0.0)
                            (* (phrase-info-pmi info) 
                               (/ match-len phrase-len))))))))))))
    votes))

(defun collect-sentence-votes (context)
  "Get predictions based on sentence-level patterns"
  (let ((votes (make-hash-table :test 'eq)))
    (when *sentence-model*
      (let ((sm *sentence-model*))
        ;; At sentence start, prefer known sentence starters
        (when (< (sentence-model-sentence-position sm) 2)
          (maphash (lambda (tok count)
                     (when (> count 2)
                       (setf (gethash tok votes) 
                             (* 0.3 (log (1+ count) 2)))))
                   (sentence-model-sentence-starts sm)))))
    votes))

(defun merge-hierarchical-votes (token-votes phrase-votes sentence-votes)
  "Combine votes from different hierarchy levels"
  (let ((combined (make-hash-table :test 'eq))
        (weights *hierarchy-weights*))
    (let ((w-tok (or (cdr (assoc :token weights)) 0.5))
          (w-phr (or (cdr (assoc :phrase weights)) 0.3))
          (w-sen (or (cdr (assoc :sentence weights)) 0.2)))
      ;; Add token-level votes
      (maphash (lambda (tok v)
                 (incf (gethash tok combined 0.0) (* w-tok v)))
               token-votes)
      ;; Add phrase-level votes
      (maphash (lambda (tok v)
                 (incf (gethash tok combined 0.0) (* w-phr v)))
               phrase-votes)
      ;; Add sentence-level votes
      (maphash (lambda (tok v)
                 (incf (gethash tok combined 0.0) (* w-sen v)))
               sentence-votes))
    combined))

(defun collect-hierarchical-votes (context)
  "Collect and merge votes from all hierarchy levels"
  (let ((token-votes (make-hash-table :test 'eq))
        (phrase-votes (collect-phrase-votes context))
        (sentence-votes (collect-sentence-votes context)))
    ;; Get standard token-level votes via existing system
    (multiple-value-bind (base-votes sources) 
        (collect-vote-distribution context)
      (maphash (lambda (k v) (setf (gethash k token-votes) v)) base-votes))
    ;; Merge all levels
    (let ((combined (merge-hierarchical-votes token-votes phrase-votes sentence-votes)))
      (normalize-votes combined)
      combined)))

;;; ============================================================================
;;; INTEGRATED PROCESSING
;;; ============================================================================

(defun process-chunk-sequence! (text &key verbose carry-over)
  "Process text with sequence-level awareness.
   Includes phrase detection, variable context, and sentence boundaries."
  (let* ((raw-tokens (if carry-over 
                         (append carry-over (tokenize text))
                         (tokenize text)))
         ;; Apply phrase detection to get mixed token/phrase sequence
         (tokens (if *phrase-model*
                     (tokenize-with-phrases raw-tokens)
                     raw-tokens))
         (context nil)
         (correct 0) (total 0))
    
    ;; Update n-gram counts for phrase detection
    (update-ngram-counts! raw-tokens)
    
    ;; Process each token
    (loop for tok in tokens
          for raw-tok in raw-tokens
          do (incf *step*)
             ;; Update sentence tracking
             (update-sentence-model! raw-tok)
             
             (when (>= (length context) 1)
               (incf total)
               (setf *call-stack* nil)
               
               ;; Select variable context length
               (multiple-value-bind (var-ctx scale) 
                   (make-variable-context context)
                 
                 ;; Get hierarchical prediction
                 (let* ((hier-votes (collect-hierarchical-votes var-ctx))
                        (predicted (greedy-from-distribution hier-votes))
                        (confidence (or (gethash predicted hier-votes) 0.0))
                        (got-it (eq predicted tok)))
                   
                   ;; Record scale outcome for learning
                   (record-scale-outcome! context scale got-it)
                   
                   (when got-it (incf correct))
                   
                   ;; Learn from outcome
                   (let ((ctx-key (make-context-key var-ctx (length var-ctx))))
                     (learn! var-ctx tok predicted t))
                   
                   (when verbose
                     (format t "~4D: ~12A [s~A] -> ~8A/~8A ~A conf:~,2F~%"
                             *step* 
                             (format nil "(~{~A~^ ~})" 
                                     (reverse (subseq var-ctx 0 (min 2 (length var-ctx)))))
                             scale
                             (or predicted '_) tok
                             (if got-it "[OK]" "[X]")
                             confidence)))))
             
             ;; Update context
             (push tok context)
             (when (> (length context) *context-max-scale*)
               (setf context (subseq context 0 *context-max-scale*)))
             
             ;; Periodically update optimal scales
             (when (zerop (mod *step* 50))
               (update-optimal-scale! context)))
    
    ;; Run maintenance
    (detect-phrases!)
    (run-consolidation!)
    (detect-and-create-travelers!)
    (recluster-types!)
    (run-compression!)
    (lifecycle-step!)
    
    (values (if (> total 0) (float (/ correct total)) 0.0)
            total
            (subseq context 0 (min *context-max-scale* (length context))))))

(defun process-text-sequence! (text &key (verbose t))
  "Process text string with sequence-level operations"
  (multiple-value-bind (acc total carry)
      (process-chunk-sequence! text :verbose verbose)
    (format t "~%Sequence Accuracy: ~,1F% (~A/~A)~%"
            (* 100 acc) (round (* acc total)) total)
    acc))

(defun process-file-sequence! (path &key (chunk-size 5000) (verbose nil))
  "Process file with sequence-level operations"
  (with-open-file (s path :direction :input)
    (let ((total-acc 0.0) (n-chunks 0) (carry nil))
      (loop for chunk = (make-string chunk-size)
            for pos = (read-sequence chunk s)
            while (> pos 0)
            do (let ((text (subseq chunk 0 pos)))
                 (multiple-value-bind (acc total new-carry)
                     (process-chunk-sequence! text :verbose verbose :carry-over carry)
                   (incf total-acc acc)
                   (incf n-chunks)
                   (setf carry new-carry))))
      (when (> n-chunks 0)
        (format t "=== Complete === Chunks: ~A, Avg Acc: ~,1F%~%"
                n-chunks (* 100 (/ total-acc n-chunks))))
      (/ total-acc (max 1 n-chunks)))))

;;; ============================================================================
;;; DIAGNOSTICS
;;; ============================================================================

(defun print-phrase-model ()
  "Print phrase detection statistics"
  (format t "~%========== Phrase Model ==========~%")
  (if *phrase-model*
      (let ((pm *phrase-model*))
        (format t "Total tokens processed: ~A~%" (phrase-model-total-tokens pm))
        (format t "Unique unigrams: ~A~%" 
                (hash-table-count (phrase-model-unigram-counts pm)))
        (format t "Unique bigrams: ~A~%" 
                (hash-table-count (phrase-model-bigram-counts pm)))
        (format t "Unique trigrams: ~A~%" 
                (hash-table-count (phrase-model-trigram-counts pm)))
        (format t "Phrases learned: ~A~%" (phrase-model-phrases-learned pm))
        (format t "~%Top phrases by PMI:~%")
        (let ((phrases nil))
          (maphash (lambda (k v) 
                     (push (cons k v) phrases))
                   (phrase-model-phrases pm))
          (setf phrases (sort phrases #'> :key (lambda (x) (phrase-info-pmi (cdr x)))))
          (loop for (tokens . info) in (subseq phrases 0 (min 15 (length phrases)))
                do (format t "  ~{~A~^ ~}: PMI=~,2F freq=~A~%"
                           tokens 
                           (phrase-info-pmi info)
                           (phrase-info-frequency info)))))
      (format t "No phrase model initialized.~%"))
  (format t "==================================~%"))

(defun print-context-scale-model ()
  "Print variable context statistics"
  (format t "~%========== Context Scale Model ==========~%")
  (if *context-scale-model*
      (let ((csm *context-scale-model*))
        (format t "Optimal scales learned: ~A~%"
                (hash-table-count (context-scale-model-optimal-scales csm)))
        (format t "~%Global scale performance:~%")
        (loop for scale from 1 to 8
              for total = (aref (context-scale-model-global-scale-total csm) scale)
              for hits = (aref (context-scale-model-global-scale-hits csm) scale)
              when (> total 0)
              do (format t "  Scale ~A: ~,1F% (~A/~A)~%"
                         scale (* 100 (/ hits total)) hits total))
        (format t "~%Sample optimal scales:~%")
        (let ((count 0))
          (maphash (lambda (sig scale)
                     (when (< count 10)
                       (format t "  ~A -> scale ~A~%" sig scale)
                       (incf count)))
                   (context-scale-model-optimal-scales csm))))
      (format t "No context scale model initialized.~%"))
  (format t "=========================================~%"))

(defun print-sentence-model ()
  "Print sentence-level statistics"
  (format t "~%========== Sentence Model ==========~%")
  (if *sentence-model*
      (let ((sm *sentence-model*))
        (format t "Total sentences: ~A~%" (sentence-model-total-sentences sm))
        (format t "Average sentence length: ~,1F tokens~%" 
                (sentence-model-avg-sentence-length sm))
        (format t "Current position: ~A~%" (sentence-model-sentence-position sm))
        (format t "~%Top sentence starters:~%")
        (let ((starters nil))
          (maphash (lambda (k v) (push (cons k v) starters))
                   (sentence-model-sentence-starts sm))
          (setf starters (sort starters #'> :key #'cdr))
          (loop for (tok . count) in (subseq starters 0 (min 10 (length starters)))
                do (format t "  ~A: ~A~%" tok count))))
      (format t "No sentence model initialized.~%"))
  (format t "=====================================~%"))

(defun print-sequence-status ()
  "Print all sequence-level model status"
  (print-phrase-model)
  (print-context-scale-model)
  (print-sentence-model))

;;; ============================================================================
;;; DEMO
;;; ============================================================================

(defun demo-sequence ()
  "Demonstrate sequence-level operations"
  (format t "~%================================================================~%")
  (format t "UHMA v6.1 - PRIORITY 14: SEQUENCE-LEVEL OPERATIONS~%")
  (format t "================================================================~%")
  (format t "Features: Phrase Detection, Variable Context, Sentence Awareness~%")
  (format t "================================================================~%")
  
  ;; Reset everything
  (reset!)
  (reset-sequence-models!)
  
  (format t "~%--- Phase 1: Initial Training ---~%")
  (let ((corpus "The cat sat on the mat. The cat was happy.
The dog ran in the park. The dog was fast.
The bird flew over the tree. The bird was small.
New York is a big city. New York never sleeps.
The cat sat on the mat again. The dog ran again.
In New York the cat sat on the mat.
The bird flew over New York."))
    (dotimes (i 5)
      (format t "Epoch ~A...~%" (1+ i))
      (process-chunk-sequence! corpus :verbose nil)))
  
  (format t "~%--- Phase 2: Phrase Detection Check ---~%")
  (detect-phrases!)
  (print-phrase-model)
  
  (format t "~%--- Phase 3: Process with Verbose Output ---~%")
  (process-text-sequence! "The cat sat on the mat" :verbose t)
  
  (format t "~%--- Phase 4: Variable Context Analysis ---~%")
  (print-context-scale-model)
  
  (format t "~%--- Phase 5: Sentence Model Analysis ---~%")
  (print-sentence-model)
  
  (format t "~%--- Phase 6: Generation Test ---~%")
  (format t "Greedy 'the cat': ~A~%" (generate "the cat" :length 8))
  (format t "Greedy 'New York': ~A~%" (generate "New York" :length 8))
  (format t "Greedy 'The': ~A~%" (generate "The" :length 10))
  
  (format t "~%--- Final Status ---~%")
  (print-status)
  
  (format t "~%Demo complete.~%"))

;;; ============================================================================
;;; HOOK INTEGRATION - Connect sequence models to core processing
;;; ============================================================================

(defun sequence-post-token-hook (tok ctx predicted got-it)
  "Hook to update sequence models during core processing."
  (declare (ignore predicted got-it))
  ;; Lazy init phrase model
  (unless *phrase-model*
    (setf *phrase-model* (create-phrase-model)))
  ;; Lazy init sentence model  
  (unless *sentence-model*
    (setf *sentence-model* (create-sentence-model)))
  ;; Update phrase model n-gram counts from context window
  ;; This accumulates data so phrase detection can work
  (let ((pm *phrase-model*))
    ;; Count this token
    (incf (gethash tok (phrase-model-unigram-counts pm) 0))
    (incf (phrase-model-total-tokens pm))
    ;; Count bigrams from context
    (when (and ctx (>= (length ctx) 1))
      (let ((bigram (list (first ctx) tok)))
        (incf (gethash bigram (phrase-model-bigram-counts pm) 0))))
    ;; Count trigrams from context
    (when (and ctx (>= (length ctx) 2))
      (let ((trigram (list (second ctx) (first ctx) tok)))
        (incf (gethash trigram (phrase-model-trigram-counts pm) 0)))))
  ;; Update sentence model with token
  (update-sentence-model! tok))

;; Register the hook for external input
(register-hook +hook-post-process-token+ 'sequence-post-token-hook)

;; Also register for +hook-post-learn+ so sequence models update during rehearsal/live mode
(defun sequence-post-learn-hook (ctx actual predicted correct learner)
  "Hook to update sequence models during learn! (including rehearsal)."
  (declare (ignore predicted learner))
  ;; Adapt arguments: actual is the token, ctx is context, correct is got-it
  (sequence-post-token-hook actual ctx nil correct))

(register-hook +hook-post-learn+ 'sequence-post-learn-hook :priority 50)

;;; ============================================================================
;;; EXPORTS
;;; ============================================================================

(export '(reset-sequence-models!
          process-text-sequence! process-file-sequence!
          detect-phrases! tokenize-with-phrases
          print-phrase-model print-context-scale-model print-sentence-model
          print-sequence-status
          demo-sequence))

(format t "~%Priority 14: Sequence-Level Operations loaded.~%")
(format t "Run (reset-sequence-models!) then (demo-sequence) to test.~%")
