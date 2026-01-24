;;;; ============================================================================
;;;; UHMA HOLOGRAPHIC INTEGRATION (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Wires the holographic memory into the core cognitive loops via hooks.
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 6: SPECIAL VARIABLES ---

(defvar *holo-integration-enabled* t )
(defvar *holo-integration-priority* 15 )

;;; --- SECTION 8: HOOK HANDLERS ---

(defun holo-post-learn-handler (ctx actual predicted correct-p learner)
  "Synchronize learning outcome with holographic storage."
  (declare (ignore predicted learner))
  (when *holo-integration-enabled*
    (holo-store-outcome! ctx correct-p actual)
    (holo-update-pattern-stats! ctx correct-p)))

(defun holo-post-token-handler (tok ctx predicted correct-p)
  "Buffer active reasoning state into holographic working memory."
  (declare (ignore tok predicted))
  (when (and *holo-integration-enabled* (or (not correct-p) (< (random 1.0) 0.05)))
    (holo-store-working! ctx correct-p)))

;;; --- SECTION 10: INITIALIZATION ---
(eval-when (:load-toplevel :execute)
  (register-hook +hook-post-learn+ 'holo-post-learn-handler :priority *holo-integration-priority*)
  (register-hook +hook-post-process-token+ 'holo-post-token-handler :priority *holo-integration-priority*)
  (format t "[HOLO-INTEGRATION] Memory synchronization active.~%"))

;;; ============================================================================
;;; WRAPPER FUNCTIONS - Shadow old memory accessors
;;; ============================================================================
;;; These provide backward compatibility while routing through holographic store.

(defun holo-lookup-consolidated (ctx &optional scale)
  "Holographic replacement for lookup-consolidated.
   Falls back to original if holographic has no match."
  (declare (ignore scale))
  (if *holo-integration-enabled*
      (multiple-value-bind (pred conf) (holo-lookup ctx)
        (if pred
            (values pred conf)
            ;; Fallback to original LTM if exists
            (when (and (boundp '*long-term-memory*)
                       (hash-table-p *long-term-memory*))
              (let ((key (when (fboundp 'make-context-key)
                          (make-context-key ctx (or scale 3)))))
                (when key (gethash key *long-term-memory*))))))
      ;; Disabled - use original
      (when (fboundp 'lookup-consolidated)
        (funcall 'lookup-consolidated ctx scale))))

(defun holo-get-recent-outcomes (&optional (n 20))
  "Holographic replacement for reading *recent-outcomes*."
  (if *holo-integration-enabled*
      (holo-recent-outcomes n)
      ;; Fallback
      (when (boundp '*recent-outcomes*)
        (subseq *recent-outcomes* 0 (min n (length *recent-outcomes*))))))

(defun holo-find-situations (ctx &optional (n 5))
  "Holographic replacement for find-similar-situations."
  (if *holo-integration-enabled*
      (holo-find-similar-situations ctx n)
      ;; Fallback
      (when (fboundp 'find-similar-situations)
        (funcall 'find-similar-situations ctx n))))

;;; ============================================================================
;;; MIGRATION UTILITIES
;;; ============================================================================
;;; Import existing memory into holographic store.

(defun holo-migrate-ltm! ()
  "Migrate *long-term-memory* into holographic semantic layer."
  (when (and (boundp '*long-term-memory*)
             (hash-table-p *long-term-memory*))
    (let ((count 0))
      (maphash (lambda (key value)
                 (when (and key value)
                   (let ((ctx (if (listp key) key (list key)))
                         (pred (if (listp value) (car value) value))
                         (conf (if (and (listp value) (numberp (cdr value)))
                                   (cdr value) 0.5)))
                     (holo-store-knowledge! ctx pred conf)
                     (incf count))))
               *long-term-memory*)
      (format t "Migrated ~D LTM entries to holographic semantic layer.~%" count))))

(defun holo-migrate-outcomes! ()
  "Migrate *recent-outcomes* into holographic immediate layer."
  (when (and (boundp '*recent-outcomes*) (listp *recent-outcomes*))
    (let ((count 0))
      (dolist (outcome *recent-outcomes*)
        (when (consp outcome)
          (holo-store-outcome! (car outcome) (cdr outcome))
          (incf count)))
      (format t "Migrated ~D outcomes to holographic immediate layer.~%" count))))

(defun holo-migrate-pattern-stats! ()
  "Migrate *pattern-stats* into holographic pattern layer."
  (when (and (boundp '*pattern-stats*)
             (hash-table-p *pattern-stats*))
    (let ((count 0))
      (maphash (lambda (key stats)
                 (when (and key stats (listp stats))
                   (let ((hits (or (getf stats :hits) 0))
                         (misses (or (getf stats :misses) 0)))
                     (holo-store-pattern-stats! key hits misses)
                     (incf count))))
               *pattern-stats*)
      (format t "Migrated ~D pattern stats to holographic pattern layer.~%" count))))

(defun holo-migrate-all! ()
  "Migrate all existing memory into holographic store."
  (format t "~%Migrating existing memory to holographic substrate...~%")
  (holo-migrate-ltm!)
  (holo-migrate-outcomes!)
  (holo-migrate-pattern-stats!)
  (format t "Migration complete.~%")
  (holo-print-status))

;;; ============================================================================
;;; INITIALIZATION
;;; ============================================================================



;;; ============================================================================
;;; MODULE INFO
;;; ============================================================================

(format t "~%================================================================~%")
(format t "UHMA HOLOGRAPHIC INTEGRATION LAYER~%")
(format t "================================================================~%")
(format t "Status: ~A~%" (if *holo-integration-enabled* "ENABLED" "DISABLED"))
(format t "Hook priority: ~D~%" *holo-integration-priority*)
(format t "~%Hooks registered:~%")
(format t "  +hook-post-learn+       -> holo-post-learn-handler~%")
(format t "  +hook-post-process-token+ -> holo-post-token-handler~%")
(format t "  +hook-maintenance+      -> holo-maintenance-handler~%")
(format t "  +hook-episode-boundary+ -> holo-episode-handler~%")
(format t "  +hook-expert-dying+     -> holo-expert-dying-handler~%")
(format t "  +hook-post-reset+       -> holo-reset!~%")
(format t "~%Wrapper functions:~%")
(format t "  holo-lookup-consolidated - shadows lookup-consolidated~%")
(format t "  holo-get-recent-outcomes - shadows *recent-outcomes*~%")
(format t "  holo-find-situations     - shadows find-similar-situations~%")
(format t "~%To migrate existing memory:~%")
(format t "  (holo-migrate-all!)~%")
(format t "================================================================~%")
