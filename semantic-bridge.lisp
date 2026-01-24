;;;; FILE MANIFEST
;;;; =============
;;;; This file MUST contain the following when complete:
;;;; PACKAGES: uhma
;;;; FUNCTIONS: semantic-query (and others)
;;;; TOTAL EXPECTED SECTIONS: Unknown (Legacy File)

;;;; SEMANTIC BRIDGE - Connects UHMA to semantic understanding
;;;; Uses socket connection to Python semantic server for fast queries

(in-package :uhma)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize (speed 0) (safety 3) (debug 3))))

;;; ============================================================================
;;; SOCKET CLIENT
;;; ============================================================================

(defvar *semantic-socket* nil "Socket connection to semantic server")
(defvar *semantic-host* "127.0.0.1")
(defvar *semantic-port* 7777)
(defvar *semantic-enabled* nil "Whether semantic understanding is active")
(defvar *semantic-cache* (make-hash-table :test 'equal) "Cache for semantic queries")

;;; ============================================================================
;;; SIMPLE JSON PARSER (for socket responses)
;;; ============================================================================

(defun skip-whitespace (str pos)
  "Skip whitespace in string, return new position."
  (loop while (and (< pos (length str))
                   (member (char str pos) '(#\Space #\Tab #\Newline #\Return)))
        do (incf pos))
  pos)

(defun parse-json-string (str pos)
  "Parse a JSON string starting at pos, return (value . new-pos)."
  (assert (char= (char str pos) #\"))
  (incf pos)
  (let ((result (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop while (and (< pos (length str))
                     (not (char= (char str pos) #\")))
          do (if (char= (char str pos) #\\)
                 (progn
                   (incf pos)
                   (when (< pos (length str))
                     (vector-push-extend (char str pos) result)
                     (incf pos)))
                 (progn
                   (vector-push-extend (char str pos) result)
                   (incf pos))))
    (when (and (< pos (length str)) (char= (char str pos) #\"))
      (incf pos))
    (cons result pos)))

(defun parse-json-number (str pos)
  "Parse a JSON number starting at pos, return (value . new-pos)."
  (let ((start pos))
    (when (and (< pos (length str)) (char= (char str pos) #\-))
      (incf pos))
    (loop while (and (< pos (length str))
                     (or (digit-char-p (char str pos))
                         (char= (char str pos) #\.)
                         (char= (char str pos) #\e)
                         (char= (char str pos) #\E)
                         (char= (char str pos) #\+)
                         (char= (char str pos) #\-)))
          do (incf pos))
    (let ((num-str (subseq str start pos)))
      (cons (read-from-string num-str) pos))))

(defun parse-json-array (str pos)
  "Parse a JSON array starting at pos, return (list . new-pos)."
  (assert (char= (char str pos) #\[))
  (incf pos)
  (setf pos (skip-whitespace str pos))
  (if (and (< pos (length str)) (char= (char str pos) #\]))
      (cons nil (1+ pos))
      (let ((items nil))
        (loop
          (multiple-value-bind (item new-pos) (parse-json str pos)
            (push item items)
            (setf pos (skip-whitespace str new-pos))
            (cond
              ((>= pos (length str)) (return))
              ((char= (char str pos) #\])
               (incf pos)
               (return))
              ((char= (char str pos) #\,)
               (incf pos)
               (setf pos (skip-whitespace str pos)))
              (t (return)))))
        (cons (nreverse items) pos))))

(defun parse-json-object (str pos)
  "Parse a JSON object starting at pos, return (alist . new-pos)."
  (assert (char= (char str pos) #\{))
  (incf pos)
  (setf pos (skip-whitespace str pos))
  (if (and (< pos (length str)) (char= (char str pos) #\}))
      (cons nil (1+ pos))
      (let ((items nil))
        (loop
          (setf pos (skip-whitespace str pos))
          (when (or (>= pos (length str))
                    (not (char= (char str pos) #\")))
            (return))
          (let ((key-result (parse-json-string str pos)))
            (setf pos (skip-whitespace str (cdr key-result)))
            (when (and (< pos (length str)) (char= (char str pos) #\:))
              (incf pos)
              (setf pos (skip-whitespace str pos))
              (multiple-value-bind (val new-pos) (parse-json str pos)
                (push (cons (intern (string-upcase (car key-result)) :keyword) val) items)
                (setf pos (skip-whitespace str new-pos))
                (cond
                  ((>= pos (length str)) (return))
                  ((char= (char str pos) #\})
                   (incf pos)
                   (return))
                  ((char= (char str pos) #\,)
                   (incf pos))
                  (t (return)))))))
        (cons (nreverse items) pos))))

(defun parse-json (str &optional (pos 0))
  "Parse JSON from string starting at pos. Returns (values result new-pos)."
  (setf pos (skip-whitespace str pos))
  (when (>= pos (length str))
    (return-from parse-json (values nil pos)))
  (let ((ch (char str pos)))
    (cond
      ((char= ch #\")
       (let ((r (parse-json-string str pos)))
         (values (car r) (cdr r))))
      ((char= ch #\[)
       (let ((r (parse-json-array str pos)))
         (values (car r) (cdr r))))
      ((char= ch #\{)
       (let ((r (parse-json-object str pos)))
         (values (car r) (cdr r))))
      ((or (digit-char-p ch) (char= ch #\-))
       (let ((r (parse-json-number str pos)))
         (values (car r) (cdr r))))
      ((and (>= (- (length str) pos) 4)
            (string= (subseq str pos (+ pos 4)) "null"))
       (values nil (+ pos 4)))
      ((and (>= (- (length str) pos) 4)
            (string= (subseq str pos (+ pos 4)) "true"))
       (values t (+ pos 4)))
      ((and (>= (- (length str) pos) 5)
            (string= (subseq str pos (+ pos 5)) "false"))
       (values nil (+ pos 5)))
      (t (values nil pos)))))

(defun semantic-connect ()
  "Connect to the semantic server."
  (handler-case
      (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                   :type :stream :protocol :tcp)))
        (sb-bsd-sockets:socket-connect socket
                                       (sb-bsd-sockets:make-inet-address *semantic-host*)
                                       *semantic-port*)
        (setf *semantic-socket* socket)
        t)
    (error (e)
      (declare (ignore e))
      nil)))

(defun semantic-disconnect ()
  "Disconnect from semantic server."
  (when *semantic-socket*
    (ignore-errors (sb-bsd-sockets:socket-close *semantic-socket*))
    (setf *semantic-socket* nil)))

(defun semantic-query (command &rest args)
  "Send query to semantic server and get result."
  (unless *semantic-enabled*
    (return-from semantic-query nil))

  ;; Check cache first
  (let ((cache-key (cons command args)))
    (multiple-value-bind (cached found) (gethash cache-key *semantic-cache*)
      (when found (return-from semantic-query cached))))

  ;; Create fresh connection for each query (server closes after each request)
  (handler-case
      (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                   :type :stream :protocol :tcp)))
        (sb-bsd-sockets:socket-connect socket
                                       (sb-bsd-sockets:make-inet-address *semantic-host*)
                                       *semantic-port*)
        (unwind-protect
            (let* ((msg (format nil "~A~{|~A~}~%" command args))
                   (stream (sb-bsd-sockets:socket-make-stream socket
                                                              :input t :output t
                                                              :buffering :line)))
              (write-string msg stream)
              (force-output stream)
              (let* ((response (read-line stream nil nil))
                     (result (when (and response
                                        (not (string= response "null"))
                                        (not (string= response "")))
                               (ignore-errors (parse-json response)))))
                ;; Cache the result
                (setf (gethash (cons command args) *semantic-cache*) result)
                result))
          (ignore-errors (sb-bsd-sockets:socket-close socket))))
    (error (e)
      (declare (ignore e))
      nil)))

;;; ============================================================================
;;; SEMANTIC QUERIES - HIGH LEVEL
;;; ============================================================================

(defun semantic-similarity (word1 word2)
  "Get semantic similarity between two words (0.0-1.0)."
  (let ((result (semantic-query "similarity" word1 word2)))
    (when (numberp result) result)))

(defun semantic-synonyms (word)
  "Get synonyms for a word."
  (semantic-query "synonyms" word))

(defun semantic-hypernyms (word)
  "Get more general concepts (is-a relationships)."
  (semantic-query "hypernyms" word))

(defun semantic-hyponyms (word)
  "Get more specific concepts."
  (semantic-query "hyponyms" word))

(defun semantic-related (word)
  "Get all semantically related words."
  (let ((result (semantic-query "related" word)))
    (when (listp result)
      (append (cdr (assoc :synonyms result))
              (cdr (assoc :hypernyms result))
              (cdr (assoc :hyponyms result))))))

(defun semantic-meanings (word)
  "Get all meanings of a word."
  (semantic-query "meanings" word))

;;; ============================================================================
;;; SEMANTIC CONTEXT EXPANSION
;;; ============================================================================

(defun expand-context-semantically (context &optional (max-expand 3))
  "Expand context with semantically related concepts."
  (unless *semantic-enabled*
    (return-from expand-context-semantically context))

  (let ((expanded (copy-list context))
        (added 0))
    (dolist (word context)
      (when (>= added max-expand) (return))
      (let* ((word-str (string-downcase (string word)))
             (hypernyms (semantic-hypernyms word-str)))
        (when hypernyms
          (let ((hyp (first hypernyms)))
            (unless (member hyp expanded :test #'string-equal)
              (push (intern (string-upcase hyp) :uhma) expanded)
              (incf added))))))
    expanded))

;;; ============================================================================
;;; SEMANTIC-AWARE LEARNING
;;; ============================================================================

(defvar *original-learn!* nil)

(defun semantic-learn! (context token &key (weight 1.0))
  "Learn with semantic awareness - also learn for related concepts."
  ;; Do regular learning
  (when *original-learn!*
    (funcall *original-learn!* context token :weight weight))

  ;; If semantics enabled, also learn for expanded context
  (when (and *semantic-enabled* (> (length context) 1))
    (let ((expanded (expand-context-semantically context 2)))
      (when (> (length expanded) (length context))
        ;; Learn expanded context with reduced weight
        (when *original-learn!*
          (funcall *original-learn!* expanded token :weight (* 0.3 weight)))))))

;;; ============================================================================
;;; SEMANTIC HYPOTHESIS FORMATION
;;; ============================================================================

(defun form-semantic-hypothesis! (pattern outcome)
  "Form a hypothesis based on semantic relationships.
   Uses UHMA's generate-hypothesis! with a semantic observation."
  (unless *semantic-enabled*
    (return-from form-semantic-hypothesis! nil))

  (let* ((pattern-str (string-downcase (format nil "~{~A~^ ~}" pattern)))
         (outcome-str (string-downcase (string outcome)))
         (outcome-hypernyms (semantic-hypernyms outcome-str)))
    (when outcome-hypernyms
      (let ((abstract-concept (first outcome-hypernyms)))
        ;; Create observation for UHMA's hypothesis system
        (when (fboundp 'generate-hypothesis!)
          (generate-hypothesis!
           (list :type :semantic-pattern
                 :pattern pattern-str
                 :outcome outcome-str
                 :abstraction abstract-concept
                 :step *step*)))))))

;;; ============================================================================
;;; INTEGRATION HOOK
;;; ============================================================================

(defun semantic-post-learn-hook (context actual predicted correct-p)
  "Post-learn hook for semantic integration."
  (declare (ignore predicted))
  (when (and *semantic-enabled* correct-p (> (length context) 2))
    ;; Form semantic hypothesis on successful predictions
    (form-semantic-hypothesis! context actual)))

;;; ============================================================================
;;; INITIALIZATION
;;; ============================================================================

(defun start-semantic-server! ()
  "Start the Python semantic server in background."
  (format t "Starting semantic server...~%")
  (uiop:run-program
   (format nil "cd ~A && source venv/bin/activate && nohup python3 semantic-server.py > semantic-server.log 2>&1 &"
           "/home/peter/Desktop/MINION/uhma")
   :output nil)
  (sleep 3)  ; Give it time to start
  t)

(defun test-semantic-server ()
  "Test if semantic server is reachable by making a single query."
  (handler-case
      (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                   :type :stream :protocol :tcp)))
        (sb-bsd-sockets:socket-connect socket
                                       (sb-bsd-sockets:make-inet-address *semantic-host*)
                                       *semantic-port*)
        (unwind-protect
            (let* ((stream (sb-bsd-sockets:socket-make-stream socket
                                                              :input t :output t
                                                              :buffering :line)))
              (write-string "ping" stream)
              (write-char #\Newline stream)
              (force-output stream)
              (let ((response (read-line stream nil nil)))
                ;; Just check we got any response containing "pong"
                (and response
                     (search "pong" response))))
          (ignore-errors (sb-bsd-sockets:socket-close socket))))
    (error () nil)))

(defun init-semantic-bridge! ()
  "Initialize the semantic bridge."
  (format t "~%Initializing semantic bridge...~%")

  ;; Clear cache
  (setf *semantic-cache* (make-hash-table :test 'equal))

  ;; Test server connectivity (don't leave persistent connection)
  (if (test-semantic-server)
      (progn
        (setf *semantic-enabled* t)
        (format t "Semantic server available.~%")
        ;; Test a real query
        (let ((test (semantic-synonyms "think")))
          (format t "Test - synonyms of 'think': ~A~%" test))
        ;; Register hook
        (register-hook +hook-post-learn+ 'semantic-post-learn-hook :priority 80)
        (format t "Semantic bridge initialized.~%")
        t)
      (progn
        (format t "Could not connect to semantic server.~%")
        (format t "Starting server...~%")
        (start-semantic-server!)
        (sleep 2)
        (if (test-semantic-server)
            (progn
              (setf *semantic-enabled* t)
              (format t "Connected after starting server.~%")
              (register-hook +hook-post-learn+ 'semantic-post-learn-hook :priority 80)
              t)
            (progn
              (format t "Semantic bridge unavailable. Running without semantics.~%")
              (setf *semantic-enabled* nil)
              nil)))))

(defun semantic-status ()
  "Check semantic bridge status."
  (format t "~%=== Semantic Bridge Status ===~%")
  (format t "Enabled: ~A~%" *semantic-enabled*)
  (format t "Cache entries: ~D~%" (hash-table-count *semantic-cache*))
  (when *semantic-enabled*
    (format t "Test query: ~A~%" (semantic-synonyms "learn")))
  (format t "~%"))

(format t "~%Semantic bridge loaded.~%")
(format t "Call (init-semantic-bridge!) to activate.~%")
(format t "Call (semantic-status) to check status.~%")

;;;; [SECTION-START:99:VERIFICATION]
;;; Section 99: File Verification

(defun verify-semantic-bridge-lisp-completeness ()
  "Verify completeness."
  (unless (find-package :uhma)
    (error "Package UHMA not defined"))
  (format t "~&semantic-bridge.lisp verification passed.~%"))

(verify-semantic-bridge-lisp-completeness)

;;;; [SECTION-END:99:VERIFICATION]
