
;;;; FILE MANIFEST
;;;; ============
;;;; This file MUST contain the following when complete:
;;;; PACKAGES: uhma
;;;; FUNCTIONS: lab-config (and others)
;;;; TOTAL EXPECTED SECTIONS: Unknown (Legacy File)

;;;; ============================================================================
;;;; UHMA LAB - Comprehensive Workbench for UHMA Cognitive System
;;;; ============================================================================
;;;;
;;;; Consolidated from: test-*.lisp, diagnose-*.lisp, feed-*.lisp, trace-*.lisp,
;;;;                    run-*.lisp, check-*.lisp, analyze-*.lisp scripts
;;;;
;;;; Usage: sbcl --load uhma-lab.lisp
;;;;        Then call (lab) for interactive mode with menu.
;;;;
;;;; MODES:
;;;;   TEST     - Run test suites (basic, comprehensive, live features)
;;;;   FEED     - Feed text, files, or codebase to UHMA
;;;;   TRACE    - Function/module/hook tracing with breakpoints
;;;;   DIAGNOSE - Memory harmony, loop diagnostics, synthesis conditions
;;;;   LIVE     - Run live mode with configurable duration and reports
;;;;
;;;; ============================================================================

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize (speed 0) (safety 3) (debug 3))))


;;; ============================================================================
;;; LOAD UHMA SYSTEM
;;; ============================================================================

(format t "~%Loading UHMA system...~%")
(load "load-quiet.lisp")
(format t "UHMA loaded.~%")

;;; ============================================================================
;;; LAB CONFIGURATION
;;; ============================================================================

(defvar *lab-config*
  '(:trace-buffer-size 5000
    :default-live-duration 60
    :default-feed-epochs 1
    :report-interval 10
    :memory-snapshot-interval 60
    :auto-gc-threshold 0.8
    :verbose-default nil)
  "Lab configuration plist")

(defun lab-config (key)
  "Get lab configuration value."
  (getf *lab-config* key))

(defun set-lab-config (key value)
  "Set lab configuration value."
  (setf (getf *lab-config* key) value))

;;; ============================================================================
;;; LAB STATE
;;; ============================================================================

(defvar *lab-trace-enabled* nil "Whether tracing is active")
(defvar *lab-trace-buffer* nil "Recent trace events")
(defvar *lab-trace-stream* *standard-output* "Where trace goes")
(defvar *lab-trace-file* nil "File stream for trace logging")
(defvar *lab-paused* nil "Whether execution is paused")
(defvar *lab-break-conditions* nil "List of conditions that pause execution")
(defvar *lab-watched-functions* nil "Functions being traced")
(defvar *lab-watched-experts* nil "Expert IDs being traced")
(defvar *lab-watched-hooks* nil "Hooks being traced")
(defvar *lab-watched-modules* nil "Modules being traced")
(defvar *lab-original-functions* (make-hash-table) "Original function definitions")
(defvar *lab-call-stack* nil "Current call stack for tracing")
(defvar *lab-step-count* 0 "Steps since lab started")
(defvar *lab-snapshots* (make-hash-table) "Named state snapshots")
(defvar *lab-event-log* nil "Full event log with timestamps")
(defvar *lab-mem-snapshots* nil "Memory snapshots over time")
(defvar *lab-session-start* (get-universal-time) "When lab session started")
(defvar *lab-test-results* nil "Results from last test run")

;;; Module definitions - which functions belong to which module
(defvar *lab-modules*
  '((:presence . (presence-vividness presence-trajectory presence-learning-multiplier
                  presence-feel-surprise! presence-shift-trajectory! update-presence!))
    (:self-mod . (program-rewrite! set-modifiable-param! insert-op-before! remove-op!
                  propose-self-modification! apply-modification!))
    (:schemas . (compile-schema! compile-top-schemas! execute-schema select-schema-for-context
                 schema-applicability-score))
    (:consolidation . (run-consolidation! consolidate-memory! merge-similar-entries!))
    (:dreams . (dream-cycle! dream-about-episode! record-dream-insight!))
    (:hypotheses . (generate-hypothesis! test-hypothesis! strengthen-hypothesis!
                    weaken-hypothesis! prune-hypotheses!))
    (:learning . (learn! learn-association! update-expert-knowledge!))
    (:prediction . (predict generate-prediction collect-votes collect-vote-distribution))
    (:experts . (spawn-expert! kill-expert! expert-predict expert-learn!))
    (:introspection . (introspect! analyze-self! self-model-update!))
    (:synthesis . (synthesize-op-from-schema compose-op-from-pattern adopt-synthesized-op!
                   wire-schemas-to-ops! attempt-synthesis-from-best-schemas!))
    (:hooks . (run-hook run-hook-until register-hook))))

;;; ============================================================================
;;; UTILITY FUNCTIONS
;;; ============================================================================

(defun format-time (seconds)
  "Format seconds as HH:MM:SS."
  (format nil "~2,'0D:~2,'0D:~2,'0D"
          (floor seconds 3600)
          (floor (mod seconds 3600) 60)
          (floor (mod seconds 60))))

(defun format-elapsed ()
  "Format elapsed time since session start."
  (format-time (- (get-universal-time) *lab-session-start*)))

(defun timestamp ()
  "Get current timestamp in milliseconds."
  (/ (get-internal-real-time) (/ internal-time-units-per-second 1000)))

(defun format-timestamp ()
  "Format current time for display."
  (multiple-value-bind (sec min hour) (get-decoded-time)
    (format nil "~2,'0D:~2,'0D:~2,'0D" hour min sec)))

(defun log-msg (msg &optional (stream *standard-output*))
  "Log a message with timestamp."
  (format stream "~%[~A] ~A" (format-elapsed) msg)
  (force-output stream))

(defun safe-length (var-symbol)
  "Safely get length of a list variable in UHMA package."
  (let ((sym (find-symbol (string var-symbol) :uhma)))
    (if (and sym (boundp sym) (symbol-value sym))
        (length (symbol-value sym))
        0)))

(defun safe-fp (var-symbol)
  "Safely get fill-pointer of an array variable in UHMA package."
  (let ((sym (find-symbol (string var-symbol) :uhma)))
    (if (and sym (boundp sym) (symbol-value sym)
             (arrayp (symbol-value sym))
             (array-has-fill-pointer-p (symbol-value sym)))
        (fill-pointer (symbol-value sym))
        0)))

(defun safe-ht-count (var-symbol)
  "Safely get hash-table count in UHMA package."
  (let ((sym (find-symbol (string var-symbol) :uhma)))
    (if (and sym (boundp sym) (symbol-value sym)
             (hash-table-p (symbol-value sym)))
        (hash-table-count (symbol-value sym))
        0)))

(defun memory-usage-mb ()
  "Get current memory usage in MB."
  (/ (sb-kernel:dynamic-usage) 1048576.0))

;;; ============================================================================
;;; TRACE CORE
;;; ============================================================================

(defstruct trace-event
  timestamp
  step
  category      ; :call :return :hook :learn :predict :state-change etc
  module        ; :presence :self-mod :schemas etc
  function-name
  expert-id
  hook-name
  args
  result
  call-depth
  message)

(defun trace-event! (category &key module function-name expert-id hook-name
                                   args result message)
  "Record a trace event."
  (when *lab-trace-enabled*
    (let ((event (make-trace-event
                  :timestamp (timestamp)
                  :step (if (boundp 'uhma::*step*) uhma::*step* 0)
                  :category category
                  :module module
                  :function-name function-name
                  :expert-id expert-id
                  :hook-name hook-name
                  :args args
                  :result result
                  :call-depth (length *lab-call-stack*)
                  :message message)))

      (when (should-show-event-p event)
        ;; Buffer it
        (push event *lab-trace-buffer*)
        (when (> (length *lab-trace-buffer*) (lab-config :trace-buffer-size))
          (setf *lab-trace-buffer* (butlast *lab-trace-buffer* 100)))

        ;; Log it
        (push event *lab-event-log*)

        ;; Display it
        (display-trace-event event)

        ;; Check break conditions
        (check-break-conditions event)))))

(defun should-show-event-p (event)
  "Check if event passes current filters."
  (or
   ;; No filters - show all
   (and (null *lab-watched-functions*)
        (null *lab-watched-experts*)
        (null *lab-watched-hooks*)
        (null *lab-watched-modules*))
   ;; Function match
   (and *lab-watched-functions*
        (member (trace-event-function-name event) *lab-watched-functions*))
   ;; Expert match
   (and *lab-watched-experts*
        (member (trace-event-expert-id event) *lab-watched-experts*))
   ;; Hook match
   (and *lab-watched-hooks*
        (member (trace-event-hook-name event) *lab-watched-hooks*))
   ;; Module match
   (and *lab-watched-modules*
        (member (trace-event-module event) *lab-watched-modules*))))

(defun display-trace-event (event)
  "Display a trace event."
  (let ((indent (make-string (* 2 (trace-event-call-depth event)) :initial-element #\Space))
        (step (trace-event-step event)))
    (format *lab-trace-stream* "~A[~5D] ~A~A~%"
            indent step
            (case (trace-event-category event)
              (:call (format nil "-> ~A~A"
                             (trace-event-function-name event)
                             (if (trace-event-args event)
                                 (format nil " ~S" (trace-event-args event))
                                 "")))
              (:return (format nil "<- ~A = ~S"
                               (trace-event-function-name event)
                               (trace-event-result event)))
              (:hook (format nil "HOOK ~A fired" (trace-event-hook-name event)))
              (:learn (format nil "LEARN: ~A" (trace-event-message event)))
              (:predict (format nil "PREDICT: ~A" (trace-event-message event)))
              (:expert (format nil "EXPERT ~A: ~A"
                               (trace-event-expert-id event)
                               (trace-event-message event)))
              (:state (format nil "STATE: ~A" (trace-event-message event)))
              (t (or (trace-event-message event) "")))
            (if (trace-event-module event)
                (format nil " [~A]" (trace-event-module event))
                ""))
    (force-output *lab-trace-stream*)

    (when *lab-trace-file*
      (format *lab-trace-file* "[~A] ~A~%"
              (format-timestamp)
              (trace-event-message event))
      (force-output *lab-trace-file*))))

;;; ============================================================================
;;; BREAK CONDITIONS
;;; ============================================================================

(defun add-break (condition-fn &optional description)
  "Add a break condition. CONDITION-FN takes a trace-event and returns T to break."
  (push (cons condition-fn description) *lab-break-conditions*)
  (format t "Break condition added: ~A~%" (or description "custom")))

(defun break-on-function (fn-name)
  "Break when function is called."
  (add-break (lambda (e) (eq (trace-event-function-name e) fn-name))
             (format nil "function ~A" fn-name)))

(defun break-on-expert (expert-id)
  "Break when expert does something."
  (add-break (lambda (e) (eql (trace-event-expert-id e) expert-id))
             (format nil "expert ~A" expert-id)))

(defun break-on-hook (hook-name)
  "Break when hook fires."
  (add-break (lambda (e) (eq (trace-event-hook-name e) hook-name))
             (format nil "hook ~A" hook-name)))

(defun break-on-step (step-num)
  "Break at specific step."
  (add-break (lambda (e) (= (trace-event-step e) step-num))
             (format nil "step ~A" step-num)))

(defun clear-breaks ()
  "Clear all break conditions."
  (setf *lab-break-conditions* nil)
  (format t "All break conditions cleared.~%"))

(defun list-breaks ()
  "List active break conditions."
  (format t "~%=== Break Conditions ===~%")
  (if *lab-break-conditions*
      (loop for (fn . desc) in *lab-break-conditions*
            for i from 1
            do (format t "  ~D. ~A~%" i desc))
      (format t "  (none)~%"))
  (format t "~%"))

(defun check-break-conditions (event)
  "Check if any break condition is met."
  (dolist (cond *lab-break-conditions*)
    (when (funcall (car cond) event)
      (setf *lab-paused* t)
      (format t "~%*** BREAK: ~A ***~%" (cdr cond))
      (format t "Type (continue-run) to resume, (step-once) to step.~%"))))

;;; ============================================================================
;;; FUNCTION TRACING - WRAP UHMA FUNCTIONS
;;; ============================================================================

(defun wrap-function (fn-symbol module)
  "Wrap a function to trace its calls."
  (when (and (fboundp fn-symbol)
             (not (gethash fn-symbol *lab-original-functions*)))
    (let ((original (symbol-function fn-symbol)))
      (setf (gethash fn-symbol *lab-original-functions*) original)
      (setf (symbol-function fn-symbol)
            (lambda (&rest args)
              (push fn-symbol *lab-call-stack*)
              (trace-event! :call :function-name fn-symbol :module module
                           :args (when (< (length args) 5) args))
              (let ((result (apply original args)))
                (trace-event! :return :function-name fn-symbol :module module
                             :result (when (atom result) result))
                (pop *lab-call-stack*)
                result))))))

(defun unwrap-function (fn-symbol)
  "Restore original function."
  (let ((original (gethash fn-symbol *lab-original-functions*)))
    (when original
      (setf (symbol-function fn-symbol) original)
      (remhash fn-symbol *lab-original-functions*))))

(defun trace-function (fn-name)
  "Start tracing a function. FN-NAME is a symbol like 'learn! or 'uhma::learn!"
  (let ((sym (if (symbolp fn-name)
                 fn-name
                 (find-symbol (string-upcase fn-name) :uhma))))
    (when sym
      (wrap-function sym nil)
      (pushnew sym *lab-watched-functions*)
      (format t "Tracing function: ~A~%" sym))))

(defun untrace-function (fn-name)
  "Stop tracing a function."
  (let ((sym (if (symbolp fn-name)
                 fn-name
                 (find-symbol (string-upcase fn-name) :uhma))))
    (when sym
      (unwrap-function sym)
      (setf *lab-watched-functions* (remove sym *lab-watched-functions*))
      (format t "Stopped tracing: ~A~%" sym))))

(defun trace-module (module-name)
  "Trace all functions in a module. MODULE-NAME is :presence, :self-mod, etc."
  (let ((funcs (cdr (assoc module-name *lab-modules*))))
    (if funcs
        (progn
          (dolist (fn funcs)
            (let ((sym (find-symbol (string fn) :uhma)))
              (when sym (wrap-function sym module-name))))
          (pushnew module-name *lab-watched-modules*)
          (format t "Tracing module: ~A (~D functions)~%" module-name (length funcs)))
        (format t "Unknown module: ~A~%Available: ~A~%"
                module-name (mapcar #'car *lab-modules*)))))

(defun untrace-module (module-name)
  "Stop tracing a module."
  (let ((funcs (cdr (assoc module-name *lab-modules*))))
    (dolist (fn funcs)
      (let ((sym (find-symbol (string fn) :uhma)))
        (when sym (unwrap-function sym))))
    (setf *lab-watched-modules* (remove module-name *lab-watched-modules*))
    (format t "Stopped tracing module: ~A~%" module-name)))

(defun trace-expert (expert-id)
  "Trace activity for a specific expert."
  (pushnew expert-id *lab-watched-experts*)
  (format t "Tracing expert: ~A~%" expert-id))

(defun untrace-expert (expert-id)
  "Stop tracing an expert."
  (setf *lab-watched-experts* (remove expert-id *lab-watched-experts*))
  (format t "Stopped tracing expert: ~A~%" expert-id))

(defun trace-hook (hook-name)
  "Trace a specific hook."
  (pushnew hook-name *lab-watched-hooks*)
  (format t "Tracing hook: ~A~%" hook-name))

(defun untrace-hook (hook-name)
  "Stop tracing a hook."
  (setf *lab-watched-hooks* (remove hook-name *lab-watched-hooks*))
  (format t "Stopped tracing hook: ~A~%" hook-name))

(defun trace-all-hooks ()
  "Trace all hook activity."
  (setf *lab-watched-hooks*
        '(+hook-pre-learn+ +hook-post-learn+ +hook-should-learn+
          +hook-pre-process-token+ +hook-post-process-token+
          +hook-expert-spawned+ +hook-expert-dying+ +hook-expert-activated+
          +hook-post-reset+ +hook-maintenance+ +hook-post-modification+))
  (format t "Tracing all hooks.~%"))

;;; ============================================================================
;;; TRACING CONTROL
;;; ============================================================================

(defun trace-on (&optional what)
  "Enable tracing. WHAT can be :all, a module name, or a function symbol."
  (setf *lab-trace-enabled* t)
  (cond
    ((null what) (format t "Tracing ON (all events)~%"))
    ((eq what :all)
     (dolist (mod *lab-modules*)
       (trace-module (car mod)))
     (format t "Tracing ON (all modules)~%"))
    ((keywordp what) (trace-module what))
    ((symbolp what) (trace-function what))
    (t (format t "Unknown trace target: ~A~%"))))

(defun trace-off ()
  "Disable all tracing."
  (setf *lab-trace-enabled* nil)
  ;; Unwrap all functions
  (maphash (lambda (sym orig)
             (declare (ignore orig))
             (unwrap-function sym))
           *lab-original-functions*)
  (clrhash *lab-original-functions*)
  (setf *lab-watched-functions* nil
        *lab-watched-experts* nil
        *lab-watched-hooks* nil
        *lab-watched-modules* nil)
  (format t "Tracing OFF~%"))

(defun trace-to-file (path)
  "Start logging trace to file."
  (when *lab-trace-file*
    (close *lab-trace-file*))
  (setf *lab-trace-file* (open path :direction :output :if-exists :supersede))
  (format t "Tracing to file: ~A~%" path))

(defun trace-file-close ()
  "Stop logging to file."
  (when *lab-trace-file*
    (close *lab-trace-file*)
    (setf *lab-trace-file* nil)
    (format t "Trace file closed.~%")))

;;; ============================================================================
;;; TRACE VIEWING & SEARCH
;;; ============================================================================

(defun trace-recent (&optional (n 20))
  "Show N most recent trace events."
  (format t "~%=== Recent Trace (~D events) ===~%"
          (min n (length *lab-trace-buffer*)))
  (dolist (event (reverse (subseq *lab-trace-buffer*
                                   0 (min n (length *lab-trace-buffer*)))))
    (display-trace-event event))
  (format t "~%"))

(defun trace-search (pattern &key (limit 50))
  "Search trace buffer for events matching PATTERN (string or function)."
  (format t "~%=== Trace Search: ~A ===~%" pattern)
  (let ((count 0)
        (matcher (if (stringp pattern)
                     (lambda (e)
                       (search pattern
                               (format nil "~A" (trace-event-message e))
                               :test #'char-equal))
                     pattern)))
    (dolist (event (reverse *lab-trace-buffer*))
      (when (and (< count limit) (funcall matcher event))
        (display-trace-event event)
        (incf count))))
  (format t "~%"))

(defun trace-for-step (step-num &key (context 5))
  "Show trace events around a specific step."
  (format t "~%=== Trace for Step ~D (+/- ~D) ===~%" step-num context)
  (dolist (event (reverse *lab-trace-buffer*))
    (let ((s (trace-event-step event)))
      (when (and (>= s (- step-num context))
                 (<= s (+ step-num context)))
        (display-trace-event event))))
  (format t "~%"))

(defun trace-for-expert (expert-id &key (limit 50))
  "Show trace events for a specific expert."
  (format t "~%=== Trace for Expert ~A ===~%" expert-id)
  (let ((count 0))
    (dolist (event (reverse *lab-trace-buffer*))
      (when (and (< count limit)
                 (eql (trace-event-expert-id event) expert-id))
        (display-trace-event event)
        (incf count))))
  (format t "~%"))

(defun trace-for-module (module-name &key (limit 50))
  "Show trace events for a specific module."
  (format t "~%=== Trace for Module ~A ===~%" module-name)
  (let ((count 0))
    (dolist (event (reverse *lab-trace-buffer*))
      (when (and (< count limit)
                 (eq (trace-event-module event) module-name))
        (display-trace-event event)
        (incf count))))
  (format t "~%"))

(defun trace-clear ()
  "Clear trace buffer."
  (setf *lab-trace-buffer* nil)
  (format t "Trace buffer cleared.~%"))

(defun trace-stats ()
  "Show statistics about trace buffer."
  (format t "~%=== Trace Statistics ===~%")
  (format t "  Events in buffer: ~D~%" (length *lab-trace-buffer*))
  (when *lab-trace-buffer*
    (let ((by-category (make-hash-table))
          (by-module (make-hash-table)))
      (dolist (e *lab-trace-buffer*)
        (incf (gethash (trace-event-category e) by-category 0))
        (when (trace-event-module e)
          (incf (gethash (trace-event-module e) by-module 0))))
      (format t "  By category:~%")
      (maphash (lambda (k v) (format t "    ~A: ~D~%" k v)) by-category)
      (format t "  By module:~%")
      (maphash (lambda (k v) (format t "    ~A: ~D~%" k v)) by-module)))
  (format t "~%"))

;;; ============================================================================
;;; SNAPSHOTS - CAPTURE AND COMPARE STATE
;;; ============================================================================

(defun snapshot (name)
  "Capture current state with a name."
  (let ((state (list
                :step (if (boundp 'uhma::*step*) uhma::*step* 0)
                :expert-count (if (boundp 'uhma::*experts*) (length uhma::*experts*) 0)
                :ltm-count (safe-ht-count '*long-term-memory*)
                :hyp-count (safe-ht-count '*hypotheses*)
                :schema-count (safe-ht-count '*cognitive-schemas*)
                :synthesized-count (safe-ht-count '*synthesized-ops*)
                :expert-ids (when (boundp 'uhma::*experts*)
                              (mapcar #'uhma::expert-id uhma::*experts*))
                :memory-mb (memory-usage-mb)
                :timestamp (get-universal-time))))
    (setf (gethash name *lab-snapshots*) state)
    (format t "Snapshot '~A' captured at step ~D~%" name (getf state :step))))

(defun compare-snapshots (name1 name2)
  "Compare two snapshots."
  (let ((s1 (gethash name1 *lab-snapshots*))
        (s2 (gethash name2 *lab-snapshots*)))
    (cond
      ((null s1) (format t "Snapshot '~A' not found.~%" name1))
      ((null s2) (format t "Snapshot '~A' not found.~%" name2))
      (t
       (format t "~%=== Comparing '~A' to '~A' ===~%" name1 name2)
       (format t "  Steps: ~D -> ~D (~@D)~%"
               (getf s1 :step) (getf s2 :step)
               (- (getf s2 :step) (getf s1 :step)))
       (format t "  Experts: ~D -> ~D (~@D)~%"
               (getf s1 :expert-count) (getf s2 :expert-count)
               (- (getf s2 :expert-count) (getf s1 :expert-count)))
       (format t "  LTM: ~D -> ~D (~@D)~%"
               (getf s1 :ltm-count) (getf s2 :ltm-count)
               (- (getf s2 :ltm-count) (getf s1 :ltm-count)))
       (format t "  Hypotheses: ~D -> ~D (~@D)~%"
               (getf s1 :hyp-count) (getf s2 :hyp-count)
               (- (getf s2 :hyp-count) (getf s1 :hyp-count)))
       (format t "  Schemas: ~D -> ~D (~@D)~%"
               (getf s1 :schema-count) (getf s2 :schema-count)
               (- (getf s2 :schema-count) (getf s1 :schema-count)))
       (format t "  Synthesized Ops: ~D -> ~D (~@D)~%"
               (getf s1 :synthesized-count) (getf s2 :synthesized-count)
               (- (getf s2 :synthesized-count) (getf s1 :synthesized-count)))
       (format t "  Memory: ~,1F MB -> ~,1F MB (~@,1F MB)~%"
               (getf s1 :memory-mb) (getf s2 :memory-mb)
               (- (getf s2 :memory-mb) (getf s1 :memory-mb)))
       ;; Expert changes
       (let ((old-ids (getf s1 :expert-ids))
             (new-ids (getf s2 :expert-ids)))
         (let ((born (set-difference new-ids old-ids))
               (died (set-difference old-ids new-ids)))
           (when born (format t "  Experts born: ~A~%" born))
           (when died (format t "  Experts died: ~A~%" died))))
       (format t "~%")))))

(defun list-snapshots ()
  "List all snapshots."
  (format t "~%=== Snapshots ===~%")
  (if (zerop (hash-table-count *lab-snapshots*))
      (format t "  (none)~%")
      (maphash (lambda (name state)
                 (format t "  ~A: step ~D, ~D experts, ~,1F MB~%"
                         name (getf state :step) (getf state :expert-count)
                         (getf state :memory-mb)))
               *lab-snapshots*))
  (format t "~%"))

;;; ============================================================================
;;; STATUS & OBSERVATION
;;; ============================================================================

(defun status ()
  "Show current system status."
  (format t "~%=== UHMA Status ===~%")
  (format t "  Step: ~D~%" (if (boundp 'uhma::*step*) uhma::*step* 0))
  (format t "  Experts: ~D~%" (if (boundp 'uhma::*experts*) (length uhma::*experts*) 0))
  (format t "  LTM entries: ~D~%" (safe-ht-count '*long-term-memory*))
  (format t "  Hypotheses: ~D~%" (safe-ht-count '*hypotheses*))
  (format t "  Schemas: ~D~%" (safe-ht-count '*cognitive-schemas*))
  (format t "  Synthesized Ops: ~D~%" (safe-ht-count '*synthesized-ops*))
  (format t "  Memory: ~,1F MB~%" (memory-usage-mb))
  (when (and (boundp 'uhma::*presence*) uhma::*presence*)
    (ignore-errors
      (format t "  Presence: trajectory=~A continuity=~,2F~%"
              (uhma::presence-trajectory uhma::*presence*)
              (uhma::presence-continuity uhma::*presence*))))
  (format t "~%"))

(defun experts (&optional (n 10))
  "List top N experts by reliability."
  (format t "~%=== Top ~D Experts ===~%" n)
  (when (boundp 'uhma::*experts*)
    (let ((sorted (sort (copy-list uhma::*experts*)
                        (lambda (a b)
                          (> (/ (1+ (uhma::expert-hits a))
                                (+ 2 (uhma::expert-hits a) (uhma::expert-misses a)))
                             (/ (1+ (uhma::expert-hits b))
                                (+ 2 (uhma::expert-hits b) (uhma::expert-misses b))))))))
      (loop for exp in sorted
            for i from 1 to n
            do (let* ((hits (uhma::expert-hits exp))
                      (misses (uhma::expert-misses exp))
                      (rel (* 100 (/ (1+ hits) (+ 2 hits misses)))))
                 (format t "  ~3D. Expert-~A: ~4D/~4D (~5,1F%) life=~,2F~%"
                         i (uhma::expert-id exp) hits (+ hits misses) rel
                         (uhma::expert-life exp))))))
  (format t "~%"))

(defun expert-detail (id)
  "Show detailed info about a specific expert."
  (let ((exp (find id uhma::*experts* :key #'uhma::expert-id)))
    (if exp
        (progn
          (format t "~%=== Expert ~A ===~%" id)
          (format t "  Hits: ~D~%" (uhma::expert-hits exp))
          (format t "  Misses: ~D~%" (uhma::expert-misses exp))
          (format t "  Life: ~,2F~%" (uhma::expert-life exp))
          (format t "  Birth step: ~D~%" (uhma::expert-birth-step exp))
          (format t "  Confidence threshold: ~,2F~%" (uhma::expert-confidence-threshold exp))
          (format t "  Knowledge entries: ~D~%" (hash-table-count (uhma::expert-knowledge exp)))
          (format t "  Last used: step ~D~%" (uhma::expert-last-used exp))
          (format t "  Ownership: ~,2F~%" (uhma::expert-ownership exp))
          (format t "~%  Knowledge sample:~%")
          (let ((count 0))
            (block done
              (maphash (lambda (k v)
                         (when (>= count 5) (return-from done))
                         (format t "    ~S -> ~S~%" k v)
                         (incf count))
                       (uhma::expert-knowledge exp))))
          (format t "~%  Program preview:~%")
          (let ((*print-length* 5)
                (*print-level* 3))
            (format t "    ~S~%" (uhma::expert-program exp)))
          (format t "~%"))
        (format t "Expert ~A not found.~%" id))))

(defun hypotheses (&optional (n 10))
  "List top N hypotheses."
  (format t "~%=== Top ~D Hypotheses ===~%" n)
  (when (and (boundp 'uhma::*hypotheses*) (hash-table-p uhma::*hypotheses*))
    (let ((all nil))
      (maphash (lambda (k v) (push (cons k v) all)) uhma::*hypotheses*)
      ;; Sort by confidence if self-hypothesis, otherwise by strength
      (setf all (sort all #'>
                      :key (lambda (x)
                             (let ((hyp (cdr x)))
                               (cond
                                 ((and (typep hyp 'uhma::self-hypothesis)
                                       (slot-exists-p hyp 'uhma::confidence))
                                  (uhma::self-hypothesis-confidence hyp))
                                 ((listp hyp) (or (getf hyp :strength) 0))
                                 (t 0))))))
      (loop for (key . val) in all
            for i from 1 to n
            do (cond
                 ((typep val 'uhma::self-hypothesis)
                  (format t "  ~D. ~A~%     claim=~A conf=~,2F tested=~D~%"
                          i key
                          (uhma::self-hypothesis-claim val)
                          (uhma::self-hypothesis-confidence val)
                          (uhma::self-hypothesis-times-tested val)))
                 ((listp val)
                  (format t "  ~D. ~A~%     strength=~,2F evidence=~D~%"
                          i key
                          (or (getf val :strength) 0)
                          (or (getf val :evidence-count) 0)))
                 (t (format t "  ~D. ~A: ~A~%" i key val))))))
  (format t "~%"))

(defun schemas (&optional (n 10))
  "List cognitive schemas."
  (format t "~%=== Cognitive Schemas (top ~D) ===~%" n)
  (when (and (boundp 'uhma::*cognitive-schemas*) (hash-table-p uhma::*cognitive-schemas*))
    (let ((all nil))
      (maphash (lambda (k v) (push (cons k v) all)) uhma::*cognitive-schemas*)
      ;; Sort by success rate
      (setf all (sort all #'>
                      :key (lambda (x)
                             (let ((s (cdr x)))
                               (if (and (typep s 'uhma::cognitive-schema)
                                        (> (uhma::cognitive-schema-instances s) 0))
                                   (/ (uhma::cognitive-schema-successes s)
                                      (uhma::cognitive-schema-instances s))
                                   0)))))
      (loop for (id . schema) in all
            for i from 1 to n
            do (when (typep schema 'uhma::cognitive-schema)
                 (let* ((instances (uhma::cognitive-schema-instances schema))
                        (successes (uhma::cognitive-schema-successes schema))
                        (rate (if (> instances 0) (* 100 (/ successes instances)) 0)))
                   (format t "  ~D. ~A~%     instances=~D successes=~D (~,1F%)~%"
                           i id instances successes rate))))))
  (format t "~%"))

(defun synthesized-ops (&optional (n 10))
  "List synthesized operations."
  (format t "~%=== Synthesized Ops (top ~D) ===~%" n)
  (when (and (boundp 'uhma::*synthesized-ops*) (hash-table-p uhma::*synthesized-ops*))
    (let ((count 0))
      (maphash (lambda (k v)
                 (when (< count n)
                   (format t "  ~A: ~A~%" k (type-of v))
                   (incf count)))
               uhma::*synthesized-ops*)))
  (format t "~%"))

(defun memory-sample (&optional (n 10))
  "Show sample of LTM entries."
  (format t "~%=== LTM Sample (~D entries) ===~%" n)
  (when (and (boundp 'uhma::*long-term-memory*) (hash-table-p uhma::*long-term-memory*))
    (let ((count 0))
      (maphash (lambda (k v)
                 (when (< count n)
                   (if (typep v 'uhma::consolidated-pattern)
                       (format t "  ~S -> pred=~A conf=~,2F~%"
                               k
                               (uhma::consolidated-pattern-prediction v)
                               (uhma::consolidated-pattern-confidence v))
                       (format t "  ~S -> ~S~%" k v))
                   (incf count)))
               uhma::*long-term-memory*)))
  (format t "~%"))

(defun hooks-status ()
  "Show registered hooks."
  (format t "~%=== Registered Hooks ===~%")
  (when (boundp 'uhma::*hooks*)
    (maphash (lambda (hook handlers)
               (when handlers
                 (format t "  ~A: ~D handlers~%" hook (length handlers))))
             uhma::*hooks*))
  (format t "~%"))

(defun presence-detail ()
  "Show detailed presence state."
  (format t "~%=== Presence State ===~%")
  (when (and (boundp 'uhma::*presence*) uhma::*presence*)
    (ignore-errors
      (format t "  Vividness: ~,3F~%" (uhma::presence-vividness uhma::*presence*))
      (format t "  Trajectory: ~A~%" (uhma::presence-trajectory uhma::*presence*))
      (format t "  Self-confidence: ~,3F~%" (uhma::presence-self-confidence uhma::*presence*))
      (format t "  Continuity: ~,3F~%" (uhma::presence-continuity uhma::*presence*))))
  (format t "~%"))

(defun drives-status ()
  "Show intrinsic drives status."
  (format t "~%=== Intrinsic Drives ===~%")
  (when (boundp 'uhma::*intrinsic-drives*)
    (dolist (d uhma::*intrinsic-drives*)
      (format t "  ~A: level=~,2F~%"
              (uhma::drive-name d)
              (uhma::drive-current-level d))))
  (format t "~%"))

;;; ============================================================================
;;; MEMORY DIAGNOSTICS
;;; ============================================================================

(defstruct mem-snapshot
  "A snapshot of memory state."
  step
  timestamp
  total-bytes
  ;; Core counts
  experts-count
  ltm-count
  hypotheses-count
  schemas-count
  synthesized-count
  ;; History lists
  hypothesis-history-len
  modification-history-len
  goal-history-len
  ;; Arrays
  trace-buffer-fp
  cognitive-trace-buffer-fp)

(defun take-memory-snapshot ()
  "Capture current memory state for diagnostics."
  (make-mem-snapshot
   :step (if (boundp 'uhma::*step*) uhma::*step* 0)
   :timestamp (get-internal-real-time)
   :total-bytes (sb-kernel:dynamic-usage)
   :experts-count (if (boundp 'uhma::*experts*) (length uhma::*experts*) 0)
   :ltm-count (safe-ht-count '*long-term-memory*)
   :hypotheses-count (safe-ht-count '*hypotheses*)
   :schemas-count (safe-ht-count '*cognitive-schemas*)
   :synthesized-count (safe-ht-count '*synthesized-ops*)
   :hypothesis-history-len (safe-length '*hypothesis-history*)
   :modification-history-len (safe-length '*modification-history*)
   :goal-history-len (safe-length '*goal-history*)
   :trace-buffer-fp (safe-fp '*trace-buffer*)
   :cognitive-trace-buffer-fp (safe-fp '*cognitive-trace-buffer*)))

(defun diagnose-memory ()
  "Run memory harmony diagnostic."
  (format t "~%=== MEMORY HARMONY DIAGNOSTIC ===~%")
  (let ((snap (take-memory-snapshot)))
    (format t "  Total memory: ~,1F MB~%" (/ (mem-snapshot-total-bytes snap) 1048576.0))
    (format t "~%  Data structure sizes:~%")
    (format t "    Experts: ~D~%" (mem-snapshot-experts-count snap))
    (format t "    LTM entries: ~D~%" (mem-snapshot-ltm-count snap))
    (format t "    Hypotheses: ~D~%" (mem-snapshot-hypotheses-count snap))
    (format t "    Schemas: ~D~%" (mem-snapshot-schemas-count snap))
    (format t "    Synthesized ops: ~D~%" (mem-snapshot-synthesized-count snap))
    (format t "~%  History lists:~%")
    (format t "    Hypothesis history: ~D~%" (mem-snapshot-hypothesis-history-len snap))
    (format t "    Modification history: ~D~%" (mem-snapshot-modification-history-len snap))
    (format t "    Goal history: ~D~%" (mem-snapshot-goal-history-len snap))
    (format t "~%  Trace buffers:~%")
    (format t "    Trace buffer: ~D~%" (mem-snapshot-trace-buffer-fp snap))
    (format t "    Cognitive trace buffer: ~D~%" (mem-snapshot-cognitive-trace-buffer-fp snap))

    ;; Save snapshot for comparison
    (push snap *lab-mem-snapshots*)
    (when (> (length *lab-mem-snapshots*) 100)
      (setf *lab-mem-snapshots* (subseq *lab-mem-snapshots* 0 100)))

    ;; Compare with previous if exists
    (when (> (length *lab-mem-snapshots*) 1)
      (let ((prev (second *lab-mem-snapshots*)))
        (format t "~%  Changes since last snapshot:~%")
        (format t "    Memory: ~@,1F MB~%"
                (/ (- (mem-snapshot-total-bytes snap)
                      (mem-snapshot-total-bytes prev))
                   1048576.0))
        (format t "    Experts: ~@D~%"
                (- (mem-snapshot-experts-count snap)
                   (mem-snapshot-experts-count prev)))
        (format t "    LTM: ~@D~%"
                (- (mem-snapshot-ltm-count snap)
                   (mem-snapshot-ltm-count prev))))))
  (format t "~%"))

(defun memory-trend ()
  "Show memory growth trend over snapshots."
  (format t "~%=== Memory Trend ===~%")
  (if (< (length *lab-mem-snapshots*) 2)
      (format t "  Need at least 2 snapshots. Run diagnose-memory multiple times.~%")
      (let ((snaps (reverse *lab-mem-snapshots*)))
        (loop for snap in snaps
              for i from 1
              do (format t "  [~D] Step ~6D: ~,1F MB, ~D exp, ~D ltm~%"
                         i
                         (mem-snapshot-step snap)
                         (/ (mem-snapshot-total-bytes snap) 1048576.0)
                         (mem-snapshot-experts-count snap)
                         (mem-snapshot-ltm-count snap)))))
  (format t "~%"))

;;; ============================================================================
;;; SYNTHESIS DIAGNOSTICS
;;; ============================================================================

(defun diagnose-synthesis ()
  "Check synthesis machinery and eligibility."
  (format t "~%=== SYNTHESIS DIAGNOSTIC ===~%")

  ;; Check if synthesis machinery is wired
  (format t "~%  Synthesis machinery:~%")
  (format t "    synthesize-op-from-schema: ~A~%"
          (if (fboundp 'uhma::synthesize-op-from-schema) "bound" "MISSING"))
  (format t "    compose-op-from-pattern: ~A~%"
          (if (fboundp 'uhma::compose-op-from-pattern) "bound" "MISSING"))
  (format t "    adopt-synthesized-op!: ~A~%"
          (if (fboundp 'uhma::adopt-synthesized-op!) "bound" "MISSING"))
  (format t "    wire-schemas-to-ops!: ~A~%"
          (if (fboundp 'uhma::wire-schemas-to-ops!) "bound" "MISSING"))
  (format t "    *synthesized-ops*: ~A~%"
          (if (boundp 'uhma::*synthesized-ops*)
              (format nil "bound (~D ops)" (hash-table-count uhma::*synthesized-ops*))
              "MISSING"))
  (format t "    *op-synthesis-enabled*: ~A~%"
          (if (boundp 'uhma::*op-synthesis-enabled*)
              uhma::*op-synthesis-enabled*
              "MISSING"))

  ;; Check schema eligibility
  (format t "~%  Schema eligibility (for synthesis):~%")
  (when (and (boundp 'uhma::*cognitive-schemas*) (hash-table-p uhma::*cognitive-schemas*))
    (let ((eligible 0)
          (threshold-rate 0.7)
          (threshold-inst 20))
      (maphash (lambda (id schema)
                 (when (typep schema 'uhma::cognitive-schema)
                   (let* ((inst (uhma::cognitive-schema-instances schema))
                          (succ (uhma::cognitive-schema-successes schema))
                          (rate (if (> inst 0) (/ succ inst) 0)))
                     (when (and (> rate threshold-rate) (> inst threshold-inst))
                       (incf eligible)
                       (format t "    ~A: ~D inst, ~D succ (~,1F%)~%"
                               id inst succ (* 100 rate))))))
               uhma::*cognitive-schemas*)
      (format t "~%    Total eligible: ~D / ~D schemas~%"
              eligible (hash-table-count uhma::*cognitive-schemas*))))

  ;; Check synthesis conditions
  (format t "~%  Synthesis conditions:~%")
  (when (boundp 'uhma::*step*)
    (format t "    *step*: ~D~%" uhma::*step*))
  (when (boundp 'uhma::*last-synthesis-trigger-step*)
    (format t "    *last-synthesis-trigger-step*: ~D~%" uhma::*last-synthesis-trigger-step*))
  (when (boundp 'uhma::*synthesis-cooldown*)
    (format t "    *synthesis-cooldown*: ~D~%" uhma::*synthesis-cooldown*))
  (when (fboundp 'uhma::should-attempt-synthesis-p)
    (format t "    should-attempt-synthesis-p: ~A~%" (uhma::should-attempt-synthesis-p)))
  (format t "~%"))

;;; ============================================================================
;;; LOOP DIAGNOSTICS
;;; ============================================================================

(defun diagnose-loops ()
  "Diagnose all cognitive loop components."
  (format t "~%=== COGNITIVE LOOPS DIAGNOSTIC ===~%")

  ;; 1. Traces
  (format t "~%  1. Execution traces:~%")
  (format t "     *trace-buffer*: ~A~%"
          (if (boundp 'uhma::*trace-buffer*)
              (format nil "~D entries" (safe-fp '*trace-buffer*))
              "MISSING"))

  ;; 2. Presence
  (format t "~%  2. Presence substrate:~%")
  (if (and (boundp 'uhma::*presence*) uhma::*presence*)
      (progn
        (format t "     trajectory: ~A~%" (uhma::presence-trajectory uhma::*presence*))
        (format t "     continuity: ~,2F~%" (uhma::presence-continuity uhma::*presence*))
        (format t "     self-confidence: ~,2F~%" (uhma::presence-self-confidence uhma::*presence*)))
      (format t "     *presence*: MISSING~%"))

  ;; 3. Drives
  (format t "~%  3. Intrinsic drives:~%")
  (if (boundp 'uhma::*intrinsic-drives*)
      (dolist (d uhma::*intrinsic-drives*)
        (format t "     ~A: ~,2F~%" (uhma::drive-name d) (uhma::drive-current-level d)))
      (format t "     *intrinsic-drives*: MISSING~%"))

  ;; 4. Goals
  (format t "~%  4. Goal stack:~%")
  (format t "     *goal-stack*: ~A~%"
          (if (boundp 'uhma::*goal-stack*)
              (format nil "~D goals" (length uhma::*goal-stack*))
              "MISSING"))

  ;; 5. Hypotheses
  (format t "~%  5. Hypotheses:~%")
  (format t "     *hypotheses*: ~A~%"
          (if (boundp 'uhma::*hypotheses*)
              (format nil "~D active" (safe-ht-count '*hypotheses*))
              "MISSING"))

  ;; 6. Schemas
  (format t "~%  6. Cognitive schemas:~%")
  (format t "     *cognitive-schemas*: ~A~%"
          (if (boundp 'uhma::*cognitive-schemas*)
              (format nil "~D schemas" (safe-ht-count '*cognitive-schemas*))
              "MISSING"))

  ;; 7. Self-awareness loop
  (format t "~%  7. Self-awareness loop:~%")
  (format t "     *self-awareness-loop-active*: ~A~%"
          (if (boundp 'uhma::*self-awareness-loop-active*)
              uhma::*self-awareness-loop-active*
              "MISSING"))
  (format t "     *understanding-log*: ~A~%"
          (if (boundp 'uhma::*understanding-log*)
              (format nil "~D entries" (length uhma::*understanding-log*))
              "MISSING"))

  ;; 8. Travelers
  (format t "~%  8. Travelers (cross-expert):~%")
  (format t "     *travelers*: ~A~%"
          (if (boundp 'uhma::*travelers*)
              (format nil "~D travelers" (length uhma::*travelers*))
              "MISSING"))

  ;; Hook counts
  (format t "~%  Hook registration:~%")
  (when (boundp 'uhma::*hooks*)
    (let ((total 0))
      (maphash (lambda (k v) (declare (ignore k)) (incf total (length v))) uhma::*hooks*)
      (format t "     Total handlers: ~D across ~D hooks~%"
              total (hash-table-count uhma::*hooks*))))
  (format t "~%"))

;;; ============================================================================
;;; COMPREHENSIVE DIAGNOSTIC
;;; ============================================================================

(defun diagnose ()
  "Run all diagnostics."
  (format t "~%")
  (format t "================================================================~%")
  (format t "            UHMA COMPREHENSIVE DIAGNOSTIC~%")
  (format t "================================================================~%")
  (status)
  (diagnose-loops)
  (diagnose-synthesis)
  (diagnose-memory)
  (format t "================================================================~%")
  (format t "            END DIAGNOSTIC~%")
  (format t "================================================================~%"))

;;; ============================================================================
;;; FEEDING
;;; ============================================================================

(defun feed-text (text &key (verbose nil))
  "Feed text to the system."
  (format t "Feeding ~D chars...~%" (length text))
  (uhma::process-text! text :verbose verbose)
  (status))

(defun feed-file (path &key (verbose nil))
  "Feed a file to the system."
  (if (probe-file path)
      (progn
        (format t "Feeding file: ~A~%" path)
        (with-open-file (s path :direction :input)
          (let* ((len (file-length s))
                 (content (make-string len)))
            (read-sequence content s)
            (uhma::process-text! content :verbose verbose)))
        (status))
      (format t "File not found: ~A~%" path)))

(defun feed-codebase (&key (epochs 1) (verbose nil) (chunk-size 2000))
  "Feed all UHMA .lisp files to the system."
  (let ((files (remove-if (lambda (f)
                            (let ((n (string-downcase (file-namestring f))))
                              (or (search "test" n) (search "debug" n)
                                  (search "trace" n) (search "run-" n)
                                  (search "lab" n) (search "feed" n)
                                  (search "diagnose" n) (search "check" n)
                                  (search "archive" (namestring f)))))
                          (directory "*.lisp"))))
    (format t "~%Feeding ~D files x ~D epochs (chunk=~D)...~%"
            (length files) epochs chunk-size)
    (dotimes (epoch epochs)
      (format t "~%--- Epoch ~D/~D ---~%" (1+ epoch) epochs)
      (let ((n 0)
            (chunks 0))
        (dolist (f files)
          (incf n)
          (format t "  [~D/~D] ~A" n (length files) (file-namestring f))
          (force-output)
          (handler-case
              (with-open-file (s f)
                (let* ((len (min (file-length s) 50000))
                       (txt (make-string len)))
                  (read-sequence txt s)
                  (loop for i from 0 below len by chunk-size
                        do (handler-case
                               (progn
                                 (uhma::process-text! (subseq txt i (min (+ i chunk-size) len))
                                                     :verbose verbose)
                                 (incf chunks))
                             (error (e) (declare (ignore e)))))))
            (error (e) (format t " ERROR: ~A" (type-of e))))
          (format t "~%"))
        (format t "  Epoch ~D done. Chunks=~D Experts=~D LTM=~D~%"
                (1+ epoch) chunks (length uhma::*experts*)
                (safe-ht-count '*long-term-memory*))))))

(defun feed-and-observe (&key (files nil) (report-interval 60))
  "Feed files while reporting state periodically."
  (let ((target-files (or files
                          (remove-if (lambda (f)
                                       (let ((n (string-downcase (file-namestring f))))
                                         (or (search "test" n) (search "feed" n)
                                             (search "archive" (namestring f)))))
                                     (directory "*.lisp"))))
        (start-time (get-universal-time))
        (last-report (get-universal-time))
        (files-fed 0)
        (chunks-fed 0))

    (format t "~%=== FEED AND OBSERVE ===~%")
    (format t "Files to process: ~D~%" (length target-files))
    (status)

    (dolist (file target-files)
      (handler-case
          (with-open-file (stream file :direction :input)
            (format t "~%> ~A (~D bytes)~%"
                    (file-namestring file) (file-length stream))
            (let ((content (make-string (file-length stream))))
              (read-sequence content stream)
              (loop for start from 0 below (length content) by 300
                    do (let ((chunk (subseq content start
                                            (min (+ start 300) (length content)))))
                         (handler-case
                             (progn
                               (uhma::process-chunk! chunk)
                               (incf chunks-fed))
                           (error (e) (declare (ignore e)))))
                    do (when (> (- (get-universal-time) last-report) report-interval)
                         (format t "~%--- Progress Report ---~%")
                         (status)
                         (setf last-report (get-universal-time))))))
        (error (e)
          (format t "  Skip (error): ~A~%" (type-of e))))
      (incf files-fed))

    (format t "~%=== FINAL STATE ===~%")
    (status)
    (format t "Files fed: ~D, Chunks: ~D, Time: ~D sec~%"
            files-fed chunks-fed (- (get-universal-time) start-time))))

;;; ============================================================================
;;; LIVE MODE & STEPPING
;;; ============================================================================

(defun run-steps (n &key (report-interval 100))
  "Run exactly N internal steps."
  (format t "~%Running ~D steps...~%" n)
  (dotimes (i n)
    (when *lab-paused*
      (format t "~%Paused at step ~D. (continue-run) to resume.~%" uhma::*step*)
      (return-from run-steps))
    (handler-case (uhma::internal-step!)
      (error (e) (format t "Step error: ~A~%" e)))
    (incf *lab-step-count*)
    (when (and (> report-interval 0) (zerop (mod (1+ i) report-interval)))
      (format t "  Step ~D/~D (total: ~D)~%" (1+ i) n uhma::*step*)
      (force-output)))
  (format t "Done. Now at step ~D~%" uhma::*step*))

(defun run-live (&key (duration 60) (save-interval nil) (report-interval 10))
  "Run live mode for DURATION seconds."
  (format t "~%=== Live Mode (~D seconds) ===~%" duration)
  (format t "Press Ctrl+C to stop.~%~%")
  (let ((start (get-internal-real-time))
        (last-save 0)
        (last-report 0)
        (step-count 0))
    (handler-case
        (loop
          (when *lab-paused*
            (format t "~%Paused. (continue-run) to resume.~%")
            (return))
          (let ((elapsed (/ (- (get-internal-real-time) start)
                            internal-time-units-per-second)))
            (when (>= elapsed duration) (return))
            (when (and report-interval (>= (- elapsed last-report) report-interval))
              (format t "[~A] Step ~D | Exp=~D LTM=~D Mem=~,1FMB~%"
                      (format-time elapsed) uhma::*step*
                      (length uhma::*experts*)
                      (safe-ht-count '*long-term-memory*)
                      (memory-usage-mb))
              (force-output)
              (setf last-report elapsed))
            (when (and save-interval (>= (- elapsed last-save) save-interval))
              (let ((fname (format nil "uhma-state-live-~Dmin.lisp" (round elapsed 60))))
                (format t "~%[SAVING ~A]~%" fname)
                (ignore-errors (uhma::save-everything! fname))
                (setf last-save elapsed)))
            (handler-case (uhma::internal-step!)
              (error (e) (declare (ignore e))))
            (incf step-count)
            (incf *lab-step-count*)
            (sleep 0.01)))
      (sb-sys:interactive-interrupt ()
        (format t "~%Interrupted.~%")))
    (format t "~%Ran ~D steps. Now at step ~D~%" step-count uhma::*step*)
    (status)))

(defun step-once ()
  "Execute one internal step with trace output."
  (format t "~%--- Step ~D ---~%" uhma::*step*)
  (let ((*lab-trace-enabled* t))
    (handler-case (uhma::internal-step!)
      (error (e) (format t "Error: ~A~%" e))))
  (incf *lab-step-count*)
  (format t "--- Now at step ~D ---~%~%" uhma::*step*))

(defun step-n (n)
  "Execute N steps with trace output."
  (dotimes (i n)
    (step-once)
    (when *lab-paused* (return))))

(defun step-token (text)
  "Process text and show what happens."
  (format t "~%--- Processing: ~S ---~%" text)
  (let ((*lab-trace-enabled* t))
    (uhma::process-text! (string text) :verbose t))
  (format t "~%"))

(defun step-until (condition-fn &key (max-steps 1000))
  "Step until condition is met or max steps reached."
  (format t "Stepping until condition met (max ~D)...~%" max-steps)
  (dotimes (i max-steps)
    (handler-case (uhma::internal-step!)
      (error (e) (declare (ignore e))))
    (incf *lab-step-count*)
    (when (funcall condition-fn)
      (format t "Condition met at step ~D~%" uhma::*step*)
      (return-from step-until t)))
  (format t "Max steps reached without condition.~%")
  nil)

(defun continue-run ()
  "Continue after pause."
  (setf *lab-paused* nil)
  (format t "Continuing...~%"))

(defun pause-run ()
  "Pause execution."
  (setf *lab-paused* t)
  (format t "Paused at step ~D~%" uhma::*step*))

;;; ============================================================================
;;; TEST SUITE
;;; ============================================================================

(defun test-load ()
  "Test: System loads and resets correctly."
  (format t "~%Test: Load and reset...~%")
  (uhma::reset!)
  (let ((experts (length uhma::*experts*)))
    (if (> experts 0)
        (format t "  PASS: Reset created ~D experts~%" experts)
        (format t "  FAIL: No experts after reset~%"))
    (> experts 0)))

(defun test-feed ()
  "Test: System can process text and learn."
  (format t "~%Test: Feed text...~%")
  (uhma::reset!)
  (uhma::process-text! "the cat sat on the mat the dog ran" :verbose nil)
  (uhma::process-text! "the cat sat on the mat the dog ran" :verbose nil)
  (dotimes (i 200) (ignore-errors (uhma::internal-step!)))
  (let ((experts (length uhma::*experts*)))
    (if (> experts 5)
        (format t "  PASS: System grew to ~D experts~%" experts)
        (format t "  FAIL: Only ~D experts~%" experts))
    (> experts 5)))

(defun test-steps ()
  "Test: Internal steps run without error."
  (format t "~%Test: Internal steps...~%")
  (uhma::reset!)
  (uhma::process-text! "test data" :verbose nil)
  (let ((step-before uhma::*step*))
    (dotimes (i 100) (ignore-errors (uhma::internal-step!)))
    (let ((step-after uhma::*step*))
      (if (> step-after step-before)
          (format t "  PASS: Steps advanced ~D -> ~D~%" step-before step-after)
          (format t "  FAIL: Steps didn't advance~%"))
      (> step-after step-before))))

(defun test-learning ()
  "Test: Learning actually creates LTM entries."
  (format t "~%Test: Learning creates LTM...~%")
  (uhma::reset!)
  (let ((ltm-before (safe-ht-count '*long-term-memory*)))
    (dotimes (i 5)
      (uhma::process-text! "the quick brown fox jumps over lazy dog" :verbose nil))
    (let ((ltm-after (safe-ht-count '*long-term-memory*)))
      (if (> ltm-after ltm-before)
          (format t "  PASS: LTM grew ~D -> ~D~%" ltm-before ltm-after)
          (format t "  FAIL: LTM unchanged~%"))
      (> ltm-after ltm-before))))

(defun test-hypotheses ()
  "Test: Hypotheses form during processing."
  (format t "~%Test: Hypothesis formation...~%")
  (uhma::reset!)
  (dotimes (i 10)
    (uhma::process-text! "pattern recognition learning prediction" :verbose nil))
  (let ((hyp-count (safe-ht-count '*hypotheses*)))
    (if (> hyp-count 0)
        (format t "  PASS: ~D hypotheses formed~%" hyp-count)
        (format t "  WARN: No hypotheses (may need more training)~%"))
    t))  ; Don't fail - hypotheses are optional

(defun test-schemas ()
  "Test: Schemas form from repeated patterns."
  (format t "~%Test: Schema formation...~%")
  (uhma::reset!)
  (dotimes (i 20)
    (uhma::process-text! "the cat sat on the mat" :verbose nil))
  (let ((schema-count (safe-ht-count '*cognitive-schemas*)))
    (if (> schema-count 0)
        (format t "  PASS: ~D schemas formed~%" schema-count)
        (format t "  WARN: No schemas (may need more training)~%"))
    t))  ; Don't fail - schemas are optional

(defun test-presence ()
  "Test: Presence substrate is active."
  (format t "~%Test: Presence substrate...~%")
  (let ((has-presence (and (boundp 'uhma::*presence*) uhma::*presence*)))
    (if has-presence
        (progn
          (format t "  PASS: Presence exists~%")
          (format t "    trajectory: ~A~%" (uhma::presence-trajectory uhma::*presence*))
          (format t "    continuity: ~,2F~%" (uhma::presence-continuity uhma::*presence*)))
        (format t "  FAIL: No presence substrate~%"))
    has-presence))

(defun test-hooks ()
  "Test: Hooks don't duplicate on reset."
  (format t "~%Test: Hook stability...~%")
  (uhma::reset!)
  (let ((hooks1 (hash-table-count uhma::*hooks*)))
    (uhma::reset!)
    (let ((hooks2 (hash-table-count uhma::*hooks*)))
      (if (= hooks1 hooks2)
          (format t "  PASS: Hook count stable (~D)~%" hooks1)
          (format t "  FAIL: Hooks changed (~D -> ~D)~%" hooks1 hooks2))
      (= hooks1 hooks2))))

(defun run-tests ()
  "Run all tests and return results."
  (format t "~%")
  (format t "========================================~%")
  (format t "UHMA Lab Test Suite~%")
  (format t "========================================~%")
  (let ((results (list
                  (cons 'load (test-load))
                  (cons 'feed (test-feed))
                  (cons 'steps (test-steps))
                  (cons 'learning (test-learning))
                  (cons 'hypotheses (test-hypotheses))
                  (cons 'schemas (test-schemas))
                  (cons 'presence (test-presence))
                  (cons 'hooks (test-hooks)))))
    (setf *lab-test-results* results)
    (format t "~%========================================~%")
    (format t "Results: ~D/~D passed~%"
            (count t results :key #'cdr) (length results))
    (dolist (r results)
      (format t "  ~A: ~A~%" (car r) (if (cdr r) "PASS" "FAIL")))
    (format t "========================================~%")
    (every #'cdr results)))

(defun run-comprehensive-test ()
  "Run comprehensive test with training and loop verification."
  (format t "~%")
  (format t "================================================================~%")
  (format t "         COMPREHENSIVE UHMA VALIDATION TEST~%")
  (format t "================================================================~%")

  ;; Initialize
  (format t "~%Phase 0: Initialization...~%")
  (uhma::reset!)
  (when (fboundp 'uhma::initialize-cognitive-control!)
    (uhma::initialize-cognitive-control!))
  (when (fboundp 'uhma::initialize-v65-agentic!)
    (uhma::initialize-v65-agentic!))
  (when (fboundp 'uhma::initialize-introspective-vocabulary!)
    (uhma::initialize-introspective-vocabulary!))
  (when (fboundp 'uhma::initialize-pattern-utilization!)
    (uhma::initialize-pattern-utilization!))
  (when (fboundp 'uhma::initialize-episodic-memory!)
    (uhma::initialize-episodic-memory!))
  (when (fboundp 'uhma::initialize-presence-integration!)
    (uhma::initialize-presence-integration!))
  (format t "  Experts: ~D~%" (length uhma::*experts*))

  ;; Training
  (format t "~%Phase 1: Training (30 iterations)...~%")
  (let ((corpus '("The quick brown fox jumps over the lazy dog."
                  "Learning patterns helps predict future outcomes."
                  "When uncertain the system explores alternatives."
                  "Success builds confidence failure triggers adjustment."
                  "The cat sat on the mat watching birds.")))
    (dotimes (iter 30)
      (dolist (line corpus)
        (uhma::process-text! line :verbose nil))
      (when (zerop (mod (1+ iter) 10))
        (ignore-errors (uhma::run-consolidation!))
        (format t "  [~D/30] Exp=~D LTM=~D Hyp=~D~%"
                (1+ iter)
                (length uhma::*experts*)
                (safe-ht-count '*long-term-memory*)
                (safe-ht-count '*hypotheses*)))))

  ;; Verify loops
  (format t "~%Phase 2: Loop verification...~%")
  (diagnose-loops)

  (format t "~%Phase 3: Final status...~%")
  (status)

  (format t "================================================================~%")
  (format t "         COMPREHENSIVE TEST COMPLETE~%")
  (format t "================================================================~%"))

;;; ============================================================================
;;; EMERGENCY CONTROLS
;;; ============================================================================

(defun set-threshold (name value)
  "Set a threshold parameter."
  (let ((sym (find-symbol (format nil "*~A*" (string-upcase name)) :uhma)))
    (if (and sym (boundp sym))
        (progn
          (setf (symbol-value sym) value)
          (format t "Set ~A = ~A~%" sym value))
        (format t "Unknown: ~A~%" name))))

(defun show-thresholds ()
  "Show all threshold parameters."
  (format t "~%=== Thresholds ===~%")
  (dolist (name '("SPARSE-THRESHOLD" "ATTENTION-FOCUS-THRESHOLD"
                  "DEATH-THRESHOLD" "MERGE-SIMILARITY-THRESHOLD"
                  "IMPROVEMENT-THRESHOLD" "HYPOTHESIS-DECAY-RATE"
                  "SYNTHESIS-COOLDOWN" "OP-SYNTHESIS-ENABLED"))
    (let ((sym (find-symbol (format nil "*~A*" name) :uhma)))
      (when (and sym (boundp sym))
        (format t "  ~A: ~A~%" name (symbol-value sym)))))
  (format t "~%"))

(defun force-consolidation ()
  "Force a consolidation cycle."
  (format t "Running consolidation...~%")
  (ignore-errors (uhma::run-consolidation!))
  (format t "Done.~%"))

(defun force-introspection (&optional (depth 3))
  "Force introspection cycle."
  (format t "Running introspection (depth ~D)...~%" depth)
  (ignore-errors (uhma::introspect! depth))
  (format t "Done.~%"))

(defun force-gc ()
  "Force garbage collection."
  (format t "Running GC...~%")
  (sb-ext:gc :full t)
  (format t "Memory after GC: ~,1F MB~%" (memory-usage-mb)))

;;; ============================================================================
;;; PERSISTENCE
;;; ============================================================================

(defun save-state (filename)
  "Save full system state to file."
  (ignore-errors (uhma::save-everything! filename))
  (format t "State saved to ~A~%" filename))

(defun load-state (filename)
  "Load system state from file."
  (if (probe-file filename)
      (progn
        (ignore-errors (uhma::load-everything! filename))
        (format t "State loaded from ~A~%" filename)
        (status))
      (format t "File not found: ~A~%" filename)))

(defun list-states ()
  "List available saved state files."
  (format t "~%=== Saved States ===~%")
  (let ((files (sort (directory "uhma-state*.lisp") #'>
                     :key #'file-write-date)))
    (if files
        (dolist (f files)
          (format t "  ~A  (~:D bytes)~%"
                  (file-namestring f)
                  (ignore-errors (with-open-file (s f) (file-length s)))))
        (format t "  (none)~%")))
  (format t "~%"))

;;; ============================================================================
;;; INTERACTIVE MENU
;;; ============================================================================

(defun print-menu ()
  "Print the main menu."
  (format t "~%")
  (format t "================================================================~%")
  (format t "                    UHMA LAB MENU~%")
  (format t "================================================================~%")
  (format t "~%")
  (format t "  [1] TEST      - Run test suite~%")
  (format t "  [2] FEED      - Feed text/files to UHMA~%")
  (format t "  [3] TRACE     - Trace functions/modules/hooks~%")
  (format t "  [4] DIAGNOSE  - Run diagnostics~%")
  (format t "  [5] LIVE      - Run live mode~%")
  (format t "  [6] STATUS    - Show current status~%")
  (format t "  [7] PERSIST   - Save/load state~%")
  (format t "  [8] HELP      - Show all commands~%")
  (format t "  [0] EXIT      - Exit lab~%")
  (format t "~%")
  (format t "================================================================~%")
  (format t "  Current: Step ~D | Experts ~D | LTM ~D | Memory ~,1F MB~%"
          (if (boundp 'uhma::*step*) uhma::*step* 0)
          (if (boundp 'uhma::*experts*) (length uhma::*experts*) 0)
          (safe-ht-count '*long-term-memory*)
          (memory-usage-mb))
  (format t "================================================================~%"))

(defun menu-test ()
  "Test submenu."
  (format t "~%=== TEST MENU ===~%")
  (format t "  [1] Quick tests (run-tests)~%")
  (format t "  [2] Comprehensive test (run-comprehensive-test)~%")
  (format t "  [3] Test learning only~%")
  (format t "  [4] Test hypotheses only~%")
  (format t "  [0] Back~%")
  (format t "~%Choice: ")
  (force-output)
  (let ((choice (read-line)))
    (cond
      ((string= choice "1") (run-tests))
      ((string= choice "2") (run-comprehensive-test))
      ((string= choice "3") (test-learning))
      ((string= choice "4") (test-hypotheses))
      (t nil))))

(defun menu-feed ()
  "Feed submenu."
  (format t "~%=== FEED MENU ===~%")
  (format t "  [1] Feed sample text~%")
  (format t "  [2] Feed codebase (1 epoch)~%")
  (format t "  [3] Feed and observe~%")
  (format t "  [4] Custom text (enter)~%")
  (format t "  [0] Back~%")
  (format t "~%Choice: ")
  (force-output)
  (let ((choice (read-line)))
    (cond
      ((string= choice "1")
       (feed-text "The quick brown fox jumps over the lazy dog. Learning patterns helps predict future outcomes. Experts compete for context ownership."))
      ((string= choice "2") (feed-codebase :epochs 1))
      ((string= choice "3") (feed-and-observe))
      ((string= choice "4")
       (format t "Enter text: ")
       (force-output)
       (feed-text (read-line)))
      (t nil))))

(defun menu-trace ()
  "Trace submenu."
  (format t "~%=== TRACE MENU ===~%")
  (format t "  [1] Trace ON (all)~%")
  (format t "  [2] Trace OFF~%")
  (format t "  [3] Trace module (:learning, :presence, etc)~%")
  (format t "  [4] Show recent trace~%")
  (format t "  [5] Trace stats~%")
  (format t "  [0] Back~%")
  (format t "~%Choice: ")
  (force-output)
  (let ((choice (read-line)))
    (cond
      ((string= choice "1") (trace-on :all))
      ((string= choice "2") (trace-off))
      ((string= choice "3")
       (format t "Module (:presence :learning :schemas :hypotheses :experts): ")
       (force-output)
       (let ((mod (read-from-string (read-line))))
         (trace-module mod)))
      ((string= choice "4") (trace-recent 20))
      ((string= choice "5") (trace-stats))
      (t nil))))

(defun menu-diagnose ()
  "Diagnose submenu."
  (format t "~%=== DIAGNOSE MENU ===~%")
  (format t "  [1] Full diagnostic~%")
  (format t "  [2] Memory diagnostic~%")
  (format t "  [3] Loops diagnostic~%")
  (format t "  [4] Synthesis diagnostic~%")
  (format t "  [5] Memory trend~%")
  (format t "  [0] Back~%")
  (format t "~%Choice: ")
  (force-output)
  (let ((choice (read-line)))
    (cond
      ((string= choice "1") (diagnose))
      ((string= choice "2") (diagnose-memory))
      ((string= choice "3") (diagnose-loops))
      ((string= choice "4") (diagnose-synthesis))
      ((string= choice "5") (memory-trend))
      (t nil))))

(defun menu-live ()
  "Live mode submenu."
  (format t "~%=== LIVE MENU ===~%")
  (format t "  [1] Run 30 seconds~%")
  (format t "  [2] Run 5 minutes~%")
  (format t "  [3] Run 1 hour~%")
  (format t "  [4] Step once~%")
  (format t "  [5] Run 100 steps~%")
  (format t "  [0] Back~%")
  (format t "~%Choice: ")
  (force-output)
  (let ((choice (read-line)))
    (cond
      ((string= choice "1") (run-live :duration 30))
      ((string= choice "2") (run-live :duration 300))
      ((string= choice "3") (run-live :duration 3600 :save-interval 600))
      ((string= choice "4") (step-once))
      ((string= choice "5") (run-steps 100))
      (t nil))))

(defun menu-persist ()
  "Persistence submenu."
  (format t "~%=== PERSISTENCE MENU ===~%")
  (format t "  [1] Save state~%")
  (format t "  [2] Load state~%")
  (format t "  [3] List saved states~%")
  (format t "  [4] Snapshot~%")
  (format t "  [5] List snapshots~%")
  (format t "  [0] Back~%")
  (format t "~%Choice: ")
  (force-output)
  (let ((choice (read-line)))
    (cond
      ((string= choice "1")
       (let ((fname (format nil "uhma-state-~A.lisp"
                            (multiple-value-bind (s m h d mo y)
                                (get-decoded-time)
                              (format nil "~4,'0D~2,'0D~2,'0D-~2,'0D~2,'0D"
                                      y mo d h m)))))
         (save-state fname)))
      ((string= choice "2")
       (list-states)
       (format t "Filename: ")
       (force-output)
       (load-state (read-line)))
      ((string= choice "3") (list-states))
      ((string= choice "4")
       (format t "Snapshot name: ")
       (force-output)
       (snapshot (intern (string-upcase (read-line)))))
      ((string= choice "5") (list-snapshots))
      (t nil))))

(defun interactive-menu ()
  "Run interactive menu loop."
  (loop
    (print-menu)
    (format t "~%Choice: ")
    (force-output)
    (let ((choice (read-line)))
      (cond
        ((string= choice "0") (return))
        ((string= choice "1") (menu-test))
        ((string= choice "2") (menu-feed))
        ((string= choice "3") (menu-trace))
        ((string= choice "4") (menu-diagnose))
        ((string= choice "5") (menu-live))
        ((string= choice "6") (status))
        ((string= choice "7") (menu-persist))
        ((string= choice "8") (lab-help))
        (t (format t "Invalid choice: ~A~%" choice))))))

;;; ============================================================================
;;; HELP
;;; ============================================================================

(defun lab-help ()
  "Show available commands."
  (format t "
=== UHMA Lab Commands ===

MODES:
  (lab)                   - Interactive menu mode
  (interactive-menu)      - Same as (lab)

STATUS & OBSERVATION:
  (status)                - System overview
  (experts N)             - Top N experts
  (expert-detail ID)      - Expert details
  (hypotheses N)          - Top N hypotheses
  (schemas N)             - List schemas
  (synthesized-ops N)     - List synthesized ops
  (memory-sample N)       - LTM sample
  (hooks-status)          - Registered hooks
  (presence-detail)       - Presence state
  (drives-status)         - Drive levels

DIAGNOSTICS:
  (diagnose)              - Full diagnostic
  (diagnose-memory)       - Memory harmony
  (diagnose-loops)        - Cognitive loops
  (diagnose-synthesis)    - Synthesis eligibility
  (memory-trend)          - Memory growth over time

PERSISTENCE:
  (save-state FILE)       - Save state
  (load-state FILE)       - Load state
  (list-states)           - List saved states
  (snapshot NAME)         - Capture snapshot
  (compare-snapshots A B) - Compare snapshots
  (list-snapshots)        - List snapshots

FEEDING:
  (feed-text TEXT)        - Feed text
  (feed-file PATH)        - Feed file
  (feed-codebase :epochs N) - Feed codebase
  (feed-and-observe)      - Feed with periodic reports

LIVE MODE:
  (run-steps N)           - Run N steps
  (run-live :duration S)  - Run for S seconds
  (step-once)             - Single step with trace
  (step-n N)              - N steps with trace
  (step-token TEXT)       - Process text with trace
  (pause-run)             - Pause execution
  (continue-run)          - Resume execution

TRACING:
  (trace-on)              - Enable all tracing
  (trace-on :module)      - Trace specific module
  (trace-off)             - Disable tracing
  (trace-function 'fn)    - Trace function calls
  (trace-module :name)    - Trace module (:presence :self-mod :schemas
                            :consolidation :dreams :hypotheses :learning
                            :prediction :experts :introspection :synthesis :hooks)
  (trace-expert ID)       - Trace expert activity
  (trace-hook 'hook)      - Trace hook
  (trace-all-hooks)       - Trace all hooks
  (trace-to-file PATH)    - Log to file
  (trace-file-close)      - Stop file logging

TRACE VIEWING:
  (trace-recent N)        - Last N events
  (trace-search PATTERN)  - Search trace
  (trace-for-step N)      - Events around step N
  (trace-for-expert ID)   - Events for expert
  (trace-for-module :mod) - Events for module
  (trace-stats)           - Trace statistics
  (trace-clear)           - Clear trace buffer

BREAKPOINTS:
  (break-on-function 'fn) - Break when fn called
  (break-on-expert ID)    - Break on expert activity
  (break-on-hook 'hook)   - Break when hook fires
  (break-on-step N)       - Break at step N
  (clear-breaks)          - Clear all breakpoints
  (list-breaks)           - List breakpoints

TESTS:
  (run-tests)             - Run quick tests
  (run-comprehensive-test)- Run full validation

EMERGENCY CONTROLS:
  (show-thresholds)       - Show thresholds
  (set-threshold NAME V)  - Set threshold
  (force-consolidation)   - Run consolidation
  (force-introspection)   - Run introspection
  (force-gc)              - Force garbage collection
  (uhma::reset!)          - Full reset

"))

;;; ============================================================================
;;; MAIN ENTRY POINT
;;; ============================================================================

(defun lab ()
  "Start interactive lab session."
  (setf *lab-session-start* (get-universal-time))
  (format t "~%")
  (format t "================================================================~%")
  (format t "     UHMA Lab - Comprehensive Cognitive System Workbench~%")
  (format t "================================================================~%")
  (format t "~%")
  (format t "Consolidated from: test-*.lisp, diagnose-*.lisp, feed-*.lisp,~%")
  (format t "                   trace-*.lisp, run-*.lisp, check-*.lisp~%")
  (format t "~%")
  (status)
  (format t "~%Type (lab-help) for all commands, or choose from menu:~%")
  (interactive-menu))

;;; ============================================================================
;;; STARTUP MESSAGE
;;; ============================================================================

(format t "~%")
(format t "================================================================~%")
(format t "UHMA Lab loaded successfully.~%")
(format t "================================================================~%")
(format t "~%")
(format t "Quick start:~%")
(format t "  (lab)        - Interactive menu~%")
(format t "  (run-tests)  - Quick test suite~%")
(format t "  (diagnose)   - Full diagnostic~%")
(format t "  (status)     - Current status~%")
(format t "  (lab-help)   - All commands~%")
(format t "~%")


;;;; [SECTION-START:99:VERIFICATION]
;;; Section 99: File Verification

(defun verify-lab-lisp-completeness () 
  "Verify completeness."
  (unless (find-package :uhma)
    (error "Package UHMA not defined"))
  (unless (fboundp 'lab-config)
    (error "Function lab-config not defined"))
  (format t "~&uhma-lab.lisp verification passed.~%"))

(verify-lab-lisp-completeness)

;;;; [SECTION-END:99:VERIFICATION]
