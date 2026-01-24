;;;; ============================================================================
;;;; UHMA COMPOSITIONAL REASONING - CORE (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Provides working memory, variable binding, and structure composition.
;;;; DEPENDS ON: uhma-v6.1-core-homoiconic.lisp
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 4: STRUCTURES ---

(defstruct working-memory
  "Limited-capacity cognitive workspace."
  (capacity 7 :type fixnum)
  (slots (make-array 7 :initial-element nil) :type (simple-array t (7)))
  (slot-ages (make-array 7 :initial-element 0 :element-type 'fixnum) :type (simple-array fixnum (7)))
  (slot-priorities (make-array 7 :initial-element 0.0 :element-type 'single-float) :type (simple-array single-float (7)))
  (current-focus nil :type (or null fixnum))
  (compression-count 0 :type fixnum)
  (overflow-count 0 :type fixnum)
  (total-bindings 0 :type fixnum))

(defstruct wm-item
  "An item in working memory."
  (content nil)
  (type :token :type symbol)
  (source nil)
  (priority 0.5 :type single-float)
  (age 0 :type fixnum)
  (bindings nil :type list)
  (relations nil :type list))

(defstruct logic-var
  "A bindable variable."
  (name nil :type symbol)
  (vtype :any :type symbol)
  (binding nil)
  (constraints nil :type list))

(defstruct binding-frame
  "A set of variable bindings."
  (id (gensym "FRAME-") :type symbol)
  (bindings (make-hash-table) :type hash-table)
  (parent nil :type (or null binding-frame))
  (created-step 0 :type fixnum))

(defstruct comp-structure
  "A compositional structure built from parts."
  (id (gensym "STRUCT-") :type symbol)
  (type :sequence :type symbol)
  (head nil)
  (slots nil :type list)
  (source-tokens nil :type list)
  (confidence 0.5 :type single-float)
  (created-step 0 :type fixnum))

;;; --- SECTION 6: SPECIAL VARIABLES ---

(defvar *working-memory* (make-working-memory))
(defvar *current-frame* nil)
(defvar *frame-stack* nil)
(defvar *structure-templates* (make-hash-table :test 'equal))
(defvar *built-structures* nil)

;;; --- SECTION 8: CORE FUNCTIONS ---

(defun wm-clear! ()
  "Flush the scratchpad."
  (let ((wm *working-memory*))
    (dotimes (i (working-memory-capacity wm))
      (setf (aref (working-memory-slots wm) i) nil
            (aref (working-memory-slot-ages wm) i) 0
            (aref (working-memory-slot-priorities wm) i) 0.0))
    (setf (working-memory-current-focus wm) nil)))

(defun wm-load! (content &key (type :token) (priority 0.5) source bindings)
  "Load an item into the magic-number-7 workspace."
  (declare (type single-float priority))
  (let* ((wm *working-memory*)
         (item (make-wm-item :content content :type type :priority priority :source source :bindings bindings))
         (slot (find-wm-slot wm priority)))
    (when slot
      (setf (aref (working-memory-slots wm) slot) item
            (aref (working-memory-slot-ages wm) slot) 0
            (aref (working-memory-slot-priorities wm) slot) priority)
      (incf (working-memory-total-bindings wm))
      slot)))

(defun var? (x)
  "Identity check for logic variables (?X)."
  (and (symbolp x) (char= (char (symbol-name x) 0) #\?)))

(defun unify (pattern datum &optional (frame *current-frame*))
  "Recursive symbolic unification."
  (let ((f (or frame (make-binding-frame))))
    (labels ((recur (p d)
               (cond ((var? p) (let ((val (gethash p (binding-frame-bindings f))))
                                 (if val (recur val d) (progn (setf (gethash p (binding-frame-bindings f)) d) t))))
                     ((atom p) (equal p d))
                     ((and (consp p) (consp d)) (and (recur (car p) (car d)) (recur (cdr p) (cdr d))))
                     (t nil))))
      (values (recur pattern datum) f))))

(defun find-wm-slot (wm priority)
  "Find best slot for new item: empty or lowest-priority."
  (let ((best-slot nil)
        (best-priority most-positive-single-float))
    (dotimes (i (working-memory-capacity wm))
      (let ((item (aref (working-memory-slots wm) i)))
        (cond ((null item) (return-from find-wm-slot i))
              ((< (aref (working-memory-slot-priorities wm) i) best-priority)
               (setf best-slot i
                     best-priority (aref (working-memory-slot-priorities wm) i))))))
    (when (and best-slot (< best-priority priority))
      best-slot)))

(defun wm-tick! ()
  "Age working memory items. Evict items past their useful life."
  (let ((wm *working-memory*))
    (dotimes (i (working-memory-capacity wm))
      (when (aref (working-memory-slots wm) i)
        (incf (aref (working-memory-slot-ages wm) i))
        (when (> (aref (working-memory-slot-ages wm) i) 15)
          (setf (aref (working-memory-slots wm) i) nil
                (aref (working-memory-slot-ages wm) i) 0
                (aref (working-memory-slot-priorities wm) i) 0.0))))))

(defun wm-contents ()
  "Return list of current working memory items."
  (let ((wm *working-memory*)
        (items nil))
    (dotimes (i (working-memory-capacity wm))
      (let ((item (aref (working-memory-slots wm) i)))
        (when item (push item items))))
    (nreverse items)))

(defun try-build-structure (tokens)
  "Attempt to find compositional structure in a token sequence."
  (when (and (listp tokens) (> (length tokens) 1))
    (let ((abstract (abstract-pattern tokens)))
      ;; Check for known templates
      (let ((template-key (format nil "~{~A~^|~}" abstract)))
        (when (gethash template-key *structure-templates*)
          (let ((struct (make-comp-structure
                         :type :template-match
                         :head (first tokens)
                         :slots (rest tokens)
                         :source-tokens tokens
                         :confidence 0.7
                         :created-step (if (boundp '*step*) *step* 0))))
            (push struct *built-structures*)
            (return-from try-build-structure struct)))))
    ;; Detect repetition structure (A B A pattern)
    (when (>= (length tokens) 3)
      (let ((first-tok (first tokens)))
        (when (member first-tok (cddr tokens) :test #'equal)
          (let ((struct (make-comp-structure
                          :type :repetition
                          :head first-tok
                          :slots (remove first-tok tokens :test #'equal :count 1)
                          :source-tokens tokens
                          :confidence 0.5
                          :created-step (if (boundp '*step*) *step* 0))))
            (push struct *built-structures*)
            (return-from try-build-structure struct)))))
    ;; Detect sequence structure (no repetition, ordered)
    (when (>= (length tokens) 3)
      (let ((struct (make-comp-structure
                      :type :sequence
                      :head (first tokens)
                      :slots (rest tokens)
                      :source-tokens tokens
                      :confidence 0.3
                      :created-step (if (boundp '*step*) *step* 0))))
        (push struct *built-structures*)
        struct))))

;;; --- SECTION 11: VERIFICATION ---
(eval-when (:load-toplevel :execute)
  (format t "[COMP-CORE] Working memory and Unification active.~%"))
