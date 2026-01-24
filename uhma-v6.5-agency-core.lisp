;;;; ============================================================================
;;;; UHMA AGENCY - CORE (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Provides executable schemas and operation synthesis.
;;;; DEPENDS ON: uhma-v6.1-core-homoiconic.lisp, uhma-v6.2-deep-mind.lisp
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 4: STRUCTURES ---

(defstruct executable-schema
  "A schema compiled into executable operation sequences."
  (id (gensym "EXEC-SCHEMA-") :type symbol)
  (source-schema-id nil :type symbol)
  (op-sequence nil :type list)
  (entry-conditions nil :type list)
  (parameter-bindings nil :type list)
  (execution-count 0 :type fixnum)
  (success-count 0 :type fixnum)
  (avg-confidence 0.0 :type single-float)
  (contexts-applied nil :type list)
  (compiled-at 0 :type fixnum)
  (last-executed 0 :type fixnum))

;;; Structs moved to forward-decl.lisp

;;; --- SECTION 6: SPECIAL VARIABLES ---

(defvar *executable-schemas* (make-hash-table :test 'equal))
(defvar *synthesized-ops* (make-hash-table :test 'eq))
(defvar *schema-execution-log* nil)
(defvar *active-schema* nil)
(defvar *schema-attempts* 0)
(defvar *schema-successes* 0)

;;; --- SECTION 8: CORE FUNCTIONS ---

(defun extract-op-sequence (pattern)
  "Normalize schema patterns into linear op sequences."
  (cond ((null pattern) nil)
        ((atom (first pattern)) (list pattern))
        (t pattern)))

(defun compile-schema-to-executable (schema)
  "Transform cognitive-schema into agency-level executable."
  (declare (type cognitive-schema schema))
  (let ((pattern (cognitive-schema-pattern schema)))
    (make-executable-schema
     :source-schema-id (cognitive-schema-id schema)
     :op-sequence (extract-op-sequence pattern)
     :compiled-at *step*)))

;;; --- SECTION 11: VERIFICATION ---
(eval-when (:load-toplevel :execute)
  (format t "[AGENCY-CORE] Agency foundation loaded.~%"))
