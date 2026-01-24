;;;; ============================================================================
;;;; UHMA DEEP MIND - INTROSPECTION (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Provides recursive layers of meta-cognition ("Thinking about Thinking").
;;;; DEPENDS ON: uhma-v6.2-deep-mind-core.lisp
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 4: STRUCTURES ---

(defstruct introspection-layer
  "A layer of meta-cognition observing the layer below."
  (level 0 :type fixnum)
  (observations nil :type list)
  (predictions nil :type list)
  (model-of-below nil)
  (error-patterns nil :type list)
  (interventions nil :type list)
  (last-introspection 0 :type fixnum))

;;; --- SECTION 6: SPECIAL VARIABLES ---

(defvar *introspection-stack* nil)
(defvar *max-introspection-depth* 3)
(defvar *surprise-threshold* 0.8)
(defvar *surprise-window* nil)
(defvar *surprise-window-size* 10)

;;; --- SECTION 8: CORE FUNCTIONS ---

(defun ensure-introspection-depth (depth)
  "Expand introspection stack to required depth."
  (declare (type fixnum depth))
  (loop while (< (length *introspection-stack*) depth)
        do (push (make-introspection-layer :level (length *introspection-stack*))
                 *introspection-stack*))
  (setf *introspection-stack* (nreverse *introspection-stack*)))

(defun introspect! (&optional (depth 1))
  "Execute introspection cycle across stack layers."
  (declare (type fixnum depth))
  (ensure-introspection-depth (min depth *max-introspection-depth*))
  (loop for level from 0 below (min depth (length *introspection-stack*))
        do (introspect-at-layer! (nth level *introspection-stack*) level)))

(defun introspect-at-layer! (layer level)
  "Observe, model, and propose interventions for a specific layer."
  (declare (type introspection-layer layer) (type fixnum level))
  (setf (introspection-layer-last-introspection layer) *step*)
  (let* ((source (if (= level 0)
                     (get-traces :n 50 :meta-level 0)
                     (introspection-layer-observations (nth (1- level) *introspection-stack*))))
         (observations (extract-patterns source level)))
    (setf (introspection-layer-observations layer) observations
          (introspection-layer-model-of-below layer) (build-layer-model source level)
          (introspection-layer-error-patterns layer) (identify-error-patterns source level)
          (introspection-layer-interventions layer) (propose-interventions (introspection-layer-error-patterns layer) level))
    ;; Auto-theorize from observations
    (dolist (obs observations)
      (let ((hyp-obs (translate-observation-to-theory obs)))
        (when hyp-obs (generate-hypothesis! hyp-obs))))
    ;; Record meta-cognitive trace
    (record-trace! (make-cognitive-trace :step *step* :meta-level (1+ level)
                                         :context `(:introspection-at ,level)
                                         :meta-observations observations))))

(defun record-surprise! (trace)
  "Surprise-driven introspection trigger."
  (declare (type cognitive-trace trace))
  (when (> (cognitive-trace-surprise trace) *surprise-threshold*)
    (push trace *surprise-window*)
    (when (> (length *surprise-window*) *surprise-window-size*)
      (setf *surprise-window* (subseq *surprise-window* 0 *surprise-window-size*)))
    (when (>= (length *surprise-window*) 3)
      (let ((burst (count-if (lambda (tr) (< (- *step* (cognitive-trace-step tr)) 50)) *surprise-window*)))
        (when (>= burst 3)
          (introspect! 2)
          (let ((common (find-common-context *surprise-window*)))
            (when common (generate-hypothesis! (list :context-causes-errors common :surprise-burst))))
          (setf *surprise-window* nil))))))

;;; --- SECTION 11: VERIFICATION ---
(eval-when (:load-toplevel :execute)
  (format t "[DEEP-MIND-INTROSPECTION] Meta-cognition active.~%"))
