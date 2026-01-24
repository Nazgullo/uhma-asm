;;;; ============================================================================
;;;; UHMA COMPOSITIONAL REASONING - LOADER & PLANNING (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Orchestrates compositional core and implements causal planning.
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 1: MODULE LOADING ---
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((dir (make-pathname :directory (pathname-directory *load-truename*))))
    (load (merge-pathnames "uhma-v6.7-compositional-core.lisp" dir))))

;;; --- SECTION 4: STRUCTURES ---

(defstruct causal-link
  "A directional relationship: X causes Y."
  (id (gensym "CAUSE-") :type symbol)
  cause effect
  (strength 0.5 :type single-float)
  (delay 1 :type fixnum)
  (status :hypothesized :type symbol)
  (observations 0 :type fixnum)
  (confirmations 0 :type fixnum))

(defstruct plan
  "A sequence of steps toward a goal."
  (id (gensym "PLAN-") :type symbol)
  goal steps
  (current-step 0 :type fixnum)
  (status :planning :type symbol))

(defstruct plan-step
  "A single step in a plan."
  (action nil)
  (preconditions nil :type list)
  (expected-result nil)
  (confidence 0.5 :type single-float))

(defstruct temporal-projection
  "A projected future sequence."
  (sequence nil :type list)
  (confidence 0.0 :type single-float)
  (horizon 0 :type fixnum)
  (source nil))

;;; --- SECTION 6: SPECIAL VARIABLES ---

(defvar *causal-model* (make-hash-table :test 'equal))
(defvar *plan-library* nil)
(defvar *action-library* nil)

;;; --- SECTION 8: CORE LOGIC ---

(defun add-causal-link! (cause effect &key (strength 0.5) (delay 1))
  "Insert directional link into causal map."
  (declare (type single-float strength) (type fixnum delay))
  (let* ((key (cons cause effect))
         (link (or (gethash key *causal-model*)
                   (setf (gethash key *causal-model*) (make-causal-link :cause cause :effect effect :strength strength :delay delay)))))
    link))

(defun observe-causal! (cause effect confirmed-p)
  "Update causal link strength based on evidence."
  (let ((link (gethash (cons cause effect) *causal-model*)))
    (when link
      (incf (causal-link-observations link))
      (when confirmed-p (incf (causal-link-confirmations link)))
      (setf (causal-link-strength link) (/ (float (causal-link-confirmations link)) (causal-link-observations link))))))

(defun get-causes (token)
  "Get all known causes of TOKEN from the causal model. Returns alist of (cause . strength)."
  (let ((causes nil))
    (maphash (lambda (key link)
               (when (equal (cdr key) token)
                 (push (cons (causal-link-cause link)
                             (causal-link-strength link))
                       causes)))
             *causal-model*)
    (sort causes #'> :key #'cdr)))

(defun get-effects (token)
  "Get all known effects of TOKEN from the causal model. Returns alist of (effect . strength)."
  (let ((effects nil))
    (maphash (lambda (key link)
               (when (equal (car key) token)
                 (push (cons (causal-link-effect link)
                             (causal-link-strength link))
                       effects)))
             *causal-model*)
    (sort effects #'> :key #'cdr)))

(defun goal-success-criteria (goal)
  "Derive success criteria from a goal's structure."
  (when goal
    (let ((desc (goal-description goal))
          (drive (goal-drive-source goal)))
      (cond
        ((and drive (eq drive :competence))
         (list :accuracy-above 0.6))
        ((and drive (eq drive :curiosity))
         (list :novelty-encountered t))
        ((and drive (eq drive :efficiency))
         (list :step-count-below 10))
        ((stringp desc)
         (list :description-match desc))
        (t (list :any-progress t))))))

(defun plan-for-goal (goal-state current-state)
  "Generate a simple plan to transition from current-state to goal-state."
  (when (and goal-state (listp goal-state))
    (let ((steps nil)
          (criteria (if (listp goal-state) goal-state nil)))
      ;; Generate steps based on criteria type
      (cond
        ((getf criteria :accuracy-above)
         (push (make-plan-step :action :seek-familiar-context
                               :expected-result :higher-accuracy
                               :confidence 0.4) steps))
        ((getf criteria :novelty-encountered)
         (push (make-plan-step :action :explore-novel-context
                               :expected-result :new-pattern
                               :confidence 0.3) steps))
        ((getf criteria :step-count-below)
         (push (make-plan-step :action :use-cached-prediction
                               :expected-result :faster-response
                               :confidence 0.5) steps))
        (t
         (push (make-plan-step :action :continue-observing
                               :expected-result :more-data
                               :confidence 0.3) steps)))
      (when steps
        (make-plan :goal goal-state
                   :steps (nreverse steps)
                   :status :ready)))))

(defun project-future (ctx horizon)
  "Project likely future sequence from current context using causal model."
  (declare (type fixnum horizon))
  (when (and (listp ctx) ctx)
    (let ((sequence nil)
          (current (first ctx)))
      (dotimes (i horizon)
        (let ((effects (get-effects current)))
          (if effects
              (let ((next (caar effects)))
                (push next sequence)
                (setf current next))
              (return))))
      (when sequence
        (make-temporal-projection
         :sequence (nreverse sequence)
         :confidence (if (> (length sequence) 0)
                         (/ 0.5 (float (length sequence)))
                         0.0)
         :horizon horizon
         :source ctx)))))

;;; --- SECTION 9: INTEGRATION HOOKS ---

(defun compositional-reasoning-hook (actual ctx predicted correct-p)
  "Periodic update of workspace and causal models."
  (declare (ignore actual predicted))
  (when (fboundp 'wm-tick!) (wm-tick!))
  (when (and (consp ctx) (car ctx))
    (observe-causal! (car ctx) actual correct-p)))

(defun initialize-compositional-reasoning! ()
  "Initialize the compositional reasoning module."
  (setf *causal-model* (make-hash-table :test 'equal))
  (setf *plan-library* nil)
  (setf *action-library* nil))

;;; --- SECTION 10: INITIALIZATION ---
(eval-when (:load-toplevel :execute)
  (register-hook +hook-post-process-token+ 'compositional-reasoning-hook)
  (format t "[COMPOSITIONAL] Reasoner and Planner active.~%"))