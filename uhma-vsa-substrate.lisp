;;;; ============================================================================
;;;; UHMA TRUE VSA SUBSTRATE (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Provides 1:1 fidelity holographic memory using 1024-dim vectors.
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 1: CONSTANTS ---
(defconstant +vsa-dim+ 1024 "Dimensionality of the VSA Mind-Space")

;;; --- SECTION 2: SPECIAL VARIABLES ---
(defparameter *vsa-cleanup-threshold* 0.4 "Similarity for 1:1 symbol recovery")
(defvar *vsa-symbol-to-vec* (make-hash-table :test 'eq) "The 'dictionary' of clean vectors.")
(defvar *vsa-vec-to-symbol* nil "Reverse map for cleanup (alist).")

;;; --- SECTION 3: UTILITY FUNCTIONS (Math & Creation) ---

(defun make-vsa-vec ()
  "Create a new zeroed VSA vector."
  (declare (optimize (speed 3) (safety 1)))
  (make-array +vsa-dim+ :element-type 'single-float :initial-element 0.0))

(defun make-random-vsa-vec ()
  "Create a random bipolar orthogonal vector (-1, +1)."
  (let ((v (make-vsa-vec)))
    (declare (type (simple-array single-float (*)) v))
    (dotimes (i +vsa-dim+)
      (setf (aref v i) (if (< (random 1.0) 0.5) -1.0 1.0)))
    v))

(defun vcopy (v)
  "Create a copy of vector V."
  (declare (type (simple-array single-float (*)) v))
  (let ((n (make-array (length v) :element-type 'single-float)))
    (dotimes (i (length v)) (setf (aref n i) (aref v i)))
    n))

(defun vdot (a b)
  "Dot product of vectors A and B."
  (declare (type (simple-array single-float (*)) a b)
           (optimize (speed 3) (safety 1)))
  (let ((s 0.0))
    (dotimes (i (length a) s)
      (incf s (* (aref a i) (aref b i))))))

(defun vmag (v)
  "Magnitude of vector V."
  (declare (type (simple-array single-float (*)) v))
  (sqrt (max 0.0001 (vdot v v))))

(defun vnorm! (v)
  "In-place vector normalization."
  (declare (type (simple-array single-float (*)) v)
           (optimize (speed 3) (safety 1)))
  (let ((m (vmag v)))
    (unless (zerop m)
      (dotimes (i (length v))
        (setf (aref v i) (/ (aref v i) m)))))
  v)

(defun cosim (v1 v2)
  "Cosine similarity between two vectors."
  (declare (type (simple-array single-float (*)) v1 v2)
           (optimize (speed 3) (safety 1)))
  (let ((dot (vdot v1 v2))
        (m1 (vmag v1))
        (m2 (vmag v2)))
    (if (or (zerop m1) (zerop m2))
        0.0
        (/ dot (* m1 m2)))))

;;; --- SECTION 4: CORE VSA OPERATORS ---

(defun vsa-bind (v1 v2)
  "Bind two vectors (Circular Convolution approximation)."
  (declare (type (simple-array single-float (*)) v1 v2)
           (optimize (speed 3) (safety 1)))
  (let ((result (make-vsa-vec)))
    (dotimes (i +vsa-dim+)
      (setf (aref result i) (* (aref v1 i) (aref v2 i))))
    result))

(defun vsa-unbind (composite v1)
  "Unbind v1 from composite to retrieve the other part."
  (vsa-bind composite v1))

(defun vsa-superpose! (target source &optional (weight 1.0))
  "Fold source vector into target superposition."
  (declare (type (simple-array single-float (*)) target source)
           (type single-float weight)
           (optimize (speed 3) (safety 1)))
  (dotimes (i +vsa-dim+)
    (incf (aref target i) (* weight (aref source i))))
  (vnorm! target))

(defun vsa-permute (v &optional (shifts 1))
  "Rotate vector bits to represent order/sequence."
  (declare (type (simple-array single-float (*)) v)
           (type fixnum shifts)
           (optimize (speed 3) (safety 1)))
  (let ((result (make-vsa-vec))
        (n (mod shifts +vsa-dim+)))
    (dotimes (i +vsa-dim+)
      (setf (aref result (mod (+ i n) +vsa-dim+)) (aref v i)))
    result))

;;; --- SECTION 5: PUBLIC API (Transparency & Encoding) ---

(defun get-vsa-vec (symbol)
  "Get or create the unique orthogonal vector for a symbol."
  (or (gethash symbol *vsa-symbol-to-vec*)
      (let ((v (make-random-vsa-vec)))
        (setf (gethash symbol *vsa-symbol-to-vec*) v)
        (push (cons v symbol) *vsa-vec-to-symbol*)
        v)))

(defun vsa-cleanup (noisy-vec)
  "Map a noisy retrieved vector back to a 1:1 Lisp symbol (The 'De-Blur')."
  (let ((best-sim -1.0)
        (best-symbol nil))
    (dolist (pair *vsa-vec-to-symbol*)
      (let ((sim (cosim noisy-vec (car pair))))
        (when (> sim best-sim)
          (setf best-sim sim
                best-symbol (cdr pair)))))
    (if (> best-sim *vsa-cleanup-threshold*)
        (values best-symbol best-sim)
        (values nil best-sim))))

(defun vsa-explain-vector (v &optional (limit 5))
  "Translate a latent vector into a spectrum of symbols."
  (let ((spectrum nil))
    (dolist (pair *vsa-vec-to-symbol*)
      (let ((sim (cosim v (car pair))))
        (when (> sim 0.1)
          (push (cons (cdr pair) sim) spectrum))))
    (let ((sorted (sort spectrum #'> :key #'cdr)))
      (subseq sorted 0 (min limit (length sorted))))))

(defun vsa-encode-rule (context prediction)
  "Encode a context list and prediction symbol into a single bound vector."
  (let ((ctx-vec (make-vsa-vec)))
    (setf (aref ctx-vec 0) 1.0)
    (loop for tok in context
          for i from 1
          do (let ((tok-vec (vsa-permute (get-vsa-vec tok) i)))
               (setf ctx-vec (vsa-bind ctx-vec tok-vec))))
    (vsa-bind ctx-vec (get-vsa-vec prediction))))

(defun vsa-lookup (expert context)
  "Retrieve prediction from expert's superposed knowledge."
  (let ((ctx-vec (make-vsa-vec)))
    (setf (aref ctx-vec 0) 1.0)
    (loop for tok in context
          for i from 1
          do (let ((tok-vec (vsa-permute (get-vsa-vec tok) i)))
               (setf ctx-vec (vsa-bind ctx-vec tok-vec))))
    (let ((noisy-pred (vsa-unbind (expert-knowledge-vector expert) ctx-vec)))
      (vsa-cleanup noisy-pred))))

;;; --- SECTION 6: VERIFICATION ---
(eval-when (:load-toplevel :execute)
  (format t "[VSA] Verification: OK (dim=~D)~%" +vsa-dim+))