(load "load.lisp")
(uhma:start! :demo nil)

(defparameter *duration* 60) ;; Run for 60 seconds

(format t "~%Starting stress test for ~D seconds...~%" *duration*)

(defparameter *corpus* '(
    "consciousness emerges from integrated information"
    "attention selects relevant signals from noise"
    "working memory holds items for manipulation"
    "the brain constructs models of reality"
    "defun creates a new function"
    "lambda makes anonymous functions"
    "mapcar applies function to list"
    "the fog comes on little cat feet"
    "two roads diverged in yellow wood"
    "i wandered lonely as a cloud"
    "neurons transmit signals through synapses"
    "mitochondria generate cellular energy"
    "dna encodes genetic information"))

(let ((end-time (+ (get-universal-time) *duration*))
      (iterations 0))
  (loop while (< (get-universal-time) end-time) do
    (let ((sentence (nth (random (length *corpus*)) *corpus*)))
      (uhma:process-text! sentence)
      (incf iterations)
      (when (zerop (mod iterations 5))
        (format t ".")
        (force-output))
      (when (zerop (mod iterations 50))
        (format t " [~D] " iterations)
        (force-output))))
  (format t "~%Finished run. Total iterations: ~D~%" iterations))

(uhma:status)
(uhma:learning-progress)
(sb-ext:exit)
