(load "load.lisp")
(in-package :uhma)

(format t "~%--- DEBUGGING FAILURES ---~%")

(start! :demo nil)
(process-text! "The cat sat on the mat.")

(format t "1. Self-Model Check:~%")
(if *self-model*
    (progn
      (format t "   *self-model* exists.~%")
      (format t "   Total verified: ~A~%" (self-model-total-verified *self-model*))
      (format t "   Total skipped: ~A~%" (self-model-total-skipped *self-model*)))
    (format t "   *self-model* is NIL!~%"))

(format t "~%2. Phase Check:~%")
(if (boundp '*phase*)
    (format t "   *phase* is bound to: ~A~%" *phase*)
    (format t "   *phase* is UNBOUND!~%"))

(format t "~%3. Checking Hook Registration:~%")
(format t "   +hook-should-verify+ handlers: ~A~%" (gethash +hook-should-verify+ *hooks*))

(sb-ext:quit)
