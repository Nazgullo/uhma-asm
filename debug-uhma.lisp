(load "uhma-load-only.lisp")
(in-package :uhma)

(format t "Reading uhma.lisp...~%")
(with-open-file (s "uhma.lisp")
  (let ((text (make-string (file-length s))))
    (read-sequence text s)
    (format t "File read. Length: ~D. Processing chunks...~%" (length text))
    (loop for start from 0 below (length text) by 1000
          for i from 1
          do (let ((chunk (subseq text start (min (+ start 1000) (length text)))))
               (format t "Processing chunk ~D (~D chars)..." i (length chunk))
               (finish-output)
               (process-chunk! chunk :verbose nil)
               (format t " Done.~%")
               (finish-output)))))
(sb-ext:exit)