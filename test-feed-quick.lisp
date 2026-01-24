(in-package :uhma)
(start! :demo nil)
(format t "~%=== TESTING FEED ===~%")

;; Test 1: text file
(format t "~%[TEST 1] Text file...~%")
(handler-case
    (ingest-file! #P"/home/peter/Desktop/STARWARS/FEED/test.txt" :force t)
  (error (e) (format t "ERROR: ~A~%" e)))

;; Test 2: code file
(format t "~%[TEST 2] Code file...~%")
(handler-case
    (ingest-file! #P"/home/peter/Desktop/STARWARS/FEED/sample-code.lisp" :force t)
  (error (e) (format t "ERROR: ~A~%" e)))

;; Test 3: ingest-feed
(format t "~%[TEST 3] Full feed scan...~%")
(ingest-feed! :force t)

(format t "~%=== FEED STATUS ===~%")
(feed-status)
