;;;; ============================================================================
;;;; UHMA FEED SYSTEM
;;;; ============================================================================
;;;; Universal file ingestion: text, code, PDF, images, books — anything.
;;;; Scans FEED/ subfolder, converts to token streams, feeds to process-text!.
;;;;
;;;; Usage:
;;;;   Place files in FEED/ directory, then:
;;;;   (ingest-feed!)                 ; Process all new files
;;;;   (ingest-feed! :force t)        ; Re-process everything
;;;;   (ingest-file! "path/to/file")  ; Process a single file
;;;;   (feed-status)                  ; Show what's been ingested
;;;;
;;;; Supported formats:
;;;;   Text:   .txt .md .org .csv .tsv .log .json .xml .html
;;;;   Code:   .lisp .cl .py .js .ts .c .h .cpp .rs .go .java .rb .sh .lua
;;;;   PDF:    .pdf (via pdftotext)
;;;;   Images: .png .jpg .jpeg .gif .bmp .tiff (via tesseract OCR)
;;;;   Books:  .epub (via unzip + html stripping)
;;;;   Data:   .dat .ndjson (line-by-line processing)
;;;; ============================================================================

(in-package :uhma)

;;; ============================================================================
;;; SECTION 1: CONFIGURATION
;;; ============================================================================

(defvar *feed-directory* nil
  "Path to the FEED/ directory. Set at load time.")

(defvar *feed-manifest* (make-hash-table :test 'equal)
  "Tracks ingested files: path → (list :ingested-at TIME :tokens N :accuracy F :size N)")

(defvar *feed-chunk-size* 2000
  "Characters per chunk when feeding large files.")

(defvar *feed-verbose* t
  "Print progress during ingestion.")

(defvar *feed-total-tokens* 0
  "Running total of tokens ingested through the feed system.")

(defvar *feed-total-files* 0
  "Running total of files ingested.")

;;; File type categories
(defparameter *text-extensions*
  '("txt" "md" "org" "csv" "tsv" "log" "json" "xml" "html" "htm"
    "yaml" "yml" "toml" "ini" "cfg" "conf" "rst" "tex" "srt" "vtt"))

(defparameter *code-extensions*
  '("lisp" "cl" "asd" "py" "js" "ts" "jsx" "tsx" "c" "h" "cpp" "hpp"
    "cc" "hh" "rs" "go" "java" "rb" "sh" "bash" "zsh" "lua" "pl" "pm"
    "el" "scm" "clj" "cljs" "hs" "ml" "mli" "scala" "kt" "swift"
    "r" "jl" "nim" "zig" "v" "sql" "css" "scss" "less"))

(defparameter *pdf-extensions* '("pdf"))

(defparameter *image-extensions* '("png" "jpg" "jpeg" "gif" "bmp" "tiff" "tif" "webp"))

(defparameter *epub-extensions* '("epub"))

;;; ============================================================================
;;; SECTION 2: UTILITY FUNCTIONS
;;; ============================================================================

(defun file-extension (path)
  "Get lowercase file extension from a path."
  (let* ((name (if (pathnamep path) (namestring path) path))
         (dot-pos (position #\. name :from-end t)))
    (when dot-pos
      (string-downcase (subseq name (1+ dot-pos))))))

(defun file-type-category (path)
  "Determine what category a file belongs to based on extension."
  (let ((ext (file-extension path)))
    (cond
      ((member ext *text-extensions* :test #'string-equal) :text)
      ((member ext *code-extensions* :test #'string-equal) :code)
      ((member ext *pdf-extensions* :test #'string-equal) :pdf)
      ((member ext *image-extensions* :test #'string-equal) :image)
      ((member ext *epub-extensions* :test #'string-equal) :epub)
      (t :unknown))))

(defun file-size-bytes (path)
  "Get file size in bytes."
  (with-open-file (s path :direction :input :element-type '(unsigned-byte 8)
                       :if-does-not-exist nil)
    (if s (file-length s) 0)))

(defun run-external (program args &key (timeout 60))
  "Run an external program, return its stdout as a string. Returns nil on failure."
  (declare (ignore timeout))
  (handler-case
      (let ((process (sb-ext:run-program program args
                                         :output :stream
                                         :error nil
                                         :wait t
                                         :search t)))
        (when process
          (let ((output (make-string-output-stream)))
            (with-open-stream (s (sb-ext:process-output process))
              (loop for line = (read-line s nil nil)
                    while line
                    do (write-string line output)
                       (write-char #\Newline output)))
            (let ((exit-code (sb-ext:process-exit-code process)))
              (if (zerop exit-code)
                  (get-output-stream-string output)
                  (progn
                    (when *feed-verbose*
                      (format t "   [FEED] External tool ~A exited with code ~D~%"
                              program exit-code))
                    nil))))))
    (error (e)
      (when *feed-verbose*
        (format t "   [FEED] External tool ~A failed: ~A~%" program e))
      nil)))

(defun strip-html-tags (html)
  "Remove HTML tags from a string, keeping text content."
  (let ((result (make-string-output-stream))
        (in-tag nil)
        (in-entity nil)
        (entity-buf (make-string-output-stream)))
    (loop for ch across html do
      (cond
        (in-entity
         (write-char ch entity-buf)
         (when (char= ch #\;)
           (setf in-entity nil)
           (let ((entity (get-output-stream-string entity-buf)))
             (cond
               ((string= entity "amp;") (write-char #\& result))
               ((string= entity "lt;") (write-char #\< result))
               ((string= entity "gt;") (write-char #\> result))
               ((string= entity "nbsp;") (write-char #\Space result))
               ((string= entity "quot;") (write-char #\" result))
               (t (write-char #\Space result))))))
        (in-tag
         (when (char= ch #\>) (setf in-tag nil)))
        ((char= ch #\<) (setf in-tag t))
        ((char= ch #\&)
         (setf in-entity t)
         (setf entity-buf (make-string-output-stream)))
        (t (write-char ch result))))
    (get-output-stream-string result)))

(defun normalize-whitespace (text)
  "Collapse multiple whitespace characters into single spaces."
  (let ((result (make-string-output-stream))
        (last-was-space nil))
    (loop for ch across text do
      (if (member ch '(#\Space #\Tab #\Newline #\Return #\Page))
          (unless last-was-space
            (write-char #\Space result)
            (setf last-was-space t))
          (progn
            (write-char ch result)
            (setf last-was-space nil))))
    (get-output-stream-string result)))

;;; ============================================================================
;;; SECTION 3: FORMAT-SPECIFIC EXTRACTORS
;;; ============================================================================

(defun extract-text-file (path)
  "Read a plain text file and return its content."
  (with-open-file (s path :direction :input :if-does-not-exist nil
                       :external-format :utf-8)
    (when s
      (let ((content (make-string-output-stream)))
        (loop for line = (read-line s nil nil)
              while line
              do (write-string line content)
                 (write-char #\Space content))
        (get-output-stream-string content)))))

(defun extract-code-file (path)
  "Read a code file. Preserves structure but normalizes for token processing.
   Comments become tokens. Identifiers become tokens. Operators become tokens."
  (with-open-file (s path :direction :input :if-does-not-exist nil
                       :external-format :utf-8)
    (when s
      (let ((content (make-string-output-stream))
            (ext (file-extension path)))
        ;; Add a file-type marker so the system knows what it's reading
        (format content "FILE-TYPE-~A " (string-upcase ext))
        (loop for line = (read-line s nil nil)
              while line
              do ;; Normalize: collapse multiple spaces, keep newlines as spaces
                 (write-string (string-trim '(#\Space #\Tab) line) content)
                 (write-char #\Space content))
        (get-output-stream-string content)))))

(defun extract-pdf (path)
  "Extract text from a PDF using pdftotext."
  (let ((text (run-external "pdftotext"
                            (list "-layout" "-enc" "UTF-8" (namestring path) "-"))))
    (when text
      (normalize-whitespace text))))

(defun extract-image-ocr (path)
  "Extract text from an image using tesseract OCR."
  (let ((text (run-external "tesseract"
                            (list (namestring path) "stdout" "-l" "eng"))))
    (when (and text (> (length text) 0))
      (normalize-whitespace text))))

(defun extract-epub (path)
  "Extract text from an EPUB by unzipping and reading HTML content files."
  (let ((tmpdir (format nil "/tmp/uhma-epub-~D/" (get-universal-time))))
    (unwind-protect
        (progn
          ;; Unzip epub to temp directory
          (run-external "unzip" (list "-o" "-q" (namestring path) "-d" tmpdir))
          ;; Find and read all HTML/XHTML files
          (let ((content (make-string-output-stream))
                (html-files nil))
            ;; Collect html/xhtml files from all subdirectories
            (dolist (pattern '("*.html" "*.xhtml" "*.htm"
                              "*/*.html" "*/*.xhtml" "*/*.htm"
                              "*/*/*.html" "*/*/*.xhtml" "*/*/*.htm"))
              (dolist (entry (directory (merge-pathnames pattern (pathname tmpdir))))
                (push entry html-files)))
            ;; Sort by name for chapter order
            (setf html-files (sort html-files #'string< :key #'namestring))
            ;; Extract text from each
            (dolist (hf html-files)
              (let ((html (extract-text-file hf)))
                (when html
                  (write-string (strip-html-tags html) content)
                  (write-char #\Space content))))
            (let ((result (get-output-stream-string content)))
              (when (> (length result) 0)
                (normalize-whitespace result)))))
      ;; Cleanup
      (run-external "rm" (list "-rf" tmpdir)))))

;;; ============================================================================
;;; SECTION 4: UNIFIED EXTRACTION DISPATCH
;;; ============================================================================

(defun extract-file-content (path)
  "Extract text content from any supported file type.
   Returns (values text-string file-category) or (values nil :unsupported)."
  (let ((category (file-type-category path)))
    (case category
      (:text
       (values (extract-text-file path) :text))
      (:code
       (values (extract-code-file path) :code))
      (:pdf
       (let ((text (extract-pdf path)))
         (if text
             (values text :pdf)
             (progn
               (when *feed-verbose*
                 (format t "   [FEED] PDF extraction failed for ~A~%" (file-namestring path)))
               (values nil :pdf-failed)))))
      (:image
       (let ((text (extract-image-ocr path)))
         (if (and text (> (length (string-trim '(#\Space #\Newline) text)) 5))
             (values text :image)
             (progn
               (when *feed-verbose*
                 (format t "   [FEED] No text extracted from image ~A~%" (file-namestring path)))
               (values nil :image-empty)))))
      (:epub
       (let ((text (extract-epub path)))
         (if text
             (values text :epub)
             (progn
               (when *feed-verbose*
                 (format t "   [FEED] EPUB extraction failed for ~A~%" (file-namestring path)))
               (values nil :epub-failed)))))
      (otherwise
       ;; Try reading as text anyway — might work for unknown extensions
       (let ((text (handler-case (extract-text-file path)
                     (error () nil))))
         (if (and text (> (length text) 0)
                  ;; Check if it looks like text (not binary)
                  (< (count-if (lambda (c) (and (not (graphic-char-p c))
                                                (not (member c '(#\Newline #\Tab #\Return)))))
                               (subseq text 0 (min 200 (length text))))
                     10))
             (values text :text-fallback)
             (values nil :unsupported)))))))

;;; ============================================================================
;;; SECTION 5: FEEDING PIPELINE
;;; ============================================================================

(defun feed-text-to-system! (text source-name &key (chunk-size *feed-chunk-size*))
  "Feed extracted text to the UHMA system in chunks.
   Returns (values total-tokens accuracy)."
  (when (or (null text) (zerop (length text)))
    (return-from feed-text-to-system! (values 0 0.0)))
  (let ((total-tokens 0)
        (total-correct 0)
        (total-verified 0)
        (pos 0)
        (len (length text))
        (chunk-num 0))
    (loop while (< pos len) do
      ;; Extract chunk, try to break at word boundary
      (let* ((end (min (+ pos chunk-size) len))
             (actual-end (if (>= end len)
                             end
                             ;; Find last space before chunk boundary
                             (let ((space-pos (position #\Space text
                                                       :start pos :end end :from-end t)))
                               (or space-pos end))))
             (chunk (subseq text pos actual-end)))
        (incf chunk-num)
        ;; Feed to system — robust against individual chunk errors
        (handler-case
            (multiple-value-bind (acc tokens verified skipped)
                (process-chunk! chunk :verbose nil)
              (declare (ignore skipped))
              (incf total-tokens tokens)
              (incf total-verified verified)
              (incf total-correct (round (* acc (max 1 verified)))))
          (error (e)
            ;; Log but continue with next chunk
            (when *feed-verbose*
              (format t "   [~A] chunk ~D error (skipping): ~A~%"
                      source-name chunk-num e))
            ;; Count tokens approximately so we don't lose track
            (incf total-tokens (count #\Space chunk))))
        ;; Progress reporting for large files
        (when (and *feed-verbose* (zerop (mod chunk-num 25)))
          (format t "   [~A] chunk ~D (~D tokens, acc=~,1F%)~%"
                  source-name chunk-num total-tokens
                  (if (> total-verified 0)
                      (* 100.0 (/ total-correct total-verified))
                      0.0)))
        (setf pos actual-end)))
    (let ((accuracy (if (> total-verified 0)
                        (float (/ total-correct total-verified))
                        0.0)))
      (values total-tokens accuracy))))

;;; ============================================================================
;;; SECTION 6: FILE INGESTION
;;; ============================================================================

(defun ingest-file! (path &key force)
  "Ingest a single file into the system.
   Skips if already ingested unless :force t."
  (let* ((path (if (pathnamep path) path (pathname path)))
         (key (namestring (truename path))))
    ;; Skip if already ingested
    (when (and (not force) (gethash key *feed-manifest*))
      (when *feed-verbose*
        (format t "   [SKIP] ~A (already ingested)~%" (file-namestring path)))
      (return-from ingest-file! nil))
    ;; Extract content
    (multiple-value-bind (text category) (extract-file-content path)
      (unless text
        (when *feed-verbose*
          (format t "   [SKIP] ~A (~A)~%" (file-namestring path) category))
        (return-from ingest-file! nil))
      ;; Feed to system
      (let ((name (file-namestring path)))
        (when *feed-verbose*
          (format t "~% [FEED] ~A (~A, ~:D bytes)~%"
                  name category (file-size-bytes path)))
        (multiple-value-bind (tokens accuracy)
            (feed-text-to-system! text name)
          ;; Record in manifest
          (setf (gethash key *feed-manifest*)
                (list :ingested-at (get-universal-time)
                      :tokens tokens
                      :accuracy accuracy
                      :size (file-size-bytes path)
                      :category category
                      :name name))
          (incf *feed-total-tokens* tokens)
          (incf *feed-total-files*)
          (when *feed-verbose*
            (format t "   [DONE] ~A: ~:D tokens, acc=~,1F%~%"
                    name tokens (* 100 accuracy)))
          (values tokens accuracy))))))

;;; ============================================================================
;;; SECTION 7: DIRECTORY SCANNING
;;; ============================================================================

(defun scan-feed-directory (&key include-subdirs)
  "Scan the FEED/ directory and return list of file paths.
   Optionally recurse into subdirectories."
  (unless (and *feed-directory* (probe-file *feed-directory*))
    (ensure-feed-directory!)
    (unless (probe-file *feed-directory*)
      (format t "[FEED] No FEED/ directory found at ~A~%" *feed-directory*)
      (return-from scan-feed-directory nil)))
  (let ((files nil))
    ;; Collect files from the feed directory
    (dolist (entry (directory (merge-pathnames "*.*" *feed-directory*)))
      (when (pathname-type entry)
        (push entry files)))
    ;; Optionally scan subdirectories
    (when include-subdirs
      (dolist (subdir (directory (merge-pathnames "*/" *feed-directory*)))
        (dolist (entry (directory (merge-pathnames "*.*" subdir)))
          (when (pathname-type entry)
            (push entry files)))))
    ;; Sort by name for consistent ordering
    (sort files #'string< :key #'namestring)))

(defun ensure-feed-directory! ()
  "Create the FEED/ directory if it doesn't exist."
  (unless (probe-file *feed-directory*)
    (ensure-directories-exist (merge-pathnames "dummy" *feed-directory*))
    (format t "[FEED] Created ~A~%" *feed-directory*)))

;;; ============================================================================
;;; SECTION 8: MAIN API
;;; ============================================================================

(defun ingest-feed! (&key force include-subdirs (passes 1))
  "Scan FEED/ directory and ingest all files.
   :force t to re-process already ingested files.
   :passes N to run N epochs over the same content (implies force on pass 2+).
   :include-subdirs t to recurse into subdirectories."
  (let ((files (scan-feed-directory :include-subdirs include-subdirs))
        (grand-total-tokens 0))
    (unless files
      (format t "~%No files found in FEED/ directory.~%")
      (format t "Place files to ingest in: ~A~%" *feed-directory*)
      (return-from ingest-feed! nil))
    (dotimes (pass passes)
      (let ((pass-force (or force (> pass 0)))
            (processed 0)
            (skipped 0)
            (failed 0)
            (total-new-tokens 0)
            (start-time (get-internal-real-time)))
        (format t "~%========================================~%")
        (format t "UHMA FEED INGESTION~:[~; (pass ~D/~D)~]~%"
                (> passes 1) (1+ pass) passes)
        (format t "========================================~%")
        (format t "Directory: ~A~%" *feed-directory*)
        (format t "Found ~D file~:P to process.~%~%" (length files))
        (dolist (file files)
          (let ((errored nil))
            (multiple-value-bind (tokens accuracy)
                (handler-case (ingest-file! file :force pass-force)
                  (error (e)
                    (format t "   [ERROR] ~A: ~A~%" (file-namestring file) e)
                    (incf failed)
                    (setf errored t)
                    (values nil nil)))
              (declare (ignore accuracy))
              (unless errored
                (cond
                  ((null tokens) (incf skipped))
                  (t (incf processed)
                     (incf total-new-tokens tokens)))))))
        (incf grand-total-tokens total-new-tokens)
        (let ((elapsed (/ (- (get-internal-real-time) start-time)
                          (float internal-time-units-per-second))))
          (format t "~%  Pass ~D: ~D file~:P, ~:D tokens, ~,1Fs~%"
                  (1+ pass) processed total-new-tokens elapsed)
          (when (> failed 0)
            (format t "  Failed: ~D~%" failed)))))
    ;; Final summary
    (format t "~%========================================~%")
    (format t "INGESTION COMPLETE~%")
    (format t "  Passes: ~D~%" passes)
    (format t "  Total tokens this run: ~:D~%" grand-total-tokens)
    (format t "  Lifetime total: ~:D tokens across ~D file~:P~%"
            *feed-total-tokens* *feed-total-files*)
    (format t "========================================~%")
    (values passes grand-total-tokens)))

(defun feed-status ()
  "Print status of the feed system."
  (format t "~%=== FEED STATUS ===~%")
  (format t "Directory: ~A~%" *feed-directory*)
  (format t "Total ingested: ~D file~:P, ~:D tokens~%"
          *feed-total-files* *feed-total-tokens*)
  (format t "~%Ingested files:~%")
  (let ((entries nil))
    (maphash (lambda (k v) (push (cons k v) entries)) *feed-manifest*)
    (setf entries (sort entries #'string< :key #'car))
    (dolist (entry entries)
      (let ((info (cdr entry)))
        (format t "  ~A (~A) ~:D tokens, acc=~,1F%~%"
                (getf info :name)
                (getf info :category)
                (getf info :tokens 0)
                (* 100 (getf info :accuracy 0.0))))))
  ;; Show pending files
  (let* ((all-files (scan-feed-directory :include-subdirs t))
         (pending (when all-files
                    (remove-if (lambda (f) (gethash (namestring (truename f)) *feed-manifest*))
                               all-files))))
    (when pending
      (format t "~%Pending (~D):~%" (length pending))
      (dolist (f (subseq pending 0 (min 10 (length pending))))
        (format t "  ~A (~A)~%" (file-namestring f) (file-type-category f)))
      (when (> (length pending) 10)
        (format t "  ... and ~D more~%" (- (length pending) 10)))))
  (format t "===================~%"))

(defun clear-feed-manifest! ()
  "Clear the ingestion manifest, allowing all files to be re-processed."
  (clrhash *feed-manifest*)
  (setf *feed-total-tokens* 0
        *feed-total-files* 0)
  (format t "[FEED] Manifest cleared.~%"))

;;; ============================================================================
;;; SECTION 9: CONVENIENCE FUNCTIONS
;;; ============================================================================

(defun feed-file! (path)
  "Convenience: ingest a single file by path (relative to working directory)."
  (let ((full-path (merge-pathnames path cl-user::*module-base*)))
    (if (probe-file full-path)
        (ingest-file! full-path :force t)
        (format t "[FEED] File not found: ~A~%" full-path))))

(defun feed-string! (text &optional (source-name "string-input"))
  "Convenience: feed a raw string directly."
  (feed-text-to-system! text source-name))

(defun feed-own-source! (&key (passes 1))
  "Feed the system its own source code. The system consuming itself.
   :passes N to run N epochs over the codebase (default 1)."
  (let ((lisp-files (directory (merge-pathnames "*.lisp" cl-user::*module-base*)))
        (grand-total 0))
    (let ((files (sort (remove-if
                        (lambda (f)
                          (or (search "test-" (file-namestring f))
                              (search "uhma-state" (file-namestring f))))
                        lisp-files)
                       #'string< :key #'namestring)))
      (format t "~%[FEED] Self-consumption: ~D files, ~D ~:[pass~;passes~]~%"
              (length files) passes (> passes 1))
      (dotimes (pass passes)
        (let ((pass-tokens 0)
              (start-time (get-internal-real-time)))
          (when (> passes 1)
            (format t "~%--- Pass ~D/~D ---~%" (1+ pass) passes))
          (dolist (f files)
            (multiple-value-bind (tokens acc)
                (ingest-file! f :force (or (> pass 0) (> passes 1)))
              (declare (ignore acc))
              (when tokens
                (incf pass-tokens tokens))))
          (incf grand-total pass-tokens)
          (let ((elapsed (/ (- (get-internal-real-time) start-time)
                            (float internal-time-units-per-second))))
            (format t "[FEED] Pass ~D: ~:D tokens (~,1Fs)~%" (1+ pass) pass-tokens elapsed)))))
    (format t "[FEED] Self-consumption complete: ~:D total tokens (~D ~:[pass~;passes~])~%"
            grand-total passes (> passes 1))
    grand-total))

;;; ============================================================================
;;; SECTION 10: INITIALIZATION
;;; ============================================================================

(eval-when (:load-toplevel :execute)
  ;; Set feed directory relative to the project root
  (setf *feed-directory*
        (merge-pathnames (make-pathname :directory '(:relative "FEED"))
                         cl-user::*module-base*))
  ;; Ensure it exists
  (ensure-feed-directory!)
  (format t "[FEED] Ingestion system ready.~%")
  (format t "   Directory: ~A~%" *feed-directory*)
  (format t "   Formats: text, code, PDF, images (OCR), EPUB~%")
  (format t "   API: (ingest-feed!)  (feed-status)  (feed-own-source!)~%"))
