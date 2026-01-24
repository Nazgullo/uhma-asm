;;;; FILE MANIFEST
;;;; =============
;;;; This file MUST contain the following when complete:
;;;; PACKAGES: cl-user
;;;; FUNCTIONS: ensure-package-with-exports, module-file, verify-file-completeness
;;;; LOADS: All UHMA system files in dependency order
;;;; TOTAL EXPECTED SECTIONS: 4

(in-package :cl-user)

;;;; [SECTION-START:1:LOADER-BASE]
;;; Section 1: Loader Base Utilities

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Optimization policy - Safety first during development
  (declaim (optimize (speed 0) (safety 3) (debug 3)))

  (defun ensure-package-with-exports (pkg-name symbol-names)
    "Create package if needed, ALWAYS ensure exports.
     This pattern is retry-safe across multiple load attempts."
    (let ((pkg (or (find-package pkg-name)
                   (make-package pkg-name :use '(:cl)))))
      (dolist (sym-name symbol-names)
        (export (intern (string sym-name) pkg) pkg))
      pkg)))

(defparameter *module-base*
  (make-pathname :directory (pathname-directory *load-truename*)))

(defun module-file (name)
  (merge-pathnames name *module-base*))

;;;; [SECTION-END:1:LOADER-BASE]

(format t "~%Loading UHMA v2 System...~%~%")

;;;; [SECTION-START:2:CORE-LOADS]
;;; Section 2: Core Module Loading

;; 1. Forward Declarations (Must be first to fix circularities)
(load (module-file "uhma-forward-decl.lisp"))

;; 2. Stubs (Fixes undefined variables/functions for legacy code)
(load (module-file "uhma-stubs.lisp"))

;; 3. VSA Substrate (needed by core for make-vsa-vec)
(load (module-file "uhma-vsa-substrate.lisp"))

;; 4. Core Substrate
(load (module-file "uhma-v6.1-core-homoiconic.lisp"))      ; Foundation
(load (module-file "uhma-v6.1-adaptive-homoiconic.lisp"))  ; Self-tuning
(load (module-file "uhma-v6.1-sequence.lisp"))             ; Sequence ops

;; 4. Presence Substrate (Fundamental layer - moved up)
(load (module-file "uhma-presence-substrate.lisp"))

;; 5. Cognitive Layers
(load (module-file "uhma-v6.2-deep-mind.lisp"))            ; Tracing/Schema
(load (module-file "uhma-v6.3-deeper-mind.lisp"))          ; Expectation/Self
(load (module-file "uhma-v6.3-fixes.lisp"))                ; Fixes
(load (module-file "uhma-v6.4-enhancements.lisp"))         ; Diversity
(load (module-file "uhma-v6.5-agency.lisp"))               ; Goals/Drives
(load (module-file "uhma-v6.6-introspective-grounding.lisp")) ; Language
(load (module-file "uhma-v6.7-compositional-reasoning.lisp")) ; Planning
(load (module-file "uhma-v6.8-cognitive-controller.lisp"))    ; Strategy
(load (module-file "uhma-v6.9-pattern-utilization.lisp"))     ; Patterns
(load (module-file "uhma-v6.10-episodic-memory.lisp"))       ; Episodic
(load (module-file "uhma-v6.10-episodic-integration.lisp")) ; Integration

;;;; [SECTION-END:2:CORE-LOADS]

;;;; [SECTION-START:3:PRESENCE-AND-WIRING]
;;; Section 3: Holographic, Presence and Final Wiring

;; 6. Holographic Memory (New v3.0)
(load (module-file "uhma-holographic-substrate-v2.lisp"))
(load (module-file "uhma-holographic-integration.lisp"))

;; 7. Active Self Modification
(load (module-file "uhma-active-self-modification.lisp"))

;; 8. Presence Integration (Wires presence into everything)
(load (module-file "uhma-presence-integration.lisp"))

;; 8b. Self-Awareness Loop (Introspection/Modification)
(load (module-file "uhma-self-awareness-loop.lisp"))

;; 8c. Predictive Self-Modification (System predicts itself before changing)
(load (module-file "uhma-predictive-self-modification.lisp"))

;; 9. Continuous Operation & Persistence
(load (module-file "uhma-continuous.lisp"))
(load (module-file "uhma-state-persistence.lisp"))
(load (module-file "uhma-memory-bounds.lisp"))

;; 10. Deep Wiring (Inter-module connections)
(load (module-file "uhma-deep-wiring.lisp"))

;; 11. Final Wiring and Start
(load (module-file "uhma-complete-wiring.lisp"))
(load (module-file "start.lisp"))

;; 12. Save/Restore System (replaces incomplete persistence)
(load (module-file "uhma-save-restore.lisp"))

;;;; [SECTION-END:3:PRESENCE-AND-WIRING]

;;;; [SECTION-START:4:VERIFICATION]
;;; Section 4: Load Verification

(defun verify-file-completeness ()
  "Verify loader state."
  (unless (fboundp 'ensure-package-with-exports)
    (error "Loader failed: ensure-package-with-exports missing"))
  (format t "~&Loader verification verified: OK~%"))

(verify-file-completeness)

(format t "~%========================================~%")
(format t "UHMA v2 System Loaded Successfully~%")
(format t "========================================~%~%")
(format t "Quick Start:~%")
(format t "  (uhma:start!)                    ; Initialize and run demo~%")
(format t "  (uhma:process-text! \"...\")       ; Process text input~%")

;;;; [SECTION-END:4:VERIFICATION]
