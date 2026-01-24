;;;; ============================================================================
;;;; UHMA HOLOGRAPHIC CONVERGENCE (Bible v3.0 Compliant)
;;;; ============================================================================
;;;; Implements the "Substrate Inversion": Unifying all symbolic logs into VSA.
;;;; ============================================================================

(in-package :uhma)

;;; --- SECTION 8: CONVERGENCE LOGIC ---

(defun vibe-context-with-self (ctx key)
  "Intuition sensor: Returns resonance strength between context and symbolic keys."
  (declare (type list ctx) (type symbol key))
  (let ((current (holo-compute-activations ctx))
        (target (case key
                  (:danger (holo-compute-activations '(:error :failure :stuck :crisis)))
                  (:growth (holo-compute-activations '(:learning :insight :discovery)))
                  (otherwise (holo-compute-activations (list key))))))
    (holo-similarity current target)))

(defun fold-cognitive-event! (type content &key ctx layer)
  "Single entry point for persisting any cognitive event into the holographic store."
  (declare (type symbol type) (type (or null list) ctx))
  (let ((ctype (case type (:prediction :outcome) (:action :working) (otherwise type))))
    (holo-store! (holo-encode content :layer (or layer :immediate) :content-type ctype :ctx ctx))))

;;; --- SECTION 10: INITIALIZATION ---
(eval-when (:load-toplevel :execute)
  (register-hook +hook-maintenance+ 'holo-maintenance! :priority 10)
  (format t "[HOLO-CONVERGENCE] Substrate inversion active.~%"))

(format t "~%================================================================~%")
(format t "UHMA HOLOGRAPHIC CONVERGENCE ACTIVE~%")
(format t "================================================================~%")
(format t "Scattered logs are now being unified into the holographic substrate.~%")
(format t "Cross-modal intuition enabled via (vibe-context-with-self).~%")
(format t "================================================================~%")
