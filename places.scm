;; ============================================================
;; Responsibility: Menu options 4 and 5.
;; Dataset record:
;;   (zipcode place state county latitude longitude)
;; Use recursion for list processing. Return results without printing.
;; Export the two public functions with provide.
;; ============================================================

;; states-for-place : string list -> list of strings
;; Return every distinct state containing place.
;; Compare place names without regard to case.
;; Do not include duplicate states.
;; Return '() if the place is absent. Result order does not matter.

;; common-places : list of strings list -> list of strings
;; The first argument contains at least two state abbreviations.
;; Return place names found in EVERY requested state.
;; Do not include duplicate place names.
;; Return '() if there are no common places. Result order does not matter.
