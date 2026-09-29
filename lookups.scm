;; ============================================================
;; Responsibility: Menu options 2, 3, and 6.
;; Dataset record:
;;   (zipcode place state county latitude longitude)
;; The dataset is passed as the second argument to each function.
;; Use recursion for searches and counting.
;; Export the three public functions with provide.
;; ============================================================

;; find-by-zip : number list -> record or #f
;; Return the FIRST record whose numeric zipcode equals zip.
;; Return #f if no record matches. Do not print from this function.

;; find-by-place : string list -> record or #f
;; Return the FIRST record whose place matches place, ignoring case.
;; Return #f if no record matches. Do not print from this function.

;; count-zips-in-state : string list -> nonnegative integer
;; Count all records whose state matches state.
;; Return 0 when there are no matches. Do not print from this function.
