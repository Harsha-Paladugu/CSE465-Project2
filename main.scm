;; ============================================================
;; Responsibility: Run the interactive menu for options 1-6.
;; Import zipcodes.scm, lookups.scm, places.scm, and run_funcs.scm.
;; Use recursion to show the menu again after an option completes.
;; ============================================================

;; Input:
;;   Read menu responses as lines.
;;   Convert zipcode input to a number; a leading zero may disappear
;;   internally because the dataset stores zipcodes as numbers.
;;   Read place names as plain text, including spaces.
;;   Read two or more state abbreviations separated by spaces.
;;   Validate input before calling a data function.

;; Output:
;;   Print clear labels for all six fields of a matching record.
;;   Display zipcodes as five digits, including leading zeroes.
;;   Display an explanatory message for a missing record, no states,
;;   no common places, or a state with zero zipcode entries.
;;   Call run-results each time option 1 is selected.
