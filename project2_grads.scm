#lang scheme
; project1_grads.scm
; This file is provided as part of project1, ONLY FOR GRADUATE STUDENTS.

(define (mydisplay value)
  (display value)
  (newline)
)

; Map_zips – When this function is called, it gets a list of zipcodes
; and returns the state(s) they belong to. The output should be pairs
; of zipcodes with their corresponding state abbreviations.
; If multiple zipcodes belong to the same state, group them into a single pair
; where the first element is the state abbreviation and the second element is a
; list of all zipcodes for that state.
(define (map_zips list)
  '()
)

(mydisplay (map_zips '(44286, 57634))) ; -> ((OH . 44286) (SD . 57634)) ; order doesn't matter
(mydisplay (map_zips '(99692, 99612, 93718))) ; -> ((KA . (99692 99612)) (CA . 93718)) ; order doesn't matter

