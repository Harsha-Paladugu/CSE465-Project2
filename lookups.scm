#lang scheme
;; ============================================================
;; Responsibility: Menu options 2, 3, and 6.
;; Dataset record:
;;   (zipcode place state county latitude longitude)
;; The dataset is passed as the second argument to each function.
;; Use recursion for searches and counting.
;; Export the three public functions with provide.
;; ============================================================
(provide find-by-zip find-by-place count-zips-in-state)

;; find-by-zip : number list -> record or #f
;; Return the FIRST record whose numeric zipcode equals zip.
;; Return #f if no record matches. Do not print from this function.
(define (find-by-zip zip lst)
  (cond ((null? lst) #f)
        ((= zip (car (car lst))) (car lst))
        (else (find-by-zip zip (cdr lst)))))

;; find-by-place : string list -> record or #f
;; Return the FIRST record whose place matches place, ignoring case.
;; Return #f if no record matches. Do not print from this function.
(define (find-by-place place lst)
  (cond ((null? lst) #f)
        ((string-ci=? place (cadr (car lst))) (car lst))
        (else (find-by-place place (cdr lst)))))

;; count-zips-in-state : string list -> nonnegative integer
;; Count all records whose state matches state.
;; Return 0 when there are no matches. Do not print from this function.
(define (count-zips-in-state state lst)
  (cond ((null? lst) 0)
        ((string-ci=? state (caddr (car lst)))
         (+ 1 (count-zips-in-state state (cdr lst))))
        (else (count-zips-in-state state (cdr lst)))))


