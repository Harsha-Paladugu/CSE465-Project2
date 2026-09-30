#lang scheme
;; lookups.scm - menu options 2, 3 and 6
;; Completed by Evan Bailey
;; Each record looks like (zipcode place state county latitude longitude).
;; These functions search the list with recursion and return the answer,
;; main.scm does the printing.
(provide find-by-zip find-by-place count-zips-in-state)
;; the first record with this zipcode, or #f if there is none
(define (find-by-zip zip lst)
  (cond ((null? lst) #f)
        ((= zip (car (car lst))) (car lst))
        (else (find-by-zip zip (cdr lst)))))
;; the first record with this place name (ignoring case), or #f if none
(define (find-by-place place lst)
  (cond ((null? lst) #f)
        ((string-ci=? place (cadr (car lst))) (car lst))
        (else (find-by-place place (cdr lst)))))
;; how many records belong to this state
(define (count-zips-in-state state lst)
  (cond ((null? lst) 0)
        ((string-ci=? state (caddr (car lst)))
         (+ 1 (count-zips-in-state state (cdr lst))))
        (else (count-zips-in-state state (cdr lst)))))
