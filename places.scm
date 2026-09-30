#lang scheme
;; places.scm - menu options 4 and 5
;; Each record looks like (zipcode place state county latitude longitude),
;; so (cadr record) is the place and (caddr record) is the state.
;; These functions just return lists, main.scm does the printing.
(provide states-for-place common-places)

;; true if item is in lst (ignoring upper/lower case)
(define (contains? item lst)
  (cond ((null? lst) #f)
        ((string-ci=? item (car lst)) #t)
        (else (contains? item (cdr lst)))))

;; add item to the front of lst, unless it is already in there.
;; this is how we keep duplicates out of the results.
(define (add-unique item lst)
  (if (contains? item lst)
      lst
      (cons item lst)))

;; option 4: all the states that have a place with this name
(define (states-for-place place lst)
  (cond ((null? lst) '())
        ((string-ci=? place (cadr (car lst)))
         (add-unique (caddr (car lst)) (states-for-place place (cdr lst))))
        (else (states-for-place place (cdr lst)))))

;; all the different place names in one state
(define (places-in-state state lst)
  (cond ((null? lst) '())
        ((string-ci=? state (caddr (car lst)))
         (add-unique (cadr (car lst)) (places-in-state state (cdr lst))))
        (else (places-in-state state (cdr lst)))))

;; keep only the places that are also in other-places
(define (keep-shared places other-places)
  (cond ((null? places) '())
        ((contains? (car places) other-places)
         (cons (car places) (keep-shared (cdr places) other-places)))
        (else (keep-shared (cdr places) other-places))))

;; option 5: the places that are in every one of the given states.
;; get the places of the first state, then keep only the ones that are
;; also common to the rest of the states. when only one state is left,
;; the answer is just that state's places.
(define (common-places states lst)
  (cond ((null? states) '())
        ((null? (cdr states)) (places-in-state (car states) lst))
        (else (keep-shared (places-in-state (car states) lst)
                           (common-places (cdr states) lst)))))
