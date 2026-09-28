#lang scheme
; run_funcs.scm
; This file is provided as part of project1.



; Students are to implement all function here and
; display the results in a clear, labeled way so
; it is obvious which output belongs to which expression
(define (mydisplay value)
  (display value)
  (newline)
)

; #1- 'select' function:
; The select function returns a new list holding only elements
; of the givel list that are in the range of given pair
(define (select range list)
  '()
)

; You can change these displays to make them look nicer
(mydisplay (select '(1 . 2) '(-1 1 2 3 4 -4 5))) ; -> (1 2)
(mydisplay (select '(-1 . 3) '(-1 1 1 2 3 4 -4 5))) ; -> (-1 1 1 2 3)
(mydisplay (select '(8 . 9) '(-1 1 1 2 3 4 -4 5))) ; -> ()
(mydisplay (select '(3 . 1) '(-1 1 1 2 3 4 -4 5))) ; -> Error

; #2- 'flatten' function:
; Returns a list identical to the first list, while having all elements
; that are inside nested loops taken out. So we want to flatten all elements and have
; them all in a single list. For example ’(a (a a) a))) should become (a a a a)
(define (flatten lst)
  '()
)
(mydisplay (flatten '("a" "b" "c")))  ; -> (a b c)
(mydisplay (flatten '("a" ("a" "a") "a")))  ; -> (a a a a)
(mydisplay (flatten '(("a" "b") ("c" ("d") "e") "f")))  ; -> (a b c d e f)

; #3- 'crossproduct' function:
; The parameters are two lists. The result should contain the cross product
; between the two lists:
; The inputs ’(1 2) and ’(a b c) should return a single list:
; ((1 a) (1 b) (1 c) (2 a) (2 b) (2 c))
; lst1 & lst2 – two flat lists.
(define (crossproduct lst1 lst2)
  '()
)
(mydisplay (crossproduct '(1 2) '("a" "b" "c"))) ; -> ((1 a) (1 b) (1 c) (2 a) (2 b) (2 c))
(mydisplay (crossproduct '(1 2 "j") '(5 -1))) ; -> ((1 5) (1 -1) (2 5) (2 -1) (j 5) (j -1))