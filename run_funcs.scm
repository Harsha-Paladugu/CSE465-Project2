#lang scheme
; run_funcs.scm
; This file is provided as part of Project 2.
;
; Menu option 1 ("Show results") calls run-results, which runs every test
; expression at the bottom of this file and prints each expression next to
; its result, so it is obvious which output belongs to which expression.
;
; Nothing is printed when main.scm requires this file. Results only appear
; when (run-results) is called, or when this file is run on its own.
;
; All three functions use plain recursion (cond / car / cdr / cons).
; No looping constructs are used.

(provide run-results select flatten crossproduct)

; mydisplay : any -> void
; Print a value followed by a newline.
(define (mydisplay value)
  (display value)
  (newline))

; show : string any -> void
; Print the expression text, an arrow, and its result on one line.
; Example output:  (select '(1 . 2) '(-1 1 2 3 4 -4 5)) => (1 2)
(define (show label result)
  (display label)
  (display " => ")
  (mydisplay result))


; ============================================================
; #1- 'select' function:
; The select function returns a new list holding only elements
; of the given list that are in the range of the given pair.
; range is a pair (low . high).
; If low is greater than high the range is invalid, so an error
; message is returned instead of a list.
; ============================================================
(define (select range lst)
  (cond ((> (car range) (cdr range)) "Error: invalid range (low > high)")
        (else (keep-in-range (car range) (cdr range) lst))))

; keep-in-range : number number list -> list
; Walk down lst one element at a time.
; Keep the element when low <= element <= high, otherwise skip it.
(define (keep-in-range low high lst)
  (cond ((null? lst) '())
        ((<= low (car lst) high)
         (cons (car lst) (keep-in-range low high (cdr lst))))
        (else (keep-in-range low high (cdr lst)))))


; ============================================================
; #2- 'flatten' function:
; Returns a list identical to the first list, while having all elements
; that are inside nested lists taken out. So we want to flatten all
; elements and have them all in a single list.
; For example '(a (a a) a) should become (a a a a)
; ============================================================
(define (flatten lst)
  (cond ((null? lst) '())
        ; first element is itself a list:
        ; flatten it, flatten the rest, then join the two results
        ((list? (car lst))
         (append (flatten (car lst)) (flatten (cdr lst))))
        ; first element is a plain value: keep it and flatten the rest
        (else (cons (car lst) (flatten (cdr lst))))))


; ============================================================
; #3- 'crossproduct' function:
; The parameters are two lists. The result should contain the cross
; product between the two lists:
; The inputs '(1 2) and '(a b c) should return a single list:
; ((1 a) (1 b) (1 c) (2 a) (2 b) (2 c))
; lst1 & lst2 - two flat lists.
; ============================================================
(define (crossproduct lst1 lst2)
  (cond ((null? lst1) '())
        ; pair the first element of lst1 with every element of lst2,
        ; do the same for the rest of lst1, then join the two results
        (else (append (pair-with (car lst1) lst2)
                      (crossproduct (cdr lst1) lst2)))))

; pair-with : any list -> list of 2-element lists
; Pair one item with every element of lst.
; Example: (pair-with 1 '(a b)) -> ((1 a) (1 b))
(define (pair-with item lst)
  (cond ((null? lst) '())
        (else (cons (list item (car lst))
                    (pair-with item (cdr lst))))))


; ============================================================
; run-results : -> void
; Run every test expression and print it next to its result.
; main.scm calls this for menu option 1.
; ============================================================
(define (run-results)
  (mydisplay "===== #1 - select =====")
  (show "(select '(1 . 2) '(-1 1 2 3 4 -4 5))"    (select '(1 . 2) '(-1 1 2 3 4 -4 5)))    ; -> (1 2)
  (show "(select '(-1 . 3) '(-1 1 1 2 3 4 -4 5))" (select '(-1 . 3) '(-1 1 1 2 3 4 -4 5))) ; -> (-1 1 1 2 3)
  (show "(select '(8 . 9) '(-1 1 1 2 3 4 -4 5))"  (select '(8 . 9) '(-1 1 1 2 3 4 -4 5)))  ; -> ()
  (show "(select '(3 . 1) '(-1 1 1 2 3 4 -4 5))"  (select '(3 . 1) '(-1 1 1 2 3 4 -4 5)))  ; -> Error
  (newline)

  (mydisplay "===== #2 - flatten =====")
  (show "(flatten '(a b c))"                 (flatten '("a" "b" "c")))                 ; -> (a b c)
  (show "(flatten '(a (a a) a))"             (flatten '("a" ("a" "a") "a")))           ; -> (a a a a)
  (show "(flatten '((a b) (c (d) e) f))"     (flatten '(("a" "b") ("c" ("d") "e") "f"))) ; -> (a b c d e f)
  (newline)

  (mydisplay "===== #3 - crossproduct =====")
  (show "(crossproduct '(1 2) '(a b c))"     (crossproduct '(1 2) '("a" "b" "c")))     ; -> ((1 a) (1 b) (1 c) (2 a) (2 b) (2 c))
  (show "(crossproduct '(1 2 j) '(5 -1))"    (crossproduct '(1 2 "j") '(5 -1)))        ; -> ((1 5) (1 -1) (2 5) (2 -1) (j 5) (j -1))
  (newline))

; When this file is run directly (e.g. the Run button in DrRacket), show
; the results right away. When main.scm requires this file, this line
; does nothing, so the menu stays in control of when results are shown.
(module+ main (run-results))
