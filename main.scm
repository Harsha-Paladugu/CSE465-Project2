#lang scheme
;; ============================================================
;; CSE 465 - Project 2 : Zipcode Explorer
;;
;; Group members and what each person did:
;;   Harsha Paladugu - main.scm (the menu, user input and output),
;;                     run_funcs.scm (select, flatten, crossproduct)
;;                     and places.scm (options 4 and 5)
;;   Evan Bailey     - lookups.scm (find-by-zip, find-by-place and
;;                     count-zips-in-state for options 2, 3 and 6)
;;
;; A menu program that works on the zipcodes.scm dataset.
;; Each record in the dataset looks like:
;;   (zipcode place state county latitude longitude)
;; Everything is written with recursion, there are no loops.
;; ============================================================

(require "zipcodes.scm"    ; the zipcodes list
         "lookups.scm"     ; find-by-zip, find-by-place, count-zips-in-state
         "places.scm"      ; states-for-place, common-places
         "run_funcs.scm")  ; run-results


;; ---------- small helpers ----------

;; print something and go to the next line
(define (print-line x)
  (display x)
  (newline))

;; show a question and read the user's answer (without spaces around it).
;; if there is no more input, return "0" so the menu just exits.
(define (ask question)
  (display question)
  (flush-output)   ; make sure the question shows up before we wait
  (let ((line (read-line)))
    (if (eof-object? line)
        "0"
        (string-trim line))))

;; print every item in the list on its own line
(define (print-list lst)
  (cond ((null? lst) (void))
        (else (display "  - ")
              (print-line (car lst))
              (print-list (cdr lst)))))

;; zipcodes are stored as numbers, so 6404 is really 06404.
;; add zeros to the front until it is 5 characters long.
(define (add-zeros str)
  (if (< (string-length str) 5)
      (add-zeros (string-append "0" str))
      str))

;; print one record with a label in front of each field
(define (print-record r)
  (display "  Zipcode:   ") (print-line (add-zeros (number->string (list-ref r 0))))
  (display "  Place:     ") (print-line (list-ref r 1))
  (display "  State:     ") (print-line (list-ref r 2))
  (display "  County:    ") (print-line (list-ref r 3))
  (display "  Latitude:  ") (print-line (list-ref r 4))
  (display "  Longitude: ") (print-line (list-ref r 5)))

;; true if every character in str is a digit
(define (all-digits? str)
  (cond ((string=? str "") #t)
        ((char-numeric? (string-ref str 0)) (all-digits? (substring str 1)))
        (else #f)))

;; a zipcode has to be exactly 5 digits
(define (valid-zip? str)
  (and (= (string-length str) 5) (all-digits? str)))

;; upper-case every string in the list, so "oh" works the same as "OH"
(define (upcase-all lst)
  (cond ((null? lst) '())
        (else (cons (string-upcase (car lst)) (upcase-all (cdr lst))))))

;; true if every state in the list has at least one zipcode record
(define (all-known? states)
  (cond ((null? states) #t)
        ((= (count-zips-in-state (car states) zipcodes) 0) #f)
        (else (all-known? (cdr states)))))


;; ---------- option 2: find by zipcode ----------

(define (find-zip)
  (let ((text (ask "Enter a 5-digit zipcode: ")))
    (cond ((not (valid-zip? text))
           (print-line "That is not a valid zipcode. Please enter exactly 5 digits."))
          (else (print-zip-record text (find-by-zip (string->number text) zipcodes))))))

;; print the record we found for a zipcode, or say it was not found
(define (print-zip-record text record)
  (cond ((not record) (print-line (string-append "No record was found for zipcode " text ".")))
        (else (print-line (string-append "Record for zipcode " text ":"))
              (print-record record))))


;; ---------- option 3: find by place ----------

(define (find-place)
  (let* ((place (ask "Enter a place name: "))
         (record (find-by-place place zipcodes)))
    (cond ((string=? place "") (print-line "You did not enter a place name."))
          ((not record) (print-line (string-append "No record was found for " place ".")))
          (else (print-line (string-append "First record for " place ":"))
                (print-record record)))))


;; ---------- option 4: states that have the given place ----------

(define (states-with-place)
  (let* ((place (ask "Enter a place name: "))
         (states (states-for-place place zipcodes)))
    (cond ((string=? place "") (print-line "You did not enter a place name."))
          ((null? states) (print-line (string-append "No state has a place named " place ".")))
          (else (print-line (string-append (number->string (length states))
                                           " state(s) have a place named " place ":"))
                (print-list states)))))


;; ---------- option 5: common places between states ----------

;; the states are typed on one line separated by spaces (commas are ok too)
(define (common)
  (let* ((line (ask "Enter two or more state abbreviations separated by spaces (like OH IN KY): "))
         (states (remove-duplicates (upcase-all (string-split (string-replace line "," " ")))))
         (places (common-places states zipcodes)))
    (cond ((< (length states) 2) (print-line "Please enter at least two different states."))
          ((not (all-known? states)) (print-line "One of those states is not in the dataset."))
          ((null? places) (print-line "There are no common places between those states."))
          (else (print-line (string-append (number->string (length places))
                                           " place(s) are common to " (string-join states ", ") ":"))
                (print-list places)))))


;; ---------- option 6: count zipcodes for a state ----------

(define (count-zips)
  (let* ((state (string-upcase (ask "Enter a state abbreviation (like OH): ")))
         (count (count-zips-in-state state zipcodes)))
    (cond ((string=? state "") (print-line "You did not enter a state."))
          ((= count 0) (print-line (string-append "There is no state " state " in the dataset.")))
          (else (print-line (string-append state " has " (number->string count) " zipcode entries."))))))


;; ---------- the menu ----------

(define (print-menu)
  (newline)
  (print-line "========== Zipcode Explorer ==========")
  (print-line " 1. Show results (run run_funcs.scm)")
  (print-line " 2. Find by zipcode")
  (print-line " 3. Find by place")
  (print-line " 4. Find states that have the given place")
  (print-line " 5. Find common places between states")
  (print-line " 6. Count zipcodes for a given state")
  (print-line " 0. Exit")
  (print-line "======================================"))

;; run whichever option the user picked
(define (do-option choice)
  (newline)
  (cond ((string=? choice "1") (run-results))
        ((string=? choice "2") (find-zip))
        ((string=? choice "3") (find-place))
        ((string=? choice "4") (states-with-place))
        ((string=? choice "5") (common))
        ((string=? choice "6") (count-zips))
        (else (print-line "That is not a menu option. Please enter a number from 0 to 6."))))

;; show the menu, do what the user picked, then show the menu again.
;; this is our "loop": menu keeps calling itself until the user enters 0.
(define (menu)
  (print-menu)
  (let ((choice (ask "Enter your choice (0-6): ")))
    (cond ((string=? choice "0") (print-line "Goodbye!"))
          (else (do-option choice)
                (menu)))))

;; start the program
(print-line "Welcome to the Zipcode Explorer!")
(display "Loaded ") (display (length zipcodes)) (print-line " zipcode records.")
(menu)
