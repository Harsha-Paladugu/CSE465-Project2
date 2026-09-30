#lang scheme
;; ============================================================
;; CSE 465 - Project 2 : Zipcode Explorer
;; A menu-driven Scheme program that works on the zipcodes.scm dataset.
;; Files:
;;   zipcodes.scm  - the dataset: a list of records shaped like
;;                   (zipcode place state county latitude longitude)
;;   lookups.scm   - options 2, 3 and 6
;;   places.scm    - options 4 and 5
;;   run_funcs.scm - option 1
;;
;; Everything here uses plain recursion; there are no looping constructs.
;; The menu itself is recursive: main-loop calls itself after each option.
;; ============================================================

(require "zipcodes.scm"    ; zipcodes
         "lookups.scm"     ; find-by-zip  find-by-place  count-zips-in-state
         "places.scm"      ; states-for-place  common-places
         "run_funcs.scm")  ; run-results


;; ------------------------------------------------------------
;; Output helpers
;; ------------------------------------------------------------

;; say : any ... -> void
;; Display every argument in order, then end the line.
;; Example: (say "Found " 3 " states.")  prints  Found 3 states.
(define (say . parts)
  (display-all parts)
  (newline))

;; display-all : list -> void
;; Display each item of the list, one after another.
(define (display-all parts)
  (cond ((null? parts) (void))
        (else (display (car parts))
              (display-all (cdr parts)))))

;; show-items : list -> void
;; Print each item on its own indented line.
(define (show-items items)
  (cond ((null? items) (void))
        (else (say "   - " (car items))
              (show-items (cdr items)))))

;; pad-left : string number -> string
;; Add "0" to the front of str until it is width characters long.
(define (pad-left str width)
  (cond ((>= (string-length str) width) str)
        (else (pad-left (string-append "0" str) width))))

;; zip->string : number -> string
;; The dataset stores zipcodes as numbers, so 6404 really means "06404".
;; Turn the number back into a 5-character string with leading zeros.
(define (zip->string zip)
  (pad-left (number->string zip) 5))

;; show-record : record -> void
;; Print all six fields of one dataset record with a label for each.
(define (show-record record)
  (say "   Zipcode   : " (zip->string (list-ref record 0)))
  (say "   Place     : " (list-ref record 1))
  (say "   State     : " (list-ref record 2))
  (say "   County    : " (list-ref record 3))
  (say "   Latitude  : " (list-ref record 4))
  (say "   Longitude : " (list-ref record 5)))


;; ------------------------------------------------------------
;; Input helpers
;; ------------------------------------------------------------

;; prompt : string -> void
;; Show a prompt and make sure it appears before the program waits for input.
(define (prompt text)
  (display text)
  (flush-output))

;; read-input : -> string
;; Read one line typed by the user, without the spaces around it.
;; If the input has ended (end-of-file), return "0" so the menu exits cleanly.
(define (read-input)
  (let ((line (read-line (current-input-port) 'any)))
    (if (eof-object? line)
        "0"
        (string-trim line))))

;; all-digits? : string -> boolean
;; True when every character in str is a digit 0-9.
(define (all-digits? str)
  (cond ((string=? str "") #t)
        ((char-numeric? (string-ref str 0))
         (all-digits? (substring str 1)))
        (else #f)))

;; valid-zip? : string -> boolean
;; A valid zipcode is exactly 5 digits, for example "45056" or "06404".
(define (valid-zip? str)
  (and (= (string-length str) 5)
       (all-digits? str)))

;; upcase-all : list of strings -> list of strings
;; Upper-case every string so "oh" and "OH" are treated the same.
(define (upcase-all items)
  (cond ((null? items) '())
        (else (cons (string-upcase (car items))
                    (upcase-all (cdr items))))))

;; read-states : -> list of strings
;; Read a line like "oh, in ky" and turn it into ("OH" "IN" "KY"):
;; commas become spaces, the line is split on spaces, every abbreviation
;; is upper-cased, and repeated states are dropped.
(define (read-states)
  (remove-duplicates
   (upcase-all
    (string-split (string-replace (read-input) "," " ")))))

;; state-exists? : string -> boolean
;; A state exists when at least one zipcode record belongs to it.
(define (state-exists? state)
  (> (count-zips-in-state state zipcodes) 0))

;; first-unknown-state : list of strings -> string or #f
;; Return the first state in the list that is not in the dataset,
;; or #f when every state is known.
(define (first-unknown-state states)
  (cond ((null? states) #f)
        ((state-exists? (car states)) (first-unknown-state (cdr states)))
        (else (car states))))


;; ------------------------------------------------------------
;; Menu option 1 - Show results
;; ------------------------------------------------------------

;; Run every expression in run_funcs.scm and show the labeled results.
(define (option-show-results)
  (say "Running all expressions in run_funcs.scm ...")
  (newline)
  (run-results))


;; ------------------------------------------------------------
;; Menu option 2 - Find by zipcode
;; ------------------------------------------------------------

;; Ask for a zipcode, check it, and look up the first matching record.
(define (option-find-by-zip)
  (prompt "Enter a 5-digit zipcode: ")
  (let ((text (read-input)))
    (cond ((not (valid-zip? text))
           (say "\"" text "\" is not a valid zipcode. Please enter exactly 5 digits."))
          (else
           (show-zip-result text (find-by-zip (string->number text) zipcodes))))))

;; show-zip-result : string (record or #f) -> void
(define (show-zip-result text record)
  (cond ((not record) (say "No record was found for zipcode " text "."))
        (else (say "Record for zipcode " text ":")
              (show-record record))))


;; ------------------------------------------------------------
;; Menu option 3 - Find by place
;; ------------------------------------------------------------

;; Ask for a place name and look up the first matching record.
(define (option-find-by-place)
  (prompt "Enter a place name: ")
  (let ((place (read-input)))
    (cond ((string=? place "") (say "No place name was entered."))
          (else (show-place-result place (find-by-place place zipcodes))))))

;; show-place-result : string (record or #f) -> void
(define (show-place-result place record)
  (cond ((not record) (say "No record was found for the place \"" place "\"."))
        (else (say "First record for the place \"" place "\":")
              (show-record record))))


;; ------------------------------------------------------------
;; Menu option 4 - Find states that have the given place
;; ------------------------------------------------------------

;; Ask for a place name and list every state that has it.
(define (option-states-for-place)
  (prompt "Enter a place name: ")
  (let ((place (read-input)))
    (cond ((string=? place "") (say "No place name was entered."))
          (else (show-states place (states-for-place place zipcodes))))))

;; show-states : string (list of strings) -> void
(define (show-states place states)
  (cond ((null? states) (say "No state has a place named \"" place "\"."))
        (else (say (length states) " state(s) have a place named \"" place "\":")
              (show-items states))))


;; ------------------------------------------------------------
;; Menu option 5 - Find common places between states
;; ------------------------------------------------------------

;; Ask for two or more states and list the places they all share.
(define (option-common-places)
  (prompt "Enter two or more state abbreviations separated by spaces (e.g. OH IN KY): ")
  (check-states (read-states)))

;; check-states : list of strings -> void
;; Make sure the states are usable before searching for common places.
(define (check-states states)
  (let ((unknown (first-unknown-state states)))
    (cond ((< (length states) 2)
           (say "Please enter at least two different state abbreviations."))
          ((string? unknown)
           (say "The state \"" unknown "\" does not exist in the dataset."))
          (else
           (show-common-places states (common-places states zipcodes))))))

;; show-common-places : (list of strings) (list of strings) -> void
(define (show-common-places states places)
  (cond ((null? places)
         (say "There are no common places between " (string-join states ", ") "."))
        (else
         (say (length places) " place(s) are common to " (string-join states ", ") ":")
         (show-items places))))


;; ------------------------------------------------------------
;; Menu option 6 - Count zipcodes for a given state
;; ------------------------------------------------------------

;; Ask for a state abbreviation and report how many zipcode entries it has.
(define (option-count-zips)
  (prompt "Enter a state abbreviation (e.g. OH): ")
  (show-zip-count (string-upcase (read-input))))

;; show-zip-count : string -> void
(define (show-zip-count state)
  (let ((count (count-zips-in-state state zipcodes)))
    (cond ((string=? state "") (say "No state abbreviation was entered."))
          ((= count 0) (say "The state \"" state "\" does not exist in the dataset."))
          (else (say "The state " state " has " count " zipcode entries.")))))


;; ------------------------------------------------------------
;; The menu
;; ------------------------------------------------------------

;; show-menu : -> void
;; Print the menu and ask for a choice.
(define (show-menu)
  (newline)
  (say "=============== Zipcode Explorer ===============")
  (say "  1. Show results (run everything in run_funcs.scm)")
  (say "  2. Find by zipcode")
  (say "  3. Find by place")
  (say "  4. Find states that have the given place")
  (say "  5. Find common places between states")
  (say "  6. Count zipcodes for a given state")
  (say "  0. Exit")
  (say "================================================")
  (prompt "Enter your choice (0-6): "))

;; run-option : string -> void
;; Run the menu option the user picked.
(define (run-option choice)
  (newline)
  (cond ((string=? choice "1") (option-show-results))
        ((string=? choice "2") (option-find-by-zip))
        ((string=? choice "3") (option-find-by-place))
        ((string=? choice "4") (option-states-for-place))
        ((string=? choice "5") (option-common-places))
        ((string=? choice "6") (option-count-zips))
        (else (say "\"" choice "\" is not a menu option. Please enter a number from 0 to 6."))))

;; main-loop : -> void
;; Show the menu, run the chosen option, then show the menu again.
;; This is the program's loop, written as recursion: main-loop calls
;; itself after every option until the user enters 0.
(define (main-loop)
  (show-menu)
  (let ((choice (read-input)))
    (cond ((string=? choice "0") (say "Goodbye!"))
          (else (run-option choice)
                (main-loop)))))

;; Start the program.
(say "Welcome to the Zipcode Explorer!")
(say "Loaded " (length zipcodes) " zipcode records.")
(main-loop)
