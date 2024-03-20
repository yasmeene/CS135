;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname div-by-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 04, Question 4
;; ***************************************************
;;

;; A Nat3 is one of:
;; * 0, 1, 2
;; * (add1 (add1 (add1 Nat)))

;; nat3-template: Nat -> Any
(define (nat3-template Nat3)
  (cond [(zero? Nat3) ...]
        [(= 1 Nat3) ...]
        [(= 2 Nat3) ...]
        [else (... Nat3 (nat3-template (- Nat3 3)))]))

;; (div-by-3? nat3) consumes a Nat3 and produces true if it is divisible by 3 and false otherwise

;; Examples:

(check-expect (div-by-3? 0) true)
(check-expect (div-by-3? 1) false)
(check-expect (div-by-3? 2) false)

;; div-by-3?: Nat -> Bool
(define (div-by-3? nat3)
  (cond
    [(zero? nat3) true]
    [(< nat3 0) false]
    [else (div-by-3? (- nat3 3))]))

;; Tests:

(check-expect (div-by-3? 3) true) 
(check-expect (div-by-3? 4) false)
(check-expect (div-by-3? 5) false)
(check-expect (div-by-3? 6) true) 
(check-expect (div-by-3? 7) false)
(check-expect (div-by-3? 8) false)
(check-expect (div-by-3? 9) true)

(