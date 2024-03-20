;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname goldbach) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 10, question 2
;; ***************************************************
;;

;; ~~~~~ 2a

;; (primes n) consumes a natural number and returns a list of primes that
;; are less than or equal to that number

;; Examples:

(check-expect (primes 20) '(2 3 5 7 11 13 17 19))
(check-expect (primes 10) '(2 3 5 7))

;; primes: Nat -> (listof Nat)

(define (primes n)
  (local
    [;; (remove-divisible n lon) consumes a number and a list of numbers and returns a list with all
     ;; numbers in the list divisible by n
     ;; remove-divisible: Nat (listof Nat)
     (define (remove-divisible n lon)
       (filter (lambda (x) (or
                            (= x n)
                            (not (zero? (modulo x n))))) lon))

     ;; (sieve lop candidates) consumes a list of primes and a list of candidates and produces the
     ;; primes that are less than or equal to n
     ;; sieve: (listof Nat) (listof Nat) -> (listof Nat)
     (define (sieve lop candidates)
       (cond
         [(empty? candidates) (reverse lop)]
         [else (sieve (cons (first candidates) lop)
                      (remove-divisible (first candidates) (rest candidates)))]))] 
   (sieve empty (rest (build-list n add1)))))

;; Tests:

(check-expect (primes 30) '(2 3 5 7 11 13 17 19 23 29))
(check-expect (primes 5) '(2 3 5))
(check-expect (primes 2) '(2))
(check-expect (primes 1) '())
;; (time (primes 100000))


;; ~~~~~ 2b

;; (goldbach n) consumes an even natural number greater than 2
;; and produces a natural number indicating the number of ways it can be written as the
;; sum of two primes

;; Example:

(check-expect (goldbach 14) 2)
(check-expect (goldbach 10000) 127)

;; goldbach: Nat -> Nat

(define (goldbach n)
  (local
    [;; (sum-primes prime-lst) consumes a list of primes and produces either 0 or
     ;; recursively calls add-checker on the first of the list
     ;; sum-primes: (listof Nat) -> (listof Nat)
     (define (sum-primes prime-lst)
       (cond
         [(empty? prime-lst) 0]
         [else (+ (add-checker (first prime-lst) prime-lst) (sum-primes (rest prime-lst)))]))
     
     ;; (add-checker val prime-lst) consumes a number and a list of primes and
     ;; recursively adds 1 if it adds to n
     ;; add-checker: Nat (listof Nat) -> Nat
     (define (add-checker val prime-lst)
       (cond
         [(empty? prime-lst) 0]
         [(= n (+ val (first prime-lst))) (+ 1 (add-checker val (rest prime-lst)))]
         [else (add-checker val (rest prime-lst))]))]
    (sum-primes (primes n))))

;; Tests:
(check-expect (goldbach 24) 3)

(check-expect (goldbach 500) 13)
  
  