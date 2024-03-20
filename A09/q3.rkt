;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 09, question 2
;; ***************************************************
;;


;; ~~~~~ 3a

;; (occ lon num) consumes a list of numbers and a number and produces the number of times that
;; the given number occurs in the list of numbers

;; Example:

(check-expect (occ (list 1 2 3 4 4 6 1 1) 1) 3)


;; occ: (listof Num) Num -> Nat

(define (occ lon num)
             (foldl (lambda (x y) (cond
                                    [(equal? x num) (+ y 1)]
                                    [else y])) 0 lon))

;; Tests:

(check-expect (occ (list 1 2 3 4 4 6 7 7 7) 2) 1)
(check-expect (occ (list 7 1 2 3 4 4 6 7 7 7) 7) 4)


;; ~~~~~ 3b

;; (pocket-change los) consumes a list of symbols and produces a number that isthe total value
;; of the change

;; Example:

(check-expect (pocket-change (list 'penny 'penny 'penny 'nickel)) 0.08)

;; pocket-change: (listof Sym) -> Num

(define (pocket-change los)
  (foldr + 0 (map (lambda (x)
                    (cond
                      [(symbol=? x 'penny) 0.01]
                      [(symbol=? x 'nickel) 0.05]
                      [(symbol=? x 'dime) 0.1]
                      [(symbol=? x 'quarter) 0.25]
                      [(symbol=? x 'loonie) 1.00]
                      [(symbol=? x 'toonie) 2.00]
                      [else 0]))
                  los)))

;; Tests:

(check-expect (pocket-change (list 'penny 'penny 'penny 'nickel 'apple)) 0.08)
(check-expect (pocket-change (list 'penny 'loonie 'dime 'nickel)) 1.16)
(check-expect (pocket-change (list 'quarter 'toonie 'dime 'nickel)) 2.4)


;; ~~~~~ 3c

;; (first-col matrix) consumes a Matrix and produces the first column of the matrix as a list

;; Example:

(check-expect (first-col '((1 2 3 4)
                           (5 6 7 8)
                           (9 10 11 12))) (list 1 5 9))

;; first-col: Matrix -> (listof Num)

(define (first-col matrix)
  (map first  matrix))

;; Tests:

(check-expect (first-col '((2 3 4 5)
                           (6 7 8 9 11)
                           (1 10 11 12 14))) (list 2 6 1))

;; ~~~~~ 3d

;; (add1-mat matrix) consumes a Matrix and produces a Matrix resulting from adding 1 to
;; every entre of the Matrix

;; Example:

(check-expect (add1-mat '((1 2 3 4)
                          (5 6 7 8)
                          (9 10 11 12)))
              (list (list 2 3 4 5)
                    (list 6 7 8 9)
                    (list 10 11 12 13)))

;; add1-mat: Matrix -> Matrix

(define (add1-mat matrix)
  (map (lambda (x) (map add1 x)) matrix))

;; Tests:

(check-expect (add1-mat '((2 3 4 5)
                          (6 7 8 9 11)
                          (1 10 11 12 14)))
              '((3 4 5 6)
                (7 8 9 10 12)
                (2 11 12 13 15)))


;; ~~~~~ 3e

;; (sum-at-zero lof) consumes a list of functions and produces the sum of those functions
;; evaluated at 0

;; Example:

(check-expect (sum-at-zero (list add1 sqr add1)) 2)

;; sum-at-zero: (list (anyof Num -> Num, Num -> Int, or Num -> Nat)) -> Num

(define (sum-at-zero lof)
  (+ (foldl (lambda (x y) (+ (x 0) y)) 0 lof)))

;; Tests:

(check-expect (sum-at-zero (list sqr sqr sub1)) -1)