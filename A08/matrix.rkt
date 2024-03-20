;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname matrix) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 08, question 5
;; ***************************************************
;;


(define M (list (list -1 2 3)
                (list 4 5 6)
                (list 7 8.5 9)))

;; A Matrix is one of:
;; * empty
;; * (cons (listof Num) Matrix)
;; requires: each (listof Num) is non-empty and has the same length

;; ~~~~~ 5a

;; (matrix-apply lof matrix) consumes a list of functions and a matrix, and produces a list of
;; matrices where each function was applied

;; Example:


(check-expect (matrix-apply (list abs floor (lambda (x) (+ x 3))) '((7 4.5 -3.2)(-3 3 13)))
              (list (list (list 7 4.5 3.2)
                          (list 3 3 13))
                    (list (list 7 4 -4)
                          (list -3 3 13))
                    (list (list 10 7.5 -0.2)
                          (list 0 6 16))))

;; matrix-apply: (list (anyof Num -> Num, Num -> Int, or Num -> Nat)) Matrix -> Matrix

(define (matrix-apply lof matrix)
  (local
    [;; (list-apply f lst) consumes a function and a list and applies that function to each element

     ;; (anyof Num -> Num, Num -> Int, or Num -> Nat) (listof Num) -> (listof Num)
     (define (list-apply f lst)
       (cond
         [(empty? lst) empty]
         [(list? (first lst)) (cons (list-apply f (first lst))
                                    (list-apply f (rest lst)))]
         [else (cons (f (first lst)) (list-apply f (rest lst)))]))]
    (cond
      [(empty? lof) empty]
      [else (cons (list-apply (first lof) matrix) (matrix-apply (rest lof) matrix))])))


;; Test:

(check-expect (matrix-apply (list sqr floor) '((1 2 3) (2 3 4)))
              (list
               (list (list 1 4 9)
                     (list 4 9 16))
               (list (list 1 2 3)
                     (list 2 3 4))))


;; ~~~~~ 5b

;; (scale-smallest matrix offset) consumes a non-empty matrix and a real number (the offset) and
;; produces a second function that consumes a number, multiplies that number by the smallest element
;; of the matrix, and adds the offset

;; Example:

(check-expect ((scale-smallest '((7 4.5 3.2) (-3 3 13)) 2.4) 7) -18.6)

;; scale-smallest: Matrix Num -> (Num -> Num)

(define (scale-smallest matrix offset)
  (local
    [;; (smallest-v1 lon) consumes a list of numbers and produces the
     ;; smallest value in the given list
     
     ;; smallest-v1: (listof Num) -> Num
     (define (smallest-v1 lon)
       (cond
         [(empty? (rest lon)) (first lon)]
         [else (min (first lon) (smallest-v1 (rest lon)))]))

     ;; (smallest-val matrix) consumes a Matrix and produces the smallest value in that matrix
     
     ;; smallest-val: Matrix -> Num
     (define (smallest-val matrix)
       (cond
         [(empty? (rest matrix)) (smallest-v1 (first matrix))]
         [(list? (first matrix)) (min (smallest-v1 (first matrix)) (smallest-val (rest matrix)))]
         [else (min (first matrix) (smallest-val (rest matrix)))]))]
    (lambda (x) (+ (* x (smallest-val matrix)) offset))))

;; Test:

(check-expect ((scale-smallest '((7 4.5 3.2) (-3 3 13)) 2.4) -2.7) 10.5)
                      