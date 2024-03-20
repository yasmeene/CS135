;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname partition) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 08, question 1
;; ***************************************************
;;

;; (partition pred lst) consumes a predicate and a list and produces a two element list where the
;; first element is a list of items that satisfy the predicate and the second element is a list of
;; items that dont

;; Example:

(check-expect (partition string? (list "banana" "apple" 'cs135 "blue" 135))
              (list (list "banana" "apple" "blue") (list 'cs135 135)))

;; partition: (Any -> Bool) (listof Any) -> (list (listof Any) (listof Any))

(define (partition pred lst)
  (local
    ;; (satisfy-pred r t f) splits the given list by if the value satisfies the predicate or not
    ;; satisfy-pred: (listof Any) Any Any -> (listof Any)
    [(define (satisfy-pred r t f)
      (cond
        [(empty? r) (list t f)]
        [(pred (first r))
         (satisfy-pred (rest r) (append t (list (first r))) f)]
         [else (satisfy-pred (rest r) t (append f (list (first r))))]))]
  (satisfy-pred lst empty empty)))

;; Test:

(check-expect (partition number? (list 111 "apple" 'cs135 "blue" 135))
              (list (list 111 135) (list "apple" 'cs135 "blue")))
