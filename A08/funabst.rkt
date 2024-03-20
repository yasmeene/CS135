;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname funabst) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 08, question 2
;; ***************************************************
;;

;; ~~~~~ 2a

;; (or-pred pred lst) consumes a predicate and a list, and produces true if any element of the
;; consumes list produces true, otherwise the function produces false

;; Examples:

(check-expect (or-pred even? empty) false)
(check-expect (or-pred odd? (list 6 10 4)) false)

;; or-pred: (Any -> Bool) (listof Any) -> Bool

(define (or-pred pred lst)
  (cond
    [(empty? lst) false]
    [(pred (first lst)) true]
    [else (or-pred pred (rest lst))]))

;; Tests:

(check-expect (or-pred string? (list 5 "wow")) true)
(check-expect (or-pred even? (list 5 3 7 6)) true)
(check-expect (or-pred number? (list 'one 'two 'three)) false)

;; ~~~~~ 2b

;; (map2argfn lof lst) consumes a list of functions and a two element list and produces a list of the
;; results of applying each function to the given two numbers

;; Example:

(check-expect (map2argfn (list + - * / list) (list 3 2)) (list 5 1 6 1.5 (list 3 2)))

;; map2argfn: (list (Num Num -> Any)) -> (listof Any)

(define (map2argfn lof lst)
  (cond
    [(empty? lof) empty]
    [else (cons ((first lof) (first lst) (second lst)) (map2argfn (rest lof) lst))]))

;; Tests:

(check-expect (map2argfn (list + - *) (list 3 2)) (list 5 1 6))
(check-expect (map2argfn (list * + - /) (list 8 4)) (list 32 12 4 2))


;; ~~~~~ 2c

;; (arranged? lop loo) consumes a (list of predicate-function binary-relational-operator)
;; pair and a list of operands. The function produces true if the list is empty, has one value that
;; is valid for the predicate, or every element is valid. otherwise it produces false.

;; Examples:

(check-expect (arranged? (list integer? <) (list)) true)
(check-expect (arranged? (list string? >) (list)) true)
(check-expect (arranged? (list integer? >) (list 1)) true)

;; arranged?: (list (Any -> Bool) (X X -> Bool)) (listof Any) -> Bool
;; requires: predicate-function produces true on elements of type X.

(define (arranged? lop loo)
  (cond
    [(empty? loo) true]
    [(= 1 (length loo)) (cond
                          [((first lop) (first loo)) true]
                          [else false])]
    [(and ((first lop) (first loo)) ((first lop) (second loo))) (cond
                                                                  [((second lop)
                                                                    (first loo) (second loo))
                                                                   (arranged? lop (rest loo))]
                                                                  [else false])]
    [else false]))


;; Tests:

(check-expect (arranged? (list integer? >) (list 'red)) false)
(check-expect (arranged? (list string? >) (list "wow" 'red)) false)
(check-expect (arranged? (list string? string>?) (list "wow" "orange" "red" 'red)) false)
(check-expect (arranged? (list string? string>?) (list "wow" "cs135" "amazing")) true)




