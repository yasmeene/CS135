;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname nested) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 08, question 4
;; ***************************************************
;;

;; ~~~~~ 4a

;; A (nested-listof X) is one of:
;; * empty
;; * (cons (nested-listof X) (nested-listof X))
;; * (cons X (nested-listof X))
;; Requires: X itself is not a list type

;; nested-listof-X-template: (nested-list-of X) -> Any

(define (nested-listof-X-template nlox)
  (cond
    [(empty? nlox) ...]
    [(cons? (first nlox)) ...]
    [else (... (first nlox)
               (nested-listof-X-template (rest nlox)))]))

;; ~~~~~ 4b

;; (nested-filter pred nl) consumes a predicate function and a nested list and removes
;; every element that appears anywhere in the nested list where the predicate function is
;; false for that element

;; Example:

(check-expect (nested-filter positive? (cons 1 (cons
                                                (cons 2 (cons 3 empty))
                                                (cons -1 (cons -5 (cons 6 (cons (cons 5
                                                                                      (cons 8 empty)
                                                                                      ) empty)))))))
              (cons 1 (cons (cons 2 (cons 3 empty)) (cons 6 (cons (cons 5 (cons 8 empty)) empty)))))

;; nested-filter: (Any -> Bool) (nested-listof X) -> (nested-listof X)

(define (nested-filter pred nl)
  (cond
    [(empty? nl) empty]
    [(cons? (first nl)) (cons (local
                                [;; (list-filter lst) consumes a list and checks if its
                                 ;; elements are valid for the predicate
                                 
                                 ;; list-filter: (listof Any) -> (listof Any)
                                 (define (list-filter lst)
                                   (cond
                                     [(empty? lst) empty]
                                     [(cons? lst) (nested-filter pred lst)]))]
                                (list-filter (first nl)))
                              (nested-filter pred (rest nl)))]
    [(pred (first nl)) (cons (first nl) (nested-filter pred (rest nl)))]
    [else (nested-filter pred (rest nl))]))

;; Tests:

(check-expect (nested-filter string? (cons 1 (cons (cons "hello"
                                                         (cons "hi" (cons
                                                                     (cons 1 (cons 2 empty)) empty)))
                                                   (cons "yum" (cons 'cs145 empty)))))
              (cons (cons "hello" (cons "hi" (cons empty empty))) (cons "yum" empty)))

(check-expect (nested-filter odd? (cons 1 (cons
                                                (cons 2 (cons 3 empty))
                                                (cons -1 (cons -5 (cons 6 (cons (cons 5
                                                                                      (cons 8 empty)
                                                                                      ) empty)))))))
              (cons 1 (cons (cons 3 empty) (cons -1 (cons -5 (cons (cons 5 empty) empty))))))


;; ~~~~~ 4c

;; (ruthless nlos) consumes a nested list of symbols and producesan identical list except all
;; instances of 'ruth have been removed

;; Example:

(check-expect (ruthless '(rabbit (apple pluto (ruth blue) ruth)
                                 hello))
              '(rabbit (apple pluto (blue)) hello))

;; ruthless: (nested-listof Sym) -> (nested-listof Sym)

(define (ruthless nlos)
  (local
    [;; (not-ruth? elem) consumes a symbol and checks to see if its not 'ruth

     ;; not-ruth?: Sym -> Bool
     (define (not-ruth? sym)
       (not (symbol=? sym 'ruth)))]
    (nested-filter not-ruth? nlos)))

;; Tests:

(check-expect (ruthless '(cs135 cs145 ruth (orange yasmeen kenzie) (ruth (ruth hello) phone)))
              '(cs135 cs145 (orange yasmeen kenzie) ((hello) phone)))

(check-expect (ruthless '(rabbit (apple pluto ruth (ruth blue ruth) ruth)
                                 hello hi (cs135 ruth cs145)))
              '(rabbit (apple pluto (blue)) hello hi (cs135 cs145)))


;; ~~~~~ 4d

;; (keep-between a b nlon) consumes two numbers and a nested list of numbers and produces a nested
;; list keeping only the values between the two numbers (inclusive)

;; Example:

(check-expect (keep-between 5 10 '(1 3 5 (7 9 10) (8 (3 4)) 8 15))
              '(5 (7 9 10) (8 ()) 8))

;; keep-between: Num Num (nested-listof Num) -> (nested-listof Num)

(define (keep-between a b nlon)
  (local
    [;; (between? elem) consumes a number and checks to see if that number is between a and b

     ;; between?: Num -> Bool
     (define (between? elem)
       (and (<= a elem)
                 (>= b elem)))]
    (nested-filter between? nlon)))

;; Tests:

(check-expect (keep-between 9 11 '(1 5 8 10 (11 12 13) (9 (10 3) 17)))
              '(10 (11) (9 (10))))

(check-expect (keep-between 4 10 '(3 4 5 6 (7 8 9 (10 11) 12) 2 2))
              '(4 5 6 (7 8 9 (10))))


;; ~~~~~ 4e

;; (nested-cleanup nloa) consumes a (nested-listof Any) and removes all empty lists

;; Example:

(check-expect (nested-cleanup '(1 () 2 () () 3))
              '(1 2 3))

(check-expect (nested-cleanup '(1 (()()) 2 ((3 () (()))) ))
              '(1 2 ((3))))

;; nested-cleanup: (nested-listof X) -> (nested-listof X)

(define (nested-cleanup nloa)
  (cond
    [(empty? nloa) false]
    [(empty? (first nloa)) (nested-cleanup (rest nloa))]
    [(cons? (first nloa)) (cond
                            [(boolean? (nested-cleanup (first nloa)))
                             (nested-cleanup (rest nloa))]
                            [(boolean? (nested-cleanup (rest nloa)))
                             (list (nested-cleanup (first nloa)))])]
    [(cons? (first nloa)) (cons (nested-cleanup (first nloa)) (nested-cleanup (rest nloa)))]
    [(boolean? (nested-cleanup (rest nloa))) (list (first nloa))]
      [else (cons (first nloa) (nested-cleanup (rest nloa)))]))

;; Tests:

(check-expect (nested-cleanup '(()(()())(())()))
              false)

(check-expect (nested-cleanup '(1 2 3 () 4 (5 ())))
              '(1 2 3 4 (5)))