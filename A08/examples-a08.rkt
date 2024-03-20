;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname examples-a08) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 08, check-expects 
;; ***************************************************
;;

;; question 1b, function partition


(check-expect (partition string? (list "banana" "apple" 'cs135 "blue" 135))
              (list (list "banana" "apple" "blue") (list 'cs135 135)))

(check-expect (partition number? (list 111 "apple" 'cs135 "blue" 135))
              (list (list 111 135) (list "apple" 'cs135 "blue")))

;; question 2a, function or-pred

(check-expect (or-pred even? empty) false)
(check-expect (or-pred odd? (list 6 10 4)) false)
(check-expect (or-pred string? (list 5 "wow")) true)
(check-expect (or-pred even? (list 5 3 7 6)) true)
(check-expect (or-pred number? (list 'one 'two 'three)) false)

;; question 2b, function map2argfn

(check-expect (map2argfn (list + - * / list) (list 3 2)) (list 5 1 6 1.5 (list 3 2)))
(check-expect (map2argfn (list + - *) (list 3 2)) (list 5 1 6))
(check-expect (map2argfn (list * + - /) (list 8 4)) (list 32 12 4 2))

;; question 2c, function arranged?

(check-expect (arranged? (list integer? <) (list)) true)
(check-expect (arranged? (list string? >) (list)) true)
(check-expect (arranged? (list integer? >) (list 1)) true)
(check-expect (arranged? (list integer? >) (list 'red)) false)
(check-expect (arranged? (list string? >) (list "wow" 'red)) false)
(check-expect (arranged? (list string? >) (list "wow" "orange" "red" 'red)) false)
(check-expect (arranged? (list string? string>?) (list "wow" "cs135" "amazing")) true)

;; question 3, function tree-pred

(check-expect ((tree-pred even?) t) false)
(check-expect ((tree-pred positive?) t) true)
(check-expect ((tree-pred string?) t) false)
(check-expect ((tree-pred number?) t) true)

;; question 4b, function nested-filter

(check-expect (nested-filter positive? (cons 1 (cons
                                                (cons 2 (cons 3 empty))
                                                (cons -1 (cons -5 (cons 6 (cons (cons 5
                                                                                      (cons 8 empty)
                                                                                      ) empty)))))))
              (cons 1 (cons (cons 2 (cons 3 empty)) (cons 6 (cons (cons 5 (cons 8 empty)) empty)))))

(check-expect (nested-filter string? (cons 1 (cons (cons "hello"
                                                         (cons "hi" (cons
                                                                     (cons 1 (cons 2 empty)) empty)))
                                                   (cons "yum" (cons 'cs145 empty)))))
              (cons (cons "hello" (cons "hi" (cons empty (cons "yum" empty)))) empty))

;; question 4c, function ruthless

(check-expect (ruthless '(rabbit (apple pluto (ruth blue) ruth)
                                 hello))
              '(rabbit (apple pluto (blue)) hello))

(check-expect (ruthless '(cs135 cs145 ruth (orange yasmeen kenzie) (ruth (ruth hello) phone)))
              '(cs135 cs145 (orange yasmeen kenzie) ((hello) phone)))

;; question 4d, function keep-between

(check-expect (keep-between 5 10 '(1 3 5 (7 9 10) (8 (3 4)) 8 15))
              '(5 (7 9 10) (8 ()) 8))

(check-expect (keep-between 9 11 '(1 5 8 10 (11 12 13) (9 (10 3) 17)))
              '(10 (11) (9 (10))))

(check-expect (keep-between 4 10 '(3 4 5 6 (7 8 9 (10 11) 12) 2 2))
              '(4 5 6 (7 8 9 (10)) 2 2))

;; question 4e, function nested-cleanup

(check-expect (nested-cleanup '(1 () 2 () () 3))
              '(1 2 3))

(check-expect (nested-cleanup '(1 (()()) 2 ((3 () (()))) ))
              '(1 2 ((3))))

(check-expect (nested-cleanup '(()(()())(())()))
              false)

(check-expect (nested-cleanup '(1 2 3 () 4 (5 ())))
              '(1 2 3 4 (5)))

;; question 5a, function nested-cleanup

(check-expect (matrix-apply (list abs floor (lambda (x) (+ x 3))) '((7 4.5 -3.2)(-3 3 13)))
              (list (list (list 7 4.5 3.2)
                          (list 3 3 13))
                    (list (list 7 4 -4)
                          (list -3 3 13))
                    (list (list 10 7.5 -0.2)
                          (list 0 6 16))))

(check-expect (matrix-apply (list exp floor '((1 2 3) (2 3 4))))
              (list
               (list (list 1 4 9)
                    (list 4 9 16))
               (list (list 1 2 3)
                     (list 2 3 4))))

;; question 5b, function nested-cleanup
                    
(check-expect ((scale-smallest '((7 4.5 3.2) (-3 3 13)) 2.4) 7) -18.6)
(check-expect ((scale-smallest '((7 4.5 3.2) (-3 3 13)) 2.4) -2.7) 10.5)
