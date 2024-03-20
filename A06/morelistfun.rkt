;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname morelistfun) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 06, question 1
;; ***************************************************
;;

;; An association list (AL) is one of:
;; * empty
;; * (cons (list Nat Str) AL)
;; Requires: each key (Nat) is unique

;; ~~~~ 1a

;; (my-list-ref lst index) consumes a list of numbers and an index, and produces the element in
;; the list at the consumed index.

;; Examples:

(check-expect (my-list-ref (list 1 2 3 4) 0) 1)
(check-expect (my-list-ref (list 5 4 3) 2) 3)

;; my-list-ref: (listof Num) Num -> (anyof Num Bool)

(define (my-list-ref lst index)
  (cond
    [(empty? lst) false]
    [(zero? index) (first lst)]
    [else (my-list-ref (rest lst) (sub1 index))]))

;; Tests:

(check-expect (my-list-ref (list 2) 20) false)
(check-expect (my-list-ref (list 3 5 7 9 11) 4) 11)
(check-expect (my-list-ref (list 2 2) 1) 2)

;; ~~~~ 1b

;; (zip lst1 lst2) consumes two lists with the same length and produces an association list
;; where the keys are the elements of the first list, and the values are the corresponding
;; elements of the second list

;; Examples:

(check-expect (zip (list 1 2 3) (list 4 5 6))
              (list (list 1 4) (list 2 5) (list 3 6)))

(check-expect (zip (list 1 2 3 4) (list "a" "b" "c" "d"))
              (list (list 1 "a") (list 2 "b") (list 3 "c") (list 4
                                                                 "d")))

;; zip: (listof Any) (listof Any) -> AL

(define (zip lst1 lst2)
  (cond
    [(empty? lst1) empty]
    [else (cons (list (first lst1) (first lst2)) (zip (rest lst1) (rest lst2)))]))

;; Tests:

(check-expect (zip empty empty) empty)

(check-expect (zip (list 'red 'blue 'green 'yellow) (list "red" "blue" "green" "yellow"))
              (list (list 'red "red") (list 'blue "blue" ) (list 'green "green")
                    (list 'yellow "yellow")))


;; ~~~~ 1c

;; (list-xor lon1 lon2) r consumes two lists of numbers that are sorted in increasing order
;; and produces a sorted list that contains only the items that are in lon1 or lon2, but not
;; both

;; Examples:

(check-expect (list-xor (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))
(check-expect (list-xor (list 1 3 5) (list 2 3 4)) (list 1 2 4 5))

;; list-xor: (listof Num) (listof Num) -> (listof Num)

(define (list-xor lst1 lst2)
  (order (append (unique-list1 lst1 lst2) (unique-list2 lst1 lst2))))

;; (list-xor1 lst1 lst2) consumes two lists and produces a sorted list of the
;; unique elements in list 1

(define (unique-list1 lst1 lst2)
  (cond
    [(empty? lst1) empty]
    [(member? (first lst1) lst2) (unique-list1 (rest lst1) lst2)]
    [else (cons (first lst1) (unique-list1 (rest lst1) lst2))]))

;; (list-xor2 lst1 lst2) consumes two lists and produces a sorted list of the unique
;; elements in list 2

(define (unique-list2 lst1 lst2)
  (cond
    [(empty? lst2) empty]
    [(member? (first lst2) lst1) (unique-list2  lst1 (rest lst2))]
    [else (cons (first lst2) (unique-list2 lst1 (rest lst2)))]))

;; (order lon) sorts the elements of lon in non-decreasing order
(define (order lon)
  (cond [(empty? lon) empty]
        [else (insert (first lon) (order (rest lon)))]))


;; (insert n slon) inserts the number n into the sorted list slon
;;     so that the resulting list is also sorted.
;; Requires: slon is sorted in non-decreasing order

(define (insert n slon)
  (cond [(empty? slon) (cons n empty)]
        [(<= n (first slon)) (cons n slon)]
        [else (cons (first slon) (insert n (rest slon)))]))


           