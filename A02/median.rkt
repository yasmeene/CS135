;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname median) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 02, Problem 2
;; ***************************************************
;;

(check-expect (median-of-3-simple 3 2 4) 3)
(check-expect (median-of-3-simple 2 5 6) 5)
(check-expect (median-of-3-simple 8 10 9) 9)


;; checking the median of three variables, a, b and c
(define (median-of-3-simple a b c)
  (cond
    [(<= a b) (cond 
                [(>= a c) a]
                [(<= b c) b]
                [else c])]

    [(<= a c) a]
    [(>= b c) b]
    [else c]))
      