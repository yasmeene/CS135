;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname blood) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 02, Problem 4
;; ***************************************************
;;


;; checking if the donor can donate to the recipient using conditionals

(define (can-donate-to/cond? x y) ; let x be the donors blood type and y be the recipeints blood type
  (cond
    [(symbol=? x y) true] ; if the blood types are the same, they can donate
    [(symbol=? y 'AB+) true] ; if the recipeient is AB+, any blood type can donate
    [(symbol=? x 'O-) true]
    [(symbol=? x 'O+) (cond
                        [(symbol=? y 'A+) true]
                        [(symbol=? y 'B+) true]
                        [else false])]
    [(symbol=? x 'A-) (cond
                         [(symbol=? y 'A+) true]
                         [(symbol=? y 'AB-) true]
                         [else false])]
    [(symbol=? x 'B-) (cond
                         [(symbol=? y 'B+) true]
                         [(symbol=? y 'AB-) true]
                         [else false])]
    [else false]))



;; checking if the donor can donate to the recipient using booleans


(define (can-donate-to/bool? x y)
  (or (symbol=? x y)
      (symbol=? y 'AB+)
      (symbol=? x 'O-)
      (and (symbol=? x 'O+) (or
                             (symbol=? y 'A+)
                             (symbol=? y 'B+)
                             false))
      (and (symbol=? x 'A-) (or
                             (symbol=? y 'A+)
                             (symbol=? y 'AB-)
                             false))
      (and (symbol=? x 'B-) (or
                             (symbol=? y 'B+)
                             (symbol=? y 'AB-)
                             false))
      false))
  