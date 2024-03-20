;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 02, Problem 3
;; ***************************************************
;;

;; simplified version of 3a

(check-expect (q3a-simplified -1 true) -2) 

(define (q3a-simplified n a?)
  (cond
    [(and a? (>= n 0)) (+ n 1)]
    [a? (- n 1)]
   [else 0]))


;; simplified version of 3b

(define (q3b-simplified a? b? c?)
  (cond
    [(and a? b?) 'elm]
    [(and (not c?) (not b?)) 'birch]
    [a? 'cedar]
    [b? 'pine]
    [else 'cherry]))


;; simplified version of 3c


(define (q3c-simplified a? b? c?)
  (cond
    [(and (or (and c? b?) (and (not a?) (not c?))) b?) 'spruce]
    [(or (and c? b?) (and (not a?) (not c?))) 'larch]
    [a? 'hazel]
    [else 'hickory]))