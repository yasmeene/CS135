;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname conversion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen (21071183)
;; CS 135 Fall 2023
;; Assignment 01, Problem 3
;; ***************************************************
;;


;; miles per hour converted to meters per second

(check-expect (mph->m/s 1) 1397/3125)

(define (mph->m/s mph)
  (* (/ 1609.344 (* 60 60)) mph)) 

;; miles per hour converted to smoots per millifortnight

(check-expect (mph->s/mfn 1) 532224/1675 )

(define (mph->s/mfn mph)
  (* (/ (* 1609.344 1209.6) (* 60 60 1.7018)) mph))

;; psi converted to pascal units

(check-expect (psi->pa 1) 111205000/16129)

(define (psi->pa psi)
  (* 4.4482 (sqr 12) (/ 1 (sqr 0.3048)) psi))
