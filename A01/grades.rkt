;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen (21071183)
;; CS 135 Fall 2023
;; Assignment 01, Problem 4
;; ***************************************************
;;

;; final cs 135 grades
;; cg --> class particpation grade
;; mg --> midterm grade
;; fg --> final exam grade
;; og --> overall assignment grade

(check-within (final-cs135-grade 75 80 87 100) 88.90 0.01)

(define (final-cs135-grade cg mg fg og)
  (+ (* 0.05 cg) (* 0.2 mg) (* 0.45 fg) (* 0.3 og)))

;; final grade needed to pass course

(check-within (cs135-final-exam-grade-needed 50 60 25) 84.44 0.01)

(define (cs135-final-exam-grade-needed cg mg og)
  (/(- 60 (+ (* 0.05 cg) (* 0.2 mg) (* 0.3 og))) 0.45))