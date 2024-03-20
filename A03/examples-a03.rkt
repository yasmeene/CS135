;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname examples-a03) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 03, check-expect examples
;; ***************************************************
;;


;; check-expect examples for question 2a, function RGB->name

(check-expect (RGB->name (cons 225 (cons 12 (cons 37 empty)))) 'unknown)  ; if it consumes a three element list, does it actually tke in a list?
(check-expect (RGB->name (cons 0 (cons 225 (cons 0 empty)))) 'green)

;; check-expect examples for question 2b, function name->RGB

(check-expect (name->RGB 'pink) (cons -1 (cons -1 (cons -1 empty))))
(check-expect (name->RGB 'magenta) (cons 225 (cons 0 (cons 225 empty))))

;; check-expect examples for question 2c, function RBG->luminosity

(check-within (RBG->luminosity (cons 0 (cons 225 (cons 0 empty)))) 132.75 0.01)

;; check-expect examples for question 2d, function valid-RGB?

(check-expect (valid-RGB? 'purple) false)
(check-expect (valid-RGB? (cons 0 (cons 50 (cons 200 empty)))) true)

;; check-expect examples for question 2e, function RGB->hex

(check-expect (RGB->hex (cons 120 (cons 40 (cons 213 empty)))) (cons "7" (cons "9" (cons "2" (cons "8" (cons "D" (cons "5" empty)))))))