;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname examples-a02) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 02, (check-expect) examples
;; ***************************************************
;;


;; check-expect examples for question 2
(check-expect (median-of-3-simple 3 2 4) 3) ; example for the first row in the cond
(check-expect (median-of-3-simple 2 5 6) 5) ; example for the first row in the cond
(check-expect (median-of-3-simple 8 10 9) 9) ; example for the first row in the cond


;; check-expect examples for question 4a
(check-expect (can-donate-to/cond? 'O- 'O-) true)
(check-expect (can-donate-to/cond? 'O- 'O+) true)
(check-expect (can-donate-to/cond? 'O- 'A-) true)
(check-expect (can-donate-to/cond? 'O- 'A+) true)
(check-expect (can-donate-to/cond? 'O- 'B-) true)
(check-expect (can-donate-to/cond? 'O- 'B+) true)
(check-expect (can-donate-to/cond? 'O- 'AB+) true)
(check-expect (can-donate-to/cond? 'O- 'AB-) true)
(check-expect (can-donate-to/cond? 'O+ 'O+) true)
(check-expect (can-donate-to/cond? 'O+ 'A+) true)
(check-expect (can-donate-to/cond? 'O+ 'B+) true)
(check-expect (can-donate-to/cond? 'O+ 'AB+) true)
(check-expect (can-donate-to/cond? 'A- 'A-) true)
(check-expect (can-donate-to/cond? 'A- 'A+) true)
(check-expect (can-donate-to/cond? 'A- 'AB-) true)
(check-expect (can-donate-to/cond? 'A- 'AB+) true)
(check-expect (can-donate-to/cond? 'A+ 'A+) true)
(check-expect (can-donate-to/cond? 'A+ 'AB+) true)
(check-expect (can-donate-to/cond? 'B- 'B-) true)
(check-expect (can-donate-to/cond? 'B- 'B+) true)
(check-expect (can-donate-to/cond? 'B- 'AB-) true)
(check-expect (can-donate-to/cond? 'B- 'AB+) true)
(check-expect (can-donate-to/cond? 'B+ 'B+) true)
(check-expect (can-donate-to/cond? 'B+ 'AB+) true)
(check-expect (can-donate-to/cond? 'AB+ 'AB+) true)
(check-expect (can-donate-to/cond? 'AB- 'AB-) true)
(check-expect (can-donate-to/cond? 'AB- 'AB+) true)
(check-expect (can-donate-to/cond? 'O+ 'O-) false)
(check-expect (can-donate-to/cond? 'O+ 'A-) false)
(check-expect (can-donate-to/cond? 'O+ 'B-) false)
(check-expect (can-donate-to/cond? 'O+ 'AB-) false)
(check-expect (can-donate-to/cond? 'A- 'O-) false)
(check-expect (can-donate-to/cond? 'A- 'O+) false)
(check-expect (can-donate-to/cond? 'A- 'B-) false)
(check-expect (can-donate-to/cond? 'A- 'B+) false)
(check-expect (can-donate-to/cond? 'A+ 'O-) false)
(check-expect (can-donate-to/cond? 'A+ 'O+) false)
(check-expect (can-donate-to/cond? 'A+ 'A-) false)
(check-expect (can-donate-to/cond? 'A+ 'B-) false)
(check-expect (can-donate-to/cond? 'A+ 'B+) false)
(check-expect (can-donate-to/cond? 'A+ 'AB-) false)
(check-expect (can-donate-to/cond? 'B- 'O-) false)
(check-expect (can-donate-to/cond? 'B- 'O+) false)
(check-expect (can-donate-to/cond? 'B- 'A-) false)
(check-expect (can-donate-to/cond? 'B- 'A+) false)
(check-expect (can-donate-to/cond? 'B+ 'O-) false)
(check-expect (can-donate-to/cond? 'B+ 'O+) false)
(check-expect (can-donate-to/cond? 'B+ 'A-) false)
(check-expect (can-donate-to/cond? 'B+ 'A+) false)
(check-expect (can-donate-to/cond? 'B+ 'B-) false)
(check-expect (can-donate-to/cond? 'B+ 'AB-) false)
(check-expect (can-donate-to/cond? 'AB+ 'O-) false)
(check-expect (can-donate-to/cond? 'AB+ 'O+) false)
(check-expect (can-donate-to/cond? 'AB+ 'A-) false)
(check-expect (can-donate-to/cond? 'AB+ 'A+) false)
(check-expect (can-donate-to/cond? 'AB+ 'B-) false)
(check-expect (can-donate-to/cond? 'AB+ 'B+) false)
(check-expect (can-donate-to/cond? 'AB+ 'AB-) false)
(check-expect (can-donate-to/cond? 'AB- 'O-) false)
(check-expect (can-donate-to/cond? 'AB- 'O+) false)
(check-expect (can-donate-to/cond? 'AB- 'A-) false)
(check-expect (can-donate-to/cond? 'AB- 'A+) false)
(check-expect (can-donate-to/cond? 'AB- 'B-) false)
(check-expect (can-donate-to/cond? 'AB- 'B+) false)

;; check-expect examples for question 5

(check-expect (triad-type 'a 'c# 'e) 'Major)
(check-expect (triad-type 'b 'd 'f#) 'Minor)
(check-expect (triad-type 'd 'f 'g#) 'Diminished)
(check-expect (triad-type 'e 'g 'b#) 'Augmented)