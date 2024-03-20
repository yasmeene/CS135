;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname signmag) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 06, question 2
;; ***************************************************
;;

;; ~~~~ 2a

(define-struct signmag (sign mag))
;; A SignMag is a (make-signmag Sym Num)
;; requires: sign is one of 'positive, 'negative, 'zero
;;           mag > 0
;;           if sign is 'zero, then mag = 1


;; ~~~~ 2b

;; signmag-template: SignMag -> Any

(define (signmag-template num)
  (... (signmag-sign num)
       (signmag-mag num)))

;; ~~~~ 2c

;; (num->signmag num) consumes a number and produces its equivalent SignMag value

;; Example:

(check-expect (num->signmag 0) (make-signmag 'zero 1))

;; num->signmag: Num -> SignMag

(define (num->signmag num)
  (cond
    [(zero? num) (make-signmag 'zero 1)]
    [(< num 0) (make-signmag 'negative (abs num))]
    [else (make-signmag 'positive num)]))

;; Tests:

(check-expect (num->signmag -3) (make-signmag 'negative 3))
(check-expect (num->signmag 19) (make-signmag 'positive 19))


;; ~~~~ 2d

;; (signmag->num SM) consumes a SignMag structure and produces its equivalent number

;; Example:

(check-expect (signmag->num (make-signmag 'zero 1)) 0)

;; signmag->num: SignMag -> Num

(define (signmag->num SM)
  (cond
    [(symbol=? 'zero (signmag-sign SM)) 0]
    [(symbol=? 'positive (signmag-sign SM)) (signmag-mag SM)]
    [else (- (signmag-mag SM))]))

;; Tests:

(check-expect (signmag->num (make-signmag 'negative 5)) -5 )
(check-expect (signmag->num (make-signmag 'positive 19)) 19)