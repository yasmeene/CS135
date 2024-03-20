;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname examples-a10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 10, check-expect
;; ***************************************************
;;

;; 1a, function adj->edge

(check-expect (adj->edge '((A (C D E)) (B (E J)) (C ()) (D (F J))
                                       (E (K)) (F (K H)) (H ()) (J (H)) (K ())))
              '((A C) (A D) (A E) (B E) (B J) (D F) (D J)
                      (E K) (F K) (F H) (J H)))

(check-expect (adj->edge '((A (D E)) (B (E J)) (C ()) (D (F J))
                                       (E (K)) (F (K H)) (H ()) (J (H)) (K ())))
              '((A D) (A E) (B E) (B J) (D F) (D J)
                      (E K) (F K) (F H) (J H)))

(check-expect (adj->edge '((A (D E)) (B (E J)) (C ()) (D (F J))
                                       (E (K)) (F (K H)) (H ()) (J (H)) (K ()) (L ())))
              '((A D) (A E) (B E) (B J) (D F) (D J)
                      (E K) (F K) (F H) (J H)))

;; 1b, function neighbours

(check-expect (neighbours A '((A D) (A E) (B E) (B J) (D F) (D J)
                      (E K) (F K) (F H) (J H)))
              '(D E))

(check-expect (neighbours B '((A C) (A D) (A E) (B E) (B J) (D F) (D J)
                      (E K) (F K) (F H) (J H)))
              '(E J))

(check-expect (neighbours J '((A C) (A D) (A E) (B E) (B J) (D F) (D J)
                      (E K) (F K) (F H) (J H)))
              '(H))

(check-expect (neighbours L '((A C) (A D) (A E) (B E) (B J) (D F) (D J)
                      (E K) (F K) (F H) (J H)))
              '())

;; 1c, function edge->adj

(check-expect (edge->adj '((A C) (A D) (A E) (B E) (B J) (D F) (D J)
                                (E K) (F K) (F H) (J H)))
              '((A (C D E)) (B (E J)) (C ()) (D (F J))
                            (E (K)) (F (K H)) (H ()) (J (H)) (K ())))

(check-expect (edge->adj '((A D) (A E) (B E) (B J) (D F) (D J)
                                 (E K) (F K) (F H) (J H)))
              '((A (D E)) (B (E J)) (C ()) (D (F J))
                          (E (K)) (F (K H)) (H ()) (J (H)) (K ())))

(check-expect (edge->adj '()) '())

;; 2a, function primes

(check-expect (primes 20) '(2 3 5 7 11 13 17 19))
(check-expect (primes 10) '(2 3 5 7))
(check-expect (primes 30) '(2 3 5 7 11 13 17 19 23 29))
(check-expect (primes 5) '(2 3 5))
(check-expect (primes 2) '(2))
(check-expect (primes 1) '())


;; 2b, function goldbach

(check-expect (goldbach 14) 2)
(check-expect (goldbach 10000) 127)
(check-expect (goldbach 24) 3)
(check-expect (goldback 500) 13)
