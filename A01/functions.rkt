;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen (21071183)
;; CS 135 Fall 2023
;; Assignment 01, Problem 2
;; ***************************************************
;;

;; manhattan distance implementation

(check-expect (manhattan-distance 1 2 3 4) 4)

(define (manhattan-distance x1 y1 x2 y2)
  (+ (abs (- x1 x2)) (abs( - y1 y2))))

;; batters slugging average

(check-expect (batter-slugging-average 3 3 4 1 3) 25/3)

(define (batter-slugging-average s d t hr ab)
  (/ (+ s (* 2 d) (* t 3) (* hr 4)) ab))

;; cone area ****

(check-within (cone-area 8 9) 503.7 0.01)

(define (cone-area r h)
  (* pi r (+ r (sqrt (+ (sqr h) (sqr r))))))

;; escape speed

(define G 6.674e-11)

(define (escape M r)
  (sqrt (/ (* 2 G M) r)))

;; partition function

(define (partition-size-approximation n)
  (* (/ 1 (* 4 n (sqrt 3))) (exp (* pi (sqrt(/(* 2 n) 3))))))


;; black-scholes formula

(define (d1 maturity rate volatility spot-price strike-price)
  (* (/ 1 (* volatility (sqrt maturity))) (* (+ (log (/ spot-price strike-price)) (+ rate (/ (sqr volatility) 2))) maturity)))