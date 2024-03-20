;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 03, Question 3
;; ***************************************************
;;


;; (List->parts lst) consumes a list and returns each part of the
;; list as lst_first, lst_second, and lst_third respectfully

(define (lst_first lst)
  (first lst))

(define (lst_second lst)
  (first (rest lst)))

(define (lst_third lst)
  (first (rest (rest lst))))

;; Examples
(check-expect (lst_first (cons 0 (cons 1 (cons 3 empty)))) 0)
(check-expect (lst_second (cons 0 (cons 1 (cons 3 empty)))) 1)
(check-expect (lst_third (cons 0 (cons 1 (cons 3 empty)))) 3)



;; A State is a (cons Nat (cons Nat (cons Sym empty)))
;; requires: the numbers to be be between 0 and 10.
;; it also requires the symbol to be 'North, 'South, 'East, or 'West.

;; (robot-ctl state cmd) consumes a State and a
;; command and produces a new State.

;; Example:

(check-expect (robot-ctl (cons 1 (cons 0 (cons 'North empty))) 'forward)
              (cons 1 (cons 1 (cons 'North empty))))
;; robot-ctl: State Sym -> State

(define (robot-ctl state cmd)
  (cond
    [(symbol=? (lst_third state) 'West)
           (cond
             [(and (> (lst_first state) 0) (symbol=? cmd 'forward))
                   (cons (- (lst_first state) 1) (cons (lst_second state)
                                                       (cons (lst_third state) empty)))]
             [(symbol=? cmd 'turn-left) (cons (lst_first state) (cons (lst_second state)
                                                                      (cons 'South empty)))]
             [(symbol=? cmd 'turn-right) (cons (lst_first state) (cons (lst_second state)
                                                                       (cons 'North empty)))]
             [else (cons (lst_first state) (cons (lst_second state)
                                                 (cons (lst_third state) empty)))])]
        [(symbol=? (lst_third state) 'East)
           (cond
             [(and (< (lst_first state) 10) (symbol=? cmd 'forward))
                   (cons (+ (lst_first state) 1)(cons (lst_second state)
                                                      (cons (lst_third state) empty)))]
             [(symbol=? cmd 'turn-left) (cons (lst_first state) (cons (lst_second state)
                                                                      (cons 'North empty)))]
             [(symbol=? cmd 'turn-right) (cons (lst_first state) (cons (lst_second state)
                                                                       (cons 'South empty)))]
             [else (cons (lst_first state) (cons (lst_second state)
                                                 (cons (lst_third state) empty)))])]
        [(symbol=? (lst_third state) 'South)
           (cond
             [(and (> (lst_second state) 0) (symbol=? cmd 'forward))
                   (cons (lst_first state) (cons (- (lst_second state) 1)
                                                 (cons (lst_third state) empty)))]
             [(symbol=? cmd 'turn-left) (cons (lst_first state) (cons (lst_second state)
                                                                      (cons 'East empty)))]
             [(symbol=? cmd 'turn-right) (cons (lst_first state) (cons (lst_second state)
                                                                       (cons 'West empty)))]
             [else (cons (lst_first state) (cons (lst_second state)
                                                 (cons (lst_third state) empty)))])]
        [(symbol=? (lst_third state) 'North)
           (cond
             [(and (< (lst_second state) 10) (symbol=? cmd 'forward))
                   (cons (lst_first state) (cons (+ (lst_second state) 1)
                                                 (cons (lst_third state) empty)))]  
             [(symbol=? cmd 'turn-left) (cons (lst_first state) (cons (lst_second state)
                                                                      (cons 'West empty)))]
             [(symbol=? cmd 'turn-right) (cons (lst_first state) (cons (lst_second state)
                                                                       (cons 'East empty)))]
             [else (cons (lst_first state) (cons (lst_second state)
                                                 (cons (lst_third state) empty)))])]))

;; Tests:


(check-expect (robot-ctl (cons 5 (cons 5 (cons 'East empty))) 'forward)
              (cons 6 (cons 5 (cons 'East empty))))
(check-expect (robot-ctl (cons 10 (cons 5 (cons 'East empty))) 'forward)
              (cons 10 (cons 5 (cons 'East empty))))
(check-expect (robot-ctl (cons 1 (cons 0 (cons 'North empty))) 'turn-left)
              (cons 1 (cons 0 (cons 'West empty))))
(check-expect (robot-ctl (cons 1 (cons 0 (cons 'North empty))) 'turn-right)
              (cons 1 (cons 0 (cons 'East empty))))
(check-expect (robot-ctl (cons 10 (cons 0 (cons 'West empty))) 'forward)
              (cons 9 (cons 0 (cons 'West empty))))
(check-expect (robot-ctl (cons 5 (cons 5 (cons 'West empty))) 'forward)
              (cons 4 (cons 5 (cons 'West empty))))
(check-expect (robot-ctl (cons 1 (cons 5 (cons 'North empty))) 'turn-left)
              (cons 1 (cons 5 (cons 'West empty))))
(check-expect (robot-ctl (cons 1 (cons 5 (cons 'East empty))) 'turn-left)
              (cons 1 (cons 5 (cons 'North empty))))
(check-expect (robot-ctl (cons 1 (cons 5 (cons 'South empty))) 'turn-left)
              (cons 1 (cons 5 (cons 'East empty))))
(check-expect (robot-ctl (cons 1 (cons 5 (cons 'West empty))) 'turn-left)
              (cons 1 (cons 5 (cons 'South empty))))
(check-expect (robot-ctl (cons 10 (cons 10 (cons 'North empty))) 'turn-right)
              (cons 10 (cons 10 (cons 'East empty))))
(check-expect (robot-ctl (cons 10 (cons 10 (cons 'East empty))) 'turn-right)
              (cons 10 (cons 10 (cons 'South empty))))
(check-expect (robot-ctl (cons 10 (cons 10 (cons 'South empty))) 'turn-right)
              (cons 10 (cons 10 (cons 'West empty))))
(check-expect (robot-ctl (cons 10 (cons 10 (cons 'West empty))) 'turn-right)
              (cons 10 (cons 10 (cons 'North empty))))
(check-expect (robot-ctl (cons 0 (cons 0 (cons 'North empty))) 'forward)
              (cons 0 (cons 1 (cons 'North empty))))
(check-expect (robot-ctl (cons 0 (cons 10 (cons 'North empty))) 'forward)
              (cons 0 (cons 10 (cons 'North empty))))
(check-expect (robot-ctl (cons 0 (cons 8 (cons 'South empty))) 'forward)
              (cons 0 (cons 7 (cons 'South empty))))
(check-expect (robot-ctl (cons 0 (cons 0 (cons 'South empty))) 'forward)
              (cons 0 (cons 0 (cons 'South empty))))
