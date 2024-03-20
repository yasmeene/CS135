;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname collide) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 03, Question 4
;; ***************************************************
;;


;; (List->parts lst) consumes a list and returns each part of the
;; list as lst_first, lst_second, lst_third, and lst_fourth respectfully

(define (lst_first lst)
  (first lst))

(define (lst_second lst)
  (first (rest lst)))

(define (lst_third lst)
  (first (rest (rest lst))))

(define (lst_fourth lst) 
  (first (rest (rest (rest lst)))))


;; Examples
(check-expect (lst_first (cons 0 (cons 1 (cons 3 empty)))) 0)
(check-expect (lst_second (cons 0 (cons 1 (cons 3 empty)))) 1)
(check-expect (lst_third (cons 0 (cons 1 (cons 3 empty)))) 3)

;; (sphere->parts) consumes a sphere and produces each part of the sphere
;; sphere->parts: (listof Nat) -> (listof Nat)

(define (e1 sphere)
  (first (first sphere)))

(define (e2 sphere)
  (first (rest (first sphere))))

(define (e3 sphere)
  (first (rest (rest (first sphere)))))

(define (sphere_r sphere)
  (first (rest sphere)))

(define (p sphere)
  (cons (first (first sphere)) (cons (first (rest (first sphere)))
                                     (cons (first (rest (rest (first sphere)))) empty))))
;; Examples:
(check-expect (e1 (cons (cons 1 (cons 2 (cons 3 empty))) (cons 4 empty))) 1)

;; (build-sphere sphere_lst) consumes a list of four numbers as (cons
;; x (cons y (cons z (cons r empty)))) and produces a sphere that has the form
;; (cons (cons x (cons y (cons z empty))) (cons r empty))

;;Examples:

(check-expect (build-sphere (cons 1 (cons 2 (cons 3 (cons 4 empty)))))
              (cons (cons 1 (cons 2 (cons 3 empty))) (cons 4 empty)))

;; build-sphere: (listof Nat) -> (listof Nat) (Listof Nat) *****
;; requirement: radius of sphere must be greater than 0

(define (build-sphere sphere_lst)
  (cons (cons (lst_first sphere_lst)
        (cons (lst_second sphere_lst)
              (cons (lst_third sphere_lst) empty))) (cons (lst_fourth sphere_lst) empty)))

;; Test:

(check-expect (build-sphere (cons 2 (cons 5 (cons 6 (cons 7 empty)))))
              (cons (cons 2 (cons 5 (cons 6 empty))) (cons 7 empty)))
(check-expect (build-sphere (cons 6 (cons 1 (cons 0 (cons 0 empty)))))
              (cons (cons 6 (cons 1 (cons 0 empty))) (cons 0 empty)))
(check-expect (build-sphere (cons -1 (cons 0 (cons 0 (cons 10 empty)))))
              (cons (cons -1 (cons 0 (cons 0 empty))) (cons 10 empty)))



;; (valid-sphere? sphere_lst) consumes a list as (cons (cons x (cons y (cons z empty)))
;; (cons r empty)) and produces true if that list is a valid sphere and returns false otherwise.

;; Examples:

(check-expect (valid-sphere? (cons (cons 1 (cons 2 (cons 3 empty))) (cons 4 empty))) true)
(check-expect (valid-sphere? (cons (cons 3 (cons 1 (cons 6 empty))) (cons -2 empty))) false)


;; valid-sphere?: (listof Nat) -> Bool

(define (valid-sphere? sphere_lst)
  (cond
   [(> (lst_second sphere_lst) 0) true]
   [else false]))


;; Test:

(check-expect (valid-sphere? (cons (cons 0 (cons 0 (cons 0 empty))) (cons 0 empty))) false)
(check-expect (valid-sphere? (cons (cons 3 (cons 2 (cons 2 empty))) (cons 6 empty))) true)



;; (distance-between-points p1 p2) consumes two points and produces the distance between them

;; Examples:

(check-within (distance-between-points (cons 1 (cons 2 (cons 3 empty)))
                                       (cons 4 (cons 5 (cons 6 empty)))) 5.19 0.1)

;; distance-between-points: (listof Nat) (listof Nat) -> Nat

(define (distance-between-points p1 p2)
  (sqrt (+ (sqr (- (lst_first p2) (lst_first p1)))
           (sqr (- (lst_second p2) (lst_second p1)))
           (sqr (- (lst_third p2) (lst_third p1))))))

;; Test:

(check-within (distance-between-points (cons 5 (cons 3 (cons 2 empty)))
                                       (cons 4 (cons 5 (cons 8 empty)))) 6.4 0.1)
(check-within (distance-between-points (cons 18 (cons 11 (cons 12 empty)))
                                       (cons 24 (cons 31 (cons 8 empty)))) 21.3 0.1)


;; (point-in-sphere? point sphere) consumes a point and a sphere and
;; produces true if the point is inside the sphere and false otherwise

;; Examples:

(check-expect (point-in-sphere? (cons 1 (cons 2 (cons 3 empty)))
                                (cons (cons 4 (cons 3 (cons 2 empty))) (cons 1 empty))) false)


;; point-in-sphere?: (listof Nat) (listof Nat) -> Bool
(define (point-in-sphere? point sphere)
  (cond
    [(<= (+ (sqr(- (lst_first point) (lst_first (lst_first sphere))))
            (sqr(- (lst_second point) (lst_second (lst_first sphere))))
            (sqr(- (lst_third point) (lst_third (lst_first sphere)))))
         (sqr (lst_second sphere))) true]
    [else false]))


;; Tests:

(check-expect (point-in-sphere? (cons 1 (cons 2 (cons 3 empty)))
                                (cons (cons 1 (cons 2 (cons 3 empty))) (cons 4 empty))) true)

(check-expect (point-in-sphere? (cons 1 (cons 2 (cons 3 empty)))
                                (cons (cons 6 (cons 7 (cons 8 empty))) (cons 2 empty))) false)




;; (collide? sphere1 sphere2)  consumes two spheres and produces true if the
;; spheres collide and false otherwise.

;; Examples:
(check-expect (collide? (cons (cons 2 (cons 6 (cons 7 empty))) (cons 8 empty))
                        (cons (cons 3 (cons 4 (cons 1 empty))) (cons 2 empty))) true)

;; collide?: (listof Nat) (listof Nat) -> Bool
;; requirement: radius of sphere must be greater than 0


(define (collide? sphere1 sphere2)
  (cond
    [(< (distance-between-points (p sphere1) (p sphere2))
        (+ (sphere_r sphere1) (sphere_r sphere2))) true]
    [else false]))


;; Tests:

(check-expect (collide? (cons (cons 0 (cons 0 (cons 0 empty))) (cons 1 empty))
                        (cons (cons 0 (cons 0 (cons 0 empty))) (cons 2 empty))) true)
(check-expect (collide? (cons (cons 3 (cons 4 (cons 7 empty))) (cons 8 empty))
                        (cons (cons 2 (cons 6 (cons 1 empty))) (cons 2 empty))) true)
(check-expect (collide? (cons (cons 3 (cons 4 (cons 7 empty))) (cons 1 empty))
                        (cons (cons 2 (cons 6 (cons 1 empty))) (cons 2 empty))) false)