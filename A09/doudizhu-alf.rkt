;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname doudizhu-alf) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 09, question 4
;; ***************************************************
;;

;; A CardCountAL (CCAL) is one of:
;; * empty
;; * (cons (list Card Nat) CardCountAL)
;; Requires:
;; Card is a unique key and Nat > 0

;; ~~~~~ 4a


(define (hand->ccal hand)
  (map (lambda (x) (list x
                         (- (length hand) (length (filter false? (map (lambda (g) (equal? x g))
                                                                      hand))))))
       (foldl (lambda (a b) (cond
                              [(list? b)
                               (cond
                                 [(not (= (length b) (length (filter false?
                                                                     (map (lambda (c) (equal? a c))
                                                                          b))))) b]
                                 [else (append b (list a))])]
                              [(= a b) (cons a empty)]
                              [else (list b a)])) (first hand) (rest hand))))

;;(define (occ lon num)
;             (foldl (lambda (x y) (cond
;                                    [(equal? x num) (+ y 1)]
;                                    [else y])) 0 lon))


;(define (dedup lon)
;  (foldr (lambda (x y) (cons x (filter (lambda (z) (not (= x z))) y))) empty lon))


;; ~~~~~ 4b

(define (find-kind n ccal)
  (map first
       (filter (lambda (x)
                 (and (list? x)
                      (or (symbol? (first x))
                          (number? (first x)))
                      (number? (second x))
                      (>= (second x) n)))
               ccal)))

(check-expect (find-kind 2 (list (list 'Ace 4) (list 'Jack 3) (list 5 1) (list 6 1)))
              (list 'Ace 'Jack))
(check-expect (find-kind 2 (list (list 3 4) (list 4 3) (list 5 1) (list 6 1))) (list 3 4))


;; ~~~~~ 4c

(define (trios ccal)
  (local
    [
     (define (trios-helper ccal)
       (map first
            (filter (lambda (x)
                      (and (list? x)
                           (or (symbol? (first x))
                               (number? (first x)))
                           (number? (second x))
                           (= (second x) 3)))
                    ccal)))]
    (map (lambda (x) (list x x x)) (trios-helper ccal))))


;; ~~~~~ 4d

(define (ccal->hand ccal)
  (local [
          (define (card-foldr-n combine card base n)
            (cond [(zero? n) base]
                  [else (combine card n (card-foldr-n combine card base (sub1
                                                                         n)))]))]
    (foldr (lambda (x y) (append (card-foldr-n (lambda (x y z) (cons x z)) (first x) empty
                                               (second x)) y)) empty ccal)))


;; ~~~~~ 4e

(define (remove-one-of-each hand)
  (reverse
   (rest
    (foldr (lambda (x y) (cond
                           [(cons? y) (cond
                                        [(equal? x (first y)) (append (list x) y)]
                                        [else (append (list x) (rest y))])]
                           [else (cond
                                   [(equal? x y) (list x y)]
                                   [else (list y)])]))
           (first hand) (reverse (rest hand))))))

(check-expect (remove-one-of-each
               (list 3 3 3 3 4 5 6 7 7 7 'Jack 'Jack 'Queen 'King 'Ace 2 2))
              (list 3 3 3 7 7 'Jack 2))

;; ~~~~~ 4f

(define (remove-one-of-each2 hand)
  (ccal->hand (map (lambda (y) (list (first y) (sub1 (second y))))
                   (filter (lambda (x) (> (second x) 1)) (hand->ccal hand)))))

(check-expect (remove-one-of-each2
               (list 3 3 3 3 4 5 6 7 7 7 'Jack 'Jack 'Queen 'King 'Ace 2 2))
              (list 3 3 3 7 7 'Jack 2))