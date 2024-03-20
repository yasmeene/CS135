;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname cardgame) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 04, Question 3
;; ***************************************************
;;

;; (cards) returns the list of cards used to play Dou Dizhu
(define cards
  (cons 3
        (cons 4
              (cons 5
                    (cons 6
                          (cons 7
                                (cons 8
                                      (cons 9
                                            (cons 10
                                                  (cons 'Jack
                                                        (cons 'Queen
                                                              (cons 'King
                                                                    (cons 'Ace
                                                                          (cons 2
                                                                                (cons 'Black
                                                                                      (cons 'Red
                                                                                            empty)
                                                                                      )))))))))))))))


;; (card? val) consumes any value and produces true if the value is a valid Card; false otherwise

;; Examples:

(check-expect (card? 'Jack) true)
(check-expect (card? "3") false)

;; card?: Any -> Bool

(define (card? val)
  (cond
    [(member? val cards) true]
    [else false]))

;; Tests:

(check-expect (card? 'Spade) false)
(check-expect (card? 5) true)


;; (card=? card1 card2) consumes two Cards and produces true if they are the same card;
;; false otherwise

;; Examples: 

(check-expect (card=? 3 3) true)
(check-expect (card=? 'Jack 'King) false)

;; card=?: Card Card -> Bool

(define (card=? card1 card2)
  (cond
    [(and (symbol? card1) (symbol? card2)) (cond
                                             [(and (symbol=? card1 card2) (card? card1)) true]
                                             [else false])]
    [(and (number? card1) (number? card2)) (cond
                                             [(and (= card1 card2) (card? card1)) true]
                                             [else false])]                                
    [else false]))


;; Tests:

(check-expect (card=? 1 'Queen) false)
(check-expect (card=? 'Black "Black") false)
(check-expect (card=? 'Blue 'Orange) false)
(check-expect (card=? 'King 'King) true)
(check-expect (card=? 11 13) false)


;; (sort-cards loc) consumes a list of Card and produces a list of Card
;; sorted in increasing Dou Dizhu order

;; Examples:

(check-expect (sort-cards
              (list 3 'King 6 7 'Jack 'Queen 2 7 3 7 3 'Ace 'Jack 2 3 4 5))
              (list 3 3 3 3 4 5 6 7 7 7 'Jack 'Jack 'Queen 'King 'Ace 2 2))

;; sort-cards: (listof Card) -> (listof Card)

(define (sort-cards loc)
  (cond
    [(empty? loc) empty]
    [else (insert (first loc)
               (sort-cards (rest loc)))]))

;; Tests:

(check-expect (sort-cards
               (list 4 4 4 'Jack 'Queen 2 7 'Ace 5 6))
              (list 4 4 4 5 6 7 'Jack 'Queen 'Ace 2))
(check-expect (sort-cards
               (list 'Jack 'King 2 5 5 8 8 3 4 'Red 'Black))
               (list 3 4 5 5 8 8 'Jack 'King 2 'Black 'Red))


;; (card->num card) consumes a card value and assigns it a value

;; Examples

(check-expect (card->num 'Jack) 9)
(check-expect (card->num 2) 13)
(check-expect (card->num 'Red) 15)
(check-expect (card->num 'Black) 14)
(check-expect (card->num 8) 6)
(check-expect (card->num 9) 7)
(check-expect (card->num 10) 8)

;; card->num: Card -> Nat
(define (card->num card)
  (cond
    [(symbol? card) (cond
                      [(symbol=? 'Jack card) 9]
                      [(symbol=? 'Queen card) 10]
                      [(symbol=? 'King card) 11]
                      [(symbol=? 'Ace card) 12]
                      [(symbol=? 'Black card) 14]
                      [else 15])]
    [(= 3 card) 1]
    [(= 4 card) 2]
    [(= 5 card) 3]
    [(= 6 card) 4]
    [(= 7 card) 5]
    [(= 8 card) 6]
    [(= 9 card) 7]
    [(= 10 card) 8]
    [else 13]))


;; (insert n slon) inserts the number n into the sorted list slon

;; Examples:
(check-expect (insert 2 (list)) (list 2))
(check-expect (insert 2 (list 3 4)) (list 3 4 2)) 
(check-expect (insert 4 (list 3 5)) (list 3 4 5))
(check-expect (insert 2 (list 3 4)) (list 3 4 2))


;; insert: Card (listof Num) -> (listof Num)
;; Requires: slon is sorted in non-decreasing order

(define (insert n slon)
  (cond
    [(empty? slon) (cons n empty)]
    [(<= (card->num n) (card->num (first slon))) (cons n slon)]
    [else (cons (first slon) (insert n (rest slon)))]))


;; (remove-one-of-each sloc) consumes a sorted list of Card and produces
;; a sorted list of Card where one occurrence of each card found in the consumed list of
;; Card has been removed

;; Examples:

(check-expect (remove-one-of-each
               (list 3 3 3 3 4 5 6 7 7 7 'Jack 'Jack 'Queen 'King 'Ace 2 2))
              (list 3 3 3 7 7 'Jack 2))

;; remove-one-of-each: (listof Card) -> (listof Card)

(define (remove-one-of-each sloc)
  (cond
    [(empty? sloc) empty]
    [(member? (first sloc) (rest sloc))
     (cons
      (first sloc) (remove-one-of-each (rest sloc)))]
    [else (remove-one-of-each (rest sloc))]))


;; Tests:

(check-expect (remove-one-of-each
               (list 3 3 3 3 4 5 6 7 7 7 'Jack 'Jack 'Queen 'King 'Ace 2 2))
              (list 3 3 3 7 7 'Jack 2))


;; (find-kind n sloc): consumes a natural number n that is greater than or
;; equal to 1 and a sorted list of Card and produces a sorted list of Card containing the
;; card values with at least n occurrences in the consumed list of Card.

;; Examples:

(check-expect
 (find-kind 3 (list 3 3 3 3 4 5 6 7 7 7 'Jack 'Jack 'Queen 'King
                    'Ace 2 2))
 (list 3 7))

;; find-kind: Nat (listof Card) -> (listof Card)) 

(define (find-kind n sloc)
  (cond
    [(empty? sloc) empty]
    [(card=? n (n-occur (first sloc) sloc))
     (cons (first sloc)
           (find-kind n (rest sloc)))]
    [else (find-kind n (rest sloc))]))

;; (n-occur n sloc) consumes a natural number n and a sorted list of Card and produces
;; the number of times n appears in the list

;; Example:

(check-expect (n-occur 4 (list 3 3 3 4 4 4 4)) 4)

;; n-occur: Nat (listof Card) -> Nat

(define (n-occur n sloc)
  (cond
    [(empty? sloc) 0]
    [(card=? n (first sloc)) (+ 1 (n-occur n (rest sloc)))]
    [else (n-occur n (rest sloc))]))