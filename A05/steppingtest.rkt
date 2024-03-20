;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname steppingtest) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))


;; Hand is one of
;; * empty
;; * Sorted (listof Card)

(define cards
  (list 3 4 5 6 7 8 9 10 'Jack 'Queen 'King 'Ace 2 'Black 'Red))


;; (remove-duplicates loa) consumes a list of Any and produces the same
;; list, with all but the last occurrence of each element removed.

(define (remove-duplicates loa)
  (cond
    [(empty? loa) empty]
    [(member? (first loa) (rest loa)) (remove-duplicates (rest loa))]
    [else
     (cons
      (first loa) (remove-duplicates (rest loa)))]))



;; 3b ~~~~~

;; (pairs hand) consumes a Hand and produces a (listof Hand) containing all the pairs that can be
;; constructed from that hand

;; Examples:

(check-expect
 (pairs (list 3 3 3 3 4 5 6 7 7 7
              'Jack 'Jack 'Queen 'King 'Ace 2 2))
 (list (list 3 3) (list 7 7) (list 'Jack 'Jack) (list 2 2)))

;; pairs: Hand -> (listof Hand)

(define (pairs hand)
  (cond
    [(or (= (length hand) 1) (empty? hand)) empty]
    [(card=? (first hand) (second hand))
     (remove-duplicates (cons (list (first hand) (second hand)) (pairs (rest hand))))]
    [else (pairs (rest hand))]))

;; Tests:

(check-expect
 (pairs
  (list 4 4 5 5 5 6 9 10 10 'Jack 'King 'King 'Ace 'Black 'Black 'Black))
 (list (list 4 4) (list 5 5) (list 10 10) (list 'King 'King) (list 'Black 'Black)))


(define (card? val)
  (cond
    [(member? val cards) true]
    [else false]))


(define (card=? card1 card2)
  (cond
    [(and (symbol? card1) (symbol? card2)) (cond
                                             [(and (symbol=? card1 card2) (card? card1)) true]
                                             [else false])]
    [(and (number? card1) (number? card2)) (cond
                                             [(and (= card1 card2) (card? card1)) true]
                                             [else false])]                                
    [else false]))
