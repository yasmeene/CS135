;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname doudizhu) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 05, question 3
;; ***************************************************
;;


;; Hand is one of
;; * empty
;; * Sorted (listof Card)

(define cards
  (list 3 4 5 6 7 8 9 10 'Jack 'Queen 'King 'Ace 2 'Black 'Red))

;; 3a ~~~~~

;; (solos hand) consumes a Hand and produces a (listof Hand), where each produced
;; hand consists of a single card from the consumed hand.

;; Examples:

(check-expect
 (solos
  (list 3 3 3 3 4 4 5 5 6 7 7 7 9
        'Jack 'Jack 'Queen 'King 'Ace 2 2))
 (list (list 3) (list 4) (list 5) (list 6) (list 7)
       (list 9) (list 'Jack) (list 'Queen) (list 'King)
       (list 'Ace) (list 2)))

(check-expect
 (solos
  (list 4 4 5 5 5 6 9 10 10 'Jack 'King 'King 'Ace 'Black 'Black 'Black))
 (list (list 4) (list 5) (list 6) (list 9) (list 10)
       (list 'Jack) (list 'King) (list 'Ace) (list 'Black)))

;; solos: Hand -> (listof Hand)

(define (solos hand)
  (cond
    [(empty? hand) empty] ; Return an empty list if the input list is empty
    [else
     (cons (list (first (remove-duplicates hand)))
           (solos (rest (remove-duplicates hand))))]))

;; Tests:

(check-expect
 (solos
  (list 4 5 5 5 6 7 7 7 9 10 10 'Jack 'King 'King 'Ace 'Black 'Black 'Black))
 (list (list 4) (list 5) (list 6) (list 7) (list 9) (list 10)
       (list 'Jack) (list 'King) (list 'Ace) (list 'Black)))

(check-expect
 (solos
  (list 5 5 5 6 6 9 10 10 'Jack 'Jack 'King 'King 'Ace 2 2 'Black 'Black 'Red))
 (list (list 5) (list 6) (list 9) (list 10)
       (list 'Jack) (list 'King) (list 'Ace) (list 2) (list 'Black) (list 'Red)))

(check-expect (solos (list 3 3 3 5 5 7 7 'Jack 'Ace 2 2))
              (list (list 3) (list 5) (list 7) (list 'Jack) (list 'Ace) (list 2)))

(check-expect (solos (list 'Jack 'Jack 'Jack 'Queen 'King 'King 'Ace 2))
              (list (list 'Jack) (list 'Queen) (list 'King) (list 'Ace) (list 2)))

;; (remove-duplicates loa) consumes a list of Any and produces the same
;; list, with all but the last occurrence of each element removed.

(define (remove-duplicates loa)
  (cond
    [(empty? loa) empty]
    [(member? (first loa) (rest loa)) (remove-duplicates (rest loa))]
    [else
     (cons
      (first loa) (remove-duplicates (rest loa)))]))


;; Tests:

(check-expect (remove-duplicates
               (cons 3 (cons 3 (cons 1 (cons 4 (cons 1 (cons 4 empty)))))))
              (cons 3 (cons 1 (cons 4 empty))))


;; 3b ~~~~~ ***** optimize 

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

(check-expect (pairs (list 4 4 4 8 9 9 9 'Ace 'Ace))
              (list (list 4 4) (list 9 9) (list 'Ace 'Ace)))

(check-expect (pairs (list 3 4 5 6 7 8 9 'Ace 2 2))
              (list (list 2 2)))
(check-expect (pairs (list 'Black)) empty)


(define (card? val)
  (cond
    [(member? val cards) true]
    [else false]))

;; Tests:


(check-expect (card? 'Spade) false)
(check-expect (card? 5) true)


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


;; (trios hand) consumes a Hand and produces a (listof Hand) containing all the trios that can be
;; constructed

;; Examples:

(check-expect
 (trios (list 3 3 3 4 5 5 5 6 6 6 6 7 7 7 8 8 9 'Queen 'Queen))
 (list (list 3 3 3) (list 5 5 5) (list 6 6 6) (list 7 7 7)))

(check-expect (trios (list 3 5 7 9 'Jack 'Jack 'Jack 'Ace 'Ace))
              (list (list 'Jack 'Jack 'Jack)))

;; trios: Hand -> (listof Hand)

(define (trios hand)
    (cond
    [(empty? hand) empty]
    [(member? (first hand) (find-kind 3 hand))
     (remove-duplicates (cons (list (first hand) (first hand) (first hand))
           (trios (rest hand))))]
    [else (trios (rest hand))]))

;; Tests:

(check-expect
 (trios
  (list 4 4 5 5 5 6 9 9 9 10 10 'Jack 'King 'King 'Ace 'Ace 'Ace 'Black 'Black 'Black))
 (list (list 5 5 5) (list 9 9 9) (list 'Ace 'Ace 'Ace) (list 'Black 'Black 'Black)))

(check-expect (trios (list 4 4 4 5 5 9 9 9 'King 'King 'King 'Ace 2 2 2))
              (list (list 4 4 4) (list 9 9 9) (list 'King 'King 'King) (list 2 2 2)))


;; find-kind: Nat (listof Card) -> (listof Card)) 

(define (find-kind n sloc)
  (cond
    [(empty? sloc) empty]
    [(card=? n (n-occur (first sloc) sloc))
     (cons (first sloc)
           (find-kind n (rest sloc)))] 
    [else (find-kind n (rest sloc))]))

;; Test:
(check-expect
 (find-kind 3 (list 3 3 3 3 4 5 6 7 7 7 'Jack 'Jack 'Queen 'King
                    'Ace 2 2))
 (list 3 7))


;; n-occur: Nat (listof Card) -> Nat

(define (n-occur n sloc)
  (cond
    [(empty? sloc) 0]
    [(card=? n (first sloc)) (+ 1 (n-occur n (rest sloc)))]
    [else (n-occur n (rest sloc))]))

;; Test:
(check-expect (n-occur 4 (list 3 3 3 4 4 4 4)) 4)


;; 3d ~~~~~

;; (sort-hands hands) consumes a (listof Hand) and produces
;; a sorted (listof Hand) containing the same hands, excluding duplicates.

;; Examples:

(check-expect
 (sort-hands (list (list 'Jack 'Queen 'King) (list 3 3 3) (list 4 4)
                   (list 'Black 'Red) (list 4 4) (list 5 5 5)
                   (list 3 4 5) (list 3 3 3)))
 (list (list 4 4) (list 3 3 3) (list 3 4 5) (list 5 5 5)
       (list 'Jack 'Queen 'King) (list 'Black 'Red)))

;; sort-hands: (listof Hand) -> (listof Hand)

(define (sort-hands hands)
  (remove-duplicates (insertion-sort hands empty)))

;; Tests:
(check-expect
 (sort-hands (list (list 'Black 'Red) (list 8 8 8 8) (list 2 2) (list 4 4) (list 9 9 9)
                   (list 4 4) (list 'Black 'Red)))
             (list (list 4 4) (list 2 2) (list 9 9 9) (list 8 8 8 8) (list 'Black 'Red)))

(check-expect
 (sort-hands (list (list 'Black 'Red) (list 7 8 9) (list 2 2) (list 4 4) (list 7 9 9)
                   (list 4 4) (list 'Black 'Red) (list 'Ace 'Ace 'Ace 'Ace)
                   (list 'Red 'Red 'Red 'Red)))
             (list (list 4 4) (list 2 2) (list 7 8 9) (list 7 9 9) (list 'Ace 'Ace 'Ace 'Ace)
                   (list 'Red 'Red 'Red 'Red) (list 'Black 'Red)))



;; insertion-sort

(define (insertion-sort lst sorted)
  (cond
    [(empty? lst) sorted]
    [else (insert (first lst) (insertion-sort (rest lst) sorted))]))

(define (insert element sorted)
  (cond
    [(empty? sorted) (list element)]
    [(hand<? element (first sorted)) (cons element sorted)]
    [else (cons (first sorted) (insert element (rest sorted)))]))


;; (bomb? hand) consumes a hand and checks if it consists of four cards of the same type

(define (bomb? hand)
         (cond
           [(and (= 4 (length hand)) (identical? hand (first hand))) true]
           [else false]))

;; Tests:

(check-expect (bomb? (list 5 5 5 5)) true)
(check-expect (bomb? (list 'Jack 2 2)) false)

;; (rocket? hand) consumes a hand and checks if it contains the two jokers, 'Black and 'Red

(define (rocket? hand)
  (cond
    [(and (= 2 (length hand)) (member? 'Black hand) (member? 'Red hand)) true]
    [else false]))

;; Tests:

(check-expect (rocket? (list 9 9)) false)
(check-expect (rocket? (list 'Black 'Red)) true)


;; (indentical? hand n) consumes a Hand and a card and checks if all the cards in Hand
;; are the same as the inputed card

(define (identical? hand n)
  (cond [(empty? hand) true]
        [(= (card->num n) (card->num (first hand))) (identical? (rest hand) n)]
        [else false]))

;; Tests:

(check-expect (identical? (list 'Black 'Black 'Red 'Red) 'Black) false)
(check-expect (identical? (list 7 7 7) 7) true)

;; (hand<? hand1 hand2) consumes two hands and checks if they are ordered
(define (hand<? hand1 hand2)
  (cond
    [(and (rocket? hand1) (rocket? hand2)) true]
    [(rocket? hand1) false]
    [(rocket? hand2) true]
    [(and (bomb? hand1) (bomb? hand2))
     (<= (card->num (first hand1)) (card->num (first hand2)))]
    [(bomb? hand1) false]
    [(bomb? hand2) true]
    [(= (length hand1) (length hand2))
     (val<=? hand1 hand2)]
    [(< (length hand1) (length hand2)) true]
    [(> (length hand1) (length hand2)) false]))

;; Tests:
(check-expect (hand<? (list 'Black 'Red) (list 'Black 'Red)) true)
(check-expect (hand<? (list 2 3) (list 3 4 5)) true)


;; (val<=? hand1 hand2) consumes two hands and checks if the values of one is less than the other
(define (val<=? hand1 hand2)
  (cond
    [(and (empty? hand1) (empty? hand2)) true]
    [(< (card->num (first hand1)) (card->num (first hand2))) true]
    [(> (card->num (first hand1)) (card->num (first hand2))) false]
    [else (val<=? (rest hand1) (rest hand2))]))

;; Tests:

(check-expect (val<=? (list 4 5 6) (list 3 4 5)) false)
(check-expect (val<=? (list 3 4 5) (list 4 5 6)) true)

;; (insert-hand hand sloh) consumes a Hand and a sorted (listof Hand) and inserts Hand to
;; the sorted list


(define (insert-hand hand sloh)
  (cond [(empty? sloh) (cons hand empty)]
        [(hand<? hand (first sloh)) (cons hand sloh)]
        [else (cons (first sloh) (insert-hand hand (rest sloh)))]))

;; Tests:

(check-expect (insert-hand (list 'Black 'Red) (list (list 4 4) (list 6 6)))
              (list (list 4 4) (list 6 6) (list 'Black 'Red)))

;; (card->num card) consumes a card value and assigns it a value

(define (card->num card)
  (cond
    [(number? card)
     (cond
       [(= card 2) 15]
       [else card])]
    [(symbol=? 'Jack card) 11]
    [(symbol=? 'Queen card) 12]
    [(symbol=? 'King card) 13]
    [(symbol=? 'Ace card) 14]
    [(symbol=? 'Black card) 16]
    [else 17]))

;; Tests:

(check-expect (card->num 'Jack) 11)
(check-expect (card->num 2) 15)
(check-expect (card->num 'Red) 17)
(check-expect (card->num 'Black) 16)
(check-expect (card->num 8) 8)
(check-expect (card->num 9) 9)
(check-expect (card->num 10) 10) 


;; 3e ~~~~

;; (straights hand) consumes a Hand and produces a (listof Hand) containing all the
;; straights that can be constructed from that hand.

;; Examples:

(check-expect
 (straights 
  (list 'Ace 2 'Black 'Red 'Red))
 empty)

;; straights: (listof Card) -> (listof Hand)

(define (straights loc) 
  (remove-duplicates
   (sort-maker
    (straight-checker
     (flatten 
      (straight-maker1 (remove-duplicates loc) 1 (length loc)))))))

;; Tests:

(check-expect
 (straights (list 3 4 4 4 5 5 6 7 8 8 'King 'King 'King 'Ace 'Ace 'Black 'Red))
 (list (list 3 4 5 6 7 ) (list 4 5 6 7 8) (list 3 4 5 6 7 8)))


;; (sort-maker hand) consumes a Hand and sorts it according to value

(define (sort-maker hands)
  (cond [(empty? hands) empty]
        [else (insert-hand (first hands) (sort-hands (rest hands)))]))


;; Tests:

(check-expect (sort-maker (list (list 2 2 2) (list 3 4 5) (list 'Black 'Black)))
              (list (list 'Black 'Black) (list 3 4 5) (list 2 2 2)))


;; straight-maker1

(define (straight-maker1 loc n len)
  (cond [(empty? loc) empty]
        [else (cons (straight-maker2 loc n len) (straight-maker1 (rest loc) n len))]))

;; Examples:

(check-expect (straight-maker1 (list 3 4 5 6 7) 1 5)
              (list
               (list
                (list 3)
                (list 3 4)
                (list 3 4 5)
                (list 3 4 5 6)
                (list 3 4 5 6 7))
               (list
                (list 4)
                (list 4 5)
                (list 4 5 6)
                (list 4 5 6 7)
                (list 4 5 6 7))
               (list
                (list 5)
                (list 5 6)
                (list 5 6 7)
                (list 5 6 7)
                (list 5 6 7))
               (list
                (list 6)
                (list 6 7)
                (list 6 7)
                (list 6 7)
                (list 6 7))
               (list
                (list 7)
                (list 7)
                (list 7)
                (list 7)
                (list 7))))

;; straight-maker2

(define (straight-maker2 loc n len)
  (cond [(or (empty? loc) (= n (+ 1 len))) empty]
        [else (cons (n-elem loc n) (straight-maker2 loc (+ n 1) len))]))

;; Tests:

(check-expect (straight-maker2 (list 3 4 5 6 7) 1 5)
              (list (list 3) (list 3 4) (list 3 4 5) (list 3 4 5 6) (list 3 4 5 6 7)))

;; n-elem

(define (n-elem loc n)
  (cond [(or (empty? loc) (= 0 n)) empty]
        [else (cons (first loc) (n-elem (rest loc) (- n 1)))]))

;; Tests:

(check-expect (n-elem (list 2 3 4 5) 3) (list 2 3 4))


(define (flatten loc)
  (cond
    [(empty? loc) empty]             
    [(list? (first loc))          
     (append (first loc) (flatten (rest loc)))]
    [else (cons (first loc) (flatten (rest loc)))]))


;; check-straights

(define (straight-checker loc)
  (cond
    [(empty? loc) empty]
    [(and
      (<= 5 (length (first loc)))
      (not (member? 2 (first loc)))
      (not (member? 'Black (first loc)))
      (not (member? 'Red (first loc)))
      (one-after-other? (first loc)))
     (cons (first loc) (straight-checker (rest loc)))]
    [else (straight-checker (rest loc))]))


;; one-after-another 
(define (one-after-other? loc)
  (cond
    [(empty? (rest loc)) true]
    [(= (- (card->num (second loc)) (card->num (first loc))) 1) (one-after-other? (rest loc))]
    [else false]))

;; Tests:
(check-expect (one-after-other? (list 3 4 5 6)) true)
(check-expect (one-after-other? (list 4 'Black 9)) false)


;; 3f ~~~~~

;; (straight-pairs hand) consumes a Hand and produces a (listof Hand)
;; containing all the straight pairs that can be constructed from that hand

;; Examples:

(check-expect
 (straight-pairs
  (list 3 3 3 4 5 5 5 6 6 6 6 7 7 7 8 8 9 'Ace 'Ace))
 (list (list 5 5 6 6 7 7) (list 6 6 7 7 8 8)
       (list 5 5 6 6 7 7 8 8)))

;; straight-pairs: Hand -> (listof Hand)

(define (straight-pairs hand)
  (remove-duplicates
   (sort-maker
    (append-loop
     (straight-checker2
      (flatten
       (straight-maker1
        (pairs hand) 1 (length (pairs hand)))))))))

;; Tests:

(check-expect
 (straight-pairs
  (list 4 4 4 5 5 6 6 7 8 8 9 9 9 10 10 10 10 'King 'King 'Ace 'Ace 2 2 'Black 'Black 'Red)) 
 (list (list 4 4 5 5 6 6) (list 8 8 9 9 10 10)))

(check-expect (straight-pairs (list 5 5 5 6 6 6 6 7 7 7 8 8 8 9 9 9 'Ace 'Ace))
              (list (list 5 5 6 6 7 7) (list 6 6 7 7 8 8) (list 7 7 8 8 9 9)
                    (list 5 5 6 6 7 7 8 8) (list 6 6 7 7 8 8 9 9) (list 5 5 6 6 7 7 8 8 9 9)))

 
(define (straight-checker2 lop)
  (cond
    [(empty? lop) empty]
    [(and
      (<= 3 (length (first lop)))
      (member-blck-red-2? (first lop))
      (one-after-another2? (first lop)))
     (cons (first lop) (straight-checker2 (rest lop)))]
    [else (straight-checker2 (rest lop))]
    ))


(define (member-blck-red-2? lop)
  (cond [(empty? lop) true]
        [(or (member? 2 (first lop))
             (member? 'Black (first lop))
             (member? 'Red (first lop))) false]
        [else (member-blck-red-2? (rest lop))]))

;; Tests:

(check-expect (member-blck-red-2? (list (list 2 2) (list 'Black 'Red))) false)
(check-expect (member-blck-red-2? (list (list 3 3) (list 5 5))) true)


(define (one-after-another2? loc)
  (cond [(empty? (rest loc)) true]
        [(= (- (card->num (first (second loc))) (card->num (first (first loc)))) 1)
         (one-after-another2? (rest loc))]
        [else false]))

;; Tests:

(check-expect (one-after-another2? (list (list 4 4) (list 5 5))) true)
(check-expect (one-after-another2? (list (list 6 6) (list 5 5))) false)


;; append2

(define (append2 loc)
  (cond [(empty? loc) empty]
        [else (append (first loc) (append2 (rest loc)))]))

;; Tests:

(check-expect (append2 (list (list (list 5 5)) (list (list 5 5) (list 5 5))
                                (list (list 6 6)) (list (list 6 6))))
              (list (list 5 5) (list 5 5) (list 5 5) (list 6 6) (list 6 6)))

(define (append-loop loc)
  (cond [(empty? loc) empty]
        [else (cons (append2 (first loc)) (append-loop (rest loc)))]))

;; Tests:

(check-expect (append-loop (list (list (list 5 5)) (list (list 5 5) (list 6 6)) (list (list 6 6))
                                    (list (list 6 6))))
              (list (list 5 5) (list 5 5 6 6) (list 6 6) (list 6 6)))


;; 3g ~~~~

;; (airplanes hand) consumes a Hand and produces a (listof Hand) containing all the airplanes
;; that can be constructed from that hand.

;; Examples:

(check-expect (airplanes (list 3 3 3 3 4 4 4 4))
              (list (list 3 3 3 4 4 4)))

;; airplanes: Hand -> (listof Hand)

(define (airplanes hand)
  (remove-duplicates
   (sort-maker
    (append-loop
     (straight-checker3
      (flatten
       (straight-maker1
        (trios hand) 1 (length (trios hand)))))))))

;; Tests:

(check-expect
 (airplanes
  (list 'Queen 'Queen 'Queen 'Queen
        'King 'King 'King
        'Ace 'Ace 'Ace 2 2 2 ))
 (list (list 'Queen 'Queen 'Queen 'King 'King 'King)
       (list 'King 'King 'King 'Ace 'Ace 'Ace)
       (list 'Queen 'Queen 'Queen 'King 'King 'King
             'Ace 'Ace 'Ace)))

(check-expect
 (airplanes (list 4 4 4 5 5 6 6 7 8 8 9 9 9 10 10 10 10 'King 'King
                  'Ace 'Ace 2 2 2 'Black 'Black 'Black 'Red 'Red 'Red 'Red))
 (list (list 9 9 9 10 10 10)))


(define (straight-checker3 lo3)
  (cond
    [(empty? lo3) empty]
    [(and
      (<= 2 (length (first lo3)))
      (member-blck-red-2? (first lo3))
      (one-after-another2? (first lo3)))
     (cons (first lo3) (straight-checker3 (rest lo3)))]
    [else (straight-checker3 (rest lo3))]
    ))

;; Tests:

(check-expect (straight-checker3
 (list (list (list 3 3)) (list (list 3 3 3) (list 4 4 4)) (list (list 4 4)) (list (list 4 4))))
 (list (list (list 3 3 3) (list 4 4 4)))) 

