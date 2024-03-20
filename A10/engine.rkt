;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname engine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 10, question 3
;; ***************************************************
;;

(require "players.rkt")

;; A Player is one of:
;; * empty
;; * (any of 'Landlord, 'Right, 'Left)

(define hand0 '(3 3 3 3 4 5 6 7 7 7 9 9 Jack Jack Queen King 2 2 Black Red))
(define hand1 '(4 4 4 5 5 6 6 7 8 9 10 Jack Queen King Ace 2 2))
(define hand2 '(5 6 8 8 8 9 10 10 10 Jack Queen Queen King King Ace Ace Ace))



;; (doudizhu players hands) consumes a list of three players, as defined above
;; and a list of the three hands they are holding, and plays the game.

;; Example:

(check-expect (doudizhu (list goldfish goldfish goldfish) (list hand0 hand1 hand2)) 'Left)

;; doudizhu: (listof Player) (listof Hand) -> Sym

(define (doudizhu players hands)
  (local
    [;; (next-player playing) consumes a player and produces who plays next according
     ;; to the game rules
     
     ;; next-player: Player -> Player
     (define (next-player playing)
       (cond
         [(symbol=? playing 'Landlord) 'Right]
         [(symbol=? playing 'Right) 'Left]
         [else 'Landlord]))

     ;; (game-play players hands playing hands-played) consumes the players, their hands, whose turn
     ;; it is, and the hands that have been played. This is the actual gameplay

     ;; game-play: (listof Player) (listof Hand) Sym (listof Hand) -> (anyof (listof Player) Player)
     (define (game-play players hands playing hands-played)
       (local
         [
          (define current-hand-played ((first players) (first hands) playing hands-played))
          (define produced-hand (remove-hand current-hand-played (first hands)))]
         (cond
           [(empty? produced-hand) playing]
           [else (game-play (append (rest players)
                                    (list (first players)))
                            (append (rest hands)
                                    (list produced-hand))
                            (next-player playing)
                            (append (list current-hand-played) hands-played))])))]
    (game-play players hands 'Landlord empty)))


;; Tests:

(check-expect (doudizhu (list reckless goldfish goldfish) (list hand0 hand1 hand2)) 'Landlord)

(check-expect (doudizhu (list cautious reckless goldfish) (list hand0 hand1 hand2)) 'Landlord)


;; (remove-hand played-hands current-hand) consumes played hands and current hand and
;; removes the played hand from the from the current hand

;; remove-hand: (listof Hand) -> (listof Hand)

(define (remove-hand played-hand current-hand)
  (local
    [;; (card=? c1 c2) checks if c1 and c2 are equal
     
     ;; card=?: Card Card -> Bool
     (define (card=? c1 c2)
       (cond
         [(or
           (and (integer? c1) (integer? c2) (= c1 c2))
           (and (symbol? c1) (symbol? c2) (symbol=? c1 c2))) true]
         [else false]))
     
     (define (remove-card card hand acc)
       (cond
         [(empty? hand) empty]
         [(card=? card (first hand)) (append (reverse acc) (rest hand))]
         [else (remove-card card (rest hand) (cons (first hand) acc))]))]
    (cond
      [(empty? played-hand) current-hand]
      [else (remove-hand (rest played-hand) (remove-card (first played-hand)
                                                         current-hand empty))])))
