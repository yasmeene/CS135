;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname examples-a06) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 06, check-expect examples
;; ***************************************************
;;

;; ~~~~~~~~~~~~~~~

;; examples for 1a, function my-list-ref

(check-expect (my-list-ref (list 1 2 3 4) 0) 1)
(check-expect (my-list-ref (list 5 4 3) 2) 3)
(check-expect (my-list-ref (list 2) 20) false)
(check-expect (my-list-ref (list 3 5 7 9 11) 4) 11)
(check-expect (my-list-ref (list 2 2) 1) 2)

;; examples for 1b, function zip

(check-expect (zip (list 1 2 3) (list 4 5 6))
              (list (list 1 4) (list 2 5) (list 3 6)))

(check-expect (zip (list 1 2 3 4) (list "a" "b" "c" "d"))
              (list (list 1 "a") (list 2 "b") (list 3 "c") (list 4
                                                                 "d")))
(check-expect (zip empty empty) empty)

(check-expect (zip (list 'red 'blue 'green 'yellow) (list "red" "blue" "green" "yellow"))
              (list (list 'red "red") (list 'blue "blue" ) (list 'green "green")
                    (list 'yellow "yellow")))

;; examples for 1c, function list-xor

(check-expect (list-xor (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))
(check-expect (list-xor (list 1 3 5) (list 2 3 4)) (list 1 2 4 5))
(check-expect (list-xor (list 2 4 6 8) (list 2 3 5 6)) (list 4 5 8))
(check-expect (list-xor (lsit 10 20 30) (list 5 10 15)) (list 15 20 30))


;; ~~~~~~~~~~~~~~~

;; examples for 2c, function num->signmag

(check-expect (num->signmag 0) (make-signmag 'zero 1))
(check-expect (num->signmag -3) (make-signmag 'negative 3))
(check-expect (num->signmag 19) (make-signmag 'positive 19))

;; examples for 2d, function signmag->num

(check-expect (signmag->num (make-signmag 'zero 1)) 0)
(check-expect (signmag->num (make-signmag 'negative -5)) -5 )
(check-expect (signmag->num (make-signmag 'positive 19)) 19)


;; ~~~~~~~~~~~~~~~

;; examples for 3a, function full?

(check-expect (full? empty) true)
(check-expect (full? (make-node 5
                                (make-node 1 empty empty)
                                (make-node 7
                                           (make-node 6 empty empty)
                                           (make-node 14 empty empty)))) true)

(check-expect (full? (make-node 5 empty (make-node 6 empty (make-node 7 empty empty)))) false)

(check-expect (full? (make-node 9 (make-node 3 (make-node 1 empty empty) empty
                                             (make-node 8 empty empty)))) false)


;; ~~~~~~~~~~~~~~~

;; question for 4a, function extreme-actions

(check-expect
 (extreme-actions "Zahra" instr-actlst)
 (list "Was disobedient at the grocery store" "Cleaned her room without being nagged"))

(check-expect
 (extreme-actions "Patrick" instr-actlst)
 (list "Joined a bicycle gang" "Joined a bicycle gang"))

(check-expect
 (extreme-actions "Byron" instr-actlst)
 (list "Harassed a goose in the park"))

(check-expect
 (extreme-actions "Yasmeen" instr-actlst) empty)


;; question 4b, function gifts-received

(check-expect (gifts-received "Byron" 20 instr-childrenlist -10)
              (list "Playing Card Deck"
                    "Colourful Pencils"
                    "Hot Wheels"))
(check-expect (gifts-received "Armin" 10 instr-childrenlist -11)
              (list "Playing Card Deck"))
(check-expect (gifts-received "Patrick" -10 instr-childrenlist -12)
              'coal)
(check-expect (gifts-received "Zahra" -15 instr-childrenlist -13)
              'krampus)
(check-expect (gifts-received "Yasmeen" -10 instr-childrenlist -10)
              empty)


;; question 4c, function add-action
(check-expect (add-action
               (make-action "Byron" 3 "Prepared tutorial questions")
               (make-actionnode
                "Mark"
                10
                (list (make-action "Mark" "Bought some stuff"))
                
                (make-actionnode
                 "Byron"
                 10
                 (list (make-action "Byron" 10 "taught the class trees"))
                 empty empty)
                
                (make-actionnode
                 "Zarah"
                 11
                 (list (make-action "Zahra" 11 "taught cool stuff"))
                 empty empty)))
              
              (make-actionnode
                "Mark"
                10
                (list (make-action "Mark" "Bought some stuff"))
                
                (make-actionnode
                 "Byron"
                 10
                 (list (make-action "Byron" 10 "taught the class trees")
                       (make-action "Byron" 3 "Prepared tutorial questions"))
                 empty empty)
                
                (make-actionnode
                 "Zarah"
                 11
                 (list (make-action "Zahra" 11 "taught cool stuff"))
                 empty empty)))

(check-expect (add-action
               (make-action "Yasmeen" 99 "Went to all lectures")
               (make-actionnode
                "Mark"
                10
                (list (make-action "Mark" "Bought some stuff"))
                
                (make-actionnode
                 "Byron"
                 10
                 (list (make-action "Byron" 10 "taught the class trees"))
                 empty empty)
                
                (make-actionnode
                 "Zarah"
                 11
                 (list (make-action "Zahra" 11 "taught cool stuff"))
                 empty empty)))
              
              (make-actionnode
                "Mark"
                10
                (list (make-action "Mark" "Bought some stuff"))
                
                (make-actionnode
                 "Byron"
                 10
                 (list (make-action "Byron" 10 "taught the class trees"))
                 empty empty)
                
                (make-actionnode
                 "Zarah"
                 11
                 (list (make-action "Zahra" 11 "taught cool stuff"))
                 empty empty)
                (make-actionnode
                 "Yasmeen"
                 99
                 (list (make-action "Yasmeen" 99 "went to all lectures"))
                 empty empty)))
              


;; question 4d, function gift-list
(check-expect
 (gift-list
  (make-actionnode
   "Mark"
   12
   (list (make-action "Mark" 12 "Bought the ISAs more Cheetos"))
   (make-actionnode 
    "Byron"
    2
    (list (make-action "Byron" 2 "Recycled an empty Cheetos packet"))
    empty
    empty)
   (make-actionnode 
    "Zahra" 
    -5
    (list (make-action "Zahra" -5 "Ate the ISAs' Cheetos"))
    empty
    empty)) instr-childrenlist -12)
 (list (list "Byron" (list "Playing Card Deck")) 
       (list "Mark" (list "Playing Card Deck" "Lego flowers"))
       (list "Zahra" 'coal)))