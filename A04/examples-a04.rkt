;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname examples-a04) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 04, check-expect examples 
;; ***************************************************
;;


;; 2a, function count-vowels
(check-expect
 (count-vowels "the quick brown fox jumped over the lazy dog") 12)
(check-expect
 (count-vowels "happy birthday") 3)
(check-expect
 (count-vowels "VowElS") 2)

;; 2b, function sorted?

(check-expect (sorted? (cons "hello" (cons "Hello" empty))) false)
(check-expect (sorted? (cons "Hi" (cons "hello"
                                        (cons "how are you" empty)))) true)
(check-expect (sorted? (cons "hello" (cons "hello" (cons "hey"
                                                         (cons "hi" empty))))) false)
(check-expect (sorted? (cons "apple" (cons "banana" (cons "citrus" empty)))) true)
(check-expect (sorted? (cons empty)) true)

;; 2c, function replace-word

(check-expect (replace-word "exam" "assessment"
                            (cons "content" (cons "exam" (cons "assignment"
                                                               empty))))
              (cons "content" (cons "assessment" (cons "assignment" empty))))

(check-expect (replace-word "dog" "cat"
                            (cons "fish" (cons "dog" (cons "bird" empty))))
              (cons "fish" (cons "cat" (cons "bird" empty))))

(check-expect (replace-word "Python" "Racket"
                            (cons "Python" (cons "is a great" (cons "language" empty))))
              (cons "Racket" (cons "is a great" (cons "language" empty))))

;; 2d, function remove-duplicate

(check-expect (remove-duplicate
               (cons 1 (cons 3 (cons 1 (cons 2
                                             (cons 4 (cons 2 (cons 7 (cons 2
                                                                           (cons 5 empty))))))))))
              (cons 3 (cons 1 (cons 4 (cons 7 (cons 2 (cons 5 empty)))))))

(check-expect (remove-duplicate
               (cons 2 (cons 2 (cons 5 (cons 5 (cons 2 (cons 4 empty)))))))
              (cons 5 (cons 2 (cons 4 empty))))


;; 3a, function card?

(check-expect (card? 'Jack) true)
(check-expect (card? "3") false)
(check-expect (card? 'Spade) false)
(check-expect (card? 5) true)


;; function card=?

(check-expect (card=? 3 3) true)
(check-expect (card=? 'Jack 'King) false)


;; 3b, function sort-cards


(check-expect (sort-cards
               (list 3 'King 6 7 'Jack 'Queen 2 7 3 7 3 'Ace 'Jack 2 3 4 5))
              (list 3 3 3 3 4 5 6 7 7 7 'Jack 'Jack 'Queen 'King 'Ace 2 2))

(check-expect (sort-cards
               (list 4 4 4 'Jack 'Queen 2 7 'Ace 5 6))
              (list 4 4 4 5 6 7 'Jack 'Queen 'Ace 2))


;; 3c, function remove-one-of-each

(check-expect (remove-one-of-each
               (list 3 3 3 3 4 5 6 7 7 7 'Jack 'Jack 'Queen 'King 'Ace 2 2))
              (list 3 3 3 7 7 'Jack 2))
(check-expect (remove-one-of-each
               (list 3 3 3 3 4 5 6 7 7 7 'Jack 'Jack 'Queen 'King 'Ace 2 2))
              (list 3 3 3 7 7 'Jack 2))

(check-expect (remove-one-of-each
               (list 4 4 4 5 6 7 'Jack 'Queen 'Ace 2))
              (list 4 4))


;; 3d, function find-kind

(check-expect
 (find-kind 3 (list 3 3 3 3 4 5 6 7 7 7 'Jack 'Jack 'Queen 'King
                    'Ace 2 2))
 (list 3 7))

(check-expect (find-kind 4 (list 3 3 3 3 4 5 6 7 7 7 'Jack 'Jack 'Queen 'King 'Ace 2 2))
              (list 3))

(check-expect (find-kind 2 (list 3 3 4 4 4 5 6 7 'Jack 'Queen 'Ace 'Ace 'Ace 2 2 2))
              (list 3 4 'Ace 2))

;; 4b, function

(check-expect (div-by-3? 0) true)
(check-expect (div-by-3? 1) false)
(check-expect (div-by-3? 2) false)
(check-expect (div-by-3? 3) true) 
(check-expect (div-by-3? 4) false)
(check-expect (div-by-3? 5) false)
(check-expect (div-by-3? 6) true) 
(check-expect (div-by-3? 7) false)
(check-expect (div-by-3? 8) false)
(check-expect (div-by-3? 9) true)