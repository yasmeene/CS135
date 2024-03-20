;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname listfun) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 04, Question 2
;; ***************************************************
;;

;; (count-vowels string) counts the vowels in a string. Both
;; upper and lower case letters count

;; Examples:

(check-expect
 (count-vowels "bonjour") 3)
(check-expect
(count-vowels "Yasmeen") 3)

;; count-vowels: Str -> Nat

(define (count-vowels string)
  (cond [(empty? (string->list string)) 0]
        [else (+ (vowel->num (first (string->list string)))
                 (count-vowels (list->string (rest (string->list string)))))]))

;; Tests:

(check-expect
 (count-vowels "the quick brown fox jumped over the lazy dog") 12)
(check-expect
 (count-vowels "happy birthday") 3)
(check-expect
 (count-vowels "VowElS") 2)

;; (vowels->num vowel) consumes a character and returns 1 if its a vowel and 0 otherwise

;; Examples
(check-expect (vowel->num #\b) 0)
(check-expect (vowel->num #\A) 1)
(check-expect (vowel->num #\U) 1)
(check-expect (vowel->num #\I) 1)
(check-expect (vowel->num #\O) 1)

;; vowel->num: Char -> Num
(define (vowel->num vowel)
  (cond
    [(char=? #\a vowel) 1]
    [(char=? #\A vowel) 1]
    [(char=? #\e vowel) 1]
    [(char=? #\E vowel) 1]
    [(char=? #\i vowel) 1]
    [(char=? #\I vowel) 1]
    [(char=? #\o vowel) 1]
    [(char=? #\O vowel) 1]
    [(char=? #\u vowel) 1]
    [(char=? #\U vowel) 1]
    [else 0]))


;; (sorted? los) consumes a list of strings and produces true if the strings
;; appear in strict lexicographic order and false otherwise

;; Examples:

(check-expect (sorted? (cons "Hi" (cons "hi" empty))) true)
(check-expect (sorted? (cons "dog" (cons "cat" empty))) false)


;; sorted?: (listof String) -> Bool

(define (sorted? los)
  (cond
   [(or (empty? los) (empty? (rest los))) true]
  [else
     (and (string<? (first los) (first (rest los)))
         (sorted? (rest los)))]))

;; Tests:

(check-expect (sorted? (cons "hello" (cons "Hello" empty))) false)
(check-expect (sorted? (cons "Hi" (cons "hello"
                                        (cons "how are you" empty)))) true)
(check-expect (sorted? (cons "hello" (cons "hello" (cons "hey"
                                                         (cons "hi" empty))))) false)
(check-expect (sorted? (cons "apple" (cons "banana" (cons "citrus" empty)))) true)
(check-expect (sorted? empty) true)


;; (replace-word str1 str2 los) consumes two strings and a list of strings, and
;; produces a new list where all occurrences of the first string have been replaced by the
;; second string

;; Examples:

(check-expect (replace-word "Python" "Racket"
                            (cons "Python" (cons "is a great" (cons "language" empty))))
              (cons "Racket" (cons "is a great" (cons "language" empty))))

(check-expect (replace-word "Game" "Minecraft"
                             (cons "Roblox" (cons "COD" (cons "Game" empty))))
               (cons "Roblox" (cons "COD" (cons "Minecraft" empty))))

;; replace-word: Str Str (listof Str) -> (listof Str)

(define (replace-word str1 str2 los)
  (cond
   [(empty? los) empty]
   [else
    (cons (cond
            [(string=? (first los) str1) str2]
            [else (first los)])
          (replace-word str1 str2 (rest los)))]))

;; Tests:
(check-expect (replace-word "exam" "assessment"
                            (cons "content" (cons "exam" (cons "assignment"
                                                               empty))))
              (cons "content" (cons "assessment" (cons "assignment" empty))))

(check-expect (replace-word "dog" "cat"
                            (cons "fish" (cons "dog" (cons "bird" empty))))
              (cons "fish" (cons "cat" (cons "bird" empty))))


;; (remove-duplicates loa) consumes a list of Any and produces the same
;; list, with all but the last occurrence of each element removed.

;; Examples:

(check-expect (remove-duplicates
               (cons 1 (cons 3 (cons 1 (cons 2
                                             (cons 4 (cons 2 (cons 7 (cons 2
                                                                           (cons 5 empty))))))))))
              (cons 3 (cons 1 (cons 4 (cons 7 (cons 2 (cons 5 empty)))))))

(check-expect (remove-duplicates
               (cons 2 (cons 2 (cons 5 (cons 5 (cons 2 (cons 4 empty)))))))
              (cons 5 (cons 2 (cons 4 empty))))

;; remove-duplicates: (listof Any) -> (listof Any)

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


  