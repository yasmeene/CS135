;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname examples-a05) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 05, check-expect examples 
;; ***************************************************
;;

(define selections
  (list
   (list "lanarey" (list 'MATH145 'MATH147 'CS145 'COMMST100 'MUSIC140))
   (list "shawkings" (list 'PHYS111 'SCI206 ' MATH145 'FINE100))
   (list "2pac" (list 'ANTH241 'STAT230 'SCI206 'MUSIC140))
   (list "6obama" (list 'COMMST223 'PSCI281 'PSCI231 'MATH137))))


;; question 2a, function taking-course?

(check-expect (taking-course? selections "lanarey" 'MATH145) true)
(check-expect (taking-course? selections "shawkings" 'PHYS111) true)
(check-expect (taking-course? selections "2pac" 'ANTH101) false)

;; question 2b, function missed-deadline-add

(check-expect (missed-deadline-add selections "m3marco") (list
   (list "lanarey" (list 'MATH145 'MATH147 'CS145 'COMMST100 'MUSIC270))
   (list "shawkings" (list 'PHYS111 'SCI206 ' MATH115 'FINE100))
   (list "2pac" (list 'ANTH241 'STAT230 'AFM101 'MUSIC140))
   (list "6obama" (list 'COMMST223 'PSCI281 'PSCI231 'MATH137))
   (list "m3marco" empty)))

(check-expect (missed-deadline-add selections "totoro") (list
   (list "lanarey" (list 'MATH145 'MATH147 'CS145 'COMMST100 'MUSIC270))
   (list "shawkings" (list 'PHYS111 'SCI206 ' MATH115 'FINE100))
   (list "2pac" (list 'ANTH241 'STAT230 'AFM101 'MUSIC140))
   (list "6obama" (list 'COMMST223 'PSCI281 'PSCI231 'MATH137))
   (list "totoro" empty)))

;; question 2c, function add-course

(check-expect (add-course 'MATH115 "2pac") (list
   (list "lanarey" (list 'MATH145 'MATH147 'CS145 'COMMST100 'MUSIC270))
   (list "shawkings" (list 'PHYS111 'SCI206 ' MATH115 'FINE100))
   (list "2pac" (list 'ANTH241 'STAT230 'AFM101 'MUSIC140 'MATH115))
   (list "6obama" (list 'COMMST223 'PSCI281 'PSCI231 'MATH137))))

(check-expect (add-course 'MATH115 "totoro") (list
   (list "lanarey" (list 'MATH145 'MATH147 'CS145 'COMMST100 'MUSIC270))
   (list "shawkings" (list 'PHYS111 'SCI206 ' MATH115 'FINE100))
   (list "2pac" (list 'ANTH241 'STAT230 'AFM101 'MUSIC140))
   (list "6obama" (list 'COMMST223 'PSCI281 'PSCI231 'MATH137))
   (list "totoro" (list 'MATH115))))

;; question 2d, function create-classlist

(check-expect (create-classlist selections 'MATH145) (list "lanarey" "shawkings"))
(check-expect (create-classlist selections 'MUSIC140) (list "lanarey" "2pac"))
(check-expect (create-classlist selections 'PSCI281) (list "6obama"))

;; question 3a, function solo

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

;; question 3b, function pairs

(check-expect
 (pairs (list 3 3 3 3 4 5 6 7 7 7
              'Jack 'Jack 'Queen 'King 'Ace 2 2))
 (list (list 3 3) (list 7 7) (list 'Jack 'Jack) (list 2 2)))

(check-expect
 (pairs
  (list 4 4 5 5 5 6 9 10 10 'Jack 'King 'King 'Ace 'Black 'Black 'Black))
 (list (list 4 4) (list 5 5) (list 10 10) (list 'King 'King) (list 'Black 'Black)))

;; question 3c, function trios

(check-expect
 (trios (list 3 3 3 4 5 5 5 6 6 6 6 7 7 7 8 8 9 'Queen 'Queen))
 (list (list 3 3 3) (list 5 5 5) (list 6 6 6) (list 7 7 7)))

(check-expect
 (trios
  (list 4 4 5 5 5 6 9 9 9 10 10 'Jack 'King 'King 'Ace 'Ace 'Ace 'Black 'Black 'Black))
 (list (list 5 5 5) (list 9 9 9) (list 'Ace 'Ace 'Ace) (list 'Black 'Black 'Black)))

;; question 3d, function sort-hands

(check-expect
 (sort-hands (list (list 'Jack 'Queen 'King) (list 3 3 3) (list 4 4)
                   (list 'Black 'Red) (list 4 4) (list 5 5 5)
                   (list 3 4 5) (list 3 3 3)))
 (list (list 4 4) (list 3 3 3) (list 3 4 5) (list 5 5 5)
       (list 'Jack 'Queen 'King) (list 'Black 'Red)))

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

;; question 3e, function straights

(check-expect
 (straights (list 3 3 3 3 4 5 6 7 8 9
                  'Jack 'Jack 'Queen 'King 'Ace 2 2))
 (list (list 3 4 5 6 7) (list 4 5 6 7 8) (list 5 6 7 8 9)
       (list 3 4 5 6 7 8) (list 4 5 6 7 8 9)
       (list 3 4 5 6 7 8 9)))

(check-expect
 (straights (list 3 4 4 4 5 5 6 7 8 8 'King 'King 'King 'Ace 'Ace 'Black 'Red))
 (list (list 3 4 5 6 7 8) (list 3 4 5 6 7) (list 4 5 6 7 8)))

;; question 3f, function straight-pairs

(check-expect
 (straight-pairs
  (list 3 3 3 4 5 5 5 6 6 6 6 7 7 7 8 8 9 'Ace 'Ace))
 (list (list 5 5 6 6 7 7) (list 6 6 7 7 8 8)
       (list 5 5 6 6 7 7 8 8)))     

(check-expect
 (straight-pairs
  (list 4 4 4 5 5 6 6 7 8 8 9 9 9 10 10 10 10 'King 'King 'Ace 'Ace 2 2 'Black 'Black 'Red))
 (list (list 4 4 5 5 6 6) (list 8 8 9 9 10 10) (list 'King 'King 'Ace 'Ace 2 2)))

;; question 3g, function airplanes

(check-expect (airplanes (list 3 3 3 3 4 4 4 4))
              (list (list 3 3 3 4 4 4)))

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
 (list (list 9 9 9 10 10 10) (list 2 2 2 'Black 'Black 'Black 'Red 'Red 'Red)))
