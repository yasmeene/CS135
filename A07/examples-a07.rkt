;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname examples-a07) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 07, check-expect examples
;; ***************************************************
;;

;; ~~~~~~~~~~~~~~~
;; question 1, function contains-component?


(define bike (make-component
              "bike" 1
              (list
               (make-component "frame" 1 empty)
               (make-component "wheel" 2
                               (list
                                (make-component "tire" 1 empty)
                                (make-component "rim" 1 empty)
                                (make-component "spoke" 30 empty)
                                (make-component "hub" 1
                                                (list (make-component "housing" 1 empty)
                                                      (make-component "axel" 1 empty)
                                                      (make-component "bearing" 20
                                                                      empty)))))
               (make-component "seat" 1 empty)
               (make-component "handlebar" 1 empty))))

(check-expect (contains-component? bike "hub") true)
(check-expect (contains-component? bike "brake") false)
(check-expect (contains-component? car "seat") true)
(check-expect (contains-component? car "cat") false)

;; ~~~~~~~~~~~~~~~
;; question 2c, function eval ****** is this even right

(check-expect (eval (list '+ 'x 1) sym-table) 2)
(check-expect (eval (list '- 'y 2) sym-table) 0)
(check-expect (eval (list '* 3 'z) sym-table) 12)


;; question 2d, function simplify

(check-expect (simplify (make-opnode '+ (list 4 'x 5)))
                        (make-opnode '+ (list 'x 9)))

(check-expect (simplify (make-opnode '* (list 4 'x 5)))
                        (make-opnode '* (list 'x 20)))

(check-expect (simplify (make-opnode '+ (list 3 'x 3)))
                        (make-opnode '+ (list 'x 6)))

(check-expect (simplify (make-opnode '* (list 1 'x 1)))
                        (make-opnode '* (list 'x 1)))

(check-expect (simplify (make-opnode '+ (list 2 3 4))) 9)

;; ~~~~~~~~~~~~~~~
;; question 3a, function list-files

(check-expect (list-files sample-fs) (list "readme.txt"
                                           "zahra.jpg"
                                           "timbit.jpg"
                                           "chonky.jpg"
                                           "beach1.jpg"
                                           "beach2.jpg"
                                           "beach3.jpg"
                                           "oreo.jpg"
                                           "anna.jpg"
                                           "beach1.jpg"
                                           "eagles-hotel-california.mp3"
                                           "bee-gees-stayin-alive.mp3"
                                           "lady-gaga-bad-romance.mp3"
                                           "beyonce-single-ladies.mp3"
                                           "shopping.txt"
                                           "todo.txt"))

;; question 3b, function backup

(check-expect (backup (make-file "filedir.rkt" 5400 1698197579))
              (make-file "filedir.rkt.bak" 5400 1698197579))

(check-expect (backup (make-file "readme.txt" 187 1319502441))
              (make-file "readme.txt.bak" 187 1319502441))

(check-expect (backup (make-file "chonky.jpg" 2709000 1319287654))
              (make-file "chonky.jpg.bak" 2709000 1319287654))

(check-expect (backup (make-file "lady-gaga-bad-romance.mp3" 9376000 1319612418))
              (make-file "lady-gaga-bad-romance.mp3.bak" 9376000 1319612418))

;; question 3c, function get-time

(check-expect (get-time (make-file "filedir.rkt" 5400 1698197579))
              1698197579)

(check-expect (get-time (make-file "readme.txt" 187 1319502441))
              1319502441)

(check-expect (get-time (make-file "chonky.jpg" 2709000 1319287654))
              1319287654)

(check-expect (get-time (make-file "lady-gaga-bad-romance.mp3" 9376000 1319612418))
              1319612418)

(check-expect (get-time (make-dir "vacation"
                                    (list                    
                                     (make-file "beach1.jpg" 3297000 1319320493)
                                     (make-file "beach2.jpg" 2173000 1319449661)
                                     (make-file "beach3.jpg" 2747000 1319617966))))
              1319617966)

(check-expect (get-time (make-dir "rock"
                                    (list 
                                     (make-file "eagles-hotel-california.mp3" 10184000 1319658262)
                                     (make-file "bee-gees-stayin-alive.mp3" 9693000 1319660517))))
              1319660517)

(check-expect (get-time (make-dir "dance"
                                    (list 
                                     (make-file "lady-gaga-bad-romance.mp3" 9376000 1319612418)
                                     (make-file "beyonce-single-ladies.mp3" 17669000 1319207025))))
              1319612418)

;; question 3d, function get-time

(check-expect (find "vacation" sample-fs)
              (list (list "root" "photos" "vacation")))

(check-expect (find "shopping.txt" sample-fs)
              (list (list "root" "notes" "shopping.txt")))

(check-expect (find "beach1.jpg" sample-fs)
              (list (list "root" "photos" "beach1.jpg")
                    (list "root" "photos" "vacation" "beach1.jpg")))

(check-expect (find "readme.txt"
                    (make-file "readme.txt" 187 1319502441))
              (list (list "readme.txt")))

(check-expect (find "a06.rkt" sample-fs)
              empty)
