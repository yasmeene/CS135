;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname alf-warmup) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 09, question 2
;; ***************************************************
;;

;; ~~~~~ 2a

(define (absolutely-odd loi)
  (foldr + 0 (map abs (filter odd? loi))))

(check-expect (absolutely-odd '(1 -5 4 6 5)) 11)
(check-expect (absolutely-odd '()) 0)

;; ~~~~~ 2b

(define (zip lst1 lst2)
  (map (lambda (x y) (list x y)) lst1 lst2))



(check-expect (zip (list 1 2 3 4) (list "a" "b" "c" "d"))
              (list (list 1 "a") (list 2 "b") (list 3 "c") (list 4 "d")))

(check-expect (zip empty empty) empty)


;; ~~~~~ 2c

(define (unzip lop)
  (cond
    [(empty? lop) (list empty empty)]
    [else (list (map (lambda (x) (first x)) lop)
                (map (lambda (x) (second x)) lop))]))


(check-expect (unzip '((1 a)(2 b)(3 c))) '((1 2 3) (a b c)))

(check-expect (unzip '()) '(()()))


;; ~~~~~ 2d

(define (dedup lon)
  (foldr (lambda (x y) (cons x (filter (lambda (z) (not (= x z))) y))) empty lon))
  
(check-expect (dedup '(1 2 1 3 3 2 4)) '(1 2 3 4))

;; ~~~~~ 2e

(define (zero-fill word)
  (list->string (append (build-list (- 20 (length (string->list word)))
                                    (lambda (x) #\0)) (string->list word))))

(check-expect (zero-fill "abcdefghijklmn") "000000abcdefghijklmn")
(check-expect (zero-fill "he00llo") "0000000000000he00llo")

;; ~~~~~ 2f

(define (subsequence loa to from)
  (foldr (lambda (x y rror) (cond [(and (< y from)
                                        (>= y to)) (cons x rror)]
                                  [else rror])) empty loa (build-list (length loa)
                                                                      (lambda (a) a))))

(check-expect (subsequence (list 1 2 3 4 5 6) 1 4) (list 2 3 4))