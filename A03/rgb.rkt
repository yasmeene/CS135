;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rgb) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 03, Question 2
;; ***************************************************
;;

;; (List->parts lst) consumes a list and returns each part of the
;; list as lst_first, lst_second, and lst_third respectfully

(define (lst_first lst)
  (first lst))

(define (lst_second lst)
  (first (rest lst)))

(define (lst_third lst)
  (first (rest (rest lst))))

;; Examples
(check-expect (lst_first (cons 0 (cons 1 (cons 3 empty)))) 0)
(check-expect (lst_second (cons 0 (cons 1 (cons 3 empty)))) 1)
(check-expect (lst_third (cons 0 (cons 1 (cons 3 empty)))) 3)


;; (RGB->name list_RGB) consumes a three element list and produces the colour
;; name correlating with that RGB value.

;; Examples:
(check-expect (RGB->name (cons 0 (cons 255 (cons 0 empty)))) 'green)

;; An RGB Triplet (RGB) is a (cons Nat (cons Nat (cons Nat empty)))

;; RGB->name: (Listof Nat) -> Sym
;; requires: each element in the list must be <= 255

(define (RGB->name list_RGB)
  (cond
    [(= (lst_first list_RGB)
        (lst_second list_RGB)) (cond
                              [(= (lst_second list_RGB)
                                  (lst_third list_RGB) 0) 'black]
                              [(= (lst_second list_RGB)
                                  (lst_third list_RGB) 255) 'white]
                              [(= (lst_third list_RGB) 255) 'blue]
                              [else 'yellow])]
    [(= (first list_RGB)
        (first (rest (rest list_RGB)))) (cond
                                     [(= (first list_RGB) 0) 'green]
                                     [else 'magenta])]
    [(= (first (rest list_RGB))
        (first (rest (rest list_RGB)))) (cond
                                          [(= (first (rest list_RGB)) 255) 'cyan]
                                          [else 'red])]
    [else 'unknown])) 

;; Tests:

(check-expect (RGB->name (cons 255 (cons 12 (cons 37 empty)))) 'unknown)
(check-expect (RGB->name (cons 255 (cons 0 (cons 0 empty)))) 'red)
(check-expect (RGB->name (cons 0 (cons 0 (cons 255 empty)))) 'blue)
(check-expect (RGB->name (cons 0 (cons 0 (cons 0 empty)))) 'black)
(check-expect (RGB->name (cons 255 (cons 255 (cons 255 empty)))) 'white)
(check-expect (RGB->name (cons 255 (cons 255 (cons 0 empty)))) 'yellow)
(check-expect (RGB->name (cons 255 (cons 0 (cons 255 empty)))) 'magenta)
(check-expect (RGB->name (cons 0 (cons 255 (cons 255 empty)))) 'cyan)



;; (name->RGB colour_sym) consumes a symbol and produces the RGB value corresponding with that symbol.

;; Examples:
(check-expect (name->RGB 'magenta) (cons 255 (cons 0 (cons 255 empty))))
(check-expect (name->RGB 'green) (cons 0 (cons 255 (cons 0 empty))))

;; name->RGB: Sym -> (listof Nat)

(define (name->RGB colour_sym)
  (cond
    [(symbol=? colour_sym 'red) (cons 255 (cons 0 (cons 0 empty)))]
    [(symbol=? colour_sym 'green) (cons 0 (cons 255 (cons 0 empty)))]
    [(symbol=? colour_sym 'blue) (cons 0 (cons 0 (cons 255 empty)))]
    [(symbol=? colour_sym 'black) (cons 0 (cons 0 (cons 0 empty)))]
    [(symbol=? colour_sym 'white) (cons 255 (cons 255 (cons 255 empty)))]
    [(symbol=? colour_sym 'yellow) (cons 255 (cons 255 (cons 0 empty)))]
    [(symbol=? colour_sym 'magenta) (cons 255 (cons 0 (cons 255 empty)))]
    [(symbol=? colour_sym 'cyan) (cons 0 (cons 255 (cons 255 empty)))]
    [else (cons -1 (cons -1 (cons -1 empty)))]))


;; Tests:

(check-expect (name->RGB 'pink) (cons -1 (cons -1 (cons -1 empty))))
(check-expect (name->RGB 'red) (cons 255 (cons 0 (cons 0 empty))))
(check-expect (name->RGB 'blue) (cons 0 (cons 0 (cons 255 empty))))
(check-expect (name->RGB 'black) (cons 0 (cons 0 (cons 0 empty))))
(check-expect (name->RGB 'white) (cons 255 (cons 255 (cons 255 empty))))
(check-expect (name->RGB 'yellow) (cons 255 (cons 255 (cons 0 empty))))
(check-expect (name->RGB 'magenta) (cons 255 (cons 0 (cons 255 empty))))
(check-expect (name->RGB 'cyan) (cons 0 (cons 255 (cons 255 empty))))


;; (RGB->luminosity lumi_list) consumes a three element list and produces a number that is the
;; corresponding luminosity.

;; Examples:
(check-within (RGB->luminosity (cons 0 (cons 255 (cons 0 empty)))) 150.45 0.01)

;; RGB->luminosity: (listof Nat) -> Num

(define (RGB->luminosity lumi_list)
  (+ (* 0.3 (lst_first lumi_list))
        (* 0.59 (lst_second lumi_list))
        (* 0.11 (lst_third lumi_list))))

 
;; Tests:

(check-within (RGB->luminosity (cons 255 (cons 0 (cons 0 empty)))) 76.5 0.01)
(check-within (RGB->luminosity (cons 255 (cons 255 (cons 0 empty)))) 226.95 0.01)
(check-within (RGB->luminosity (cons 0 (cons 0 (cons 0 empty)))) 0 0.01)


;; (valid-RGB? elem) consumes anything and produces true if
;; the consumed argument is a valid RGB Triplet and false otherwise.

;; Examples:

(check-expect (valid-RGB? 'purple) false)
(check-expect (valid-RGB? (cons 0 (cons 0 (cons 255 empty)))) true)

;; valid-RGB?: Any -> Bool

(define (valid-RGB? elem)
  (or (and (list? elem) (or
                    (and (or (= (lst_first elem) 0) (= (lst_first elem) 255))
                          (or (= (lst_second elem) 0) (= (lst_second elem) 255))
                          (or (= (lst_third elem) 0)
                              (= (lst_third elem) 255)))
                    false))
    false))

;; Tests:

(check-expect (valid-RGB? (cons 30 (cons 0 (cons 200 empty)))) false)
(check-expect (valid-RGB? (cons 0 (cons 0 (cons 1 empty)))) false)
(check-expect (valid-RGB? (cons 255 (cons 255 (cons 255 empty)))) true)
(check-expect (valid-RGB? "hello") false)
(check-expect (valid-RGB? 'pink) false)
(check-expect (valid-RGB? (cons 255 (cons 255 (cons 0 empty)))) true)
(check-expect (valid-RGB? (cons 0 (cons 255 (cons 0 empty)))) true)


;; (RGB->hex rgb_triplet) consumes an RGB value in the form of a three
;; element natural number list and produces a six element list that contains the hexadecimal
;; value of that colour.

;; Examples:
(check-expect (RGB->hex (cons 0 (cons 255 (cons 0 empty))))
              (cons "0" (cons "0" (cons "F" (cons "F" (cons "0" (cons "0" empty)))))))

;; RGB->hex: (listof Nat) -> (listof Str)

(define (RGB->hex rgb_triplet)
  (cons (RGB->letter (quotient (lst_first rgb_triplet) 16))
        (cons (RGB->letter (remainder (lst_first rgb_triplet) 16))
              (cons (RGB->letter (quotient (lst_second rgb_triplet) 16))
                    (cons (RGB->letter (remainder (lst_second rgb_triplet) 16))
                          (cons (RGB->letter (quotient (lst_third rgb_triplet) 16))
                                (cons (RGB->letter
                                       (remainder (lst_third rgb_triplet) 16)) empty)))))))
         
  
;; Tests:

(check-expect (RGB->hex (cons 255 (cons 0 (cons 0 empty))))
              (cons "F" (cons "F" (cons "0" (cons "0" (cons "0" (cons "0" empty)))))))
(check-expect (RGB->hex (cons 255 (cons 255 (cons 0 empty))))
              (cons "F" (cons "F" (cons "F" (cons "F" (cons "0" (cons "0" empty)))))))
(check-expect (RGB->hex (cons 77 (cons 135 (cons 95 empty))))
              (cons "4" (cons "D" (cons "8" (cons "7" (cons "5" (cons "F" empty)))))))
(check-expect (RGB->hex (cons 255 (cons 255 (cons 0 empty))))
              (cons "F" (cons "F" (cons "F" (cons "F" (cons "0" (cons "0" empty)))))))
(check-expect (RGB->hex (cons 125 (cons 133 (cons 176 empty))))
              (cons "7" (cons "D" (cons "8" (cons "5" (cons "B" (cons "0" empty))))))) 

;; (RGB->letter digit) consumes a digit and transforms it into a string

;; Examples:

(check-expect (RGB->letter 5) "5")
(check-expect (RGB->letter 15) "F")

;; RGB->letter: Nat -> Str

(define (RGB->letter digit)
  (cond
    [(<= digit 9) (number->string digit)]
    [(= digit 10) "A"]
    [(= digit 11) "B"]
    [(= digit 12) "C"]
    [(= digit 13) "D"]
    [(= digit 14) "E"]
    [else "F"]))