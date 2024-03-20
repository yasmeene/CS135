;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname aexp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 07, question 2
;; ***************************************************
;;


;; ~~~~~ 2a


;; An Arithmetic Expression (AExp) is one of:
;; * Num
;; * Sym
;; * OpNode

(define-struct opnode (op args))
;; An Operator Node (OpNode) is a
;; (make-opnode (anyof '* '+) (listof AExp))

(define a-exp
  (make-opnode '+ (list 'x 'y (make-opnode '* (list 5 'z)))))

(define sym-table (list (list 'x 1) (list 'y 2) (list 'z 4)))
(check-expect (eval a-exp sym-table) 23)


;; ~~~~~ 2b

;; aexp-template: AExp -> Any
(define (aexp-template exp)
  (cond
    [(number? exp) exp]
    [(symbol? exp)  ...]
    [(opnode? exp) (opnode-template exp)]))
                      
;; opnode-template: OpNode -> Any 
(define (opnode-template exp)
  (... (opnode-op exp)
       (listof-args-template (opnode-args exp))))

;; listof-args-template: (listof (anyof Num Sym)) -> Any
(define (listof-args-template args)
  (cond
    [(empty? args) empty]
    [else (... (aexp-template (first args)) (listof-args-template (rest args)))]))


;; ~~~~~ 2c

;; (eval exp) consumes an arithmetic expression and evaluates it

;; Example:

(check-expect (eval (make-opnode '+ (list 'x 1)) sym-table) 2)

;; eval: AExp -> Num

(define (eval exp sym-table)
  (cond
    [(number? exp) exp]
    [(symbol? exp) (lookup-al exp sym-table)]
    [(opnode? exp) (apply2 (opnode-op exp)
                          (opnode-args exp))]))

;; Tests:
(check-expect (eval (make-opnode '+ (list 'y 2)) sym-table) 4)
(check-expect (eval (make-opnode '* (list 3 'z)) sym-table) 12)


;; (apply2 op args table) applies the arithmetic operator op to args from symtable

;; Examples:

(check-expect (apply2 '+ (list 1 2 3 4)) 10)
(check-expect (apply2 '* (list 2 3 4)) 24)
(check-expect (apply2 '+ (list )) 0)
(check-expect (apply2 '* (list )) 1)

;; apply2: (anyof '+ '*) (listof AExp) -> Num

(define (apply2 op args)
  (cond
    [(empty? args) (cond
                     [(symbol=? op '+) 0]
                     [(symbol=? op '*) 1])]
    [(symbol=? op '+) (+ (eval (first args) sym-table)
                        (apply2 op (rest args)))]
    [(symbol=? op '*) (* (eval (first args) sym-table)
                         (apply2 op (rest args)))]))


(define (key kv) (first kv))
(define (val kv) (second kv))

(define (lookup-al k alst)
  (cond
    [(empty? alst) false]
    [(symbol=? k (key (first alst))) (val (first alst))]
    [else (lookup-al k (rest alst))]))



;; ~~~~~ 2d

;; (simplify ex) consumes an AExp and produces an equivalent AExp which has been simplified

;; Examples:

(check-expect (simplify (make-opnode '+ (list 4 'x 5)))
                        (make-opnode '+ (list 'x 9)))

(check-expect (simplify (make-opnode '* (list 4 'x 5)))
                        (make-opnode '* (list 'x 20)))

;; simplify: AExp -> AExp

(define (simplify ex)
  (cond
    [(symbol=? (opnode-op ex) '+)
     (maybe-simplify-constant '+ (simplify/lst (opnode-op ex) (opnode-args ex) 0))]
    [(symbol=? (opnode-op ex) '*)
     (maybe-simplify-constant '* (simplify/lst (opnode-op ex) (opnode-args ex) 1))]))

;; Tests:

(check-expect (simplify (make-opnode '+ (list 3 'x 3)))
                        (make-opnode '+ (list 'x 6)))

(check-expect (simplify (make-opnode '* (list 1 'x 1)))
                        (make-opnode '* (list 'x 1)))



;; (simplify/lst op lst acc) consumes an operator a (listof AExp) and an accumulator and produces a
;; simplified list

;; Example:

(check-expect (simplify/lst '* (list 3 'z 3) 1) (list 'z 9))

;; simplifiy/lst: Sym (listof AExp) Num -> (listof AExp)

(define (simplify/lst op lst acc)
  (cond
    [(empty? lst) (list acc)]
    [(symbol? (first lst)) (cons (first lst) (simplify/lst op (rest lst) acc))]
    [(opnode? (first lst)) (cons (simplify (rest lst)) (simplify/lst op (rest lst) acc))]
    [(symbol=? op '+) (simplify/lst op (rest lst) (+ (first lst) acc))]
    [(symbol=? op '*) (simplify/lst op (rest lst) (* (first lst) acc))]))

;; (maybe-simplify-constant op lst) consumes a operator and a list and produces if it has all
;; constants it simplifies the list, if not it produces an opnode

;; Example:

(check-expect (maybe-simplify-constant '+ (list 'x 2)) (make-opnode '+ (list 'x 2)))

;; maybe-simplify-constant: Sym (listof AExp) -> (anyof Nat OpNode)

(define (maybe-simplify-constant op lst)
  (cond
    [(num-list? lst) (eval (make-opnode op lst) empty)]
    [else (make-opnode op lst)]))

;; (num-list? lst) consumes a list and produces true if it's only made up of numbers. false otherwise

;; Example:

(check-expect (num-list? (list 1 2 2 3)) true)
(check-expect (num-list? (list 1 2 2 3 'z)) false)

;; num-list?: (listof Any) -> Bool

(define (num-list? lst)
  (cond
    [(empty? lst) true]
    [(not (number? (first lst))) false]
    [else (num-list? (rest lst))]))
