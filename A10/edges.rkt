;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname edges) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 10, question 1
;; ***************************************************
;;


;; A Node is a Sym
;; A Graph is a (listof (list Node (listof Node)))

;; An EdgeList is a (listof (list Node Node))

;; A Graph is one of:
;; * empty
;; * (cons (list v (list w_1 ... w_n)) g)
;;   where g is a graph
;;   v, w_1 ... w_n are Nodes
;;   v is the in-neighbour to w_1 ... w_n in the Graph
;;   v does not appear as an in-neighbour in g

;; ~~~~~ 1a

;; (adj->edge adj-g) consumes a graph in  adjacency-list representation and converts it into
;; edge-list representation.

;; Example:

(check-expect (adj->edge '((A (C D E)) (B (E J)) (C ()) (D (F J))
                                       (E (K)) (F (K H)) (H ()) (J (H)) (K ())))
              '((A C) (A D) (A E) (B E) (B J) (D F) (D J)
                      (E K) (F K) (F H) (J H)))

;; adj->edge: Graph -> EdgeList

(define (adj->edge adj-g)
  (local
    [;; (flatten-pairs pair) consumes a nested list (a pair) and flattens it
     ;; flatten: (listof (listof Any)) -> (listof Any)
     (define (flatten-pairs pair)
       (cond
         [(empty? pair) empty]
         [else (append (first pair) (flatten-pairs (rest pair)))]))]
    (flatten-pairs (map (lambda (x)
                          (map (lambda (y)
                                 (list (first x) y))
                               (first (rest x))))
                        adj-g))))

;; Tests:

(check-expect (adj->edge '((A (D E)) (B (E J)) (C ()) (D (F J))
                                       (E (K)) (F (K H)) (H ()) (J (H)) (K ())))
              '((A D) (A E) (B E) (B J) (D F) (D J)
                      (E K) (F K) (F H) (J H)))

(check-expect (adj->edge '((A (D E)) (B (E J)) (C ()) (D (F J))
                                       (E (K)) (F (K H)) (H ()) (J (H)) (K ()) (L ())))
              '((A D) (A E) (B E) (B J) (D F) (D J)
                      (E K) (F K) (F H) (J H)))


;; ~~~~~ 1b

;; (neighbours v edge-g) consumes a node v and a graph edge-g in edge-list representation,
;; and produces the list of out-neighbours of v in edge-g

;; Examples:

(check-expect (neighbours 'A '((A D) (A E) (B E) (B J) (D F) (D J)
                      (E K) (F K) (F H) (J H)))
              '(D E))

(check-expect (neighbours 'B '((A C) (A D) (A E) (B E) (B J) (D F) (D J)
                      (E K) (F K) (F H) (J H)))
              '(E J))

;; neighbours: Node EdgeList -> (listof Node)

(define (neighbours v edge-g)
  (cond
    [(empty? edge-g) empty]
    [(symbol=? v (first (first edge-g))) (cons (second (first edge-g))
                                                 (neighbours v (rest edge-g)))]
    [else (neighbours v (rest edge-g))]))

;; Tests:

(check-expect (neighbours 'J '((A C) (A D) (A E) (B E) (B J) (D F) (D J)
                      (E K) (F K) (F H) (J H)))
              '(H))

(check-expect (neighbours 'L '((A C) (A D) (A E) (B E) (B J) (D F) (D J)
                      (E K) (F K) (F H) (J H)))
              '())

;; ~~~~~ 1c

;; (edge->adj edge-g) converts a graph in edge-list representation to a graph in
;; adjacency-list representation

;; Examples

(check-expect (edge->adj '((A C) (A D) (A E) (B E) (B J) (D F) (D J)
                                 (E K) (F K) (F H) (J H)))
              '(
                (A (C D E))
                (C ())
                (D (F J))
                (E (K))
                (B (E J))
                (J (H))
                (F (K H))
                (K ())
                (H ())))

;; edge->adj: EdgeList -> Graph

(define (edge->adj edge-g)
  (map (lambda (x) (list x (neighbours x edge-g)))
       (foldr (lambda (y z) (cons y (filter (lambda (w) (not (equal? y w))) z)))
              empty (foldr append empty edge-g))))


;; Tests:

(check-expect (edge->adj '((A D) (A E) (B E) (B J) (D F) (D J)
                                 (E K) (F K) (F H) (J H)))
              '(
               (A (D E))
               (D (F J))
               (E (K))
               (B (E J))
               (J (H))
               (F (K H))
               (K ())
               (H ())))

(check-expect (edge->adj '()) '())