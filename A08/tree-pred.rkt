;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tree-pred) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 08, question 3
;; ***************************************************
;;

(define-struct node (key left right))
;; A Node is a (make-node Nat BT BT)

;; A Binary Tree (BT) is one of:
;; * empty
;; * Node

(define t (make-node 5
                     (make-node 10 empty empty)
                     (make-node 15
                                (make-node 20 empty empty)
                                (make-node 33 empty empty))))

;; (tree-pred pred) consumes a one-argument predicate and produces a function that consumes a binary
;; tree and produces true if the predicate is valid for every value in the tree, false othwerwise.

;; Examples:

(check-expect ((tree-pred even?) t) false)
(check-expect ((tree-pred positive?) t) true)

;; tree-pred: (Nat -> Bool) -> Bool

(define (tree-pred pred)
  (local
    [;; (tree-pred-helper t) consumes a tree and checks if the tree
     ;; is valid for the predicate

     ;; tree-pred-helper: BT -> Bool
     (define (tree-pred-helper t)
       (cond
         [(empty? t) true]
         [(node? t)
          (and (pred (node-key t))
               (tree-pred-helper (node-left t))
               (tree-pred-helper (node-right t)))]))]
  (lambda (t) (tree-pred-helper t))))

;; Tests:

(check-expect ((tree-pred string?) t) false)
(check-expect ((tree-pred number?) t) true)