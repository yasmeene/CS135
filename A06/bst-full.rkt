;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname bst-full) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 06, question 3
;; ***************************************************
;;


(define-struct node (key left right))

;; A Node is a (make-node Nat BST BST)
;; requires: key > every key in left BST
;;           key < every key in right BST


;; A Binary Search Tree (BST) is one of:
;; * empty
;; * Node


;;~~~~ 3a

;; (full? bst) consumes a binary search tree and produces true iff
;; the given binary search tree is full

;; Examples:

(check-expect (full? empty) true)
(check-expect (full? (make-node 5
                                (make-node 1 empty empty)
                                (make-node 7
                                           (make-node 6 empty empty)
                                           (make-node 14 empty empty)))) true)

;; full?: BST -> Bool

(define (full? bst)
  (cond
    [(empty? bst) true]
    [(and (empty? (node-left bst)) (empty? (node-right bst))) true]
    [(or (and (empty? (node-left bst)) (not (empty? (node-right bst))))
         (and (empty? (node-right bst)) (not (empty? (node-left bst))))) false]
    [else (and (full? (node-right bst)) (full? (node-left bst)))]))


;; Tests:

(check-expect (full? (make-node 5 empty (make-node 6 empty (make-node 7 empty empty)))) false)

(check-expect (full? (make-node 9 (make-node 3 (make-node 1 empty empty) empty)
                                             (make-node 8 empty empty))) false)


;;~~~~ 3b

(define bad-full-tree (make-node 5 (make-node 1 empty empty)
                                 (make-node 6 (make-node 2 empty empty)
                                            (make-node 7 (make-node 3 empty empty)
                                                       (make-node 8 (make-node 4 empty empty)
                                                                  (make-node 9 empty empty))))))