;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname components) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 07, question 1
;; ***************************************************
;;


(define-struct component (name num subcomponents))
;; A Component is a (make-component Str Num (listof Component))

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

;; (contains-component? comp name) consumes a component and a name and produces
;; true if the component or one of its subcomponents has the given name and false otherwise

;; Examples:

(check-expect (contains-component? bike "hub") true)
(check-expect (contains-component? bike "brake") false)

;; contains-component?: Component Str -> Bool

(define (contains-component? comp name)
  (cond
    [(string=? (component-name comp) name) true]
    [else (contains-subcomponent? (component-subcomponents comp) name)]))

;; Tests:
(check-expect (contains-component? bike "hub") true)
(check-expect (contains-component? bike "brake") false)


;; (contains-subcomponent? subcomp name) consumes a subcomponent and a name and produces true if the
;; subcomponent contains the name and false otherwise

;; Examples:

(check-expect (contains-subcomponent? empty "bike") false)

(check-expect (contains-subcomponent? (list
               (make-component "frame" 1 empty)
               (make-component "wheel" 2 empty)) "frame") true)

;; contains-subcomponent? Component Str -> Bool
(define (contains-subcomponent? subcomp name)
  (cond
    [(empty? subcomp) false]
    [(contains-component? (first subcomp) name) true]
    [else (contains-subcomponent? (rest subcomp) name)]))