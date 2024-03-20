;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname course-selection) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 05, question 2
;; ***************************************************
;;

;; 2a ~~~~~~

;; A DesiredCourses is one of:
;; * empty
;; * (cons (list Str (listof Sym)) DesiredCourses)

(define selections
  (list
   (list "lanarey" (list 'MATH145 'MATH147 'CS145 'COMMST100 'MUSIC140))
   (list "shawkings" (list 'PHYS111 'SCI206 ' MATH145 'FINE100))
   (list "2pac" (list 'ANTH241 'STAT230 'SCI206 'MUSIC140))
   (list "6obama" (list 'COMMST223 'PSCI281 'PSCI231 'MATH137)))) 

;; (taking-course? dc q-user c-code) consumes a DesiredCourses, a student’s Quest username, and a
;; course code, and produces true if the student has selected the course (and false otherwise).

;; Example:

(check-expect (taking-course? selections "lanarey" 'MATH145) true)
(check-expect (taking-course? empty "lanarey" 'MATH145) false)

;; taking-course?: DesiredCourses Str Sym -> Bool 

(define (taking-course? dc q-user c-code)
  (cond
    [(empty? dc) false]
    [(member? q-user (key dc)) (cond
                                 [(member? c-code (lookup-dc q-user dc)) true]
                                 [else false])]
    [else (taking-course? (rest dc) q-user c-code)]))

;; Tests:

(check-expect (taking-course? selections "shawkings" 'SCI206) true)
(check-expect (taking-course? selections "2pac" 'ANTH101) false)
(check-expect (taking-course? selections "yelkheir" 'ANTH241) false)
(check-expect (taking-course? selections "6obama" 'PSCI281) true)


(define (key selections) (first selections))
(define (val selections) (second selections))

;; (lookup-dc k alst) consumes a key and a dictionary and produces the corresponding
;; value when it's found
 
;; Example:

(check-expect (lookup-dc "lanarey" selections) (list 'MATH145 'MATH147 'CS145 'COMMST100 'MUSIC140))
(check-expect (lookup-dc "6obama" empty) false)
(check-expect (lookup-dc "2pac" selections) (list 'ANTH241 'STAT230 'SCI206 'MUSIC140))

;; lookup-dc: Str DesiredCourse -> anyof (listof Sym) Bool

(define (lookup-dc k alst)
  (cond
    [(empty? alst) false]
    [(string=? k (key (first alst))) (val (first alst))]
    [else (lookup-dc k (rest alst))]))


;; 2b ~~~~~~

;; (missed-deadline-add dc q-user) consumes a DesiredCourses and a student’s Quest username.
;; Unless the student already appears in DesiredCourses, it adds the student to the end.

;; Examples: 

(check-expect (missed-deadline-add selections "m3marco") (list
   (list "lanarey" (list 'MATH145 'MATH147 'CS145 'COMMST100 'MUSIC140))
   (list "shawkings" (list 'PHYS111 'SCI206 ' MATH145 'FINE100))
   (list "2pac" (list 'ANTH241 'STAT230 'SCI206 'MUSIC140))
   (list "6obama" (list 'COMMST223 'PSCI281 'PSCI231 'MATH137))
   (list "m3marco" empty)))

;; missed-deadline-add: DesiredCourse Str -> DesiredCourse

(define (missed-deadline-add dc q-user)
  (cond
    [(empty? dc) empty]
    [(user-exist? dc q-user) dc]
    [else (my-append dc q-user)]))

;; Tests:

(check-expect (missed-deadline-add selections "totoro") (list
   (list "lanarey" (list 'MATH145 'MATH147 'CS145 'COMMST100 'MUSIC140))
   (list "shawkings" (list 'PHYS111 'SCI206 ' MATH145 'FINE100))
   (list "2pac" (list 'ANTH241 'STAT230 'SCI206 'MUSIC140))
   (list "6obama" (list 'COMMST223 'PSCI281 'PSCI231 'MATH137))
   (list "totoro" empty)))

(check-expect (missed-deadline-add selections "6obama") (list
   (list "lanarey" (list 'MATH145 'MATH147 'CS145 'COMMST100 'MUSIC140))
   (list "shawkings" (list 'PHYS111 'SCI206 ' MATH145 'FINE100))
   (list "2pac" (list 'ANTH241 'STAT230 'SCI206 'MUSIC140))
   (list "6obama" (list 'COMMST223 'PSCI281 'PSCI231 'MATH137))))

(check-expect (missed-deadline-add empty "lanarey") empty)

;; (my-append lst1 lst2) appends lst2 to the end of lst1

;; Examples: ***************

;; (check-expect (my-append empty "apple") (list "apple"))
;; (check-expect (my-append (list 3 4) "cherry-cola") (list (list 3 4) "cherry-cola"))

;; my-append: (listof Any) Str -> (listof Any)

(define (my-append lst user)
  (cond
    [(empty? lst) (list (list user empty))]
    [else (cons (first lst)
                (my-append (rest lst) user))]))


;; (user-exist? dc q-user) consumes DesiredCourses and a quest username and checks if the usename
;; already exists in DesiredCourses.

;; Examples:

(check-expect (user-exist? selections "2pac") true)
(check-expect (user-exist? selections "totoro") false)

;; user-exist?: DesiredCourses Str -> Bool

(define (user-exist? dc q-user)
  (cond
    [(empty? dc) false]
    [(member? q-user (key dc)) true]
    [else (user-exist? (rest dc) q-user)]))


;; 2c ~~~~~~

;; (add-course dc q-user c-course) consumes a DesiredCourses, a student’s Quest username, and a course
;; code, and adds the given code to the end of the list of courses the student wishes to take.

;; Example:

(check-expect (add-course selections "2pac" 'MATH115) (list
   (list "lanarey" (list 'MATH145 'MATH147 'CS145 'COMMST100 'MUSIC140))
   (list "shawkings" (list 'PHYS111 'SCI206 ' MATH145 'FINE100))
   (list "2pac" (list 'ANTH241 'STAT230 'SCI206 'MUSIC140 'MATH115))
   (list "6obama" (list 'COMMST223 'PSCI281 'PSCI231 'MATH137))))

 
;; add-course: DesiredCourses Str Sym -> DesiredCourses

(define (add-course dc q-user c-code)
  (cond
    [(taking-course? dc q-user c-code) dc]
    [(user-exist? dc q-user) (add2end dc q-user c-code)]
    [else (append dc (list (list q-user (list c-code))))]))

;; Tests:

(check-expect (add-course selections "totoro" 'MATH115) (list
   (list "lanarey" (list 'MATH145 'MATH147 'CS145 'COMMST100 'MUSIC140))
   (list "shawkings" (list 'PHYS111 'SCI206 ' MATH145 'FINE100))
   (list "2pac" (list 'ANTH241 'STAT230 'SCI206 'MUSIC140))
   (list "6obama" (list 'COMMST223 'PSCI281 'PSCI231 'MATH137))
   (list "totoro" (list 'MATH115))))

(check-expect (add-course selections "6obama" 'COMMST223) (list
   (list "lanarey" (list 'MATH145 'MATH147 'CS145 'COMMST100 'MUSIC140))
   (list "shawkings" (list 'PHYS111 'SCI206 ' MATH145 'FINE100))
   (list "2pac" (list 'ANTH241 'STAT230 'SCI206 'MUSIC140))
   (list "6obama" (list 'COMMST223 'PSCI281 'PSCI231 'MATH137))))

(define (add2end dc q-user c-code)
  (cond
    [(empty? dc) empty]
    [(string=? (first (first dc)) q-user)
     (cons (list (first (first dc)) (add2end-lst (second (first dc)) c-code)) (rest dc))]
    [else (cons (first dc) (add2end (rest dc) q-user c-code))]))

;; Test:

(check-expect (add2end empty "shawkings" 'GEOG219) empty)


(define (add2end-lst lst-user c-code)
  [cond
    [(member? c-code lst-user) lst-user]
    [else (append lst-user (list c-code))]])

;; Test:

(check-expect (add2end-lst (list 'ANTH241 'STAT230 'SCI206 'MUSIC140) 'ANTH241)
              (list 'ANTH241 'STAT230 'SCI206 'MUSIC140))

;; 2d ~~~~~~

;; (create-classlist dc c-code) consumes a DesiredCourses and a course code, and produces a
;; list of all the students that want to take the consumed course

;; Example:

(check-expect (create-classlist selections 'MATH145) (list "lanarey" "shawkings"))

;; create-classlist: DesiredCourses Sym -> (listof Str)

(define (create-classlist dc c-code)
  (cond
    [(empty? dc) empty]
    [(member? c-code (key (rest (key dc)))) (cons (key (key dc))
                                                     (create-classlist (rest dc) c-code))]
    [else (create-classlist (rest dc) c-code)]))
                               
;; Tests:

(check-expect (create-classlist selections 'MUSIC140) (list "lanarey" "2pac"))
(check-expect (create-classlist selections 'PSCI281) (list "6obama"))