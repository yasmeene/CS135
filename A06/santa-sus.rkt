;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname santa-sus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 06, question 4
;; ***************************************************
;;

(define-struct action (name niceness desc))
;; An Action is a (make-action Str Int Str)
;; requires: -100 <= niceness <= 100
;;           niceness cannot be zero

(define instr-actlst  
  (list (make-action "Zahra" -4 "Procrastinated on CS 135 assignment")
        (make-action "Armin" 10 "Helped Zahra proofread the assignment")
        (make-action "Mark" 3 "Shared his lego with Byron during playtime")
        (make-action "Patrick" -10 "Joined a bicycle gang")
        (make-action "Armin" -1 "Stayed up too late on a schoolnight")
        (make-action "Charlie" -5 "Scribbled on the walls")
        (make-action "Byron" -4 "Harassed a goose in the park")
        (make-action "Zahra" -5 "Ate the ISAs' Cheetos")
        (make-action "Armin" 6 "Did his homework before going out to play")
        (make-action "Zahra" 3 "Cleaned her room without being nagged")
        (make-action "Zahra" -7 "Was disobedient at the grocery store")
        (make-action "Charlie" 5 "Saved Patrick from a rival bicycle gang")))


(define-struct wish (score gift))
;; A Wish is a (make-wish Nat Str)
;; requires: score > 0

;; A WishList is a (listof Wish)
;; requires: wishes are sorted in non-increasing order by niceness
;;           score.

;; A ChildrenList is either
;; * empty
;; * (cons (list Str WishList) ChildrenList)

(define instr-childrenlist 
  (list (list "Byron"   (list (make-wish 52 "Commodore 64")
                              (make-wish 17 "Hot Wheels")
                              (make-wish 10 "Colourful Pencils")))
        
        (list "Zahra"   (list (make-wish 57 "Ken's Mojo Dojo Casa House")
                              (make-wish 43 "Bibble Plushie")))
        
        (list "Mark"    (list (make-wish 37 "Telescope")
                              (make-wish 11 "Lego flowers")))
        
        (list "Charlie" (list (make-wish 32 "Amigurumi Bee Plushie")
                              (make-wish 20 "Chemistry Set")))
        
        (list "Patrick" (list (make-wish 42 "Nerf gun")
                              (make-wish 20 "Elvis Head Chia Pet")))
        
        (list "Armin"   (list (make-wish 54 "Toy Car")
                              (make-wish 32 "Very big pizza")
                              (make-wish 15 "Ice Cream")))))

(define-struct actionnode (name score actions left right))
;; An ActionNode is a (make-actionnode Str Int (listof Action)
;;                                      ActionSearchTree
;;                                      ActionSearchTree)
;; requires: name > every name in left ActionNode
;;           name < every name in right ActionNode

;; An ActionSearchTree (ActionST) is one of: 
;; * empty
;; * an ActionNode

;;~~~~ 4a

;; (extreme-actions name loa) consumes a child's name and a list of Actions and produces a list
;; of strings where the first element is the description of the action the child performed
;; with the smallest niceness score, and the seccond element is the description of the action the
;; child performed with the larges niceness score

;; Examples:

(check-expect
 (extreme-actions "Zahra" instr-actlst)
 (list "Was disobedient at the grocery store" "Cleaned her room without being nagged"))

(check-expect
 (extreme-actions "Patrick" instr-actlst)
 (list "Joined a bicycle gang" "Joined a bicycle gang"))

;; extreme-actions: Str (listof Actions) -> (listof Str)

(define (extreme-actions name loa)
  (cond
    [(empty? loa) empty]
    [(string=? name (action-name (first loa)))
     (max-min/acc loa name
                  (action-niceness (first loa))
                  (action-niceness (first loa))
                  (action-desc (first loa))
                  (action-desc (first loa)))]
    [else (extreme-actions name (rest loa))]))


;; (max-min/acc loa name max-so-far min-so-far s_desc l_desc) consumes a list of Actions, a name,
;; a max and min niceness, and a corresponding description to those niceness scores. The function
;; produces a list with the s_desc and l_desc for the consumed name

;; Example:

(check-expect (max-min/acc instr-actlst "Zahra"
                           (action-niceness (first instr-actlst))
                           (action-niceness (first instr-actlst))
                           (action-desc (first instr-actlst))
                           (action-desc (first instr-actlst)))
              (list "Was disobedient at the grocery store"
                    "Cleaned her room without being nagged"))

;; 
(define (max-min/acc loa name max-so-far min-so-far s_desc l_desc)
  (cond
    [(empty? loa) (list s_desc l_desc)]
    [(not (string=? name (action-name (first loa))))
     (max-min/acc (rest loa) name max-so-far min-so-far s_desc l_desc)]
    [(> (action-niceness (first loa)) max-so-far)
        (max-min/acc (rest loa)
                     name
                     (action-niceness (first loa))
                     min-so-far s_desc
                     (action-desc (first loa)))]
    [(< (action-niceness (first loa)) min-so-far)
     (max-min/acc
      (rest loa)
      name
      max-so-far
      (action-niceness (first loa))
      (action-desc (first loa))
      l_desc)]
    [else (max-min/acc (rest loa) name max-so-far min-so-far s_desc l_desc)]))

;; Tests:

(check-expect
 (extreme-actions "Byron" instr-actlst)
 (list
  "Harassed a goose in the park"
  "Harassed a goose in the park"))

(check-expect
 (extreme-actions "Yasmeen" instr-actlst) empty)

;;~~~~ 4b

;; (gifts-received name score loc threshold) consumes a child’s name, that child’s total
;; niceness score, a ChildrenList, and a negative integer representing Santa’s naughtiness
;; threshold and produces the gifts the child recieves according to where their score falls within
;; santas threshold

;; Examples:

(check-expect (gifts-received "Byron" 20 instr-childrenlist -10)
              (list "Playing Card Deck"
                    "Colourful Pencils"
                    "Hot Wheels"))
(check-expect (gifts-received "Armin" 10 instr-childrenlist -11)
              (list "Playing Card Deck"))

;; gifts-received: Str Num ChildrenList Num -> (anyof Sym (listof Str))
;; Requires:  Num < 0

(define (gifts-received name score loc threshold)
  (cond
    [(empty? loc) empty]
    [(string=? name (first (first loc))) (cond
                                           [(> threshold score) 'krampus]
                                           [(> 0 score) 'coal]
                                           [else (cons "Playing Card Deck"
                                                       (poss-gifts name score
                                                                   (first (rest (first loc)))))])]
    [else (gifts-received name score (rest loc) threshold)]))


;; Tests:

(check-expect (gifts-received "Patrick" -10 instr-childrenlist -12)
              'coal)
(check-expect (gifts-received "Zahra" -15 instr-childrenlist -13)
              'krampus)
(check-expect (gifts-received "Yasmeen" -10 instr-childrenlist -10)
              empty)

;; (poss-gifts name score loc) consumes a ChildrenList and produces a list of make-wishes
;; corresponding to a name

(define (poss-gifts name score loc)
  (cond
    [(empty? loc) empty]
    [(>= score (wish-score (first loc))) (poss-gifts-lst loc)]
    [else (poss-gifts name score (rest loc))]))

;; (poss-gifts-lst loc) consumes a ChildrenList and creates a non-decreasing list of gifts by
;; niceness score

(define (poss-gifts-lst loc)
  (poss-gifts-lst/acc (rest loc) (first loc)))


;; (poss-gifts-lst/acc loc min-so-far) consumes a ChildrenList and a min-so-far and is an
;; accumulator for poss-gifts-lst
(define (poss-gifts-lst/acc loc min-so-far)
  (cond
    [(empty? loc) (cons (wish-gift min-so-far) empty)]
    [(<= (wish-score (first loc)) (wish-score min-so-far))
     (cons (wish-gift (first loc)) (poss-gifts-lst/acc (rest loc) min-so-far))]
    [else (cons (wish-gift min-so-far) (poss-gifts-lst/acc (rest loc) (first loc)))]))

;;~~~~ 4c

;; (add-action action actionst) consumes an Action and an ActionSearchTree and 
;; produces an ActionST consisting of the original tree with the new Action added

;; Examples:
(check-expect (add-action
               (make-action "Byron" 3 "Prepared tutorial questions")
               (make-actionnode
                "Mark"
                10
                (list (make-action "Mark" "Bought some stuff"))
                
                (make-actionnode
                 "Byron"
                 10
                 (list (make-action "Byron" 10 "taught the class trees"))
                 empty empty)
                
                (make-actionnode
                 "Zarah"
                 11
                 (list (make-action "Zahra" 11 "taught cool stuff"))
                 empty empty)))
              
              (make-actionnode
               "Mark"
               10
               (list (make-action "Mark" 10 "Bought some stuff"))
                
               (make-actionnode
                "Byron"
                10
                (list (make-action "Byron" 10 "taught the class trees")
                      (make-action "Byron" 3 "Prepared tutorial questions"))
                empty empty)
                
               (make-actionnode
                "Zarah"
                11
                (list (make-action "Zahra" 11 "taught cool stuff"))
                empty empty)))

;; add-action: Action ActionST -> ActionST

(define (add-action action actionst)
  (cond
    [(empty? actionst) (make-actionnode (action-name action) (action-niceness action)
                                                (list action)
                                                empty empty)]
    [(string=? (action-name action) (actionnode-name actionst))
     (make-actionnode (action-name action)
                      (+ (action-niceness action) (actionnode-score actionst))
                      (append (list action) (actionnode-actions actionst))
                      (actionnode-left actionst)
                      (actionnode-right actionst))]
    [(string<? (action-name action) (actionnode-name actionst))
     (make-actionnode (actionnode-name actionst)
                      (actionnode-score actionst)
                      (actionnode-actions actionst)
                      (add-action action (actionnode-left actionst))
                      (actionnode-right actionst))]
    [(string>? (action-name action) (actionnode-name actionst))
     (make-actionnode (actionnode-name actionst)
                      (actionnode-score actionst)
                      (actionnode-actions actionst)
                      (actionnode-left actionst)
                      (add-action action (actionnode-right actionst)))]))

;; Tests:

(check-expect (add-action
               (make-action "Byron" 3 "Prepared tutorial questions")
               (make-actionnode
                "Mark"
                10
                (list (make-action "Mark" "Bought some stuff"))
                
                (make-actionnode
                 "Byron"
                 10
                 (list (make-action "Byron" 10 "taught the class trees"))
                 empty empty)
                
                (make-actionnode
                 "Zarah"
                 11
                 (list (make-action "Zahra" 11 "taught cool stuff"))
                 empty empty)))
              
              (make-actionnode
                "Mark"
                10
                (list (make-action "Mark" "Bought some stuff"))
                
                (make-actionnode
                 "Byron"
                 10
                 (list (make-action "Byron" 10 "taught the class trees")
                       (make-action "Byron" 3 "Prepared tutorial questions"))
                 empty empty)
                
                (make-actionnode
                 "Zarah"
                 11
                 (list (make-action "Zahra" 11 "taught cool stuff"))
                 empty empty)))




;; ~~~~ 4d

;; (gift-list actionst loc threshold) consumes an ActionST, a ChildrenList, and a naughtiness
;; threshold and produces a list of pairs. The first element of each pair is the child’s name,
;; and the second is the gift(s) the child receives.

;; Example:
(check-expect
 (gift-list
  (make-actionnode
   "Mark"
   12
   (list (make-action "Mark" 12 "Bought the ISAs more Cheetos"))
   (make-actionnode 
    "Byron"
    2
    (list (make-action "Byron" 2 "Recycled an empty Cheetos packet"))
    empty
    empty)
   (make-actionnode 
    "Zahra" 
    -5
    (list (make-action "Zahra" -5 "Ate the ISAs' Cheetos"))
    empty
    empty)) instr-childrenlist -12)
 (list (list "Byron" (list "Playing Card Deck")) 
       (list "Mark" (list "Playing Card Deck" "Lego flowers"))
       (list "Zahra" 'coal)))

;; gift-list: ActionST ChildrenList Num -> listof Any
(define (gift-list actionst loc threshold)
  (cond
    [(empty? actionst) empty]
    [else (append
           (append (gift-list (actionnode-left actionst) loc threshold)
                   (list (list (actionnode-name actionst) (gifts-received
                                                           (actionnode-name actionst)
                                                           (actionnode-score actionst) loc
                                                           threshold))))
           (gift-list (actionnode-right actionst) loc threshold))]))

;; Test:

(check-expect
 (gift-list
  (make-actionnode
   "Mark"
   12
   (list (make-action "Mark" 12 "Bought the ISAs more Cheetos"))
   (make-actionnode 
    "Byron"
    2
    (list (make-action "Byron" 2 "Recycled an empty Cheetos packet"))
    empty
    empty)
   (make-actionnode 
    "Zahra" 
    -5
    (list (make-action "Zahra" -5 "Ate the ISAs' Cheetos"))
    empty
    empty)) instr-childrenlist -12)
 (list (list "Byron" (list "Playing Card Deck")) 
       (list "Mark" (list "Playing Card Deck" "Lego flowers"))
       (list "Zahra" 'coal)))