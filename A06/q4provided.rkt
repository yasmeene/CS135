;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname q4provided) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
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

define-struct wish (score gift))
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
