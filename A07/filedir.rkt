;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname fildedir) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 07, question 3
;; ***************************************************
;;

(require "fs-print.rkt")

(define example-fs
  (make-dir "home"
            (list 
             (make-file "document.txt" 500 1636842620)
             (make-dir "images"
                       (list 
                        (make-file "cat.jpg" 2000 1636842630)
                        (make-file "dog.jpg" 1500 1636842640)))
             (make-dir "music"
                       (list 
                        (make-file "song1.mp3" 3000 1636842650)
                        (make-file "song2.mp3" 2500 1636842660)))
             (make-file "notes.txt" 100 1636842670))))

;; ~~~~~ 3a

;; (list-files fd) consumes a FileDir and produces a (listof Str) that contains the names of
;; all the files in fd and produces the files in the same order as generated

;; Example:

(check-expect (list-files sample-fs) (list "readme.txt"
                                           "zahra.jpg"
                                           "timbit.jpg"
                                           "chonky.jpg"
                                           "beach1.jpg"
                                           "beach2.jpg"
                                           "beach3.jpg"
                                           "oreo.jpg"
                                           "anna.jpg"
                                           "beach1.jpg"
                                           "eagles-hotel-california.mp3"
                                           "bee-gees-stayin-alive.mp3"
                                           "lady-gaga-bad-romance.mp3"
                                           "beyonce-single-ladies.mp3"
                                           "shopping.txt"
                                           "todo.txt"))

;; list-files: FileDir -> (listof Str)

(define (list-files fd)
  (cond
    [(file? fd) (list (file-name fd))]
    [(dir? fd) (find-files (dir-contents fd))]))


;; Test:

(check-expect (list-files example-fs) (list "document.txt"
                                           "cat.jpg"
                                           "dog.jpg"
                                           "song1.mp3"
                                           "song2.mp3"
                                           "notes.txt"))


;; (find-files lofd) consumes a FDList and produces the names of all the files
;; within that directory

;; Examples:

(check-expect (find-files (list
                           (make-file "yasmeen" 18 8)
                           (make-dir "school"
                                     (list
                                      (make-file "waterloo" 19 2023)
                                      (make-file "laurier" 18 2023)
                                      (make-file "uoft" 17 2023)))))
              (list "yasmeen" "waterloo" "laurier" "uoft"))

;; find-files: FDList -> (listof Str)
(define (find-files lofd)
  (cond
    [(empty? lofd) empty]
    [(file? (first lofd)) (cons (file-name (first lofd))
                                (find-files (rest lofd)))]
    [else (append (list-files (first lofd)) (find-files (rest lofd)))]))



;; ~~~~~ 3b

;; (backup dir) consumes a Dir and produces a Dir with an extra copy (backup) of each file it contains

;; Examples:

(check-expect (backup (make-dir "file"
                                (list
                                 (make-file "filedir.rkt" 5400 1698197579))))
              (make-dir
               "file"
               (list
                (make-file
                 "filedir.rkt"
                 5400
                 1698197579)
                (make-file
                 "filedir.rkt.bak"
                 5400
                 1698197579))))

(check-expect (backup (make-dir "readme"
                                (list
                                 (make-file "readme.txt" 187 1319502441))))
              (make-dir "readme"
               (list
                (make-file "readme.txt" 187 1319502441)
                (make-file "readme.txt.bak" 187 1319502441))))


;; backup: Dir -> Dir
(define (backup dir)
  (cond
    [(empty? (dir-contents dir)) (make-dir (dir-name dir)
                                           empty)]
    [else (make-dir (dir-name dir) (bak-file (dir-contents dir)))]))

;; Tests:

(check-expect (backup (make-dir "chonky"
                                (list
                                 (make-file "chonky.jpg" 2709000 1319287654))))
              (make-dir "chonky"
               (list
                (make-file "chonky.jpg" 2709000 1319287654)
                (make-file "chonky.jpg.bak" 2709000 1319287654))))

(check-expect (backup (make-dir "lady-gaga"
                                (list
                                 (make-file "lady-gaga-bad-romance.mp3" 9376000 1319612418))))
              (make-dir "lady-gaga"
               (list
                (make-file "lady-gaga-bad-romance.mp3" 9376000 1319612418)
                (make-file "lady-gaga-bad-romance.mp3.bak" 9376000 1319612418))))

(check-expect (backup (make-dir "empty"
                                empty))
              (make-dir "empty"
                        empty))


;; (bak-file) consumes a FDList and produces the same list but with each file having a backup

;; Example:

(check-expect (bak-file (list
                           (make-file "yasmeen" 18 8)
                           (make-dir "school"
                                     (list
                                      (make-file "waterloo" 19 2023)
                                      (make-file "laurier" 18 2023)
                                      (make-file "uoft" 17 2023)))))
              (list (make-file "yasmeen" 18 8)
                    (make-file "yasmeen.bak" 18 8)
                    (make-dir "school"
                          (list
                           (make-file "waterloo" 19 2023)
                           (make-file "waterloo.bak" 19 2023)
                           (make-file "laurier" 18 2023)
                           (make-file "laurier.bak" 18 2023)
                           (make-file "uoft" 17 2023)
                           (make-file "uoft.bak" 17 2023)))))

;; bak-file: FDList -> FDList

(define (bak-file fdl)
  (cond
    [(empty? fdl) empty]
    [(file? (first fdl)) (cons (first fdl)
                               (cons (make-file (string-append (file-name (first fdl)) ".bak")
                                                (file-size (first fdl))
                                                (file-timestamp (first fdl)))
                                     (bak-file (rest fdl))))]
    [(dir? (first fdl)) (cons (backup (first fdl)) (bak-file (rest fdl)))]))


;; ~~~~~ 3c

;; (get-time fdr) consumes a FileDir and produces the timestamp of a file or the largest timestamp of
;; any file in a directory. False if there are no files

;; Examples:

(check-expect (get-time (make-file "filedir.rkt" 5400 1698197579))
              1698197579)

(check-expect (get-time (make-file "readme.txt" 187 1319502441))
              1319502441)

(check-expect (get-time (make-file "chonky.jpg" 2709000 1319287654))
              1319287654)


;; get-time: FileDir -> (anyof Nat Bool)

(define (get-time fdr)
  (pos-time (find-time fdr)))

;; Tests:

(check-expect (get-time (make-file "lady-gaga-bad-romance.mp3" 9376000 1319612418))
              1319612418)

(check-expect (get-time (make-dir "vacation"
                                    (list                    
                                     (make-file "beach1.jpg" 3297000 1319320493)
                                     (make-file "beach2.jpg" 2173000 1319449661)
                                     (make-file "beach3.jpg" 2747000 1319617966))))
              1319617966)

(check-expect (get-time (make-dir "rock"
                                    (list 
                                     (make-file "eagles-hotel-california.mp3" 10184000 1319658262)
                                     (make-file "bee-gees-stayin-alive.mp3" 9693000 1319660517))))
              1319660517)

(check-expect (get-time (make-dir "dance"
                                    (list 
                                     (make-file "lady-gaga-bad-romance.mp3" 9376000 1319612418)
                                     (make-file "beyonce-single-ladies.mp3" 17669000 1319207025))))
              1319612418)


;; (find-time fdl) consumes a FDList and produces a timestamp

;; find-time: (listof FileDir) -> (listof Nat)
(define (find-time fdl)
  (cond
    [(file? fdl) (file-timestamp fdl)]
    [else (max-time (dir-contents fdl) -1)]))

;; (pos-time val) consumes a number and returns false if its -1 and the value otherwise

;; pos-time: Int -> (anyof false Nat)
(define (pos-time val)
  (cond
    [(= val -1) false]
    [else val]))

;; (max-time fdl maxx) produces the highest timestamp in FDList

;; max-time: FDList Int -> Int

(define (max-time fdl maxx)
  (cond
    [(empty? fdl) maxx]
    [else (max-time (rest fdl) (max (find-time (first fdl)) maxx))]))

;; ~~~~~ 3d

;; A Path is a (listof Str).

;; (find name dir) finds every file and directory in dir that has the consumed name

;; Examples:

(check-expect (find "vacation" sample-fs)
              (list (list "root" "photos" "vacation")))

(check-expect (find "shopping.txt" sample-fs)
              (list (list "root" "notes" "shopping.txt")))

;; find: Str Dir -> Path

(define (find name dir)
  (cond
    [(file? dir) (cond
                   [(string=? name (file-name dir)) (list (list name))]
                   [else empty])]
    [(dir? dir) (cond
                  [(string=? name (dir-name dir))
                   (cons (list name)
                         (add-dirname (dir-name dir)
                                      (search-contents (dir-contents dir) (dir-name dir) name)))]
                  [else (add-dirname (dir-name dir)
                                     (search-contents (dir-contents dir) (dir-name dir) name))])]
    [else empty]))


;; Tests:

(check-expect (find "beach1.jpg" sample-fs)
              (list (list "root" "photos" "beach1.jpg")
                    (list "root" "photos" "vacation" "beach1.jpg")))

(check-expect (find "readme.txt"
                    (make-file "readme.txt" 187 1319502441))
              (list (list "readme.txt")))

(check-expect (find "a06.rkt" sample-fs)
              empty)

;; (search-contents fdlir dirname name) Consumes a FDList, dirname and a name and produces a
;; (listof Path) 
;; search-contents FDList Str Str -> (listof Path)

(define (search-contents fdlir dirstr name)
  (cond
    [(empty? fdlir) empty]
    [(not (empty? (find name (first fdlir))))
     (append (search-contents (rest fdlir) dirstr name)
             (find name (first fdlir)))]
    [else (search-contents (rest fdlir) dirstr name)]))

;; Example:
(check-expect (add-dirname "root" (list (list "todo") (list "school" "todo")))
              (list (list "root" "todo") (list "root" "school" "todo")))
(check-expect (add-dirname "root" (list (list "school")))
              (list (list "root" "school")))

;; (add-dirname) Consumes a dirname and a listof Path, and produces a listofPath
;; add-dirname: Str (listof Path) -> (listof Path)
(define (add-dirname dirname lop)
  (cond
    [(empty? lop) empty]
    [(empty? (rest lop)) (list (cons dirname (first lop)))]
    [else (cons (cons dirname (first lop)) (add-dirname dirname (rest lop)))]))
