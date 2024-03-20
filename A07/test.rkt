;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname test) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
(require "fs-print.rkt")

(check-expect (find "shopping.txt" sample-fs)
              (list (list "root" "notes" "shopping.txt")))

;; find: Str Dir -> Path

(define (find name dir)
  (cond
    [(empty? (dir-contents dir)) empty]
    [(and (file? (first (dir-contents dir)))
          (string=? name (file-name (first (dir-contents dir)))))
     (list name (find name (rest (dir-contents dir))))]
    [else (append (find name (rest (dir-contents dir))) (first (dir-contents dir)))]))