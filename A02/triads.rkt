;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname triads) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Yasmeen Elkheir (21071183)
;; CS 135 Fall 2023
;; Assignment 02, Problem 5
;; ***************************************************
;;

;; converts pitches into numbered values

(define (pitch-num x)
  (cond
    [(symbol=? x 'c) 0]
    [(symbol=? x 'c#) 1]
    [(symbol=? x 'd) 2]
    [(symbol=? x 'd#) 3]
    [(symbol=? x 'e) 4]
    [(symbol=? x 'f) 5]
    [(symbol=? x 'f#) 6]
    [(symbol=? x 'g) 7]
    [(symbol=? x 'g#) 8]
    [(symbol=? x 'a) 9]
    [(symbol=? x 'a#) 10]
    [(symbol=? x 'b) 11]))


;; checks the triad type given a root and two other pitches

(define (triad-type r pitch_1 pitch_2) ; r is the root, pitch_1 and pitch_2 are any other two pitches
  (cond
    [(= (modulo (+ (pitch-num r) 7) 12) (pitch-num pitch_2)) (cond
                                   [(= (modulo (+ (pitch-num r) 4) 12) (pitch-num pitch_1) ) 'Major]
                                   [else 'Minor])]
    [(and (= (modulo (+ (pitch-num r) 3) 12) (pitch-num pitch_1)) (= (modulo (+ (pitch-num r) 6) 12) (pitch-num pitch_2))) 'Diminished]
    [else 'Augmented]))
