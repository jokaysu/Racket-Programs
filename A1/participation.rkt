;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname participation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 1
;; Problem 4
;; ############

;; (cs135-participation total correct incorrect) produces
;;   the total participation grade
;;   for CS 135
;; cs135-participation: Num Num Num -> Num
;; Examples:
(check-expect (cs135-participation 12 10 2) 100)

;; Contants:
(define pass (/ 3 4))

(define (cs135-participation total correct incorrect)
  (* (/ (- (+ (* 2 correct) incorrect)
           (* (round (+ (/ (- (+ correct incorrect)
                              (* total pass))
                           10000) 0.5))
              (- (+ correct incorrect)
                 (* total pass)))
           (* (round (+ (/ (- correct (* total pass))
                           10000) 0.5))
              (- correct (* total pass))))
        (* 2 (* pass total)))
     100))
  
;; Tests:
(check-expect (cs135-participation 12 8 2) (/ 1700 18))
(check-expect (cs135-participation 16 13 1) 100)
(check-expect (cs135-participation 16 6 2) (/ 700 12))