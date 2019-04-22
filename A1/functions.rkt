;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 1
;; Problem 1
;; ############


;; (stirling n) produces the stirling's upper bound of n
;; stirling: Num -> Num
;; Examples:
(check-expect (stirling 0) 0)

(define (stirling n)
  (* (expt n (+ n 0.5))
     (exp (- 1 n))))

;; Tests:
(check-expect (stirling 1) 1)


;; (hm x y z) produces the harmonic mean of x, y and z
;; hm: Num Num Num -> Num
;; Examples:
(check-expect (hm 1 1 1) 1)
(check-expect (hm 1 2 2) 1.5)

(define (hm x y z)
  (/ 3 (+ (/ 1 x)
          (/ 1 y)
          (/ 1 z))))

;; Tests:
(check-expect (hm 3 3 3) 3)
(check-expect (hm 4 4 2) 3)


;; (height v t) produces the height of a ballistic motion
;;   in the velocity of v and the time of t
;; height: Num Num -> Num
;; Examples:
(check-expect (height 4.9 1) 0)

;; Constants:
(define g 9.8)

(define (height v t)
  (- (* v t)
     (* 0.5 g (sqr t))))

;; Tests:
(check-expect (height 9.8 1) 4.9)
(check-expect (height 9.8 2) 0)
