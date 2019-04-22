;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname composite) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 9
;; Problem 3
;; ############


;; a 

;; (composite f g) consumes two functions and produces
;;   the composite function that f(g(x))
;; composite: (X -> Any) (Any -> X) -> (Any -> Any)
;; Examples:
(check-expect ((composite add1 abs) -1) 2)

(define (composite f g)
  (lambda (x)
    (f (g x))))

;; Tests:
(check-expect ((composite abs add1) -5) 4)
(check-expect ((composite add1 add1) 3) 5)


;; b

;; (inverse-of-square-list lopn) consumes a list of positive
;;   numbers and produces a new list of positive numbers with
;;   each element being the inverse of the square of the 
;;   element in the original list
;; inverse-of-square-list: (listof Num) -> (listof Num)
;; requires: All elements in lopn must be > 0
;; Examples:
(check-expect (inverse-of-square-list
               (list 1 2 5))
              (list 1 0.25 0.04))

(define (inverse-of-square-list lopn)
  (map (composite 
        (lambda (x) (/ 1 x)) sqr) lopn))

;; Tests:
(check-expect (inverse-of-square-list
               (list 1 2 10))
              (list 1 0.25 0.01))
(check-expect (inverse-of-square-list
               (list 2 10 100))
              (list 0.25 0.01 0.0001))


;; c

;; (composite-list lof) consumes a list of functions and
;;   produces the composite function (f1(f2..fn(x)))
;; composite-list: (listof (Num -> Num)) -> (Num -> Num)
;; Examples:
(check-expect ((composite-list 
                (list abs add1 add1)) -4) 2)

(define (composite-list lof)
  (cond
    [(empty? lof) (lambda (x) x)]
    [else 
     (foldr (lambda (x y)
              (cond
                [(empty? y) x]
                [else (composite x y)]))
            empty lof)]))

;; Tests:
(check-expect ((composite-list
                (list sqr add1 abs)) -5) 36)
(check-expect ((composite-list empty) 520) 520)












