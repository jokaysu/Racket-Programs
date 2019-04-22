;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mapfn) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 8
;; Problem 2
;; ############


;; a

;; (mapfn lof lon) consumes a list of functions and a list of
;;   two numbers and produces the results of each function 
;;   in the given two numbers.
;; mapfn: (listof (Any Any -> Any)) (listof Num) -> (listof Any)
;; Examples:
(check-expect (mapfn (list + -) '(2 1)) '(3 1))

(define (mapfn lof lon)
  (cond 
    [(empty? lof) empty]
    [else 
     (local
       [(define f (first lof))
        (define a (first lon))
        (define b (second lon))]
       (cons (f a b)
             (mapfn (rest lof) lon)))]))

;; Tests:
(check-expect (mapfn (list * / list) '(4 2)) '(8 2 (4 2)))
(check-expect (mapfn (list + - *) '(2 5)) '(7 -3 10))


;; b

;; helper

(define (all-pred-wrong? lof loo)
  (cond 
    [(empty? lof) true]
    [((first (first lof)) (first loo)) false]
    [else (all-pred-wrong? (rest lof) loo)]))

(define (all-true? f loo)
  (cond
    [(empty? (rest loo)) true]
    [(not (f (first loo) (second loo))) false]
    [else (all-true? f (rest loo))]))

(define (is-in-order-safe lof loo)
  (cond
    [(empty? lof) false]
    [(and ((first (first lof)) (first loo)) 
          (all-true? (second (first lof)) loo)) true]
    [else (is-in-order-safe (rest lof) loo)]))

;; (is-in-order? lof loo) consumes a list of predicate and
;;   relational operator and a list of operands and produces
;;   true if the operands if always in the order given, and
;;   false otherwise. If there is no such type of data,
;;   produces 'error
;; is-in-order?: (listof (list (Any -> Bool) (X X -> Bool)))
;;               (listof Any) -> (anyof Bool 'error)
;; requires: first list is non-empty
;; Exmaples:
(check-expect (is-in-order? (list (list integer? <)) 
                            '(1 2 7)) true)

(define (is-in-order? lof loo)
  (cond
    [(or (empty? loo) (empty? (rest loo))) true]
    [(all-pred-wrong? lof loo) 'error]
    [else (is-in-order-safe lof loo)]))

;; Tests:
(check-expect (is-in-order? (list (list integer? <))
                            empty) true)
(check-expect (is-in-order? (list (list integer? <))
                            (list 'a 'b 'c 'd)) 'error)
(check-expect (is-in-order? (list (list integer? >))
                            (list 7 2 1 0)) true)
(check-expect (is-in-order? (list (list integer? >))
                            (list 7 1 2 0)) false)
(check-expect (is-in-order? (list (list symbol? symbol=?)
  (list integer? =)) (list 1 1 1 1 1 1)) true)
(check-expect (is-in-order? (list (list symbol? symbol=?)
  (list integer? <)) (list 1 1 1 1 1 9)) false)















