;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTEGRITY STATEMENT (modify if neccessary):
;;   I received help from the following sources:
;;     None. 
;;   I am the sole author of this work .
;; Sign this statement by removing the line below and entering your name
;;   Name: Chenzheng Su
;;   login ID: c28su

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To test the code by yourself, 
;;  set run-test.rkt as the main file, then click TEST
;; You may wish to add more tests before submitting the question.

;; WRITE YOUR CODE BELOW

#lang racket 

;; A module for providing function range-missing

(provide range-missing)

;; (range-missing lon a b) consumes a list of integers and
;;   and two integer parameters a and b, and produces a list
;;   of all integers from a to b that do NOT appear in lon.
;; range-missing: (listof Int) Int Int -> (listof Int)
;; requires: a <= b

;;;;;;IMPLEMENTATION;;;;;;

(define (range-missing lon a b)
	(cond
		[(> a b) empty]
		[(not (false? (member a lon)))
			(range-missing lon (add1 a) b)]
		[else 
			(cons a (range-missing lon (add1 a) b))]))