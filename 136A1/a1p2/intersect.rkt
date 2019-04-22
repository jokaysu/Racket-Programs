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

;; A module for providing function intersect

(provide intersect)

;; (intersect lst1 lst2) consumes two lists and produces
;;   a list that contains all the same elements in both
;;   lst1 and lst2
;; intersect: (listof Any) (listof Any) -> (listof Any)

;;;;;;IMPLEMENTATION;;;;;;

(define (intersect lst1 lst2)
	(cond
		[(empty? lst1) empty]
		[(false? (member (first lst1) lst2))
			(intersect (rest lst1) lst2)]
		[else 
			(cons (first lst1) (intersect (rest lst1) lst2))]))
