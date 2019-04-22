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

;; A module for providing function difference

(provide difference)

;; (difference lst1 lst2) consumes two lists lst1 and lst2
;;   and produces a list that contains all the elements
;;   that are only in lst1 or only in lst2
;; difference: (listof Any) (listof Any) -> (listof Any)

;;;;;;IMPLEMENTATION;;;;;;

(define (difference lst1 lst2)
	(append (find-diff lst1 lst2)
					(find-diff lst2 lst1)))


;; (find-diff lsta lstb) consumes two lists and produces
;;  the list that contains all the elements only exist in
;;  lsta but not in lstb
;; find-diff: (listof Any) (listof Any) -> (listof Any)

(define (find-diff lsta lstb)
	(cond
		[(empty? lsta) empty]
		[(false? (member (first lsta) lstb))
			(cons (first lsta) (find-diff (rest lsta) lstb))]
		[else (find-diff (rest lsta) lstb)]))
