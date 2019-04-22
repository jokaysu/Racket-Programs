;;(counts loi) consumes a list of intergres and produces the
;;   number of elements in this list.
;; counts: (listof Int) -> Int
;; Examples:
(check-expect (counts
             	(cons 1 (cons 2 (cons 3 empty)))) 
              3)

(define (counts loi)
	(cond
		[(empty? loi) 0]
		[else (+ 1 (counts (rest loi)))]))

;; Tests:
(check-expect (counts
             	(cons 1 (cons 2 empty))) 
              2)