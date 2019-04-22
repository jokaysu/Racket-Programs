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

(require "uw-api.rkt")

;; A module for providing more user friendly functions
;;   of using UW API

(provide parking-availability course-sections course-capacity)

;; (parking-availability lot) consumes the lot name and 
;;   produces the currently available parking spots
;; parking-availability: Str -> Num

;; (course-sections term subject catalog) consumes the term
;;   the subject and catalog, and produces the section names
;;   for the course
;; course-sections: Num Str Num -> (listof Str)

;; (course-capacity term subject catalog) consumes the term, 
;;   the subject and the catalog of a course and produces
;;   a list of lists that tell how full the course is
;; corse-capacity: Num Str Num -> (listof (listof Str Num Num))


;;;;;;IMPLEMENTATION;;;;;;

(define (get key lst)
	(cond
		[(empty? lst) #f]
		[(string=? key (first (first lst)))
			(second (first lst))]
		[else (get key (rest lst))]))

(define (parking-availability lot)
	(local
		[(define apir (uw-api "/parking/watpark.json"))
		 (define (find-capa lot lor)
		 		(cond
		 			[(string=? lot (get "lot_name" (first lor)))
		 				(- (get "capacity" (first lor))
		 					 (get "current_count" (first lor)))]
		 			[else (find-capa lot (rest lor))]))]
		 (find-capa lot apir)))

(define (course-sections term subject catalog)
	(local
		[(define apir (uw-api (string-append "/terms/"
				(number->string term) "/" subject "/"
				(number->string catalog) "/schedule.json")))
     (define (find-secs lor) 
     	(cond
     		[(empty? lor) empty]
     		[else (cons (get "section" (first lor))
     			(find-secs (rest lor)))]))]
		 (find-secs apir)))

(define (course-capacity term subject catalog)
	(local
		[(define apir (uw-api (string-append "/terms/"
				(number->string term) "/" subject "/"
				(number->string catalog) "/schedule.json")))
		 (define (find-full lor)
		 	(cond
		 		[(empty? lor) empty]
		 		[(string=? "LEC" (substring (get "section" (first lor)) 0 3))
		 			(cons (list (get "section" (first lor))
		 									(get "enrollment_capacity" (first lor))
		 									(get "enrollment_total" (first lor)))
		 						(find-full (rest lor)))]
		 		[else (find-full (rest lor))]))]
		 (find-full apir)))