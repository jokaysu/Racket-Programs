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

(provide cur-temp course-desc total-parking-capacity)

;; (cur-temp) consumes nothing and produces the current
;;   temperature at UW (in Celsius)
;; cur-temp: -> Num

;; (course-desc subject catalog) consumes a string and 
;;  an integer and produces a string with the description
;;  of the course.
;; course-desc: Str Int -> Str

;; (total-parking-capacity) consumes nothing and produces
;;   the total parking capacity in UW
;; total-parking-capacity: -> Num


;;;;;;IMPLEMENTATION;;;;;;

(define (cur-temp)
	(get-temp (uw-api "/weather/current")))

;; (get-temp loapi) consumes a list of APIResult and 
;;   produces the temperature
;; get-temp: (listof APIResult) -> Num
(define (get-temp loapi)
	(cond
		[(empty? loapi) #f]
		[(string=? "temperature_current_c"
							 (first (first loapi)))
			(second (first loapi))]
		[else (get-temp (rest loapi))]))


(define (course-desc subject catalog)
	(get-desc (uw-api (string-append "/courses/"
		subject "/" (number->string catalog)))))

;; (get-desc loapi) consumes a list of APIResult and 
;;   produces the description
;; get-desc: (listof APIResult) -> Str
(define (get-desc loapi)
	(cond
		[(empty? loapi) #f]
		[(string=? "description"
							 (first (first loapi)))
			(second (first loapi))]
		[else (get-desc (rest loapi))]))


(define (total-parking-capacity)
	(get-capa (first (uw-api "/parking/watpark.json"))))

;; (get-capa loapi) consumes a list of APIResult and 
;;   produces the capacity
;; get-capa: (listof APIResult) -> Str
(define (get-capa loapi)
	(cond
		[(empty? loapi) #f]
    [(not (string? (first (first loapi))))
       (get-capa (rest loapi))]
		[(string=? "capacity"
							 (first (first loapi)))
			(second (first loapi))]
		[else (get-capa (rest loapi))]))


