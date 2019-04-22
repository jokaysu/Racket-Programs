#lang racket
(require "uw-tools-gold.rkt")
(require "uw-api.rkt")
        
;; Set this file as the main file to run, then click TEST.
 
;; Add more test cases here and their results in 
;;  Basic.expect

;; Some minimal tests to ensure the produced types
(integer? (parking-availability "X"))
(list? (course-sections 1161 "CS" 136))
(list? (course-capacity 1161 "CS" 136))
(course-sections 1161 "CS" 135)
(course-capacity 1165 "CS" 350)
