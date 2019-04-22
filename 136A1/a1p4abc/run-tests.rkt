#lang racket
(require "uw-tools-black.rkt")
(require "uw-api.rkt")

;; Set this file as the main file to run, then click TEST.

;; Add more test cases here and their results in 
;;  Basic.expect

;; API examples
(list? (uw-api "/weather/current"))
(list? (uw-api "/events/holidays"))
(list? (uw-api "/courses/CS/136"))
(list? (uw-api "/terms/1161/CS/136/schedule"))
(list? (uw-api "/foodservices/products/2189"))
(list? (uw-api "/foodservices/2014/3/menu"))
(list? (uw-api "/parking/watpark.json"))

;; Some basic tests to ensure the produced types
(number? (cur-temp))
(string? (course-desc "CS" 136))
(integer? (total-parking-capacity))

;; Add your own tests below:
(string? (course-desc "MATH" 239))
(string? (course-desc "CS" 350))
