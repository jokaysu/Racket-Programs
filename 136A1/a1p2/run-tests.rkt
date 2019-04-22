#lang racket
(require "intersect.rkt")

;; Set this file as the main file to run, then click TEST.

;; Add more test cases here and their results in 
;;  Basic.expect
(intersect '(1 2 3) '(2 3))
(intersect '(3 5 6) '(9 9 6 5))
(intersect '(9 8 7 6) '(1 2 3 4 5 6 7 8))
