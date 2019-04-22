#lang racket
(require "difference.rkt")

;; Set this file as the main file to run, then click TEST.

;; Add more test cases here and their results in 
;;  Basic.expect
(difference '(1 2 3) '(2 3))
(difference '(2 4 5) '(2 4 6))
(difference '(5 6) '(4 5 6))
