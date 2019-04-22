#lang racket
(require "range-missing.rkt")

;; Set this file as the main file to run, then click TEST.

;; Add more test cases here and their results in 
;;  Basic.expect

(range-missing '(1 2 4) 2 5)
(range-missing '(1 2 3) 1 2)
(range-missing '(2 5 6 8) 2 7)