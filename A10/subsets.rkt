;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname subsets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 10
;; Bonus Problem
;; ############

;; (a)

(define (subsets1 s)
  (foldr(lambda(i j)(append j (map(lambda(k)(cons i k))j)))(cons empty empty)s))


;; (b)

(define (subsets2 s)
  (foldr(lambda(i j)(append j (map(lambda(k)(cons i k))j)))(cons empty empty)s))