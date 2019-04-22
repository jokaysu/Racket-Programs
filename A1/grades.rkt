;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 1
;; Problem 3
;; ############

;; (cs135-grade-sofar midterm participation assignment) produces
;;   the grade students get before final exam in CS 135
;; cs135-grade-sofar: Num Num Num -> Num
;; Examples:
(check-expect (cs135-grade-sofar 100 100 100) 100)

;; Contants:
(define midterm% 25)
(define participation% 5)
(define assignment% 20)

(define (cs135-grade-sofar midterm participation assignment)
  (/ (+ (* midterm% (/ midterm 100))
        (* participation% (/ participation 100))
        (* assignment% (/ assignment 100)))
     (/ (+ midterm% participation% assignment%) 100)))

;; Tests:
(check-expect (cs135-grade-sofar 100 0 100) 90)


;; (cs135-final-exam grade-sofar final-exam-grade) produces
;;   the grade students get after the final exam in CS 135
;; cs135-final-exam: Num Num -> Num
;; Examples:
(check-expect (cs135-final-exam 100 100) 100)

(define (cs135-final-exam grade-sofar final-exam-grade)
  (+ (/ grade-sofar 2)
     (/ final-exam-grade 2)))

;; Tests:
(check-expect (cs135-final-exam 90 100) 95)