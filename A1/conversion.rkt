;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname conversion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 1
;; Problem 2
;; ############

;; (m/s->mph m/s) consumes a speed in m/s and produces
;;   the same speed in mph
;; m/s->mph: Num -> Num
;; Examples:

;; Constants:
(define meters-in-mile 1609.344)
(define seconds-in-hour 3600)

(define (m/s->mph m/s)
  (/ (* seconds-in-hour m/s)
     meters-in-mile))

;; Tests:


;; (fpf->mph fpf) consumes a speed in fpf and produces
;;   the same speed in mph
;; fpf->mph: Num -> Num
;; Examples:

;; Constants:
(define meters-in-fathom 1.8288)
(define days-in-fortnight 14)
(define hours-in-day 24)
(define hours-in-fortnight (* days-in-fortnight hours-in-day))

(define (fpf->mph fpf)
  (/ (* meters-in-fathom fpf)
     (* meters-in-mile hours-in-fortnight)))

;; Tests:


;; (mph->S/nc mph) consumes a speed in mph and produces
;;   and the same speed in S/nc
;; mph->S/nc: Num -> Num
;; Examples:

;;Constants:
(define meters-in-smoot 1.7018)
(define seconds-in-nanocentury 3.15576)

(define (mph->S/nc mph)
  (/ (* seconds-in-nanocentury meters-in-mile)
     (* seconds-in-hour meters-in-smoot)))

;; Tests:
