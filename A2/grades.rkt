;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 2
;; Problem 2
;; ############

;; (cal-final mid par ass fin) concumes the midterm, participation,
;;   assignments and final exam grade and produces the final grade
;;   of CS 135 without the situation of failing any parts of the
;;   grades.
;; cal-final: Num Num Num Num -> Num
;;Examples:
(check-expect (cal-final 100 100 100 100) 100)
(check-expect (cal-final 0 0 0 0) 0)

;;Constants:
(define mid% 25)
(define par% 5)
(define ass% 20)
(define fin% 50)

(define (cal-final mid par ass fin)
  (/ (+ (* mid mid%)
        (* par par%)
        (* ass ass%)
        (* fin fin%))
     100))

;;Tests:
(check-expect (cs135-final-grade 100 100 100 50) 75)
(check-expect (cs135-final-grade 80 100 100 100) 95)


;; (cs135-final-grade mid par ass fin) concumes the midterm grade,
;;   the participation grade, the assignments grade and the final
;;   exam grade and produces the final grade of CS135.
;; cs135-final-grade: Num Num Num Num -> Num
;;Examples:
(check-expect (cs135-final-grade 100 100 100 100) 100)
(check-expect (cs135-final-grade 0 0 0 0) 0)

(define (cs135-final-grade mid par ass fin)
  (cond
    [(or (< mid 50)
         (< ass 50)
         (< fin 50))
     (min (cal-final mid par ass fin) 46)]
    [else (cal-final mid par ass fin)]))

;;Test:
(check-expect (cs135-final-grade 100 100 100 50) 75)
(check-expect (cs135-final-grade 100 100 100 49) 46)