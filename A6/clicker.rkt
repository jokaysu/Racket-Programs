;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname clicker) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 6
;; Problem 1
;; ############

;;helper function
(define (cs135-participation a b c) 
  (* (/ (+ (* 2 (min (* (/ 3 4) a) b))
           (min c (max 0 (- (* (/ 3 4) a) b))))
        (* 2 (/ 3 4) a)) 100))  
(define (check-answer los loc)
  (cond
    [(empty? los) (list 0 0 0)]
    [(symbol=? (first los) 'none) 
     (list (+ 1 (first (check-answer (rest los) (rest loc))))
           (second (check-answer (rest los) (rest loc)))
           (third (check-answer (rest los) (rest loc))))]
    [(symbol=? (first los) (first loc))
              (list (+ 1 (first (check-answer (rest los) (rest loc))))
                    (+ 1(second (check-answer (rest los) (rest loc))))
                    (third (check-answer (rest los) (rest loc))))]
    [else 
     (list (+ 1 (first (check-answer (rest los) (rest loc))))
           (second (check-answer (rest los) (rest loc)))
           (+ 1 (third (check-answer (rest los) (rest loc)))))]))

;; (clicker-grade los loc) consumes a list of student clicker
;;   clicker responses and a list of correct answers and 
;;   produces the participation grade.
;; clicker-grade: (listof (anyof 'A 'B 'C 'D 'E 'none))
;;		      (listof (anyof 'A 'B 'C 'D 'E 'none))
;;              -> Num
;;Examples:
(check-expect (clicker-grade (list 'A) (list 'A)) 100)

(define (clicker-grade los loc)
  (cs135-participation 
   (first (check-answer los loc))
   (second (check-answer los loc))
   (third (check-answer los loc))))

;;Tests:
(check-expect (clicker-grade (list 'none) (list 'B)) 0)
(check-expect (clicker-grade (list 'B 'C) (list 'C 'D)) 50)