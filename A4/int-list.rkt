;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname int-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 4
;; Problem 2
;; ############

;;(elements-more-than loi x) consumes a (listof Int) and and 
;;   Int and produces a list of integers that are strictly
;;   greater than x.
;; elements-more-than: (listof Int) Int -> (listof Int)
;; Examples:
(check-expect (elements-more-than 
               (cons 1 (cons 2 (cons 3 empty))) 2) 
              (cons 3 empty))

(define (elements-more-than loi x)
  (cond
    [(empty? loi) empty]
    [else
     (cond
       [(> (first loi) x)
        (cons (first loi) (elements-more-than (rest loi) x))]
       [else 
        (elements-more-than (rest loi) x)])]))

;; Tests:
(check-expect (elements-more-than 
               (cons 5 (cons -1 (cons 3 empty))) 0) 
              (cons 5 (cons 3 empty)))


;;(arithmetic-sequence? loi) consumes a list of integers and
;;   produces true if the list is an arithmetic sequence and
;;   false otherwise.
;; arithmetic-sequence?: (listof Int) -> Bool
;; Examles:
(check-expect (arithmetic-sequence?
               (cons 1 (cons 2 (cons 3 empty)))) 
              true)

(define (arithmetic-sequence? loi)
  (cond
    [(or 
      (empty? loi);for 0
      (empty? (rest loi));for 1
      (empty? (rest (rest loi))));for 2
     true]
    [else
     (cond
       [(not (= (- (first loi)
                   (first (rest loi)))
                (- (first (rest loi))
                   (first (rest (rest loi))))))
        false]
       [else (arithmetic-sequence? (rest loi))])]))

;; Tests:
(check-expect (arithmetic-sequence?
               (cons 2 (cons 5 (cons 6 empty))))
              false)
(check-expect (arithmetic-sequence? empty) true)
(check-expect (arithmetic-sequence? (cons 1 empty)) true)


;;(digits->integer loi) consumes a list of integers and produces
;;   the integer, which is combined by the list decreasingly.
;; digits->integer: (listof Int) -> Int
;; Examples:
(check-expect (digits->integer 
               (cons 4 (cons 5 (cons 6 empty))))
              654)

(define (digits->integer loi)
  (cond
    [(empty? loi) 0]
    [(or
      (< (first loi) 0)
      (> (first loi) 9)
      (symbol? (digits->integer (rest loi))))
     'error]
    [else
     (+ (first loi)
        (* 10 (digits->integer (rest loi))))]))

;; Tests:
(check-expect (digits->integer 
               (cons 0 (cons 1 (cons 2 (cons 0 empty)))))
              210)
(check-expect (digits->integer 
               (cons 8 (cons 9 (cons -7 empty))))
              'error)








