;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname alfs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 9
;; Problem 2
;; ############


;; a

;; (x-coords-of-posns loa) consumes a list of anything and
;;   produces the list of all of the x-coords of the Posns
;;   in this list.
;; x-coords-of-posns: (listof Any) -> (listof Any)
;; Examples:
(check-expect (x-coords-of-posns 
               (list 1 2 (make-posn 1 2)))
              (list 1))

(define (x-coords-of-posns loa)
  (map (lambda (po) (posn-x po))
       (filter posn? loa)))

;; Tests:
(check-expect (x-coords-of-posns 
               (list (make-posn 'a 'b) 3 4 5 (make-posn 6 7)))
              (list 'a 6))
(check-expect (x-coords-of-posns
               (list 1 2 (make-posn 5 6) (make-posn "abc" 'd)))
              (list 5 "abc"))


;;b

;; (alternating-sum lon) consumes a list of numbers and
;;   produces the alternating sum.
;; alternating-sum: (listof Num) -> Num
;; Examples:
(check-expect (alternating-sum 
               (list 1 2 3 4)) -2)

(define (alternating-sum lon)
  (local
    [(define (my-length lst)
       (foldr (lambda (x y) (+ y 1)) 0 lst))
     (define (alter lst)
       (cond
         [(even? (my-length lst))
          (foldr (lambda (x y)
                   (cond
                     [(even? (my-length y)) (cons (- x) y)]
                     [else (cons x y)])) empty lst)]
         [else
          (foldr (lambda (x y)
                   (cond
                     [(even? (my-length y)) (cons x y)]
                     [else (cons (- x) y)])) empty lst)]))]
    (foldr + 0 (alter lon))))

;; Tests:
(check-expect (alternating-sum 
               (list 1 1 1 1 1 1)) 0)
(check-expect (alternating-sum
               (list 5 4 3 2 1)) 3)


;; c

;; (remove-duplicates lon) consumes a list of numbers and
;;   produces the list with all but the first occurrence
;;   of every number removed
;; remove-duplicates: (listof Num) -> (listof Num)
;; Examples:
(check-expect (remove-duplicates
               (list 1 4 2 1 4 5 4))
              (list 1 4 2 5))

(define (remove-duplicates lon)
  (foldr 
   (lambda (x y)
     (cons x 
           (filter 
            (lambda (z)
              (not (= x z))) y))) empty lon))

;; Tests:
(check-expect (remove-duplicates
               (list 2 4 3 2 2 2 1 ))
              (list 2 4 3 1))
(check-expect (remove-duplicates
               (list 8 9 8 9 8 9 8 9 8 9))
              (list 8 9))


;; d

;; (first-col lolon) consumes a list of list of numbers and
;;   produces the first column of the matrix
;; first-col: (listof (listof Num)) -> (listof Num)
;; Examples:
(check-expect (first-col
               (list (list 1 2 3 4)
                     (list 5 6 7 8)
                     (list 9 10 11 12)))
              (list 1 5 9))

(define (first-col lolon)
  (map (lambda (x) (first x)) lolon))

;; Tests:
(check-expect (first-col
               (list (list 1 2)
                     (list 3 4)))
              (list 1 3))
(check-expect (first-col
               (list (list 5 9 9)
                     (list 2 9 9)
                     (list 0 9 9)))
              (list 5 2 0))


;; e

;; (add1-mat lolon) consumes a list of list of numbers as 
;;   a matrix and produces the matrix that adding 1 to 
;;   every entry of the matrix
;; add1-mat: (listof (listof Num)) -> (listof (listof Num))
;; Examples:
(check-expect (add1-mat
               (list (list 1 2)
                     (list 3 4)))
              (list (list 2 3)
                    (list 4 5)))

(define (add1-mat lolon)
  (map
   (lambda (x)
     (map
      (lambda (y)
        (+ y 1)) x)) lolon))

;; Tests:
(check-expect (add1-mat
               (list (list 0 0 0)
                     (list 0 0 0)))
              (list (list 1 1 1)
                    (list 1 1 1)))
(check-expect (add1-mat
               (list (list 2 1)
                     (list 2 3)))
              (list (list 3 2)
                    (list 3 4)))


;; f

;; (sum-at-zero lof) consumes a list of functions and
;;   produces the sum of each function applying zero.
;; sum-at-zero: (listof (Num -> Num)) -> Num
;; Exmaples:
(check-expect (sum-at-zero 
               (list add1 sqr add1)) 2)

(define (sum-at-zero lof)
  (foldr + 0 
         (map (lambda (x)
                (x 0)) lof)))

;; Tests:
(check-expect (sum-at-zero
               (list add1 add1 add1)) 3)
(check-expect (sum-at-zero
               (list sqr abs add1)) 1)





























