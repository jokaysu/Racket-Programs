;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname student) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 6
;; Problem 2
;; ############

;; (a)

;;helper function
(define (cal-tall lon)
  (cond
    [(empty? lon) 0]
    [else (max (first lon) (cal-tall (rest lon)))]))


;; (tallest los lon) consumes a list of names and a list of
;;   heights and produces the name of the student who is the
;;   tallest
;; tallest: (listof Sym) (listof Num) -> Sym
;; Examples:
(check-expect (tallest (list 'A 'B) (list 170 180)) 'B)

(define (tallest los lon)
  (cond
    [(= (first lon) (cal-tall lon)) (first los)]
    [else (tallest (rest los) (rest lon))])) 

;; Tests:
(check-expect (tallest (list 'A 'B 'C 'D) (list 1 2 3 4)) 'D)


;; (b)

;;helper function
(define (cal-short lon short-sofar)
  (cond
    [(empty? lon) short-sofar]
    [(< (first lon) short-sofar)
     (cal-short (rest lon) (first lon))]
    [else
     (cal-short (rest lon) short-sofar)]))


;; (shortest los lon) consumes a list of names and a list of
;;   heights and produces the name of the shortest 
;; shortest: (listof Sym) (listof Num) -> Sym
;; Examples:
(check-expect (shortest (list 'A 'B) (list 170 180)) 'A)

(define (shortest los lon)
  (cond
    [(= (first lon) (cal-short lon (cal-tall lon))) (first los)]
    [else (shortest (rest los) (rest lon))]))

;; Tests:
(check-expect (shortest (list 'A 'B 'C 'D) (list 1 2 3 4)) 'A)


;; (c)

;; (student-al los lon) consumes a list of names and a list of
;;   heights and produces an association list with the names
;;   and the heights as an element to the new list
;; student-al: (listof Sym) (listof Num) -> (listof (Sym Num))
;; Examples:
(check-expect (student-al (list 'A 'B) (list 170 180))
              (list (list 'A 170) (list 'B 180)))

(define (student-al los lon)
  (cond
    [(empty? los) empty]
    [else
     (cons (list (first los) (first lon))
           (student-al (rest los) (rest lon)))]))

;; Tests:
(check-expect (student-al (list 'A 'B 'C 'D) (list 1 2 3 4))
              (list (list 'A 1)
                    (list 'B 2)
                    (list 'C 3)
                    (list 'D 4)))


;; (d)

;; (basketball al h) consumes an association list and a height
;;   and produces the list of names that are least as tall as
;;  as the given height
;; basketball: (listof (Sym Num)) Num -> (listof Sym)
;; Examples:
(check-expect (basketball (list (list 'A 160) (list 'B 170)) 165)
              (list 'B))

(define (basketball al h)
  (cond
    [(empty? al) empty]
    [(>= (second (first al)) h)
     (cons (first (first al)) (basketball (rest al) h))]
    [else 
     (basketball (rest al) h)]))

;; Tests:
(check-expect (basketball (list (list 'A 1) 
                                (list 'B 2) 
                                (list 'C 3)) 2)
              (list 'B 'C))
