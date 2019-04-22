;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname rle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 5
;; Problem 2
;; ############

;; An RlePair is:
;; (list Any Nat)

;; An RleList is one of:
;; * empty
;; * (cons RlePair RleList)

;; Helper Function
(define (decode rlep)
  (cond
    [(<= (second rlep) 0) empty]
    [else
     (cons (first rlep) 
           (decode (list (first rlep) 
                         (- (second rlep) 1))))]))

;; (rle-decode rlel) consumes a run-length encoded list
;;   and produces the original list
;; rle-decode: RleList -> listof Any
;; Examples:
(check-expect (rle-decode (cons 
                           (cons 'red (cons 4 empty)) 
                           (cons (cons 'blue (cons 2 empty)) 
                                 empty)))
              (cons 'red (cons 'red 
                               (cons 'red (cons 'red 
                                                (cons 'blue (cons 'blue empty)))))))

(define (rle-decode rlel)
  (cond
    [(empty? rlel) empty]
    [else
     (append (decode (first rlel))
             (rle-decode (rest rlel)))]))

;; Tests:
(check-expect (rle-decode (list (list 1 2) (list 2 3))) (list 1 1 2 2 2))