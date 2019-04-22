;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pizza) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 2
;; Problem 4
;; ############

;; (cal-price size stop ptop) consumes the size of the pizza,
;;   the number of standard and premium toppings and produces
;;   the price of the pizza
;; pizza-price: Sym Num Num -> Num
;; Examples:
(check-expect (cal-price 'small 1 4) 13)
(check-expect (cal-price 'medium 2 2) 13)

(define (cal-price size stop ptop)
  (cond 
    [(symbol=? size 'small) (+ sp
                               (* stp stop)
                               (* ptp ptop))]
    [(symbol=? size 'medium) (+ mp
                                (* stp stop)
                                (* ptp ptop))]
    [(symbol=? size 'large) (+ lp
                               (* stp stop)
                               (* ptp ptop))]
    [else 1000000])) ;to report error

;; Tests:
(check-expect (cal-price 'large 5 1) 16)
(check-expect (cal-price 'medium 0 0) 8)


;; (pizza-price size stop ptop coup) consumes the size of the pizza,
;;   the number of standard and premium toppings, and the coupon
;;   code and produces the price of the pizza
;; pizza-price: Sym Num Num Sym -> Num
;; Examples:
(check-expect (pizza-price 'small 1 2 'half-off) 5)
(check-expect (pizza-price 'medium 2 4 'big-eater) 18)

;; Const:
(define sp 6)    ;small price
(define mp 8)    ;medium price
(define lp 9.5)  ;large price
(define stp 1)   ;stand topping price
(define ptp 1.5) ;premium topping price

(define (pizza-price size stop ptop coup)
  (cond
    [(symbol=? coup 'big-eater) 18]
    [(and (symbol=? coup 'solo)
          (symbol=? size 'small)
          (= stop 0)
          (= ptop 2))
     8]
    [(symbol=? coup 'supersize) (cal-price 'small stop ptop)]
    [(symbol=? coup 'half-off) (/ (cal-price size stop ptop) 2)]
    [else (cal-price size stop ptop)]))
;; Tests:
(check-expect (pizza-price 'large 2 2 'supersize) 11)
(check-expect (pizza-price 'small 0 2 'solo) 8)
(check-expect (pizza-price 'medium 0 2 'solo) 11)
(check-expect (pizza-price 'small 0 0 'none) 6)