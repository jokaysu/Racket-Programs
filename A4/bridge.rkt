;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bridge) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 4
;; Problem 3
;; ############

;; A BridgeHand is one of:
;; * empty
;; * (cons bhe BridgeHand), where bhe is a BridgeHandElement

;; A BridgeHandElement is one of:
;; * a Str
;; * a Sym, one of 'Ace, 'King, 'Queen, or 'Jack
;; * a Nat in the range 2 to 10 inclusive.

;; my-bridgehand-fn: listof BridgeHandElement -> Any

;;(define (my-bridgehand-fn BridgeHand)
;;	(cond
;;		[(empty? BridgeHand) ...]
;;		[else ... (first BridgeHand) ... 
;;			(my-bridgehand-fn (rest BridgeHand) ...)]))

;; my-bridgehandelement-fn: BridgeHandElement -> Any

(define (my-bridgehandelement-fn BridgeHandElement)
	(cond
		[(string? BridgeHandElement) ...]
		[(symbol? BridgeHandElement) ...]
		[(number? BridgeHandElement) ...]))


;; (count-points bh) consumes a BridgeHand and produces the 
;;   number of points that the BridgeHand is worth.
;; count-points: BridgeHand -> Int
;; Examples:
(check-expect (count-points (cons "Clubs Start" (cons 8 
	(cons 'King (cons 'Ace (cons "Clubs End" 
		(cons "Hearts Start" (cons 4 (cons "Hearts End"
			(cons "Diamonds Start" (cons 4 (cons 'Jack 
				(cons "Diamonds End" empty))))))))))))) 8)

(define (count-points bh)
	(cond
		[(empty? bh) 0]
		[else
			(cond
				[(symbol? (first bh)) 
					(+
						(cond
							[(symbol=? (first bh) 'Ace) 4]
							[(symbol=? (first bh) 'King) 3]
							[(symbol=? (first bh) 'Queen) 2]
							[(symbol=? (first bh) 'Jack) 1])
						(count-points (rest bh)))]
				[else (count-points (rest bh))])]))

;; Tests:
(check-expect (count-points (cons "Diamonds Start"
	(cons 'Queen (cons "Diamonds End" empty)))) 2)


