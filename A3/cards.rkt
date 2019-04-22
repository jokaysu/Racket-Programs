;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cards) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 3
;; Problem 4
;; ############

(define-struct card (rank suit))
;; A Card is a (make-card Nat Sym)

(define-struct hand (c1 c2 c3))
;; A Hand is a (make-hand Card Card Card)

;; (better-card ca cb) consumes two cards, Card A and Card B and
;;   produces the better of the two.
;; better-card: Card Card -> Card
;; Examples:
(check-expect (better-card (make-card 2 'diamonds)
                           (make-card 1 'spades))
              (make-card 1 'spades))

(define (better-card ca cb)
  (cond 
    [(symbol=? (card-suit ca) (card-suit cb))
     (make-card (max (card-rank ca)
                     (card-rank cb))
                (card-suit ca))]
    [else 
     (cond
       [(symbol=? (card-suit ca) 'clubs) cb]
       [(symbol=? (card-suit ca) 'diamonds) 
        (cond
          [(symbol=? (card-suit cb) 'clubs) ca]
          [else cb])]
       [(symbol=? (card-suit ca) 'hearts) 
        (cond
          [(symbol=? (card-suit cb) 'spades) cb]
          [else ca])]
       [(symbol=? (card-suit ca) 'spades) ca])]))

;; Tests:
(check-expect (better-card (make-card 2 'hearts)
                           (make-card 1 'hearts))
              (make-card 2 'hearts))


;; (hand-value myhand) consumes a Hand and produces a symbol
;;   indicating the best hand-value of the given Hand.
;; hand-value: Hand -> Sym
;; Examples:
(check-expect (hand-value (make-hand (make-card 1 'clubs)
                                     (make-card 3 'clubs)
                                     (make-card 2 'clubs)))
              'straight-flush)

(define (hand-value myhand)
  (cond
    [(and
      (= (- (max (card-rank (hand-c1 myhand))
                 (card-rank (hand-c2 myhand))
                 (card-rank (hand-c3 myhand)))
            (min (card-rank (hand-c1 myhand))
                 (card-rank (hand-c2 myhand))
                 (card-rank (hand-c3 myhand))))
         2)
      (or
       (= (+ (min (card-rank (hand-c1 myhand))
                  (card-rank (hand-c2 myhand))
                  (card-rank (hand-c3 myhand)))
             1)
          (card-rank (hand-c1 myhand)))
       (= (+ (min (card-rank (hand-c1 myhand))
                  (card-rank (hand-c2 myhand))
                  (card-rank (hand-c3 myhand)))
             1)
          (card-rank (hand-c2 myhand)))
       (= (+ (min (card-rank (hand-c1 myhand))
                  (card-rank (hand-c2 myhand))
                  (card-rank (hand-c3 myhand)))
             1)
          (card-rank (hand-c3 myhand)))))
     (cond
       [(and
         (symbol=? (card-suit (hand-c1 myhand)) 
                   (card-suit (hand-c2 myhand)))
         (symbol=? (card-suit (hand-c1 myhand)) 
                   (card-suit (hand-c3 myhand))))
        'straight-flush]
       [else 'straight])]
    [(and
      (symbol=? (card-suit (hand-c1 myhand))
                (card-suit (hand-c2 myhand)))
      (symbol=? (card-suit (hand-c1 myhand))
                (card-suit (hand-c3 myhand))))
     'flush]
    [(and
      (= (card-rank (hand-c1 myhand))
         (card-rank (hand-c2 myhand)))
      (= (card-rank (hand-c1 myhand))
         (card-rank (hand-c3 myhand))))
     'three-of-a-kind]
    [(or
      (= (card-rank (hand-c1 myhand))
         (card-rank (hand-c2 myhand)))
      (= (card-rank (hand-c1 myhand))
         (card-rank (hand-c3 myhand)))
      (= (card-rank (hand-c2 myhand))
         (card-rank (hand-c3 myhand))))
     'pair]
    [else 'high-card]))

;; Tests: 
(check-expect (hand-value (make-hand (make-card 1 'clubs)
                                     (make-card 1 'spades)
                                     (make-card 1 'diamonds)))
              'three-of-a-kind)

(check-expect (hand-value (make-hand (make-card 1 'clubs)
                                     (make-card 3 'spades)
                                     (make-card 2 'clubs)))
              'straight)

(check-expect (hand-value (make-hand (make-card 5 'clubs)
                                     (make-card 3 'clubs)
                                     (make-card 2 'clubs)))
              'flush)
(check-expect (hand-value (make-hand (make-card 1 'clubs)
                                     (make-card 3 'diamonds)
                                     (make-card 1 'spades)))
              'pair)
(check-expect (hand-value (make-hand (make-card 1 'clubs)
                                     (make-card 3 'diamonds)
                                     (make-card 9 'spades)))
              'high-card)





