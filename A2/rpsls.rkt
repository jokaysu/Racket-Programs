;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rpsls) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 2
;; Problem 3
;; ############

;; (rpsls player1 player2) consumes two symbols the players choose
;;   and produces the result of RPSLS
;; (rpsls player1 player2): Sym Sym -> Sym
;; Examples:
(check-expect (rpsls 'scissors 'lizard) 'player1)
(check-expect (rpsls 'spock 'spock) 'tie)

(define (rpsls player1 player2)
  (cond
    [(symbol=? player1 'rock)
     (cond
       [(symbol=? player2 'rock)     'tie]
       [(symbol=? player2 'paper)    'player2]
       [(symbol=? player2 'scissors) 'player1]
       [(symbol=? player2 'lizard)   'player1]
       [(symbol=? player2 'spock)    'player2]
       [else 'wrongtype])]
    [(symbol=? player1 'paper)
     (cond
       [(symbol=? player2 'rock)     'player1]
       [(symbol=? player2 'paper)    'tie]
       [(symbol=? player2 'scissors) 'player2]
       [(symbol=? player2 'lizard)   'player2]
       [(symbol=? player2 'spock)    'player1]
       [else 'wrongtype])]
    [(symbol=? player1 'scissors)
     (cond
       [(symbol=? player2 'rock)     'player2]
       [(symbol=? player2 'paper)    'player1]
       [(symbol=? player2 'scissors) 'tie]
       [(symbol=? player2 'lizard)   'player1]
       [(symbol=? player2 'spock)    'player2]
       [else 'wrongtype])]
    [(symbol=? player1 'lizard)
     (cond
       [(symbol=? player2 'rock)     'player2]
       [(symbol=? player2 'paper)    'player1]
       [(symbol=? player2 'scissors) 'player2]
       [(symbol=? player2 'lizard)   'tie]
       [(symbol=? player2 'spock)    'player1]
       [else 'wrongtype])]
    [(symbol=? player1 'spock)
     (cond
       [(symbol=? player2 'rock)     'player1]
       [(symbol=? player2 'paper)    'player2]
       [(symbol=? player2 'scissrors) 'player1]
       [(symbol=? player2 'lizard)   'player2]
       [(symbol=? player2 'spock)    'tie]
       [else 'wrongtype])]
   [else 'wrongtype])) ;for safe

;;Tests:
(check-expect (rpsls 'paper 'lizard) 'player2)
(check-expect (rpsls 'lizard 'spock) 'player1)
(check-expect (rpsls 'rock 'scissors) 'player1)