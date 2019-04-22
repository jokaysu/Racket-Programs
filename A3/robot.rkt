;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 3
;; Problem 2
;; ############

;; (robot pos face turn dist) consumes the initial robot position,
;;   the direction the robot is faceing, the turning action and the
;;   the distance to move and produces the new position after
;;   turning and moving.
;; robot: Posn Sym Sym Num -> Posn
;; Examples:
(check-expect (robot (make-posn 0 0) 'north 'left 1) 
              (make-posn -1 0))
(check-expect (robot (make-posn 1 1) 'east 'noturn 10)
              (make-posn 11 1))

(define (robot pos face turn dist)
  (make-posn
   (cond
     [(or (and (symbol=? face 'east) (symbol=? turn 'noturn))
          (and (symbol=? face 'north) (symbol=? turn 'right))
          (and (symbol=? face 'south) (symbol=? turn 'left)))
      (+ (posn-x pos) dist)]
     [(or (and (symbol=? face 'west) (symbol=? turn 'noturn))
          (and (symbol=? face 'south) (symbol=? turn 'right))
          (and (symbol=? face 'north) (symbol=? turn 'left)))
      (- (posn-x pos) dist)]
     [else (posn-x pos)])
   (cond
     [(or (and (symbol=? face 'north) (symbol=? turn 'noturn))
          (and (symbol=? face 'west) (symbol=? turn 'right))
          (and (symbol=? face 'east) (symbol=? turn 'left)))
      (+ (posn-y pos) dist)]
     [(or (and (symbol=? face 'south) (symbol=? turn 'noturn))
          (and (symbol=? face 'east) (symbol=? turn 'right))
          (and (symbol=? face 'west) (symbol=? turn 'left)))
      (- (posn-y pos) dist)]
     [else (posn-y pos)])))

;; Tests:
(check-expect (robot (make-posn 0 0) 'west 'right 5)
              (make-posn 0 5))
(check-expect (robot (make-posn 1 1) 'south 'noturn 8)
              (make-posn 1 -7))