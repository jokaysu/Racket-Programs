;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname area) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 3
;; Problem 3
;; ############

(define-struct 3dposn (x y z))
;; A 3dPosn is a (make-3dposn Num Num Num)


;; (dis po1 po2) consumes two points and produces a number of
;;   distance between point1 and point2
;; dis: 3dPosn 3dPosn -> Num

(define (dis po1 po2)
  (sqrt (+ (sqr (- (3dposn-x po1) (3dposn-x po2)))
           (sqr (- (3dposn-y po1) (3dposn-y po2)))
           (sqr (- (3dposn-z po1) (3dposn-z po2))))))


;; (area s1 s2 s3) consumes three sides and produces the area
;;   of the triangle formed by these three sides.
;; area: Num Num Num -> Num

(define (area s1 s2 s3)
  (* (/ 1 4)
     (sqrt (- (+ (* 2 s2 s2 s3 s3)
                 (* 2 s3 s3 s1 s1)
                 (* 2 s1 s1 s2 s2))
              (+ (sqr (sqr s1))
                 (sqr (sqr s2))
                 (sqr (sqr s3)))))))


;; (area-triangle p1 p2 p3) concumes three 3dPosn and produces
;;   the area of the triangle formed by these three points.
;; area-triangle: 3dPosn 3dPosn 3dPosn -> Num
;; Examples:
(check-within (area-triangle (make-3dposn 0 0 0)
                             (make-3dposn 1 0 0)
                             (make-3dposn 0 1 0))
              0.5 0.001)

(define (area-triangle p1 p2 p3)
  (cond
    [(= (area (dis p1 p2)
              (dis p2 p3)
              (dis p3 p1))
        0) 'undefined]
    [else (area (dis p1 p2)
                (dis p2 p3)
                (dis p3 p1))]))

;; Tests:
(check-expect (area-triangle (make-3dposn 0 0 0)
                             (make-3dposn 1 0 0)
                             (make-3dposn 2 0 0)) 
              'undefined)

