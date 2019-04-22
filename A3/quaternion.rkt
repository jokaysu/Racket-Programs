;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname quaternion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 3
;; Problem 5
;; ############

(define-struct quaternion (cc ic jc kc))
;; A Quaternion is a (make-quaternion Num Num Num Num)

;; (quat-mult a b q1 q2) consumes two numbers, a and b, and
;;   two elements q1 and q2 and produces the multiplication
;;   of q1 and q2.
;; quat-mult: Num Num Quaternion Quaternion -> Quaternion
;; Examples:
(check-expect (quat-mult -1 -1 (make-quaternion 2 3 0 0)
                         (make-quaternion 5 6 0 0))
              (make-quaternion -8 27 0 0))

(define (quat-mult a b q1 q2)
  (make-quaternion
   (+ (* (quaternion-cc q1) (quaternion-cc q2))
      (* (quaternion-ic q1) (quaternion-ic q2) a)
      (* (quaternion-jc q1) (quaternion-jc q2) b)
      (* (quaternion-kc q1) (quaternion-kc q2) a b -1))
   (+ (* (quaternion-cc q1) (quaternion-ic q2))
      (* (quaternion-ic q1) (quaternion-cc q2))
      (* (quaternion-jc q1) (quaternion-kc q2) b -1)
      (* (quaternion-kc q1) (quaternion-jc q2) b))
   (+ (* (quaternion-cc q1) (quaternion-jc q2))
      (* (quaternion-jc q1) (quaternion-cc q2))
      (* (quaternion-ic q1) (quaternion-kc q2) a)
      (* (quaternion-kc q1) (quaternion-ic q2) a -1))
   (+ (* (quaternion-cc q1) (quaternion-kc q2))
      (* (quaternion-kc q1) (quaternion-cc q2))
      (* (quaternion-ic q1) (quaternion-jc q2))
      (* (quaternion-jc q1) (quaternion-ic q2) -1))))

;; Tests:
(check-expect (quat-mult 1 1 (make-quaternion 3 3 0 0)
                         (make-quaternion 2 2 0 0))
              (make-quaternion 12 12 0 0))