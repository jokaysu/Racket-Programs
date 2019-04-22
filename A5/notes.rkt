;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname notes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 5
;; Problem 1
;; ############


;; (a)

(define-struct note (letter modifier))
;; A Note is a (make-note Sym Sym)
;; *Letter is any of 'A ... 'G
;; *Modifier is any of 'sharp 'flat 'natural


;; (b)

;;(normalize-note nnote) consumes a Note and produces
;;  an integer between 1 and 12 representing the note's
;;  position.
;; normalize-note: Note -> Int
;; Exmaples:
(check-expect (normalize-note (make-note 'C 'natural)) 1)

(define (normalize-note nnote)
  (cond
    [(or
      (and
       (symbol=? (note-letter nnote)   'C)
       (symbol=? (note-modifier nnote) 'natural))
      (and
       (symbol=? (note-letter nnote)   'B)
       (symbol=? (note-modifier nnote) 'sharp)))
     1]
    [(or
      (and
       (symbol=? (note-letter nnote)   'C)
       (symbol=? (note-modifier nnote) 'sharp))
      (and
       (symbol=? (note-letter nnote)   'D)
       (symbol=? (note-modifier nnote) 'flat)))
     2]
    [(and
      (symbol=? (note-letter nnote)   'D)
      (symbol=? (note-modifier nnote) 'natural))
     3]
    [(or
      (and
       (symbol=? (note-letter nnote)   'D)
       (symbol=? (note-modifier nnote) 'sharp))
      (and
       (symbol=? (note-letter nnote)   'E)
       (symbol=? (note-modifier nnote) 'flat)))
     4]
    [(or
      (and
       (symbol=? (note-letter nnote)   'E)
       (symbol=? (note-modifier nnote) 'natural))
      (and
       (symbol=? (note-letter nnote)   'F)
       (symbol=? (note-modifier nnote) 'flat)))
     5]
    [(or
      (and
       (symbol=? (note-letter nnote)   'F)
       (symbol=? (note-modifier nnote) 'natural))
      (and
       (symbol=? (note-letter nnote)   'E)
       (symbol=? (note-modifier nnote) 'sharp)))
     6]
    [(or
      (and
       (symbol=? (note-letter nnote)   'F)
       (symbol=? (note-modifier nnote) 'sharp))
      (and
       (symbol=? (note-letter nnote)   'G)
       (symbol=? (note-modifier nnote) 'flat)))
     7]
    [(and
      (symbol=? (note-letter nnote)   'G)
      (symbol=? (note-modifier nnote) 'natural))
     8]
    [(or
      (and
       (symbol=? (note-letter nnote)   'G)
       (symbol=? (note-modifier nnote) 'sharp))
      (and
       (symbol=? (note-letter nnote)   'A)
       (symbol=? (note-modifier nnote) 'flat)))
     9]
    [(and
      (symbol=? (note-letter nnote)   'A)
      (symbol=? (note-modifier nnote) 'natural))
     10]
    [(or
      (and
       (symbol=? (note-letter nnote)   'A)
       (symbol=? (note-modifier nnote) 'sharp))
      (and
       (symbol=? (note-letter nnote)   'B)
       (symbol=? (note-modifier nnote) 'flat)))
     11]
    [(or
      (and
       (symbol=? (note-letter nnote)   'B)
       (symbol=? (note-modifier nnote) 'natural))
      (and
       (symbol=? (note-letter nnote)   'C)
       (symbol=? (note-modifier nnote) 'flat)))
     12]))

;; Tests:
(check-expect (normalize-note (make-note 'E 'sharp)) 6)
(check-expect (normalize-note (make-note 'G 'natural)) 8)


;; (c)

;; (normalize-note-list lon) consumes a list of Note sturctures,
;;   and produces a list of numbers that indicate the position
;;   of the list of Note
;; normalize-note-list: listof Note -> listof Int
;; Examples:
(check-expect (normalize-note-list 
               (cons (make-note 'D 'natural) 
                     (cons (make-note 'F 'sharp) empty)))
              (cons 3 (cons 7 empty)))

(define (normalize-note-list lon)
  (cond
    [(empty? lon) empty]
    [else
     (cons
      (normalize-note (first lon))
      (normalize-note-list (rest lon)))]))

;; Tests:
(check-expect (normalize-note-list 
               (cons (make-note 'D 'sharp) 
                     (cons (make-note 'F 'flat) empty)))
              (cons 4 (cons 5 empty)))


;; (d)

;; (interval notea noteb) consumes two Note structures
;;   and produces the number of increasing steps need
;;   to get from first to second note.
;; interval: Note Note -> Int
;; Examples:
(check-expect (interval (make-note 'G 'natural)
                        (make-note 'A 'sharp))
              3)

(define (interval notea noteb)
  (cond
    [(> (normalize-note notea) (normalize-note noteb))
     (- (+ (normalize-note noteb) 12)
        (normalize-note notea))]
    [else (- (normalize-note noteb)
             (normalize-note notea))]))

;; Tests:
(check-expect (interval (make-note 'B 'natural)
                        (make-note 'B 'sharp))
              1)


;; (e)

;; Helper Function

(define (nltil lon)
  (cond
    [(empty? (rest lon)) empty]
    [else
     (cons
      (interval (first lon) (first (rest lon)))
      (nltil (rest lon)))]))


;; (note-list-to-interval-list lon) consumes a list of
;;   Note structures and produces a list of the intevals
;;   between every adjacent pair of notes
;; note-list-to-interval-list: listof Note -> listof Int
;; Examples:
(check-expect (note-list-to-interval-list
               (cons (make-note 'C 'natural)
                     (cons (make-note 'D 'natural)
                           (cons (make-note 'E 'natural)
                                 empty))))
              (cons 2 (cons 2 (cons 8 empty))))

(define (note-list-to-interval-list lon)
  (nltil (append lon (cons (first lon) empty))))

;; Tests:
(check-expect (note-list-to-interval-list
               (cons (make-note 'C 'natural)
                     (cons (make-note 'D 'natural)
                           (cons (make-note 'G 'natural)
                                 empty))))
              (cons 2 (cons 5 (cons 5 empty))))























