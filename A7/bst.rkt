;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname bst) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 7
;; Problem 1
;; ############

(require "a7.rkt")


;; a

(define my-bst
  (make-node 10 "10" 
     (make-node 6 "6" 
        (make-node 4 "4" 
           (make-node 3 "3" empty empty)
           (make-node 5 "5" empty empty))
  (make-node 8 "8"
     (make-node 7 "7" empty empty)
         (make-node 9 "9" empty empty)))
  (make-node 14 "14"
     (make-node 12 "12"
         (make-node 11 "11" empty empty)
         (make-node 13 "13" empty empty))
      (make-node 16 "16"
         (make-node 15 "15" empty empty) empty))))


;; (bst-internal-count bst) consumes a BST and produces the
;;   total number of internal nodes in the BST
;; bst-internal-count: BST -> Num
;; Examples:
(check-expect (bst-internal-count 
               (make-node 1 "23" empty empty)) 0)

(define (bst-internal-count bst)
  (cond
    [(empty? bst) 0]
    [(and (empty? (node-left bst))
          (empty? (node-right bst))) 0]
    [else 
     (+ (bst-internal-count (node-left bst))
        (bst-internal-count (node-right bst))
        1)]))

;; Tests:
(check-expect (bst-internal-count my-bst) 7)


;; b

;; (bst-bounded? bst lower upper) consumes a bst and two
;;   ints that are lower and upper bounds for a range, 
;;   and produces true if all keys in the bst are in the 
;;   range and false otherwise
;; bst-bounded?: BST Int Int -> Bool
;; requires: lower <= upper
;; Examples:
(check-expect (bst-bounded? 
   (make-node 2 "2" 
      (make-node 1 "1" empty empty)
      (make-node 3 "3" empty empty)) 1 3) true)

(define (bst-bounded? bst lower upper)
  (cond
    [(empty? bst) true]
    [(< (node-key bst) lower) false]
    [(> (node-key bst) upper) false]
    [else (and (bst-bounded? (node-left bst) lower upper)
               (bst-bounded? (node-right bst) lower upper))]))

;; Tests:
(check-expect (bst-bounded? my-bst 4 16) false)
(check-expect (bst-bounded? my-bst 3 16) true)


;; c

;; (bst-add bst k v) consumes a bst and key and a value
;;   and produces a bst with the new value if the key
;;   exists, and a bst with the new node otherwise
;; bst-add: BST Num Str -> BST
;; Examples:
(check-expect (bst-add
               (make-node 2 "2" 
                          (make-node 1 "1" empty empty)
                          (make-node 3 "3" empty empty)) 1 "3") 
              (make-node 2 "2" 
                         (make-node 1 "3" empty empty)
                         (make-node 3 "3" empty empty)))

(define (bst-add bst k v)
  (cond
    [(empty? bst) (make-node k v empty empty)]
    [(= k (node-key bst)) 
     (make-node k v (node-left bst) (node-right bst))]
    [(< k (node-key bst))
     (make-node (node-key bst) (node-val bst)
                (bst-add (node-left bst) k v) (node-right bst))]
    [(> k (node-key bst))
     (make-node (node-key bst) (node-val bst)
                (node-left bst) (bst-add (node-right bst) k v))]
    [else "error"])); for safety

;; Tests:
(check-expect (bst-add
               (make-node 2 "2" 
                 (make-node 1 "1" empty empty)
                 (make-node 3 "3" empty empty)) 0 "3") 
              (make-node 2 "2" 
                (make-node 1 "1" (make-node 0 "3" empty empty) empty)
                (make-node 3 "3" empty empty)))

(check-expect (bst-add my-bst 17 "17")
  (make-node 10 "10" 
    (make-node 6 "6" 
      (make-node 4 "4" 
         (make-node 3 "3" empty empty)
         (make-node 5 "5" empty empty))
  (make-node 8 "8"
    (make-node 7 "7" empty empty)
    (make-node 9 "9" empty empty)))
  (make-node 14 "14"
    (make-node 12 "12"
    (make-node 11 "11" empty empty)
    (make-node 13 "13" empty empty))
  (make-node 16 "16"
    (make-node 15 "15" empty empty) 
    (make-node 17 "17" empty empty)))))

(check-expect (bst-add my-bst 12 "10000") 
  (make-node 10 "10" 
    (make-node 6 "6" 
        (make-node 4 "4" 
          (make-node 3 "3" empty empty)
          (make-node 5 "5" empty empty))
        (make-node 8 "8"
          (make-node 7 "7" empty empty)
          (make-node 9 "9" empty empty)))
    (make-node 14 "14"
         (make-node 12 "10000"
            (make-node 11 "11" empty empty)
            (make-node 13 "13" empty empty))
         (make-node 16 "16"
            (make-node 15 "15" empty empty) empty))))


;; d

(define my-bst2
  (make-node 2 "2" 
             (make-node 1 "1" empty empty)
             (make-node 3 "3" empty empty)) )


;; helper
;; (combine x y) cosumes two lists and produces
;;   a combining list
;; combine: (listof (list Num Str))
;;          (listof (list Num Str))
;;       -> (listof (list Num Str))
(define (combine x y)
  (cond
    [(empty? x) y]
    [else (cons (first x) (combine (rest x) y))]))


;; (bst->al bst) consumes a bst and produces an AL that
;;   contains all entries in the bst with ascending order
;; bst->al: BST -> (listof (list Num Str))
;; Examples:
(check-expect (bst->al my-bst2)
              (list (list 1 "1") (list 2 "2") (list 3 "3")))

(define (bst->al bst)
  (cond
    [(and (empty? (node-left bst))
          (empty? (node-right bst)))
     (cons (list (node-key bst) (node-val bst)) empty)]
    [(empty? (node-left bst)) 
     (combine (list (list (node-key bst) (node-val bst)))
              (bst->al (node-right bst)))]
    [(empty? (node-right bst))
     (combine (bst->al (node-left bst))
              (list (list (node-key bst) (node-val bst))))]
    [else 
     (combine (combine (bst->al (node-left bst))
                       (list (list (node-key bst) (node-val bst))))
              (bst->al (node-right bst)))]))

;; Tests:
(check-expect (bst->al 
           (make-node 3 "3" 
                      (make-node 2 "2"
                                 (make-node 1 "1" empty empty) empty)
                      (make-node 4 "4" empty empty)))
          (list (list 1 "1") (list 2 "2") (list 3 "3") (list 4 "4")))


;; e


;;helper
(define (combine2 x y)
  (cond
    [(empty? x) y]
    [(member? (first x) y) (combine2 (rest x) y)]
    [else (cons (first x) (combine2 (rest x) y))]))


;; (bst-value-list bst) consumes a BST and produces a (listof Str)
;;   that is decreasing order of the keys and without any duplicate
;; bst-value-list: BST -> (listof Str)
;; Examples:
(check-expect (bst-value-list my-bst2)
              (list "3" "2" "1"))

(define (bst-value-list bst)
  (cond
    [(and (empty? (node-left bst))
          (empty? (node-right bst)))
     (list (node-val bst))]
    [(empty? (node-left bst)) 
     (combine2 (bst-value-list (node-right bst))	
               (list (node-val bst)))]
    [(empty? (node-right bst))
     (combine2 (list (node-val bst))
               (bst-value-list (node-left bst)))]
    [else 
     (combine2 (combine2 (bst-value-list (node-right bst))
                         (list (node-val bst)))
               (bst-value-list (node-left bst)))]))

;; Tests:
(check-expect (bst-value-list
             (make-node 3 "3" 
                      (make-node 2 "2"
                                 (make-node 1 "1" empty empty) empty)
                      (make-node 4 "4" empty empty)))
          (list "4" "3" "2" "1"))

(check-expect (bst-value-list
           (make-node 3 "3" 
                      (make-node 2 "2"
                                (make-node 1 "4" empty empty) empty)
                      (make-node 4 "4" empty empty)))
          (list "3" "2" "4"))

