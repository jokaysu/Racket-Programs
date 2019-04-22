;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bst) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 8
;; Problem 2
;; ############


;; Given

(define-struct node (key val left right))


;; a

(define my-bst2
  (make-node 3 "3" 
             (make-node 1 "1" empty 
                        (make-node 2 "2" empty empty))
             (make-node 4 "4" empty empty)))

(define my-bst
  (make-node 5 "5"
             (make-node 3 "3"
                        (make-node 1 "1" empty 
                                   (make-node 2 "2" empty empty))
                        (make-node 4 "4" empty empty))
             (make-node 6 "6" empty empty)))

;; (root-at-smallest bst) consumes a BST and produces a 
;;   BST which is the smallest node as the root.
;; root-at-smallest: BST -> BST
;; Examples:
(check-expect (root-at-smallest my-bst2)
              (make-node 1 "1" empty 
                         (make-node 3 "3" 
                                    (make-node 2 "2" empty empty)
                                    (make-node 4 "4" empty empty))))

(define (root-at-smallest bst)
  (cond
    [(or (empty? bst) (empty? (node-left bst))) bst]
    [else (local [(define y bst) 
                  (define x (root-at-smallest (node-left bst)))]
            (make-node (node-key x) (node-val x) empty
                       (make-node (node-key y)
                                  (node-val y) 
                                  (node-right x)
                                  (node-right y))))]))

;; Tests:
(check-expect (root-at-smallest my-bst)
  (make-node 1 "1" empty
    (make-node 5 "5"
      (make-node 3 "3"
        (make-node 2 "2" empty empty)
        (make-node 4 "4" empty empty))
      (make-node 6 "6" empty empty))))


;; b

;; (bst-remove k bst) consumes a key and a BST and produces
;;   a BST that the node, which key is equal to k, is del.
;; bst-remove: Num BST -> BST
;; Examples:
(check-expect (bst-remove 2 my-bst2)
              (make-node 3 "3"
                         (make-node 1 "1" empty empty)
                         (make-node 4 "4" empty empty)))

(define (bst-remove k bst)
  (cond
    [(empty? bst) bst]
    [(< k (node-key bst)) (make-node (node-key bst) (node-val bst)
       (bst-remove k (node-left bst)) (node-right bst))]
    [(> k (node-key bst)) (make-node (node-key bst) (node-val bst)
       (node-left bst) (bst-remove k (node-right bst)))]
    [(empty? (node-left bst)) (node-right bst)]
    [(empty? (node-right bst)) (node-left bst)]
    [else (local [(define a (node-left bst))
                  (define x (root-at-smallest (node-right bst)))]
            (make-node (node-key x) (node-val x) a 
                       (node-right x)))]))

;; Tests:
(check-expect (bst-remove 3 my-bst)
  (make-node 5 "5" 
    (make-node 4 "4"
      (make-node 1 "1" empty
        (make-node 2 "2" empty empty))
      empty)
    (make-node 6 "6" empty empty)))

(check-expect (bst-remove 1 my-bst)
  (make-node 5 "5"
    (make-node 3 "3" 
      (make-node 2 "2" empty empty)
      (make-node 4 "4" empty empty))
    (make-node 6 "6" empty empty)))

