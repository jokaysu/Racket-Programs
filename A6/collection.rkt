;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname collection) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 6
;; Problem 3
;; ############


(define-struct magazine (title issue))
  ;; A Magazine is a (make-magazine Str Nat)
  
  ;; An Index is one of:
  ;; * empty
  ;; * (cons (list Str (listof Nat)) Index)
  
  
  ;; (a)
  
  ;; (magazine<? ma1 ma2) consumes two Magazines and produces true
  ;;   if the first Magazine is lexicographically strictly less
  ;;   than the second Magazine
  ;; magazine<?: Magazine Magazine -> Bool
  ;; Examples:
  (check-expect (magazine<? (make-magazine "Dragon" 27)
                            (make-magazine "Dungeon" 6))
                true)
  
  (define (magazine<? ma1 ma2)
    (cond
      [(string<? (magazine-title ma1) (magazine-title ma2))
       true]
      [(and (string=? (magazine-title ma1) (magazine-title ma2))
            (< (magazine-issue ma1) (magazine-issue ma2)))
       true]
      [else false]))
  
  ;; Tests:
  (check-expect (magazine<? (make-magazine "Dragon" 22)
                            (make-magazine "Dragon" 27))
                true)
  (check-expect (magazine<? (make-magazine "Dragon" 27)
                            (make-magazine "Dragon" 22))
                false)
  
  
  ;; (b)
  
  ;;helper function
  
  (define (insert-ma ma lom)
    (cond
      [(empty? lom) (cons ma empty)]
      [(magazine<? ma (first lom)) (cons ma lom)]
      [else (cons (first lom) (insert-ma ma (rest lom)))]))
  
  
  ;; (sort-magazines lom) consumes a list of magazine and produces
  ;;   a sorted list of magazine
  ;; sor-magazines: (listof Magazines) -> (listof Magazine)
  ;; Examples:
  (check-expect (sort-magazines (list (make-magazine "A" 2)
                                      (make-magazine "A" 1)))
                (list (make-magazine "A" 1)
                      (make-magazine "A" 2)))
  
  (define (sort-magazines lom)
    (cond
      [(empty? lom) empty]
      [else
       (insert-ma (first lom) (sort-magazines (rest lom)))]))
  
  ;; Tests:
  (check-expect (sort-magazines (list (make-magazine "A" 2)
                                      (make-magazine "B" 1)))
                (list (make-magazine "A" 2)
                      (make-magazine "B" 1)))
  
  
  ;; (c)
  
  ;; helper function
  (define (exist? ma lom)
    (cond
      [(empty? lom) false]
      [(and (string=? (magazine-title ma)
                      (magazine-title (first lom)))
            (= (magazine-issue ma)
               (magazine-issue (first lom))))
       true]
      [else (exist? ma (rest lom))]))
  
  
  ;; (need-between lom tit low high) consumes a sorted list of magazine
  ;;   and a title and a low and high bound and produces a sorted list
  ;;   of issues that are missing in the list of magazine with the title
  ;; need-between: (listof Magazine) Str Nat Nat -> (listof Nat)
  ;; Examples:
  (define my-slom (list (make-magazine "Dragon" 2) 
                        (make-magazine "Dragon" 3)))
  (check-expect (need-between my-slom "Dragon" 2 3) empty)
  
  (define (need-between lom tit low high)
    (cond
      [(> low high) empty]
      [(exist? (make-magazine tit low) lom) 
       (need-between lom tit (+ low 1) high)]
      [else (cons low (need-between lom tit (+ low 1) high))]))
  
  ;; Tests:
  (check-expect (need-between my-slom "Dungeon" 2 3) (list 2 3))
  
  
  ;; (d)
  
  ;; (magazine-lists-equal? lom1 lom2) consumes two sorted list of
  ;;   magazines and produces true if the two lists are the same
  ;; magazine-lists-equal?: (listof Magazine) (listof Magazine) -> Bool
  ;; Examples:
  (check-expect (magazine-lists-equal? my-slom my-slom) true)
  
  (define (magazine-lists-equal? lom1 lom2)
    (cond
      [(empty? lom1) true]
      [(and (string=? (magazine-title (first lom1))
                      (magazine-title (first lom2)))
            (= (magazine-issue (first lom1))
               (magazine-issue (first lom2))))
       (magazine-lists-equal? (rest lom1) (rest lom2))]
      [else false]))
  
  ;; Tests:
  (define my-slom2 (list (make-magazine "Dragon" 2) 
                         (make-magazine "Dragon" 4)))
  (check-expect (magazine-lists-equal? my-slom my-slom2) false)
  
  
  ;; (e)
  
  ;; (merge-collections lom1 lom2) consumes two sorted list of magzines
  ;;   and produces the sorted list of magazine which doesn't have any
  ;;   duplicates
  ;; merge-collections: (listof Magazine) (listof Magazine)
  ;;								-> (listof Magazine)
  ;; Examples:
  (check-expect (merge-collections my-slom my-slom) my-slom)
  
  (define (merge-collections lom1 lom2)
    (cond
      [(empty? lom1) lom2]
      [(empty? lom2) lom1]
      [(magazine<? (first lom1) (first lom2))
       (cons (first lom1) (merge-collections (rest lom1) lom2))]
      [(magazine<? (first lom2) (first lom2))
       (cons (first lom2) (merge-collections lom1 (rest lom2)))]
      [else (cons (first lom1)
                   (merge-collections (rest lom1) (rest lom2)))]))
  
  ;; Tests:
  (check-expect (merge-collections my-slom my-slom2)
                (list (make-magazine "Dragon" 2)
                      (make-magazine "Dragon" 3)
                      (make-magazine "Dragon" 4)))
  
  
  ;; (f)
  
  ;; helper function
  (define (count-list lom title)
    (cond
      [(empty? lom) empty]
      [(not (string=? title (magazine-title (first lom))))
       empty]
      [else (cons (magazine-issue (first lom))
                  (count-list (rest lom) title))]))
  
  
  (define (next-title lom title)
  (cond
    [(empty? lom) empty]
    [(not (string=? title (magazine-title (first lom))))
     lom]
    [else (next-title (rest lom) title)]))


;; (create-index lom) consumes a sorted list of magazine and
;;   produces an Index
;; create-index: (listof Magazine) -> Index
;; Examples:
(define my-slom3 (list (make-magazine "Dragon" 1) 
                       (make-magazine "Dragon" 10) 
                       (make-magazine "Omni" 19)))
(check-expect (create-index my-slom3) 
              (list (list "Dragon" (list 1 10)) (list "Omni" (list 19))))

(define (create-index lom)
  (cond
    [(empty? lom) empty]
    [else
     (cons (list (magazine-title (first lom))
                 (count-list lom (magazine-title (first lom))))
           (create-index (next-title lom 
                                     (magazine-title (first lom)))))]))

;; (g)

(define (in ma1 ma2)
  (and (string=? (magazine-title ma1) (magazine-title ma2))
       (= (magazine-issue ma1) (magazine-issue ma2))))

(define (own-magazine index ma)
  (cond
    [(empty? index) false]
    [(in (first index) ma) true]
    [else (own-magazine (rest index) ma)]))

























