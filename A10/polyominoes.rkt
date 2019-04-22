;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname polyominoes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 10
;; Problem 1 - 5
;; ############

(require "a10.rkt")

;; Uncomment the following line if you want to use
;; the examples in kanoodle.rkt
;; (require "kanoodle.rkt")

;; A Grid is a (listof (listof Char))
;; requires: both inner and outer lists of Grid are non-empty

(define-struct pos (x y))
;; A Pos is a (make-pos Int Int)

(define-struct state (puzzle pieces))
;; A State is a (make-state Grid (listof Grid))

;; A temporary neighbours function that always fails.  
;; Provide only the purpose, contract and function definition.
;;(define (neighbours s)
;;  empty)

;; solve-puzzle: Grid (listof Grid) Sym -> (anyof (listof Str) false)
;; Solve a polyomino puzzle, given the initial empty (or partially filled 
;; in) grid, a set of pieces that must be placed, and a Symbol indicating
;; what visualization style to use.  Legal viz styles are 'interactive
;; (draw every step of the search), 'at-end (just draw the solution, if one
;; is found), or 'offline (don't draw anything).  Produce either the solved
;; Grid (converted to a list of Strings, just for convenience) or false if
;; no solution exists.
;;
;; You don't need to modify this function at all.  It is provided for you
;; so that you can test your puzzle solving algorithm interactively.  If
;; you decide you want to write check-expect tests using solve-puzzle
;; (which you don't have to do, but can if you want), be sure to pass in
;; 'offline for viz-style.

;; solve-puzzle: Grid (listof Grid) Sym -> (anyof (listof Str) false)
;; requires: viz-style is one of {'interactive, 'at-end or 'offline}

;; Some Examples are included below after the solve-puzzle function definition.

;; DO NOT MODIFY THIS CODE
(define (solve-puzzle grid polys viz-style)
  (local
    [(define result
       (search 
        (lambda (S) (empty? (state-pieces S)))
        neighbours
        (cond
          [(symbol=? viz-style 'interactive)
           (lambda (S) (draw-grid (state-puzzle S)))]
          [else false])
        (make-state grid polys)))
     
     (define maybe-last-draw
       (cond
         [(and (state? result)
               (symbol=? viz-style 'at-end))
          (draw-grid (state-puzzle result))]
         [else false]))]
    (cond
      [(boolean? result) result]
      [else (map list->string (state-puzzle result))])))

;; Examples:
;; (The examples are not provided in check-expect form.  They're meant to
;; demonstrate typical uses of the function, but we don't want to them to
;; open interactive visualizations every time you start the program.)

;; Solve offline (i.e. work like a normal Scheme function).
;(solve-puzzle
;  (strlist->grid '("...." "...." "...." "...." "...." "...."))
;  (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
;  'offline)

;; Display the result graphically, if a solution is found.
;(solve-puzzle
;  (strlist->grid '("...." "...." "...." "...." "...." "...."))
;  (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
;  'at-end)

;; Display every step of the search as it progresses.
;(solve-puzzle
;  (strlist->grid '("...." "...." "...." "...." "...." "...."))
;  (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
;  'interactive)


;; 1 (a)

;; (build-2dlist w h f) consumes the width and height of a grid
;;   and a funtion, and produces the a grid that is applied to
;;   all (x,y) by f.
;; build-2dlist: Nat Nat (Nat Nat -> Any) -> (listof (listof Any))
;; Examples:
(check-expect (build-2dlist 3 2 +)
  (list (list (+ 0 0) (+ 1 0) (+ 2 0))
        (list (+ 0 1) (+ 1 1) (+ 2 1))))

(define (build-2dlist w h f)
  (build-list h 
    (lambda (i)
      (build-list w
        (lambda (j) (f j i))))))

;; Tests:
(check-expect (build-2dlist 3 2 -)
  (list (list (- 0 0) (- 1 0) (- 2 0))
        (list (- 0 1) (- 1 1) (- 2 1))))


;; 1 (b)

;; (all-positions w h) consumes two numbers, w and hm and produces
;;   a (listof Pos) containing all positions in a grid with width
;;   w and height h
;; all-positions: Nat Nat -> (listof Pos)
;; requies: w > 0 and h > 0
;; Examples:
(check-expect (lists-equiv? (all-positions 2 2)
  (list (make-pos 0 0)
        (make-pos 0 1)
        (make-pos 1 0)
        (make-pos 1 1)))
  true)

(define (all-positions w h)
  (foldr append empty (build-2dlist w h make-pos)))

;; Tests:
(check-expect (lists-equiv? (all-positions 3 2)
  (list (make-pos 0 0)
        (make-pos 0 1)
        (make-pos 1 0)
        (make-pos 1 1)
        (make-pos 2 0)
        (make-pos 2 1)))
  true)


;; 2


;; helper function

(define (get-ele g i j)
  (cond
    [(and (= i 0) (= j 1)) (first g)]
    [(= i 0) (get-ele (rest g) i (sub1 j))]
    [(= i 1) (get-ele (first g) 0 j)]
    [else (get-ele (rest g) (sub1 i) j)]))

(define (grid-h g)
  (length g))

(define (grid-w g)
  (length (first g)))

(define (build-grid w h f)
  (build-list h 
    (lambda (i)
      (build-list w
        (lambda (j) (f (add1 i) (add1 j)))))))

(define (grid-left g)
  (build-grid (grid-h g) (grid-w g) 
    (lambda (i j)
      (get-ele g j (- (grid-w g) -1 i)))))

(define (grid-right g)
  (build-grid (grid-h g) (grid-w g)
    (lambda (i j)
      (get-ele g (- (grid-h g) -1 j) i))))

(define (grid-up g)
  (build-grid (grid-w g) (grid-h g)
    (lambda (i j)
      (get-ele g (- (grid-h g) -1 i) j))))

(define (grid-rev g)
  (map reverse g))

(define (del-same logd) ; list of grid
  (cond 
    [(empty? logd) empty]
    [(empty? (first logd)) empty]
    [(check-same-grid (first logd) (rest logd))
      (del-same (rest logd))]
    [else (cons (first logd) (del-same (rest logd)))]))

(define (check-same-grid gd logd)
  (cond
    [(empty? logd) false]
    [(check-single-grid gd (first logd)) true]
    [else (check-same-grid gd (rest logd))]))

(define (check-single-grid g1 g2)
  (cond
    [(and (empty? g1) (empty? g2)) true]
    [(or (empty? g1) (empty? g2)) false]
    [(and (char? g1) (char? g2)
          (not (char=? g1 g2))) false]
    [(and (char? g1) (char? g2)
          (char=? g1 g2)) true]
    [else (and (check-single-grid (first g1) (first g2))
               (check-single-grid (rest g1) (rest g2)))]))


;; (all-orientations g) consumes a grid that is a single
;;   polyomino and produces a list of grid that are all the
;;   distinct rotations and reflections of that polyomino
;; all-orientations: Grid -> (listof Grid)
;; Examples:
(check-expect (lists-equiv? (all-orientations
  (list (list #\A #\A)
        (list #\A #\A)
        (list #\A #\A)))
  (list (list (list #\A #\A #\A)
              (list #\A #\A #\A))
        (list (list #\A #\A)
              (list #\A #\A)
              (list #\A #\A))))
  true)

(define (all-orientations g)
  (del-same (list 
    g 
    (grid-left g)
    (grid-right g)
    (grid-up g)
    (grid-rev g)
    (grid-rev (grid-left g))
    (grid-rev (grid-right g))
    (grid-rev (grid-up g)))))

;; Tests:
(check-expect (lists-equiv? (all-orientations
  (list (list #\B #\B)
        (list #\B #\B)
        (list #\B #\.)))
  (list 
  (list (list #\B #\B)
        (list #\B #\B)
        (list #\. #\B))
  (list (list #\. #\B)
        (list #\B #\B)
        (list #\B #\B))
  (list (list #\B #\.)
        (list #\B #\B)
        (list #\B #\B))
  (list (list #\B #\B)
        (list #\B #\B)
        (list #\B #\.))
  (list (list #\B #\B #\B)
        (list #\B #\B #\.))
  (list (list #\B #\B #\.)
        (list #\B #\B #\B))
  (list (list #\. #\B #\B)
        (list #\B #\B #\B))
  (list (list #\B #\B #\B)
        (list #\. #\B #\B))))
  true)

(check-expect (lists-equiv? (all-orientations
  (list (list #\. #\C #\.)
        (list #\C #\C #\C)
        (list #\. #\C #\.)))
  (list 
  (list (list #\. #\C #\.)
        (list #\C #\C #\C)
        (list #\. #\C #\.))))
  true)

(check-expect (lists-equiv? (all-orientations
  (list (list #\D #\D #\D #\D)))
  (list 
  (list (list #\D #\D #\D #\D))
  (list (list #\D)
        (list #\D)
        (list #\D)
        (list #\D))))
  true)


;; 3

;; helper
(define (find-empty g lop)
  (cond
    [(empty? lop) false]
    [(char=? #\. (get-ele g 
      (add1 (pos-y (first lop)))
      (add1 (pos-x (first lop)))))
      (first lop)]
    [else (find-empty g (rest lop))]))

;; (first-empty-pos g) consumes a grid and produces the pos
;;   of the first #\. character in the grid
;; first-empty-pos: Grid -> (anyof Pos false)
;; Examples:
(check-expect (first-empty-pos 
  (list (list #\A #\A #\.)
        (list #\A #\. #\A)))
  (make-pos 2 0))

(define (first-empty-pos g)
  (cond
    [(empty? g) false]
    [else (find-empty g 
      (all-positions (grid-w g) (grid-h g)))]))

;; Tests:
(check-expect (first-empty-pos 
  (list (list #\B #\B #\B)
        (list #\B #\. #\B)))
  (make-pos 1 1))

(check-expect (first-empty-pos
  (list (list #\C)
        (list #\C)
        (list #\C)))
  false)


;; 4

;; (superimpose b t p) consumes two grids as base and top
;;   and a grid, and produces a new grid that covers 
;;   the content of top to the base with the posision.
;; superimpose: Grid Grid Pos -> Grid
;; Examples:
(check-expect (superimpose
  (list (list #\A #\.)
        (list #\A #\A)
        (list #\. #\A))
  (list (list #\B #\B)
        (list #\. #\B)
        (list #\. #\B))
  (make-pos 0 0))
  (list (list #\B #\B)
        (list #\A #\B)
        (list #\. #\B)))

(define (superimpose b t p)
  (build-list (grid-h b)
    (lambda (i)
      (build-list (grid-w b)
        (lambda (j) (cond
  [(or (< j (pos-x p))
       (< i (pos-y p))
       (>= j (+ (pos-x p) (grid-w t)))
       (>= i (+ (pos-y p) (grid-h t))))
   (get-ele b (add1 i) (add1 j))]
  [(char=? #\. 
    (get-ele t (- (add1 i) (pos-y p)) 
               (- (add1 j) (pos-x p))))
   (get-ele b (add1 i) (add1 j))]
  [else (get-ele t (- (add1 i) (pos-y p)) 
                   (- (add1 j) (pos-x p)))]))))))

;; Tests:
(check-expect (superimpose
  (list (list #\A #\.)
        (list #\A #\A)
        (list #\. #\A))
  (list (list #\B #\B)
        (list #\. #\B)
        (list #\. #\B))
  (make-pos 1 0))
  (list (list #\A #\B)
        (list #\A #\A)
        (list #\. #\A)))

(check-expect (superimpose
  (list (list #\A #\A #\A)
        (list #\A #\A #\A)
        (list #\A #\A #\A))
  (list (list #\B #\B #\B)
        (list #\B #\. #\B)
        (list #\B #\B #\B))
  (make-pos 1 1))
  (list (list #\A #\A #\A)
        (list #\A #\B #\B)
        (list #\A #\B #\A)))

(check-expect (superimpose
  (list (list #\A #\A #\A)
        (list #\A #\A #\A)
        (list #\A #\A #\A))
  (list (list #\B))
  (make-pos 1 1))
  (list (list #\A #\A #\A)
        (list #\A #\B #\A)
        (list #\A #\A #\A)))


;; 5

;; helper function

(define (try-all-ori p g logd s)
  (cond
    [(empty? logd) empty]
    [(fit-puzzle? p (first logd) 
      (grid-w (first logd))
      (grid-h (first logd))) 
    (cons
      (make-state (get-fit-puzzle p (first logd))
        (filter (lambda (i) (not (lists-equiv? g i))) 
          (state-pieces s)))
      (try-all-ori p g (rest logd) s))]
    [else (try-all-ori p g (rest logd) s)]))

(define (fit-puzzle? p g x y)
  (local
    [(define sp (first-empty-pos p))];start-point
    (cond
      [(and (= y 1) (= x 0)) true]
      [(= x 0) (fit-puzzle? p g (grid-w g) (- y 1))]
      [(and (not (char=? #\.
                  (get-ele p 
                    (+ (pos-y sp) (- (grid-h g) y -1))
                    (+ (pos-x sp) (- (grid-w g) x -1)))))
            (not (char=? #\.
                  (get-ele g 
                    (- (grid-h g) y -1)
                    (- (grid-w g) x -1))))) false]
      [else (fit-puzzle? p g (- x 1) y)])))

(define (get-fit-puzzle p g)
  (superimpose p g (first-empty-pos p)))


;; (neighbours s) consumes a state and produces a list of
;;   states in which one additional polyomino has been placed
;;   in the puzzle and removed from the list of pieces
;; neighbours: State -> (listof State)

(define (neighbours s)
  (cond
    [(empty? (state-pieces s)) empty]
    [else (append
      (try-all-ori (state-puzzle s) 
        (first (state-pieces s))
        (all-orientations (first (state-pieces s)))
        s)
      (neighbours (make-state (state-puzzle s) 
                              (rest (state-pieces s)))))]))

(check-expect (neighbours (make-state
  (list (list #\A #\. #\A)
        (list #\. #\. #\.))
  (list (list (list #\B)
              (list #\B)))))
  (list (make-state
    (list (list #\A #\B #\A)
          (list #\. #\B #\.))
    empty)))































