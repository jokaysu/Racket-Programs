;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname plants) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ############
;; Chenzheng Su
;; 20643469
;; CS 135
;; Winter 2016
;; Assignment 5
;; Problem 3
;; ############


(define-struct cityinfo (name zone subzone))
;; A CityInfo is a (make-cityinfo Str Nat Sym)

(define-struct plantinfo (name zone subzone))
;; A PlantInfo is a (make-plantinfo Str Nat Sym)

;; A PlanInfoList is a (listof PlantInfo)
;; A CityInfoList is a (listof CityInfo)


;; Test DATA
(define sample-plant-data2
  (list (make-plantinfo "blue eyed grass" 5 'b)
        (make-plantinfo "hosta" 3 'a)
        (make-plantinfo "columbine" 4 'a)
        (make-plantinfo "chrysanthemum" 3 'b)
        (make-plantinfo "toad lily" 4 'b)
        (make-plantinfo "agapanthus" 8 'a)
        (make-plantinfo "liriope" 7 'b)))

(define sample-city-data2
  (list (make-cityinfo "Vancouver" 8 'b)
        (make-cityinfo "Edmonton" 3 'a) 
        (make-cityinfo "Waterloo" 5 'b)
        (make-cityinfo "Saint John" 5 'a)
        (make-cityinfo "Halifax" 6 'a)
        (make-cityinfo "Happy Valley-Goose Bay" 1 'a)))

(define sample-plant-data
  (cons (make-plantinfo "blue eyed grass" 5 'b) 
        (cons (make-plantinfo "hosta" 3 'a)
              (cons (make-plantinfo "columbine" 4 'a) 
                    (cons (make-plantinfo "chrysanthemum" 3 'b) empty)))))
(define sample-city-data
  (cons (make-cityinfo "Vancouver" 8 'b) 
        (cons (make-cityinfo "Edmonton" 3 'a)
              (cons (make-cityinfo "Waterloo" 5 'b) empty))))
;; Test DATA ends


;; Helper Function
(define (check ci pi)
  (cond
    [(< (cityinfo-zone ci) (plantinfo-zone pi)) false]
    [(and (= (cityinfo-zone ci) (plantinfo-zone pi))
          (symbol=? (cityinfo-subzone ci)  'a)
          (symbol=? (plantinfo-subzone pi) 'b))
     false]
    [else true]))


;; (find-hardy-plants cif pifl) consumes a CityInfo and a
;;   PlantInfoList and produces a PlantInfoList that can
;;   survive in that city
;; find-hardy-plants: 
;; CityInfo (listof PlantInfo) -> (listof PlantInfo)
;; Examples:
(check-expect (find-hardy-plants 
               (make-cityinfo "Edmonton" 3 'a) sample-plant-data)
              (list (make-plantinfo "hosta" 3 'a)))

(define (find-hardy-plants cif pifl)
  (cond
    [(empty? pifl) empty]
    [else 
     (cond
       [(check cif (first pifl))
        (cons (first pifl) 
              (find-hardy-plants cif (rest pifl)))]
       [else
        (find-hardy-plants cif (rest pifl))])]))

;; Tests:
(check-expect (find-hardy-plants 
               (make-cityinfo "Quebec" 4 'b) sample-plant-data)
              (list (make-plantinfo "hosta" 3 'a)
                    (make-plantinfo "columbine" 4 'a)
                    (make-plantinfo "chrysanthemum" 3 'b)))


;; (find-growing-cities pif cifl) consumes a PlantInfo
;;   and a CityInfoList and produces a list of Str
;;   containing the names of cities that plant will grow
;; find-growing-cities: 
;; PlantInfo (listof CityInfo) -> (listof Str)
;; Examples:
(check-expect (find-growing-cities
               (make-plantinfo "columbine" 4 'a) sample-city-data)
              (list "Vancouver" "Waterloo"))

(define (find-growing-cities pif cifl)
  (cond
    [(empty? cifl) empty]
    [else
     (cond
       [(check (first cifl) pif)
        (cons (cityinfo-name (first cifl))
              (find-growing-cities pif (rest cifl)))]
       [else
        (find-growing-cities pif (rest cifl))])]))

;; Tests:
(check-expect (find-growing-cities
               (make-plantinfo "columb" 1 'a) sample-city-data)
              (list "Vancouver" "Edmonton" "Waterloo"))


;; Helper Function:
(define (more-check pifl cif)
  (cond
    [(empty? pifl) true]
    [else
     (cond
       [(check cif (first pifl)) false]
       [else (more-check (rest pifl) cif)])]))


;; (find-plantless-cities pifl cifl) consumes a PlantInfoList
;;   and a CitiInfoList and produces a CityInfoList that
;;   contains all the cities without plants being able
;;   to survive
;; find-plantless-cities:
;; (listof PlanInfo) (listof CityInfo) -> (listof CityInfo)
;; Examples:
(check-expect (find-plantless-cities
               sample-plant-data sample-city-data)
              empty)

(define (find-plantless-cities pifl cifl)
  (cond
    [(empty? cifl) empty]
    [else
     (cond
       [(more-check pifl (first cifl))
        (cons (first cifl)
              (find-plantless-cities pifl (rest cifl)))]
       [else
        (find-plantless-cities pifl (rest cifl))])]))

;; Tests:
(check-expect (find-plantless-cities
               sample-plant-data2 sample-city-data2)
              (list (make-cityinfo "Happy Valley-Goose Bay" 1 'a)))















