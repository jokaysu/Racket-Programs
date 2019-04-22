(define sample-plant-data
  (list (make-plantinfo "blue eyed grass" 5 'b)
        (make-plantinfo "hosta" 3 'a)
        (make-plantinfo "columbine" 4 'a)
        (make-plantinfo "chrysanthemum" 3 'b)
        (make-plantinfo "toad lily" 4 'b)
        (make-plantinfo "agapanthus" 8 'a)
        (make-plantinfo "liriope" 7 'b)))

(define sample-city-data
  (list (make-cityinfo "Vancouver" 8 'b)
        (make-cityinfo "Edmonton" 3 'a) 
        (make-cityinfo "Waterloo" 5 'b)
        (make-cityinfo "Saint John" 5 'a)
        (make-cityinfo "Halifax" 6 'a)
        (make-cityinfo "Happy Valley-Goose Bay" 1 'a)))

(define sample-plant-data
  (cons (make-plantinfo "blue eyed grass" 5 ’b) (cons (make-plantinfo "hosta" 3 ’a)
                                                      (cons (make-plantinfo "columbine" 4 ’a) (cons (make-plantinfo "chrysanthemum" 3 ’b) empty)))))
(define sample-city-data
  (cons (make-cityinfo "Vancouver" 8 ’b) (cons (make-cityinfo "Edmonton" 3 ’a)
                                               (cons (make-cityinfo "Waterloo" 5 ’b) empty))))