#lang racket

(define (create-list given-list index)
  (create-list-iter given-list (list) index 0))

(define (create-list-iter lst lst2 row column)
  (cond ((= column (length lst)) lst2)
        (else (create-list-iter lst
                                (append lst2 (list (row-iter lst (list) row column)))
                                row
                                (+ column 1)))))                                
                                
(define (row-iter lst lst2 row column)
  (if (= row 14) lst2
      (row-iter lst
                (append lst2 (list (string->number(list-ref (string-split(list-ref lst column) ",") row))))
                (+ row 1)
                column)))

(define (data-searcher procedure lst return-list row column clmn-list) 
  (cond ((= column (length lst)) (begin (set! return-list (append return-list (list (procedure clmn-list))))
                                        (set! column 0)
                                        (set! row (+ row 1))
                                        (set! clmn-list (list) ))))
         (if (= row 13) return-list
             (data-searcher procedure lst return-list row
                            (+ column 1)
                            (append clmn-list (list(list-ref (list-ref data-list column) row)))))) 

(define (mean clmn-list )
  (/ (apply + clmn-list) (length clmn-list)))

(define (variance clmn-list)
    (/ (apply + (map
                 (lambda (number1 number2)(square(- number1 number2)))
                  clmn-list (make-list (length clmn-list) (mean clmn-list))))
       (- (length clmn-list) 1)))
 
(define (median clmn-list)
  (set! clmn-list (sort clmn-list <))
  (define lngth(length clmn-list))
  (if (even? lngth) (/ (+(list-ref clmn-list (/ lngth 2)) (list-ref clmn-list (+ (/ lngth 2) 1))) 2) 
      (list-ref clmn-list (+ (/ lngth 2) 1))))

(define (split-set lst)
 (split-iter (shuffle lst) 0 (length lst) (list) (list)))

(define (split-iter lst counter max-count list80 list20)
  (define return-list(list))
  (cond ((< counter (find80 max-count)) (split-iter lst (+ counter 1) max-count (append list80 (list(list-ref lst counter))) list20))
        ((>= counter (find80 max-count)) (begin(if (= counter max-count) (append (append return-list (list (append list80))) (list list20))
                                                   (split-iter lst (+ counter 1) max-count list80 (append list20 (list(list-ref lst counter)))))))))

(define (classify-iter test-list accuracy counter)
  (define real-class(rnd-number))
  (define chosen-class(rnd-number))
  (if (= counter (length test-list)) (percentage accuracy counter)
      (begin (set! real-class (list-ref (list-ref test-list counter) 0))
             (if (= chosen-class real-class) (classify-iter test-list (+ accuracy 1) (+ counter 1))
                 (classify-iter test-list accuracy (+ counter 1))))))
  
(define (rnd-number) (round(+ (* (random) 2) 1)))
(define (find80 x) (/ (* x 8) 10)) 
(define (square x) (* x x))
(define (percentage x y) (/ (* x 100) y))

(define (summary-statistics lst attr-list counter)
  (display (list-ref attr-list counter))
  (display ":\n")
  (display "Mean: ")
  (display (exact->inexact (list-ref (data-searcher mean lst (list) 0 0 (list)) counter)))
  (display "\n")
  (display "Median: ")
  (display (exact->inexact (list-ref (data-searcher median lst (list) 0 0 (list)) counter)))
  (display "\n")
  (display "Variance:")
  (display (exact->inexact (list-ref (data-searcher variance lst (list) 0 0 (list)) counter)))
  (display "\n\n")
  ( if ( < counter 12)
       (summary-statistics lst attr-list (+ counter 1))
       (display"\n")))

(define (report-accuracy original-list)
  (define splitted-list(split-set original-list))
  (display "Given data is splitted into training and test set after shuffling.\n")
  (define test-set(create-list (list-ref splitted-list 1) 0))
  (display "Total accuracy: %")
  (display (exact->inexact (classify-iter test-set 0 0))))
  

(define attributes(list "Alcohol" "Malic Acid" "Ash" "Alcalinity of Ask" "Magnesium" "Total Phenols" "Flavanoids" "Nonflavanoid Phenols"
                            "Proanthocyanins" "Color Intensity" "Hue" "OD280/OD315 of Diluted Wines" "Proline"))

(define input-list(file->lines "wine.data.txt"))
(define data-list(create-list input-list 1))
(summary-statistics data-list attributes 0)
(report-accuracy input-list)
