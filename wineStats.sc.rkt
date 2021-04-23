#lang racket

(define (create-list given-list index) ;Converts the given string list into numbers list.
  (create-list-iter given-list '() index 0))

(define (create-list-iter lst lst2 row column)
  (if (= column (length lst)) lst2
      (create-list-iter lst
                        (append lst2 (list (column-iter lst '() row column)))
                         row
                         (+ column 1))))                               
                                
(define (column-iter lst lst2 row column)
  (if (= row 14) lst2
      (column-iter lst
                (append lst2 (list (string->number(list-ref (string-split(list-ref lst column) ",") row))))
                (+ row 1)
                column)))

(define (data-searcher procedure given-list column) ;Calculates the given procedure of the given attribute. (mean,median or variance)
  (data-searcher-iter procedure given-list 0 column '()))

(define (data-searcher-iter procedure lst row column return-list) 
  (if (= row (length lst)) (procedure return-list)
      (data-searcher-iter procedure lst (+ row 1) column
                          (append return-list (list(list-ref (list-ref lst row) column))))))

(define (mean clmn-list ) ;Calculates mean
  (/ (apply + clmn-list) (length clmn-list)))

(define (variance clmn-list) ;Calculates variance
    (/ (apply + (map
                 (lambda (number1 number2)(square(- number1 number2)))
                  clmn-list (make-list (length clmn-list) (mean clmn-list))))
       (- (length clmn-list) 1)))
 
(define (median clmn-list) ;Calculates median
  (define lngth(length clmn-list))
  (if (even? lngth) (/ (+(list-ref (sort clmn-list <) (/ lngth 2)) (list-ref (sort clmn-list <) (- (/ lngth 2) 1))) 2) 
      (list-ref (sort clmn-list <) ((/ (- lngth 1) 2)))))

(define (split-set lst) ; TASK2: Shuffles the list and splits with ratios 80% and 20%.
 (split-iter (shuffle lst) 0 (length lst) '() '()))

(define (split-iter lst counter max-count list80 list20)
  (define return-list'())
  (cond ((< counter (find80 max-count)) (split-iter lst (+ counter 1) max-count (append list80 (list(list-ref lst counter))) list20))
        ((>= counter (find80 max-count)) (begin(if (= counter max-count) (append (append return-list (list (append list80))) (list list20))
                                                   (split-iter lst (+ counter 1) max-count list80 (append list20 (list(list-ref lst counter)))))))))

(define (classify-iter test-list accuracy counter) ; TASK 3: Randomly classifies the data instances and returns total accuracy.
  (define chosen-class (rnd-number))
  (if (= counter (length test-list)) (percentage accuracy counter)
      (begin (if (= chosen-class (car (list-ref test-list counter))) (classify-iter test-list (+ accuracy 1) (+ counter 1))
                 (classify-iter test-list accuracy (+ counter 1))))))
  
(define (rnd-number) (round(+ (* (random) 2) 1))) ; Select random number 1, 2 or 3
(define (find80 x) (/ (* x 8) 10)) ; Find the 80% of the list
(define (square x) (* x x)) 
(define (percentage x y) (/ (* x 100) y)) ; Convert the given fraction into percentage.

(define (summary-statistics lst attr-list counter) ; TASK 1: Report the summary statistics.
  (display (list-ref attr-list counter))
  (display ":\n")
  (display "Mean: ")
  (display (exact->inexact (data-searcher mean lst counter)))
  (display "\n")
  (display "Median: ")
  (display (exact->inexact (data-searcher median lst counter)))
  (display "\n")
  (display "Variance:")
  (display (exact->inexact (data-searcher variance lst counter)))
  (display "\n\n")
  ( if ( < counter 12)
       (summary-statistics lst attr-list (+ counter 1))
       (display"\n")))

(define (report-accuracy original-list) ; Report the total accuracy
  (define splitted-list(split-set original-list))
  (display "Given data is splitted into training and test set after shuffling.\n")
  (define test-set(create-list (cadr splitted-list) 0))
  (display "Total accuracy: %")
  (display (exact->inexact (classify-iter test-set 0 0))))
  
(define attributes(list "Alcohol" "Malic Acid" "Ash" "Alcalinity of Ash" "Magnesium" "Total Phenols" "Flavanoids" "Nonflavanoid Phenols"
                            "Proanthocyanins" "Color Intensity" "Hue" "OD280/OD315 of Diluted Wines" "Proline"))
(define input-list(file->lines "wine.data.txt")) ;Read the given dataset into a list
(define data-list(create-list input-list 1))     ;Convert the string list into a number list
(summary-statistics data-list attributes 0)      ;TASK 1: Give summary statistics for each attribute.
(report-accuracy input-list)                     ;TASK 2&3: Shuffle and split the data set, then randomly classify and report the accuracy.
