#lang racket
 (define my-list
  (file->lines "wine.data.txt"
    ))

(define (create-list-iter lst lst2 row column)
  (cond ((= column (length lst)) lst2)
        (else (create-list-iter lst
                                (append lst2 (list (row-iter lst (list) row column)))
                                row
                                (+ column 1)))))
                                
                                
(define (row-iter lst lst2 row column)
  (if (= row 14) lst2
      (row-iter lst
                (append lst2 (list (string->number
                       (list-ref
                           (string-split
                               (list-ref my-list column) ",") row))))
                (+ row 1) column)))


(define (data-searcher proc lst return-list row column clmn-list) 
  (cond ((= column (length lst)) (begin (set! return-list (append return-list (list (proc clmn-list))))
                                 (set! column 0)
                                 (set! row (+ row 1))
                                 (set! clmn-list (list) ))))
         (if (= row 13) return-list
         (data-searcher proc lst return-list row
         (+ column 1)
         (append clmn-list (list(list-ref (list-ref data-list column) row)))))) 


(define (mean clmn-list )
  (/ (apply + clmn-list) (length clmn-list)))

(define (variance clmn-list)
    (/ (apply + (map (lambda (number1 number2)
         (square(- number1 number2)))
             clmn-list (make-list (length clmn-list) (mean clmn-list)))) (- (length clmn-list) 1)))
 
(define (median clmn-list)
  (set! clmn-list (sort clmn-list <))
  (define lngth(length clmn-list))
  (if (even? lngth) (/ (+(list-ref clmn-list (/ lngth 2)) (list-ref clmn-list (+ (/ lngth 2) 1))) 2) 
      ( list-ref clmn-list (+ (/ lngth 2) 1))))

(define (split-set lst)
 (split-iter (shuffle lst) 0 (length lst) (list) (list)))

(define (split-iter lst counter max-count list80 list20)
  (define return-list(list))
  (cond ((< counter (find80 max-count)) 
                                     (split-iter lst (+ counter 1) max-count (append list80 (list(list-ref lst counter))) list20))
        ((>= counter (find80 max-count)) (begin
                                          (if (= counter max-count) (append (append return-list (list (append list80))) (list list20))
                                                   (split-iter lst (+ counter 1) max-count list80 (append list20 (list(list-ref lst counter)))))))))




(define (find80 x) (/ (* x 8) 10)) 
(define (square x) (* x x))

(define (list-printer lst1 lst2)
  (print(list-ref lst1 0))
  (display ": ") 
  (print(list-ref lst2 0))
  (display "\n")
  (if (= (length lst1) 1) (display "\n")
  (list-printer (list-tail lst1 1) (list-tail lst2 1))))

(define (summary-statistics lst )
  (display "MEAN:\n")
  (list-printer attributes (data-searcher mean lst (list) 0 0 (list)))
  (display "MEDIAN:\n")
  (list-printer attributes (data-searcher median lst (list) 0 0 (list)))
  (display "VARIANCE:\n")
  (list-printer attributes (data-searcher variance lst (list) 0 0 (list))))

(define attributes(list "Alcohol" "Malic acid" "Ash" "Alcalinity of ask" "Magnesium" "Total phenols" "Flavanoids" "Nonflavanoid phenols"
                            "Proanthocyanins" "Color intensity" "Hue" "OD280/OD315 of diluted wines" "Proline"))

(define data-list(create-list-iter my-list (list) 1 0))
(summary-statistics data-list)
(define splitted-list(split-set my-list))
splitted-list
