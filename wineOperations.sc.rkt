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
                                 (set! row (+ row 1)))))
  (if (= row 13) return-list
  (data-searcher proc lst return-list row
                (+ column 1)
                (append clmn-list (list(list-ref (list-ref data-list column) row)))))) 

  
(define (summary-statistics lst)
  (data-searcher mean lst (list) 0 0 (list)))

(define (sum lst) (apply + lst))

(define (mean clmn-list )
  (/ (apply + clmn-list) (length clmn-list)))


                                  
(define data-list(create-list-iter my-list (list) 1 0))
(summary-statistics data-list)