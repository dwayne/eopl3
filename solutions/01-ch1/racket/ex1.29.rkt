#lang racket

(define (sort loi)
  (define (insert x sorted-loi)
    (if (null? sorted-loi)
        (list x)
        (if (<= x (car sorted-loi))
            (cons x sorted-loi)
            (cons (car sorted-loi)
                  (insert x (cdr sorted-loi))))))
  (if (null? loi)
      '()
      (insert (car loi) (sort (cdr loi)))))

(module+ test
  (require rackunit)

  (check-equal?
   (sort '(8 2 5 2 3))
   '(2 2 3 5 8)))
