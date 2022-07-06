#lang racket

(define (sort/predicate pred loi)
  (define (insert x sorted-loi)
    (if (null? sorted-loi)
        (list x)
        (if (pred x (car sorted-loi))
            (cons x sorted-loi)
            (cons (car sorted-loi)
                  (insert x (cdr sorted-loi))))))
  (if (null? loi)
      '()
      (insert (car loi) (sort/predicate pred (cdr loi)))))

(module+ test
  (require rackunit)

  (check-equal?
   (sort/predicate < '(8 2 5 2 3))
   '(2 2 3 5 8))

  (check-equal?
   (sort/predicate > '(8 2 5 2 3))
   '(8 5 3 2 2)))
