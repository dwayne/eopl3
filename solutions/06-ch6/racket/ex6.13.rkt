#lang racket

;; 1.
(define (removeall n s k)
  (if (null? s)
      (k '())
      (if (number? (car s))
          (if (equal? n (car s))
              (removeall n (cdr s) k)
              (removeall n (cdr s)
                         (lambda (t)
                           (k (cons (car s) t)))))
          (removeall n (car s)
                     (lambda (h)
                       (removeall n (cdr s)
                                  (lambda (t)
                                    (k (cons h t)))))))))

(module+ test
  (require rackunit)

  (check-equal?
   (removeall 1 '(1 2 3 4 1 2 1)
              (lambda (n)
                (displayln "End of computation")
                n))
   '(2 3 4 2)))