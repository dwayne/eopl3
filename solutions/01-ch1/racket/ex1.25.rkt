#lang racket

(define (exists? pred lst)
  (if (null? lst)
      #f
      (or
       (pred (car lst))
       (exists? pred (cdr lst)))))

(module+ test
  (require rackunit)

  (check-true (exists? number? '(a b c 3 e)))
  (check-false (exists? number? '(a b c d e))))
