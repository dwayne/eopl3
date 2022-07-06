#lang racket

(define (every? pred lst)
  (if (null? lst)
      #t
      (and
       (pred (car lst))
       (every? pred (cdr lst)))))

(module+ test
  (require rackunit)

  (check-false (every? number? '(a b c 3 e)))
  (check-true (every? number? '(1 2 3 4 5))))
