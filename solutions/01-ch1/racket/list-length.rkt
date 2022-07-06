#lang racket

(provide (contract-out
          (list-length ((listof any/c) . -> . natural-number/c))))

(define (list-length l)
  (if (null? l)
      0
      (+ 1 (list-length (cdr l)))))

(module+ test
  (require rackunit)

  (check-eq? (list-length '()) 0)
  (check-eq? (list-length '(a b c)) 3)
  (check-eq? (list-length '((x) ())) 2)
  (check-eq? (list-length '(a (b c) d)) 3))
