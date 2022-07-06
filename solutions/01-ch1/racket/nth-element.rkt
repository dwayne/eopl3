#lang racket

(provide (contract-out
          (nth-element ((listof any/c) natural-number/c . -> . any/c))))

(define (nth-element lst n)
  (if (null? lst)
      (report-list-too-short n)
      (if (zero? n)
          (car lst)
          (nth-element (cdr lst) (- n 1)))))

(define (report-list-too-short n)
  (error 'nth-element "List too short by ~s elements.~n" (+ n 1)))

(module+ test
  (require rackunit)

  (check-eq? (nth-element '(a b c) 1) 'b)
  (check-eq? (nth-element '(a b c d e) 3) 'd))
