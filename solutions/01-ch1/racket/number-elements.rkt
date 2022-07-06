#lang racket

(define (number-elements lst)
  (number-elements-from lst 0))

(define (number-elements-from lst n)
  (if (null? lst)
      '()
      (cons
       (list n (car lst))
       (number-elements-from (cdr lst) (+ n 1)))))

(module+ test
  (require rackunit)

  (check-equal?
   (number-elements '(a b c))
   '((0 a) (1 b) (2 c))))