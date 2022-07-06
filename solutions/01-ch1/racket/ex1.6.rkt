#lang racket

(provide (contract-out
          (nth-element ((listof any/c) natural-number/c . -> . any/c))))

;; Q: What would go wrong if we reversed the order of the test?

(define (nth-element lst n)
  (if (zero? n)
      (car lst)
      (if (null? lst)
          (report-list-too-short n)
          (nth-element (cdr lst) (- n 1)))))

(define (report-list-too-short n)
  (error 'nth-element "List too short by ~s elements.~n" (+ n 1)))

(module+ test
  (require rackunit)

  (check-eq? (nth-element '(a b c) 1) 'b)
  (check-eq? (nth-element '(a b c d e) 3) 'd)

  ;; We try to get the 0-th element from the empty list.
  ;; As a result, we receive an error message from `car`.
  (check-exn #rx"^car" (lambda () (nth-element '() 0))))
