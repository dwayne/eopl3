#lang racket

(provide (contract-out
          (remove-first (symbol? (listof symbol?) . -> . (listof symbol?)))))

;; Returns the rest of the list after the first occurrence of s is removed.
;; If s is not in the list then the empty list is returned.
(define (remove-first s los)
  (if (null? los)
      '()
      (if (symbol=? s (car los))
          (cdr los)
          (remove-first s (cdr los)))))

(module+ test
  (require rackunit)

  (check-equal? (remove-first 'a '(a b c)) '(b c))
  (check-equal? (remove-first 'b '(e f g)) '())
  (check-equal? (remove-first 'a4 '(c1 a4 c1 a4)) '(c1 a4))
  (check-equal? (remove-first 'x '()) '()))
