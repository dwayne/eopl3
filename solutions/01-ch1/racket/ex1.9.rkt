#lang racket

(provide (contract-out
          (remove (symbol? (listof symbol?) . -> . (listof symbol?)))))

(define (remove s los)
  (if (null? los)
      '()
      (if (symbol=? s (car los))
          (remove s (cdr los))
          (cons (car los) (remove s (cdr los))))))

(module+ test
  (require rackunit)

  (check-equal? (remove 'a '(a b c)) '(b c))
  (check-equal? (remove 'b '(e f g)) '(e f g))
  (check-equal? (remove 'a4 '(c1 a4 c1 a4)) '(c1 c1))
  (check-equal? (remove 'x '()) '()))
