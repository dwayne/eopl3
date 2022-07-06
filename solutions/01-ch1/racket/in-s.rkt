#lang racket

(provide (contract-out
          (in-S? (natural-number/c . -> . boolean?))))

(module+ test
  (require rackunit))

; The book's implementation.
;
; in-S? : N -> Bool
; (define (in-S? n)
;   (if (zero? n)
;       #t
;       (if (>= (- n 3) 0)
;           (in-S? (- n 3))
;           #f)))

(define (in-S? n)
  (or (zero? n)
      (let ([m (- n 3)])
        (and (>= m 0) (in-S? m)))))

(module+ test
  (check-true (in-S? 0))
  (check-true (in-S? 3))
  (check-true (in-S? 6))
  (check-true (in-S? 9))
  (check-true (in-S? 12))
  (check-true (in-S? 15))

  (check-false (in-S? 1))
  (check-false (in-S? 2))
  (check-false (in-S? 4))
  (check-false (in-S? 5))
  (check-false (in-S? 7))
  (check-false (in-S? 8))
  (check-false (in-S? 10))
  (check-false (in-S? 11))
  (check-false (in-S? 13))
  (check-false (in-S? 14)))
