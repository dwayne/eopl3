#lang racket

(provide (contract-out
          (nth-element ((listof any/c) natural-number/c . -> . any/c))))

(define (nth-element original-lst original-n)
  (define (go lst n)
    (if (null? lst)
        (report-list-too-short original-lst original-n)
        (if (zero? n)
            (car lst)
            (go (cdr lst) (- n 1)))))
  (go original-lst original-n))

(define (report-list-too-short lst n)
  (error 'nth-element "~s does not have ~s elements.~n" lst (+ n 1)))

(module+ test
  (require rackunit)

  (check-eq? (nth-element '(a b c) 1) 'b)
  (check-eq? (nth-element '(a b c d e) 3) 'd)

  (check-exn
   #rx"\\(a b c\\) does not have 8 elements\\."
   (lambda () (nth-element '(a b c) 7))))
