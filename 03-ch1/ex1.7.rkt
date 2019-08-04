#lang eopl

(define (nth-element start-lst start-n)
  (define (helper lst n)
    (if (zero? n)
        (car lst)
        (if (null? lst)
            (report-list-too-short start-lst start-n)
            (helper (cdr lst) (- n 1)))))
  (helper start-lst start-n))

(define (report-list-too-short lst n)
  (eopl:error
   'nth-element
   "~s does not have ~s elements.~%" lst (+ n 1)))