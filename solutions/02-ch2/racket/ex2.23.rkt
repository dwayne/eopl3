#lang racket

;; Exercise 2.23
;;
;; An identifier is any symbol other than "lambda". Modify the definition
;; of identifier? to capture this condition.

(define (identifier? v)
  (and
   (symbol? v)
   (not (symbol=? v 'lambda))))

(module+ test
  (require rackunit)

  (check-true (identifier? 'foo))
  (check-false (identifier? 'lambda)))
