#lang racket

(require "./array.rkt")
(require "./store.rkt")

(require rackunit)

(initialize-store!)

(let ([arr (newarray 3 -99)])
  (check-equal? (arrayref arr 0) -99)
  (check-equal? (arrayref arr 1) -99)
  (check-equal? (arrayref arr 2) -99)

  (arrayset arr 1 99)
  (check-equal? (arrayref arr 1) 99)

  (check-equal? (arraylength arr) 3))
