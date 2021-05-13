#lang racket

(require "./store.rkt")

(require rackunit)

(begin
  (initialize-store!)

  (let ([r1 (newref 5)]
        [r2 (newref 10)])
    (check-equal? (deref r1) 5)
    (check-equal? (deref r2) 10)

    (setref! r1 (deref r2))
    (setref! r2 15)

    (check-equal? (deref r1) 10)
    (check-equal? (deref r2) 15)))
