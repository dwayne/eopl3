#lang racket

(require "./screen.rkt")

(require rackunit)

(begin
  (initialize-screen!)

  (print 1)
  (print 2)
  (print 3)

  (check-equal? (get-output) '(3 2 1)))
