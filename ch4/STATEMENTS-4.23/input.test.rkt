#lang racket

(require "./input.rkt")

(require rackunit)

(initialize-input! '(1 2))

(check-eq? (getint) 1)
(check-eq? (getint) 2)