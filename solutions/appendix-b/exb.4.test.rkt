#lang racket

(require "./exb.4.interpreter.rkt")

(require rackunit)

(check-eq? (interpret "x") 5)
(check-eq? (interpret "y") 10)
(check-eq? (interpret "x+y") (interpret "z"))
