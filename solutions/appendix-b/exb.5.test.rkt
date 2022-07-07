#lang racket

(require "./exb.5.interpreter.rkt")

(require rackunit)

(check-eq? (interpret "-1") -1)
(check-eq? (interpret "-x") -5)
(check-eq? (interpret "-(x+y)") -15)
(check-eq? (interpret "-x--y+z") 20)
