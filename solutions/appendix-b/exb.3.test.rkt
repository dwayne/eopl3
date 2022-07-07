#lang racket

(require "./exb.3.rkt")

(require rackunit)

(check-eq? (interpret "1") 1)
(check-eq? (interpret "12345") 12345)

(check-eq? (interpret "1+23") 24)
(check-eq? (interpret "45-6") 39)
(check-eq? (interpret "7*8") 56)
(check-eq? (interpret "99/3") 33)

(check-eq? (interpret "5-2-1") 2)
(check-eq? (interpret "5-(2-1)") 4)

(check-eq? (interpret "3+2*66-5") 130)
