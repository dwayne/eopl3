#lang racket

(require "./mutpair.rkt")
(require "./store.rkt")

(require rackunit)

(initialize-store!)

(check-equal?
 (left (newpair 1 2))
 1)

(check-equal?
 (right (newpair 1 2))
 2)

(check-equal?
 (let ([p (newpair 1 2)])
   (setleft p 3)
   (left p))
 3)

(check-equal?
 (let ([p (newpair 1 2)])
   (setright p 4)
   (right p))
 4)
