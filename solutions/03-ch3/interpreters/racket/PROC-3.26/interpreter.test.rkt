#lang racket

(require "./interpreter.rkt")

(require rackunit)

(check-equal?
 (run "5")
 (num-val 5))

(check-equal?
 (run "x")
 (num-val 10))

(check-equal?
 (run "zero?(i)")
 (bool-val #f))

(check-equal?
 (run "zero?(-(i, 1))")
 (bool-val #t))

(check-equal?
 (run "-(55, -(x, 11))")
 (num-val 56))

(check-equal?
 (run "-(-(x, 3), -(v, i))")
 (num-val 3))

(check-equal?
 (run
  #<<CODE
let x = 33
in let y = 22
   in if zero?(-(x, 11)) then -(y, 2) else -(y, 4)
CODE
  )
 (num-val 18))

(check-equal?
 (run "let x = 5 in -(x, 3)")
 (num-val 2))

(check-equal?
 (run
  #<<CODE
let z = 5
in let x = 3
   in let y = -(x, 1)
      in let x = 4 in -(z, -(x, y))
CODE
  )
 (num-val 3))

(check-equal?
 (run
  #<<CODE
let x = 7
in let y = 2
   in let y = let x = -(x, 1)
              in -(x, y)
      in -(-(x, 8), y)
CODE
  )
 (num-val -5))

(check-equal?
 (run
  #<<CODE
let f = proc (x) -(x, 11)
in (f (f 77))
CODE
  )
 (num-val 55))

(check-equal?
 (run
  #<<CODE
(proc (f) (f (f 77))
 proc (x) -(x, 11))
CODE
  )
 (num-val 55))

(check-equal?
 (run
  #<<CODE
let x = 200
in let f = proc (z) -(z, x)
   in let x = 100
      in let g = proc (z) -(z, x)
         in -((f 1), (g 1))
CODE
  )
 (num-val -100))

;; Exercise 3.20
;;
;; Write a curried procedure that takes two arguments and returns their sum.

(check-equal?
 (run
  #<<CODE
let f = proc (x) proc (y) -(x, -(0, y))
in let a = ((f 1) 2)
   in let b = ((f a) 3)
      in let c = ((f b) 4)
         in let d = ((f c) 5) in d
CODE
  )
 (num-val 15))

;; Exercise 3.26

(check-equal?
 (run
  #<<CODE
let x = 1
in let y = 2
   in let f = proc (x)
                let x = -(5, x)
                in let y = -(0, x)
                   in -(x, y)
      in let x = (f x)
         in x
CODE
  )
 (num-val 8))
