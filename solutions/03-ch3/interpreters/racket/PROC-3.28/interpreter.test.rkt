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
 ; (num-val -100)) This isn't true anymore when using dynamic binding
 (num-val 0))

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

;; Exercise 3.28
;;
;; Dynamic binding (or dynamic scoping)

(check-equal?
 (run
  #<<CODE
let a = 3
in let p = proc (x) -(x, a)
   in let a = 5
      in -(a, (p 2))
CODE
  )
 ; with lexical binding we'd get
 ; (num-val 6)
 (num-val 8))

(check-equal?
 (run
  #<<CODE
let a = 3
in let p = proc (z) a
   in let f = proc (x) (p 0)
      in let a = 5
         in (f 2)
CODE
  )
 ; with lexical binding we'd get
 ; (num-val 3)
 (num-val 5))

;; Exercise 3.29
;;
;; What if f's formal parameter were a?

(check-equal?
 (run
  #<<CODE
let a = 3
in let p = proc (z) a
   in let f = proc (a) (p 0)
      in let a = 5
         in (f 2)
CODE
  )
 (num-val 2))

;; Exercise 3.37
;;
;; With dynamic binding you get 120.

(check-equal?
 (run
  #<<CODE
let fact = proc (n) (addone n)
in let fact = proc (n)
                if zero?(n)
                then 1
                else *(n, (fact -(n, 1)))
   in (fact 5)
CODE
  )
 (num-val 120))

;; The mutually recursive procedures even and odd from section 3.4

(check-equal?
 (run
  #<<CODE
let even = proc (x) if zero?(x) then 1 else (odd -(x, 1))
in let odd = proc (x) if zero?(x) then 0 else (even -(x, 1))
   in (odd 13)
CODE
  )
 (num-val 1))