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


;; LETREC tests

(check-equal?
 (run
  #<<CODE
letrec sum(n) = if zero?(n)
                then 0
                else -(n, -(0, (sum -(n, 1))))
in (sum 5)
CODE
  )
 (num-val 15))

(check-equal?
 (run
  #<<CODE
let sum = 1
in let n = 2
   in letrec sum(n) = if zero?(n)
                      then 0
                      else -(n, -(0, (sum -(n, 1))))
   in (sum 5)
CODE
  )
 (num-val 15))

;; Multideclaration LET tests

(check-equal?
 (run
  #<<CODE
let in 5
CODE
  )
 (num-val 5))

(check-equal?
 (run
  #<<CODE
let a=1 b=2 c=3
in let d=-(b, -(0, c))
   in let e=-(a, -(0, d))
      in e
CODE
  )
 (num-val 6))

;; Multiargument procedures and procedure calls tests

(check-equal?
 (run
  #<<CODE
let f = proc () 5
in (f)
CODE
  )
 (num-val 5))

(check-equal?
 (run
  #<<CODE
let sub = proc (a, b) -(a, b)
in (sub 10 4)
CODE
  )
 (num-val 6))

;; Multideclaration LETREC tests
;; Handle any number of mutually recursive unary procedures

(check-equal?
 (run
  #<<CODE
letrec
  even(x) = if zero?(x) then 1 else (odd -(x, 1))
  odd(x) = if zero?(x) then 0 else (even -(x, 1))
in (odd 13)
CODE
  )
 (num-val 1))

;; Multiargument LETREC tests
;; Handle any number of mutually recursive n-ary procedures
;; where n >= 0

(check-equal?
 (run
  #<<CODE
letrec
  f() = (fact 5)
  fact(n) = if zero?(n) then 1 else (mult n (fact -(n, 1)))
  mult(a, b) = if zero?(b) then 0 else (add (mult a -(b, 1)) a)
  add(a, b) = -(a, -(0, b))
in (f)
CODE
  )
 (num-val 120))
