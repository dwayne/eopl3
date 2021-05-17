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

;; Exercise 3.37
;;
;; With lexical binding you get 25.

(check-equal?
 (run
  #<<CODE
let fact = proc (n) add1(n)
in let fact = proc (n)
                if zero?(n)
                then 1
                else *(n, (fact -(n, 1)))
   in (fact 5)
CODE
  )
 (num-val 25))

;; Exercise 4.38
;;
;; What happens if the program below is run under call-by-value?

;; Uncomment to see what happens:
;(check-equal?
; (run
;  #<<CODE
;let makerec = proc (f)
;                let d = proc (x) (f (x x))
;                in (f (d d))
;in let maketimesfour = proc (f)
;                         proc (x)
;                           if zero?(x)
;                           then 0
;                           else -((f -(x, 1)), -(0, 4))
;   in let timesfour = (makerec maketimesfour)
;      in (timesfour 3)
;CODE
;  )
; (num-val 12))

;; Answer: It loops indefinitely until it runs out of memory.
;;
;; Why?
;;
;; Because
;;
;; (timesfour 3)
;; = ((makerec maketimesfour) 3)
;; = ((maketimesfour (d d)) 3)
;; = ((maketimesfour (maketimesfour (d d))) 3)
;; = ((maketimesfour (maketimesfour (maketimesfour (d d)))) 3)
;; = ...
;;
;; i.e. (f (d d)) expands indefinitely under call-by-value.
