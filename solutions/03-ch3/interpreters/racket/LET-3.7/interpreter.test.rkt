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
  #<<LET
let x = 33
in let y = 22
   in if zero?(-(x, 11)) then -(y, 2) else -(y, 4)
LET
  )
 (num-val 18))

(check-equal?
 (run "let x = 5 in -(x, 3)")
 (num-val 2))

(check-equal?
 (run
  #<<LET
let z = 5
in let x = 3
   in let y = -(x, 1)
      in let x = 4 in -(z, -(x, y))
LET
  )
 (num-val 3))

(check-equal?
 (run
  #<<LET
let x = 7
in let y = 2
   in let y = let x = -(x, 1)
              in -(x, y)
      in -(-(x, 8), y)
LET
  )
 (num-val -5))

(check-equal?
 (run "minus(-(minus(5), 9))")
 (num-val 14))

(check-equal?
 (run "add(6, 2)")
 (num-val 8))

(check-equal?
 (run "mul(6, 2)")
 (num-val 12))

(check-equal?
 (run "div(6, 2)")
 (num-val 3))

(check-equal?
 (run "div(6, 4)")
 (num-val 1))

(check-exn
 #rx"division by 0 is undefined"
 (lambda () (run "div(6, 0)")))
