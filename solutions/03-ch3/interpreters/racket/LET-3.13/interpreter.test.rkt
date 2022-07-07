#lang racket

(require "./interpreter.rkt")

(require rackunit)

(check-equal?
 (run "5")
 5)

(check-equal?
 (run "x")
 10)

(check-equal?
 (run "zero?(i)")
 0)

(check-equal?
 (run "zero?(-(i, 1))")
 1)

(check-equal?
 (run "-(55, -(x, 11))")
 56)

(check-equal?
 (run "-(-(x, 3), -(v, i))")
 3)

(check-equal?
 (run
  #<<LET
let x = 33
in let y = 22
   in if zero?(-(x, 11)) then -(y, 2) else -(y, 4)
LET
  )
 18)

(check-equal?
 (run "let x = 5 in -(x, 3)")
 2)

(check-equal?
 (run
  #<<LET
let z = 5
in let x = 3
   in let y = -(x, 1)
      in let x = 4 in -(z, -(x, y))
LET
  )
 3)

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
 -5)

(check-equal?
 (run "minus(-(minus(5), 9))")
 14)

(check-equal?
 (run "add(6, 2)")
 8)

(check-equal?
 (run "mul(6, 2)")
 12)

(check-equal?
 (run "div(6, 2)")
 3)

(check-equal?
 (run "div(6, 4)")
 1)

(check-exn
 #rx"division by 0 is undefined"
 (lambda () (run "div(6, 0)")))

(check-equal?
 (run "equal?(1, 2)")
 0)

(check-equal?
 (run "greater?(1, 2)")
 0)

(check-equal?
 (run "less?(1, 2)")
 1)
