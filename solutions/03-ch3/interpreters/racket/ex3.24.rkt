#lang racket

(require "./PROC/interpreter.rkt")

(module+ test
  (require rackunit)

  (check-equal?
   (run
    #<<CODE
let makeeven = proc (f)
                 proc (g)
                   proc (x)
                     if zero?(x)
                     then 1
                     else (((g g) f) -(x, 1))
in let makeodd = proc (f)
                   proc (g)
                     proc (x)
                       if zero?(x)
                       then 0
                       else (((g g) f) -(x, 1))
   in let even = proc (x) (((makeeven makeeven) makeodd) x)
      in let odd = proc (x) (((makeodd makeodd) makeeven) x)
         in list((odd 12), (odd 13), (even 14), (even 15))
CODE
    )
   (list-val (list (num-val 0)
                   (num-val 1)
                   (num-val 1)
                   (num-val 0)))))
