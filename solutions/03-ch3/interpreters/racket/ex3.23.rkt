#lang racket

(require "./PROC/interpreter.rkt")

(module+ test
  (require rackunit)

  (check-equal?
   (run
    #<<CODE
let makemult = proc (maker)
                 proc (x)
                   if zero?(x)
                   then 0
                   else -(((maker maker) -(x, 1)), -(0, 4))
in let timesfour = proc (x) ((makemult makemult) x)
   in (timesfour 3)
CODE
    )
   (num-val 12))

  (check-equal?
   (run
    #<<CODE
let maketimes = proc (maker)
                  proc (n)
                    proc (a)
                      if zero?(n)
                      then 0
                      else -((((maker maker) -(n, 1)) a), -(0, a))
in let times = proc (n) proc (a) (((maketimes maketimes) n) a)
   in ((times 4) 3)
CODE
    )
   (num-val 12))

  (check-equal?
   (run
    #<<CODE
let maketimes = proc (maker)
                  proc (n)
                    proc (a)
                      if zero?(n)
                      then 0
                      else -((((maker maker) -(n, 1)) a), -(0, a))
in let times = proc (n) proc (a) (((maketimes maketimes) n) a)
   in let makefact = proc (maker)
                       proc (n)
                         if zero?(n)
                         then 1
                         else ((times n) ((maker maker) -(n, 1)))
      in let fact = proc (n) ((makefact makefact) n)
         in (fact 5)
CODE
    )
   (num-val 120)))
