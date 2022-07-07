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

(check-equal?
 (run
  #<<CODE
letrec double(x) = if zero?(x)
                   then 0
                   else -((double -(x, 1)), -(0, 2))
in (double 6)
CODE
  )
 (num-val 12))

;; List tests

(check-equal?
 (run "emptylist")
 (list-val '()))

(check-equal?
 (run "cons(5, emptylist)")
 (list-val (list (num-val 5))))

(check-equal?
 (run "car(cons(x, emptylist))")
 (num-val 10))

(check-equal?
 (run "cdr(cons(x, emptylist))")
 (list-val '()))

(check-equal?
 (run "if null?(emptylist) then 0 else 1")
 (num-val 0))

(check-equal?
 (run
  #<<LET
let x = 4
in cons(x,
        cons(cons(-(x, 1),
                  emptylist),
             emptylist))
LET
  )
 ; (4 (3))
 (list-val (list (num-val 4)
                 (list-val (list (num-val 3))))))

(check-equal?
 (run "list()")
 (list-val '()))

(check-equal?
 (run "list(1)")
 (list-val (list (num-val 1))))

(check-equal?
 (run
  #<<LET
let x = 4
in list(x, -(x, 1), -(x, 3))
LET
  )
 ; (4 3 1)
 (list-val (list (num-val 4)
                 (num-val 3)
                 (num-val 1))))

(check-equal?
 (run "car(cdr(list(1,if zero?(0) then 2 else 4,3)))")
 (num-val 2))

;; Test multideclaration let

(check-equal?
 (run
  #<<LET
let x = 30
in let x = -(x,1)
       y = -(x,2)
   in -(x,y)
LET
  )
 (num-val 1))

;; Test multiargument procedures

(check-equal?
 (run "let f = proc () 1 in (f)")
 (num-val 1))

(check-equal?
 (run "let g = proc (x, y, z) -(z, -(y, x)) in (g 1 2 3)")
 (num-val 2))

;; Test exception-handling

(check-equal?
 (run
  #<<CODE
let index =
  proc (n)
    letrec inner (lst)
      = if null?(lst)
        then raise 99
        else if zero?(-(car(lst), n))
             then 0
             else -((inner cdr(lst)), -(0, 1))
    in proc (lst)
         try (inner lst)
         catch (x) -(0, 1)
in ((index 5) list(2, 3))
CODE
  )
 (num-val -1))

(check-exn
 #rx"Uncaught exception: .*1"
 (lambda () (run "raise 1")))

;; Test letcc

(check-equal?
 (run
  #<<CODE
-(100, letcc c in -(1, -(2, -(3, (c 1)))))
CODE
  )
 (num-val 99))
