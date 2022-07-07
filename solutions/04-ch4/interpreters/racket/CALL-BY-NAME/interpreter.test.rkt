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

;; Multideclaration letrec

(check-equal?
 (run
  #<<CODE
letrec f(x) = (g x)
       g(y) = y
in (f 2)
CODE
  )
 (num-val 2))

;; IMPLICIT-REF tests

(check-equal?
 (run
  #<<CODE
let x = 0
in letrec even(dummy)
          = if zero?(x)
            then 1
            else begin
                   set x = -(x, 1);
                   (odd 888)
                 end
          odd(dummy)
          = if zero?(x)
            then 0
            else begin
                   set x = -(x, 1);
                   (even 888)
                 end
   in begin set x = 13; (odd 888) end
CODE
  )
 (num-val 1))

(check-equal?
 (run
  #<<CODE
let g = let counter = 0
        in proc(dummy)
             begin
               set counter = -(counter, -(0, 1));
               counter
             end
in let a = (g 11)
   in let b = (g 11)
      in -(a, b)
CODE
  )
 (num-val -1))

;; CALL-BY-REFERENCE tests

(check-equal?
 (run
  #<<CODE
let p = proc (x) set x = 4
in let a = 3
   in begin (p a); a end
CODE
  )
 (num-val 4))

(check-equal?
 (run
  #<<CODE
let f = proc (x) set x = 44
in let g = proc (y) (f y)
   in let z = 55
      in begin (g z); z end
CODE
  )
 (num-val 44))

(check-equal?
 (run
  #<<CODE
let swap = proc (x) proc (y)
             let temp = x
             in begin
                  set x = y;
                  set y = temp
                end
in let a = 33
   in let b = 44
      in begin
           ((swap a) b);
           -(a, b)
         end
CODE
  )
 (num-val 11))

(check-equal?
 (run
  #<<CODE
let b = 3
in let p = proc (x) proc (y)
             begin
               set x = 4;
               y
             end
   in ((p b) b)
CODE
  )
 (num-val 4))

;; CALL-BY-NAME tests

(check-equal?
 (run
  #<<CODE
letrec loop(x) = (loop x)
in let f = proc (z) 11
   in (f (loop 0))
CODE
  )
 (num-val 11))

;; Exercise 4.39

(check-equal?
 (run
  #<<CODE
let double = proc (x) -(x, -(0, x))
in let a = 2
   in begin
        (double begin set a = -(a, 1); 8 end);
        a
      end
CODE
  )
 (num-val 0))
