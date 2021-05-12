#lang racket

(require "./interpreter.rkt")

(require rackunit)

(check-equal?
 (run "print 5")
 (list (num-val 5)))

(check-equal?
 (run "print x")
 (list (num-val 10)))

(check-equal?
 (run "print zero?(i)")
 (list (bool-val #f)))

(check-equal?
 (run "print zero?(-(i, 1))")
 (list (bool-val #t)))

(check-equal?
 (run "print -(55, -(x, 11))")
 (list (num-val 56)))

(check-equal?
 (run "print -(-(x, 3), -(v, i))")
 (list (num-val 3)))

(check-equal?
 (run
  #<<CODE
print let x = 33
in let y = 22
   in if zero?(-(x, 11)) then -(y, 2) else -(y, 4)
CODE
  )
 (list (num-val 18)))

(check-equal?
 (run "print let x = 5 in -(x, 3)")
 (list (num-val 2)))

(check-equal?
 (run
  #<<CODE
print let z = 5
in let x = 3
   in let y = -(x, 1)
      in let x = 4 in -(z, -(x, y))
CODE
  )
 (list (num-val 3)))

(check-equal?
 (run
  #<<CODE
print let x = 7
in let y = 2
   in let y = let x = -(x, 1)
              in -(x, y)
      in -(-(x, 8), y)
CODE
  )
 (list (num-val -5)))

(check-equal?
 (run
  #<<CODE
print let f = proc (x) -(x, 11)
in (f (f 77))
CODE
  )
 (list (num-val 55)))

(check-equal?
 (run
  #<<CODE
print (proc (f) (f (f 77))
 proc (x) -(x, 11))
CODE
  )
 (list (num-val 55)))

(check-equal?
 (run
  #<<CODE
print let x = 200
in let f = proc (z) -(z, x)
   in let x = 100
      in let g = proc (z) -(z, x)
         in -((f 1), (g 1))
CODE
  )
 (list (num-val -100)))

;; Exercise 3.20
;;
;; Write a curried procedure that takes two arguments and returns their sum.

(check-equal?
 (run
  #<<CODE
print let f = proc (x) proc (y) -(x, -(0, y))
in let a = ((f 1) 2)
   in let b = ((f a) 3)
      in let c = ((f b) 4)
         in let d = ((f c) 5) in d
CODE
  )
 (list (num-val 15)))

(check-equal?
 (run
  #<<CODE
print letrec double(x) = if zero?(x)
                   then 0
                   else -((double -(x, 1)), -(0, 2))
in (double 6)
CODE
  )
 (list (num-val 12)))

;; Multideclaration letrec

(check-equal?
 (run
  #<<CODE
print letrec f(x) = (g x)
       g(y) = y
in (f 2)
CODE
  )
 (list (num-val 2)))

;; IMPLICIT-REF tests

(check-equal?
 (run
  #<<CODE
print let x = 0
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
 (list (num-val 1)))

(check-equal?
 (run
  #<<CODE
print let g = let counter = 0
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
 (list (num-val -1)))

;; STATEMENTS-4.22 tests

(check-equal?
 (run
  #<<CODE
var x,y; {x = 3; y = 4; print +(x,y)}
CODE
  )
 (list (num-val 7)))

(check-equal?
 (run
  #<<CODE
var x,y,z; { x = 3;
             y = 4;
             z = 0;
             while not(zero?(x))
               { z = +(z,y); x = -(x,1) };
             print z }
CODE
  )
 (list (num-val 12)))

(check-equal?
 (run
  #<<CODE
var x; { x = 3;
         print x;
         var x; { x = 4; print x };
         print x }
CODE
  )
 (list (num-val 3) (num-val 4) (num-val 3)))

(check-equal?
 (run
  #<<CODE
var f, x; { f = proc(x) proc(y) *(x, y);
            x = 3;
            print ((f 4) x) }
CODE
  )
 (list (num-val 12)))
