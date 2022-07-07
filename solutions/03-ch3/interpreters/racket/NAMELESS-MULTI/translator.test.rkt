#lang racket

(require "./parser.rkt")
(require "./translator.rkt")

(require rackunit)

(check-equal?
 (translate
  (parse
   #<<CODE
let x = 37
in proc (y)
     let z = -(y, x)
     in -(x, y)
CODE
   ))
 (a-program
  (nameless-let-exp
   (list (const-exp 37))
   (nameless-proc-exp
    (nameless-let-exp
     (list
      (diff-exp
       (nameless-var-exp 0 0)
       (nameless-var-exp 1 0)))
     (diff-exp
      (nameless-var-exp 2 0)
      (nameless-var-exp 1 0)))))))

(check-equal?
 (translate
  (parse
   #<<CODE
letrec sum(n) = if zero?(n)
                then 0
                else -(n, -(0, (sum -(n, 1))))
in (sum 5)
CODE
   ))
 (a-program
  (nameless-letrec-exp
   (list (if-exp (zero?-exp (nameless-var-exp 0 0))
                 (const-exp 0)
                 (diff-exp (nameless-var-exp 0 0)
                           (diff-exp (const-exp 0)
                                     (call-exp (nameless-var-exp 1 0)
                                               (list (diff-exp (nameless-var-exp 0 0)
                                                               (const-exp 1))))))))
   (call-exp (nameless-var-exp 0 0) (list (const-exp 5))))))

(check-equal?
 (translate
  (parse
   #<<CODE
letrec
  even(x) = if zero?(x) then 1 else (odd -(x, 1))
  odd(x) = if zero?(x) then 0 else (even -(x, 1))
in (odd 13)
CODE
   ))
 (a-program
  (nameless-letrec-exp
   (list
    (if-exp (zero?-exp (nameless-var-exp 0 0))
            (const-exp 1)
            (call-exp (nameless-var-exp 1 1)
                      (list (diff-exp (nameless-var-exp 0 0)
                                      (const-exp 1)))))
    (if-exp (zero?-exp (nameless-var-exp 0 0))
            (const-exp 0)
            (call-exp (nameless-var-exp 1 0)
                      (list (diff-exp (nameless-var-exp 0 0)
                                      (const-exp 1))))))
   (call-exp (nameless-var-exp 0 1)
             (list (const-exp 13))))))

(check-equal?
 (translate
  (parse
   #<<CODE
letrec
  f() = (fact 5)
  fact(n) = if zero?(n) then 1 else (mult n (fact -(n, 1)))
  mult(a, b) = if zero?(b) then 0 else (add (mult a -(b, 1)) a)
  add(a, b) = -(a, -(0, b))
in (f)
CODE
   ))
 (a-program
  (nameless-letrec-exp
   (list
    (call-exp (nameless-var-exp 1 1)
              (list (const-exp 5)))
    (if-exp (zero?-exp (nameless-var-exp 0 0))
            (const-exp 1)
            (call-exp (nameless-var-exp 1 2)
                      (list (nameless-var-exp 0 0)
                            (call-exp (nameless-var-exp 1 1)
                                      (list (diff-exp (nameless-var-exp 0 0)
                                                      (const-exp 1)))))))
    (if-exp (zero?-exp (nameless-var-exp 0 1))
                       (const-exp 0)
                       (call-exp (nameless-var-exp 1 3)
                                 (list (call-exp (nameless-var-exp 1 2)
                                                 (list (nameless-var-exp 0 0)
                                                       (diff-exp (nameless-var-exp 0 1)
                                                                 (const-exp 1))))
                                       (nameless-var-exp 0 0))))
    (diff-exp (nameless-var-exp 0 0)
              (diff-exp (const-exp 0)
                        (nameless-var-exp 0 1))))
   (call-exp (nameless-var-exp 0 0)
             '()))))
