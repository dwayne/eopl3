#lang racket

(require "./parser.rkt")

(require rackunit)

(check-equal?
 (parse "1")
 (a-program (const-exp 1)))

(check-equal?
 (parse "x")
 (a-program (var-exp 'x)))

(check-equal?
 (parse "-(5, y)")
 (a-program (diff-exp (const-exp 5) (var-exp 'y))))

(check-equal?
 (parse "zero?(z)")
 (a-program (zero?-exp (var-exp 'z))))

(check-equal?
 (parse "if zero?(2) then 0 else 1")
 (a-program (if-exp (zero?-exp (const-exp 2))
                    (const-exp 0)
                    (const-exp 1))))

(check-equal?
 (parse "emptylist")
 (a-program (emptylist-exp)))

(check-equal?
 (parse "cons(5, emptylist)")
 (a-program (cons-exp (const-exp 5)
                      (emptylist-exp))))

(check-equal?
 (parse "car(cons(x, emptylist))")
 (a-program (car-exp (cons-exp (var-exp 'x)
                               (emptylist-exp)))))

(check-equal?
 (parse "cdr(cons(x, emptylist))")
 (a-program (cdr-exp (cons-exp (var-exp 'x)
                               (emptylist-exp)))))

(check-equal?
 (parse "list()")
 (a-program (list-exp '())))

(check-equal?
 (parse "list(1)")
 (a-program (list-exp (list (const-exp 1)))))

(check-equal?
 (parse "list(1, if zero?(i) then x else -(0, x))")
 (a-program (list-exp (list (const-exp 1)
                            (if-exp (zero?-exp (var-exp 'i))
                                    (var-exp 'x)
                                    (diff-exp (const-exp 0)
                                              (var-exp 'x)))))))

(check-equal?
 (parse "if null?(emptylist) then 0 else 1")
 (a-program (if-exp (null?-exp (emptylist-exp))
                    (const-exp 0)
                    (const-exp 1))))

(check-equal?
 (parse "let n=10 in -(n, 1)")
 (a-program (let-exp (list 'n)
                     (list (const-exp 10))
                     (diff-exp (var-exp 'n) (const-exp 1)))))

(check-equal?
 (parse
  #<<LET
let x = 30
in let x = -(x,1)
       y = -(x,2)
   in -(x,y)
LET
  )
 (a-program (let-exp (list 'x)
                     (list (const-exp 30))
                     (let-exp (list 'x 'y)
                              (list (diff-exp (var-exp 'x) (const-exp 1))
                                    (diff-exp (var-exp 'x) (const-exp 2)))
                              (diff-exp (var-exp 'x)
                                        (var-exp 'y))))))

(check-equal?
 (parse "proc (x) -(x, 1)")
 (a-program (proc-exp '(x) (diff-exp (var-exp 'x)
                                     (const-exp 1)))))

(check-equal?
 (parse "letrec f(x) = a in b")
 (a-program (letrec-exp 'f
                        'x
                        (var-exp 'a)
                        (var-exp 'b))))

(check-equal?
 (parse "(f x)")
 (a-program (call-exp (var-exp 'f)
                      (list (var-exp 'x)))))

;; Multiargument procedure tests

(check-equal?
 (parse "proc () 1")
 (a-program (proc-exp '() (const-exp 1))))

(check-equal?
 (parse "proc (a, b, c) b")
 (a-program (proc-exp '(a b c) (var-exp 'b))))

(check-equal?
 (parse "(f)")
 (a-program (call-exp (var-exp 'f) '())))

(check-equal?
 (parse "(f 1 2 3)")
 (a-program (call-exp (var-exp 'f) (list (const-exp 1)
                                         (const-exp 2)
                                         (const-exp 3)))))

;; Test try and raise

(check-equal?
 (parse "try 1 catch (x) 2")
 (a-program (try-exp (const-exp 1) 'x (const-exp 2))))

(check-equal?
 (parse "raise 1")
 (a-program (raise-exp (const-exp 1))))

;; Test letcc

(check-equal?
 (parse "letcc x in y")
 (a-program (letcc-exp 'x (var-exp 'y))))
