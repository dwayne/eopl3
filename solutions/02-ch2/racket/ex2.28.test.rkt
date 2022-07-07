#lang racket

(require "./ex2.28.rkt")

(require rackunit)

(check-equal?
 (unparse-lc-exp (var-exp 'x))
 "x")

(check-equal?
 (unparse-lc-exp (lambda-exp 'x (var-exp 'x)))
 "(lambda (x) x)")

(check-equal?
 (unparse-lc-exp (app-exp (var-exp 'x) (var-exp 'y)))
 "(x y)")

(check-equal?
 (unparse-lc-exp (app-exp (lambda-exp 'a (app-exp (var-exp 'a)
                                                  (var-exp 'b)))
                          (var-exp 'c)))
 "((lambda (a) (a b)) c)")

(check-equal?
 (unparse-lc-exp
  (lambda-exp 'x
              (lambda-exp 'y
                          (app-exp (lambda-exp 'x
                                               (app-exp (var-exp 'x)
                                                        (var-exp 'y)))
                                   (var-exp 'x)))))
 "(lambda (x) (lambda (y) ((lambda (x) (x y)) x)))")
