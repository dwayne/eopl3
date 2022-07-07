#lang racket

(require "./ex2.30.rkt")

(require rackunit)

(check-equal?
 (parse-expression 'x)
 (var-exp 'x))

(check-equal?
 (parse-expression '(lambda (x) x))
 (lambda-exp 'x (var-exp 'x)))

(check-equal?
 (parse-expression '(x y))
 (app-exp (var-exp 'x) (var-exp 'y)))

(check-exn
 #rx"Invalid concrete syntax: \\(a b c\\)"
 (lambda ()
   (parse-expression '(a b c))))

(check-exn
 #rx"Invalid concrete syntax: \\(lambda\\)"
 (lambda ()
   (parse-expression '(lambda))))

(check-exn
 #rx"Invalid concrete syntax: lambda"
 (lambda ()
   (parse-expression '(lambda (x) (x lambda)))))
