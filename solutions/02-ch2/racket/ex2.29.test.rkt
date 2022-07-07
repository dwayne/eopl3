#lang racket

(require "./ex2.29.rkt")

(require rackunit)

(check-equal?
 (parse-expression 'x)
 (var-exp 'x))

(check-equal?
 (parse-expression '(lambda (x) x))
 (lambda-exp (list 'x) (var-exp 'x)))

(check-equal?
 (parse-expression '(x y))
 (app-exp (var-exp 'x) (list (var-exp 'y))))

(check-equal?
 (parse-expression '(lambda (x y) (add x y)))
 (lambda-exp (list 'x 'y)
             (app-exp (var-exp 'add)
                      (list (var-exp 'x)
                            (var-exp 'y)))))
