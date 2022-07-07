#lang racket

;; occurs-free? works with either representation.
;;
;(require "./ex2.15.rkt")
;(require "./ex2.16.rkt")
(require "./ex2.17-1.rkt")
;(require "./2.17-2.rkt")

(define (occurs-free? var exp)
  (cond
    [(var-exp? exp) (symbol=? var (var-exp->var exp))]
    [(lambda-exp? exp)
     (and
      (not (symbol=? var (lambda-exp->bound-var exp)))
      (occurs-free? var (lambda-exp->body exp)))]
    [(app-exp? exp)
     (or
      (occurs-free? var (app-exp->rator exp))
      (occurs-free? var (app-exp->rand exp)))]))

(module+ test
  (require rackunit)

  (check-true (occurs-free? 'x (var-exp 'x)))

  (check-true
   (occurs-free? 'x (lambda-exp 'y (app-exp (var-exp 'x) (var-exp 'y)))))

  (check-true
   (occurs-free? 'x (app-exp
                     (lambda-exp 'x (var-exp 'x))
                     (app-exp (var-exp 'x) (var-exp 'y)))))

  (check-true
   (occurs-free? 'x (lambda-exp 'y
                                (lambda-exp 'z
                                            (app-exp
                                             (var-exp 'x)
                                             (app-exp (var-exp 'y) (var-exp 'z)))))))

  (check-false
   (occurs-free? 'x (var-exp 'y)))

  (check-false
   (occurs-free? 'x (lambda-exp 'x (app-exp (var-exp 'x) (var-exp 'y))))))
