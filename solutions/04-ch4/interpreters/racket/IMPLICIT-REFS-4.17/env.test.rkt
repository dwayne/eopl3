#lang racket

(require "./env.rkt")

(require rackunit)

(let ([env (extend-env '(d y) '(6 8)
                       (extend-env '(x y) '(7 14)
                                   (empty-env)))])

  (check-eq? (apply-env env 'd #f) 6)
  (check-eq? (apply-env env 'y #f) 8)
  (check-eq? (apply-env env 'x #f) 7)

  (check-exn #rx"No binding for a" (lambda () (apply-env env 'a #f))))
