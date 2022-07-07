#lang racket

(require "./env.rkt")

(require rackunit)

(let ([env (extend-env
            'd 6
            (extend-env
             'y 8
             (extend-env
              'x 7
              (extend-env
               'y 14
               (empty-env)))))])

  (check-eq? (apply-env env 'd #f) 6)
  (check-eq? (apply-env env 'y #f) 8)
  (check-eq? (apply-env env 'x #f) 7)

  (check-exn #rx"No binding for a" (lambda () (apply-env env 'a #f))))

(let ([env (extend-env* '(d y x y) '(6 8 7 14) (empty-env))])
  (check-eq? (apply-env env 'd #f) 6)
  (check-eq? (apply-env env 'y #f) 14)
  (check-eq? (apply-env env 'x #f) 7)

  (check-exn #rx"No binding for a" (lambda () (apply-env env 'a #f))))