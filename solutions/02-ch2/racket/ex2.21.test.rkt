#lang racket

(require "./ex2.21.rkt")

(module+ test
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

    (check-true (has-binding? env 'd))
    (check-true (has-binding? env 'y))
    (check-true (has-binding? env 'x))
    (check-false (has-binding? env 'a))))
