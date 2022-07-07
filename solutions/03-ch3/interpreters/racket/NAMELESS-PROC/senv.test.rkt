#lang racket

(require "./senv.rkt")

(require rackunit)

(let ([senv (extend-senv
             'd
             (extend-senv
              'y
              (extend-senv
               'x
               (extend-senv
                'y
                (empty-senv)))))])

  (check-eq? (apply-senv senv 'd) 0)
  (check-eq? (apply-senv senv 'y) 1)
  (check-eq? (apply-senv senv 'x) 2)

  (check-exn #rx"No binding for a" (lambda () (apply-senv senv 'a))))
