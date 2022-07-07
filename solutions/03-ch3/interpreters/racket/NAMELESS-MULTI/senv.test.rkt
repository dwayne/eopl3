#lang racket

(require "./senv.rkt")

(require rackunit)

(let ([senv (extend-senv
             '(d)
             (extend-senv
              '(y)
              (extend-senv
               '(x)
               (extend-senv
                '(y)
                (empty-senv)))))])

  (check-equal? (apply-senv senv 'd) (cons 0 0))
  (check-equal? (apply-senv senv 'y) (cons 1 0))
  (check-equal? (apply-senv senv 'x) (cons 2 0))

  (check-exn #rx"No binding for a" (lambda () (apply-senv senv 'a))))
