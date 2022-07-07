#lang racket

(require "./senv.rkt")

(require rackunit)

(let ([senv (extend-senv '(d y)
                         (extend-senv '(x y)
                                      (empty-senv)))])
  (check-equal? (apply-senv senv 'd) (cons 0 0))
  (check-equal? (apply-senv senv 'y) (cons 0 1))
  (check-equal? (apply-senv senv 'x) (cons 1 0))

  (check-exn #rx"No binding for a" (lambda () (apply-senv senv 'a))))
