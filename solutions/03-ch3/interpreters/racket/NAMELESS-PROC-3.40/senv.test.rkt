#lang racket

(require "./senv.rkt")

(require rackunit)

(let ([senv (extend-senv
             'd
             (extend-senv
              'y
              (extend-senv-rec
               'f
               (extend-senv
                'x
                (extend-senv
                 'y
                 (empty-senv))))))])

  (check-equal? (apply-senv senv 'd) (cons 0 #f))
  (check-equal? (apply-senv senv 'y) (cons 1 #f))
  (check-equal? (apply-senv senv 'f) (cons 2 #t))
  (check-equal? (apply-senv senv 'x) (cons 3 #f))

  (check-exn #rx"No binding for a" (lambda () (apply-senv senv 'a))))
