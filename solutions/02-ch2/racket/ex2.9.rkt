#lang racket

;; Exercise 2.9
;;
;; Add an observer called has-binding? and
;; implement it using the a-list representation.

(define (empty-env)
  '())

(define (empty-env? env)
  (null? env))

(define (extend-env var val env)
  (cons (cons var val) env))

(define (apply-env env search-var)
  (if (empty-env? env)
      (error 'apply-env "No binding for ~s" search-var)
      (let ([saved-var (caar env)])
        (if (symbol=? search-var saved-var)
            (cdar env)
            (apply-env (cdr env) search-var)))))

(define (has-binding? env search-var)
  (if (empty-env? env)
      #f
      (let ([saved-var (caar env)])
        (if (symbol=? search-var saved-var)
            #t
            (has-binding? (cdr env) search-var)))))

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

    (check-eq? (apply-env env 'd) 6)
    (check-eq? (apply-env env 'y) 8)
    (check-eq? (apply-env env 'x) 7)

    (check-exn #rx"No binding for a" (lambda () (apply-env env 'a)))

    (check-false (empty-env? env))

    (check-true (has-binding? env 'd))
    (check-true (has-binding? env 'y))
    (check-true (has-binding? env 'x))
    (check-false (has-binding? env 'a)))

  (check-true (empty-env? (empty-env))))
