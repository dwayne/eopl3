#lang racket

;; Exercise 2.11
;;
;; Implement the environment interface using the ribcage representation.

(define (empty-env)
  '())

(define (empty-env? env)
  (null? env))

(define (extend-env var val env)
  (cons (cons (list var) (list val)) env))

(define (extend-env* vars vals env)
  ;; assumes (= (length vars) (length vals))
  (cons (cons vars vals) env))

(define (apply-env env search-var)
  (if (empty-env? env)
      (error 'apply-env "No binding for ~s" search-var)
      (let ([saved-val (find search-var (caar env) (cdar env))])
        (if saved-val
            saved-val
            (apply-env (cdr env) search-var)))))

(define (has-binding? env search-var)
  (if (empty-env? env)
      #f
      (let ([saved-val (find search-var (caar env) (cdar env))])
        (if saved-val
            #t
            (has-binding? (cdr env) search-var)))))

(define (find search-var vars vals)
  (if (null? vars)
      #f
      (if (symbol=? search-var (car vars))
          (car vals)
          (find search-var (cdr vars) (cdr vals)))))

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

  (let ([env (extend-env* '(d y x y) '(6 8 7 14) (empty-env))])
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
