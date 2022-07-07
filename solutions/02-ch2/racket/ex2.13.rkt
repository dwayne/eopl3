#lang racket

(define (empty-env)
  (list
   (lambda (search-var)
     (error 'apply-env "No binding for ~s" search-var))
   (lambda () #t)))

(define (extend-env saved-var saved-val saved-env)
  (list
   (lambda (search-var)
     (if (symbol=? search-var saved-var)
         saved-val
         (apply-env saved-env search-var)))
   (lambda () #f)))

(define (apply-env env search-var)
  ((car env) search-var))

(define (empty-env? env)
  ((cadr env)))

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

    (check-false (empty-env? env)))

  (check-true (empty-env? (empty-env))))