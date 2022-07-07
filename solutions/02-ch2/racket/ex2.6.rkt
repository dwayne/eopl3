#lang racket

(module+ test
  (require rackunit))

;; Invent at least 3 different representations of the environment interface
;; and implement them.

;; a. The empty environment is represented as the empty list, and
;;    the non-empty environment is represented as
;;    (var_1 val_1 var_2 val_2 ... var_n val_n).

(define (a-empty-env)
  '())

(define (a-extend-env var val env)
  (cons var (cons val env)))

(define (a-apply-env env search-var)
  (if (null? env)
      (error 'a-apply-env "No binding for ~s" search-var)
      (let ([saved-var (car env)])
        (if (symbol=? search-var (car env))
            (cadr env)
            (a-apply-env (cddr env) search-var)))))

(module+ test
  (let ([env (a-extend-env
              'd 6
              (a-extend-env
               'y 8
               (a-extend-env
                'x 7
                (a-extend-env
                 'y 14
                 (a-empty-env)))))])

    (check-eq? (a-apply-env env 'd) 6)
    (check-eq? (a-apply-env env 'y) 8)
    (check-eq? (a-apply-env env 'x) 7)

    (check-exn #rx"No binding for a" (lambda () (a-apply-env env 'a)))))

;; b. Using a hash table.

(define (b-empty-env)
  (hash))

(define (b-extend-env var val env)
  (hash-set env var val))

(define (b-apply-env env search-var)
  (hash-ref env
            search-var
            (lambda () (error 'b-apply-env "No binding for ~s" search-var))))

(module+ test
  (let ([env (b-extend-env
              'd 6
              (b-extend-env
               'y 8
               (b-extend-env
                'x 7
                (b-extend-env
                 'y 14
                 (b-empty-env)))))])

    (check-eq? (b-apply-env env 'd) 6)
    (check-eq? (b-apply-env env 'y) 8)
    (check-eq? (b-apply-env env 'x) 7)

    (check-exn #rx"No binding for a" (lambda () (b-apply-env env 'a)))))

;; c. The empty environment is represented as the empty list, and
;;    the non-empty environment is represented as a function.

(define (c-empty-env)
  '())

(define (c-extend-env saved-var val env)
  (lambda (search-var)
    (if (symbol=? search-var saved-var)
        val
        (c-apply-env env search-var))))

(define (c-apply-env env search-var)
  (if (null? env)
      (error 'c-apply-env "No binding for ~s" search-var)
      (env search-var)))

(module+ test
  (let ([env (c-extend-env
              'd 6
              (c-extend-env
               'y 8
               (c-extend-env
                'x 7
                (c-extend-env
                 'y 14
                 (c-empty-env)))))])

    (check-eq? (c-apply-env env 'd) 6)
    (check-eq? (c-apply-env env 'y) 8)
    (check-eq? (c-apply-env env 'x) 7)

    (check-exn #rx"No binding for a" (lambda () (c-apply-env env 'a)))))
