#lang eopl

(require "./parser.rkt")

(provide

 ;; Build
 empty-env
 extend-env
 extend-env-rec

 ;; Query
 env?
 apply-env

 identifier? identifier=?)

(define-datatype env env?
  [empty]
  [extend
   (var identifier?)
   (val any?)
   (saved-env env?)]
  [extend-rec
   (p-name identifier?)
   (box vector?)
   (saved-env env?)])

(define (empty-env)
  (empty))

(define (extend-env var val env)
  (extend var val env))

(define (extend-env-rec proc-name var proc-body env construct-proc-val)
  (let ([box (make-vector 1)])
    (let ([env-rec (extend-rec proc-name box env)])
      (vector-set! box 0 (construct-proc-val var proc-body env-rec))
      env-rec)))

(define (apply-env env1 search-var)
  (cases env env1
    [empty ()
           (eopl:error 'apply-env "No binding for ~s" search-var)]

    [extend (saved-var saved-val saved-env)
            (if (identifier=? search-var saved-var)
                saved-val
                (apply-env saved-env search-var))]

    [extend-rec (p-name box saved-env)
                (if (identifier=? search-var p-name)
                    (vector-ref box 0)
                    (apply-env saved-env search-var))]))

(define identifier? symbol?)

(define identifier=? eq?)

(define (any? v) #t)
