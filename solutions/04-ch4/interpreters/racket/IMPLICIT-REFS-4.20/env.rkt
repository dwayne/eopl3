#lang eopl

(require "./parser.rkt")
(require "./store.rkt")

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
   (p-names (list-of identifier?))
   (b-vars (list-of identifier?))
   (p-bodies (list-of expression?))
   (saved-env env?)])

(define (empty-env)
  (empty))

(define (extend-env var ref env)
  (extend var ref env))

(define (extend-env-rec proc-names vars proc-bodies env)
  (extend-rec proc-names vars proc-bodies env))

(define (apply-env env1 search-var construct-proc-val)
  (define (find-rec-proc p-names b-vars p-bodies)
    (if (null? p-names)
        #f
        (if (identifier=? search-var (car p-names))
            (list (car b-vars) (car p-bodies))
            (find-rec-proc (cdr p-names) (cdr b-vars) (cdr p-bodies)))))
  (cases env env1
    [empty ()
           (eopl:error 'apply-env "No binding for ~s" search-var)]

    [extend (saved-var saved-val saved-env)
            (if (identifier=? search-var saved-var)
                saved-val
                (apply-env saved-env search-var construct-proc-val))]

    [extend-rec (p-names b-vars p-bodies saved-env)
                (let ([result (find-rec-proc p-names b-vars p-bodies)])
                  (if result
                      (let ([b-var (car result)]
                            [p-body (cadr result)])
                        (construct-proc-val b-var p-body env1))
                      (apply-env saved-env search-var construct-proc-val)))]))

(define identifier? symbol?)

(define identifier=? eq?)

(define (any? v) #t)
