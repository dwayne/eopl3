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
   (vars (list-of identifier?))
   (vals (list-of any?))
   (saved-env env?)]
  [extend-rec
   (p-names (list-of identifier?))
   (p-bound-vars (list-of (list-of identifier?)))
   (p-bodies (list-of expression?))
   (saved-env env?)])

(define (empty-env)
  (empty))

(define (extend-env vars vals env)
  (extend vars vals env))

(define (extend-env-rec proc-names proc-bound-vars proc-bodies env)
  (extend-rec proc-names proc-bound-vars proc-bodies env))

(define (apply-env env1 search-var construct-proc-val)
  (define (find-val saved-vars saved-vals)
    (if (null? saved-vars)
        #f
        (if (identifier=? search-var (car saved-vars))
            (car saved-vals)
            (find-val (cdr saved-vars) (cdr saved-vals)))))
  (define (find-rec-proc p-names p-bound-vars p-bodies)
    (if (null? p-names)
        #f
        (if (identifier=? search-var (car p-names))
            (list (car p-bound-vars) (car p-bodies))
            (find-rec-proc (cdr p-names) (cdr p-bound-vars) (cdr p-bodies)))))
  (cases env env1
    [empty ()
           (eopl:error 'apply-env "No binding for ~s" search-var)]

    [extend (saved-vars saved-vals saved-env)
            (let ([saved-val (find-val saved-vars saved-vals)])
              (if saved-val
                  saved-val
                  (apply-env saved-env search-var construct-proc-val)))]

    [extend-rec (p-names p-bound-vars p-bodies saved-env)
                (let ([result (find-rec-proc p-names p-bound-vars p-bodies)])
                  (if result
                      (let ([bound-vars (car result)]
                            [p-body (cadr result)])
                        (construct-proc-val bound-vars p-body env1))
                      (apply-env saved-env search-var construct-proc-val)))]))

(define identifier? symbol?)

(define identifier=? eq?)

(define (any? v) #t)
