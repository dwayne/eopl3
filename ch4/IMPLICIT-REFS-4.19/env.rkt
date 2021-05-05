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
   (proc-vals vector?)
   (saved-env env?)])

(define (empty-env)
  (empty))

(define (extend-env var ref env)
  (extend var ref env))

(define (extend-env-rec proc-names vars proc-bodies env construct-proc-val)
  (define (set-proc-vals! proc-vals vars proc-bodies saved-env i)
    (if (null? vars)
        #t
        (begin
          (vector-set! proc-vals i (newref (construct-proc-val (car vars)
                                                               (car proc-bodies)
                                                               saved-env)))
          (set-proc-vals!
           proc-vals (cdr vars) (cdr proc-bodies) saved-env (+ i 1)))))

  (let ([proc-vals (make-vector (length proc-names))])
    (let ([next-env (extend-rec proc-names proc-vals env)])
      (set-proc-vals! proc-vals vars proc-bodies next-env 0)
      next-env)))

(define (apply-env env1 search-var)
  (define (locate p-names i)
    (if (null? p-names)
        #f
        (if (identifier=? search-var (car p-names))
            i
            (locate (cdr p-names) (+ i 1)))))

  (cases env env1
    [empty ()
           (eopl:error 'apply-env "No binding for ~s" search-var)]

    [extend (saved-var saved-val saved-env)
            (if (identifier=? search-var saved-var)
                saved-val
                (apply-env saved-env search-var))]

    [extend-rec (p-names proc-vals saved-env)
                (let ([n (locate p-names 0)])
                  (if n
                      (vector-ref proc-vals n)
                      (apply-env saved-env search-var)))]))

(define identifier? symbol?)

(define identifier=? eq?)

(define (any? v) #t)
