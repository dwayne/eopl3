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
   (p-names (list-of identifier?))
   (boxes vector?)
   (saved-env env?)])

(define (empty-env)
  (empty))

(define (extend-env var val env)
  (extend var val env))

(define (extend-env-rec proc-names vars proc-bodys env construct-proc-val)
  (let ([n (length proc-names)])
    (let ([boxes (make-vector n)])
      (let ([env-rec (extend-rec proc-names boxes env)])
        (boxes-set! boxes 0 n vars proc-bodys env-rec construct-proc-val)
        env-rec))))

(define (boxes-set! boxes i n vars proc-bodys env construct-proc-val)
  (if (< i n)
      (begin
        (vector-set! boxes i (construct-proc-val (car vars) (car proc-bodys) env))
        (boxes-set! boxes (+ i 1) n (cdr vars) (cdr proc-bodys) env construct-proc-val))
      #t))

(define (apply-env env1 search-var)
  (cases env env1
    [empty ()
           (eopl:error 'apply-env "No binding for ~s" search-var)]

    [extend (saved-var saved-val saved-env)
            (if (identifier=? search-var saved-var)
                saved-val
                (apply-env saved-env search-var))]

    [extend-rec (p-names boxes saved-env)
                (let ([i (find-proc-val p-names search-var 0)])
                  (if i
                      (vector-ref boxes i)
                      (apply-env saved-env search-var)))]))

(define (find-proc-val p-names search-var index)
  (if (null? p-names)
      #f
      (if (identifier=? search-var (car p-names))
          index
          (find-proc-val (cdr p-names) search-var (+ index 1)))))

(define identifier? symbol?)

(define identifier=? eq?)

(define (any? v) #t)
