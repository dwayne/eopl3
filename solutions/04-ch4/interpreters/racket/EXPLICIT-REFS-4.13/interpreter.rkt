#lang eopl

(require "./env.rkt")
(require "./parser.rkt")
(require "./store.rkt")

(provide

 ;; Expressed Values
 num-val bool-val

 ;; Interpreter
 run)

(define (run s)
  (let ([init-env (extend-env
                   '(i v x)
                   (list (num-val 1) (num-val 5) (num-val 10))
                   (empty-env))])
    (value-of-program (parse s) init-env (empty-store))))

(define (value-of-program prog env store)
  (cases program prog
    [a-program (exp)
               (cases answer (value-of-exp exp env store)
                 [an-answer (val next-store) val])]))

(define (value-of-exp exp env store)
  (cases expression exp
    [const-exp (n)
               (an-answer (num-val n) store)]

    [var-exp (var)
             (an-answer (apply-env env var construct-proc-val) store)]

    [diff-exp (exp1 exp2)
              (cases answer (value-of-exp exp1 env store)
                [an-answer (val1 store1)
                           (cases answer (value-of-exp exp2 env store1)
                             [an-answer (val2 store2)
                                        (an-answer
                                         (num-val
                                          (- (expval->num val1)
                                             (expval->num val2)))
                                         store2)])])]

    [zero?-exp (exp1)
               (cases answer (value-of-exp exp1 env store)
                 [an-answer (val1 store1)
                            (an-answer
                             (if (zero? (expval->num val1))
                                 (bool-val #t)
                                 (bool-val #f))
                             store1)])]

    [if-exp (exp1 exp2 exp3)
            (cases answer (value-of-exp exp1 env store)
              [an-answer (val1 store1)
                         (if (expval->bool val1)
                             (value-of-exp exp2 env store1)
                             (value-of-exp exp3 env store1))])]

    [let-exp (var exp1 body)
             (cases answer (value-of-exp exp1 env store)
               [an-answer (val1 store1)
                          (value-of-exp body (extend-env (list var) (list val1) env) store1)])]

    [proc-exp (vars body)
              (an-answer
               (proc-val (procedure vars body env))
               store)]

    [letrec-exp (proc-names proc-bound-vars proc-bodies letrec-body)
                (value-of-exp letrec-body (extend-env-rec proc-names proc-bound-vars proc-bodies env) store)]

    [call-exp (rator rands)
              (cases answer (value-of-exp rator env store)
                [an-answer (val1 store1)
                           (let ([proc (expval->proc val1)])
                             (let ([result (value-of-args rands env store1)])
                               (let ([vals (car result)]
                                     [store2 (cadr result)])
                                 (apply-procedure proc vals store2))))])]

    [begin-exp (exp1 exps)
               (value-of-begin-exp (cons exp1 exps) env store)]

    [newref-exp (exp1)
                (cases answer (value-of-exp exp1 env store)
                  [an-answer (val1 store1)
                             (let ([newref-result (newref store1 val1)])
                               (an-answer (ref-val (cadr newref-result)) (car newref-result)))])]

    [deref-exp (exp1)
               (cases answer (value-of-exp exp1 env store)
                 [an-answer (val1 store1)
                            (let ([ref (expval->ref val1)])
                              (an-answer (deref store1 ref) store1))])]

    [setref-exp (exp1 exp2)
                (cases answer (value-of-exp exp1 env store)
                  [an-answer (val1 store1)
                             (let ([ref (expval->ref val1)])
                               (cases answer (value-of-exp exp2 env store1)
                                 [an-answer (val2 store2)
                                            (an-answer (num-val 23) (setref! store2 ref val2))]))])]))

(define (value-of-args args env store)
  (if (null? args)
      (list '() store)
      (cases answer (value-of-exp (car args) env store)
        [an-answer (val store1)
                   (let ([result (value-of-args (cdr args) env store1)])
                     (let ([vals (car result)]
                           [store2 (cadr result)])
                       (list (cons val vals) store2)))])))

(define (value-of-begin-exp exps env store)
  (if (null? (cdr exps))
      (value-of-exp (car exps) env store)
      (cases answer (value-of-exp (car exps) env store)
        [an-answer (val1 store1)
                   (value-of-begin-exp (cdr exps) env store1)])))

(define (construct-proc-val var body saved-env)
  (proc-val (procedure var body saved-env)))

;; Procedure ADT

(define-datatype proc proc?
  [procedure
   (vars (list-of identifier?))
   (body expression?)
   (saved-env env?)])

(define (apply-procedure proc1 vals store)
  (cases proc proc1
    [procedure (vars body saved-env)
               (value-of-exp body (extend-env vars vals saved-env) store)]))

;; Answer

(define-datatype answer answer?
  [an-answer
   (val expval?)
   (store store?)])

;; Values
;;
;; ExpVal = Int + Bool + Proc + Ref(ExpVal)
;; DenVal = ExpVal

(define-datatype expval expval?
  [num-val (n number?)]
  [bool-val (b boolean?)]
  [proc-val (p proc?)]
  [ref-val (r reference?)])

(define (expval->num val)
  (cases expval val
    [num-val (n) n]
    [else (eopl:error 'expval->num "Not a number: ~s" val)]))

(define (expval->bool val)
  (cases expval val
    [bool-val (b) b]
    [else (eopl:error 'expval->bool "Not a boolean: ~s" val)]))

(define (expval->proc val)
  (cases expval val
    [proc-val (p) p]
    [else (eopl:error 'expval->proc "Not a procedure: ~s" val)]))

(define (expval->ref val)
  (cases expval val
    [ref-val (r) r]
    [else (eopl:error 'expval->ref "Not a reference: ~s" val)]))
