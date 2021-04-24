#lang eopl

(require "./parser.rkt")
(require "./senv.rkt")

(provide

 ;; Translator
 translate)

(define (translate prog)
  (let ([init-senv (extend-senv
                    'i
                    (extend-senv
                     'v
                     (extend-senv
                      'x
                      (empty-senv))))])
    (cases program prog
      [a-program (exp) (a-program (translate-exp exp init-senv))])))

(define (translate-exp exp senv)
  (cases expression exp
    [const-exp (n)
               (const-exp n)]

    [var-exp (var)
             (let ([result (apply-senv senv var)])
               (if (cdr result)
                   (nameless-letrec-var-exp (car result))
                   (nameless-var-exp (car result))))]

    [diff-exp (exp1 exp2)
              (diff-exp (translate-exp exp1 senv)
                        (translate-exp exp2 senv))]

    [zero?-exp (exp1)
               (zero?-exp (translate-exp exp1 senv))]

    [if-exp (exp1 exp2 exp3)
            (if-exp (translate-exp exp1 senv)
                    (translate-exp exp2 senv)
                    (translate-exp exp3 senv))]

    [let-exp (var exp1 body)
             (nameless-let-exp
              (translate-exp exp1 senv)
              (translate-exp body (extend-senv var senv)))]

    [proc-exp (var body)
              (nameless-proc-exp
               (translate-exp body (extend-senv var senv)))]

    [letrec-exp (proc-name bound-var proc-body letrec-body)
                (let ([next-senv (extend-senv-rec proc-name senv)])
                  (nameless-letrec-exp
                   (translate-exp proc-body (extend-senv bound-var next-senv))
                   (translate-exp letrec-body next-senv)))]

    [call-exp (rator rand)
              (call-exp (translate-exp rator senv)
                        (translate-exp rand senv))]

    [else
     (eopl:error 'translate-exp "Invalid source expression: ~s" exp)]))
