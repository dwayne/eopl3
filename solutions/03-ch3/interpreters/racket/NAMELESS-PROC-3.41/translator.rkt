#lang eopl

(require "./parser.rkt")
(require "./senv.rkt")

(provide

 ;; Translator
 translate)

(define (translate prog)
  (let ([init-senv (extend-senv '(i v x) (empty-senv))])
    (cases program prog
      [a-program (exp) (a-program (translate-exp exp init-senv))])))

(define (translate-exp exp senv)
  (cases expression exp
    [const-exp (n)
               (const-exp n)]

    [var-exp (var)
             (let ([result (apply-senv senv var)])
               (nameless-var-exp (car result) (cdr result)))]

    [diff-exp (exp1 exp2)
              (diff-exp (translate-exp exp1 senv)
                        (translate-exp exp2 senv))]

    [zero?-exp (exp1)
               (zero?-exp (translate-exp exp1 senv))]

    [if-exp (exp1 exp2 exp3)
            (if-exp (translate-exp exp1 senv)
                    (translate-exp exp2 senv)
                    (translate-exp exp3 senv))]

    [let-exp (vars exps body)
             (nameless-let-exp
              (map (lambda (exp1) (translate-exp exp1 senv)) exps)
              (translate-exp body (extend-senv vars senv)))]

    [proc-exp (vars body)
              (nameless-proc-exp
               (translate-exp body (extend-senv vars senv)))]

    [call-exp (rator rands)
              (call-exp (translate-exp rator senv)
                        (map (lambda (rand) (translate-exp rand senv)) rands))]

    [else
     (eopl:error 'translate-exp "Invalid source expression: ~s" exp)]))
