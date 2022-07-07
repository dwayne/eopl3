#lang eopl

(require "./parser.rkt")
(require "./senv.rkt")
(require "./set.rkt")

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
             (nameless-var-exp
              (apply-senv senv var))]

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
              (let ([saved-env (keep-free-vars (free-variables exp) senv)])
                (nameless-proc-exp
                 (translate-exp body (extend-senv var saved-env))))]

    [call-exp (rator rand)
              (call-exp (translate-exp rator senv)
                        (translate-exp rand senv))]

    [else
     (eopl:error 'translate-exp "Invalid source expression: ~s" exp)]))

(define (free-variables exp)
  (cases expression exp
    [const-exp (n)
               (empty-set)]

    [var-exp (var)
             (singleton-set var)]

    [diff-exp (exp1 exp2)
              (union-set (free-variables exp1)
                         (free-variables exp2))]

    [zero?-exp (exp1)
               (free-variables exp1)]

    [if-exp (exp1 exp2 exp3)
            (union-set (union-set (free-variables exp1)
                                  (free-variables exp2))
                       (free-variables exp3))]

    [let-exp (var exp1 body)
             (union-set (free-variables exp1)
                        (diff-set (free-variables body)
                                  (singleton-set var)))]

    [proc-exp (var body)
              (diff-set (free-variables body)
                        (singleton-set var))]

    [call-exp (rator rand)
              (union-set (free-variables rator)
                         (free-variables rand))]

    [else
     (eopl:error 'free-variables "Invalid source expression: ~s" exp)]))
