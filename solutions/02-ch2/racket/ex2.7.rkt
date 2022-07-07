#lang racket

;; Exercise 2.7
;;
;; Rewrite apply-env in Figure 2.1 to give a more informative error message.

(define (empty-env)
  (list 'empty-env))

(define (extend-env var val env)
  (list 'extend-env var val env))

(define (apply-env env search-var)
  (if (and (list? env)
           (not (null? env)))
      (cond
        ;; (list 'empty-env)
        [(and (eq? (car env) 'empty-env)
              (null? (cdr env)))
         (error 'apply-env "No binding for ~s" search-var)]

        ;; (list 'extend-env saved-var saved-val saved-env)
        [(and (eq? (car env) 'extend-env)
              (not (null? (cdr env)))
              (not (null? (cddr env)))
              (not (null? (cdddr env)))
              (null? (cddddr env))
              (symbol? (cadr env)))
         (let ([saved-var (cadr env)]
               [saved-val (caddr env)]
               [saved-env (cadddr env)])
           (if (eq? search-var saved-var)
               saved-val
               (apply-env saved-env search-var)))]

        [else
         (report-invalid-env env)])
      (report-invalid-env env)))

(define (report-invalid-env env)
  (error 'apply-env "Bad environment: ~s" env))

;; Using the implementation from Figure 2.1:
;;
;; > (apply-env (list 'extend-env 'd) 'd)
;;
;; caddr: contract violation
;; expected: (cons/c any/c (cons/c any/c pair?))
;; given: '(extend-env d)
;;
;; Using the implementation from above:
;;
;; > (apply-env (list 'extend-env 'd) 'd)
;;
;; apply-env: Bad environment: (extend-env d)
