#lang racket

(require "./set.rkt")

;; Static environment

(provide

 ;; Build
 empty-senv
 extend-senv

 ;; Query
 apply-senv

 keep-free-vars)

(define (empty-senv)
  '())

(define (extend-senv var senv)
  (cons var senv))

(define (keep-free-vars free-variables senv)
  (if (null? senv)
      senv
      (let ([saved-var (car senv)]
            [saved-env (cdr senv)])
        (if (member-of-set? saved-var free-variables)
            ;; Keep the free variables
            (cons saved-var
                  (keep-free-vars
                   ;; But only the first occurrence
                   (diff-set free-variables (singleton-set saved-var))
                   saved-env))

            ;; Remove the bound variables
            (keep-free-vars free-variables saved-env)))))

(define (apply-senv senv search-var)
  (if (null? senv)
      (error 'apply-senv "No binding for ~s" search-var)
      (if (symbol=? (car senv) search-var)
          0
          (+ 1 (apply-senv (cdr senv) search-var)))))
