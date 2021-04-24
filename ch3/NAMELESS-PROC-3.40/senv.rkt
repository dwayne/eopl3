#lang racket

;; Static environment

(provide

 ;; Build
 empty-senv
 extend-senv
 extend-senv-rec

 ;; Query
 apply-senv)

(define (empty-senv)
  '())

(define (extend-senv var senv)
  (cons (cons var #f) senv))

(define (extend-senv-rec var senv)
  (cons (cons var #t) senv))

(define (apply-senv senv search-var)
  (if (null? senv)
      (error 'apply-senv "No binding for ~s" search-var)
      (if (symbol=? (caar senv) search-var)
          (cons 0 (cdar senv))
          (let ([result (apply-senv (cdr senv) search-var)])
            (cons (+ (car result) 1)
                  (cdr result))))))
