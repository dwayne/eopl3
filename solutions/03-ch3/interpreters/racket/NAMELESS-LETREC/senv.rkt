#lang racket

;; Static environment

(provide

 ;; Build
 empty-senv
 extend-senv

 ;; Query
 apply-senv)

(define (empty-senv)
  '())

(define (extend-senv var senv)
  (cons var senv))

(define (apply-senv senv search-var)
  (if (null? senv)
      (error 'apply-senv "No binding for ~s" search-var)
      (if (symbol=? (car senv) search-var)
          0
          (+ 1 (apply-senv (cdr senv) search-var)))))
