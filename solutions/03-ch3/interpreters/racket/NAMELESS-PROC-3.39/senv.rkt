#lang racket

;; Static environment

(provide

 ;; Build
 empty-senv
 extend-senv
 extend-unpack-senv

 ;; Query
 apply-senv)

(define (empty-senv)
  '())

(define (extend-senv var senv)
  (cons var senv))

(define (extend-unpack-senv vars senv)
  (if (null? vars)
      senv
      (extend-unpack-senv
       (cdr vars)
       (cons (car vars) senv))))

(define (apply-senv senv search-var)
  (if (null? senv)
      (error 'apply-senv "No binding for ~s" search-var)
      (if (symbol=? (car senv) search-var)
          0
          (+ 1 (apply-senv (cdr senv) search-var)))))
