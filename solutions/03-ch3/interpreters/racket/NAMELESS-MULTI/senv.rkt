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

(define (extend-senv vars senv)
  (cons vars senv))

(define (apply-senv senv search-var)
  (find-address search-var senv 0))

(define (find-address search-var senv depth)
  (define (find-index vars i)
    (if (null? vars)
        #f
        (if (symbol=? search-var (car vars))
            i
            (find-index (cdr vars) (+ i 1)))))
  (if (null? senv)
      (error 'apply-senv "No binding for ~s" search-var)
      (let ([index (find-index (car senv) 0)])
        (if index
            (cons depth index)
            (find-address search-var (cdr senv) (+ depth 1))))))
