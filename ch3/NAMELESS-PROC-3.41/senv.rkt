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
  (define (helper lst depth)
    (if (null? lst)
        (error 'apply-senv "No binding for ~s" search-var)
        (let ([position (find-position (car lst) 0)])
          (if position
              (cons depth position)
              (helper (cdr lst) (+ depth 1))))))
  (define (find-position lst i)
    (if (null? lst)
        #f
        (if (symbol=? (car lst) search-var)
            i
            (find-position (cdr lst) (+ i 1)))))
  (helper senv 0))
