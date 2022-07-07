#lang eopl

;; Exercise 2.21
;;
;; Implement the data type of environments using define-datatype.
;; Then, include has-binding? of exercise 2.9.

(provide

 ;; Construct
 empty-env extend-env

 ;; Query
 has-binding?)

(define-datatype environment environment?
  [empty-env]
  [extend-env
   (saved-var symbol?)
   (saved-val any?)
   (saved-env environment?)])

(define (has-binding? env search-var)
  (cases environment env
    [empty-env () #f]
    [extend-env
     (saved-var saved-val saved-env)
     (or (eq? search-var saved-var)
         (has-binding? saved-env search-var))]))

;; Helpers

(define (any? v) #t)
