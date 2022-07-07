#lang eopl

;; Exercise 2.22
;;
;; Using define-datatype, implement the stack data type of exercise 2.4.

(provide

 ;; Construct
 empty-stack
 push
 pop

 ;; Query
 empty-stack?
 top)

(define-datatype stack stack?
  [empty]
  [non-empty
   (x any?)
   (next stack?)])

(define (empty-stack)
  (empty))

(define (push x s)
  (non-empty x s))

(define (pop s)
  (cases stack s
    [empty () (eopl:error 'pop "Stack is empty")]
    [non-empty (x next) next]))

(define (empty-stack? s)
  (cases stack s
    [empty () #t]
    [non-empty (x next) #f]))

(define (top s)
  (cases stack s
    [empty () (eopl:error 'top "Stack is empty")]
    [non-empty (x next) x]))

;; Helpers

(define (any? v) #t)
