#lang eopl

(define-datatype stack stack?
  (mt-stack)
  (non-mt-stack (top always?) (rest stack?)))

(define (empty-stack)
  (mt-stack))

(define (push v s)
  (non-mt-stack v s))

(define (pop s)
  (cases stack s
    (mt-stack () (eopl:error 'pop "stack is empty"))
    (non-mt-stack (top rest) rest)))

(define (top s)
  (cases stack s
    (mt-stack () (eopl:error 'top "stack is empty"))
    (non-mt-stack (top rest) top)))

(define (empty-stack? s)
  (cases stack s
    (mt-stack () #t)
    (else #f)))

(define STACK
  (push 'a
        (push 'b
              (push 'c (empty-stack)))))

(eopl:pretty-print (empty-stack? STACK)) ; #f
(eopl:pretty-print (top STACK)) ; 'a
(eopl:pretty-print (top (pop STACK))) ; 'b
(eopl:pretty-print (top (pop (pop STACK)))) ; 'c
(eopl:pretty-print (empty-stack? (pop (pop (pop STACK))))) ; #t