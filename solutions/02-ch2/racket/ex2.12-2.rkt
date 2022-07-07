#lang racket

;; Constructors

(define (empty-stack)
  (list
   (lambda () (error 'top "Stack is empty"))
   (lambda () #t)))

(define (push x s)
  (list
   (lambda () x)
   (lambda () #f)
   s))

(define (pop s)
  ((car s))
  (caddr s))

;; Observers

(define (top s)
  ((car s)))

(define (empty-stack? s)
  ((cadr s)))

(module+ test
  (require rackunit)

  (let ([s (push 3
                 (push 2
                       (push 1 (empty-stack))))])
    (check-eq? (top s) 3)
    (check-eq? (top (pop s)) 2)
    (check-eq? (top (pop (pop s))) 1)

    (check-true (empty-stack? (pop (pop (pop s)))))

    (check-exn #rx"Stack is empty" (lambda () (pop (pop (pop (pop s))))))))