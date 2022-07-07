#lang racket

;; Constructors

(define (empty-stack)
  (lambda (n)
    (error 'empty-stack "Stack is empty")))

(define (push x s)
  (lambda (n)
    (if (zero? n)
        x
        (s (- n 1)))))

(define (pop s)
  (s 0)
  (lambda (n)
    (s (+ n 1))))

;; Observers

(define (top s)
  (s 0))

(define (empty-stack? s)
  (with-handlers ([exn:fail? (lambda (exn) #t)])
    (s 0)
    #f))

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