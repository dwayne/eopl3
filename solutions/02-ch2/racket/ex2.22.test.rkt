#lang racket

(require "./ex2.22.rkt")

(require rackunit)

(let ([s (push 3
               (push 2
                     (push 1 (empty-stack))))])

  (check-eq? (top s) 3)
  (check-eq? (top (pop s)) 2)
  (check-eq? (top (pop (pop s))) 1)

  (check-true (empty-stack? (pop (pop (pop s)))))

  (check-exn #rx"Stack is empty" (lambda () (pop (pop (pop (pop s)))))))
