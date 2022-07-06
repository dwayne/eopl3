#lang racket

(require "ex1.31.rkt")

(define (number-leaves t)
  (define (number t counter)
    (if (leaf? t)
        (values (leaf counter) (+ counter 1))
        (let*-values ([(left left-counter) (number (lson t) counter)]
                      [(right right-counter) (number (rson t) left-counter)])
          (values (interior-node (contents-of t) left right) right-counter))))
  (let-values ([(t counter) (number t 0)])
    t))

(module+ test
  (require rackunit)

  (check-equal?
   (number-leaves
    (interior-node
     'red
     (interior-node 'bar (leaf 26) (leaf 12))
     (interior-node
      'red
      (leaf 11)
      (interior-node 'quux (leaf 117) (leaf 14)))))
   (interior-node
     'red
     (interior-node 'bar (leaf 0) (leaf 1))
     (interior-node
      'red
      (leaf 2)
      (interior-node 'quux (leaf 3) (leaf 4))))))
