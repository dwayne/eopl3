#lang racket

(require "./ex2.24.rkt")

(require rackunit)

(check-equal?
 (bintree-to-list
  (interior-node 'a
                 (interior-node 'b
                                (leaf-node 1)
                                (leaf-node 2))
                 (interior-node 'c
                                (leaf-node 3)
                                (leaf-node 4))))
 '(1 b 2 a 3 c 4))
