#lang eopl

;; Exercise 2.24
;;
;; Implement a bintree-to-list procedure for binary trees.

(provide

 ;; Construct
 leaf-node
 interior-node

 ;; Convert
 bintree-to-list)

(define-datatype bintree bintree?
  [leaf-node
   (num integer?)]
  [interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)])

(define (bintree-to-list t)
  (cases bintree t
    [leaf-node (num) (list num)]
    [interior-node
     (key left right)
     (append (bintree-to-list left)
             (list key)
             (bintree-to-list right))]))
