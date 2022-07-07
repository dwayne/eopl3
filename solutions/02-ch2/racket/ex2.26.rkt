#lang eopl

;; Exercise 2.26
;;
;; Red-blue-tree    ::= Red-blue-subtree
;; Red-blue-subtree ::= (red-node Red-blue-subtree Red-blue-subtree)
;;                  ::= (blue-node {Red-blue-subtree}*)
;;                  ::= (leaf-node Int)
;;
;; Write an equivalent definition using define-datatype, and use the
;; resulting interface to write a procedure that takes a tree and
;; builds a tree of the same shape, except that each leaf node is
;; replaced by a leaf node that contains the number of red nodes
;; on the path between it and the root.

(require "../ch1/ex1.31.rkt")

(provide

 ;; Construct
 a-red-blue-subtree
 red-node blue-node leaf-node

 ;; Convert
 mark-leaves-with-red-depth)

(define-datatype red-blue-tree red-blue-tree?
  [a-red-blue-subtree
   (tree red-blue-subtree?)])

(define-datatype red-blue-subtree red-blue-subtree?
  [red-node
   (left red-blue-subtree?)
   (right red-blue-subtree?)]
  [blue-node
   (subtrees (list-of red-blue-subtree?))]
  [leaf-node
   (num integer?)])

(define (mark-leaves-with-red-depth t)
  (define (mark t red-depth)
    (if (leaf? t)
        (leaf-node red-depth)
        (if (eq? (contents-of t) 'red)
            (red-node (mark (lson t) (+ red-depth 1))
                      (mark (rson t) (+ red-depth 1)))
            (blue-node (list
                        (mark (lson t) red-depth)
                        (mark (rson t) red-depth))))))
  (a-red-blue-subtree (mark t 0)))

;; Helpers

(define (list-of pred)
  (lambda (v)
    (or (null? v)
        (and
         (pair? v)
         (pred (car v))
         ((list-of pred) (cdr v))))))
