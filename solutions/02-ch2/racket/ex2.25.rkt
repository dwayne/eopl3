#lang eopl

;; Exercise 2.25
;;
;; Use cases to write max-interior, which takes a binary tree of integers
;; with at least one interior node and returns the symbol associated with
;; an interior node with a maximal leaf sum.

(provide

 ;; Construct
 leaf-node interior-node

 ;; Find
 max-interior)

(define-datatype bintree bintree?
  [leaf-node
   (num integer?)]
  [interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)])

(define (max-interior t)
  (get-key (max-interior-helper t)))

;; NOTE: max-interior-helper traverses the bintree twice. Once to get
;;       the sum and a second time to get the max-interior. An
;;       alternative is to get both values at once.
;;
;;       max-interior-helper:
;;         bintree? -> (number? maybe[(symbol? number?)])

;; Helpers

;; max-interior-helper: bintree? -> maybe[(symbol? number?)]
(define (max-interior-helper t)
  (cases bintree t
    [leaf-node (num) #f]
    [interior-node
     (key left right)
     (max3 (list key (bintree-sum t))
           (max-interior-helper left)
           (max-interior-helper right))]))

;; bintree-sum: bintree? -> number?
(define (bintree-sum t)
  (cases bintree t
    [leaf-node (num) num]
    [interior-node
     (key left right)
     (+ (bintree-sum left)
        (bintree-sum right))]))

;; get-key: maybe[(symbol? number?)] -> maybe[symbol?]
(define (get-key key-sum)
  (if key-sum (car key-sum) #f))

;; max3:
;;   maybe[(symbol? number?)] ->
;;   maybe[(symbol? number?)] ->
;;   maybe[(symbol? number?)] ->
;;   maybe[(symbol? number?)]
(define (max3 a b c)
  (max2 (max2 a b) c))

;; max2:
;;   maybe[(symbol? number?)] ->
;;   maybe[(symbol? number?)] ->
;;   maybe[(symbol? number?)]
(define (max2 a b)
  (if a
      (if b
          (if (> (cadr b) (cadr a)) b a)
          a)
      b))
