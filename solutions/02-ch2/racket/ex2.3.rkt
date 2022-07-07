#lang racket

;; Exercise 2.3
;;
;; Define a representation of all the integers (negative and nonnegative)
;; as diff-trees, where a diff-tree is a list defined by the grammar
;;
;; Diff-tree ::= (one) | (diff Diff-tree Diff-tree)

;; 2. Turn the Diff-tree representation into an implementation of the integers.

(define (zero)
  '(diff (one) (one)))

(define (is-zero? n)
  (zero? (reduce n)))

(define (successor n)
  `(diff ,n (diff (diff (one) (one)) (one))))

(define (predecessor n)
  `(diff ,n (one)))

(module+ test
  (require rackunit)

  (check-true (is-zero? (zero)))
  (check-true (is-zero? (predecessor (successor (zero)))))
  (check-true (is-zero? (successor (predecessor (zero)))))
  (check-false (is-zero? (successor (zero))))

  (check-eq?
   (reduce (zero))
   0)

  (check-eq?
   (reduce (successor (zero)))
   1)

  (check-eq?
   (reduce (successor (successor (zero))))
   2)

  (check-eq?
   (reduce (predecessor (zero)))
   -1)

  (check-eq?
   (reduce (predecessor (predecessor (zero))))
   -2))

;; 3. Write an addition procedure.

(define (diff-tree-plus a b)
  ;; a + b = a - (0 - b)
  `(diff ,a (diff ,(zero) ,b)))

(module+ test
  (check-true
   (is-zero? (diff-tree-plus (to-diff-tree 10)
                             (to-diff-tree -10))))

  (check-eq?
   (reduce (diff-tree-plus (to-diff-tree 2)
                           (to-diff-tree 3)))
   5))

;; Helpers

(define (reduce n)
  (if (symbol=? (car n) 'one)
      1
      (- (reduce (cadr n))
         (reduce (caddr n)))))

(define (to-diff-tree n)
  (cond
    [(= n 0) (zero)]
    [(> n 0) `(diff ,(to-diff-tree (- n 1)) (diff (diff (one) (one)) (one)))]
    [(< n 0) `(diff ,(to-diff-tree (+ n 1)) (one))]))
