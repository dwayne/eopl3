#lang racket

(define (number-elements lst)
  (if (null? lst)
      '()
      (g (list 0 (car lst)) (number-elements (cdr lst)))))

;; How to define g?

;; (number-elements '(a b c))
;; => (g '(0 a) (number-elements '(b c)))
;; => (g '(0 a) (g '(0 b) (number-elements '(c))))
;; => (g '(0 a) (g '(0 b) (g '(0 c) (number-elements '()))))
;; => (g '(0 a) (g '(0 b) (g '(0 c) '())))
;; => (g '(0 a) (g '(0 b) '((0 c))))
;; => (g '(0 a) '((0 b) (1 c)))
;; => '((0 a) (1 b) (2 c))

(define (g x xs)
  (define (add1 x)
    (list (+ (car x) 1) (cadr x)))
  (if (null? xs)
      (list x)
      (cons x (map add1 xs))))

(module+ test
  (require rackunit)

  (check-equal?
   (number-elements '())
   '())

  (check-equal?
   (number-elements '(a b c))
   '((0 a) (1 b) (2 c))))
