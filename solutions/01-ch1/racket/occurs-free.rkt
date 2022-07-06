#lang racket

;; LcExp ::= Identifier
;;       ::= (lambda (Identifier) LcExp)
;;       ::= (LcExp LcExp)

;; symbol? lc-exp? . -> . boolean?
;;
;; Returns #t if the symbol var occurs free in exp, otherwise returns #f.

;; The book's implementation
;(define (occurs-free? var exp)
;  (cond
;    [(symbol? exp) (symbol=? var exp)]
;    [(eq? (car exp) 'lambda)
;     (and
;      (not (symbol=? var (caadr exp)))
;      (occurs-free? var (caddr exp)))]
;    [else
;     (or
;      (occurs-free? var (car exp))
;      (occurs-free? var (cadr exp)))]))

;; An alternative implementation using match
(define (occurs-free? var exp)
  (match exp
    ;; Identifier
    [(? symbol? x) (symbol=? var x)]

    ;; (lambda (Identifier) LcExp)
    [`(lambda (,x) ,body)
     (and
      (not (symbol=? var x))
      (occurs-free? var body))]

    ;; (LcExp LcExp)
    [`(,f ,arg)
     (or
      (occurs-free? var f)
      (occurs-free? var arg))]))

(module+ test
  (require rackunit)

  (check-true (occurs-free? 'x 'x))
  (check-true (occurs-free? 'x '(lambda (y) (x y))))
  (check-true (occurs-free? 'x '((lambda (x) x) (x y))))
  (check-true (occurs-free? 'x '(lambda (y) (lambda (z) (x (y z))))))

  (check-false (occurs-free? 'x 'y))
  (check-false (occurs-free? 'x '(lambda (x) (x y)))))
