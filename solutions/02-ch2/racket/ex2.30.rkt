#lang eopl

;; Exercise 2.30
;;
;; Rewrite parse-expression, as defined in the book, to make it robust
;; to accept any s-exp and issue the appropriate error message if the
;; s-exp does not represent a lambda-calculus expression.

(provide

 ;; Build
 var-exp lambda-exp app-exp

 ;; Parse
 parse-expression)

(define-datatype lc-exp lc-exp?
  [var-exp
   (var identifier?)]
  [lambda-exp
   (bound-var symbol?)
   (body lc-exp?)]
  [app-exp
   (rator lc-exp?)
   (rand lc-exp?)])

(define (parse-expression datum)
  (cond
    [(identifier? datum) (var-exp datum)]
    [(and (list? datum)
          (= (length datum) 3)
          (eq? (car datum) 'lambda)
          ((list-of identifier?) (cadr datum))
          (= (length (cadr datum)) 1))
     (lambda-exp (caadr datum) (parse-expression (caddr datum)))]
    [(and (list? datum)
          (= (length datum) 2))
     (app-exp (parse-expression (car datum))
              (parse-expression (cadr datum)))]
    [else
     (eopl:error 'parse-expression "Invalid concrete syntax: ~s" datum)]))

;; Helpers

(define (identifier? v)
  (and (symbol? v)
       (not (eq? v 'lambda))))

(define (list-of pred)
  (lambda (v)
    (or (null? v)
        (and
         (pair? v)
         (pred (car v))
         ((list-of pred) (cdr v))))))
