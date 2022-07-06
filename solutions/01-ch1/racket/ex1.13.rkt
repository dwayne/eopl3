#lang racket

;; S-list ::= ({S-exp}*)
;;  S-exp ::= Symbol | S-list

;; symbol? symbol? s-list? . -> . s-list?
(define (subst new old slist)
  (map
   ;; subst-in-s-exp
   (lambda (sexp)
     (if (symbol? sexp)
         (if (symbol=? sexp old) new sexp)
         (subst new old sexp)))
   slist))

(module+ test
  (require rackunit)

  (check-equal?
   (subst 'a 'b '((b c) (b () d)))
   '((a c) (a () d))))
