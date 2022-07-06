#lang racket

;; S-list ::= ()
;;          | (S-exp . S-list)
;;
;;  S-exp ::= Symbol
;;          | S-list

;; symbol? symbol? s-list? . -> . s-list?
(define (subst new old slist)
  (if (null? slist)
      '()
      (cons
       (let ([sexp (car slist)])
         (if (symbol? sexp)
             (if (symbol=? sexp old) new sexp)
             (subst new old sexp)))
       (subst new old (cdr slist)))))

(module+ test
  (require rackunit)

  (check-equal?
   (subst 'a 'b '((b c) (b () d)))
   '((a c) (a () d))))
