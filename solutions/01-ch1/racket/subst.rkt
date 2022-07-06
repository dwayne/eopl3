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
       (subst-in-s-exp new old (car slist))
       (subst new old (cdr slist)))))

(define (subst-in-s-exp new old sexp)
  (if (symbol? sexp)
      (if (symbol=? sexp old) new sexp)
      (subst new old sexp)))

;; Exercise 1.11
;;
;; Why is the recursion guaranteed to halt?
;;
;; A:
;;
;; (subst new old sexp) on line 20 recurs on a sublist of the original list
;; which is indeed a smaller substructure.
;;
;; To understand what happens we can inline the definition of subst-in-s-exp
;; into subst and then it's clear that each recursive call to subst is on a
;; smaller substructure.

(module+ test
  (require rackunit)

  (check-equal?
   (subst 'a 'b '((b c) (b () d)))
   '((a c) (a () d))))
