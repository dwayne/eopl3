#lang eopl

;; Exercise 2.31
;;
;; Prefix-list ::= (Prefix-exp)
;;  Prefix-exp ::= Int
;;             ::= - Prefix-exp Prefix-exp
;;
;; Write a parser to convert a prefix-list to abstract syntax.

(provide

 ;; Build
 const-exp diff-exp

 ;; Parse
 parse-prefix-list)

(define-datatype prefix-exp prefix-exp?
  [const-exp
   (num integer?)]
  [diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)])

(define (parse-prefix-list datum)
  (define (report-invalid-prefix-list datum)
    (eopl:error 'parse-prefix-list "Invalid prefix-list: ~s" datum))

  (if (list? datum)
      (let ([result (parse-prefix-exp datum)])
        (let ([exp (car result)]
              [rest (cdr result)])
          (if (null? rest)
              (list exp)
              (report-invalid-prefix-list datum))))
      (report-invalid-prefix-list datum)))

(define (parse-prefix-exp datum)
  (if (not (null? datum))
      (cond
        [(integer? (car datum))
         (cons (const-exp (car datum)) (cdr datum))]
        [(eq? (car datum) '-)
         (let ([left (parse-prefix-exp (cdr datum))])
           (let ([operand1 (car left)]
                 [rest (cdr left)])
             (let ([right (parse-prefix-exp rest)])
               (let ([operand2 (car right)]
                     [rest (cdr right)])
                 (cons (diff-exp operand1 operand2) rest)))))])
      (eopl:error 'parse-prefix-exp "Invalid prefix-exp: ~s" datum)))
