#lang eopl

(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

(define (parse input)
  (define (parse-helper list)
    (if (null? list)
        (eopl:error 'parse "(syntax error) missing terms: ~s" input)
        (let ((head (car list)))
          (cond
            ((integer? head) (cons (const-exp head) (cdr list)))
            ((eqv? head '-)
             (let ((operand1-pair (parse-helper (cdr list))))
               (let ((operand2-pair (parse-helper (cdr operand1-pair))))
                 (cons (diff-exp (car operand1-pair)
                                 (car operand2-pair))
                       (cdr operand2-pair)))))))))
  (let ((result (parse-helper input)))
    (if (null? (cdr result))
        (car result)
        (eopl:error 'parse "(syntax error) extra terms: ~s" (cdr result)))))

(eopl:pretty-print (parse '(- - 3 2 - 4 - 12 7)))
; (diff-exp
;  (diff-exp
;   (const-exp 3)
;   (const-exp 2))
;  (diff-exp
;   (const-exp 4)
;   (diff-exp
;    (const-exp 12)
;    (const-exp 7))))
