#lang racket

;; Exercise 2.5
;;
;; Implement environments using an a-list or association list representation.

(provide empty-env extend-env apply-env)

(define (empty-env)
  '())

(define (extend-env var val env)
  (cons (cons var val) env))

(define (apply-env env search-var)
  (if (null? env)
      (error 'apply-env "No binding for ~s" search-var)
      (let ([saved-var (caar env)])
        (if (symbol=? search-var saved-var)
            (cdar env)
            (apply-env (cdr env) search-var)))))

(module+ test
  (require rackunit)

  (let ([env (extend-env
              'd 6
              (extend-env
               'y 8
               (extend-env
                'x 7
                (extend-env
                 'y 14
                 (empty-env)))))])

    (check-eq? (apply-env env 'd) 6)
    (check-eq? (apply-env env 'y) 8)
    (check-eq? (apply-env env 'x) 7)

    (check-exn #rx"No binding for a" (lambda () (apply-env env 'a)))))
