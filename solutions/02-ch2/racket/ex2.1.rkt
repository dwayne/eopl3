#lang racket

;; Exercise 2.1

(module+ test
  (require rackunit))

;; 1. Implement the four required operations for bigits.
;;
;; zero
;; is-zero?
;; successor
;; predecessor

(define N (make-parameter 16))

(define (zero)
  '())

(define (is-zero? n)
  (null? n))

(define (successor n)
  (if (null? n)
      '(1)
      (let ([b (+ 1 (car n))])
        (if (= b (N))
            (cons 0 (successor (cdr n)))
            (cons b (cdr n))))))

(module+ test
  (check-equal?
   (successor '())
   '(1))

  (check-equal?
   (successor '(1))
   '(2))

  (check-equal?
   (successor '(15))
   '(0 1))

  (check-equal?
   (successor '(0 1))
   '(1 1))

  (check-equal?
   (successor '(15 1))
   '(0 2))

  (check-equal?
   (successor '(0 2))
   '(1 2))

  (check-equal?
   (successor '(15 15))
   '(0 0 1)))

(define (predecessor n)
  (if (null? n)
      '()
      (let ([b (car n)])
        (cond
          [(= b 0)
           (cons (- (N) 1) (predecessor (cdr n)))]
          [(= b 1)
           (if (null? (cdr n))
               '()
               (cons 0 (cdr n)))]
          [else
           (cons (- b 1) (cdr n))]))))

(module+ test
  (check-equal?
   (predecessor '())
   '())

  (check-equal?
   (predecessor '(1))
   '())

  (check-equal?
   (predecessor '(2))
   '(1))

  (check-equal?
   (predecessor '(15))
   '(14))

  (check-equal?
   (predecessor '(0 1))
   '(15))

  (check-equal?
   (predecessor '(0 2))
   '(15 1))

  (check-equal?
   (predecessor '(0 0 1))
   '(15 15))

  (check-equal?
   (predecessor '(0 0 1 2))
   '(15 15 0 2)))

;; 2. Calculate the factorial of 10.

(define (fact n)
  (if (is-zero? n)
      (successor n)
      (mul (fact (predecessor n)) n)))

(define (mul a b)
  (if (is-zero? a)
      (zero)
      (add b (mul (predecessor a) b))))

(define (add a b)
  (if (is-zero? a)
      b
      (successor (add (predecessor a) b))))

(module+ test
  (check-equal?
   (fact '())
   (to-bigit 1))

  (check-equal?
   (fact '(1))
   (to-bigit 1))

  (check-equal?
   (fact '(2))
   (to-bigit 2))

  (check-equal?
   (fact '(3))
   (to-bigit 6))

  (check-equal?
   (fact '(4))
   (to-bigit 24))

  (check-equal?
   (fact '(5))
   (to-bigit 120))

  (check-equal?
   (fact '(10))
   (to-bigit 3628800)))

;; 3. How does the execution time vary as the argument to fact changes?
;;
;; As n increases, the execution time of (fact n) increases.

;; 4. How does the execution time vary as the base changes?
;;
;; As the base increases fewer bigits will be needed to represent the same
;; number. Since the execution time varies based on the length of the list
;; of bigits used to represent the number the execution time decreases.

;; Helpers

(define (to-bigit n)
  (if (zero? n)
      '()
      (let-values ([(q r) (quotient/remainder n (N))])
        (cons r (to-bigit q)))))
