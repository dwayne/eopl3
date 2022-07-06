#lang racket

(define (vector-sum v)
  (let ([n (vector-length v)])
    (if (zero? n)
        0
        (partial-vector-sum v (- n 1)))))

(define (partial-vector-sum v n)
  (if (zero? n)
      (vector-ref v 0)
      (+ (vector-ref v n)
         (partial-vector-sum v (- n 1)))))

(module+ test
  (require rackunit)

  (check-eq?
   (vector-sum #(1 2 3 4 5 6 7 8 9 10))
   55))

;; Exercise 1.14
;;
;; Given the assumption 0 <= n < length(v), prove that partial-vector-sum is
;; correct.
;;
;; Proof:
;;
;; We proceed by induction on n.
;;
;; When n = 0:
;;
;; We need to show that (partial-vector-sum v 0) = sigma(v_i, i=0, i=0).
;;
;; Now, (partial-vector-sum v 0) = (vector-ref v 0)
;;                               = v_0
;;                               = sigma(v_i, i=0, i=0)
;;
;; as required.
;;
;; When 0 <= n < length(v) - 1:
;;
;; Assume that (partial-vector-sum v n) is correct. We will show that
;; (partial-vector-sum v (+ n 1)) is correct, i.e.
;;
;; (partial-vector-sum v (+ n 1)) = sigma(v_i, i=0, i=n+1)
;;
;; Firstly, note that 1 <= n + 1 < length(v). So, n + 1 is a valid index into
;; the vector.
;;
;; Now, (partial-vector-sum v (+ n 1)) = (+ (vector-ref v (+ n 1))
;;                                          (partial-vector-sum v (- (+ n 1) 1)))
;;                                     = (+ (vector-ref v (+ n 1))
;;                                          (partial-vector-sum v n))
;;                                     = v_n+1 + sigma(v_i, i=0, i=n)
;;                                     = sigma(v_i, i=0, i=n+1)
;;
;; as required.
;;
;; Q.E.D.
