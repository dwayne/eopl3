Suppose `nth-element` was implemented as follows:

```
(define (nth-element lst n)
  (if (zero? n)
      (car lst)
      (if (null? lst)
          (report-list-too-short n)
          (nth-element (cdr lst) (- n 1)))))
```

i.e. we swap the tests in the original implementation.

**What would go wrong?**

Consider what happens when `lst = '()` and `n = 0`.

Since `n = 0` the first test passes and so it takes the `car` of an empty list
which causes `car` to fail with an unhelpful error message.
