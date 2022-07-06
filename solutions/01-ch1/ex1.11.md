This,

```
(define (subst-in-s-exp new old sexp)
  (if (symbol? sexp)
      (if (eqv? sexp old) new sexp)
      (subst new old sexp)))
```

is equivalent to:

```
(define (subst-in-s-exp new old sexp)
  (if (symbol? sexp)
      (if (eqv? sexp old) new sexp)
      (if (null? sexp)
          '()
          (cons
           (subst-in-s-exp new old (car sexp))
           (subst new old (cdr sexp))))))
```

by inlining `(subst new old sexp)`.

It's now clear that all the recursion happens on a smaller substructure of
`sexp`.

Thus, the recursion is guaranteed to halt.
