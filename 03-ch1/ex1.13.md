The original grammar is:

```
S-list ::= ({S-exp}*)
S-exp  ::= Symbol | S-list
```

Using `map` to implement `subst` and following the original grammar we get:

```
(define (subst new old slist)
  (map
   (lambda (sexp)
     (if (symbol? sexp)
         (if (eqv? sexp old) new sexp)
         (subst new old sexp)))
   slist))
```
