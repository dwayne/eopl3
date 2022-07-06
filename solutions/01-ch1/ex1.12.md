On inlining `subst-in-s-exp` we get,

```
(define (subst new old slist)
  (if (null? slist)
      '()
      (cons
       (if (symbol? (car slist))
           (if (eqv? (car slist) old) new (car slist))
           (subst new old (car slist)))
       (subst new old (cdr slist)))))
```

And, by simplifying we get,

```
(define (subst new old slist)
  (if (null? slist)
      '()
      (cons
       (let ((sexp (car slist)))
         (if (symbol? sexp)
             (if (eqv? sexp old) new sexp)
             (subst new old sexp)))
       (subst new old (cdr slist)))))
```
