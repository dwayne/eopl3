The syntax:

```
Expression ::= equal? (Expression, Expression)
               [(equal?-exp exp1 exp2)]

           ::= greater? (Expression, Expression)
               [(greater?-exp exp1 exp2)]

           ::= less? (Expression, Expression)
               [(less?-exp exp1 exp2)]
```

The semantics:

```
(value-of (equal?-exp exp1 exp2) ρ)
= (bool-val
    (= (expval->num (value-of exp1 ρ))
       (expval->num (value-of exp2 ρ))))
```

```
(value-of (greater?-exp exp1 exp2) ρ)
= (bool-val
    (> (expval->num (value-of exp1 ρ))
       (expval->num (value-of exp2 ρ))))
```

```
(value-of (less?-exp exp1 exp2) ρ)
= (bool-val
    (< (expval->num (value-of exp1 ρ))
       (expval->num (value-of exp2 ρ))))
```
