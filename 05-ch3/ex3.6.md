The syntax:

```
Expression ::= minus (Expression)
               [(minus-exp exp)]
```

The semantics:

```
(value-of (minus-exp exp) ρ)
= (num-val
    (- (expval->num (value-of exp ρ))))
```
