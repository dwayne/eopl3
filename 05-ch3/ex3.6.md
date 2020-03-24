The syntax:

```
Expression ::= minus (Expression)
               [(minus exp)]
```

The semantics:

```
(value-of (minus exp) ρ)
= (num-val
    (- (expval->num (value-of exp ρ))))
```
