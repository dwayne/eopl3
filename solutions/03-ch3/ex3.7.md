The syntax:

```
Expression ::= add (Expression, Expression)
               [(add-exp exp1 exp2)]

           ::= mul (Expression, Expression)
               [(mul-exp exp1 exp2)]

           ::= div (Expression, Expression)
               [(div-exp exp1 exp2)]
```

The semantics:

```
(value-of (add-exp exp1 exp2) ρ)
= (num-val
    (+ (expval->num (value-of exp1 ρ))
       (expval->num (value-of exp2 ρ))))
```

```
(value-of (mul-exp exp1 exp2) ρ)
= (num-val
    (* (expval->num (value-of exp1 ρ))
       (expval->num (value-of exp2 ρ))))
```

```
(value-of (div-exp exp1 exp2) ρ)
= (num-val
    (/ (expval->num (value-of exp1 ρ))
       (expval->num (value-of exp2 ρ))))
```
