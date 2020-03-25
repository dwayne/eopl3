The values:

```
ExpVal = Int + Bool + Listof(ExpVal)
DenVal = ExpVal
```

The syntax:

```
Expression ::= list (Expression, ..., Expression)
               [(list-exp (exp1 ... expn))]
```

The semantics:

```
list-val     : Listof(ExpVal) -> ExpVal
expval->list : ExpVal -> Listof(ExpVal)
```

```
(value-of (list-exp exps) ρ)
= (list-val
    (if (null? exps)
        '()
        (cons (value-of (car exps) ρ)
              (expval->list (value-of (list-exp (cdr exps)) ρ)))))
```
