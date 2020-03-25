The values:

```
ExpVal = Int + Bool + Listof(ExpVal)
DenVal = ExpVal
```

The syntax:

```
Expression ::= cons (Expression, Expression)
               [(cons-exp exp1 exp2)]

           ::= car (Expression)
               [(car-exp exp)]

           ::= cdr (Expression)
               [(cdr-exp exp)]

           ::= null? (Expression)
               [(null?-exp exp)]

           ::= emptylist
               [(emptylist)]
```

The semantics:

```
list-val     : Listof(ExpVal) -> ExpVal
expval->list : ExpVal -> Listof(ExpVal)
```

```
(value-of (cons-exp exp1 exp2) ρ)
= (list-val
    (cons (value-of exp1 ρ)
          (expval->list (value-of exp2 ρ))))
```

```
(value-of (car-exp exp) ρ)
= (car (expval->list (value-of exp ρ)))
```

```
(value-of (cdr-exp exp) ρ)
= (list-val
    (cdr (expval->list (value-of exp ρ))))
```

```
(value-of (null?-exp) ρ)
= (bool-val
    (null? (expval->list (value-of exp ρ))))
```

```
(value-of (emptylist) ρ)
= (list-val '())
```
