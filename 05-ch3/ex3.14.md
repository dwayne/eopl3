The syntax:

```
Expression      ::= if Bool-Expression then Expression else Expression
                    [(if-exp bool-exp exp1 exp2)]

Bool-Expression ::= zero? (Expression)
                    [(zero?-exp exp)]

                ::= equal? (Expression, Expression)
                    [(equal?-exp exp1 exp2)]

                ::= greater? (Expression, Expression)
                    [(greater?-exp exp1 exp2)]

                ::= less? (Expression, Expression)
                    [(less?-exp exp1 exp2)]
```

The semantics:

```
value-of : Expression x Env -> Int
value-of-bool-exp : Bool-Expression x Env -> Bool
```

```
(value-of (if-exp bool-exp exp1 exp2) ρ)
= (if (value-of-bool-exp bool-exp ρ)
      (value-of exp1 ρ)
      (value-of exp2 ρ))
```

```
(value-of (equal?-exp exp1 exp2) ρ)
= (= (value-of exp1 ρ) (value-of exp2 ρ))
```

```
(value-of (greater?-exp exp1 exp2) ρ)
= (> (value-of exp1 ρ) (value-of exp2 ρ))
```

```
(value-of (less?-exp exp1 exp2) ρ)
= (< (value-of exp1 ρ) (value-of exp2 ρ))
```
