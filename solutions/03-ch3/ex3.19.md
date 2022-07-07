The syntax:

```
Expression ::= letproc Identifier ( Identifier ) = Expression in Expression
               [(letproc name var body exp)]
```

The semantics:

```
(value-of (letproc name var body exp) ρ)
= (value-of exp [name=(proc-val (procedure var body ρ))]ρ)
```
