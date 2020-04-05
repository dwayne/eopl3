The syntax:

```
Expression ::= letrec Identifier ({Identifier}*(,)) = Expression in Expression
               [letrec-exp (p-name b-vars p-body letrec-body)]
```

The semantics:

```
(value-of (letrec-exp proc-name bound-vars proc-body letrec-body) ρ)

= (value-of letrec-body (extend-env-rec proc-name bound-vars proc-body)ρ)
```

Let `ρ1 = (extend-env-rec proc-name bound-vars proc-body)ρ` then,

```
(apply-env ρ1 proc-name)
= (proc-val (procedure bound-vars proc-body ρ1))
```

The main change is storing `bound-vars` instead of `bound-var`. This will also
affect `apply-procedure`.
