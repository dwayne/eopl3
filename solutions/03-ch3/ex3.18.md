The syntax:

```
Expression ::= unpack {Identifier}* = Expression in Expression
               [(unpack-exp vs exp body)]
```

The semantics:

```
expval->list : ExpVal -> Listof(ExpVal)
```

```
(value-of (unpack-exp vs exp body) ρ)
= (value-of body (bind vs (expval->list (value-of exp ρ)))ρ)
```

**TODO:** Work on my semantic specification skills. What I wrote above can be
significantly improved.
