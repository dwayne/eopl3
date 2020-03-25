The syntax:

```
Expression ::= cond {Expression ==> Expression}* end
               [(cond-exp (clause1 ... clausen))]
```

The semantics:

```
clause-test   : Clause -> Expr
clause-result : Clause -> Expr

(clause-test (clause test result)) == test
(clause-result (clause test result)) == result
```

```
(value-of (cond-exp clauses) ρ)
= error                              if (empty? clauses)
= (value-of (clause-test clause1) ρ) if (expval->bool (value-of (clause-result clause1) ρ))
= (value-of (clause-test clause2) ρ) if (expval->bool (value-of (clause-result clause2) ρ))
...
= (value-of (clause-test clausen) ρ) if (expval->bool (value-of (clause-result clausen) ρ))
```

**Q:** *Is there a better way to specify this?*
