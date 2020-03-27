The syntax:

```
Expression ::= let {Identifier = Expression}* in Expression
               [(let-exp bindings body)]
```

The semantics:

```
binding-identifier : Binding -> Identifier
binding-expression : Binding -> Expression

(binding-identifier (binding id exp)) == id
(binding-expression (binding id exp)) == exp
```

```
(value-of (let-exp bindings body) ρ)
= (value-of body [x1 = val1, x2 = val2, ...]ρ)

where the ith element of bindings is such that

xi = (binding-identifier binding-i) and
vali = (value-of (binding-expression binding-i) ρ)
```
