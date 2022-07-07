The syntax:

```
Expression ::= let* {Identifier = Expression}* in Expression
               [(let*-exp bindings body)]
```

The semantics:

```
binding-identifier : Binding -> Identifier
binding-expression : Binding -> Expression

(binding-identifier (binding id exp)) == id
(binding-expression (binding id exp)) == exp
```

```
Let,

x1 = (binding-identifier binding-1)
val1 = (value-of (binding-expression binding-1) ρ)

x2 = (binding-identifier binding-2)
val2 = (value-of (binding-expression binding-2) [x1=val1]ρ)

...

xi = (binding-identifier binding-i)
vali = (value-of (binding-expression binding-i) [xi-1=vali-1,...,x2=val2,x1=val1]ρ)

and

ρ0 = ρ
ρ1 = [x1=val1]ρ
...
ρi = [xi-1=vali-1,...,x2=val2,x1=val1]ρ

Then,

(value-of (let-exp bindings body) ρ)
= (value-of body ρn+1)

where n = (length bindings)
```

**N.B.**

```
let* x = ... y = ...
in ...

is equivalent to

let x = ...
in let y = ...
   in ...
```
