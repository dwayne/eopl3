The syntax:

```
Expression ::= proc ({Identifier}*(,)) Expression
               [(proc-exp vs body)]

           ::= (Expression {Expression}*)
               [(call-exp rator rands)]
```

The semantics:

```
(value-of (proc-exp vs body) ρ)
= (proc-val (procedure vs body ρ))
```

```
(value-of (call-exp rator rands) ρ)
= (let ((proc (expval->proc (value-of rator ρ)))
        (args (map (lambda (rand) (value-of rand ρ)) rands)))
    (apply-procedure proc args))

(apply-procedure (procedure (v1 ... vn) body ρ) (arg1 ... argn))
= (value-of body [v1=arg1, ..., vn=argn]ρ)
```
