We want to extend LETREC to allow the declaration of any number of mutually
recursive unary procedures.

For e.g.

```
letrec
  even(x) = if zero?(x) then 1 else (odd -(x, 1))
  odd(x) = if zero?(x) then 0 else (even -(x, 1))
in (odd 13)
```

The syntax:

```
Expression ::= letrec {Identifier (Identifier) = Expression}* in Expression
               [letrec-exp (rec-procs letrec-body)]
```

where each element of `rec-procs` has the form
`(proc-name bound-var proc-body)`.

The semantics:

```
(value-of (letrec-exp rec-procs letrec-body) ρ)

= (value-of letrec-body
   (extend-env-rec rec-procs ρ))
```
