The syntax:

```
Expression ::= print (Expression)
               [(print-exp exp)]
```

The semantics:

```
(value-of (print-exp exp) ρ)
= (num-val 1)
```

How do you specify that it prints `(value-of exp ρ)` to the screen?

**Idea:** Change `value-of` like so:

```
value-of : Expression x Env -> ExpVal x String
                                          ^------ represents the screen
```
