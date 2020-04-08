Recall the process:

```
Concrete syntax

-> [Parser]

-> AST

-> [Translator]

-> Nameless AST

-> [Interpreter]

-> Expressed value
```

**Concrete syntax**

```
letrec
  sum(n) =
    if zero?(n) then
      0
    else
      -((sum -(n, 1)), -(0, n))
in
  (sum 5)
```

**AST**

```
(letrec-exp
  sum
  n
  (if-exp (zero?-exp (var-exp n))
          (const-exp 0)
          (diff-exp (call-exp (var-exp sum)
                              (diff-exp (var-exp n) (const-exp 1)))
                    (diff-exp (const-exp 0)
                              (var-exp n))))
  (call-exp (var-exp sum) (const-exp 5)))
```

**Nameless AST**

```
(nameless-letrec-exp
  (if-exp (zero?-exp (nameless-var-exp 0))
          (const-exp 0)
          (diff-exp (call-exp (nameless-var-exp 1)
                              (diff-exp (nameless-var-exp 0) (const-exp 1)))
                    (diff-exp (const-exp 0)
                              (nameless-var-exp 0))))
  (call-exp (nameless-var-exp 0) (const-exp 5)))
```

Within scope of `n`:

```
senv = [n, sum]
```

Within scope of `sum` but outside scope of `n`:

```
senv = [sum]
```

**Expressed Value**

```
(value-of (nameless-letrec-exp ...) [])

= (value-of
    (call-exp (nameless-var-exp 0) (const-exp 5))
    env1)

where env1 = [(proc-val (procedure (if-exp ...) env1))]

= (apply-procedure
    (to-procedure (value-of (nameless-var-exp 0) env1))
    (value-of (const-exp 5) env1))

= (apply-procedure
    (to-procedure (proc-val (procedure (if-exp ...) env1)))
    5)

= (apply-procedure
    (procedure (if-exp ...) env1)
    (num-val 5))

= (value-of
    (if-exp (zero?-exp (nameless-var-exp 0))
            (const-exp 0)
            (diff-exp (call-exp (nameless-var-exp 1)
                                (diff-exp (nameless-var-exp 0) (const-exp 1)))
                      (diff-exp (const-exp 0)
                                (nameless-var-exp 0))))
    env2)

where env2 = [(num-val 5), (proc-val (procedure (if-exp ...) env1))]

= (value-of
    (diff-exp (call-exp (nameless-var-exp 1)
                        (diff-exp (nameless-var-exp 0) (const-exp 1)))
              (diff-exp (const-exp 0)
                        (nameless-var-exp 0)))
    env2)

= (num-val
    (- (expval->num (value-of (call-exp ...) env2))
       (expval->num (value-of (diff-exp ...) env2))))

= ...
```

**N.B.** *From thinking through each step I was able to figure out an
implementation.*
