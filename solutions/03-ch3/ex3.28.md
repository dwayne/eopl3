Given,

```
let a = 3
in let p = proc (x) -(x, a)
       a = 5
   in -(a, (p 2))
```

With dynamic binding (or dynamic scoping) the value of the program is given as
follows:

```
(value-of <<let a = 3 ...>> ρ)

= (value-of <<let p = proc (x) -(x, a) ...>> [a=⎡3⎤]ρ)

= (value-of <<-(a, (p 2))>>
   [a=⎡5⎤]
     [p=(proc-val (procedure x <<-(x, a)>>))]
       [a=⎡3⎤]ρ)

Notice that we don't store the environment at the time the procedure was
defined.

= ⎡(- ⎣(value-of a ρ1)⎦
      ⎣(value-of <<(p 2)>> ρ1)⎦)⎤

where ρ1 = [a=⎡5⎤][p=(proc-val (procedure x <<-(x, a)>>))][a=⎡3⎤]ρ

= ⎡(- 5
      ⎣(value-of <<(p 2)>> ρ1)⎦)⎤

= ⎡(- 5
      ⎣(apply-procedure (procedure x <<-(x, a)>>) ⎡2⎤ ρ1)⎦)⎤

= ⎡(- 5
      ⎣(value-of <<-(x, a)>> [x=⎡2⎤]ρ1)⎦)⎤

Notice that we apply the procedure in the environment at the time the procedure
was called.

= ⎡(- 5
      (- ⎣(value-of x [x=⎡2⎤]ρ1)⎦
         ⎣(value-of a [x=⎡2⎤]ρ1)⎦))⎤

= ⎡(- 5 (- 2 5))⎤
= ⎡8⎤
```

With static binding (or static scoping) the value of the program would have
been:

```
= ⎡(- 5 (- 2 3))⎤
= ⎡6⎤
```
