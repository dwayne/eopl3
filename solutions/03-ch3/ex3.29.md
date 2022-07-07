Given,

```
let a = 3
in let p = proc (z) a
   in let f = proc (x) (p 0)
      in let a = 5
         in (f 2)
```

**Q:** *What if `f`'s formal parameter were `a`?*

**A:**

We need to find the value of the program:

```
let a = 3
in let p = proc (z) a
   in let f = proc (a) (p 0)
      in let a = 5
         in (f 2)
```

```
(value-of <<let a = 3 ...>> ρ)

= (value-of <<let p = proc (z) a ...>> [a=⎡3⎤]ρ)

= (value-of <<let f = proc (a) (p 0) ...>>
   [p=(proc-val (procedure z a))]
     [a=⎡3⎤]ρ)

= (value-of <<let a = 5 ...>>
   [f=(proc-val (procedure a <<(p 0)>>))]
     [p=(proc-val (procedure z a))]
       [a=⎡3⎤]ρ)

= (value-of <<(f 2)>>
   [a=⎡5⎤]
     [f=(proc-val (procedure a <<(p 0)>>))]
       [p=(proc-val (procedure z a))]
         [a=⎡3⎤]ρ)

= (apply-procedure (procedure a <<(p 0)>>) ⎡2⎤ ρ1)

where ρ1 = [a=⎡5⎤]
             [f=(proc-val (procedure a <<(p 0)>>))]
               [p=(proc-val (procedure z a))]
                 [a=⎡3⎤]ρ

= (value-of <<(p 0)>> [a=⎡2⎤]ρ1)

= (apply-procedure (procedure z a) ⎡0⎤ [a=⎡2⎤]ρ1)

= (value-of a [z=⎡0⎤][a=⎡2⎤]ρ1)

= ⎡2⎤
```
