Given,

```
let makerec =
  proc (f)
    let d =
      proc (x)
        proc (z) ((f (x x)) z)
    in proc (n) ((f (d d)) n)
in let maketimes4 =
     proc (f)
       proc (x)
         if zero?(x) then
           0
         else
           -((f -(x, 1)), -4)
   in let times4 = (makerec maketimes4)
      in (times4 3)
```

**Q:** *Show that it returns 12.*

**A:**

```
(times4 3)
= ((makerec maketimes4) 3)

= (let d =
     proc (x)
       proc (z) ((maketimes4 (x x)) z)
   in proc (n) ((maketimes4 (d d)) n)
   3)

[ d = proc (x) proc(z) ((maketimes4 (x x)) z) ]

= (proc (n) ((maketimes4 (d d)) n) 3)

= ((maketimes4 (d d)) 3)
= -(((d d) -(3, 1)), -4)
= -(((d d) 2), -4)

= -(((maketimes4 (d d)) 2), -4)
= -(-(((d d) -(2, 1)), -4), -4)
= -(-(((d d) 1), -4), -4)

= -(-(((maketimes4 (d d)) 1), -4), -4)
= -(-(-(((d d) -(1, 1)), -4), -4), -4)
= -(-(-(((d d) 0), -4), -4), -4)

= -(-(-(((maketimes4 (d d)) 0), -4), -4), -4)
= -(-(-(0, -4), -4), -4)
= -(-(4, -4), -4)
= -(8, -4)
= 12
```

**Notes:**

I'm trying to understand how they figured out `d`.

Let's start with `times4`:

```
let times4 =
  proc (x)
    if zero?(x) then
      0
    else
      -((times4 -(x, 1)), -4)
in (times4 3)
```

That doesn't quite work since `times4` isn't defined in the body of the
procedure. We're currently defining `times4` and `let` doesn't work that way.

But we can use a version of the trick from Exercise 3.23 to get:

```
let maketimes4 =
  proc (f)
    proc (x)
      if zero?(x) then
        0
      else
        -((f -(x, 1)), -4)
in let times4 = (maketimes4 times4)
   in (times4 3)
```

This still doesn't quite work since `times4 = (maketimes4 times4)` will cause
an error due to the fact that the second `times4` isn't defined as before.

But if we expand this definition of `times4` we something very interesting:

```
times4
= (maketimes4 times4)
= (maketimes4 (maketimes4 times4))
= (maketimes4 (maketimes4 (maketimes4 times4)))
...
```

So `times4` is equal to this infinitely recursive expression.

How can we make this infinitely recursive expression?

```
let inf =
  proc (inf)
    (maketimes4 (inf inf))
```

Now,

```
(inf inf)
= (maketimes4 (inf inf))
= (maketimes4 (maketimes4 (inf inf)))
= (maketimes4 (maketimes4 (maketimes4 (inf inf))))
```

Cool, so:

```
times4 = (maketimes4 (inf inf))
```

And we get,

```
let maketimes4 =
  proc (f)
    proc (x)
      if zero?(x) then
        0
      else
        -((f -(x, 1)), -4)
in let inf =
     proc (inf)
       (maketimes4 (inf inf))
   in let times4 = (maketimes4 (inf inf))
      in (times4 3)
```

Finally, we abstract `inf` so that it doesn't depend on `maketimes4`:

```
let makerec =
  proc (f)
    let inf =
      proc (inf)
        (f (inf inf))
    in (inf inf)
in let maketimes4 =
     proc (f)
       proc (x)
         if zero?(x) then
           0
         else
           -((f -(x, 1)), -4)
   in let times4 = (makerec maketimes4)
      in (times4 3)
```

So the key to making recursive functions is `inf`. That's really interesting.
