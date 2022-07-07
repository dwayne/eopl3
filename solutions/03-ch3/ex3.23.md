**Q:** *What is the value of `(times4 3)`?*

```
let makemult =
  proc (maker)
    proc (x)
      if zero?(x) then
        0
      else
        -(((maker maker) -(x, 1)), -4)
in let times4 = proc (x) ((makemult makemult) x)
   in (times4 3)
```

**A:**

```
(times4 3)
= ((makemult makemult) 3)

= -(((makemult makemult) -(3, 1)), -4)
= -(((makemult makemult) 2), -4)

= -(-(((makemult makemult) -(2, 1)), -4), -4)
= -(-(((makemult makemult) 1), -4), -4)

= -(-(-(((makemult makemult) -(1, 1)), -4), -4), -4)
= -(-(-(((makemult makemult) 0), -4), -4), -4)

= -(-(-(0, -4), -4), -4)
= -(-(4, -4), -4)
= -(8, -4)
= 12
```

`makemult` is a curried procedure. You can think of it as a 2-argument
procedure.

It's first argument, `maker`, is a reference to itself.

It's second argument, `x`, is the argument to the function we're defining.

**Q:** *Write a procedure for factorial in PROC*

**A:**

```
x * 0 = 0

x * y = x * ((y-1) + 1)
      = x * (y-1) + x
```

```
A + B = -(A, -(0, B))

Put A = x * -(y, 1) and B = x to get,

x * y = -(x * -(y, 1), -(0, x))
```

```
let timesmaker =
  proc (maker)
    proc (x)
      proc (y)
        if zero?(y) then
          0
        else
          -((((maker maker) x) -(y, 1)), -(0, x))
in let times = proc (x) proc (y) (((timesmaker timesmaker) x) y)
   in let factmaker =
        proc (maker)
          proc (n)
            if zero?(n) then
              1
            else
              ((times n) ((maker maker) -(n, 1)))
      in let fact = proc (n) ((factmaker factmaker) n)
         in (fact 5)
```

`timesmaker` is a procedure that takes a procedure (itself) and returns a
procedure that computes `x * y`.

`factmaker` is a procedure that takes a procedure (itself) and returns a
procedure that computes `n!`.
