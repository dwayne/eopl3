From Exercise 3.32:

```
letrec
  even(x) = if zero?(x) then 1 else (odd -(x, 1))
  odd(x) = if zero?(x) then 0 else (even -(x, 1))
in (odd 13)
```

Using the tricks of Exercise 3.23 we can write the pair of mutually recursive
procedures, `odd` and `even`, as follows:

```
let evenmaker =
  proc (evenmaker)
    proc (oddmaker)
      proc (x)
        if zero?(x) then
          1
        else
          (((oddmaker oddmaker) evenmaker) -(x, 1))
in let oddmaker =
     proc (oddmaker)
       proc (evenmaker)
         proc (x)
           if zero?(x) then
             0
           else
             (((evenmaker evenmaker) oddmaker) -(x, 1))
   in let odd = ((oddmaker oddmaker) evenmaker)
      in (odd 13)
```

**N.B.**

```
even = ((evenmaker evenmaker) oddmaker)
```
