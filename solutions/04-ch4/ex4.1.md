```
let g =
  proc (dummy)
    let counter = newref(0)
    in
      begin
        setref(counter, -(deref(counter), -1));
        deref(counter)
      end
in let a = (g 11)
   in let b = (g 11)
      in -(a, b)
```

A new reference, counter, is created each time `g` is called.

```
a = 1
b = 1
-(a, b) = 0
```
