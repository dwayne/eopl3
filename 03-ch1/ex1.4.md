A derivation from `List-of-Int` to `(-7 . (3 . (14 . ())))`.

```
List-of-Int
=> (Int . List-of-Int)
=> (Int . (Int . List-of-Int))
=> (Int . (Int . (Int . List-of-Int)))
=> (Int . (Int . (Int . ())))
=> (Int . (Int . (14 . ())))
=> (Int . (3 . (14 . ())))
=> (-7 . (3 . (14 . ())))
```
