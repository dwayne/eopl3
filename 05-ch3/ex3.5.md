A derivation of figure 3.5 as a derivation tree.

```
(value-of <<7>> ρ)
= ⎡7⎤
--------------------------------
(value-of
  <<let x = 7
    in let y = 2
       in let y = let x = -(x,1)
                  in -(x,y)
          in -(-(x,8),y)>>
  ρ)
=
(value-of
  <<let y = 2
    in let y = let x = -(x,1)
               in -(x,y)
       in -(-(x,8),y)>>
  [x=⎡7⎤]ρ)




(value-of <<2>> [x=⎡7⎤]ρ)
= ⎡2⎤
-----------------------------
(value-of
  <<let y = 2
    in let y = let x = -(x,1)
               in -(x,y)
       in -(-(x,8),y)>>
  [x=⎡7⎤]ρ)
=
(value-of
  <<let y = let x = -(x,1)
            in -(x,y)
    in -(-(x,8),y)>>
  [y=⎡2⎤][x=⎡7⎤]ρ)




(value-of <<-(x,1)>> [y=⎡2⎤][x=⎡7⎤]ρ)
= ⎡6⎤
--------------------------------------------
(value-of
  <<let x = -(x,1)
    in -(x,y)>>
  [y=⎡2⎤][x=⎡7⎤]ρ)
=
(value-of <<-(x,y)>> [x=⎡6⎤][y=⎡2⎤][x=⎡7⎤]ρ)
= ⎡4⎤
-------------------------------------------------
(value-of
  <<let y = let x = -(x,1)
            in -(x,y)
    in -(-(x,8),y)>>
  [y=⎡2⎤][x=⎡7⎤]ρ)
=
(value-of <<-(-(x,8),y)>> [y=⎡4⎤][y=⎡2⎤][x=⎡7⎤]ρ)
= ⎡-5⎤
```
