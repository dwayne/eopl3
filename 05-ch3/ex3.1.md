An expanded calculation of the value of `-(-(x,3), -(v,i))` given that
`ρ = [i=⎡1⎤,v=⎡5⎤,x=⎡10⎤]`.

In addition I list all the places where the fact, `⎣⎡n⎤⎦ = n`, is used.

```
(value-of <<-(-(x,3), -(v,i))>> ρ)

=

⎡(- ⎣(value-of <<-(x,3)>> ρ)⎦
    ⎣(value-of <<-(v,i)>> ρ)⎦)⎤

=

⎡(- ⎣⎡(- ⎣(value-of <<x>> ρ)⎦
         ⎣(value-of <<3>> ρ)⎦)⎤⎦
    ⎣(value-of <<-(v,i)>> ρ)⎦)⎤

= ; use ⎣⎡n⎤⎦ = n

⎡(- (- ⎣(value-of <<x>> ρ)⎦
       ⎣(value-of <<3>> ρ)⎦)
    ⎣(value-of <<-(v,i)>> ρ)⎦)⎤

=

⎡(- (- ⎣⎡10⎤⎦
       ⎣(value-of <<3>> ρ)⎦)
    ⎣(value-of <<-(v,i)>> ρ)⎦)⎤

= ; use ⎣⎡n⎤⎦ = n

⎡(- (- 10
       ⎣(value-of <<3>> ρ)⎦)
    ⎣(value-of <<-(v,i)>> ρ)⎦)⎤

=

⎡(- (- 10
       ⎣⎡3⎤⎦)
    ⎣(value-of <<-(v,i)>> ρ)⎦)⎤

= ; use ⎣⎡n⎤⎦ = n

⎡(- (- 10
       3)
    ⎣(value-of <<-(v,i)>> ρ)⎦)⎤

=

⎡(- 7
    ⎣(value-of <<-(v,i)>> ρ)⎦)⎤

=

⎡(- 7
    ⎣⎡(- ⎣(value-of <<v>> ρ)⎦
         ⎣(value-of <<i>> ρ)⎦)⎤⎦)⎤

= ; use ⎣⎡n⎤⎦ = n

⎡(- 7
    (- ⎣(value-of <<v>> ρ)⎦
       ⎣(value-of <<i>> ρ)⎦))⎤

=

⎡(- 7
    (- ⎣⎡5⎤⎦
       ⎣(value-of <<i>> ρ)⎦))⎤

= ; use ⎣⎡n⎤⎦ = n

⎡(- 7
    (- 5
       ⎣(value-of <<i>> ρ)⎦))⎤

=

⎡(- 7
    (- 5
       ⎣⎡1⎤⎦))⎤

= ; use ⎣⎡n⎤⎦ = n

⎡(- 7
    (- 5
       1))⎤

=

⎡(- 7
    4)⎤

=

⎡3⎤
```
