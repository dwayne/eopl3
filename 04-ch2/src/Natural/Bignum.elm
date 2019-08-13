module Natural.Bignum exposing
  ( Natural
  , zero
  , isZero
  , succ
  , pred

  , fromInt, toInt

  , plus
  )


type Natural = Natural (List Int)


base : Int
base = 16


zero : Natural
zero =
  Natural []


isZero : Natural -> Bool
isZero (Natural n) =
  List.isEmpty n


succ : Natural -> Natural
succ (Natural n) =
  Natural (succHelper n)


succHelper : List Int -> List Int
succHelper bigits =
  case bigits of
    [] ->
      [1]

    b :: rest ->
      let
        next =
          b + 1
      in
        if next < base then
          next :: rest
        else
          0 :: succHelper rest


pred : Natural -> Natural
pred (Natural n) =
  Natural (predHelper n)


predHelper : List Int -> List Int
predHelper bigits =
  case bigits of
    [] ->
      []

    b :: rest ->
      if b == 0 then
        (base - 1) :: predHelper rest
      else if b == 1 && List.isEmpty rest then
        []
      else
        (b - 1) :: rest


fromInt : Int -> Natural
fromInt n =
  Natural (toBase (max 0 n))


toBase : Int -> List Int
toBase n =
  if n == 0 then
    []
  else
    let
      r =
        modBy base n

      q =
        n // base
    in
      r :: toBase q


toInt : Natural -> Int
toInt (Natural n) =
  fromBase n 1


fromBase : List Int -> Int -> Int
fromBase bigits pow =
  case bigits of
    [] ->
      0

    b :: rest ->
      b * pow + fromBase rest (pow * base)


plus : Natural -> Natural -> Natural
plus a b =
  if isZero a then
    b
  else
    succ (plus (pred a) b)
