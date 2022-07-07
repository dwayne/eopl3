module Natural.Elm exposing
  ( Natural
  , zero
  , isZero
  , succ
  , pred

  , fromInt, toInt

  , plus
  )


type Natural = Natural Int


zero : Natural
zero =
  Natural 0


isZero : Natural -> Bool
isZero (Natural n) =
  n == 0


succ : Natural -> Natural
succ (Natural n) =
  Natural (n + 1)


pred : Natural -> Natural
pred (Natural n) =
  Natural (max 0 (n - 1))


fromInt : Int -> Natural
fromInt n =
  Natural (max 0 n)


toInt : Natural -> Int
toInt (Natural n) =
  n


plus : Natural -> Natural -> Natural
plus a b =
  if isZero a then
    b
  else
    succ (plus (pred a) b)
