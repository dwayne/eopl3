module Natural.Unary exposing
  ( Natural
  , zero
  , isZero
  , succ
  , pred

  , fromInt, toInt

  , plus
  )


type Natural = Natural (List ())


zero : Natural
zero =
  Natural []


isZero : Natural -> Bool
isZero (Natural n) =
  List.isEmpty n


succ : Natural -> Natural
succ (Natural n) =
  Natural (() :: n)


pred : Natural -> Natural
pred (Natural n) =
  Natural (Maybe.withDefault [] (List.tail n))


fromInt : Int -> Natural
fromInt n =
  Natural (List.repeat n ())


toInt : Natural -> Int
toInt (Natural n) =
  List.length n


plus : Natural -> Natural -> Natural
plus a b =
  if isZero a then
    b
  else
    succ (plus (pred a) b)
