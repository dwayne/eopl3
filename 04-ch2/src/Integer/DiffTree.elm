module Integer.DiffTree exposing
  ( Integer
  , zero
  , isZero
  , succ
  , pred

  , fromInt, toInt

  , plus
  )


type Integer
  = Integer DiffTree


type DiffTree
  = One
  | Diff DiffTree DiffTree


zero : Integer
zero =
  Integer (Diff One One)


isZero : Integer -> Bool
isZero (Integer t) =
  case t of
    One ->
      False

    Diff left right ->
      left == right


succ : Integer -> Integer
succ (Integer t) =
  Integer (Diff t negativeOne)


pred : Integer -> Integer
pred (Integer t) =
  Integer (Diff t one)


one : DiffTree
one =
  One


negativeOne : DiffTree
negativeOne =
  Diff (Diff one one) one


fromInt : Int -> Integer
fromInt n =
  if n == 0 then
    zero
  else if n > 0 then
    succ (fromInt (n-1))
  else
    pred (fromInt (n+1))


toInt : Integer -> Int
toInt (Integer t) =
  toIntHelper t


toIntHelper : DiffTree -> Int
toIntHelper t =
  case t of
    One ->
      1

    Diff left right ->
      toIntHelper left - toIntHelper right


plus : Integer -> Integer -> Integer
plus (Integer t1) (Integer t2) =
  Integer (Diff t1 (Diff (Diff one one) t2))
  -- t1 - (0 - t2)
  -- = t1 - (-t2)
  -- = t1 + t2
