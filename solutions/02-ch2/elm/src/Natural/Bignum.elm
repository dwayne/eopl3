module Natural.Bignum exposing
  ( Natural
  , zero
  , isZero
  , succ
  , pred

  , fromInt, toInt

  , plus

  , fact
  )


type Natural = Natural (List Int)


base : Int
base = 64


zero : Natural
zero =
  Natural []


isZero : Natural -> Bool
isZero (Natural n) =
  List.isEmpty n


succ : Natural -> Natural
succ (Natural n) =
  Natural (succHelper n [])


succHelper : List Int -> List Int -> List Int
succHelper bigits zeros =
  case bigits of
    [] ->
      List.append zeros [1]

    b :: rest ->
      let
        next =
          b + 1
      in
        if next < base then
          List.append zeros (next :: rest)
        else
          succHelper rest (0 :: zeros)


pred : Natural -> Natural
pred (Natural n) =
  Natural (predHelper n [])


predHelper : List Int -> List Int -> List Int
predHelper bigits bases =
  case bigits of
    [] ->
      bases

    [1] ->
      bases

    b :: rest ->
      if b == 0 then
        predHelper rest ((base - 1) :: bases)
      else
        List.append bases ((b - 1) :: rest)


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
  plusHelper a b


plusHelper : Natural -> Natural -> Natural
plusHelper a sum =
  if isZero a then
    sum
  else
    plusHelper (pred a) (succ sum)


-- Exercise 2.1
--
-- 10! = 10 x 9 x 8 x ... x 1
--
-- So, we need to be able to do multiplication.

mult : Natural -> Natural -> Natural
mult a b =
  multHelper a b zero


multHelper : Natural -> Natural -> Natural -> Natural
multHelper a b result =
  if isZero a then
    result
  else
    multHelper (pred a) b (plus b result)


-- 0! = 1

one : Natural
one =
  succ zero


fact : Natural -> Natural
fact n =
  factHelper n one


factHelper : Natural -> Natural -> Natural
factHelper n result =
  if isZero n then
    result
  else
    factHelper (pred n) (mult n result)

-- I find that 12! still takes a long time to compute.
--
-- As n increases, the execution time of fact n increases.
--
-- As the base changes the execution time will vary as well. For e.g. using
-- a larger base will result in fewer bigits being used and so shorter lists
-- will be involved in the calculations making the execution time decrease.
--
-- N.B. These are qualitative remarks. The math gets involved when I try to
-- quantify the execution time.
