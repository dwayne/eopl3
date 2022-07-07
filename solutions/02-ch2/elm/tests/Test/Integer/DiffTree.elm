module Test.Integer.DiffTree exposing
  ( zero
  , isZero
  , predSucc
  , succPred
  , zeroIsIdentityForAddition
  , additionIsCommutative
  )


import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, test)


import Integer.DiffTree as Integer exposing (Integer)


zero : Test
zero =
  describe "zero"
    [ test "it returns a representation of zero" <|
        \_ ->
          Integer.zero
            |> Integer.toInt
            |> Expect.equal 0
    ]


isZero : Test
isZero =
  fuzz integerFuzzer "isZero n == True iff ⎡n⎤ == 0" <|
    \n ->
      Integer.isZero n
        |> Expect.equal ((Integer.toInt n) == 0)


predSucc : Test
predSucc =
  fuzz integerFuzzer "pred (succ n) == n for all integers n" <|
    \n ->
      Integer.pred (Integer.succ n)
        |> Integer.toInt
        |> Expect.equal (Integer.toInt n)


succPred : Test
succPred =
  fuzz integerFuzzer "succ (pred n) == n for all integers n" <|
    \n ->
      Integer.succ (Integer.pred n)
        |> Integer.toInt
        |> Expect.equal (Integer.toInt n)


zeroIsIdentityForAddition : Test
zeroIsIdentityForAddition =
  fuzz integerFuzzer "plus zero n == n for all integers n" <|
    \n ->
      Integer.plus Integer.zero n
        |> Integer.toInt
        |> Expect.equal (Integer.toInt n)


additionIsCommutative : Test
additionIsCommutative =
  let
    ab =
      Fuzz.tuple (integerFuzzer, integerFuzzer)
  in
    fuzz ab "plus a b == plus b a for all integers a, b" <|
      \(a, b) ->
        Integer.plus a b
          |> Integer.toInt
          |> Expect.equal (Integer.toInt (Integer.plus b a))


-- HELPERS


integerFuzzer : Fuzzer Integer
integerFuzzer =
  Fuzz.map Integer.fromInt (Fuzz.intRange -100 100)
