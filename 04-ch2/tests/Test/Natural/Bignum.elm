module Test.Natural.Bignum exposing
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


import Natural.Bignum as Natural exposing (Natural)


zero : Test
zero =
  describe "zero"
    [ test "it returns a representation of zero" <|
        \_ ->
          Natural.zero
            |> Natural.toInt
            |> Expect.equal 0
    ]


isZero : Test
isZero =
  fuzz naturalFuzzer "isZero n == True iff ⎡n⎤ == 0" <|
    \n ->
      Natural.isZero n
        |> Expect.equal ((Natural.toInt n) == 0)


predSucc : Test
predSucc =
  fuzz naturalFuzzer "pred (succ n) == n for all natural n" <|
    \n ->
      Natural.pred (Natural.succ n)
        |> Expect.equal n


succPred : Test
succPred =
  fuzz nonZeroNaturalFuzzer "succ (pred n) == n for all natural n > 0" <|
    \n ->
      Natural.succ (Natural.pred n)
        |> Expect.equal n


zeroIsIdentityForAddition : Test
zeroIsIdentityForAddition =
  fuzz naturalFuzzer "plus zero n == n for all natural n" <|
    \n ->
      Natural.plus Natural.zero n
        |> Expect.equal n


additionIsCommutative : Test
additionIsCommutative =
  let
    ab =
      Fuzz.tuple (naturalFuzzer, naturalFuzzer)
  in
    fuzz ab "plus a b == plus b a for all natural a, b" <|
      \(a, b) ->
        Natural.plus a b
          |> Expect.equal (Natural.plus b a)


-- HELPERS


naturalFuzzer : Fuzzer Natural
naturalFuzzer =
  Fuzz.map Natural.fromInt (Fuzz.intRange 0 100)


nonZeroNaturalFuzzer : Fuzzer Natural
nonZeroNaturalFuzzer =
  Fuzz.map Natural.fromInt (Fuzz.intRange 1 100)
