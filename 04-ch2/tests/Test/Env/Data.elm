module Test.Env.Data exposing
  ( extendApply
  , tests
  )


import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz2, test)


import Env.Data as Env


extendApply : Test
extendApply =
  fuzz2 Fuzz.string Fuzz.int "apply (extend var value env) var = value for all var, value, env" <|
    \var value ->
      Env.apply (Env.extend var value Env.empty) var
        |> Expect.equal (Just value)


tests : Test
tests =
  let
    env =
      Env.empty
        |> Env.extend "y" 14
        |> Env.extend "x" 7
        |> Env.extend "y" 8
        |> Env.extend "d" 6
  in
    describe "tests"
      [ test "example 1" <|
          \_ ->
            Env.apply env "d"
              |> Expect.equal (Just 6)
      , test "example 2" <|
          \_ ->
            Env.apply env "y"
              |> Expect.equal (Just 8)
      , test "example 3" <|
          \_ ->
            Env.apply env "x"
              |> Expect.equal (Just 7)
      ]
