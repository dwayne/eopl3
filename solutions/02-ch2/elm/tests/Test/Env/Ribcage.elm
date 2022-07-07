module Test.Env.Ribcage exposing
  ( extendApply
  , tests
  , extendMany
  )


import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz2, test)


import Env.Ribcage as Env


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


extendMany : Test
extendMany =
  let
    env =
      Env.empty
        |> Env.extendMany ["x", "y"] [88, 99]
        |> Env.extendMany ["x", "z"] [66, 77]
        |> Env.extendMany ["a", "b", "c"] [11, 12, 13]
  in
    describe "extendMany"
      [ test "example 1" <|
          \_ ->
            Env.apply env "a"
              |> Expect.equal (Just 11)
      , test "example 2" <|
          \_ ->
            Env.apply env "b"
              |> Expect.equal (Just 12)
      , test "example 3" <|
          \_ ->
            Env.apply env "c"
              |> Expect.equal (Just 13)
      , test "example 4" <|
          \_ ->
            Env.apply env "x"
              |> Expect.equal (Just 66)
      , test "example 5" <|
          \_ ->
            Env.apply env "z"
              |> Expect.equal (Just 77)
      , test "example 6" <|
          \_ ->
            Env.apply env "y"
              |> Expect.equal (Just 99)
      , test "example 7" <|
          \_ ->
            Env.apply env "d"
              |> Expect.equal Nothing
      ]
