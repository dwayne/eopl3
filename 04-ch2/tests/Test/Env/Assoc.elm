module Test.Env.Assoc exposing
  ( extendApply
  , tests
  , isEmpty
  , hasBinding
  , extendMany
  )


import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz2, test)


import Env.Assoc as Env


extendApply : Test
extendApply =
  fuzz2 Fuzz.string Fuzz.int "apply (extend var value env) var = value for all var, value, env" <|
    \var value ->
      Env.apply (Env.extend var value Env.empty) var
        |> Expect.equal (Just value)
-- TODO:
--
-- Write a custom fuzzer for generating environments and use it in place of
-- Env.empty to improve the test.


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


isEmpty : Test
isEmpty =
  describe "isEmpty"
    [ test "the empty environment is empty" <|
        \_ ->
          Env.isEmpty Env.empty
            |> Expect.equal True
    , test "a non-empty environment is not empty" <|
        \_ ->
          Env.isEmpty (Env.extend "a" 1 Env.empty)
            |> Expect.equal False
    ]


hasBinding : Test
hasBinding =
  let
    env =
      Env.empty
        |> Env.extend "a" 1
        |> Env.extend "b" 2
  in
    describe "hasBinding"
      [ test "it has a binding for a" <|
          \_ ->
            Env.hasBinding env "a"
              |> Expect.equal True
      , test "it has a binding for b" <|
          \_ ->
            Env.hasBinding env "b"
              |> Expect.equal True
      , test "it does not have a binding for c" <|
          \_ ->
            Env.hasBinding env "c"
              |> Expect.equal False
      ]


extendMany : Test
extendMany =
  let
    env =
      Env.empty
        |> Env.extendMany ["d", "y", "x", "y"] [6, 8, 7, 14]
  in
    describe "extendMany"
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
