module Test.Bintree exposing (suite)


import Expect
import Test exposing (Test, describe, test)

import Bintree


suite : Test
suite =
  let
    tree =
      Bintree.fromInt 13
        |> Bintree.insertToLeft 12
        |> Bintree.insertToRight 14
  in
    describe "Bintree test suite"
      [ test "example 1" <|
          \_ ->
            tree
              |> Bintree.current
              |> Expect.equal (Just 13)
      , test "example 2" <|
          \_ ->
            tree
              |> Bintree.moveToLeft
              |> Maybe.andThen Bintree.current
              |> Expect.equal (Just 12)
      , test "example 3" <|
          \_ ->
            tree
              |> Bintree.moveToRight
              |> Maybe.andThen Bintree.current
              |> Expect.equal (Just 14)
      , test "example 4" <|
          \_ ->
            tree
              |> Bintree.moveToLeft
              |> Maybe.andThen Bintree.moveToRight
              |> Maybe.map Bintree.atLeaf
              |> Expect.equal (Just True)
      , test "example 5" <|
          \_ ->
            tree
              |> Bintree.insertToLeft 15
              |> Bintree.moveToLeft
              |> Maybe.andThen Bintree.moveToLeft
              |> Maybe.andThen Bintree.current
              |> Expect.equal (Just 12)
      ]
