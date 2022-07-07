module Test.BiBintree exposing (suite)


import Expect
import Test exposing (Test, describe, test)

import BiBintree


suite : Test
suite =
  let
    tree =
      BiBintree.fromInt 13
        |> BiBintree.insertToLeft 12
        |> BiBintree.insertToRight 14
  in
    describe "BiBintree test suite"
      [ test "example 1" <|
          \_ ->
            tree
              |> BiBintree.moveToLeft
              |> Maybe.map (BiBintree.insertToLeft 15)
              |> Maybe.andThen BiBintree.moveUp
              |> Maybe.andThen BiBintree.moveToLeft
              |> Maybe.andThen BiBintree.moveToLeft
              |> Maybe.andThen BiBintree.current
              |> Expect.equal (Just 15)
      ]
