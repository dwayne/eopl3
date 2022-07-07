module Test.InteriorNodeBintree exposing
  ( toList
  , maxInterior
  )


import Expect
import Test exposing (Test, describe, test)

import InteriorNodeBintree as Bintree exposing (Bintree(..), Value(..))


toList : Test
toList =
  describe "toList"
    [ test "example 1" <|
        \_ ->
          Interior "a" (Leaf 3) (Leaf 4)
            |> Bintree.toList
            |> Expect.equal [Num 3, Symbol "a", Num 4]
    ]


maxInterior : Test
maxInterior =
  let
    tree1 =
      Interior "foo" (Leaf 2) (Leaf 3)

    tree2 =
      Interior "bar" (Leaf -1) tree1

    tree3 =
      Interior "baz" tree2 (Leaf 1)
  in
    describe "maxInterior"
      [ test "example 1" <|
          \_ ->
            Bintree.maxInterior tree2
              |> Expect.equal (Just "foo")
      , test "example 2" <|
          \_ ->
            Bintree.maxInterior tree3
              |> Expect.equal (Just "baz")
      ]
