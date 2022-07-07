module Test.NonEmptyBiSeq exposing
  ( fromInt
  , moveToLeft
  , moveToRight
  , insertToLeft
  , insertToRight
  )


import Expect
import Test exposing (Test, describe, test)


import NonEmptyBiSeq as Seq exposing (NonEmptyBiSeq)


fromInt : Test
fromInt =
  describe "fromInt"
    [ test "example 1" <|
        \_ ->
          Seq.fromInt 5
            |> Seq.current
            |> Expect.equal 5
    , test "example 2" <|
        \_ ->
          Seq.fromInt 5
            |> Seq.atLeftEnd
            |> Expect.equal True
    , test "example 3" <|
        \_ ->
          Seq.fromInt 5
            |> Seq.atRightEnd
            |> Expect.equal True
    ]


seq : NonEmptyBiSeq
seq =
  Seq.fromInt 6
    |> Seq.insertToLeft 1
    |> Seq.insertToLeft 2
    |> Seq.insertToLeft 3
    |> Seq.insertToLeft 4
    |> Seq.insertToLeft 5
    |> Seq.insertToRight 9
    |> Seq.insertToRight 8
    |> Seq.insertToRight 7


moveToLeft : Test
moveToLeft =
  describe "moveToLeft"
    [ test "example 1" <|
        \_ ->
          Seq.moveToLeft seq
            |> Maybe.map Seq.current
            |> Expect.equal (Just 5)
    ]


moveToRight : Test
moveToRight =
  describe "moveToRight"
    [ test "example 1" <|
        \_ ->
          Seq.moveToRight seq
            |> Maybe.map Seq.current
            |> Expect.equal (Just 7)
    ]


insertToLeft : Test
insertToLeft =
  describe "insertToLeft"
    [ test "example 1" <|
        \_ ->
          Seq.insertToLeft 10 seq
            |> Seq.toList
            |> Expect.equal [1, 2, 3, 4, 5, 10, 6, 7, 8, 9]
    ]


insertToRight : Test
insertToRight =
  describe "insertToRight"
    [ test "example 1" <|
        \_ ->
          Seq.insertToRight 10 seq
            |> Seq.toList
            |> Expect.equal [1, 2, 3, 4, 5, 6, 10, 7, 8, 9]
    ]
