module Test.Stack.Proc exposing
  ( empty
  , push
  , pop
  )


import Expect
import Test exposing (Test, describe, test)

import Stack.Proc as Stack


empty : Test
empty =
  describe "empty"
    [ test "it is empty" <|
        \_ ->
          Stack.empty
            |> Stack.isEmpty
            |> Expect.equal True
    ]


push : Test
push =
  describe "push"
    [ test "it puts the item on top" <|
        \_ ->
          Stack.push 10 (Stack.push 5 Stack.empty)
            |> Stack.top
            |> Expect.equal (Just 10)
    ]


pop : Test
pop =
  describe "pop"
    [ test "it removes the top item" <|
        \_ ->
          Stack.push "read a book" (Stack.push "clean room" Stack.empty)
            |> Stack.pop
            |> Stack.top
            |> Expect.equal (Just "clean room")
    ]
