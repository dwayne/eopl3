module Test.RedBlueTree exposing (markLeavesWithRedDepth)


import Expect
import Test exposing (Test, describe, test)

import RedBlueTree exposing (RedBlueTree(..), RedBlueSubtree(..))


markLeavesWithRedDepth : Test
markLeavesWithRedDepth =
  describe "markLeavesWithRedDepth"
    [ test "example 1" <|
        \_ ->
          let
            input =
              RedBlueTree <|
                Red
                  (Blue [Leaf 26, Leaf 12])
                  (Red
                    (Leaf 11)
                    (Blue [Leaf 117, Leaf 14]))

            output =
              RedBlueTree <|
                Red
                  (Blue [Leaf 1, Leaf 1])
                  (Red
                    (Leaf 2)
                    (Blue [Leaf 2, Leaf 2]))
          in
            RedBlueTree.markLeavesWithRedDepth input
              |> Expect.equal output
    ]
