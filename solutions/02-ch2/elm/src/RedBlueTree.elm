module RedBlueTree exposing
  ( RedBlueTree(..), RedBlueSubtree(..)
  , markLeavesWithRedDepth
  )


type RedBlueTree
  = RedBlueTree RedBlueSubtree


type RedBlueSubtree
  = Red RedBlueSubtree RedBlueSubtree
  | Blue (List RedBlueSubtree)
  | Leaf Int


markLeavesWithRedDepth : RedBlueTree -> RedBlueTree
markLeavesWithRedDepth (RedBlueTree subtree) =
  RedBlueTree (markLeavesWithRedDepthHelper 0 subtree)


markLeavesWithRedDepthHelper : Int -> RedBlueSubtree -> RedBlueSubtree
markLeavesWithRedDepthHelper count subtree =
  case subtree of
    Red left right ->
      let
        newCount =
          count + 1
      in
        Red
          (markLeavesWithRedDepthHelper newCount left)
          (markLeavesWithRedDepthHelper newCount right)

    Blue trees ->
      Blue (List.map (markLeavesWithRedDepthHelper count) trees)

    Leaf _ ->
      Leaf count
