module Bintree exposing
  ( Bintree

  , fromInt
  , current

  , moveToLeft
  , moveToRight

  , atLeaf

  , insertToLeft
  , insertToRight
  )


type Bintree
  = Leaf
  | Branch Int Bintree Bintree


fromInt : Int -> Bintree
fromInt n =
  Branch n Leaf Leaf


current : Bintree -> Maybe Int
current tree =
  case tree of
    Leaf ->
      Nothing

    Branch n _ _ ->
      Just n


moveToLeft : Bintree -> Maybe Bintree
moveToLeft tree =
  case tree of
    Leaf ->
      Nothing

    Branch _ left _ ->
      Just left


moveToRight : Bintree -> Maybe Bintree
moveToRight tree =
  case tree of
    Leaf ->
      Nothing

    Branch _ _ right ->
      Just right


atLeaf : Bintree -> Bool
atLeaf tree =
  case tree of
    Leaf ->
      True

    Branch _ _ _ ->
      False


insertToLeft : Int -> Bintree -> Bintree
insertToLeft n tree =
  case tree of
    Leaf ->
      Branch n Leaf Leaf

    Branch m left right ->
      Branch m (Branch n left Leaf) right


insertToRight : Int -> Bintree -> Bintree
insertToRight n tree =
  case tree of
    Leaf ->
      Branch n Leaf Leaf

    Branch m left right ->
      Branch m left (Branch n Leaf right)
