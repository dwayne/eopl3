module BiBintree exposing
  ( BiBintree

  , fromInt
  , current

  , moveToLeft
  , moveToRight
  , moveUp

  , atRoot
  , atLeaf

  , insertToLeft
  , insertToRight
  )


type BiBintree
  = BiBintree Bintree (List (Direction, Bintree))


type Bintree
  = Leaf
  | Branch Int Bintree Bintree


type Direction
  = Left
  | Right


fromInt : Int -> BiBintree
fromInt n =
  BiBintree (Branch n Leaf Leaf) []


current : BiBintree -> Maybe Int
current (BiBintree tree _) =
  case tree of
    Leaf ->
      Nothing

    Branch n _ _ ->
      Just n


moveToLeft : BiBintree -> Maybe BiBintree
moveToLeft (BiBintree tree ancestors) =
  case tree of
    Leaf ->
      Nothing

    Branch _ left _ ->
      Just (BiBintree left ((Left, tree) :: ancestors))


moveToRight : BiBintree -> Maybe BiBintree
moveToRight (BiBintree tree ancestors) =
  case tree of
    Leaf ->
      Nothing

    Branch _ _ right ->
      Just (BiBintree right ((Right, tree) :: ancestors))


moveUp : BiBintree -> Maybe BiBintree
moveUp (BiBintree tree ancestors) =
  case ancestors of
    [] ->
      Nothing

    (dir, parent) :: trees ->
      case (dir, parent) of
        (Left, Branch n _ right) ->
          Just (BiBintree (Branch n tree right) trees)

        (Right, Branch n left _) ->
          Just (BiBintree (Branch n left tree) trees)

        _ ->
          -- This will never happen since the ancestor list
          -- never contains a Leaf.
          Nothing


atLeaf : BiBintree -> Bool
atLeaf (BiBintree tree _) =
  case tree of
    Leaf ->
      True

    Branch _ _ _ ->
      False


atRoot : BiBintree -> Bool
atRoot (BiBintree _ ancestors) =
  List.isEmpty ancestors


insertToLeft : Int -> BiBintree -> BiBintree
insertToLeft n (BiBintree tree ancestors) =
  case tree of
    Leaf ->
      BiBintree (Branch n Leaf Leaf) ancestors

    Branch m left right ->
      BiBintree (Branch m (Branch n left Leaf) right) ancestors


insertToRight : Int -> BiBintree -> BiBintree
insertToRight n (BiBintree tree ancestors) =
  case tree of
    Leaf ->
      BiBintree (Branch n Leaf Leaf) ancestors

    Branch m left right ->
      BiBintree (Branch m left (Branch n Leaf right)) ancestors
