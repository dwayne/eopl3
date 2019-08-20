module BiBintreeWrong exposing
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


-- This was the first solution that I came up with but
-- I realized it wouldn't work.
--
-- What's wrong with this representation and implementation?


type BiBintree
  = BiBintree Bintree (List Bintree)


type Bintree
  = Leaf
  | Branch Int Bintree Bintree


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
      Just (BiBintree left (tree :: ancestors))


moveToRight : BiBintree -> Maybe BiBintree
moveToRight (BiBintree tree ancestors) =
  case tree of
    Leaf ->
      Nothing

    Branch _ _ right ->
      Just (BiBintree right (tree :: ancestors))


moveUp : BiBintree -> Maybe BiBintree
moveUp (BiBintree _ ancestors) =
  case ancestors of
    [] ->
      Nothing

    tree :: trees ->
      Just (BiBintree tree trees)


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


-- Consider what happens when you move around left and right and then
-- insert into the tree. The ancestors aren't updated so that when you
-- move up the changes made to the tree via insertion aren't reflected
-- in the list of ancestors.
