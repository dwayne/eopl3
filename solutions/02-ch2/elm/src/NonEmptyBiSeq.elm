module NonEmptyBiSeq exposing
  ( NonEmptyBiSeq

  , fromInt -- number->sequence
  , current -- current-element

  , moveToLeft -- move-to-left
  , moveToRight -- move-to-right

  , insertToLeft -- insert-to-left
  , insertToRight -- insert-to-right

  , atLeftEnd -- at-left-end?
  , atRightEnd -- at-right-end?

  , toList
  )


type NonEmptyBiSeq
  = NonEmptyBiSeq Int (List Int) (List Int)


fromInt : Int -> NonEmptyBiSeq
fromInt n =
  NonEmptyBiSeq n [] []


current : NonEmptyBiSeq -> Int
current (NonEmptyBiSeq n _ _) =
  n


moveToLeft : NonEmptyBiSeq -> Maybe NonEmptyBiSeq
moveToLeft (NonEmptyBiSeq n left right) =
  case left of
    [] ->
      Nothing

    x :: xs ->
      Just (NonEmptyBiSeq x xs (n :: right))


moveToRight : NonEmptyBiSeq -> Maybe NonEmptyBiSeq
moveToRight (NonEmptyBiSeq n left right) =
  case right of
    [] ->
      Nothing

    x :: xs ->
      Just (NonEmptyBiSeq x (n :: left) xs)


insertToLeft : Int -> NonEmptyBiSeq -> NonEmptyBiSeq
insertToLeft m (NonEmptyBiSeq n left right) =
  NonEmptyBiSeq n (m :: left) right


insertToRight : Int -> NonEmptyBiSeq -> NonEmptyBiSeq
insertToRight m (NonEmptyBiSeq n left right) =
  NonEmptyBiSeq n left (m :: right)


atLeftEnd : NonEmptyBiSeq -> Bool
atLeftEnd (NonEmptyBiSeq _ left _) =
  List.isEmpty left


atRightEnd : NonEmptyBiSeq -> Bool
atRightEnd (NonEmptyBiSeq _ _ right) =
  List.isEmpty right


toList : NonEmptyBiSeq -> List Int
toList (NonEmptyBiSeq n left right) =
  List.reverse left ++ [n] ++ right
