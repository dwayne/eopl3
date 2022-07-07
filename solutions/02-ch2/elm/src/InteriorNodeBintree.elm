module InteriorNodeBintree exposing
  ( Bintree(..), Value(..)
  , toList
  , maxInterior
  )


type Bintree
  = Leaf Int
  | Interior String Bintree Bintree


type Value
  = Num Int
  | Symbol String


-- Exercise 2.24
toList : Bintree -> List Value
toList tree =
  case tree of
    Leaf n ->
      [Num n]

    Interior s left right ->
      toList left ++ [Symbol s] ++ toList right


-- Exercise 2.25
maxInterior : Bintree -> Maybe String
maxInterior tree =
  case maxInteriorHelper tree of
    (_, Nothing) ->
      Nothing

    (_, Just (s, _)) ->
      Just s


maxInteriorHelper : Bintree -> (Int, Maybe (String, Int))
maxInteriorHelper tree =
  case tree of
    Leaf n ->
      (n, Nothing)

    Interior s left right ->
      let
        (l, maybeLMax) =
          maxInteriorHelper left

        (r, maybeRMax) =
          maxInteriorHelper right

        sum =
          l + r
      in
        ( sum
        , Just <|
            case (maybeLMax, maybeRMax) of
              (Nothing, Nothing) ->
                (s, sum)

              (Just (t, lmax), Nothing) ->
                maximum (s, sum) (t, lmax)

              (Nothing, Just (u, rmax)) ->
                maximum (s, sum) (u, rmax)

              (Just (t, lmax), Just (u, rmax)) ->
                maximum (maximum (s, sum) (t, lmax)) (u, rmax)
        )


maximum : (String, Int) -> (String, Int) -> (String, Int)
maximum (s, m) (t, n) =
  if m >= n then
    (s, m)
  else
    (t, n)
