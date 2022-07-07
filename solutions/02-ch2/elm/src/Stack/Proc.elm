module Stack.Proc exposing
  ( Stack
  , empty
  , push
  , pop
  , top
  , isEmpty
  )


-- A procedural representation for stacks.
--
-- The interface we need to implement has two observers, top and isEmpty. Each
-- observer needs to be represented by its action under application.
--
-- Hence, a stack will be represented by a pair of functions one for top and
-- the other for isEmpty.


type Stack a
  = Stack
      ( () -> (Maybe a, Maybe (Stack a)) -- top
      , () -> Bool -- isEmpty
      )


empty : Stack a
empty =
  Stack
    ( \_ -> (Nothing, Nothing)
    , \_ -> True
    )


push : a -> Stack a -> Stack a
push x stack =
  Stack
    ( \_ -> (Just x, Just stack)
    , \_ -> False
    )


pop : Stack a -> Stack a
pop (Stack (currentTop, _)) =
  Maybe.withDefault empty (Tuple.second (currentTop ()))


top : Stack a -> Maybe a
top (Stack (currentTop, _)) =
  Tuple.first (currentTop ())


isEmpty : Stack a -> Bool
isEmpty (Stack (_, currentIsEmpty)) =
  currentIsEmpty ()
