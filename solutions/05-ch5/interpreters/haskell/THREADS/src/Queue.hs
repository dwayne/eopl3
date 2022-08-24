module Queue
  ( Queue
  , empty
  , enqueue, dequeue
  ) where


data Queue a
  = Queue [a] [a]


empty :: Queue a
empty =
  Queue [] []


enqueue :: a -> Queue a -> Queue a
enqueue x (Queue front revBack) =
  Queue front (x : revBack)


dequeue :: Queue a -> (Maybe a, Queue a)
dequeue q@(Queue front revBack) =
  case (front, revBack) of
    ([], []) ->
      (Nothing, q)

    (x:restFront, _) ->
      (Just x, Queue restFront revBack)

    _ ->
      dequeue (Queue (reverse revBack) [])
