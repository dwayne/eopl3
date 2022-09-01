module Mutex
  ( Mutex
  , new
  , isClosed
  , open, close
  , enqueue, dequeue
  ) where


import qualified Queue

import Queue (Queue)
import Thread (Thread)


data Mutex a
  = Mutex
      { _isClosed :: Bool
      , _waitQueue :: Queue (Thread a)
      }


new :: Mutex a
new =
  Mutex
    { _isClosed = False
    , _waitQueue = Queue.empty
    }


isClosed :: Mutex a -> Bool
isClosed = _isClosed


open :: Mutex a -> Mutex a
open mutex =
  mutex { _isClosed = False }


close :: Mutex a -> Mutex a
close mutex =
  mutex { _isClosed = True }


enqueue :: Thread a -> Mutex a -> Mutex a
enqueue thread mutex@(Mutex { _waitQueue = waitQueue }) =
  mutex { _waitQueue = Queue.enqueue thread waitQueue }


dequeue :: Mutex a -> (Maybe (Thread a), Mutex a)
dequeue mutex@(Mutex { _waitQueue = waitQueue }) =
  let
    (maybeThread, newWaitQueue) =
      Queue.dequeue waitQueue
  in
  ( maybeThread
  , mutex { _waitQueue = newWaitQueue }
  )
