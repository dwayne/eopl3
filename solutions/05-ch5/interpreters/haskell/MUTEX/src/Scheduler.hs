module Scheduler
  ( Scheduler
  , new
  , setFinalAnswer
  , isTimeExpired
  , schedule
  , runNextThread
  , tick
  ) where


import qualified Queue
import qualified Thread

import Queue (Queue)
import Thread (Thread)


data Scheduler a
  = Scheduler
      { _readyQueue :: Queue (Thread a)
      , _finalAnswer :: Maybe a
      , _maxTimeSlice :: Int
      , _timeRemaining :: Int
      }


new :: Int -> Scheduler a
new maxTimeSlice =
  -- NOTE:
  --
  -- 1. We assume maxTimeSlice is positive.
  -- 2. We can improve the API by giving maxTimeSlice a type that is positive.
  Scheduler
    { _readyQueue = Queue.empty
    , _finalAnswer = Nothing
    , _maxTimeSlice = maxTimeSlice
    , _timeRemaining = maxTimeSlice
    }


setFinalAnswer :: a -> Scheduler a -> Scheduler a
setFinalAnswer a scheduler =
  scheduler
    { _finalAnswer = Just a
    }


isTimeExpired :: Scheduler a -> Bool
isTimeExpired =
  (<= 0) . _timeRemaining


-- NOTE: This is called place-on-ready-queue! in the book.
schedule :: Thread a -> Scheduler a -> Scheduler a
schedule thread scheduler@(Scheduler { _readyQueue = readyQueue }) =
  scheduler
    { _readyQueue = Queue.enqueue thread readyQueue
    }


runNextThread :: Scheduler a -> (Maybe a, Scheduler a)
runNextThread scheduler@(Scheduler { _readyQueue = readyQueue, _finalAnswer = finalAnswer, _maxTimeSlice = maxTimeSlice }) =
  let
    (maybeThread, newReadyQueue) =
      Queue.dequeue readyQueue
  in
  case maybeThread of
    Nothing ->
      ( finalAnswer
      , scheduler
      )

    Just thread ->
      ( Just $ Thread.run thread
      , scheduler
          { _readyQueue = newReadyQueue
          , _timeRemaining = maxTimeSlice
          }
      )


-- NOTE: This is called decrement-timer! in the book.
tick :: Scheduler a -> Scheduler a
tick scheduler@(Scheduler { _timeRemaining = timeRemaining }) =
  scheduler
    { _timeRemaining = timeRemaining - 1
    }
