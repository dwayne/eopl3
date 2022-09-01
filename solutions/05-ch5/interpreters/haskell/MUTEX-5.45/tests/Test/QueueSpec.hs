module Test.QueueSpec (spec) where


import qualified Queue

import Queue (Queue)
import Test.Hspec


spec :: Spec
spec =
  describe "queue" $ do
    it "example 1" $ do
      let q = Queue.empty :: Queue Int

      fst (Queue.dequeue q) `shouldBe` Nothing

    it "example 2" $ do
      let q = Queue.enqueue 5 Queue.empty :: Queue Int

      fst (Queue.dequeue q) `shouldBe` Just 5
