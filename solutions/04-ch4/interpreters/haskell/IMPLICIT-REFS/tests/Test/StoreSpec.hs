module Test.StoreSpec (spec) where


import qualified Store
import Test.Hspec


spec :: Spec
spec =
  let
    store =
      Store.empty

    (ref0, store0) =
      Store.newref 'a' store

    (ref1, store1) =
      Store.newref 'b' store0

    (ref2, store2) =
      Store.newref 'c' store1

    Just store3 =
      Store.setref ref1 'B' store2
  in
  describe "store" $ do
    it "example 1" $
      Store.deref ref0 store2 `shouldBe` Just 'a'

    it "example 2" $
      Store.deref ref1 store2 `shouldBe` Just 'b'

    it "example 3" $
      Store.deref ref2 store2 `shouldBe` Just 'c'

    it "example 4" $
      Store.deref ref2 store1 `shouldBe` Nothing

    it "example 5" $
      Store.deref ref1 store3 `shouldBe` Just 'B'

    it "example 6" $
      Store.setref ref2 'A' store0 `shouldBe` Nothing
