module Test.FactorialSpec (spec) where

import qualified Factorial
import qualified Factorial.Iterative
import qualified Factorial.CPSDataStructure
import qualified Factorial.CPSProcedural
import qualified Factorial.Registerized
import qualified Factorial.Trampolined

import Test.Hspec


spec :: Spec
spec =
    describe "fact" $ do
        it "computes 5!" $ do
            Factorial.fact 5 `shouldBe` 120
            Factorial.Iterative.fact 5 `shouldBe` 120
            Factorial.CPSDataStructure.fact 5 `shouldBe` 120
            Factorial.CPSProcedural.fact 5 `shouldBe` 120
            Factorial.Registerized.fact 5 `shouldBe` 120
            Factorial.Trampolined.fact 5 `shouldBe` 120
