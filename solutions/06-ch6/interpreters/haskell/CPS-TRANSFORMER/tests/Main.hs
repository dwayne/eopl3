module Main (main) where

import Test.Hspec

import qualified Test.Interpreter_IN

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Interpreter_IN" Test.Interpreter_IN.spec
