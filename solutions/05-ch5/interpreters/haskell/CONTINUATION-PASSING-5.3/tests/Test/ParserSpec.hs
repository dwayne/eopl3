module Test.ParserSpec (spec) where


import Parser
import Test.Hspec


spec :: Spec
spec =
  describeExamples "parser"
    [ ( "1", Const 1 )
    , ( "x", Var "x" )
    , ( "-(5, y)", Diff (Const 5) (Var "y") )
    , ( "zero?(z)", Zero (Var "z") )
    , ( "if zero?(2) then 0 else 1", If (Zero (Const 2)) (Const 0) (Const 1) )
    , ( "let n = 10 in -(n, 1)", Let "n" (Const 10) (Diff (Var "n") (Const 1)) )
    , ( "proc (x) -(x, 1)", Proc "x" (Diff (Var "x") (Const 1)) )
    , ( "letrec f(x) = a in b", Letrec "f" "x" (Var "a") (Var "b") )
    , ( "(f x)", Call (Var "f") (Var "x") )

    , ( "let2 a = 1 b = 2 in -(b, a)"
      , Let2 "a" (Const 1) "b" (Const 2) (Diff (Var "b") (Var "a"))
      )
    ]


describeExamples :: String -> [(String, Expr)] -> Spec
describeExamples description =
  describe description . mapM_ (uncurry toSpecItem) . zip [1..]
  where
    toSpecItem :: Int -> (String, Expr) -> Spec
    toSpecItem i (input, expr) =
      it ("example " ++ show i) $ do
        parse input `shouldBe` Right (Program expr)
