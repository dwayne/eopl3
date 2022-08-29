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
    , ( "cons(1, l)", Cons (Const 1) (Var "l") )
    , ( "car(l)", Car (Var "l") )
    , ( "cdr(l)", Cdr (Var "l") )
    , ( "null?(l)", Null (Var "l") )
    , ( "emptylist", Empty )
    , ( "list()", List [] )
    , ( "list(1)", List [ Const 1 ] )
    , ( "list(1, 2)", List [ Const 1, Const 2 ] )
    , ( "list(1, 2, 3)", List [ Const 1, Const 2, Const 3 ] )
    , ( "if zero?(2) then 0 else 1", If (Zero (Const 2)) (Const 0) (Const 1) )
    , ( "let n = 10 in -(n, 1)", Let "n" (Const 10) (Diff (Var "n") (Const 1)) )
    , ( "proc (x) -(x, 1)", Proc "x" (Diff (Var "x") (Const 1)) )
    , ( "letrec f(x) = a in b"
      , Letrec [("f", "x", (Var "a"))] (Var "b")
      )
    , ( "(f x)", Call (Var "f") (Var "x") )
    , ( "begin    \
        \  1;     \
        \  x;     \
        \ -(5, y) \
        \end      "
      , Begin
          [ Const 1
          , Var "x"
          , Diff (Const 5) (Var "y")
          ]
      )
    , ( "set x = 1", Assign "x" (Const 1) )
    , ( "print(100)", Print (Const 100) )
    , ( "spawn(proc (x) x)", Spawn (Proc "x" (Var "x")) )
    , ( "mutex()", Mutex )
    , ( "wait(mut)", Wait (Var "mut") )
    , ( "signal(mut)", Signal (Var "mut") )
    ]


describeExamples :: String -> [(String, Expr)] -> Spec
describeExamples description =
  describe description . mapM_ (uncurry toSpecItem) . zip [1..]
  where
    toSpecItem :: Int -> (String, Expr) -> Spec
    toSpecItem i (input, expr) =
      it ("example " ++ show i) $ do
        parse input `shouldBe` Right (Program expr)
