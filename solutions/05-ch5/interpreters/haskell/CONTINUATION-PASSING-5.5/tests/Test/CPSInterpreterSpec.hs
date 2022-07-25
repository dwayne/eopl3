module Test.CPSInterpreterSpec (spec) where


import CPSInterpreter
import Test.Hspec


spec :: Spec
spec =
  describeExamples "interpreter"
    [ ( "5", VNumber 5 )
    , ( "x", VNumber 10 )
    , ( "zero?(i)", VBool False )
    , ( "zero?(-(i, 1))", VBool True )
    , ( "-(55, -(x, 11))", VNumber 56 )
    , ( "-(-(x, 3), -(v, i))", VNumber 3 )
    , ( "let x = 33                                      \
        \in let y = 22                                   \
        \in if zero?(-(x, 11)) then -(y, 2) else -(y, 4) "
      , VNumber 18
      )
    , ( "let x = 5 in -(x, 3)", VNumber 2 )
    , ( "let z = 5                           \
        \in let x = 3                        \
        \   in let y = -(x, 1)               \
        \      in let x = 4 in -(z, -(x, y)) "
      , VNumber 3
      )
    , ( "let x = 7                     \
        \in let y = 2                  \
        \   in let y = let x = -(x, 1) \
        \              in -(x, y)      \
        \      in -(-(x, 8), y)        "
      , VNumber (-5)
      )
    , ( "let f = proc (x) -(x, 11) \
        \in (f (f 77))             "
      , VNumber 55
      )
    , ( "(proc (f) (f (f 77)) \
        \ proc (x) -(x, 11))  "
      , VNumber 55
      )
    , ( "let x = 200                       \
        \in let f = proc (z) -(z, x)       \
        \   in let x = 100                 \
        \      in let g = proc (z) -(z, x) \
        \         in -((f 1), (g 1))       "
      , VNumber (-100)
      )

    -- Exercise 3.20
    --
    -- Write a curried procedure that takes two arguments and returns their sum.
    , ( "let f = proc (x) proc (y) -(x, -(0, y)) \
        \in let a = ((f 1) 2)                    \
        \   in let b = ((f a) 3)                 \
        \      in let c = ((f b) 4)              \
        \         in let d = ((f c) 5) in d      "
      , VNumber 15
      )

    , ( "letrec double(x) = if zero?(x)                       \
        \                   then 0                            \
        \                   else -((double -(x, 1)), -(0, 2)) \
        \in (double 6)                                        "
      , VNumber 12
      )

    -- Exercise 5.3
    , ( "let2                    \
        \  x = 33                \
        \  y = 22                \
        \in                      \
        \if zero?(-(x, 11)) then \
        \  -(y, 2)               \
        \else                    \
        \  -(y, 4)               "
      , VNumber 18
      )

    -- Exercise 5.4
    , ( "let3         \
        \  a = 10     \
        \  b = 5      \
        \  c = 1      \
        \in           \
        \-(a, -(b, c))"
      , VNumber 6
      )
    ]


describeExamples :: String -> [(String, Value)] -> Spec
describeExamples description =
  describe description . mapM_ (uncurry toSpecItem) . zip [1..]
  where
    toSpecItem :: Int -> (String, Value) -> Spec
    toSpecItem i (input, value) =
      it ("example " ++ show i) $ do
        run input `shouldBe` Right value
