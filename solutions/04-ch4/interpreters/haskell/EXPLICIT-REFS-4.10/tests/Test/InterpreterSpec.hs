module Test.InterpreterSpec (spec) where


import Interpreter
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

    -- There is support for multideclaration letrec.
    -- This example is from Exercise 3.32.
    , ( "letrec                                             \
        \  even(x) = if zero?(x) then 1 else (odd -(x, 1))  \
        \  odd(x)  = if zero?(x) then 0 else (even -(x, 1)) \
        \in (odd 13)                                        "
      , VNumber 1
      )

    -- Examples from 4.2

    , ( "let x = newref(0)                              \
        \in letrec                                      \
        \     even(dummy) =                             \
        \       if zero?(deref(x)) then                 \
        \         1                                     \
        \       else                                    \
        \         let dummy = setref(x, -(deref(x), 1)) \
        \         in (odd 888)                          \
        \     odd(dummy) =                              \
        \       if zero?(deref(x)) then                 \
        \         0                                     \
        \       else                                    \
        \         let dummy = setref(x, -(deref(x), 1)) \
        \         in (even 888)                         \
        \   in let dummy = setref(x, 13) in (odd 888)   "
      , VNumber 1
      )

    , ( "let g = let counter = newref(0)                                      \
        \        in proc (dummy)                                              \
        \             let dummy = setref(counter, -(deref(counter), -(0, 1))) \
        \             in deref(counter)                                       \
        \in let a = (g 11)                                                    \
        \   in let b = (g 11)                                                 \
        \      in -(a, b)                                                     "
      , VNumber (-1)
      )

    , ( "let g = proc (dummy)                                                 \
        \          let counter = newref(0)                                    \
        \          in let dummy = setref(counter, -(deref(counter), -(0, 1))) \
        \             in deref(counter)                                       \
        \in let a = (g 11)                                                    \
        \   in let b = (g 11)                                                 \
        \      in -(a, b)                                                     "
      , VNumber 0
      )

    , ( "let x = newref(newref(0))           \
        \in let dummy = setref(deref(x), 11) \
        \   in deref(deref(x))               "
      , VNumber 11
      )

    , ( "let x = newref(22)                     \
        \in let f =                             \
        \     proc (z)                          \
        \       let zz = newref(-(z, deref(x))) \
        \       in deref(zz)                    \
        \   in -((f 66), (f 55))                "
      , VNumber 11
      )

    -- Testing begin

    , ( "let x = newref(0)                        \
        \in letrec                                \
        \     even(dummy) =                       \
        \       if zero?(deref(x)) then           \
        \         1                               \
        \       else                              \
        \         begin                           \
        \           setref(x, -(deref(x), 1));    \
        \           (odd 888)                     \
        \         end                             \
        \     odd(dummy) =                        \
        \       if zero?(deref(x)) then           \
        \         0                               \
        \       else                              \
        \         begin                           \
        \           setref(x, -(deref(x), 1));    \
        \           (even 888)                    \
        \         end                             \
        \   in begin setref(x, 13); (odd 888) end "
      , VNumber 1
      )

    , ( "let g = let counter = newref(0)                             \
        \        in proc (dummy)                                     \
        \             begin                                          \
        \               setref(counter, -(deref(counter), -(0, 1))); \
        \               deref(counter)                               \
        \             end                                            \
        \in let a = (g 11)                                           \
        \   in let b = (g 11)                                        \
        \      in -(a, b)                                            "
      , VNumber (-1)
      )

    , ( "let g = proc (dummy)                                       \
        \          let counter = newref(0)                          \
        \          in                                               \
        \            begin                                          \
        \              setref(counter, -(deref(counter), -(0, 1))); \
        \              deref(counter)                               \
        \            end                                            \
        \in let a = (g 11)                                          \
        \   in let b = (g 11)                                       \
        \      in -(a, b)                                           "
      , VNumber 0
      )

    , ( "let x = newref(newref(0)) \
        \in                        \
        \  begin                   \
        \    setref(deref(x), 11); \
        \    deref(deref(x))       \
        \  end                     "
      , VNumber 11
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
