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

    , ( "let x = 0                                \
        \in letrec                                \
        \     even(dummy) =                       \
        \       if zero?(x) then                  \
        \         1                               \
        \       else                              \
        \         let dummy = set x = -(x, 1)     \
        \         in (odd 888)                    \
        \     odd(dummy) =                        \
        \       if zero?(x) then                  \
        \         0                               \
        \       else                              \
        \         let dummy = set x = -(x, 1)     \
        \         in (even 888)                   \
        \   in let dummy = set x = 13 in (odd 888)"
      , VNumber 1
      )

    , ( "let g = let counter = 0                                    \
        \        in proc (dummy)                                    \
        \             let dummy = set counter = -(counter, -(0, 1)) \
        \             in counter                                    \
        \in let a = (g 11)                                          \
        \   in let b = (g 11)                                       \
        \      in -(a, b)                                           "
      , VNumber (-1)
      )

    , ( "let g = proc (dummy)                                       \
        \          let counter = 0                                  \
        \          in let dummy = set counter = -(counter, -(0, 1)) \
        \             in counter                                    \
        \in let a = (g 11)                                          \
        \   in let b = (g 11)                                       \
        \      in -(a, b)                                           "
      , VNumber 0
      )

    , ( "let y = 0                    \
        \in let x = y                 \
        \   in let dummy = set y = 11 \
        \      in x                   "
        -- Aliasing does not work since x stores a reference to a location that
        -- stores 0, i.e. x does not store a reference to y.
      , VNumber 0
      )

    , ( "let x = 0                            \
        \in letrec                            \
        \     even(dummy) =                   \
        \       if zero?(x) then              \
        \         1                           \
        \       else                          \
        \         begin                       \
        \           set x = -(x, 1);          \
        \           (odd 888)                 \
        \         end                         \
        \     odd(dummy) =                    \
        \       if zero?(x) then              \
        \         0                           \
        \       else                          \
        \         begin                       \
        \           set x = -(x, 1);          \
        \           (even 888)                \
        \         end                         \
        \   in begin set x = 13; (odd 888) end"
      , VNumber 1
      )

    , ( "let g = let counter = 0                           \
        \        in proc (dummy)                           \
        \             begin                                \
        \               set counter = -(counter, -(0, 1)); \
        \               counter                            \
        \             end                                  \
        \in let a = (g 11)                                 \
        \   in let b = (g 11)                              \
        \      in -(a, b)                                  "
      , VNumber (-1)
      )

    , ( "let g = proc (dummy)                             \
        \          let counter = 0                        \
        \          in                                     \
        \            begin                                \
        \              set counter = -(counter, -(0, 1)); \
        \              counter                            \
        \            end                                  \
        \in let a = (g 11)                                \
        \   in let b = (g 11)                             \
        \      in -(a, b)                                 "
      , VNumber 0
      )

    , ( "let y = 0          \
        \in let x = y       \
        \   in              \
        \     begin         \
        \       set y = 11; \
        \       x           \
        \     end           "
        -- Aliasing does not work since x stores a reference to a location that
        -- stores 0, i.e. x does not store a reference to y.
      , VNumber 0
      )

    , ( "let f =                      \
        \  proc (x) proc (y)          \
        \    begin                    \
        \      set x = -(x, -(0, 1)); \
        \      -(x, y)                \
        \    end                      \
        \in ((f 44) 33)               "
      , VNumber 12
      )

    -- Test list related functions

    , ( "emptylist"
      , VList []
      )

    , ( "cons(4, emptylist)"
      , VList [ VNumber 4 ]
      )

    , ( "let                        \
        \  x = 4                    \
        \in                         \
        \cons(x,                    \
        \     cons(cons(-(x, 1),    \
        \               emptylist), \
        \          emptylist))      "
      , VList [ VNumber 4, VList [ VNumber 3 ] ]
      )

    , ( "car(cons(1, cons(2, emptylist)))"
      , VNumber 1
      )

    , ( "cdr(cons(1, cons(2, emptylist)))"
      , VList [ VNumber 2 ]
      )

    , ( "null?(cdr(cdr(cons(1, cons(2, emptylist)))))"
      , VBool True
      )

    , ( "null?(cdr(cons(1, cons(2, emptylist))))"
      , VBool False
      )

    , ( "let                      \
        \  x = 4                  \
        \in                       \
        \list(x, -(x, 1), -(x, 3))"
      , VList [ VNumber 4, VNumber 3, VNumber 1 ]
      )

    , ( "null?(list())"
      , VBool True
      )

    , ( "yield"
      , VNumber 99
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
