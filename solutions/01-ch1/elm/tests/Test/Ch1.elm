module Test.Ch1 exposing
  ( inS
  , listLength
  , nthElement
  , removeFirst
  , occursFree
  , subst

  , remove

  , duple
  , invert
  , down
  , swapper
  , listSet
  , countOccurrences
  , product
  , filterIn
  , listIndex
  , every
  , exists
  , flatten
  , merge
  , sort
  , sortWithPredicate
  , doubleTree
  , markLeavesWithRedDepth
  , path
  , numberLeaves
  , numberElements
  )


import Expect
import Fuzz
import Test exposing (Test, describe, fuzz, test)


import Ch1 exposing
  ( LcExp(..)
  , SList(..)
  , SExp(..)
  , BinTree(..)
  , BinarySearchTree(..)
  , Direction(..)
  )


inS : Test
inS =
  describe "inS"
    [ fuzz Fuzz.int "is True for non-negative multiples of 3 and False otherwise" <|
        \n ->
          Ch1.inS n
            |> Expect.equal (n >= 0 && modBy 3 n == 0)
    ]


listLength : Test
listLength =
  describe "listLength"
    [ fuzz (Fuzz.list Fuzz.unit) "computes the length of any list" <|
        \list ->
          Ch1.listLength list
            |> Expect.equal (List.length list)
    ]


nthElement : Test
nthElement =
  describe "nthElement"
    [ describe "empty list"
        [ fuzz Fuzz.int "can never find the nth element of the empty list" <|
            \n ->
              Ch1.nthElement [] n
                |> Expect.err
        ]
    , describe "non-empty list"
        [ test "it returns the 1st element" <|
            \_ ->
              Ch1.nthElement [1, 2, 3, 4, 5] 0
                |> Expect.equal (Ok 1)
        , test "it returns the 2nd element" <|
            \_ ->
              Ch1.nthElement [1, 2, 3, 4, 5] 1
                |> Expect.equal (Ok 2)
        , test "it returns the 5th element" <|
            \_ ->
              Ch1.nthElement [1, 2, 3, 4, 5] 4
                |> Expect.equal (Ok 5)
        , test "it returns an error given a negative index" <|
            \_ ->
              Ch1.nthElement [1, 2, 3, 4, 5] -1
                |> Expect.equal (Err "The index must be non-negative: -1")
        , test "it returns an error given an out of bounds index" <|
            \_ ->
              Ch1.nthElement [1, 2, 3, 4, 5] 5
                |> Expect.equal (Err "List too short by 1 element")
        ]
    ]


removeFirst : Test
removeFirst =
  describe "removeFirst"
    [ test "it removes the only a" <|
        \_ ->
          Ch1.removeFirst "a" ["a", "b", "c"]
            |> Expect.equal ["b", "c"]
    , test "it returns the original list" <|
        \_ ->
          Ch1.removeFirst "b" ["e", "f", "g"]
            |> Expect.equal ["e", "f", "g"]
    , test "it removes the first a4" <|
        \_ ->
          Ch1.removeFirst "a4" ["c1", "a4", "c1", "a4"]
            |> Expect.equal ["c1", "c1", "a4"]
    , test "it returns the empty list" <|
        \_ ->
          Ch1.removeFirst "x" []
            |> Expect.equal []
    ]


occursFree : Test
occursFree =
  describe "occursFree"
    [ test "it returns True for x in x" <|
        \_ ->
          Ch1.occursFree "x" (Id "x")
            |> Expect.equal True
    , test "it returns False for x in y" <|
        \_ ->
          Ch1.occursFree "x" (Id "y")
            |> Expect.equal False
    , test "it returns False for x in λx.(x y)" <|
        \_ ->
          Ch1.occursFree "x" (Lambda "x" (App (Id "x") (Id "y")))
            |> Expect.equal False
    , test "it returns True for x in λy.(x y)" <|
        \_ ->
          Ch1.occursFree "x" (Lambda "y" (App (Id "x") (Id "y")))
            |> Expect.equal True
    , test "it returns True for x in (λx.x (x y))" <|
        \_ ->
          Ch1.occursFree "x" (App (Lambda "x" (Id "x")) (App (Id "x") (Id "y")))
            |> Expect.equal True
    , test "it returns True for x in λy.λz.(x (y z))" <|
        \_ ->
          Ch1.occursFree "x" (Lambda "y" (Lambda "z" (App (Id "x") (App (Id "y") (Id "z")))))
            |> Expect.equal True
    ]


subst : Test
subst =
  describe "subst"
    [ test "it replaces b's with a's" <|
        \_ ->
          let
            -- ((b c) (b () d))
            input =
              (Cons
                (SList (Cons (Symbol "b") (Cons (Symbol "c") Empty)))
                (Cons
                  (SList (Cons (Symbol "b") (Cons (SList Empty) (Cons (Symbol "d") Empty))))
                  Empty))

            -- ((a c) (a () d))
            output =
              (Cons
                (SList (Cons (Symbol "a") (Cons (Symbol "c") Empty)))
                (Cons
                  (SList (Cons (Symbol "a") (Cons (SList Empty) (Cons (Symbol "d") Empty))))
                  Empty))
          in
            Ch1.subst "a" "b" input
              |> Expect.equal output
    ]


-- TEST SOLUTIONS TO EXERCISES


remove : Test
remove =
  describe "remove"
    [ test "it removes the only a" <|
        \_ ->
          Ch1.remove "a" ["a", "b", "c"]
            |> Expect.equal ["b", "c"]
    , test "it returns the original list" <|
        \_ ->
          Ch1.remove "b" ["e", "f", "g"]
            |> Expect.equal ["e", "f", "g"]
    , test "it removes all a4" <|
        \_ ->
          Ch1.remove "a4" ["c1", "a4", "c1", "a4"]
            |> Expect.equal ["c1", "c1"]
    , test "it returns the empty list" <|
        \_ ->
          Ch1.remove "x" []
            |> Expect.equal []
    ]


duple : Test
duple =
  describe "duple"
    [ test "it returns 2 3's" <|
        \_ ->
          Ch1.duple 2 3
            |> Expect.equal [3, 3]
    , test "it returns 4 [\"ha\", \"ha\"]'s" <|
        \_ ->
          Ch1.duple 4 ["ha", "ha"]
            |> Expect.equal [["ha", "ha"], ["ha", "ha"], ["ha", "ha"], ["ha", "ha"]]
    , test "it returns the empty list" <|
        \_ ->
          Ch1.duple 0 ["blah"]
            |> Expect.equal []
    ]


invert : Test
invert =
  describe "invert"
    [ test "it reverses each 2-tuple in the list" <|
        \_ ->
          Ch1.invert [("a", "1"), ("a", "2"), ("1", "b"), ("2", "b")]
            |> Expect.equal [("1", "a"), ("2", "a"), ("b", "1"), ("b", "2")]
    ]


down : Test
down =
  describe "down"
    [ test "it returns [[1], [2], [3]]" <|
        \_ ->
          Ch1.down [1, 2, 3]
            |> Expect.equal [[1], [2], [3]]
    , test "it returns [[\"a\"], [\"fine\"], [\"idea\"]]" <|
        \_ ->
          Ch1.down [["a"], ["fine"], ["idea"]]
            |> Expect.equal [[["a"]], [["fine"]], [["idea"]]]
    , test "it returns the empty list" <|
        \_ ->
          Ch1.down []
            |> Expect.equal []
    ]


swapper : Test
swapper =
  describe "swapper"
    [ test "example 1" <|
        \_ ->
          let
            input =
              (Cons
                (Symbol "a")
                (Cons
                  (Symbol "b")
                  (Cons
                    (Symbol "c")
                    (Cons
                      (Symbol "d")
                      Empty))))

            output =
              (Cons
                (Symbol "d")
                (Cons
                  (Symbol "b")
                  (Cons
                    (Symbol "c")
                    (Cons
                      (Symbol "a")
                      Empty))))
          in
            Ch1.swapper "a" "d" input
              |> Expect.equal output
    , test "example 2" <|
        \_ ->
          let
            input =
              (Cons
                (Symbol "a")
                (Cons
                  (Symbol "d")
                  (Cons
                    (SList Empty)
                    (Cons
                      (Symbol "c")
                      (Cons
                        (Symbol "d")
                        Empty)))))

            output =
              (Cons
                (Symbol "d")
                (Cons
                  (Symbol "a")
                  (Cons
                    (SList Empty)
                    (Cons
                      (Symbol "c")
                      (Cons
                        (Symbol "a")
                        Empty)))))
          in
            Ch1.swapper "a" "d" input
              |> Expect.equal output
    , test "example 3" <|
        \_ ->
          let
            input =
              (Cons
                (SList (Cons (Symbol "x") Empty))
                (Cons
                  (Symbol "y")
                  (Cons
                    (SList
                      (Cons
                        (Symbol "z")
                        (Cons
                          (SList (Cons (Symbol "x") Empty))
                          Empty)))
                    Empty)))

            output =
              (Cons
                (SList (Cons (Symbol "y") Empty))
                (Cons
                  (Symbol "x")
                  (Cons
                    (SList
                      (Cons
                        (Symbol "z")
                        (Cons
                          (SList (Cons (Symbol "y") Empty))
                          Empty)))
                    Empty)))
          in
            Ch1.swapper "x" "y" input
              |> Expect.equal output
    ]


listSet : Test
listSet =
  describe "listSet"
    [ test "example 1" <|
        \_ ->
          Ch1.listSet ["a", "b", "c", "d"] 2 "(1 2)"
            |> Expect.equal ["a", "b", "(1 2)", "d"]
    , test "example 2" <|
        \_ ->
          Ch1.listSet ["a", "b", "c", "d"] 3 "(1 5 10)"
            |> Expect.equal ["a", "b", "c", "(1 5 10)"]
    , test "when index is negative it returns the original list" <|
        \_ ->
          Ch1.listSet ["a", "b"] -1 "c"
            |> Expect.equal ["a", "b"]
    , test "when index is positively out of bounds it returns the original list" <|
        \_ ->
          Ch1.listSet ["a", "b"] 2 "c"
            |> Expect.equal ["a", "b"]
    ]


countOccurrences : Test
countOccurrences =
  let
    fx =
      Cons (Symbol "f") (Cons (Symbol "x") Empty)

    xz =
      Cons (Symbol "x") (Cons (Symbol "z") Empty)

    xzx =
      Cons (SList xz) (Cons (Symbol "x") Empty)

    xzex =
      Cons (SList xz) (Cons (SList Empty) (Cons (Symbol "x") Empty))
  in
    describe "countOccurrences"
      [ test "example 1" <|
          \_ ->
            let
              input =
                Cons
                  (SList fx)
                  (Cons
                    (Symbol "y")
                    (Cons
                      (SList (Cons (SList xzx) Empty))
                      Empty))
            in
              Ch1.countOccurrences "x" input
                |> Expect.equal 3
      , test "example 2" <|
          \_ ->
            let
              input =
                Cons
                  (SList fx)
                  (Cons
                    (Symbol "y")
                    (Cons
                      (SList (Cons (SList xzex) Empty))
                      Empty))
            in
              Ch1.countOccurrences "x" input
                |> Expect.equal 3
      , test "example 3" <|
          \_ ->
            let
              input =
                Cons
                  (SList fx)
                  (Cons
                    (Symbol "y")
                    (Cons
                      (SList (Cons (SList xzx) Empty))
                      Empty))
            in
              Ch1.countOccurrences "w" input
                |> Expect.equal 0
      ]


product : Test
product =
  let
    one : Int
    one = 1
  in
    describe "product"
      [ test "example 1" <|
          \_ ->
            Ch1.product [] []
              |> Expect.equal []
      , test "example 2" <|
          \_ ->
            Ch1.product [1] []
              |> Expect.equal []
      , test "example 3" <|
          \_ ->
            Ch1.product [] [1]
              |> Expect.equal []
      , test "example 4" <|
          \_ ->
            Ch1.product [one] [1]
              |> Expect.equal [(1, 1)]
      , test "example 5" <|
          \_ ->
            Ch1.product [one, 2] [1, 2, 3]
              |> Expect.equal [(1, 1), (1, 2), (1, 3), (2, 1), (2, 2), (2, 3)]
      ]


filterIn : Test
filterIn =
  describe "filterIn"
    [ test "even" <|
        \_ ->
          Ch1.filterIn (\n -> modBy 2 n == 0) [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
            |> Expect.equal [2, 4, 6, 8, 10]
    , test "odd" <|
        \_ ->
          Ch1.filterIn (\n -> modBy 2 n == 1) [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
            |> Expect.equal [1, 3, 5, 7, 9]
    ]


listIndex : Test
listIndex =
  describe "listIndex"
    [ test "example 1" <|
        \_ ->
          Ch1.listIndex (\n -> modBy 2 n == 0) [1, 3, 5, 7, 9]
            |> Expect.equal Nothing
    , test "example 2" <|
        \_ ->
          Ch1.listIndex ((==) "Eli") ["Dwayne", "Denzil", "Eli", "Darryl"]
            |> Expect.equal (Just 2)
    ]


every : Test
every =
  describe "every"
    [ test "example 1" <|
        \_ ->
          Ch1.every (\n -> modBy 2 n == 0) [2, 4, 6]
            |> Expect.equal True
    , test "example 2" <|
        \_ ->
          Ch1.every (\n -> modBy 2 n == 0) [2, 4, 5, 6]
            |> Expect.equal False
    ]


exists : Test
exists =
  describe "exists"
    [ test "example 1" <|
        \_ ->
          Ch1.exists (\n -> modBy 2 n == 0) [1, 3, 5, 6, 7]
            |> Expect.equal True
    , test "example 2" <|
        \_ ->
          Ch1.exists (\n -> modBy 2 n == 0) [1, 3, 5, 7]
            |> Expect.equal False
    ]


flatten : Test
flatten =
  describe "flatten"
    [ test "example 1" <|
        \_ ->
          -- '(a b c)
          Ch1.flatten (Cons (Symbol "a") (Cons (Symbol "b") (Cons (Symbol "c") Empty)))
            |> Expect.equal ["a", "b", "c"]
    , test "example 2" <|
        \_ ->
          let
            -- '((a) () (b ()) () (c))
            input =
              Cons
                (SList (Cons (Symbol "a") Empty))
                (Cons
                  (SList Empty)
                  (Cons
                    (SList (Cons (Symbol "b") (Cons (SList Empty) Empty)))
                    (Cons
                      (SList Empty)
                      (Cons
                        (SList (Cons (Symbol "c") Empty))
                        Empty))))
          in
            Ch1.flatten input
              |> Expect.equal ["a", "b", "c"]
    , test "example 3" <|
        \_ ->
          let
            -- '((a b) c (((d)) e))
            input =
              Cons
                (SList (Cons (Symbol "a") (Cons (Symbol "b") Empty)))
                (Cons
                  (Symbol "c")
                  (Cons
                    (SList
                      (Cons
                        (SList
                          (Cons (SList (Cons (Symbol "d") Empty)) Empty))
                        (Cons
                          (Symbol "e")
                          Empty)))
                    Empty))
          in
            Ch1.flatten input
              |> Expect.equal ["a", "b", "c", "d", "e"]
    , test "example 4" <|
        \_ ->
          let
            -- '(a b (() (c)))
            input =
              Cons
                (Symbol "a")
                (Cons
                  (Symbol "b")
                  (Cons
                    (SList
                      (Cons
                        (SList Empty)
                        (Cons
                          (SList (Cons (Symbol "c") Empty))
                          Empty)))
                    Empty))
          in
            Ch1.flatten input
              |> Expect.equal ["a", "b", "c"]
    ]


merge : Test
merge =
  describe "merge"
    [ test "example 1" <|
        \_ ->
          Ch1.merge [1, 4] [1, 2, 8]
            |> Expect.equal [1, 1, 2, 4, 8]
    , test "example 2" <|
        \_ ->
          Ch1.merge [35, 62, 81, 90, 91] [3, 83, 85, 90]
            |> Expect.equal [3, 35, 62, 81, 83, 85, 90, 90, 91]
    ]


sort : Test
sort =
  describe "sort"
    [ test "example 1" <|
        \_ ->
          Ch1.sort []
            |> Expect.equal []
    , test "example 2" <|
        \_ ->
          Ch1.sort [1]
            |> Expect.equal [1]
    , test "example 3" <|
        \_ ->
          Ch1.sort [1, 2]
            |> Expect.equal [1, 2]
    , test "example 4" <|
        \_ ->
          Ch1.sort [2, 1]
            |> Expect.equal [1, 2]
    , test "example 5" <|
        \_ ->
          Ch1.sort [8, 2, 5, 2, 3]
            |> Expect.equal [2, 2, 3, 5, 8]
    ]


sortWithPredicate : Test
sortWithPredicate =
  describe "sortWithPredicate"
    [ test "example 1" <|
        \_ ->
          Ch1.sortWithPredicate (<) [8, 2, 5, 2, 3]
            |> Expect.equal [2, 2, 3, 5, 8]
    , test "example 2" <|
        \_ ->
          Ch1.sortWithPredicate (>) [8, 2, 5, 2, 3]
            |> Expect.equal [8, 5, 3, 2, 2]
    ]


doubleTree : Test
doubleTree =
  describe "doubleTree"
    [ test "example 1" <|
        \_ ->
          let
            input =
              Node "a"
                (Node "b" (Leaf 1) (Leaf 2))
                (Node "c" (Leaf 3) (Leaf 4))

            output =
              Node "a"
                (Node "b" (Leaf 2) (Leaf 4))
                (Node "c" (Leaf 6) (Leaf 8))
          in
            Ch1.doubleTree input
              |> Expect.equal output
    ]


markLeavesWithRedDepth : Test
markLeavesWithRedDepth =
  describe "markLeavesWithRedDepth"
    [ test "example 1" <|
        \_ ->
          let
            input =
              Node "red"
                (Node "bar" (Leaf 26) (Leaf 12))
                (Node "red" (Leaf 11) (Node "quux" (Leaf 117) (Leaf 14)))

            output =
              Node "red"
                (Node "bar" (Leaf 1) (Leaf 1))
                (Node "red" (Leaf 2) (Node "quux" (Leaf 2) (Leaf 2)))
          in
            Ch1.markLeavesWithRedDepth input
              |> Expect.equal output
    ]


path : Test
path =
  describe "path" <|
    let
      input =
        Branch 14
          (Branch 7
            Nil
            (Branch 12 Nil Nil))
          (Branch 26
            (Branch 20
              (Branch 17 Nil Nil)
              Nil)
            (Branch 31 Nil Nil))
    in
      [ test "example 1" <|
          \_ ->
            Ch1.path 1 Nil
              |> Expect.equal Nothing
      , test "example 2" <|
          \_ ->
            Ch1.path 19 input
              |> Expect.equal Nothing
      , test "example 3" <|
          \_ ->
            Ch1.path 14 input
              |> Expect.equal (Just [])
      , test "example 4" <|
          \_ ->
            Ch1.path 7 input
              |> Expect.equal (Just [TurnLeft])
      , test "example 5" <|
          \_ ->
            Ch1.path 26 input
              |> Expect.equal (Just [TurnRight])
      , test "example 6" <|
          \_ ->
            Ch1.path 17 input
              |> Expect.equal (Just [TurnRight, TurnLeft, TurnLeft])
      ]


numberLeaves : Test
numberLeaves =
  describe "numberLeaves" <|
    [ test "example 1" <|
        \_ ->
          let
            input =
              Node "foo"
                (Node "bar" (Leaf 26) (Leaf 12))
                (Node "baz" (Leaf 11) (Node "quux" (Leaf 117) (Leaf 14)))

            output =
              Node "foo"
                (Node "bar" (Leaf 0) (Leaf 1))
                (Node "baz" (Leaf 2) (Node "quux" (Leaf 3) (Leaf 4)))
          in
            Ch1.numberLeaves input
              |> Expect.equal output
    ]


numberElements : Test
numberElements =
  describe "numberElements" <|
    [ test "example 1" <|
        \_ ->
          Ch1.numberElements []
            |> Expect.equal []
    , test "example 2" <|
        \_ ->
          Ch1.numberElements [1]
            |> Expect.equal [(0, 1)]
    , test "example 3" <|
        \_ ->
          Ch1.numberElements [1, 2]
            |> Expect.equal [(0, 1), (1, 2)]
    , test "example 4" <|
        \_ ->
          Ch1.numberElements [1, 2, 3]
            |> Expect.equal [(0, 1), (1, 2), (2, 3)]
    ]
