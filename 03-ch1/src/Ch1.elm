module Ch1 exposing
  ( inS

  , listLength
  , nthElement
  , removeFirst

  , LcExp(..)
  , occursFree

  , SList(..), SExp(..)
  , subst

  , remove

  , duple
  , invert
  , down
  , swapper
  , listSet
  )


inS : Int -> Bool
inS n =
  if n == 0 then
    True
  else if n >= 3 then
    inS (n-3)
  else
    False


listLength : List a -> Int
listLength list =
  case list of
    [] ->
      0

    _ :: rest ->
      1 + listLength rest


nthElement : List a -> Int -> Result String a
nthElement list n =
  if n >= 0 then
    nthElementHelper list n
  else
    Err ("The index must be non-negative: " ++ String.fromInt n)


nthElementHelper : List a -> Int -> Result String a
nthElementHelper list n =
  case list of
    [] ->
      Err ("List too short by " ++ String.fromInt (n+1) ++ " " ++ pluralize (n+1) "element" "elements")

    x :: rest ->
      if n == 0 then
        Ok x
      else
        nthElementHelper rest (n-1)


removeFirst : a -> List a -> List a
removeFirst s list =
  case list of
    [] ->
      list

    x :: rest ->
      if x == s then
        rest
      else
        x :: removeFirst s rest


type LcExp
  = Id String
  | Lambda String LcExp
  | App LcExp LcExp


occursFree : String -> LcExp -> Bool
occursFree var exp =
  case exp of
    Id x ->
      var == x

    Lambda x body ->
      var /= x && occursFree var body

    App f arg ->
      occursFree var f || occursFree var arg


type SList
  = Empty
  | Cons SExp SList


type SExp
  = Symbol String
  | SList SList


subst : String -> String -> SList -> SList
subst new old slist =
  case slist of
    Empty ->
      slist

    Cons sexp rest ->
      Cons (substInSExp new old sexp) (subst new old rest)


substInSExp : String -> String -> SExp -> SExp
substInSExp new old sexp =
  case sexp of
    Symbol s ->
      if s == old then
        Symbol new
      else
        Symbol s

    SList slist ->
      SList (subst new old slist)


-- An alternative implementation of subst based on exercise 1.12.
-- substInSExp is inlined.
-- subst : String -> String -> SList -> SList
-- subst new old slist =
--   case slist of
--     Empty ->
--       slist
--
--     Cons sexp rest ->
--       let
--         head =
--           case sexp of
--             Symbol s ->
--               if s == old then
--                 Symbol new
--               else
--                 Symbol s
--
--             SList inner ->
--               SList (subst new old inner)
--
--         tail =
--           subst new old rest
--       in
--         Cons head tail


-- HELPERS


pluralize : Int -> String -> String -> String
pluralize n singular plural =
  if n == 1 then
    singular
  else
    plural


-- SOLUTIONS TO EXERCISES


-- Exercise 1.9
remove : a -> List a -> List a
remove s list =
  case list of
    [] ->
      list

    x :: rest ->
      if x == s then
        remove s rest
      else
        x :: remove s rest


-- Exercise 1.15
duple : Int -> a -> List a
duple n x =
  if n <= 0 then
    []
  else
    x :: duple (n-1) x


-- Exercise 1.16
invert : List (a, b) -> List (b, a)
invert list =
  case list of
    [] ->
      []

    (x, y) :: rest ->
      (y, x) :: invert rest


-- Exercise 1.17
down : List a -> List (List a)
down list =
  case list of
    [] ->
      []

    x :: rest ->
      [x] :: down rest
--
-- or
--
-- (define (down lst)
--   (if (null? lst)
--       '()
--       (cons (list (car lst)) (down (cdr lst)))))
--
-- > (down '(1 2 3))
-- '((1) (2) (3))
-- > (down '((a) (fine) (idea)))
-- '(((a)) ((fine)) ((idea)))
-- > (down '(a (more (complicated)) object))
-- '((a) ((more (complicated))) (object))


-- Exercise 1.18
swapper : String -> String -> SList -> SList
swapper s1 s2 slist =
  case slist of
    Empty ->
      slist

    (Cons sexp rest) ->
      (Cons
        (swapperForSExp s1 s2 sexp)
        (swapper s1 s2 rest))


swapperForSExp : String -> String -> SExp -> SExp
swapperForSExp s1 s2 sexp =
  case sexp of
    Symbol s ->
      if s == s1 then
        Symbol s2
      else if s == s2 then
        Symbol s1
      else
        Symbol s

    SList slist ->
      SList (swapper s1 s2 slist)


-- Exercise 1.19
listSet : List a -> Int -> a -> List a
listSet list n x =
  if n < 0 then
    list
  else if n == 0 then
    case list of
      [] ->
        list

      _ :: rest ->
        x :: rest
  else
    case list of
      [] ->
        list

      y :: rest ->
        y :: listSet rest (n-1) x
