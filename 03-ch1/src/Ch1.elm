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
