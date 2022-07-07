module Nameless.AST.AST (Program(..), Expr(..), Number, Id) where

data Program = Program Expr deriving Show

data Expr
  = Const Number
  | Var Id
  | Diff Expr Expr
  | Zero Expr
  | Cons Expr Expr
  | Car Expr
  | Cdr Expr
  | Null Expr
  | Empty
  | If Expr Expr Expr
  | Let Id Expr Expr
  | Unpack [Id] Expr Expr
  | Proc Id Expr
  | Call Expr Expr
  deriving Show

type Number = Integer

type Id = String
