module Nameless.AST.AST (Program(..), Expr(..), Number, Id) where

data Program = Program Expr deriving Show

data Expr
  = Const Number
  | Var Id
  | Diff Expr Expr
  | Zero Expr
  | If Expr Expr Expr
  | Cond [(Expr, Expr)]
  | Let Id Expr Expr
  | Proc Id Expr
  | Call Expr Expr
  deriving Show

type Number = Integer

type Id = String
