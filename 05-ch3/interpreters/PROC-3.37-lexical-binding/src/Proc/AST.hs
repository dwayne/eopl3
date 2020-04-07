module Proc.AST (Program(..), Expr(..), Number, Id) where

data Program = Program Expr deriving Show

data Expr
  = Const Number
  | Var Id
  | Mul Expr Expr
  | Diff Expr Expr
  | Add1 Expr
  | Zero Expr
  | If Expr Expr Expr
  | Let Id Expr Expr
  | Proc Id Expr
  | Call Expr Expr
  deriving Show

type Number = Integer

type Id = String
