module Nameless.AST.Nameless (Program(..), Expr(..), Number) where

data Program = Program Expr deriving Show

data Expr
  = Const Number
  | Var (Int, Int)
  | Diff Expr Expr
  | Zero Expr
  | If Expr Expr Expr
  | Let [Expr] Expr
  | Proc Expr
  | Call Expr [Expr]
  deriving Show

type Number = Integer
