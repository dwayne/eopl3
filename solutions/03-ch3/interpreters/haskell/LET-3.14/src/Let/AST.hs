module Let.AST (Program(..), Expr(..), BoolExpr(..), Number, Id) where

data Program = Program Expr deriving Show

data Expr
  = Const Number
  | Var Id
  | Diff Expr Expr
  | Minus Expr
  | Add Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | If BoolExpr Expr Expr
  | Let Id Expr Expr
  deriving Show

data BoolExpr
  = Zero Expr
  | Equal Expr Expr
  | Greater Expr Expr
  | Less Expr Expr
  deriving Show

type Number = Integer

type Id = String
