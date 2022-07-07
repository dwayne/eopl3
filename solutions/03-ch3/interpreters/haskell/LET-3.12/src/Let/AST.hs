module Let.AST (Program(..), Expr(..), Number, Id) where

data Program = Program Expr deriving Show

data Expr
  = Const Number
  | Var Id
  | Diff Expr Expr
  | Minus Expr
  | Add Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Zero Expr
  | Equal Expr Expr
  | Greater Expr Expr
  | Less Expr Expr
  | Cons Expr Expr
  | Car Expr
  | Cdr Expr
  | Null Expr
  | Empty
  | List [Expr]
  | If Expr Expr Expr
  | Cond [(Expr, Expr)]
  | Let Id Expr Expr
  deriving Show

type Number = Integer

type Id = String
