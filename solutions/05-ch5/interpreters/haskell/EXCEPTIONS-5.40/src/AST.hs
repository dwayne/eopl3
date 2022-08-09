module AST (Program(..), Expr(..), Number, Id) where


data Program = Program Expr deriving (Eq, Show)

data Expr
  = Const Number
  | Var Id
  | Diff Expr Expr
  | Zero Expr
  | If Expr Expr Expr
  | Let Id Expr Expr
  | Proc Id Expr
  | Letrec Id Id Expr Expr
  | Call Expr Expr
  | Try Expr Id Id Expr
  | Raise Expr
  | Resume Expr Expr
  deriving (Eq, Show)

type Number = Integer

type Id = String
