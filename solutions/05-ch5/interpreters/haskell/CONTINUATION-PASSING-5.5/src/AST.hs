module AST (Program(..), Expr(..), Number, Id) where


data Program = Program Expr deriving (Eq, Show)

data Expr
  = Const Number
  | Var Id
  | Diff Expr Expr
  | Cons Expr Expr
  | Car Expr
  | Cdr Expr
  | Null Expr
  | EmptyList
  | Zero Expr
  | If Expr Expr Expr
  | Let Id Expr Expr
  | Let2 Id Expr Id Expr Expr
  | Let3 Id Expr Id Expr Id Expr Expr
  | Proc Id Expr
  | Letrec Id Id Expr Expr
  | Call Expr Expr
  deriving (Eq, Show)

type Number = Integer

type Id = String
