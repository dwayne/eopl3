module AST (Program(..), Expr(..), Number, Id) where


data Program = Program Expr deriving (Eq, Show)

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
  | List [Expr]
  | If Expr Expr Expr
  | Let Id Expr Expr
  | Proc Id Expr
  | Letrec [(Id, Id, Expr)] Expr
  | Call Expr Expr
  | Begin [Expr]
  | Assign Id Expr
  | Print Expr
  | Spawn Expr
  | Mutex
  | Wait Expr
  | Signal Expr
  | Yield
  deriving (Eq, Show)

type Number = Integer

type Id = String
