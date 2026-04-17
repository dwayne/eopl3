module AST.CPS_OUT (Program(..), TfExpr(..), SimpleExpr(..), Number, Id) where

data Program = Program TfExpr deriving Show

data TfExpr
  = Simple SimpleExpr
  | If SimpleExpr TfExpr TfExpr
  | Let Id SimpleExpr TfExpr
  | Letrec [(Id, [Id], TfExpr)] TfExpr
  | Call SimpleExpr [SimpleExpr]
  deriving Show

data SimpleExpr
  = Const Number
  | Var Id
  | Diff SimpleExpr SimpleExpr
  | Zero SimpleExpr
  | Proc Id TfExpr
  deriving Show

type Number = Integer

type Id = String
