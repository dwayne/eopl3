module Interpreter
  ( Value(..)
  , Type(..)
  , Error(..), ParseError, RuntimeError(..)
  , run
  ) where


import qualified Env

import Data.Bifunctor (first)
import Parser


data Value
  = VNumber Number
  | VBool Bool
  | VProc Procedure

data Procedure
  = Procedure Id Expr Env

data Type
  = TNumber
  | TBool
  | TProc
  deriving (Eq, Show)

type Env = Env.Env Id Value Id Expr

data Error
  = SyntaxError ParseError
  | RuntimeError RuntimeError
  deriving (Eq, Show)

data RuntimeError
  = IdentifierNotFound Id
  | TypeError Type Type
  deriving (Eq, Show)


instance Eq Value where
  (VNumber n1) == (VNumber n2) = n1 == n2
  (VBool b1) == (VBool b2) = b1 == b2
  _ == _ = False


instance Show Value where
  show (VNumber n) = show n
  show (VBool b) = show b
  show (VProc _) = "<proc>"


run :: String -> Either Error Value
run input =
  case parse input of
    Left err ->
      Left $ SyntaxError err

    Right program ->
      first RuntimeError $ valueOfProgram program


valueOfProgram :: Program -> Either RuntimeError Value
valueOfProgram (Program expr) =
  valueOfExpr expr initEnv
  where
    initEnv =
      Env.extend "i" (VNumber 1)
        (Env.extend "v" (VNumber 5)
          (Env.extend "x" (VNumber 10)
            Env.empty))


valueOfExpr :: Expr -> Env -> Either RuntimeError Value
valueOfExpr expr env =
  case expr of
    Const n ->
      Right $ VNumber n

    Var x ->
      case Env.find x env of
        Just (Env.Value value) ->
          Right value

        Just (Env.Procedure param body savedEnv) ->
          Right $ VProc $ Procedure param body savedEnv

        Nothing ->
          Left $ IdentifierNotFound x

    Diff aExpr bExpr -> do
      aValue <- valueOfExpr aExpr env
      bValue <- valueOfExpr bExpr env
      diff aValue bValue

    Zero aExpr -> do
      aValue <- valueOfExpr aExpr env
      zero aValue

    If condition consequent alternative -> do
      conditionValue <- valueOfExpr condition env
      computeIf conditionValue consequent alternative env

    Let x aExpr body -> do
      aValue <- valueOfExpr aExpr env
      valueOfExpr body $ Env.extend x aValue env

    Let2 x xExpr y yExpr body -> do
      xValue <- valueOfExpr xExpr env
      yValue <- valueOfExpr yExpr env
      valueOfExpr body $
        Env.extend y yValue $
          Env.extend x xValue env
      -- N.B. This formulation of Let2 doesn't allow for the following:
      --
      -- let2 x = 5 y = -(x, 1) in -(x, y)
      -- => 1
      --
      -- For that we need to do the following instead:
      --
      -- xValue <- valueOfExpr xExpr env
      -- let env1 = Env.extend x xValue env
      -- yValue <- valueOfExpr yExpr env1
      -- let env2 = Env.extend y yValue env1
      -- valueOfExpr body env2
      --
      -- TODO: As an exercise try converting this one to CPS.

    Proc param body ->
      return $ VProc $ Procedure param body env

    Letrec name param body letrecBody ->
      valueOfExpr letrecBody $ Env.extendRec name param body env

    Call rator rand -> do
      ratorValue <- valueOfExpr rator env
      randValue <- valueOfExpr rand env
      apply ratorValue randValue


diff :: Value -> Value -> Either RuntimeError Value
diff aValue bValue = do
  a <- toNumber aValue
  b <- toNumber bValue
  return $ VNumber $ a - b


zero :: Value -> Either RuntimeError Value
zero aValue = do
  a <- toNumber aValue
  return $ VBool $ a == 0


computeIf :: Value -> Expr -> Expr -> Env -> Either RuntimeError Value
computeIf conditionValue consequent alternative env = do
  b <- toBool conditionValue
  let expr = if b then consequent else alternative
  valueOfExpr expr env


apply :: Value -> Value -> Either RuntimeError Value
apply ratorValue arg = do
  Procedure param body savedEnv <- toProcedure ratorValue
  valueOfExpr body $ Env.extend param arg savedEnv


toNumber :: Value -> Either RuntimeError Number
toNumber (VNumber n) = Right n
toNumber value = Left $ TypeError TNumber (typeOf value)


toBool :: Value -> Either RuntimeError Bool
toBool (VBool b) = Right b
toBool value = Left $ TypeError TBool (typeOf value)


toProcedure :: Value -> Either RuntimeError Procedure
toProcedure (VProc p) = Right p
toProcedure value = Left $ TypeError TProc (typeOf value)


typeOf :: Value -> Type
typeOf (VNumber _) = TNumber
typeOf (VBool _) = TBool
typeOf (VProc _) = TProc
