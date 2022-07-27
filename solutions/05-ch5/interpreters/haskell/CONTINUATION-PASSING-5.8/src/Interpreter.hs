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
  | VList [Value]
  | VProc Procedure

data Procedure
  = Procedure [Id] Expr Env

data Type
  = TNumber
  | TBool
  | TList
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
  | EmptyListError
  | IncorrectNumberOfArguments Int Int
  deriving (Eq, Show)


instance Eq Value where
  (VNumber n1) == (VNumber n2) = n1 == n2
  (VBool b1) == (VBool b2) = b1 == b2
  (VList l1) == (VList l2) = l1 == l2
  _ == _ = False


instance Show Value where
  show (VNumber n) = show n
  show (VBool b) = show b
  show (VList l) = show l
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
          Right $ VProc $ Procedure [param] body savedEnv

        Nothing ->
          Left $ IdentifierNotFound x

    Diff aExpr bExpr -> do
      aValue <- valueOfExpr aExpr env
      bValue <- valueOfExpr bExpr env
      diff aValue bValue

    Cons aExpr bExpr -> do
      aValue <- valueOfExpr aExpr env
      bValue <- valueOfExpr bExpr env
      cons aValue bValue

    Car aExpr -> do
      aValue <- valueOfExpr aExpr env
      car aValue

    Cdr aExpr -> do
      aValue <- valueOfExpr aExpr env
      cdr aValue

    Null aExpr -> do
      aValue <- valueOfExpr aExpr env
      isNull aValue

    EmptyList -> do
      Right $ VList []

    List exprs ->
      list exprs env

    Zero aExpr -> do
      aValue <- valueOfExpr aExpr env
      zero aValue

    If condition consequent alternative -> do
      conditionValue <- valueOfExpr condition env
      computeIf conditionValue consequent alternative env

    Let bindingExprs body ->
      valueOfLetExpr bindingExprs body env env

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

    Let3 x xExpr y yExpr z zExpr body -> do
      xValue <- valueOfExpr xExpr env
      yValue <- valueOfExpr yExpr env
      zValue <- valueOfExpr zExpr env
      valueOfExpr body $
        Env.extend z zValue $
          Env.extend y yValue $
            Env.extend x xValue env

    Proc params body ->
      return $ VProc $ Procedure params body env

    Letrec name param body letrecBody ->
      valueOfExpr letrecBody $ Env.extendRec name param body env

    Call rator rands -> do
      ratorValue <- valueOfExpr rator env
      randValues <- mapM (flip valueOfExpr env) rands
      apply ratorValue randValues


valueOfLetExpr :: [(Id, Expr)] -> Expr -> Env -> Env -> Either RuntimeError Value
valueOfLetExpr bindings body env accEnv =
  case bindings of
    [] ->
      valueOfExpr body accEnv

    (x, xExpr) : tailExpr -> do
      xValue <- valueOfExpr xExpr env
      valueOfLetExpr tailExpr body env (Env.extend x xValue accEnv)


diff :: Value -> Value -> Either RuntimeError Value
diff aValue bValue = do
  a <- toNumber aValue
  b <- toNumber bValue
  return $ VNumber $ a - b


cons :: Value -> Value -> Either RuntimeError Value
cons x value = do
  l <- toList value
  return $ VList $ x : l


car :: Value -> Either RuntimeError Value
car value = do
  l <- toList value
  case l of
    x : _ ->
      Right x

    [] ->
      Left EmptyListError


cdr :: Value -> Either RuntimeError Value
cdr value = do
  l <- toList value
  case l of
    _ : rest ->
      Right $ VList rest

    [] ->
      Left EmptyListError


isNull :: Value -> Either RuntimeError Value
isNull value = do
  l <- toList value
  return $ VBool $ l == []


list :: [Expr] -> Env -> Either RuntimeError Value
list exprs env =
  listHelper exprs env []


listHelper :: [Expr] -> Env -> [Value] -> Either RuntimeError Value
listHelper exprs env values =
  case exprs of
    [] ->
      Right $ VList $ reverse values

    expr : rest -> do
      value <- valueOfExpr expr env
      listHelper rest env (value : values)


zero :: Value -> Either RuntimeError Value
zero aValue = do
  a <- toNumber aValue
  return $ VBool $ a == 0


computeIf :: Value -> Expr -> Expr -> Env -> Either RuntimeError Value
computeIf conditionValue consequent alternative env = do
  b <- toBool conditionValue
  let expr = if b then consequent else alternative
  valueOfExpr expr env


apply :: Value -> [Value] -> Either RuntimeError Value
apply ratorValue args = do
  Procedure params body savedEnv <- toProcedure ratorValue
  let nArgs = length args
  let nParams = length params
  if nArgs == nParams then
    valueOfExpr body $ Env.extendMany (zip params args) savedEnv
  else
    Left $ IncorrectNumberOfArguments nParams nArgs


toNumber :: Value -> Either RuntimeError Number
toNumber (VNumber n) = Right n
toNumber value = Left $ TypeError TNumber (typeOf value)


toBool :: Value -> Either RuntimeError Bool
toBool (VBool b) = Right b
toBool value = Left $ TypeError TBool (typeOf value)


toList :: Value -> Either RuntimeError [Value]
toList (VList l) = Right l
toList value = Left $ TypeError TList (typeOf value)


toProcedure :: Value -> Either RuntimeError Procedure
toProcedure (VProc p) = Right p
toProcedure value = Left $ TypeError TProc (typeOf value)


typeOf :: Value -> Type
typeOf (VNumber _) = TNumber
typeOf (VBool _) = TBool
typeOf (VList _) = TList
typeOf (VProc _) = TProc
