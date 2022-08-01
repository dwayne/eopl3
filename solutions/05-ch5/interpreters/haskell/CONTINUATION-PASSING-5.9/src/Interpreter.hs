module Interpreter
  ( Value(..)
  , Type(..)
  , Error(..), ParseError, RuntimeError(..)
  , run
  ) where


import qualified Env
import qualified Store

import Data.Bifunctor (bimap)
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

type Store = Store.Store Value

data Error
  = SyntaxError ParseError
  | RuntimeError RuntimeError
  deriving (Eq, Show)

data RuntimeError
  = IdentifierNotFound Id
  | LocationNotFound Store.Ref
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
      bimap RuntimeError fst $ valueOfProgram program


valueOfProgram :: Program -> Either RuntimeError (Value, Store)
valueOfProgram (Program expr) =
  valueOfExpr expr initEnv initStore
  where
    initEnv =
      Env.extend "i" (VNumber 1)
        (Env.extend "v" (VNumber 5)
          (Env.extend "x" (VNumber 10)
            Env.empty))

    initStore =
      Store.empty


valueOfExpr :: Expr -> Env -> Store -> Either RuntimeError (Value, Store)
valueOfExpr expr env store =
  case expr of
    Const n ->
      Right (VNumber n, store)

    Var x ->
      case Env.find x env of
        Just (Env.Value value) ->
          Right (value, store)

        Just (Env.Procedure param body savedEnv) ->
          Right (VProc $ Procedure param body savedEnv, store)

        Nothing ->
          Left $ IdentifierNotFound x

    Diff aExpr bExpr -> do
      (aValue, store1) <- valueOfExpr aExpr env store
      (bValue, store2) <- valueOfExpr bExpr env store1
      diff aValue bValue store2

    Zero aExpr -> do
      (aValue, store1) <- valueOfExpr aExpr env store
      zero aValue store1

    If condition consequent alternative -> do
      (conditionValue, store1) <- valueOfExpr condition env store
      computeIf conditionValue consequent alternative env store1

    Let x aExpr body -> do
      (aValue, store1) <- valueOfExpr aExpr env store
      valueOfExpr body (Env.extend x aValue env) store1

    Proc param body ->
      Right (VProc $ Procedure param body env, store)

    Letrec declarations letrecBody ->
      valueOfExpr letrecBody (Env.extendRec declarations env) store

    Call rator rand -> do
      (ratorValue, store1) <- valueOfExpr rator env store
      (randValue, store2) <- valueOfExpr rand env store1
      apply ratorValue randValue store2

    Begin exprs -> do
      computeBegin exprs env store


diff :: Value -> Value -> Store -> Either RuntimeError (Value, Store)
diff aValue bValue store = do
  a <- toNumber aValue
  b <- toNumber bValue
  Right (VNumber $ a - b, store)


zero :: Value -> Store -> Either RuntimeError (Value, Store)
zero aValue store = do
  a <- toNumber aValue
  Right (VBool $ a == 0, store)


computeIf :: Value -> Expr -> Expr -> Env -> Store -> Either RuntimeError (Value, Store)
computeIf conditionValue consequent alternative env store = do
  a <- toBool conditionValue
  let expr = if a then consequent else alternative
  valueOfExpr expr env store


apply :: Value -> Value -> Store -> Either RuntimeError (Value, Store)
apply ratorValue arg store = do
  Procedure param body savedEnv <- toProcedure ratorValue
  valueOfExpr body (Env.extend param arg savedEnv) store


computeBegin :: [Expr] -> Env -> Store -> Either RuntimeError (Value, Store)
computeBegin exprs env store =
  case exprs of
    [expr] ->
      valueOfExpr expr env store

    expr : restExprs -> do
      (_, store1) <- valueOfExpr expr env store
      computeBegin restExprs env store1

    [] ->
      -- N.B. Based on the grammar this condition will never be reached.
      -- Maybe we can make that clearer in the AST by using a
      -- non-empty list of expressions.
      --
      -- See Data.List.NonEmpty in base.
      undefined


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
