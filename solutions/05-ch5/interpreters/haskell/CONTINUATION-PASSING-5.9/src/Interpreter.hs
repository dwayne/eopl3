module Interpreter
  ( Value(..)
  , Type(..)
  , Error(..), ParseError, RuntimeError(..)
  , run
  ) where


import qualified Env
import qualified Store

import Data.Bifunctor (bimap, first)
import Parser


data Value
  = VNumber Number
  | VBool Bool
  | VRef Store.Ref
  | VProc Procedure

data Procedure
  = Procedure Id Expr Env

data Type
  = TNumber
  | TBool
  | TRef
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
  (VRef r1) == (VRef r2) = r1 == r2
  _ == _ = False


instance Show Value where
  show (VNumber n) = show n
  show (VBool b) = show b
  show (VRef r) = show r
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

    Newref aExpr -> do
      (aValue, store1) <- valueOfExpr aExpr env store
      newref aValue store1

    Deref aExpr -> do
      (aValue, store1) <- valueOfExpr aExpr env store
      deref aValue store1

    Setref lExpr rExpr -> do
      -- N.B. This can be improved.
      -- Current we find the value of both lExpr and rExpr before checking
      -- if lExpr is a VRef. If we do the check first then we can save an
      -- unnecessary evaluation when lExpr is not a VRef.
      (lValue, store1) <- valueOfExpr lExpr env store
      (rValue, store2) <- valueOfExpr rExpr env store1
      setref lValue rValue store2


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


newref :: Value -> Store -> Either RuntimeError (Value, Store)
newref value store =
  Right $ first VRef $ Store.newref value store


deref :: Value -> Store -> Either RuntimeError (Value, Store)
deref location store = do
  ref <- toRef location
  case Store.deref ref store of
    Just value ->
      Right (value, store)

    Nothing ->
      Left $ LocationNotFound ref


setref :: Value -> Value -> Store -> Either RuntimeError (Value, Store)
setref location value store = do
  ref <- toRef location
  case Store.setref ref value store of
    Just store1 ->
      Right (value, store1)

    Nothing ->
      Left $ LocationNotFound ref


toNumber :: Value -> Either RuntimeError Number
toNumber (VNumber n) = Right n
toNumber value = Left $ TypeError TNumber (typeOf value)


toBool :: Value -> Either RuntimeError Bool
toBool (VBool b) = Right b
toBool value = Left $ TypeError TBool (typeOf value)


toRef :: Value -> Either RuntimeError Store.Ref
toRef (VRef r) = Right r
toRef value = Left $ TypeError TRef (typeOf value)


toProcedure :: Value -> Either RuntimeError Procedure
toProcedure (VProc p) = Right p
toProcedure value = Left $ TypeError TProc (typeOf value)


typeOf :: Value -> Type
typeOf (VNumber _) = TNumber
typeOf (VBool _) = TBool
typeOf (VRef _) = TRef
typeOf (VProc _) = TProc
