module Interpreter
  ( Value(..)
  , Type(..)
  , Error(..), ParseError, RuntimeError(..)
  , run
  ) where


import qualified Env

import Data.Bifunctor (first)
import Debug.Trace (trace)
import Parser


data Value
  = VNumber Number
  | VBool Bool
  | VProc Procedure

data Procedure
  = Procedure [Id] Expr Env

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
  | UncaughtException Value
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
  valueOfExpr expr initEnv EndCont
  where
    initEnv =
      Env.extend "i" (VNumber 1)
        (Env.extend "v" (VNumber 5)
          (Env.extend "x" (VNumber 10)
            Env.empty))


valueOfExpr :: Expr -> Env -> Cont -> Either RuntimeError Value
valueOfExpr expr env cont =
  case expr of
    Const n ->
      applyCont cont $ Right $ VNumber n

    Var x ->
      case Env.find x env of
        Just (Env.Value value) ->
          applyCont cont $ Right value

        Just (Env.Procedure param body savedEnv) ->
          applyCont cont $ Right $ VProc $ Procedure [param] body savedEnv

        Nothing ->
          applyCont cont $ Left $ IdentifierNotFound x

    Diff aExpr bExpr ->
      valueOfExpr aExpr env (Diff1Cont bExpr env cont)

    Zero aExpr ->
      valueOfExpr aExpr env (ZeroCont cont)

    If condition consequent alternative ->
      valueOfExpr condition env (IfCont consequent alternative env cont)

    Let x aExpr body ->
      valueOfExpr aExpr env (LetCont x body env cont)

    Proc params body ->
      applyCont cont $ Right $ VProc $ Procedure params body env

    Letrec name param body letrecBody ->
      valueOfExpr letrecBody (Env.extendRec name param body env) cont

    Call rator rands ->
      valueOfExpr rator env (RatorCont rands env cont)

    Try aExpr x handlerExpr ->
      valueOfExpr aExpr env (TryCont x handlerExpr env cont)

    Raise aExpr ->
      valueOfExpr aExpr env (RaiseCont cont)


data Cont
  = EndCont
  | ZeroCont Cont
  | LetCont Id Expr Env Cont
  | IfCont Expr Expr Env Cont
  | Diff1Cont Expr Env Cont
  | Diff2Cont Value Cont
  | RatorCont [Expr] Env Cont
  | RandsCont Value [Value] [Expr] Env Cont
  | TryCont Id Expr Env Cont
  | RaiseCont Cont


applyCont :: Cont -> Either RuntimeError Value -> Either RuntimeError Value
applyCont cont input = do
  value <- input
  case cont of
    EndCont ->
      trace "End of computation" $
        Right value

    ZeroCont nextCont ->
      applyCont nextCont $ zero value

    LetCont x body env nextCont ->
      valueOfExpr body (Env.extend x value env) nextCont

    IfCont consequent alternative env nextCont ->
      computeIf value consequent alternative env nextCont

    Diff1Cont bExpr env nextCont ->
      valueOfExpr bExpr env (Diff2Cont value nextCont)

    Diff2Cont aValue nextCont ->
      applyCont nextCont $ diff aValue value

    RatorCont rands env nextCont ->
      case rands of
        [] ->
          apply value [] nextCont

        rand : restRands ->
          valueOfExpr rand env (RandsCont value [] restRands env nextCont)

    RandsCont ratorValue revArgs rands env nextCont ->
      let
        nextRevArgs =
          value : revArgs
      in
      case rands of
        [] ->
          apply ratorValue (reverse nextRevArgs) nextCont

        rand : restRands ->
          valueOfExpr rand env (RandsCont ratorValue nextRevArgs restRands env nextCont)

    TryCont _ _ _ nextCont ->
      applyCont nextCont input

    RaiseCont nextCont ->
      applyHandler nextCont value


applyHandler :: Cont -> Value -> Either RuntimeError Value
applyHandler cont value =
  case cont of
    TryCont x handlerExpr savedEnv nextCont ->
      valueOfExpr handlerExpr (Env.extend x value savedEnv) nextCont

    EndCont ->
      Left $ UncaughtException value

    ZeroCont nextCont ->
      applyHandler nextCont value

    LetCont _ _ _ nextCont ->
      applyHandler nextCont value

    IfCont _ _ _ nextCont ->
      applyHandler nextCont value

    Diff1Cont _ _ nextCont ->
      applyHandler nextCont value

    Diff2Cont _ nextCont ->
      applyHandler nextCont value

    RatorCont _ _ nextCont ->
      applyHandler nextCont value

    RandsCont _ _ _ _ nextCont ->
      applyHandler nextCont value

    RaiseCont nextCont ->
      applyHandler nextCont value


diff :: Value -> Value -> Either RuntimeError Value
diff aValue bValue = do
  a <- toNumber aValue
  b <- toNumber bValue
  return $ VNumber $ a - b


zero :: Value -> Either RuntimeError Value
zero aValue = do
  a <- toNumber aValue
  return $ VBool $ a == 0


computeIf :: Value -> Expr -> Expr -> Env -> Cont -> Either RuntimeError Value
computeIf conditionValue consequent alternative env cont = do
  b <- toBool conditionValue
  let expr = if b then consequent else alternative
  valueOfExpr expr env cont


apply :: Value -> [Value] -> Cont -> Either RuntimeError Value
apply ratorValue args cont = do
  Procedure params body savedEnv <- toProcedure ratorValue
  let nParams = length params
  let nArgs = length args
  if nArgs == nParams then
    valueOfExpr body (Env.extendMany (zip params args) savedEnv) cont
  else
    valueOfExpr (Raise $ Const $ fromIntegral $ nParams - nArgs) savedEnv cont


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
