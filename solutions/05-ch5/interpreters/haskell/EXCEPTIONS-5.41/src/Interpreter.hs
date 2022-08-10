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
  valueOfExpr expr initEnv endCont
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
          applyCont cont $ Right $ VProc $ Procedure param body savedEnv

        Nothing ->
          applyCont cont $ Left $ IdentifierNotFound x

    Diff aExpr bExpr ->
      valueOfExpr aExpr env (diff1Cont bExpr env cont)

    Zero aExpr ->
      valueOfExpr aExpr env (zeroCont cont)

    If condition consequent alternative ->
      valueOfExpr condition env (ifCont consequent alternative env cont)

    Let x aExpr body ->
      valueOfExpr aExpr env (letCont x body env cont)

    Proc param body ->
      applyCont cont $ Right $ VProc $ Procedure param body env

    Letrec name param body letrecBody ->
      valueOfExpr letrecBody (Env.extendRec name param body env) cont

    Call rator rand ->
      valueOfExpr rator env (ratorCont rand env cont)

    Try aExpr x handlerExpr ->
      valueOfExpr aExpr env (tryCont x handlerExpr env cont)

    Raise aExpr ->
      valueOfExpr aExpr env (raiseCont cont)


type Cont =
  ( Either RuntimeError Value -> Either RuntimeError Value
  , Value -> Either RuntimeError Value
  )


applyCont :: Cont -> Either RuntimeError Value -> Either RuntimeError Value
applyCont cont = fst cont


applyHandler :: Cont -> Value -> Either RuntimeError Value
applyHandler cont = snd cont


endCont :: Cont
endCont =
  ( \input -> do
      value <- input
      trace "End of computation" $
        Right value
  , Left . UncaughtException
  )


zeroCont :: Cont -> Cont
zeroCont cont =
  ( \input -> do
      value <- input
      applyCont cont $ zero value
  , applyHandler cont
  )


letCont :: Id -> Expr -> Env -> Cont -> Cont
letCont x body env cont =
  ( \input -> do
      value <- input
      valueOfExpr body (Env.extend x value env) cont
  , applyHandler cont
  )


ifCont :: Expr -> Expr -> Env -> Cont -> Cont
ifCont consequent alternative env cont =
  ( \input -> do
      value <- input
      computeIf value consequent alternative env cont
  , applyHandler cont
  )


diff1Cont :: Expr -> Env -> Cont -> Cont
diff1Cont bExpr env cont =
  ( \input -> do
      value <- input
      valueOfExpr bExpr env (diff2Cont value cont)
  , applyHandler cont
  )


diff2Cont :: Value -> Cont -> Cont
diff2Cont aValue cont =
  ( \input -> do
      value <- input
      applyCont cont $ diff aValue value
  , applyHandler cont
  )


ratorCont :: Expr -> Env -> Cont -> Cont
ratorCont rand env cont =
  ( \input -> do
      value <- input
      valueOfExpr rand env (randCont value cont)
  , applyHandler cont
  )


randCont :: Value -> Cont -> Cont
randCont ratorValue cont =
  ( \input -> do
      value <- input
      apply ratorValue value cont
  , applyHandler cont
  )


tryCont :: Id -> Expr -> Env -> Cont -> Cont
tryCont x handlerExpr savedEnv cont =
  ( applyCont cont
  , \value ->
      valueOfExpr handlerExpr (Env.extend x value savedEnv) cont
  )


raiseCont :: Cont -> Cont
raiseCont cont =
  ( \input -> do
      value <- input
      applyHandler cont value
  , applyHandler cont
  )


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


apply :: Value -> Value -> Cont -> Either RuntimeError Value
apply ratorValue arg cont = do
  Procedure param body savedEnv <- toProcedure ratorValue
  valueOfExpr body (Env.extend param arg savedEnv) cont


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
