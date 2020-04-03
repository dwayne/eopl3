module Proc.Interpreter (Value, run) where

import Control.Monad.Writer

import qualified Proc.Env as Env

import Proc.AST
import Proc.Parser (parse)

data Value
  = NumberVal Number
  | BoolVal Bool
  | ProcedureVal Procedure

type Environment = Env.Env Id Value

instance Show Value where
  show (NumberVal n) = show n
  show (BoolVal b) = if b then "True" else "False"
  show (ProcedureVal _) = "<<proc>>"

run :: String -> (Value, String)
run = runWriter . valueOfProgram . parse

valueOfProgram :: Program -> Writer String Value
valueOfProgram (Program expr) =
  valueOfExpr expr initEnv
  where
    initEnv =
      Env.extend "i" (NumberVal 1)
        (Env.extend "v" (NumberVal 5)
          (Env.extend "x" (NumberVal 10)
            Env.empty))

valueOfExpr :: Expr -> Environment -> Writer String Value
valueOfExpr expr env =
  case expr of
    Const n ->
      return $ NumberVal n

    Var v ->
      return $ Env.apply env v

    Diff a b -> do
      aVal <- valueOfExpr a env
      bVal <- valueOfExpr b env
      return $ NumberVal (toNumber aVal - toNumber bVal)

    Zero e -> do
      val <- valueOfExpr e env
      return $ BoolVal (toNumber val == 0)

    If test consequent alternative -> do
      testVal <- valueOfExpr test env

      if (toBool testVal) then
        valueOfExpr consequent env
      else
        valueOfExpr alternative env

    Let var e body -> do
      val <- valueOfExpr e env
      valueOfExpr body (Env.extend var val env)

    Proc trace var body ->
      return $ ProcedureVal (procedure trace var body env)

    Call f arg -> do
      fVal <- valueOfExpr f env
      argVal <- valueOfExpr arg env

      applyProcedure (toProcedure fVal) argVal

toNumber :: Value -> Number
toNumber (NumberVal n) = n
toNumber x = error ("Expected a number: " ++ show x)

toBool :: Value -> Bool
toBool (BoolVal b) = b
toBool x = error ("Expected a boolean: " ++ show x)

toProcedure :: Value -> Procedure
toProcedure (ProcedureVal p) = p
toProcedure x = error ("Expected a procedure: " ++ show x)

-- Procedure ADT

data Procedure = Procedure Bool Id Expr Environment

procedure :: Bool -> Id -> Expr -> Environment -> Procedure
procedure = Procedure

applyProcedure :: Procedure -> Value -> Writer String Value
applyProcedure (Procedure trace var body env) val =
  if trace then do
    let binding = var ++ "=" ++ show val

    tell $ "Entering: " ++ binding ++ "\n"

    result <- valueOfExpr body (Env.extend var val env)

    tell $ "Leaving: " ++ binding ++ " result=" ++ show result ++ "\n"

    return result
  else
    valueOfExpr body (Env.extend var val env)
