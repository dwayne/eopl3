module Let.Interpreter (Value, run) where

import Control.Monad.Writer

import qualified Let.Env as Env

import Let.AST
import Let.Parser (parse)

data Value
  = NumberVal Number
  | BoolVal Bool

type Environment = Env.Env Id Value

instance Show Value where
  show (NumberVal n) = show n
  show (BoolVal b) = if b then "True" else "False"

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

    Print e -> do
      val <- valueOfExpr e env

      tell $ show val ++ "\n"

      return $ NumberVal 1

toNumber :: Value -> Number
toNumber (NumberVal n) = n
toNumber x = error ("Expected a number: " ++ show x)

toBool :: Value -> Bool
toBool (BoolVal b) = b
toBool x = error ("Expected a boolean: " ++ show x)
