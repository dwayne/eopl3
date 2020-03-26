module Let.Interpreter (Value, run) where

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
run = valueOfProgram . parse

valueOfProgram :: Program -> (Value, String)
valueOfProgram (Program expr) =
  valueOfExpr expr initEnv
  where
    initEnv =
      Env.extend "i" (NumberVal 1)
        (Env.extend "v" (NumberVal 5)
          (Env.extend "x" (NumberVal 10)
            Env.empty))

valueOfExpr :: Expr -> Environment -> (Value, String)
valueOfExpr expr env =
  case expr of
    Const n ->
      (NumberVal n, "")

    Var v ->
      (Env.apply env v, "")

    Diff a b ->
      let
        (aVal, s) = valueOfExpr a env
        (bVal, t) = valueOfExpr b env
      in
        (NumberVal (toNumber aVal - toNumber bVal), s ++ t)

    Zero e ->
      let
        (val, s) = valueOfExpr e env
      in
        (BoolVal (toNumber val == 0), s)

    If test consequent alternative ->
      let
        (testVal, s) = valueOfExpr test env
      in
        if (toBool testVal) then
          let
            (result, t) = valueOfExpr consequent env
          in
            (result, s ++ t)
        else
          let
            (result, t) = valueOfExpr alternative env
          in
            (result, s ++ t)

    Let var e body ->
      let
        (val, s) = valueOfExpr e env
        (result, t) = valueOfExpr body (Env.extend var val env)
      in
        (result, s ++ t)

    Print e ->
      let
        (val, s) = valueOfExpr e env
      in
        (NumberVal 1, s ++ show val ++ "\n")

toNumber :: Value -> Number
toNumber (NumberVal n) = n
toNumber x = error ("Expected a number: " ++ show x)

toBool :: Value -> Bool
toBool (BoolVal b) = b
toBool x = error ("Expected a boolean: " ++ show x)
