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

run :: String -> Value
run = valueOfProgram . parse

valueOfProgram :: Program -> Value
valueOfProgram (Program expr) =
  valueOfExpr expr initEnv
  where
    initEnv =
      Env.extend "i" (NumberVal 1)
        (Env.extend "v" (NumberVal 5)
          (Env.extend "x" (NumberVal 10)
            Env.empty))

valueOfExpr :: Expr -> Environment -> Value
valueOfExpr expr env =
  case expr of
    Const n ->
      NumberVal n

    Var v ->
      Env.apply env v

    Diff a b ->
      let
        aVal = valueOfExpr a env
        bVal = valueOfExpr b env
      in
        NumberVal (toNumber aVal - toNumber bVal)

    Zero e ->
      let
        val = valueOfExpr e env
      in
        BoolVal (toNumber val == 0)

    If test consequent alternative ->
      let
        testVal = valueOfExpr test env
      in
        if (toBool testVal) then
          valueOfExpr consequent env
        else
          valueOfExpr alternative env

    Let bindings body ->
      let
        extendMany [] = env
        extendMany ((var, e):bs) =
          let
            val = valueOfExpr e env
          in
            Env.extend var val (extendMany bs)
      in
        valueOfExpr body (extendMany bindings)

toNumber :: Value -> Number
toNumber (NumberVal n) = n
toNumber x = error ("Expected a number: " ++ show x)

toBool :: Value -> Bool
toBool (BoolVal b) = b
toBool x = error ("Expected a boolean: " ++ show x)
