module Let.Interpreter (Value, run) where

import qualified Let.Env as Env

import Let.AST
import Let.Parser (parse)


type Value = Number

type Environment = Env.Env Id Value

run :: String -> Value
run = valueOfProgram . parse

valueOfProgram :: Program -> Value
valueOfProgram (Program expr) =
  valueOfExpr expr initEnv
  where
    initEnv =
      Env.extend "i" 1
        (Env.extend "v" 5
          (Env.extend "x" 10
            Env.empty))

valueOfExpr :: Expr -> Environment -> Value
valueOfExpr expr env =
  case expr of
    Const n ->
      n

    Var v ->
      Env.apply env v

    Diff a b ->
      valueOfExpr a env - valueOfExpr b env

    Minus e ->
      negate (valueOfExpr e env)

    Add a b ->
      valueOfExpr a env + valueOfExpr b env

    Mul a b ->
      valueOfExpr a env * valueOfExpr b env

    Div a b ->
      quot (valueOfExpr a env) (valueOfExpr b env)

    If test consequent alternative ->
      if valueOfBoolExpr test env then
        valueOfExpr consequent env
      else
        valueOfExpr alternative env

    Let var e body ->
      let
        val = valueOfExpr e env
      in
        valueOfExpr body (Env.extend var val env)

valueOfBoolExpr :: BoolExpr -> Environment -> Bool
valueOfBoolExpr expr env =
  case expr of
    Zero e ->
      valueOfExpr e env == 0

    Equal a b ->
      valueOfExpr a env == valueOfExpr b env

    Greater a b ->
      valueOfExpr a env > valueOfExpr b env

    Less a b ->
      valueOfExpr a env < valueOfExpr b env
