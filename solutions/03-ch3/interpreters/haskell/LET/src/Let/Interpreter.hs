module Let.Interpreter (Value, run) where

import qualified Let.Env as Env

import Let.AST
import Let.Parser (parse)


data Value
  = VNumber Number
  | VBool Bool


type Environment = Env.Env Id Value


instance Show Value where
  show (VNumber n) = show n
  show (VBool b) = if b then "True" else "False"


run :: String -> Value
run = valueOfProgram . parse


valueOfProgram :: Program -> Value
valueOfProgram (Program expr) =
  valueOfExpr expr initEnv
  where
    initEnv =
      Env.extend "i" (VNumber 1)
        (Env.extend "v" (VNumber 5)
          (Env.extend "x" (VNumber 10)
            Env.empty))


valueOfExpr :: Expr -> Environment -> Value
valueOfExpr expr env =
  case expr of
    Const n ->
      VNumber n

    Var v ->
      Env.apply env v

    Diff a b ->
      let
        aVal = valueOfExpr a env
        bVal = valueOfExpr b env
      in
        VNumber (toNumber aVal - toNumber bVal)

    Zero e ->
      let
        val = valueOfExpr e env
      in
        VBool (toNumber val == 0)

    If test consequent alternative ->
      let
        testVal = valueOfExpr test env
      in
        if toBool testVal then
          valueOfExpr consequent env
        else
          valueOfExpr alternative env

    Let var e body ->
      let
        val = valueOfExpr e env
      in
        valueOfExpr body (Env.extend var val env)


toNumber :: Value -> Number
toNumber (VNumber n) = n
toNumber x = error ("Expected a number: " ++ show x)


toBool :: Value -> Bool
toBool (VBool b) = b
toBool x = error ("Expected a boolean: " ++ show x)
