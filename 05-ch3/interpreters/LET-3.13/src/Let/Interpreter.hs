module Let.Interpreter (Value, run) where

import qualified Let.Env as Env

import Let.AST
import Let.Parser (parse)

data Value
  = NumberVal Number

type Environment = Env.Env Id Value

instance Show Value where
  show (NumberVal n) = show n

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

    Minus e ->
      let
        val = valueOfExpr e env
      in
        NumberVal (negate (toNumber val))

    Add a b ->
      let
        aVal = valueOfExpr a env
        bVal = valueOfExpr b env
      in
        NumberVal (toNumber aVal + toNumber bVal)

    Mul a b ->
      let
        aVal = valueOfExpr a env
        bVal = valueOfExpr b env
      in
        NumberVal (toNumber aVal * toNumber bVal)

    Div a b ->
      let
        aVal = valueOfExpr a env
        bVal = valueOfExpr b env
      in
        NumberVal (quot (toNumber aVal) (toNumber bVal))

    Zero e ->
      let
        val = valueOfExpr e env
      in
        if (toNumber val == 0) then
          true
        else
          false

    Equal a b ->
      let
        aVal = valueOfExpr a env
        bVal = valueOfExpr b env
      in
        if (toNumber aVal == toNumber bVal) then
          true
        else
          false

    Greater a b ->
      let
        aVal = valueOfExpr a env
        bVal = valueOfExpr b env
      in
        if (toNumber aVal > toNumber bVal) then
          true
        else
          false

    Less a b ->
      let
        aVal = valueOfExpr a env
        bVal = valueOfExpr b env
      in
        if (toNumber aVal < toNumber bVal) then
          true
        else
          false

    If test consequent alternative ->
      let
        testVal = valueOfExpr test env
      in
        if (toNumber testVal /= 0) then
          valueOfExpr consequent env
        else
          valueOfExpr alternative env

    Let var e body ->
      let
        val = valueOfExpr e env
      in
        valueOfExpr body (Env.extend var val env)

toNumber :: Value -> Number
toNumber (NumberVal n) = n

true :: Value
true = NumberVal 1

false :: Value
false = NumberVal 0
