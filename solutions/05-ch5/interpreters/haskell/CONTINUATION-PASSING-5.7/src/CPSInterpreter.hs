module CPSInterpreter
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
  | VList [Value]
  | VProc Procedure

data Procedure
  = Procedure Id Expr Env

data Type
  = TNumber
  | TBool
  | TList
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
  | EmptyListError
  deriving (Eq, Show)


instance Eq Value where
  (VNumber n1) == (VNumber n2) = n1 == n2
  (VBool b1) == (VBool b2) = b1 == b2
  (VList l1) == (VList l2) = l1 == l2
  _ == _ = False


instance Show Value where
  show (VNumber n) = show n
  show (VBool b) = show b
  show (VList l) = show l
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
          applyCont cont $ Right $ VProc $ Procedure param body savedEnv

        Nothing ->
          applyCont cont $ Left $ IdentifierNotFound x

    Diff aExpr bExpr ->
      valueOfExpr aExpr env (Diff1Cont bExpr env cont)

    Cons aExpr bExpr ->
      valueOfExpr aExpr env (Cons1Cont bExpr env cont)

    Car aExpr ->
      valueOfExpr aExpr env (CarCont cont)

    Cdr aExpr ->
      valueOfExpr aExpr env (CdrCont cont)

    Null aExpr ->
      valueOfExpr aExpr env (NullCont cont)

    EmptyList ->
      applyCont cont $ Right $ VList []

    List exprs ->
      applyCont (HeadCont exprs env cont) $ Right $ VList []

    Zero aExpr ->
      valueOfExpr aExpr env (ZeroCont cont)

    If condition consequent alternative ->
      valueOfExpr condition env (IfCont consequent alternative env cont)

    Let bindings body ->
      case bindings of
        [] ->
          valueOfExpr body env cont

        (x, xExpr) : tailExpr ->
          valueOfExpr xExpr env (LetCont x tailExpr body env env cont)

    Let2 x xExpr y yExpr body ->
      valueOfExpr xExpr env (Let2XCont x y yExpr body env cont)

    Let3 x xExpr y yExpr z zExpr body ->
      valueOfExpr xExpr env (Let3XCont x y yExpr z zExpr body env cont)

    Proc param body ->
      applyCont cont $ Right $ VProc $ Procedure param body env

    Letrec name param body letrecBody ->
      valueOfExpr letrecBody (Env.extendRec name param body env) cont

    Call rator rand ->
      valueOfExpr rator env (RatorCont rand env cont)


data Cont
  = EndCont
  | ZeroCont Cont
  | LetCont Id [(Id, Expr)] Expr Env Env Cont
  | Let2XCont Id Id Expr Expr Env Cont
  | Let2YCont Id Value Id Expr Env Cont
  | Let3XCont Id Id Expr Id Expr Expr Env Cont
  | Let3YCont Id Value Id Id Expr Expr Env Cont
  | Let3ZCont Id Value Id Value Id Expr Env Cont
  | IfCont Expr Expr Env Cont
  | Diff1Cont Expr Env Cont
  | Diff2Cont Value Cont
  | RatorCont Expr Env Cont
  | RandCont Value Cont
  | Cons1Cont Expr Env Cont
  | Cons2Cont Value Cont
  | CarCont Cont
  | CdrCont Cont
  | NullCont Cont
  | HeadCont [Expr] Env Cont
  | TailCont [Expr] [Value] Env Cont


applyCont :: Cont -> Either RuntimeError Value -> Either RuntimeError Value
applyCont cont input = do
  value <- input
  case cont of
    EndCont ->
      trace "End of computation" $
        Right value

    ZeroCont nextCont ->
      applyCont nextCont $ zero value

    LetCont x xTailExpr body env accEnv nextCont ->
      let
        newAccEnv =
          Env.extend x value accEnv
      in
      case xTailExpr of
        [] ->
          valueOfExpr body newAccEnv nextCont

        (y, yExpr) : yTailExpr ->
          valueOfExpr yExpr env $
            LetCont y yTailExpr body env newAccEnv nextCont

    Let2XCont x y yExpr body env nextCont ->
      valueOfExpr yExpr env (Let2YCont x value y body env nextCont)

    Let2YCont x xValue y body env1 nextCont ->
      let
        env2 =
          Env.extend y value $
            Env.extend x xValue env1
      in
      valueOfExpr body env2 nextCont

    Let3XCont x y yExpr z zExpr body env nextCont ->
      valueOfExpr yExpr env (Let3YCont x value y z zExpr body env nextCont)

    Let3YCont x xValue y z zExpr body env nextCont ->
      valueOfExpr zExpr env (Let3ZCont x xValue y value z body env nextCont)

    Let3ZCont x xValue y yValue z body env1 nextCont ->
      let
        env2 =
          Env.extend z value $
            Env.extend y yValue $
              Env.extend x xValue env1
      in
        valueOfExpr body env2 nextCont

    IfCont consequent alternative env nextCont ->
      computeIf value consequent alternative env nextCont

    Diff1Cont bExpr env nextCont ->
      valueOfExpr bExpr env (Diff2Cont value nextCont)

    Diff2Cont aValue nextCont ->
      applyCont nextCont $ diff aValue value

    RatorCont rand env nextCont ->
      valueOfExpr rand env (RandCont value nextCont)

    RandCont ratorValue nextCont ->
      apply ratorValue value nextCont

    Cons1Cont bExpr env nextCont ->
      valueOfExpr bExpr env (Cons2Cont value nextCont)

    Cons2Cont aValue nextCont ->
      applyCont nextCont $ cons aValue value

    CarCont nextCont ->
      applyCont nextCont $ car value

    CdrCont nextCont ->
      applyCont nextCont $ cdr value

    NullCont nextCont ->
      applyCont nextCont $ isNull value

    HeadCont exprs env nextCont ->
      case exprs of
        [] ->
          input

        headExpr : tailExpr ->
          valueOfExpr headExpr env (TailCont tailExpr [] env nextCont)

    TailCont exprs values env nextCont ->
      case exprs of
        [] ->
          applyCont nextCont $ Right $ VList $ reverse $ value : values

        expr : rest ->
          valueOfExpr expr env (TailCont rest (value : values) env nextCont)


diff :: Value -> Value -> Either RuntimeError Value
diff aValue bValue = do
  a <- toNumber aValue
  b <- toNumber bValue
  return $ VNumber $ a - b


cons :: Value -> Value -> Either RuntimeError Value
cons x value = do
  l <- toList value
  return $ VList $ x : l


car :: Value -> Either RuntimeError Value
car value = do
  l <- toList value
  case l of
    x : _ ->
      Right x

    [] ->
      Left EmptyListError


cdr :: Value -> Either RuntimeError Value
cdr value = do
  l <- toList value
  case l of
    _ : rest ->
      Right $ VList rest

    [] ->
      Left EmptyListError


isNull :: Value -> Either RuntimeError Value
isNull value = do
  l <- toList value
  return $ VBool $ l == []


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


toList :: Value -> Either RuntimeError [Value]
toList (VList l) = Right l
toList value = Left $ TypeError TList (typeOf value)


toProcedure :: Value -> Either RuntimeError Procedure
toProcedure (VProc p) = Right p
toProcedure value = Left $ TypeError TProc (typeOf value)


typeOf :: Value -> Type
typeOf (VNumber _) = TNumber
typeOf (VBool _) = TBool
typeOf (VList _) = TList
typeOf (VProc _) = TProc
