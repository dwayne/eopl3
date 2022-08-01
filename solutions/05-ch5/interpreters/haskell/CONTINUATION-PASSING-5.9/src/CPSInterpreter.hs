module CPSInterpreter
  ( Value(..)
  , Type(..)
  , Error(..), ParseError, RuntimeError(..)
  , run
  ) where


import qualified Env
import qualified Store

import Data.Bifunctor (bimap)
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

type Env = Env.Env Id Store.Ref Id Expr

type Store = Store.Store Value

data Error
  = SyntaxError ParseError
  | RuntimeError RuntimeError
  deriving (Eq, Show)

data RuntimeError
  = IdentifierNotFound Id
  | LocationNotFound Store.Ref
  | TypeError Type Type
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
      bimap RuntimeError fst $ valueOfProgram program


valueOfProgram :: Program -> Either RuntimeError (Value, Store)
valueOfProgram (Program expr) =
  valueOfExpr expr initEnv initStore EndCont
  where
    store0 = Store.empty
    (iRef, store1) = Store.newref (VNumber 1) store0
    (vRef, store2) = Store.newref (VNumber 5) store1
    (xRef, initStore) = Store.newref (VNumber 10) store2

    initEnv =
      Env.extend "i" iRef
        (Env.extend "v" vRef
          (Env.extend "x" xRef
            Env.empty))


valueOfExpr :: Expr -> Env -> Store -> Cont -> Either RuntimeError (Value, Store)
valueOfExpr expr env store cont =
  case expr of
    Const n ->
      applyCont cont $ Right (VNumber n, store)

    Var x ->
      applyCont cont $
        case find x env store of
          Just (ref, store1) ->
            case Store.deref ref store1 of
              Just value ->
                Right (value, store1)

              Nothing ->
                Left $ LocationNotFound ref

          Nothing ->
            Left $ IdentifierNotFound x

    Diff aExpr bExpr ->
      valueOfExpr aExpr env store (Diff1Cont bExpr env cont)

    Zero aExpr ->
      valueOfExpr aExpr env store (ZeroCont cont)

    If condition consequent alternative ->
      valueOfExpr condition env store (IfCont consequent alternative env cont)

    Let x aExpr body ->
      valueOfExpr aExpr env store (LetCont x body env cont)

    Proc param body ->
      applyCont cont $ Right (VProc $ Procedure param body env, store)

    Letrec declarations letrecBody ->
      valueOfExpr letrecBody (Env.extendRec declarations env) store cont

    Call rator rand ->
      valueOfExpr rator env store (RatorCont rand env cont)

    Begin exprs ->
      computeBegin exprs env store cont

    Assign x aExpr ->
      case find x env store of
        Just (ref, store1) ->
          valueOfExpr aExpr env store1 (AssignCont ref cont)

        Nothing ->
          Left $ IdentifierNotFound x


data Cont
  = EndCont
  | ZeroCont Cont
  | LetCont Id Expr Env Cont
  | Diff1Cont Expr Env Cont
  | Diff2Cont Value Cont
  | IfCont Expr Expr Env Cont
  | RatorCont Expr Env Cont
  | RandCont Value Cont
  | BeginCont [Expr] Env Cont
  | AssignCont Store.Ref Cont


applyCont :: Cont -> Either RuntimeError (Value, Store) -> Either RuntimeError (Value, Store)
applyCont cont input = do
  (value, store) <- input
  case cont of
    EndCont ->
      trace "End of computation" $
        Right (value, store)

    ZeroCont nextCont ->
      zero value store nextCont

    LetCont x body env nextCont ->
      let
        (aRef, store1) =
          Store.newref value store
      in
      valueOfExpr body (Env.extend x aRef env) store1 nextCont

    Diff1Cont bExpr env nextCont ->
      valueOfExpr bExpr env store (Diff2Cont value nextCont)

    Diff2Cont aValue nextCont ->
      diff aValue value store nextCont

    IfCont consequent alternative env nextCont ->
      computeIf value consequent alternative env store nextCont

    RatorCont rand env nextCont ->
      valueOfExpr rand env store (RandCont value nextCont)

    RandCont ratorValue nextCont ->
      apply ratorValue value store nextCont

    BeginCont restExprs env nextCont ->
      computeBegin restExprs env store nextCont

    AssignCont ref nextCont ->
      applyCont nextCont $
        case Store.setref ref value store of
          Just store1 ->
            Right (value, store1)

          Nothing ->
            Left $ LocationNotFound ref


find :: Id -> Env -> Store -> Maybe (Store.Ref, Store)
find x env store =
  case Env.find x env of
    Just (Env.Value ref) ->
      Just (ref, store)

    Just (Env.Procedure param body savedEnv) ->
      let
        value =
          VProc $ Procedure param body savedEnv

        (ref, store1) =
          Store.newref value store
      in
      Just (ref, store1)

    Nothing ->
      Nothing


diff :: Value -> Value -> Store -> Cont -> Either RuntimeError (Value, Store)
diff aValue bValue store cont = do
  a <- toNumber aValue
  b <- toNumber bValue
  applyCont cont $ Right (VNumber $ a - b, store)


zero :: Value -> Store -> Cont -> Either RuntimeError (Value, Store)
zero aValue store cont = do
  a <- toNumber aValue
  applyCont cont $ Right (VBool $ a == 0, store)


computeIf :: Value -> Expr -> Expr -> Env -> Store -> Cont -> Either RuntimeError (Value, Store)
computeIf conditionValue consequent alternative env store cont = do
  a <- toBool conditionValue
  let expr = if a then consequent else alternative
  valueOfExpr expr env store cont


apply :: Value -> Value -> Store -> Cont -> Either RuntimeError (Value, Store)
apply ratorValue arg store cont = do
  Procedure param body savedEnv <- toProcedure ratorValue
  let (argRef, store1) = Store.newref arg store
  valueOfExpr body (Env.extend param argRef savedEnv) store1 cont


computeBegin :: [Expr] -> Env -> Store -> Cont -> Either RuntimeError (Value, Store)
computeBegin exprs env store cont =
  case exprs of
    [expr] ->
      valueOfExpr expr env store cont

    expr : restExprs -> do
      valueOfExpr expr env store (BeginCont restExprs env cont)

    [] ->
      -- N.B. Based on the grammar this condition will never be reached.
      -- Maybe we can make that clearer in the AST by using a
      -- non-empty list of expressions.
      --
      -- See Data.List.NonEmpty in base.
      undefined


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
