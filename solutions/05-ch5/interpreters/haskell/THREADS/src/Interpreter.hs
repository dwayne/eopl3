module Interpreter
  ( Value(..)
  , Type(..)
  , Error(..), ParseError, RuntimeError(..)
  , run
  , runIO
  ) where


import qualified Env
import qualified Store

import Data.Bifunctor (bimap)
import Data.List (intercalate)
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

type Env = Env.Env Id Store.Ref Id Expr

type Store = Store.Store Value

data Error
  = SyntaxError ParseError
  | RuntimeError RuntimeError
  deriving (Eq, Show)

data RuntimeError
  = IdentifierNotFound Id
  | LocationNotFound Store.Ref
  | EmptyListError
  | TypeError Type Type
  deriving (Eq, Show)


instance Eq Value where
  (VNumber n1) == (VNumber n2) = n1 == n2
  (VBool b1) == (VBool b2) = b1 == b2
  (VList l1) == (VList l2) = l1 == l2
  _ == _ = False


instance Show Value where
  show (VNumber n) = show n
  show (VBool b) = show b
  show (VList l) = "[" ++ intercalate ", " (map show l) ++ "]"
  show (VProc _) = "<proc>"


run :: String -> Either Error Value
run input =
  case parse input of
    Left err ->
      Left $ SyntaxError err

    Right program ->
      bimap RuntimeError fst $ valueOfProgram program


--
-- NOTE: You can test print using runIO.
--
-- Example 1:
--
-- $ stack ghci
-- > runIO "let x = 5 in begin print(list(1, 2, 3)); set x = 6; print(x); -(x, 1) end"
-- End of computation
-- [1, 2, 3]
-- 6
-- 5
--
runIO :: String -> IO Value
runIO input =
  case parse input of
    Left err ->
      error $ show $ SyntaxError err

    Right program ->
      case valueOfProgram program of
        Left err ->
          error $ show $ RuntimeError err

        Right (value, (_, io)) ->
          io >> return value


valueOfProgram :: Program -> Either RuntimeError (Value, (Store, IO ()))
valueOfProgram (Program expr) =
  valueOfExpr expr initEnv initStore (return ()) EndCont
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


valueOfExpr :: Expr -> Env -> Store -> IO () -> Cont -> Either RuntimeError (Value, (Store, IO ()))
valueOfExpr expr env store io cont =
  case expr of
    Const n ->
      applyCont cont $ Right (VNumber n, (store, io))

    Var x ->
      applyCont cont $
        case find x env store of
          Just (ref, store1) ->
            case Store.deref ref store1 of
              Just value ->
                Right (value, (store1, io))

              Nothing ->
                Left $ LocationNotFound ref

          Nothing ->
            Left $ IdentifierNotFound x

    Diff aExpr bExpr ->
      valueOfExpr aExpr env store io (Diff1Cont bExpr env cont)

    Zero aExpr ->
      valueOfExpr aExpr env store io (ZeroCont cont)

    Cons aExpr bExpr ->
      valueOfExpr aExpr env store io (Cons1Cont bExpr env cont)

    Car aExpr ->
      valueOfExpr aExpr env store io (CarCont cont)

    Cdr aExpr ->
      valueOfExpr aExpr env store io (CdrCont cont)

    Null aExpr ->
      valueOfExpr aExpr env store io (NullCont cont)

    Empty ->
      applyCont cont $ Right (VList [], (store, io))

    List exprs ->
      case exprs of
        [] ->
          applyCont cont $ Right (VList [], (store, io))

        aExpr : restExprs ->
          valueOfExpr aExpr env store io (ListCont restExprs env [] cont)

    If condition consequent alternative ->
      valueOfExpr condition env store io (IfCont consequent alternative env cont)

    Let x aExpr body ->
      valueOfExpr aExpr env store io (LetCont x body env cont)

    Proc param body ->
      applyCont cont $ Right (VProc $ Procedure param body env, (store, io))

    Letrec declarations letrecBody ->
      valueOfExpr letrecBody (Env.extendRec declarations env) store io cont

    Call rator rand ->
      valueOfExpr rator env store io (RatorCont rand env cont)

    Begin exprs ->
      computeBegin exprs env store io cont

    Assign x aExpr ->
      case find x env store of
        Just (ref, store1) ->
          valueOfExpr aExpr env store1 io (AssignCont ref cont)

        Nothing ->
          Left $ IdentifierNotFound x

    Print aExpr ->
      valueOfExpr aExpr env store io (PrintCont cont)

    Spawn _ ->
      undefined


data Cont
  = EndCont
  | ZeroCont Cont
  | LetCont Id Expr Env Cont
  | Diff1Cont Expr Env Cont
  | Diff2Cont Value Cont
  | Cons1Cont Expr Env Cont
  | Cons2Cont Value Cont
  | CarCont Cont
  | CdrCont Cont
  | NullCont Cont
  | ListCont [Expr] Env [Value] Cont
  | IfCont Expr Expr Env Cont
  | RatorCont Expr Env Cont
  | RandCont Value Cont
  | BeginCont [Expr] Env Cont
  | AssignCont Store.Ref Cont
  | PrintCont Cont


applyCont :: Cont -> Either RuntimeError (Value, (Store, IO ())) -> Either RuntimeError (Value, (Store, IO ()))
applyCont cont input = do
  (value, (store, io)) <- input
  case cont of
    EndCont ->
      trace "End of computation" $
        Right (value, (store, io))

    ZeroCont nextCont ->
      zero value store io nextCont

    LetCont x body env nextCont ->
      let
        (aRef, store1) =
          Store.newref value store
      in
      valueOfExpr body (Env.extend x aRef env) store1 io nextCont

    Diff1Cont bExpr env nextCont ->
      valueOfExpr bExpr env store io (Diff2Cont value nextCont)

    Diff2Cont aValue nextCont ->
      diff aValue value store io nextCont

    Cons1Cont bExpr env nextCont ->
      valueOfExpr bExpr env store io (Cons2Cont value nextCont)

    Cons2Cont aValue nextCont ->
      cons aValue value store io nextCont

    CarCont nextCont ->
      car value store io nextCont

    CdrCont nextCont ->
      cdr value store io nextCont

    NullCont nextCont ->
      isNull value store io nextCont

    ListCont exprs env revValues nextCont ->
      let
        newRevValues =
          value : revValues
      in
      case exprs of
        [] ->
          applyCont nextCont $ Right (VList $ reverse newRevValues, (store, io))

        aExpr : restExprs ->
          valueOfExpr aExpr env store io (ListCont restExprs env newRevValues nextCont)

    IfCont consequent alternative env nextCont ->
      computeIf value consequent alternative env store io nextCont

    RatorCont rand env nextCont ->
      valueOfExpr rand env store io (RandCont value nextCont)

    RandCont ratorValue nextCont ->
      apply ratorValue value store io nextCont

    BeginCont restExprs env nextCont ->
      computeBegin restExprs env store io nextCont

    AssignCont ref nextCont ->
      applyCont nextCont $
        case Store.setref ref value store of
          Just store1 ->
            Right (value, (store1, io))

          Nothing ->
            Left $ LocationNotFound ref

    PrintCont nextCont ->
      applyCont nextCont $ Right (value, (store, io >> print value))


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


diff :: Value -> Value -> Store -> IO () -> Cont -> Either RuntimeError (Value, (Store, IO ()))
diff aValue bValue store io cont = do
  a <- toNumber aValue
  b <- toNumber bValue
  applyCont cont $ Right (VNumber $ a - b, (store, io))


cons :: Value -> Value -> Store -> IO () -> Cont -> Either RuntimeError (Value, (Store, IO ()))
cons a bValue store io cont = do
  l <- toList bValue
  applyCont cont $ Right (VList $ a : l, (store, io))


car :: Value -> Store -> IO () -> Cont -> Either RuntimeError (Value, (Store, IO ()))
car aValue store io cont = do
  l <- toList aValue
  case l of
    [] ->
      Left EmptyListError

    x : _ ->
      applyCont cont $ Right (x, (store, io))


cdr :: Value -> Store -> IO () -> Cont -> Either RuntimeError (Value, (Store, IO ()))
cdr aValue store io cont = do
  l <- toList aValue
  case l of
    [] ->
      Left EmptyListError

    _ : rest ->
      applyCont cont $ Right (VList rest, (store, io))


isNull :: Value -> Store -> IO () -> Cont -> Either RuntimeError (Value, (Store, IO ()))
isNull aValue store io cont = do
  l <- toList aValue
  applyCont cont $ Right (VBool $ l == [], (store, io))


zero :: Value -> Store -> IO () -> Cont -> Either RuntimeError (Value, (Store, IO ()))
zero aValue store io cont = do
  a <- toNumber aValue
  applyCont cont $ Right (VBool $ a == 0, (store, io))


computeIf :: Value -> Expr -> Expr -> Env -> Store -> IO () -> Cont -> Either RuntimeError (Value, (Store, IO ()))
computeIf conditionValue consequent alternative env store io cont = do
  a <- toBool conditionValue
  let expr = if a then consequent else alternative
  valueOfExpr expr env store io cont


apply :: Value -> Value -> Store -> IO () -> Cont -> Either RuntimeError (Value, (Store, IO ()))
apply ratorValue arg store io cont = do
  Procedure param body savedEnv <- toProcedure ratorValue
  let (argRef, store1) = Store.newref arg store
  valueOfExpr body (Env.extend param argRef savedEnv) store1 io cont


computeBegin :: [Expr] -> Env -> Store -> IO () -> Cont -> Either RuntimeError (Value, (Store, IO ()))
computeBegin exprs env store io cont =
  case exprs of
    [expr] ->
      valueOfExpr expr env store io cont

    expr : restExprs -> do
      valueOfExpr expr env store io (BeginCont restExprs env cont)

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
