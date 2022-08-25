module Interpreter
  ( Value(..)
  , Type(..)
  , Error(..), ParseError, RuntimeError(..)
  , run
  , runIO
  ) where


import qualified Env
import qualified Store

import Data.Bifunctor (first)
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
      first RuntimeError $ snd $ valueOfProgram program


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
      let
        ((_, io), eitherValue) =
          valueOfProgram program
      in
      case eitherValue of
        Left err ->
          error $ show $ RuntimeError err

        Right value ->
          io >> return value


valueOfProgram :: Program -> ((Store, IO ()), Either RuntimeError Value)
valueOfProgram (Program expr) =
  runEval initStore io $
    valueOfExpr expr initEnv EndCont
  where
    initEnv =
      Env.extend "i" iRef
        (Env.extend "v" vRef
          (Env.extend "x" xRef
            Env.empty))

    store0 = Store.empty
    (iRef, store1) = Store.newref (VNumber 1) store0
    (vRef, store2) = Store.newref (VNumber 5) store1
    (xRef, initStore) = Store.newref (VNumber 10) store2

    io = return ()


data Eval a =
  Eval (Store -> IO () -> ((Store, IO ()), Either RuntimeError a))


runEval :: Store -> IO () -> Eval a -> ((Store, IO ()), Either RuntimeError a)
runEval store io (Eval t) = t store io


instance Functor Eval where
  fmap f (Eval t) =
    Eval $ \store io ->
      (fmap . fmap) f $ t store io


instance Applicative Eval where
  pure a =
    Eval $ \store io ->
      ((store, io), Right a)

  Eval tF <*> Eval t =
    Eval $ \store0 io0 ->
      let
        ((store1, io1), eitherF) =
          tF store0 io0

        ((store2, io2), eitherA) =
          t store1 io1
      in
      ((store2, io2), eitherF <*> eitherA)


instance Monad Eval where
  Eval t >>= f =
    Eval $ \store0 io0 ->
      let
        ((store1, io1), eitherA) =
          t store0 io0
      in
      case eitherA of
        Right a ->
          let
            Eval u =
              f a
          in
          u store1 io1

        Left err ->
          ((store1, io1), Left err)


throwError :: RuntimeError -> Eval a
throwError err =
  Eval $ \store io ->
    ((store, io), Left err)


getStore :: Eval Store
getStore =
  Eval $ \store io ->
    ((store, io), Right store)


setStore :: Store -> Eval ()
setStore store =
  Eval $ \_ io ->
    ((store, io), Right ())


getIO :: Eval (IO ())
getIO =
  Eval $ \store io ->
    ((store, io), Right io)


setIO :: IO () -> Eval ()
setIO io =
  Eval $ \store _ ->
    ((store, io), Right ())


newref :: Value -> Eval Store.Ref
newref value = do
  store0 <- getStore
  let (ref, store1) = Store.newref value store0
  setStore store1
  return ref


deref :: Store.Ref -> Eval (Maybe Value)
deref ref = do
  store <- getStore
  return $ Store.deref ref store


setref :: Store.Ref -> Value -> Eval ()
setref ref value = do
  store0 <- getStore
  case Store.setref ref value store0 of
    Just store1 -> do
      setStore store1

    Nothing ->
      throwError $ LocationNotFound ref


println :: Show a => a -> Eval ()
println a = do
  io <- getIO
  setIO $ io >> print a


valueOfExpr :: Expr -> Env -> Cont -> Eval Value
valueOfExpr expr env cont =
  case expr of
    Const n ->
      applyCont cont $ VNumber n

    Var x -> do
      maybeRef <- find x env
      case maybeRef of
        Just ref -> do
          maybeValue <- deref ref
          case maybeValue of
            Just value ->
              applyCont cont value

            Nothing ->
              throwError $ LocationNotFound ref

        Nothing ->
          throwError $ IdentifierNotFound x

    Diff aExpr bExpr ->
      valueOfExpr aExpr env (Diff1Cont bExpr env cont)

    Zero aExpr ->
      valueOfExpr aExpr env (ZeroCont cont)

    Cons aExpr bExpr ->
      valueOfExpr aExpr env (Cons1Cont bExpr env cont)

    Car aExpr ->
      valueOfExpr aExpr env (CarCont cont)

    Cdr aExpr ->
      valueOfExpr aExpr env (CdrCont cont)

    Null aExpr ->
      valueOfExpr aExpr env (NullCont cont)

    Empty ->
      applyCont cont $ VList []

    List exprs ->
      case exprs of
        [] ->
          applyCont cont $ VList []

        aExpr : restExprs ->
          valueOfExpr aExpr env (ListCont restExprs env [] cont)

    If condition consequent alternative ->
      valueOfExpr condition env (IfCont consequent alternative env cont)

    Let x aExpr body ->
      valueOfExpr aExpr env (LetCont x body env cont)

    Proc param body ->
      applyCont cont $ VProc $ Procedure param body env

    Letrec declarations letrecBody ->
      valueOfExpr letrecBody (Env.extendRec declarations env) cont

    Call rator rand ->
      valueOfExpr rator env (RatorCont rand env cont)

    Begin exprs ->
      computeBegin exprs env cont

    Assign x aExpr -> do
      maybeRef <- find x env
      case maybeRef of
        Just ref ->
          valueOfExpr aExpr env (AssignCont ref cont)

        Nothing ->
          throwError $ IdentifierNotFound x

    Print aExpr ->
      valueOfExpr aExpr env (PrintCont cont)

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


applyCont :: Cont -> Value -> Eval Value
applyCont cont value =
  case cont of
    EndCont ->
      trace "End of computation" $
        return value

    ZeroCont nextCont ->
      zero value nextCont

    LetCont x body env nextCont -> do
      aRef <- newref value
      valueOfExpr body (Env.extend x aRef env) nextCont

    Diff1Cont bExpr env nextCont ->
      valueOfExpr bExpr env (Diff2Cont value nextCont)

    Diff2Cont aValue nextCont ->
      diff aValue value nextCont

    Cons1Cont bExpr env nextCont ->
      valueOfExpr bExpr env (Cons2Cont value nextCont)

    Cons2Cont aValue nextCont ->
      cons aValue value nextCont

    CarCont nextCont ->
      car value nextCont

    CdrCont nextCont ->
      cdr value nextCont

    NullCont nextCont ->
      isNull value nextCont

    ListCont exprs env revValues nextCont ->
      let
        newRevValues =
          value : revValues
      in
      case exprs of
        [] ->
          applyCont nextCont $ VList $ reverse newRevValues

        aExpr : restExprs ->
          valueOfExpr aExpr env (ListCont restExprs env newRevValues nextCont)

    IfCont consequent alternative env nextCont ->
      computeIf value consequent alternative env nextCont

    RatorCont rand env nextCont ->
      valueOfExpr rand env (RandCont value nextCont)

    RandCont ratorValue nextCont ->
      apply ratorValue value nextCont

    BeginCont restExprs env nextCont ->
      computeBegin restExprs env nextCont

    AssignCont ref nextCont -> do
      setref ref value
      applyCont nextCont value

    PrintCont nextCont -> do
      println value
      applyCont nextCont value


find :: Id -> Env -> Eval (Maybe Store.Ref)
find x env =
  case Env.find x env of
    Just (Env.Value ref) ->
      return $ Just ref

    Just (Env.Procedure param body savedEnv) ->
      let
        value =
          VProc $ Procedure param body savedEnv
      in do
      Just <$> newref value

    Nothing ->
      return Nothing


diff :: Value -> Value -> Cont -> Eval Value
diff aValue bValue cont = do
  a <- toNumber aValue
  b <- toNumber bValue
  applyCont cont $ VNumber $ a - b


cons :: Value -> Value -> Cont -> Eval Value
cons a bValue cont = do
  l <- toList bValue
  applyCont cont $ VList $ a : l


car :: Value -> Cont -> Eval Value
car aValue cont = do
  l <- toList aValue
  case l of
    [] ->
      throwError EmptyListError

    x : _ ->
      applyCont cont x


cdr :: Value -> Cont -> Eval Value
cdr aValue cont = do
  l <- toList aValue
  case l of
    [] ->
      throwError EmptyListError

    _ : rest ->
      applyCont cont $ VList rest


isNull :: Value -> Cont -> Eval Value
isNull aValue cont = do
  l <- toList aValue
  applyCont cont $ VBool $ l == []


zero :: Value -> Cont -> Eval Value
zero aValue cont = do
  a <- toNumber aValue
  applyCont cont $ VBool $ a == 0


computeIf :: Value -> Expr -> Expr -> Env -> Cont -> Eval Value
computeIf conditionValue consequent alternative env cont = do
  a <- toBool conditionValue
  let expr = if a then consequent else alternative
  valueOfExpr expr env cont


apply :: Value -> Value -> Cont -> Eval Value
apply ratorValue arg cont = do
  Procedure param body savedEnv <- toProcedure ratorValue
  argRef <- newref arg
  valueOfExpr body (Env.extend param argRef savedEnv) cont


computeBegin :: [Expr] -> Env -> Cont -> Eval Value
computeBegin exprs env cont =
  case exprs of
    [expr] ->
      valueOfExpr expr env cont

    expr : restExprs -> do
      valueOfExpr expr env (BeginCont restExprs env cont)

    [] ->
      -- N.B. Based on the grammar this condition will never be reached.
      -- Maybe we can make that clearer in the AST by using a
      -- non-empty list of expressions.
      --
      -- See Data.List.NonEmpty in base.
      undefined


toNumber :: Value -> Eval Number
toNumber (VNumber n) = return n
toNumber value = throwError $ TypeError TNumber (typeOf value)


toBool :: Value -> Eval Bool
toBool (VBool b) = return b
toBool value = throwError $ TypeError TBool (typeOf value)


toList :: Value -> Eval [Value]
toList (VList l) = return l
toList value = throwError $ TypeError TList (typeOf value)


toProcedure :: Value -> Eval Procedure
toProcedure (VProc p) = return p
toProcedure value = throwError $ TypeError TProc (typeOf value)


typeOf :: Value -> Type
typeOf (VNumber _) = TNumber
typeOf (VBool _) = TBool
typeOf (VList _) = TList
typeOf (VProc _) = TProc
