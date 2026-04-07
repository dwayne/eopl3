module Factorial.Trampolined (fact) where


data Globals
    = Globals
        { gN :: Integer
        , gK :: Continuation
        , gVal :: Integer
        , gPc :: Maybe (Globals -> Globals)
        }
--
-- gN, gVal: Integer registers
--       gK: A stack
--      gPC: A function pointer
--


fact :: Integer -> Integer
fact n =
    let
        initial =
            Globals
                { gN = n
                , gK = End
                , gVal = 1
                , gPc = Just factHelper
                }
    in
    gVal (trampoline initial)


trampoline :: Globals -> Globals
trampoline globals@(Globals { gPc = pc }) =
    case pc of
        Just f ->
            --
            -- N.B. All function calls occur here.
            --
            trampoline (f globals)

        Nothing ->
            globals


factHelper :: Globals -> Globals
factHelper globals@(Globals { gN = n, gK = k }) =
    if n <= 1 then
        globals
            { gVal = 1
            , gPc = Just applyContinuation
            }

    else
        globals
            { gN = n - 1
            , gK = Fact n k
            , gPc = Just factHelper
            }


data Continuation
    = End
    | Fact Integer Continuation
    deriving (Show)


applyContinuation :: Globals -> Globals
applyContinuation globals@(Globals { gK = k, gVal = val }) =
    case k of
        End ->
            globals
                { gPc = Nothing
                }

        Fact n nextK ->
            globals
                { gK = nextK
                , gVal = val * n
                , gPc = Just applyContinuation
                }
