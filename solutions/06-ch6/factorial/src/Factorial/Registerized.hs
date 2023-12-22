module Factorial.Registerized (fact) where


data Globals
    = Globals
        { gN :: Integer
        , gK :: Continuation
        , gVal :: Integer
        }


fact :: Integer -> Integer
fact n =
    let
        initial =
            Globals { gN = n, gK = End, gVal = 1 }
    in
    gVal (factHelper initial)


factHelper :: Globals -> Globals
factHelper globals@(Globals { gN = n, gK = k }) =
    if n <= 1 then
        applyContinuation (globals { gVal = 1 })

    else
        factHelper (globals { gN = n - 1, gK = Fact n k })


data Continuation
    = End
    | Fact Integer Continuation
    deriving (Show)


applyContinuation :: Globals -> Globals
applyContinuation globals@(Globals { gK = k, gVal = val }) =
    case k of
        End ->
            globals

        Fact n nextK ->
            applyContinuation (globals { gK = nextK, gVal = val * n })
