{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module Small (reduceFully, Machine(..), Result(..),Env) where

import qualified Control.Monad.State as S
import Term (Term(..))
import Debug.Trace (trace)

----- The Machine type class -----

-- The micro-ops that a machine must support
-- Allow an implementation to define its own semantics

class Machine m where
    type V m -- The value type for this machine
    -- Uses associated an associated type family for the value type
    -- This requires the TypeFamilies extension
    -- The way you read the type signature is:
    --    for any type m that is an instance of Machine, there is an associated type (V m)

    -- Get and set variables
    getVar :: String -> Env m
    setVar :: String -> V m -> Env m

    -- I/O
    inputVal :: Env m
    outputVal :: V m -> Env m

    -- Arithmetic and control
    subVal :: V m -> V m -> Env m
    selectValue :: V m -> Env m -> Env m -> Env m

    -- Type Conversion
    intToV :: m -> Integer -> V m
    vToInt :: m -> V m -> Integer

----- The Result type -----

data Result a = Happy a -- produced an answer
    | Continue Term -- need to keep going
    | Sad String -- error
    deriving (Eq, Show)

----- The Env monad -----

-- abstract semantics that glue micro-ops together

type Env m = (Machine m) => S.State m (Result (V m))

premise :: (Machine m) =>Env m -> (Term -> Term) -> (V m -> Env m) -> Env m
premise e l r = do
    v <- e
    case v of
        Continue t' -> return $ Continue (l t')
        Happy n -> r n
        Sad _ -> return v


------ Small-step reduction ------

reduce_ :: (Machine m, Show m) => Term -> Env m

reduce_ (Literal n) = do
    m <- S.get
    return $ Happy $ intToV m n

reduce_ (Var x) =
    getVar x

reduce_ (Let x t) = do
    premise (reduce t)
        (Let x)
        (setVar x)

reduce_ (Seq t1 t2) = do
    premise (reduce t1)
        (`Seq` t2)
        (\_  -> return $ Continue t2)

reduce_ (If cond tThen tElse) = do
    premise (reduce cond)
        (\cond' ->  If cond' tThen tElse)
        (\v -> selectValue v (return $ Continue tThen) (return $ Continue tElse))

reduce_ w@(While cond body) =
    return $ Continue (If cond (Seq body w) Skip)

reduce_ (Read x) = do
    m <- S.get
    premise inputVal
        id
        (\v -> return $ Continue (Let x (Literal $ vToInt m v)))


reduce_ (Write t) = do
    premise (reduce t)
        Write
        outputVal

reduce_ Skip = do
    m <- S.get
    return $ Happy (intToV m 0)

reduce_ (Sub t1 t2) = do
    m <- S.get
    premise (reduce t1)
        (`Sub` t2)
        (\v1 -> premise (reduce t2)
                    (Sub (Literal $ vToInt m v1))
                    (subVal v1))


reduce :: (Machine m, Show m) => Term -> Env m
reduce t = do
    e <- S.get
    trace ("Simulating: " ++ show t) () `seq`
        trace ("     Machine: " ++ show e) () `seq`
        reduce_  t


reduceFully :: (Machine m, Show m) => Term -> m -> (Either String (V m), m)
reduceFully term machine =
        case S.runState (reduce term) machine of
            (Sad msg, m) -> (Left msg, m)
            (Continue t, m) -> reduceFully t m
            (Happy n, m) -> (Right n, m)
