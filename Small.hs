{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Small (reduceFully, Machine (..), Result (..), Env) where

import qualified Control.Monad.State as S
import Data.Either
import Debug.Trace (trace)
import Term (BinaryOp (..), Term (..), UnaryOp (..))
import Value (Value (..), valueToInt)

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

  -- Lexical scoping
  getScope :: m -> [(String, Value)] -- Variable bindings only.
  pushScope :: [(String, Value)] -> Env m
  popScope :: Env m

  -- I/O
  inputVal :: Env m
  outputVal :: V m -> Env m

  -- Arithmetic and control
  addVal :: V m -> V m -> Env m
  subVal :: V m -> V m -> Env m
  mulVal :: V m -> V m -> Env m
  divVal :: V m -> V m -> Env m
  modVal :: V m -> V m -> Env m
  negVal :: V m -> Env m

  -- Comparison operations (operate on integers, return booleans)
  ltVal :: V m -> V m -> Env m
  gtVal :: V m -> V m -> Env m
  lteVal :: V m -> V m -> Env m
  gteVal :: V m -> V m -> Env m
  eqVal :: V m -> V m -> Env m
  neqVal :: V m -> V m -> Env m

  -- Logical operations (operate on booleans)
  andVal :: V m -> V m -> Env m
  orVal :: V m -> V m -> Env m
  notVal :: V m -> Env m

  -- Control flow - selectValue uses boolean semantics
  selectValue :: V m -> Env m -> Env m -> Env m

----- The Result type -----

data Result a
  = Happy a -- produced an answer
  | Continue Term -- need to keep going
  | Sad String -- error
  deriving (Eq, Show)

----- The Env monad -----

-- abstract semantics that glue micro-ops together

type Env m = S.State m (Result (V m))

premise :: Env m -> (Term -> Term) -> (V m -> Env m) -> Env m
premise e l r = do
  v <- e
  case v of
    Continue t' -> return $ Continue (l t')
    Happy n -> r n
    Sad _ -> return v

------ Small-step reduction ------

reduce_ :: (Machine m, Show m, V m ~ Value) => Term -> Env m
reduce_ (Literal n) =
  return $ Happy $ IntVal n
reduce_ (StringLiteral s) =
  return $ Happy $ StringVal s
reduce_ (Var x) =
  getVar x
reduce_ (Let x t) = do
  premise
    (reduce t)
    (Let x)
    (setVar x)
reduce_ (Seq t1 t2) = do
  premise
    (reduce t1)
    (`Seq` t2)
    (\_ -> return $ Continue t2)
reduce_ (If cond tThen tElse) = do
  premise
    (reduce cond)
    (\cond' -> If cond' tThen tElse)
    (\v -> selectValue v (return $ Continue tThen) (return $ Continue tElse))
reduce_ w@(While cond body) =
  return $ Continue (If cond (Seq body w) Skip)
reduce_ (Read x) =
  premise
    inputVal
    id
    (setVar x)
reduce_ (Write t) = do
  premise
    (reduce t)
    Write
    outputVal
reduce_ Skip =
  return $ Happy (IntVal 0)
reduce_ (BinaryOps op t1 t2) =
  premise
    (reduce t1)
    (\t1' -> BinaryOps op t1' t2)
    ( \v1 ->
        premise
          (reduce t2)
          (BinaryOps op (Literal $ fromRight (-1) (valueToInt v1)))
          (applyBinaryOp op v1)
    )
  where
    applyBinaryOp Add = addVal
    applyBinaryOp Sub = subVal
    applyBinaryOp Mul = mulVal
    applyBinaryOp Div = divVal
    applyBinaryOp Mod = modVal
    applyBinaryOp Lt = ltVal
    applyBinaryOp Gt = gtVal
    applyBinaryOp Lte = lteVal
    applyBinaryOp Gte = gteVal
    applyBinaryOp Eq = eqVal
    applyBinaryOp Neq = neqVal
    applyBinaryOp And = andVal
    applyBinaryOp Or = orVal
reduce_ (BoolLit b) =
  return $ Happy $ BoolVal b
reduce_ (UnaryOps op t) =
  premise
    (reduce t)
    (UnaryOps op)
    (applyUnaryOp op)
  where
    applyUnaryOp Neg = negVal
    applyUnaryOp Not = notVal
reduce_ (Fun x t) = do
  env <- S.get
  let vars = getScope env
  return $ Happy (ClosureVal x t vars)
reduce_ (App tf ta) =
  premise
    (reduce tf)
    (`App` ta)
    (premise (reduce ta) (App tf) . apply)

apply :: (Machine m, Show m, V m ~ Value) => Value -> Value -> Env m
apply (ClosureVal x body _caps) arg = do
  m0 <- S.get
  let (_resPush, m1) = S.runState (pushScope _caps) m0
  let (_resSet, m2) = S.runState (setVar x arg) m1
  let (res, m3) = reduceFully body m2
  let (_resPop, m4) = S.runState popScope m3 -- Restore previous scope.
  S.put m4
  case res of
    Left msg -> return $ Sad msg
    Right v -> return $ Happy v
apply _ _ = return $ Sad "attempt to call a non-function"

reduce :: (Machine m, Show m, V m ~ Value) => Term -> Env m
reduce t = do
  e <- S.get
  trace ("Simulating: " ++ show t) () `seq`
    trace ("     Machine: " ++ show e) () `seq`
      reduce_ t

reduceFully :: (Machine m, Show m, V m ~ Value) => Term -> m -> (Either String (V m), m)
reduceFully term machine =
  case S.runState (reduce term) machine of
    (Sad msg, m) -> (Left msg, m)
    (Continue t, m) -> reduceFully t m
    (Happy n, m) -> (Right n, m)
