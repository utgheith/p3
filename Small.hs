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
  | StepContinue Term -- need to keep going
  | BreakLoop -- break out of loop
  | ContinueLoop -- continue to next iteration
  | Sad String -- error
  deriving (Eq, Show)

----- The Env monad -----

-- abstract semantics that glue micro-ops together

type Env m = S.State m (Result (V m))

premise :: Env m -> (Term -> Term) -> (V m -> Env m) -> Env m
premise e l r = do
  v <- e
  case v of
    StepContinue t' -> return $ StepContinue (l t')
    BreakLoop -> return BreakLoop
    ContinueLoop -> return ContinueLoop
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
  result <- reduce t1
  case result of
    BreakLoop -> return BreakLoop
    ContinueLoop -> return ContinueLoop
    Happy _ -> reduce t2
    StepContinue t' -> return $ StepContinue (Seq t' t2)
    Sad msg -> return $ Sad msg
reduce_ (If cond tThen tElse) = do
  condResult <- reduce cond
  case condResult of
    Sad msg -> return $ Sad msg
    BreakLoop -> return BreakLoop
    ContinueLoop -> return ContinueLoop
    Happy v -> selectValue v (reduce tThen) (reduce tElse)
    StepContinue t' -> return $ StepContinue (If t' tThen tElse)
reduce_ Break =
  return BreakLoop
reduce_ Continue =
  return ContinueLoop
reduce_ w@(While cond body) = do
  condResult <- reduce_ cond
  case condResult of
    StepContinue t' ->
      return $ StepContinue (While t' body)
    Happy (BoolVal True) -> do
      bodyResult <- reduce_ body
      case bodyResult of
        BreakLoop -> return $ Happy (IntVal 0) -- exit loop
        ContinueLoop -> return $ StepContinue w -- skip to next iteration
        Happy _ -> return $ StepContinue w -- normal next iteration
        StepContinue t' -> return $ StepContinue (While cond t') -- propagate small-step
        Sad msg -> return $ Sad msg
    Happy (BoolVal False) ->
      return $ Happy (IntVal 0) -- done looping
    Happy (IntVal n) ->
      if n /= 0
        then do
          bodyResult <- reduce_ body
          case bodyResult of
            BreakLoop -> return $ Happy (IntVal 0)
            ContinueLoop -> return $ StepContinue w
            Happy _ -> return $ StepContinue w
            StepContinue t' -> return $ StepContinue (Seq t' w)
            Sad msg -> return $ Sad msg
        else return $ Happy (IntVal 0) -- done looping
    Sad msg -> return $ Sad msg
    _ -> return $ Sad "Condition in while must evaluate to a boolean"
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
    (StepContinue t, m) -> reduceFully t m
    (BreakLoop, m) -> (Right (IntVal 0), m)
    (ContinueLoop, m) -> (Right (IntVal 0), m)
    (Happy n, m) -> (Right n, m)
