{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Small (reduceFully, Machine (..), Result (..), Env) where

import qualified Control.Monad.State as S
import Data.Either
import qualified Data.Map as M
import Debug.Trace (trace)
import Term (BinaryOp (..), Term (..), UnaryOp (..))
import Value (Value (..), valueToInt, valueToTuple)

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

  -- Access/Manage Bracket Values
  getBracketValue :: V m -> V m -> Env m
  setBracketValue :: String -> V m -> V m -> Env m

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
reduce_ (BreakSignal) =
  return $ Continue BreakSignal
reduce_ (ContinueSignal) =
  return $ Continue ContinueSignal
reduce_ (Fun xs t) = do
  env <- S.get
  let vars = getScope env
  return $ Happy (ClosureVal xs t vars)
reduce_ (ApplyFun tf tas) =
  premise
    (reduce tf)
    (`ApplyFun` tas)
    (reduceArgsAndApply tf tas)
reduce_ (TupleTerm elements) =
  case elements of
    (x : xs) ->
      premise
        (reduce x)
        (\term' -> TupleTerm $ term' : xs)
        ( \v1 ->
            premise
              (reduce $ TupleTerm xs)
              ( \term' ->
                  case term' of
                    TupleTerm xs' -> TupleTerm (x : xs')
                    _ -> error "TupleTerm recursion somehow returned a non TupleTerm continuation"
              )
              (\v2 -> return $ Happy $ Tuple $ v1 : (fromRight [] $ valueToTuple v2))
        )
    [] -> return $ Happy $ Tuple []
reduce_ (AccessBracket t i) =
  premise
    (reduce t)
    (\term' -> AccessBracket term' i)
    ( \v1 ->
        premise
          (reduce i)
          (AccessBracket t)
          (getBracketValue v1)
    )
reduce_ (SetBracket name terms val) =
  case terms of
    TupleTerm tupleTerm ->
      premise
        (reduce $ TupleTerm tupleTerm)
        (\terms' -> SetBracket name terms' val)
        ( \terms' ->
            premise
              (reduce val)
              (\val' -> SetBracket name terms val')
              (\val' -> setBracketValue name terms' val')
        )
    _ -> error "SetBracket should only have tuple term as second argument"
reduce_ (NewDictionary) =
  return $ Happy $ Dictionary M.empty

reduceArgsAndApply :: (Machine m, Show m, V m ~ Value) => Term -> [Term] -> Value -> Env m
reduceArgsAndApply tf args funVal =
  case args of
    [] -> applyFuncNoArg funVal
    (a : rest) ->
      premise
        (reduce a)
        (\a' -> ApplyFun tf (a' : rest))
        (applyFunArgList tf rest funVal)

applyFunArgList :: (Machine m, Show m, V m ~ Value) => Term -> [Term] -> Value -> Value -> Env m
applyFunArgList tf rest funVal argVal = do
  res1 <- applyFunArg funVal argVal
  case res1 of
    Happy v1 -> case rest of
      [] -> return (Happy v1)
      _ -> reduceArgsAndApply tf rest v1
    Continue t -> return (Continue t)
    Sad msg -> return (Sad msg)

applyFunArg :: (Machine m, Show m, V m ~ Value) => Value -> Value -> Env m
applyFunArg (ClosureVal [] _ _) _ = do
  return $ Sad "too many arguments: function takes 0 arguments"
applyFunArg (ClosureVal (x : xs) body caps) arg = do
  let newCaps = caps ++ [(x, arg)]
  if null xs
    then evalClosureBody body newCaps
    else return $ Happy (ClosureVal xs body newCaps)
applyFunArg _ _ = return $ Sad "attempt to call a non-function"

applyFuncNoArg :: (Machine m, Show m, V m ~ Value) => Value -> Env m
applyFuncNoArg (ClosureVal [] body caps) = evalClosureBody body caps
applyFuncNoArg (ClosureVal (_ : _) _ _) = return $ Sad "missing arguments: function requires parameters"
applyFuncNoArg _ = return $ Sad "attempt to call a non-function"

-- Bind captured args, evaluate body, restore machine state
evalClosureBody :: (Machine m, Show m, V m ~ Value) => Term -> [(String, Value)] -> Env m
evalClosureBody body caps = do
  m0 <- S.get
  let (_resPush, m1) = S.runState (pushScope []) m0
  case bindMany caps m1 of
    Left msg -> return (Sad msg)
    Right m2 -> do
      let (res, m3) = reduceFully body m2
      let (_resPop, m4) = S.runState popScope m3 -- Restore previous scope.
      S.put m4
      case res of
        Left msg -> return $ Sad msg
        Right v -> return $ Happy v

bindMany :: (Machine m, V m ~ Value) => [(String, Value)] -> m -> Either String m
bindMany [] m = Right m
bindMany ((k, v) : rest) m =
  case S.runState (setVar k v) m of
    (Sad msg, _m') -> Left msg
    (Continue _, _m') -> Left "internal: setVar requested Continue"
    (Happy _, m') -> bindMany rest m'

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
    (BreakLoop, m) -> (Left "unhandled break signal", m)
    (ContinueLoop, m) -> (Left "unhandled continue signal", m)
    (Happy n, m) -> (Right n, m)
