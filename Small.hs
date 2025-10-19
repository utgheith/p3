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
reduce_ Nil =
  return $ Happy $ ListVal []
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
    applyBinaryOp Cons = \v1 v2 ->
      case v2 of
        ListVal xs -> return $ Happy $ ListVal (v1 : xs)
        _ -> return $ Sad $ "Type error: Cons operator expects a list as its second argument"
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
    applyUnaryOp Head = \v -> case v of
      ListVal (x : _) -> return $ Happy x
      ListVal [] -> return $ Sad $ "Head called on empty list"
      _ -> return $ Sad $ "Type error: Head called on non-list"
    applyUnaryOp Tail = \v -> case v of
      ListVal (_ : xs) -> return $ Happy $ ListVal xs
      ListVal [] -> return $ Sad $ "Tail called on empty list"
      _ -> return $ Sad $ "Type error: Tail called on non-list"
    applyUnaryOp Length = \v -> case v of
      ListVal xs -> return $ Happy $ IntVal (fromIntegral (length xs))
      StringVal s -> return $ Happy $ IntVal (fromIntegral (length s))
      BoolVal _ -> return $ Happy $ IntVal 1
      IntVal n ->
        let m = abs n
         in let digits = if m == 0 then 1 else length (show m)
             in return $ Happy $ IntVal (fromIntegral digits)
    applyUnaryOp IsNil = \v -> case v of
      ListVal [] -> return (Happy (BoolVal True))
      ListVal _ -> return (Happy (BoolVal False))
      _ -> return (Sad "Type error: IsNil called on non-list")

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
