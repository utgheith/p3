{-# LANGUAGE TypeFamilies #-}

module Machine (Machine (..), Env, Error, Result (..)) where

import qualified Control.Monad.State as S
import Term (ErrorKind (..), Term (..))
import TypeSignature (TypedName)
import Value (Value (..))

----- The Machine type class -----

-- The micro-ops that a machine must support
-- Allow an implementation to define its own semantics

type Error = (ErrorKind, String)

data Result a
  = Happy a -- produced an answer
  | Continue Term -- need to keep going
  | Sad Error -- error
  deriving (Eq, Show)

type Env m = S.State m (Result (V m))

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
  powVal :: V m -> V m -> Env m
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
  xorVal :: V m -> V m -> Env m
  notVal :: V m -> Env m
  bitNotVal :: V m -> Env m

  -- Increment/Decrement operations (modify variables)
  preIncrementVal :: TypedName -> Env m -- ++x: increment then return new value
  preDecrementVal :: TypedName -> Env m -- --x: decrement then return new value
  postIncrementVal :: TypedName -> Env m -- x++: return old value then increment
  postDecrementVal :: TypedName -> Env m -- x--: return old value then decrement

  -- Access/Manage Bracket Values
  getBracketValue :: V m -> V m -> Env m
  setBracketValue :: V m -> V m -> V m -> Env m

  -- Control flow - selectValue uses boolean semantics
  selectValue :: V m -> Env m -> Env m -> Env m
