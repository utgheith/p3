module Value (Value(..)) where

import qualified Data.Map as M
import {-# SOURCE #-} Term (Term)

data Value
  = IntVal Integer
  | BoolVal Bool
  | StringVal String
  | Tuple [Value]
  | ClosureVal [String] Term [(String, Value)]
  | Dictionary (M.Map Integer Value)

instance Eq Value
instance Show Value
