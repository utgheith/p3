module Value (Value (..)) where

import qualified Data.Map as M
import qualified Data.Set as S
import {-# SOURCE #-} Term (Term)

data Value
  = IntVal Integer
  | BoolVal Bool
  | StringVal String
  | Tuple [Value]
  | ClosureVal [String] Term [(String, Value)]
  | Dictionary (M.Map Integer Value)
  | Set (S.Set Integer)

instance Eq Value

instance Show Value
