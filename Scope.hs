module Scope
  ( Scope (..),
    lookupScope,
    insertScope,
    getAllBindings,
    emptyScope,
    scopeFromList,
  )
where

import qualified Data.Map as M
import Value (Value)

data Scope = Scope (M.Map String Value) (Maybe Scope)
  deriving (Eq, Show)

lookupScope :: String -> Scope -> Maybe Value
lookupScope name (Scope m _) =
  M.lookup name m -- Do not look in parent scopes.

insertScope :: String -> Value -> Scope -> Scope
insertScope name val (Scope m parent) = Scope (M.insert name val m) parent -- Insert into inner scope.

getAllBindings :: Scope -> [(String, Value)]
getAllBindings scope = addAll scope []
  where
    addAll (Scope m Nothing) vars = M.toList m ++ vars
    addAll (Scope m (Just parent)) vars = addAll parent (M.toList m ++ vars)

emptyScope :: Scope
emptyScope = Scope M.empty Nothing

scopeFromList :: [(String, Value)] -> Scope
scopeFromList vars = Scope (M.fromList vars) Nothing
