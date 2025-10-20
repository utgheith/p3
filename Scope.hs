module Scope
  ( Scope (..),
    lookupScope,
    insertScope,
    getAllBindings,
    emptyScope,
    scopeFromList,
    markFinal,
    isFinal,
    moveToGlobal,
  )
where

import qualified Data.Map as M
import Value (Value)

-- Each variable is stored as (Value, isFinal)
type VarInfo = (Value, Bool)
data Scope = Scope (M.Map String VarInfo) (Maybe Scope)
  deriving (Eq, Show)

lookupScope :: String -> Scope -> Maybe Value
lookupScope name (Scope m parent) =
  case M.lookup name m of
    Just (v, _) -> Just v
    Nothing ->
      case parent of
        Just p -> lookupScope name p -- Look in parent scope.
        Nothing -> Nothing

insertScope :: String -> Value -> Scope -> Scope
insertScope name val (Scope m parent) =
  let isVarFinal = case M.lookup name m of
                  Just (_, f) -> f
                  Nothing -> False
  in Scope (M.insert name (val, isVarFinal) m) parent -- Insert into inner scope.

getAllBindings :: Scope -> [(String, Value)]
getAllBindings scope = addAll scope []
  where
    addAll (Scope m Nothing) vars = map (\(k, (v, _)) -> (k, v)) (M.toList m) ++ vars
    addAll (Scope m (Just parent)) vars = addAll parent (map (\(k, (v, _)) -> (k, v)) (M.toList m) ++ vars)

emptyScope :: Scope
emptyScope = Scope M.empty Nothing

scopeFromList :: [(String, Value)] -> Scope
scopeFromList vars = Scope (M.fromList (map (\(k, v) -> (k, (v, False))) vars)) Nothing

-- Mark a variable as final in the current scope
markFinal :: String -> Scope -> Scope
markFinal name (Scope m parent) =
  Scope (M.alter (\kf -> case kf of
                    Just (v, _) -> Just (v, True)
                    Nothing -> Nothing) name m) parent

-- Check if a variable is final in the current or parent scopes
isFinal :: String -> Scope -> Bool
isFinal name (Scope m parent) =
  case M.lookup name m of
    Just (_, True) -> True
    _ -> maybe False (isFinal name) parent

-- Move a variable to the global scope
moveToGlobal :: String -> Scope -> Scope
moveToGlobal name scope =
  case lookupScope name scope of
    Nothing -> scope
    Just v -> recMove scope v
  where
    recMove (Scope m Nothing) v = Scope (M.insert name (v, isFinal name scope) m) Nothing
    recMove (Scope m (Just parent)) v = Scope m (Just (recMove parent v))
