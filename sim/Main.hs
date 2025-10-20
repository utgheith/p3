{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import qualified Control.Monad.State as S
import qualified Data.Map as M
import qualified Progs
import Scope (Scope (..), emptyScope, getAllBindings, insertScope, lookupScope)
import Small (Env, Machine (..), Result (..), reduceFully)
import Term (ErrorKind (..), Term (..))
import Value (Value (..))

data Simulator = Simulator Scope [Value] [Value] deriving (Eq, Show)

instance Machine Simulator where
  type V Simulator = Value
  getVar :: String -> Env Simulator
  getVar name = do
    (Simulator m _ _) <- S.get
    case lookupScope name m of
      Just v -> return $ Happy v
      Nothing -> return $ Sad $ (VariableNotFound, "get: " ++ name ++ " not found")

  setVar :: String -> Value -> Env Simulator
  setVar name val = do
    (Simulator m inp out) <- S.get
    let m' = insertScope name val m
    S.put (Simulator m' inp out)
    return $ Happy val

  getScope :: Simulator -> [(String, Value)]
  getScope (Simulator m _ _) = getAllBindings m

  pushScope :: [(String, Value)] -> Env Simulator
  pushScope vars = do
    (Simulator m inp out) <- S.get
    let newScope = Scope (M.fromList vars) (Just m)
    S.put (Simulator newScope inp out)
    return $ Happy (IntVal 0)

  popScope :: Env Simulator
  popScope = do
    (Simulator m inp out) <- S.get
    let parent = case m of
          Scope _ (Just p) -> p
          Scope _ Nothing -> emptyScope
    S.put (Simulator parent inp out)
    return $ Happy (IntVal 0)

  inputVal :: Env Simulator
  inputVal = do
    (Simulator m inp out) <- S.get
    case inp of
      (x : xs) -> do
        S.put (Simulator m xs out)
        return $ Happy x
      [] -> return $ Sad (Input, "Input stream is empty")

  outputVal :: Value -> Env Simulator
  outputVal val = do
    (Simulator m inp out) <- S.get
    let out' = out ++ [val]
    S.put (Simulator m inp out')
    return $ Happy val

  subVal :: Value -> Value -> Env Simulator
  subVal (IntVal v1) (IntVal v2) = return $ Happy (IntVal (v1 - v2))
  subVal _ _ = return $ Sad (Type, "Type error in subtraction")

  addVal :: Value -> Value -> Env Simulator
  addVal (IntVal v1) (IntVal v2) = return $ Happy (IntVal (v1 + v2))
  addVal _ _ = return $ Sad (Type, "Type error in addition")

  mulVal :: Value -> Value -> Env Simulator
  mulVal (IntVal v1) (IntVal v2) = return $ Happy (IntVal (v1 * v2))
  mulVal _ _ = return $ Sad (Type, "Type error in multiplication")

  divVal :: Value -> Value -> Env Simulator
  divVal (IntVal v1) (IntVal v2) =
    if v2 == 0
      then return $ Sad (Arithmetic, "Cannot divide by 0")
      else return $ Happy (IntVal (v1 `div` v2)) -- I don't want the actual interpreter to crash
  divVal _ _ = return $ Sad (Type, "Type error in division")

  modVal :: Value -> Value -> Env Simulator
  modVal (IntVal v1) (IntVal v2) =
    if v2 == 0
      then return $ Sad (Arithmetic, "Cannot mod by 0")
      else return $ Happy (IntVal (v1 `mod` v2)) -- I don't want the actual interpreter to crash
  modVal _ _ = return $ Sad (Type, "Type error in modulus")

  negVal (IntVal v) = return $ Happy (IntVal (-v))
  negVal _ = return $ Sad (Type, "Type error in neg")

  selectValue :: Value -> Env Simulator -> Env Simulator -> Env Simulator
  selectValue (BoolVal True) e1 _ = e1
  selectValue (BoolVal False) _ e2 = e2
  selectValue (IntVal n) e1 e2 = if n /= 0 then e1 else e2 -- backward compat
  selectValue (StringVal s) e1 e2 = if not (null s) then e1 else e2
  selectValue (Tuple l) e1 e2 = if not (null l) then e1 else e2
  selectValue (ClosureVal {}) _ _ = return $ Sad "Type error in select"
  selectValue (Dictionary _) _ _ = return $ Sad "Type error in select"

  ltVal :: Value -> Value -> Env Simulator
  ltVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 < v2))
  ltVal _ _ = return $ Sad (Type, "Type error in <")

  gtVal :: Value -> Value -> Env Simulator
  gtVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 > v2))
  gtVal _ _ = return $ Sad (Type, "Type error in >")

  lteVal :: Value -> Value -> Env Simulator
  lteVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 <= v2))
  lteVal _ _ = return $ Sad (Type, "Type error in <=")

  gteVal :: Value -> Value -> Env Simulator
  gteVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 >= v2))
  gteVal _ _ = return $ Sad (Type, "Type error in >=")

  eqVal :: Value -> Value -> Env Simulator
  eqVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 == v2))
  eqVal (BoolVal v1) (BoolVal v2) = return $ Happy (BoolVal (v1 == v2))
  eqVal (StringVal v1) (StringVal v2) = return $ Happy (BoolVal (v1 == v2))
  eqVal v1 v2 = return $ Sad $ (Type, "Type error in ==: cannot compare " ++ show v1 ++ " and " ++ show v2)

  neqVal :: Value -> Value -> Env Simulator
  neqVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 /= v2))
  neqVal (BoolVal v1) (BoolVal v2) = return $ Happy (BoolVal (v1 /= v2))
  neqVal (StringVal v1) (StringVal v2) = return $ Happy (BoolVal (v1 /= v2))
  neqVal v1 v2 = return $ Sad $ (Type, "Type error in !=: cannot compare " ++ show v1 ++ " and " ++ show v2)

  andVal :: Value -> Value -> Env Simulator
  andVal (BoolVal v1) (BoolVal v2) = return $ Happy (BoolVal (v1 && v2))
  andVal _ _ = return $ Sad (Type, "Type error in &&")

  orVal :: Value -> Value -> Env Simulator
  orVal (BoolVal v1) (BoolVal v2) = return $ Happy (BoolVal (v1 || v2))
  orVal _ _ = return $ Sad (Type, "Type error in ||")

  notVal :: Value -> Env Simulator
  notVal (BoolVal v) = return $ Happy (BoolVal (not v))
  notVal _ = return $ Sad (Type, "Type error in !")

  getBracketValue :: Value -> Value -> Env Simulator
  getBracketValue (Tuple (x : xs)) (IntVal pos) = if pos == 0 then return (Happy x) else getBracketValue (Tuple xs) (IntVal (pos - 1))
  getBracketValue (Dictionary d) (IntVal val) = case M.lookup val d of
    Just v -> return $ Happy v
    Nothing -> return $ Sad "Unable to find element in dictionary"
  getBracketValue (Dictionary _) _ = return $ Sad "Unable to index into dictionary with type"
  getBracketValue _ _ = return $ Sad "Tuple Lookup Bad Input"

  setBracketValue :: String -> Value -> Value -> Env Simulator
  setBracketValue n t v = do
    (Simulator m inp out) <- S.get
    case lookupScope n m of
      Just oldVal -> case oldVal of
        Tuple _ ->
          let newVal = updateBracket oldVal t v
           in case newVal of
                Just newVal' -> do
                  let m' = insertScope n newVal' m
                  S.put (Simulator m' inp out)
                  return $ Happy v
                Nothing -> return $ Sad "Something went wrong while trying to update Tuple value"
        Dictionary _ ->
          let newVal = updateBracket oldVal t v
           in case newVal of
                Just newVal' -> do
                  let m' = insertScope n newVal' m
                  S.put (Simulator m' inp out)
                  return $ Happy v
                Nothing -> return $ Sad "Something went wrong while trying to update Tuple value"
        _ -> return $ Sad "Attempting to Index but didn't find Tuple"
      Nothing -> return $ Sad "Attempting to Set Tuple That Doesn't Exist"
    where
      updateBracket :: Value -> Value -> Value -> Maybe Value
      updateBracket (Tuple (x : xs)) (Tuple (y : ys)) val = case y of
        IntVal index ->
          if index == 0
            then
              let returnVal = updateBracket x (Tuple ys) val
               in case returnVal of
                    Just a -> Just $ Tuple (a : xs)
                    Nothing -> Nothing
            else
              let returnVal = updateBracket (Tuple xs) (Tuple (IntVal (index - 1) : ys)) val
               in case returnVal of
                    Just (Tuple a) -> Just $ Tuple (x : a)
                    Just _ -> error "Unable to rebuild tuple"
                    Nothing -> Nothing
        _ -> Nothing
      updateBracket (Dictionary d) (Tuple (y : ys)) val = case y of
        IntVal index -> case M.lookup index d of
          Just r ->
            let returnVal = updateBracket r (Tuple ys) val
             in case returnVal of
                  Just w -> Just (Dictionary (M.insert index w d))
                  Nothing -> Nothing
          Nothing -> Nothing
        _ -> Nothing
      updateBracket _ (Tuple []) val = Just val
      updateBracket _ _ _ = Nothing

infixl 1 ~

(~) :: Term -> Term -> Term
(~) = Seq

infixl 9 <=>

(<=>) :: String -> Term -> Term
(<=>) = Let

prog :: Term
prog =
  "x" <=> Literal 10
    ~ "y" <=> Literal 29
    ~ "z" <=> Literal 3

main :: IO ()
main = do
  let out = reduceFully prog (Simulator emptyScope [] [])
  print out
  putStrLn "-----------------------------"
  let out2 = reduceFully Progs.prog (Simulator emptyScope [] [])
  print out2
  putStrLn "-----------------------------"
  putStrLn "Testing booleans and comparisons:"
  let out3 = reduceFully Progs.prog3 (Simulator emptyScope [] [])
  print out3
