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
import System.Random as R
import Term (Term (..))
import Value (Value (..))

-- TODO: this should really be refactored to use random monads
data Simulator = Simulator Scope [Value] [Value] R.StdGen deriving (Eq, Show)

decide :: (R.RandomGen g) => g -> a -> a -> (a, g)
decide rng a1 a2 =
  let (num, rng') = R.uniformR (0 :: Int, 1 :: Int) rng
   in (if num == 0 then a1 else a2, rng')

instance Machine Simulator where
  type V Simulator = Value
  getVar :: String -> Env Simulator
  getVar name = do
    (Simulator m _ _ _) <- S.get
    case lookupScope name m of
      Just v -> return $ Happy v
      Nothing -> return $ Sad $ "get: " ++ name ++ " not found"

  setVar :: String -> Value -> Env Simulator
  setVar name val = do
    (Simulator m inp out rng) <- S.get
    let m' = insertScope name val m
    S.put (Simulator m' inp out rng)
    return $ Happy val

  getScope :: Simulator -> [(String, Value)]
  getScope (Simulator m _ _ _) = getAllBindings m

  pushScope :: [(String, Value)] -> Env Simulator
  pushScope vars = do
    (Simulator m inp out rng) <- S.get
    let newScope = Scope (M.fromList vars) (Just m)
    S.put (Simulator newScope inp out rng)
    return $ Happy (IntVal 0)

  popScope :: Env Simulator
  popScope = do
    (Simulator m inp out rng) <- S.get
    let parent = case m of
          Scope _ (Just p) -> p
          Scope _ Nothing -> emptyScope
    S.put (Simulator parent inp out rng)
    return $ Happy (IntVal 0)

  inputVal :: Env Simulator
  inputVal = do
    (Simulator m inp out rng) <- S.get
    case inp of
      (x : xs) -> do
        S.put (Simulator m xs out rng)
        return $ Happy x
      [] -> return $ Sad "Input stream is empty"

  outputVal :: Value -> Env Simulator
  outputVal val = do
    (Simulator m inp out rng) <- S.get
    let out' = out ++ [val]
    S.put (Simulator m inp out' rng)
    return $ Happy val

  subVal :: Value -> Value -> Env Simulator
  subVal (IntVal v1) (IntVal v2) = return $ Happy (IntVal (v1 - v2))
  subVal _ _ = return $ Sad "Type error in subtraction"

  addVal :: Value -> Value -> Env Simulator
  addVal (IntVal v1) (IntVal v2) = return $ Happy (IntVal (v1 + v2))
  addVal _ _ = return $ Sad "Type error in addition"

  mulVal :: Value -> Value -> Env Simulator
  mulVal (IntVal v1) (IntVal v2) = return $ Happy (IntVal (v1 * v2))
  mulVal _ _ = return $ Sad "Type error in multiplication"

  divVal :: Value -> Value -> Env Simulator
  divVal (IntVal v1) (IntVal v2) =
    if v2 == 0
      then return $ Sad "Cannot divide by 0"
      else return $ Happy (IntVal (v1 `div` v2)) -- I don't want the actual interpreter to crash
  divVal _ _ = return $ Sad "Type error in division"

  modVal :: Value -> Value -> Env Simulator
  modVal (IntVal v1) (IntVal v2) =
    if v2 == 0
      then return $ Sad "Cannot mod by 0"
      else return $ Happy (IntVal (v1 `mod` v2)) -- I don't want the actual interpreter to crash
  modVal _ _ = return $ Sad "Type error in modulus"

  negVal (IntVal v) = return $ Happy (IntVal (-v))
  negVal _ = return $ Sad "Type error in neg"

  selectValue :: Value -> Env Simulator -> Env Simulator -> Env Simulator
  selectValue (BoolVal True) e1 _ = e1
  selectValue (BoolVal False) _ e2 = e2
  selectValue (IntVal n) e1 e2 = if n /= 0 then e1 else e2 -- backward compat
  selectValue (StringVal s) e1 e2 = if not (null s) then e1 else e2
  selectValue (Tuple l) e1 e2 = if not (null l) then e1 else e2
  selectValue (ClosureVal {}) _ _ = return $ Sad "Type error in select"

  selectRandom :: Simulator -> Env Simulator -> Env Simulator -> Env Simulator
  selectRandom (Simulator _ _ _ rng) e1 e2 =
    let (e', rng') = decide rng e1 e2
     in do
          v <- e'
          (Simulator m inp out _) <- S.get
          S.put (Simulator m inp out rng')
          return v
  ltVal :: Value -> Value -> Env Simulator
  ltVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 < v2))
  ltVal _ _ = return $ Sad "Type error in <"

  gtVal :: Value -> Value -> Env Simulator
  gtVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 > v2))
  gtVal _ _ = return $ Sad "Type error in >"

  lteVal :: Value -> Value -> Env Simulator
  lteVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 <= v2))
  lteVal _ _ = return $ Sad "Type error in <="

  gteVal :: Value -> Value -> Env Simulator
  gteVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 >= v2))
  gteVal _ _ = return $ Sad "Type error in >="

  eqVal :: Value -> Value -> Env Simulator
  eqVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 == v2))
  eqVal (BoolVal v1) (BoolVal v2) = return $ Happy (BoolVal (v1 == v2))
  eqVal (StringVal v1) (StringVal v2) = return $ Happy (BoolVal (v1 == v2))
  eqVal v1 v2 = return $ Sad $ "Type error in ==: cannot compare " ++ show v1 ++ " and " ++ show v2

  neqVal :: Value -> Value -> Env Simulator
  neqVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 /= v2))
  neqVal (BoolVal v1) (BoolVal v2) = return $ Happy (BoolVal (v1 /= v2))
  neqVal (StringVal v1) (StringVal v2) = return $ Happy (BoolVal (v1 /= v2))
  neqVal v1 v2 = return $ Sad $ "Type error in !=: cannot compare " ++ show v1 ++ " and " ++ show v2

  andVal :: Value -> Value -> Env Simulator
  andVal (BoolVal v1) (BoolVal v2) = return $ Happy (BoolVal (v1 && v2))
  andVal _ _ = return $ Sad "Type error in &&"

  orVal :: Value -> Value -> Env Simulator
  orVal (BoolVal v1) (BoolVal v2) = return $ Happy (BoolVal (v1 || v2))
  orVal _ _ = return $ Sad "Type error in ||"

  notVal :: Value -> Env Simulator
  notVal (BoolVal v) = return $ Happy (BoolVal (not v))
  notVal _ = return $ Sad "Type error in !"

  getTupleValue :: Value -> Value -> Env Simulator
  getTupleValue (Tuple (x : xs)) (IntVal pos) = if pos == 0 then return (Happy x) else getTupleValue (Tuple xs) (IntVal (pos - 1))
  getTupleValue _ _ = return $ Sad "Tuple Lookup Bad Input"

  setTupleValue :: String -> Value -> Value -> Env Simulator
  setTupleValue n t v = do
    (Simulator m inp out rng) <- S.get
    case lookupScope n m of
      Just oldVal -> case oldVal of
        Tuple _ ->
          let newVal = updateTuple oldVal t v
           in case newVal of
                Just newVal' -> do
                  let m' = insertScope n newVal' m
                  S.put (Simulator m' inp out rng)
                  return $ Happy v
                Nothing -> return $ Sad "Something went wrong while trying to update Tuple value"
        _ -> return $ Sad "Attempting to Index but didn't find Tuple"
      Nothing -> return $ Sad "Attempting to Set Tuple That Doesn't Exist"
    where
      updateTuple :: Value -> Value -> Value -> Maybe Value
      updateTuple (Tuple (x : xs)) (Tuple (y : ys)) val = case y of
        IntVal index ->
          if index == 0
            then
              let returnVal = updateTuple x (Tuple ys) val
               in case returnVal of
                    Just a -> Just $ Tuple (a : xs)
                    Nothing -> Nothing
            else
              let returnVal = updateTuple (Tuple xs) (Tuple (IntVal (index - 1) : ys)) val
               in case returnVal of
                    Just (Tuple a) -> Just $ Tuple (x : a)
                    Nothing -> Nothing
                    _ -> error "Unable to rebuild tuple"
        _ -> Nothing
      updateTuple _ (Tuple []) val = Just val
      updateTuple _ _ _ = Nothing

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
  let out = reduceFully prog (Simulator emptyScope [] [] (R.mkStdGen 42))
  print out
  putStrLn "-----------------------------"
  let out2 = reduceFully Progs.prog (Simulator emptyScope [] [] (R.mkStdGen 42))
  print out2
  putStrLn "-----------------------------"
  putStrLn "Testing booleans and comparisons:"
  let out3 = reduceFully Progs.prog3 (Simulator emptyScope [] [] (R.mkStdGen 42))
  print out3
  let out4 = reduceFully Progs.prog4 (Simulator emptyScope [] [] (R.mkStdGen 2))
  print out4
  putStrLn "-----------------------------"
  putStrLn "Testing data races:"
  let out5 = reduceFully Progs.prog5 (Simulator emptyScope [] [] (R.mkStdGen 44))
  print out5
