{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import qualified Control.Monad.State as S
import qualified Data.Map as M
import qualified Progs
import Small (Env, Machine (..), Result (..), reduceFully)
import Term (Term (..))
import Value (Value (..))

data Simulator = Simulator (M.Map String Value) [Value] [Value] deriving (Eq, Show)

instance Machine Simulator where
  type V Simulator = Value
  getVar :: String -> Env Simulator
  getVar name = do
    (Simulator m _ _) <- S.get
    case M.lookup name m of
      Just v -> return $ Happy v
      Nothing -> return $ Sad $ "get: " ++ name ++ " not found"

  setVar :: String -> Value -> Env Simulator
  setVar name val = do
    (Simulator m inp out) <- S.get
    let m' = M.insert name val m
    S.put (Simulator m' inp out)
    return $ Happy val

  inputVal :: Env Simulator
  inputVal = do
    (Simulator m inp out) <- S.get
    case inp of
      (x : xs) -> do
        S.put (Simulator m xs out)
        return $ Happy x
      [] -> return $ Sad "Input stream is empty"

  outputVal :: Value -> Env Simulator
  outputVal val = do
    (Simulator m inp out) <- S.get
    let out' = out ++ [val]
    S.put (Simulator m inp out')
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

  selectValue :: Value -> Env Simulator -> Env Simulator -> Env Simulator
  selectValue (BoolVal True) e1 _ = e1
  selectValue (BoolVal False) _ e2 = e2
  selectValue (IntVal n) e1 e2 = if n /= 0 then e1 else e2 -- backward compat

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
  eqVal _ _ = return $ Sad "Type error in =="

  neqVal :: Value -> Value -> Env Simulator
  neqVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 /= v2))
  neqVal _ _ = return $ Sad "Type error in !="

  andVal :: Value -> Value -> Env Simulator
  andVal (BoolVal v1) (BoolVal v2) = return $ Happy (BoolVal (v1 && v2))
  andVal _ _ = return $ Sad "Type error in &&"

  orVal :: Value -> Value -> Env Simulator
  orVal (BoolVal v1) (BoolVal v2) = return $ Happy (BoolVal (v1 || v2))
  orVal _ _ = return $ Sad "Type error in ||"

  notVal :: Value -> Env Simulator
  notVal (BoolVal v) = return $ Happy (BoolVal (not v))
  notVal _ = return $ Sad "Type error in !"

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
  let out = reduceFully prog (Simulator M.empty [] [])
  print out
  putStrLn "-----------------------------"
  let out2 = reduceFully Progs.prog (Simulator M.empty [] [])
  print out2
  putStrLn "-----------------------------"
  putStrLn "Testing booleans and comparisons:"
  let out3 = reduceFully Progs.prog3 (Simulator M.empty [] [])
  print out3
