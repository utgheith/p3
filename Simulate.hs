{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import qualified Control.Monad.State as S
import qualified Data.Map as M
import qualified Progs
import Small (Env, Machine (..), Result (..), reduceFully)
import Term (Term (..))

data Simulator = Simulator (M.Map String Integer) [Integer] [Integer] deriving (Eq, Show)

instance Machine Simulator where
  type V Simulator = Integer
  getVar :: String -> Env Simulator
  getVar name = do
    (Simulator m _ _) <- S.get
    case M.lookup name m of
      Just v -> return $ Happy v
      Nothing -> return $ Sad $ "get: " ++ name ++ " not found"

  setVar :: String -> Integer -> Env Simulator
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
      [] -> return $ Sad $ "Input stream is empty"

  outputVal :: Integer -> Env Simulator
  outputVal val = do
    (Simulator m inp out) <- S.get
    let out' = out ++ [val]
    S.put (Simulator m inp out')
    return $ Happy val

  subVal :: Integer -> Integer -> Env Simulator
  subVal v1 v2 = return $ Happy (v1 - v2)

  selectValue :: Integer -> Env Simulator -> Env Simulator -> Env Simulator
  selectValue n e1 e2 =
    if n /= 0
      then e1
      else e2

  intToV :: Simulator -> Integer -> Integer
  intToV _ = id

  -- toLiteral :: Env Simulator -> Integer

  vToInt :: Simulator -> Integer -> Integer
  vToInt _ = id

infixl 1 ~

(~) :: Term -> Term -> Term
(~) = Seq

infixl 9 <=>

(<=>) :: String -> Term -> Term
(<=>) = Let

prog :: Term
prog =
  "x"
    <=> Literal 10
    ~ "y"
    <=> Literal 29
    ~ "z"
    <=> Literal 3

main :: IO ()
main = do
  let out = reduceFully prog (Simulator M.empty [] [])
  print out
  let out2 = reduceFully Progs.prog (Simulator M.empty [] [])
  print out2
