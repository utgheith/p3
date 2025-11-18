{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Result (Result (..)) where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus (mplus, mzero))
import Control.Monad.Except (MonadError (catchError, throwError))
import Data.String (IsString (fromString))

data Result e a = Err e | Ok a
  deriving (Show, Eq)

instance Functor (Result e) where
  fmap :: (a -> b) -> Result e a -> Result e b
  fmap _ (Err e) = Err e
  fmap f (Ok a) = Ok (f a)

instance Applicative (Result e) where
  pure :: a -> Result e a
  pure = Ok

  (<*>) :: Result e (a -> b) -> Result e a -> Result e b
  (Err e) <*> _ = Err e
  _ <*> (Err e) = Err e
  (Ok f) <*> (Ok a) = Ok (f a)

instance Monad (Result e) where
  return :: a -> Result e a
  return = pure

  (>>=) :: Result e a -> (a -> Result e b) -> Result e b
  (Err e) >>= _ = Err e
  (Ok a) >>= f = f a

instance (IsString e) => MonadFail (Result e) where
  fail :: String -> Result e a
  fail msg = Err (fromString msg)

instance MonadError e (Result e) where
  throwError :: e -> Result e a
  throwError = Err

  catchError :: Result e a -> (e -> Result e a) -> Result e a
  catchError (Ok a) _ = Ok a
  catchError (Err e) handler = handler e

instance (IsString e) => Alternative (Result e) where
  empty :: Result e a
  empty = Err (fromString "no rule matched")

  (<|>) :: Result e a -> Result e a -> Result e a
  (Ok a) <|> _ = Ok a -- precedence to left alternative
  _ <|> (Ok a) = Ok a
  _ <|> _ = empty

instance (IsString e) => MonadPlus (Result e) where
  mzero :: Result e a
  mzero = Err (fromString "no rule matched")

  mplus :: Result e a -> Result e a -> Result e a
  (Ok a) `mplus` _ = Ok a -- precedence to left alternative
  _ `mplus` (Ok a) = Ok a
  _ `mplus` _ = mzero
