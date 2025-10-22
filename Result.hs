{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Result (Result (..)) where

import Control.Monad.Except (MonadError (catchError, throwError))

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

instance (Monoid e) => MonadFail (Result e) where
  fail :: String -> Result e a
  fail _ = Err mempty

instance MonadError e (Result e) where
  throwError :: e -> Result e a
  throwError = Err

  catchError :: Result e a -> (e -> Result e a) -> Result e a
  catchError (Ok a) _ = Ok a
  catchError (Err e) handler = handler e
