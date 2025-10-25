{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ParserCombinators (eof, oneof, opt, Parser, rpt, rptSep, rptDropSep, satisfy, token, tokens, string, (<|>), alt, some, sepBy, sepBy1, between, skip, lookAhead, chainl1, chainr1, parse) where

import Control.Applicative (asum, (<|>))
import Control.Monad.Except (catchError, throwError)
import Control.Monad.State.Lazy (StateT, get, put, runStateT)
import qualified Data.Functor
import Result (Result (..))
import Sprintf ((%), (<<))

-- Parse combinators:
--
--    The type 'Parser t a' represents a function that:
--      - consumes tokens of type 't'
--      - produces a value of type 'Either String a'
--        - may succeed with a value value 'Right a'
--        - may fail with an error messge 'Left String'
--      - returns the result with a list of remaining tokens
--        - (Either String (a, [t]))
--
--    This fits neatly into the StateT monad transformer where:
--       - the state is the list of remaining tokens [f]
--       - the inner monad is Either String
--
--    We rely on the fact that 'Either String' has an instance
--       of MonadFail and use 'Left' to indicate failure and
--       'Right' to indicate success. This allows us to use
--       'do' notation to sequence parsing operations and
--       have failure propagate automatically. With convenient
--       use of 'catchError' to implement choice and optionality.
--

----------- Parser -----------

type Parser t = StateT [t] (Result String)

-- Succeed if there are no more tokens, fail otherwise
eof :: (Show t) => Parser t ()
eof = do
  ts <- get
  case ts of
    [] -> return ()
    _ -> throwError $ "expected eof but found: %s" % ts << []

assert :: (Show t) => String -> (t -> Maybe a) -> Parser t a
assert msg p = do
  ts <- get
  case ts of
    [] -> throwError "out of tokens" -- need better error reporting
    (t : rest) ->
      case p t of
        Just a -> do
          put rest
          return a
        Nothing -> throwError $ msg % (t : rest) << []

satisfy :: (Show t) => (t -> Maybe a) -> Parser t a
satisfy = assert "failed to satisfy predicate at %s"

token :: (Show t, Eq t) => t -> Parser t t
token t = assert
  ("expected %s, found %%s" % t << [])
  $ \t' -> if t == t' then Just t else Nothing

-- Alternative operator that preserves both types
alt :: Parser t a -> Parser t b -> Parser t (Either a b)
alt p1 p2 =
  catchError
    (Left <$> p1)
    (\_ -> Right <$> p2)

oneof :: [Parser t a] -> Parser t a
oneof = asum

opt :: Parser t a -> Parser t (Maybe a)
opt p = (Just <$> p) <|> return Nothing

rpt :: Parser t a -> Parser t [a]
rpt p = some p <|> return []

type RepeatResult a b = Maybe (a, [(b, a)])

dropSep :: RepeatResult a b -> [a]
dropSep Nothing = []
dropSep (Just (x, xbs)) = x : map snd xbs

rptSep :: Parser t a -> Parser t b -> Parser t (RepeatResult a b)
rptSep p sep = opt $ do
  x <- p
  xs <- rpt $ (,) <$> sep <*> p
  return (x, xs)

rptDropSep :: Parser t a -> Parser t b -> Parser t [a]
rptDropSep p sep = dropSep <$> rptSep p sep

-- One or more repetitions
some :: Parser t a -> Parser t [a]
some p = do
  x <- p
  xs <- rpt p
  return (x : xs)

-- Parse zero or more occurrences separated by a separator
sepBy :: Parser t a -> Parser t sep -> Parser t [a]
sepBy p sep = sepBy1 p sep <|> return []

-- Parse one or more occurrences separated by a separator
sepBy1 :: Parser t a -> Parser t sep -> Parser t [a]
sepBy1 p sep = do
  x <- p
  xs <- rpt (sep *> p)
  return (x : xs)

-- Parse between delimiters
between :: Parser t open -> Parser t close -> Parser t a -> Parser t a
between open close p = open *> p <* close

-- Skip/ignore result
skip :: Parser t a -> Parser t ()
skip p = p Data.Functor.$> ()

-- Parse with lookahead (don't consume)
lookAhead :: Parser t a -> Parser t a
lookAhead p = do
  ts <- get
  result <- p
  put ts
  return result

-- Left-associative chaining: parses "1 + 2 + 3" as "(1 + 2) + 3"
chainl1 :: Parser t a -> Parser t (a -> a -> a) -> Parser t a
chainl1 p op = p >>= rest
  where
    rest x = ((\f y -> f x y) <$> op <*> p >>= rest) <|> return x

-- Right-associative chaining: parses "1 ^ 2 ^ 3" as "1 ^ (2 ^ 3)"
chainr1 :: Parser t a -> Parser t (a -> a -> a) -> Parser t a
chainr1 p op = p >>= \x -> ((\f y -> f x y) <$> op <*> chainr1 p op) <|> return x

-- Match a specific sequence of tokens
tokens :: (Show t, Eq t) => [t] -> Parser t [t]
tokens = mapM token

-- Convenient alias for parsing strings
string :: String -> Parser Char String
string = tokens

-- Run parser and ensure EOF
parse :: (Show t) => Parser t a -> [t] -> Result String a
parse p ts = do
  (result, _) <- runStateT (p <* eof) ts
  return result
