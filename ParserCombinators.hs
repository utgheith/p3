module ParserCombinators (eof, oneof, opt, Parser, Result, rpt, rptSep, rptDropSep, satisfy, token, tokens, string, (<|>), alt, some, sepBy, sepBy1, between, skip, lookAhead, chainl1, chainr1, parse) where

import Control.Monad ((>=>))
import Control.Monad.Except (catchError, throwError)
import Control.Monad.State.Lazy (StateT, get, put, runStateT)
import qualified Data.Functor

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

----------- Result -----------

type Result = Either String

----------- Parser -----------

type Parser t = StateT [t] Result

-- Succeed if there are no more tokens, fail otherwise
eof :: (Show t) => Parser t ()
eof = do
  ts <- get
  case ts of
    [] -> return ()
    _ -> throwError $ "expected eof but found: " ++ show ts

satisfy_ :: (Show t) => [Char] -> (t -> Maybe a) -> Parser t a
satisfy_ msg p = do
  tokens <- get
  case tokens of
    [] -> throwError "out of tokens"

satisfy :: (Show t) => (t -> Maybe a) -> Parser t a
satisfy p = do
  ts <- get
  case ts of
    [] -> throwError "out of tokens" -- need better error reporting
    (t : rest) -> do
      case p t of
        Just a -> do
          put rest
          return a
        Nothing -> throwError $ msg ++ show (t : rest)

satisfy :: (Show t) => (t -> Maybe a) -> Parser t a
satisfy = satisfy_ "failed to satisfy predicate at "

token :: (Show t, Eq t) => t -> Parser t t
token t = do
  ts <- get
  case ts of
    [] -> throwError "out of tokens"
    (t' : rest) ->
      if t == t'
        then do
          put rest
          return t
        else throwError ("expected " ++ show t ++ ", found " ++ show (t' : rest))

-- Choice operator: try first parser, if it fails try second
(<|>) :: Parser t a -> Parser t a -> Parser t a
p1 <|> p2 = catchError p1 (const p2)

-- Alternative operator that preserves both types
alt :: Parser t a -> Parser t b -> Parser t (Either a b)
alt p1 p2 =
  catchError
    (Left <$> p1)
    (\_ -> Right <$> p2)

oneof :: [Parser t a] -> Parser t a
oneof = foldr (<|>) (throwError "no choices left in oneof")

opt :: Parser t a -> Parser t (Maybe a)
opt p =
  catchError
    (Just <$> p)
    (const $ return Nothing)

rpt :: Parser t a -> Parser t [a]
rpt p =
  catchError
    ( do
        x <- p
        xs <- rpt p
        return (x : xs)
    )
    (const $ return [])

type RepeatResult a b = Maybe (a, [(b, a)])

dropSep :: RepeatResult a b -> [a]
dropSep Nothing = []
dropSep (Just (x, xbs)) = x : map snd xbs

rptSep :: Parser t a -> Parser t b -> Parser t (RepeatResult a b)
rptSep p sep =
  catchError
    ( do
        x <- p
        xs <- rpt $ do
          s <- sep
          v <- p
          return (s, v)
        return $ Just (x, xs)
    )
    (const $ return Nothing)

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
chainl1 p op = do
  x <- p
  rest x
  where
    rest x =
      ( do
          f <- op
          y <- p
          rest (f x y)
      )
        <|> return x

-- Right-associative chaining: parses "1 ^ 2 ^ 3" as "1 ^ (2 ^ 3)"
chainr1 :: Parser t a -> Parser t (a -> a -> a) -> Parser t a
chainr1 p op = do
  x <- p
  ( do
      f <- op
      y <- chainr1 p op
      return (f x y)
    )
    <|> return x

-- Match a specific sequence of tokens
tokens :: (Show t, Eq t) => [t] -> Parser t [t]
tokens [] = return []
tokens (t : ts) = do
  _ <- token t
  _ <- tokens ts
  return (t : ts)

-- Convenient alias for parsing strings
string :: String -> Parser Char String
string = tokens

-- Run parser and ensure EOF
parse :: (Show t) => Parser t a -> [t] -> Result a
parse p ts = do
  (result, _) <- runStateT (p <* eof) ts
  return result
