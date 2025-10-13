{-# LANGUAGE LambdaCase #-}

module ParserCombinators (eof, oneof, opt, Parser, Result, rpt, rptSep, rptDropSep, satisfy, token, (<|>)) where

import Control.Monad.Except (catchError, throwError)
import Control.Monad.State.Lazy (StateT, get, put)

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
  tokens <- get
  case tokens of
    [] -> return ()
    _ -> throwError $ "expected eof but found: " ++ show tokens

satisfy :: (Show t) => (t -> Maybe a) -> Parser t a
satisfy p = do
  tokens <- get
  case tokens of
    [] -> throwError "out of tokens" -- need better error reporting
    (t : rest) -> do
      case p t of
        Just a -> do
          put rest
          return a
        Nothing -> throwError $ "failed to satisfy predicate at " ++ show (t : rest)

token :: (Show t, Eq t) => t -> Parser t t
token t = do
  tokens <- get
  case tokens of
    [] -> throwError "out of tokens"
    (t' : rest) ->
      if t == t'
        then do
          put rest
          return t
        else throwError ("expected " ++ show t ++ ", found " ++ show (t' : rest))

(<|>) :: Parser t a -> Parser t b -> Parser t (Either a b)
p1 <|> p2 =
  catchError
    (Left <$> p1)
    (\_ -> Right <$> p2)

oneof :: [Parser t a] -> Parser t a
oneof [] = throwError "no choices left in oneof" -- need better error reporting
oneof (p : ps) =
  fmap
    ( \case
        (Left a) -> a
        (Right a) -> a
    )
    (p <|> oneof ps)

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
