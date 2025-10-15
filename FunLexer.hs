module FunLexer (lexer, Token (Num, Ident, Keyword, Symbol, Error)) where

import Data.Char (isAlpha, isAlphaNum, isNumber, isSpace)
import Data.List (unfoldr)
import qualified Data.Set as S

data Token
  = Num Integer
  | Ident String
  | Keyword String
  | Symbol String
  | StringLiteral String
  | Error String
  deriving (Show, Eq)

symbols :: S.Set Char
symbols = S.fromList "-*+(){}=,"

keywords :: S.Set String
keywords =
  S.fromList
    [ "fun",
      "var",
      "if",
      "else",
      "while",
      "print",
      "try",
      "catch"
    ]

lexer :: [Char] -> [Token]
lexer = unfoldr step
  where
    step [] = Nothing
    -- skip spaces and new lines
    step (c : rest) | isSpace c = step rest
    -- numbers
    step s@(c : _)
      | isNumber c =
          let (num, rest) = span isNumber s
           in Just (Num $ read num, rest)
    -- identifiers and keywords
    step s@(c : _)
      | isAlpha c =
          let (var, rest) = span isAlphaNum s
           in Just (if S.member var keywords then Keyword var else Ident var, rest)
    -- string literals
    step ('"' : rest) =
      let (str, rest2) = span (/= '"') rest
       in case rest2 of
            ('"' : rest3) -> Just (StringLiteral str, rest3)
            _ -> Just (Error "Unclosed string literal", "")
    -- symbols
    step (c : rest)
      | S.member c symbols =
          Just (Symbol [c], rest)
    -- comments
    step ('$' : rest) =
      let (_, rest2) = span (/= '$') rest
       in case rest2 of
            ('$' : rest3) -> step rest3
            _ -> Just (Error "Unclosed comment", "")
    -- syntax errors
    step s = Just (Error ("Unexpected character: " ++ take 20 s), "")
