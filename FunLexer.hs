module FunLexer (lexer, Token (Num, Ident, Keyword, Symbol, StringLiteralLexed, Error)) where

import Data.Char (isAlpha, isAlphaNum, isNumber, isSpace)
import Data.List (sortOn, stripPrefix, unfoldr)
import Data.Ord (Down (Down))
import qualified Data.Set as S

data Token
  = Num Integer
  | Ident String
  | Keyword String
  | Symbol String
  | StringLiteralLexed String
  | Error String
  deriving (Show, Eq)

symbols :: [String] -- sorted in decreasing order of length to allow prefix detection
symbols =
  sortOn
    (Data.Ord.Down . length)
    [ "-",
      "*",
      "+",
      "/",
      ">=",
      "<=",
      "<",
      ">",
      "==",
      "!",
      "||",
      "&&",
      "(",
      ")",
      "{",
      "}",
      "[",
      "]",
      "=",
      ",",
      "#",
      "**",
      "^",
      "++",
      "--",
      "%",
      "!=",
      ".",
      ";",
      ":",
      "->"
    ]

keywords :: S.Set String
keywords =
  S.fromList
    [ "auto",
      "int",
      "bool",
      "str",
      "unit",
      "metric",
      "invariant",
      "assert",
      "fun",
      "var",
      "if",
      "else",
      "while",
      "write",
      "for",
      "read",
      "try",
      "catch",
      "true",
      "false", -- force boolean literals to be tokens in the parser
      "unknown",
      "break"
    ]

-- a lexer combinator, i suppose
matches :: String -> [String] -> Maybe (String, String)
matches s (p : ps) =
  case stripPrefix p s of
    Just rest -> Just (p, rest)
    Nothing -> matches s ps
matches _ [] = Nothing

lexer :: String -> [Token]
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
    -- identifiers
    step s@(c : _)
      | isAlpha c =
          let (var, rest) = span isAlphaNum s
           in Just (if S.member var keywords then Keyword var else Ident var, rest)
    -- string literals
    step ('"' : rest) =
      let (str, rest2) = span (/= '"') rest
       in case rest2 of
            ('"' : rest3) -> Just (StringLiteralLexed str, rest3)
            _ -> Just (Error "Unclosed string literal", "")
    -- more string literals
    step ('\'' : rest) =
      let (str, rest2) = span (/= '\'') rest
       in case rest2 of
            ('\'' : rest3) -> Just (StringLiteralLexed str, rest3)
            _ -> Just (Error "Unclosed string literal", "")
    -- comments
    step ('$' : rest) =
      let (_, rest2) = span (/= '$') rest
       in case rest2 of
            ('$' : rest3) -> step rest3
            _ -> Just (Error "Unclosed comment", "")
    -- special tokens
    step s = case matches s symbols of
      Just (p, rest) -> Just (Symbol p, rest)
      Nothing -> Just (Error ("Unexpected character: " ++ take 20 s), "") -- syntax error
