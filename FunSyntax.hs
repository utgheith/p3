{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module FunSyntax (parse, prog, term, Term (Assign, BinaryOp, Block, Call, Const, ConstString, FunDef, IfThenElse, Negate, VarDef, VarRef, While)) where

import qualified Control.Monad as M
import Control.Monad.State.Lazy (runStateT)
-- import Debug.Trace (trace)

import qualified Data.Set as S
import FunLexer (Token (Ident, Keyword, Num, StringLiteral, Symbol), lexer)
import ParserCombinators (Parser, Result, oneof, opt, rpt, rptDropSep, satisfy, token)

data Term
  = Assign String Term
  | BinaryOp String Term Term
  | Block [Term]
  | Call Term [Term]
  | Const Integer
  | ConstString String
  | FunDef String [String] Term
  | IfThenElse Term Term (Maybe Term)
  | Negate Term
  | VarDef String (Maybe Term)
  | VarRef String
  | While Term Term
  deriving
    ( -- | more term constructors
      Show,
      Eq
    )

-- succeed if the next token is the given symbol
symbol :: String -> Parser Token ()
-- using explicit bind
symbol s = M.void (token (Symbol s))

-- succeed if the next token is the given keyword
keyword :: String -> Parser Token ()
-- using do notation (syntactic sugar for >>=)
keyword k = do
  _ <- token $ Keyword k
  return ()

-- identifier
ident :: Parser Token String
ident = satisfy $ \case
  Ident name -> Just name
  _ -> Nothing

-- symbol
checkSymbol :: (String -> Bool) -> Parser Token String
checkSymbol predicate = satisfy $ \case
  Symbol s | predicate s -> Just s
  _ -> Nothing

----------
-- term --
----------

term :: Parser Token Term
term = binaryExp precedence

------------------- binary operators (left associative) -------------------

-- precedence levels, from lowest to highest
precedence :: [S.Set String]
precedence = [S.fromList ["+"], S.fromList ["*", "/"]]

binaryExp :: [S.Set String] -> Parser Token Term
binaryExp [] = unaryExp
binaryExp (ops : rest) = do
  -- lhs
  lhs <- binaryExp rest

  -- find the longest sequence of (op, subexpression) at this precedence level
  -- then combine them left to right
  rhss <- rpt $ do
    op <- checkSymbol (`S.member` ops)
    rhs <- term
    return (op, rhs)

  -- combine results left to right
  return $ foldl (\acc (op, rhs) -> BinaryOp op acc rhs) lhs rhss

------------------- unary operators  -------------------

assign :: Parser Token Term
assign = [Assign name expr | name <- ident, _ <- symbol "=", expr <- term]

-- We can use monad comprehensions (GHC extension) to make parsers more concise
minus :: Parser Token Term
minus = [Negate e | _ <- symbol "-", e <- unaryExp]

num :: Parser Token Term
num = do
  n <- satisfy $ \case
    Num n -> Just n
    _ -> Nothing
  return $ Const n

string :: Parser Token Term
string = do
  s <- satisfy $ \case
    StringLiteral s -> Just s
    _ -> Nothing
  return $ ConstString s

parens :: Parser Token Term
parens = [t | _ <- symbol "(", t <- term, _ <- symbol ")"]

funDef :: Parser Token Term
funDef =
  [ FunDef name params body | _ <- keyword "fun", name <- ident, _ <- symbol "(", params <- rptDropSep ident (symbol ","), _ <- symbol ")", body <- term
  ]

varRef :: Parser Token Term
varRef = VarRef <$> ident

block :: Parser Token Term
block = do
  _ <- token $ Symbol "{"
  ts <- rpt term
  _ <- token $ Symbol "}"
  return $ Block ts

ifExpr :: Parser Token Term
ifExpr = do
  _ <- keyword "if"
  cond <- term
  thenTerm <- term
  elseTerm <- opt $ keyword "else" >> term
  return $ IfThenElse cond thenTerm elseTerm

varDef :: Parser Token Term
varDef = do
  _ <- keyword "var"
  name <- ident
  expr <- opt $ symbol "=" >> term
  return $ VarDef name expr

whileTerm :: Parser Token Term
whileTerm = do
  _ <- keyword "while"
  cond <- term
  body <- term
  return $ While cond body

unaryExp :: Parser Token Term
unaryExp = oneof [assign, ifExpr, block, funDef, minus, num, string, parens, varDef, varRef, whileTerm]

----------- prog ----------

prog :: Parser Token Term
prog = Block <$> rpt term

----------- parse ----------

parse :: [Char] -> Parser Token a -> Result (a, [Token])
parse input p =
  let tokens = lexer input
   in runStateT p tokens
