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
import ParserCombinators (Parser, Result, between, oneof, opt, rpt, rptDropSep, satisfy, token)

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
-- succeed if the next token is a symbol satisfying the given predicate
checkSymbol :: (String -> Bool) -> Parser Token String
checkSymbol predicate = satisfy $ \case
  Symbol s | predicate s -> Just s
  _ -> Nothing

-- isNum
-- succeed if the next token is a number, returning its value
isNum :: Parser Token Integer
isNum = satisfy $ \case
  Num n -> Just n
  _ -> Nothing

-- isString
-- succeed if the next token is a string literal, returning its value
isString :: Parser Token String
isString = satisfy $ \case
  StringLiteral s -> Just s
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
  rhss <- rpt [(op, rhs) | op <- checkSymbol (`S.member` ops), rhs <- term]

  -- combine results left to right
  return $ foldl (\acc (op, rhs) -> BinaryOp op acc rhs) lhs rhss

------------------- unary operators  -------------------

assign :: Parser Token Term
assign = [Assign name expr | name <- ident, _ <- symbol "=", expr <- term]

-- We can use monad comprehensions (GHC extension) to make parsers more concise
minus :: Parser Token Term
minus = [Negate e | e <- symbol "-" >> unaryExp]

num :: Parser Token Term
num = [Const n | n <- isNum]

string :: Parser Token Term
string = [ConstString s | s <- isString]

parens :: Parser Token Term
parens = [t | t <- between (symbol "(") (symbol ")") term]

funDef :: Parser Token Term
funDef =
  [FunDef name params body | name <- keyword "fun" >> ident, params <- between (symbol "(") (symbol ")") (rptDropSep ident (symbol ",")), body <- term]

varRef :: Parser Token Term
varRef = VarRef <$> ident

block :: Parser Token Term
block = [Block ts | ts <- between (symbol "{") (symbol "}") (rpt term)]

ifExpr :: Parser Token Term
ifExpr = [IfThenElse cond thenTerm elseTerm | cond <- keyword "if" >> term, thenTerm <- term, elseTerm <- opt (keyword "else" >> term)]

varDef :: Parser Token Term
varDef = [VarDef name expr | name <- keyword "var" >> ident, expr <- opt $ symbol "=" >> term]

whileTerm :: Parser Token Term
whileTerm = [While cond body | cond <- keyword "while" >> term, body <- term]

unaryExp :: Parser Token Term
unaryExp = oneof [assign, ifExpr, block, funDef, minus, num, string, parens, varDef, varRef, whileTerm]

----------- prog ----------

prog :: Parser Token Term
prog = [Block terms | terms <- rpt term]

----------- parse ----------

parse :: [Char] -> Parser Token a -> Result (a, [Token])
parse input p =
  let tokens = lexer input
   in runStateT p tokens
