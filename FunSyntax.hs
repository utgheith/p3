{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module FunSyntax (parse, prog, term, Term (Let, BinaryOps, Seq, Skip, UnaryOps, Var, While, Write, BoolLit, Literal, StringLiteral, Fun, ApplyFun)) where

import qualified Control.Monad as M
import Control.Monad.State.Lazy (runStateT)
import Data.Maybe (fromMaybe)
-- import Debug.Trace (trace)

import qualified Data.Set as S
import FunLexer (Token (Ident, Keyword, Num, StringLiteralLexed, Symbol), lexer)
import ParserCombinators (Parser, Result, oneof, opt, rpt, rptDropSep, satisfy, token)
import Term (BinaryOp (..), ErrorKind (..), ErrorKindOrAny (..), Term (..), UnaryOp (..))

-- data Term
--   = Assign String Term
--   | BinaryOp String Term Term
--   | Block [Term]
--   | Call Term [Term]
--   | Const Integer
--   | ConstString String
--   | FunDef String [String] Term
--   | IfThenElse Term Term (Maybe Term)
--   | Negate Term
--   | VarDef String (Maybe Term)
--   | VarRef String
--   | While Term Term
--   deriving
--     ( -- | more term constructors
--       Show,
--       Eq
--     )

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
precedence = [S.fromList ["||"], S.fromList ["&&"], S.fromList ["==", "!="], S.fromList ["<", ">", "<=", ">="], S.fromList ["+", "-"], S.fromList ["*", "/", "%"]]

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
  return $ foldl (\acc (op, rhs) -> BinaryOps (stringToBinaryOp op) acc rhs) lhs rhss

stringToBinaryOp :: String -> BinaryOp
stringToBinaryOp "+" = Add
stringToBinaryOp "-" = Sub
stringToBinaryOp "*" = Mul
stringToBinaryOp "/" = Div
stringToBinaryOp "%" = Mod
stringToBinaryOp "<" = Lt
stringToBinaryOp ">" = Gt
stringToBinaryOp "<=" = Lte
stringToBinaryOp ">=" = Gte
stringToBinaryOp "==" = Eq
stringToBinaryOp "!=" = Neq
stringToBinaryOp "&&" = And
stringToBinaryOp "||" = Or
stringToBinaryOp _ = error "Unknown binary operator"

------------------- unary operators  -------------------

assign :: Parser Token Term
assign = [Let name expr | name <- ident, _ <- symbol "=", expr <- term]

-- We can use monad comprehensions (GHC extension) to make parsers more concise
minus :: Parser Token Term
minus = [UnaryOps Neg e | _ <- symbol "-", e <- unaryExp]

num :: Parser Token Term
num = do
  n <- satisfy $ \case
    Num n -> Just n
    _ -> Nothing
  return $ Literal n

string :: Parser Token Term
string = do
  s <- satisfy $ \case
    StringLiteralLexed s -> Just s
    _ -> Nothing
  return $ StringLiteral s

bool :: Parser Token Term
bool = do
  b <- satisfy $ \case
    Keyword "true" -> Just True
    Keyword "false" -> Just False
    _ -> Nothing
  return $ BoolLit b

tuple :: Parser Token Term
tuple = do
  _ <- symbol "["
  elems <- rptDropSep term (symbol ",")
  _ <- symbol "]"
  return $ TupleTerm elems

dictionary :: Parser Token Term
dictionary = do
  _ <- symbol "#"
  _ <- symbol "["
  _ <- symbol "]"
  return $ NewDictionary

parens :: Parser Token Term
parens = [t | _ <- symbol "(", t <- term, _ <- symbol ")"]

funDef :: Parser Token Term
funDef = do
  _ <- keyword "fun"
  name <- ident
  _ <- symbol "("
  params <- rptDropSep ident (symbol ",")
  _ <- symbol ")"
  body <- term
  return $ Let name (Fun params body)

varRef :: Parser Token Term
varRef = Var <$> ident

block :: Parser Token Term
block = do
  _ <- token $ Symbol "{"
  ts <- rpt term
  _ <- token $ Symbol "}"
  return $ case ts of
    [] -> Skip
    [t] -> t
    _ -> foldl1 Seq ts

ifExpr :: Parser Token Term
ifExpr = do
  _ <- keyword "if"
  cond <- term
  thenTerm <- term
  elseTerm <- opt $ keyword "else" >> term
  return $ If cond thenTerm (fromMaybe Skip elseTerm)

varDef :: Parser Token Term
varDef = do
  _ <- keyword "var"
  name <- ident
  expr <- opt $ symbol "=" >> term
  return $ case expr of
    Nothing -> Let name (Literal 0)
    Just e -> Let name e

whileTerm :: Parser Token Term
whileTerm = do
  _ <- keyword "while"
  cond <- term
  body <- term
  return $ While cond body

bracketSet :: Parser Token Term
bracketSet = do
  name <- ident
  _ <- symbol "["
  index <- term
  _ <- symbol "]"
  _ <- symbol "="
  value <- term
  return $ SetBracket name index value

bracketAccess :: Parser Token Term
bracketAccess = do
  tupleName <- varRef
  _ <- symbol "["
  index <- term
  _ <- symbol "]"
  return $ AccessBracket tupleName index

tryCatch :: Parser Token Term
tryCatch = do
  _ <- keyword "try"
  tryBranch <- term
  _ <- keyword "catch"
  errorType <- ident
  catchBranch <- term
  case errorType of
    ("Any") -> return $ Try tryBranch (Any) catchBranch
    ("Arithmetic") -> return $ Try tryBranch (Specific Arithmetic) catchBranch
    ("Type") -> return $ Try tryBranch (Specific Type) catchBranch
    ("Input") -> return $ Try tryBranch (Specific Input) catchBranch
    ("VariableNotFound") -> return $ Try tryBranch (Specific VariableNotFound) catchBranch
    ("Arguments") -> return $ Try tryBranch (Specific Arguments) catchBranch
    _ -> error "Invalid Error Type Provided"

funCall :: Parser Token Term
funCall = do
  name <- ident
  _ <- symbol "("
  args <- rptDropSep term (symbol ",")
  _ <- symbol ")"
  return $ ApplyFun (Var name) args

printStmt :: Parser Token Term
printStmt = do
  _ <- keyword "print"
  expr <- term
  return $ Write expr

unaryExp :: Parser Token Term
unaryExp = oneof [assign, ifExpr, block, funDef, minus, num, string, bool, tuple, dictionary, bracketSet, bracketAccess, tryCatch, parens, varDef, funCall, varRef, whileTerm, printStmt]

----------- prog ----------

prog :: Parser Token Term
prog = do
  ts <- rpt term
  return $ case ts of
    [] -> Skip
    [t] -> t
    _ -> foldl1 Seq ts

-- since we don't have a block constructor, this was a temporary fix

----------- parse ----------

parse :: [Char] -> Parser Token a -> Result (a, [Token])
parse input p =
  let tokens = lexer input
   in runStateT p tokens
