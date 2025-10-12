-- Copyright 2025 Ahmed Gheith
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module FunSyntax(parse, prog, term, Term(Assign, BinaryOp, Block, Call, Const, FunDef, IfThenElse, Negate, VarDef, VarRef, While)) where

import Control.Monad.State.Lazy (runStateT)

-- import Debug.Trace (trace)

import FunLexer (lexer, Token(Ident, Num, Keyword, Symbol))
import ParserCombinators (oneof, opt, Parser, Result, rpt, rptDropSep, satisfy, token)
import qualified Data.Set as S

data Term =
    Assign String Term
    | BinaryOp String Term Term -- String just identifies the type of binary operation
    | Block [Term]
    | Call Term [Term]
    | Const Integer
    | FunDef String [String] Term
    | IfThenElse Term Term (Maybe Term) -- The (Maybe Term) here is to specify whether the if has an else statement
    | Negate Term
    | VarDef String (Maybe Term) -- The (Maybe Term) is a way to wrap initializing and not initializing a var
    -- In one definition.
    | VarRef String
    | While Term Term
    -- | more term constructors
    deriving (Show, Eq)

-- succeed if the next token is the given symbol
symbol :: String -> Parser Token ()
-- using explicit bind
symbol s = token (Symbol s) >>= \_ -> return ()

-- succeed if the next token is the given keyword
keyword :: String -> Parser Token ()
-- using do notation (syntactic sugar for >>=/bind)
keyword k = do
    _ <- token $ Keyword k
    return ()

-- identifier
ident :: Parser Token String
ident = satisfy $ \case -- Lambda case seems to just be a way of making a function that's solely a case statement
    Ident name -> Just name
    _ -> Nothing -- So it just throws an error if it can't get an Ident name

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
binaryExp (ops:rest) = do
    -- lhs
    lhs <- binaryExp rest

    -- find the longest sequence of (op, subexpression) at this precedence level
    -- then combine them left to right
    rhss <- rpt $ do
        op <- checkSymbol (`S.member` ops)
        rhs <- term
        return (op, rhs)

    -- combine results left to right
    -- this seems to be like reduce in Python
    return $ foldl (\acc (op, rhs) -> BinaryOp op acc rhs) lhs rhss

------------------- unary operators  -------------------

assign :: Parser Token Term
assign = [ Assign name expr | name <- ident, _ <- symbol "=", expr <- term ]

-- We can use monad comprehensions (GHC extension) to make parsers more concise
minus :: Parser Token Term
minus = [ Negate e | _ <- symbol "-", e <- unaryExp ] -- as a num is a unaryExp

num :: Parser Token Term
num = do
    n <- satisfy $ \case
        Num n -> Just n
        _ -> Nothing
    return $ Const n

parans :: Parser Token Term
parans = [t | _ <- symbol "(", t <- term, _ <- symbol ")"]

funDef :: Parser Token Term
funDef = [ FunDef name params body | _ <- keyword "fun",
    name <- ident,
    _ <- symbol "(",
    params <- rptDropSep ident (symbol ","), -- In this case, the list of as would be idents
    _ <- symbol ")",
    body <- term
    ]

varRef :: Parser Token Term
varRef = VarRef <$> ident

block :: Parser Token Term
block = do
    _ <- token $ Symbol "{"
    ts <- rpt term -- 
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
unaryExp = oneof [assign, ifExpr, block, funDef, minus, num, parans, varDef, varRef, whileTerm]

----------- prog ----------

prog :: Parser Token Term
prog = Block <$> rpt term -- Just apply the Block term functor unto the result of rpt term
-- This is a program

----------- parse ----------

parse :: [Char] -> Parser Token a -> Result (a, [Token])
parse input p = let
    tokens = lexer input in
    runStateT p tokens

