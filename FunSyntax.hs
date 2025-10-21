{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module FunSyntax (parse, prog, term, Term (Let, BinaryOps, Seq, Skip, UnaryOps, Var, While, Write, BoolLit, Literal, StringLiteral, Fun, ApplyFun)) where

import qualified Control.Monad as M
import Control.Monad.State.Lazy (runStateT)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import FunLexer (Token (Ident, Keyword, Num, StringLiteralLexed, Symbol), lexer)
import ParserCombinators (Parser, Result, oneof, opt, rpt, rptDropSep, satisfy, token)
import Term (BinaryOp (..), ErrorKind (..), ErrorKindOrAny (..), Ref (..), Term (..), UnaryOp (..))

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

-- convert list of statements to seq chain
blockToSeq :: [Term] -> Term
blockToSeq [] = Skip
blockToSeq [t] = t
blockToSeq (t:ts) = Seq t (blockToSeq ts)

----------
-- term --
----------

term :: Parser Token Term
term = binaryExp precedence

------------------- binary operators (left associative) -------------------

-- precedence levels, from lowest to highest
precedence :: [S.Set String]
precedence = map S.fromList [["||"], ["^"], ["&&"], ["==", "!="], ["<", ">", "<=", ">="], ["+", "-"], ["*", "/", "%"], ["**"]]

binaryExp :: [S.Set String] -> Parser Token Term
binaryExp [] = unaryExp
binaryExp (ops : rest) = do
  -- lhs
  lhs <- binaryExp rest

  -- find the longest sequence of (op, subexpression) at this precedence level
  -- then combine them left to right
  rhss <- rpt $ do
    op <- checkSymbol (`S.member` ops)
    rhs <- binaryExp rest
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
stringToBinaryOp "**" = Pow
stringToBinaryOp "^" = Xor
stringToBinaryOp _ = error "Unknown binary operator"

------------------- unary operators  -------------------

assign :: Parser Token Term
assign = [Let (OnlyStr name) expr | name <- ident, _ <- symbol "=", expr <- term]

-- We can use monad comprehensions (GHC extension) to make parsers more concise
minus :: Parser Token Term
minus = [UnaryOps Neg e | _ <- symbol "-", e <- unaryExp]

bitnot :: Parser Token Term
bitnot = [UnaryOps BitNot e | _ <- symbol "~", e <- unaryExp]

preIncrement :: Parser Token Term
preIncrement = [PreIncrement var | _ <- symbol "++", var <- ident]

preDecrement :: Parser Token Term
preDecrement = [PreDecrement var | _ <- symbol "--", var <- ident]

postIncrement :: Parser Token Term
postIncrement = [PostIncrement var | var <- ident, _ <- symbol "++"]

postDecrement :: Parser Token Term
postDecrement = [PostDecrement var | var <- ident, _ <- symbol "--"]

num :: Parser Token Term
num = [Literal n |
    n <- satisfy $ \case
        Num n -> Just n
        _ -> Nothing]
                          
string :: Parser Token Term
string = [StringLiteral s |
    s <- satisfy $ \case
        StringLiteralLexed s -> Just s
        _ -> Nothing]

bool :: Parser Token Term
bool = [BoolLit b | b <- oneof [keyword "true" >> return True, keyword "false" >> return False]]

tuple :: Parser Token Term
tuple = [TupleTerm elems | _ <- symbol "[", elems <- rptDropSep term (symbol ","), _ <- symbol "]"]

dictionary :: Parser Token Term
dictionary = [NewDictionary | _ <- symbol "#", _ <- symbol "[", _ <- symbol "]"]

parens :: Parser Token Term
parens = [t | _ <- symbol "(", t <- term, _ <- symbol ")"]

funDef :: Parser Token Term
funDef = [Let (OnlyStr name) (Fun params body) |
    _ <- keyword "fun",
    name <- ident,
    _ <- symbol "(",
    params <- rptDropSep ident (symbol ","),
    _ <- symbol ")",
    body <- term
    ]

varRef :: Parser Token Term
varRef = Var . OnlyStr <$> ident

block :: Parser Token Term
block = [blockToSeq ts | _ <- token $ Symbol "{", ts <- rpt term, _ <- token $ Symbol "}"]

ifExpr :: Parser Token Term
ifExpr = [If cond thenTerm (fromMaybe Skip elseTerm) |
    _ <- keyword "if",
    cond <- term,
    thenTerm <- term,
    elseTerm <- opt $ keyword "else" >> term
    ]

varDef :: Parser Token Term
varDef = [Let (OnlyStr name) (fromMaybe (Literal 0) expr) |
    _ <- keyword "var",
    name <- ident,
    expr <- opt $ symbol "=" >> term
    ]

whileTerm :: Parser Token Term
whileTerm = [While cond body | _ <- keyword "while", cond <- term, body <- term]

inBrackets :: Parser Token v -> Parser Token v
inBrackets p = [tok | _ <- symbol "[", tok <- p, _ <- symbol "]"]

bracketSet :: Parser Token Term
bracketSet = [Let (Bracket (OnlyStr name) index) value |
    name <- ident,
    index <- inBrackets term,
    _ <- symbol "=",
    value <- term
    ]

bracketAccess :: Parser Token Term
bracketAccess = [Var (Bracket (OnlyStr name) index) |
    name <- ident,
    index <- inBrackets term
    ]

tryCatch :: Parser Token Term
tryCatch = [Try tryBranch (errorType err) catchBranch |
      _ <- keyword "try",
      tryBranch <- term,
      _ <- keyword "catch",
      err <- ident,
      catchBranch <- term
      ]
      where errorType err = case err of
              "Any" -> (Any)
              "Arithmetic" -> (Specific Arithmetic)
              "Type" -> (Specific Type)
              "Input" -> (Specific Input)
              "VariableNotFound" -> (Specific VariableNotFound)
              "Arguments" -> (Specific Arguments)
              _ -> error "Invalid Error Type Provided"

funCall :: Parser Token Term
funCall = [ApplyFun (Var (OnlyStr name)) args |
    name <- ident,
    _ <- symbol "(",
    args <- rptDropSep term (symbol ","),
    _ <- symbol ")"
    ]

printStmt :: Parser Token Term
printStmt = [Write expr |
    _ <- keyword "print",
    expr <- term
    ]

unaryExp :: Parser Token Term
unaryExp = oneof [assign, ifExpr, block, funDef, minus, bitnot, preIncrement, preDecrement, num, string, bool, tuple, dictionary, bracketSet, bracketAccess, tryCatch, parens, varDef, funCall, postIncrement, postDecrement, varRef, whileTerm, printStmt]

----------- prog ----------

prog :: Parser Token Term
prog = blockToSeq <$> rpt term

----------- parse ----------

parse :: [Char] -> Parser Token a -> Result (a, [Token])
parse input p =
  let tokens = lexer input
   in runStateT p tokens
