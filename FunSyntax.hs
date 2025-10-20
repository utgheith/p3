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
import Term (BinaryOp (..), Term (..), UnaryOp (..))

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

-- namespace: parse a namespace and qualify all names inside it
namespaceDef :: Parser Token Term
namespaceDef = do
  _ <- keyword "namespace"
  name <- ident
  body <- term
  return $ qualify name body

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

tupleSet :: Parser Token Term
tupleSet = do
  name <- ident
  _ <- symbol "["
  index <- term
  _ <- symbol "]"
  _ <- symbol "="
  value <- term
  return $ SetBracket name index value

tupleAccess :: Parser Token Term
tupleAccess = do
  tupleName <- varRef
  _ <- symbol "["
  index <- term
  _ <- symbol "]"
  return $ AccessBracket tupleName index

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
unaryExp = oneof [assign, ifExpr, block, funDef, namespaceDef, minus, num, string, bool, tuple, tupleSet, tupleAccess, parens, varDef, funCall, varRef, whileTerm, printStmt]

-- Qualify names inside a term with a namespace prefix.
-- We thread a "whitelist" (set of names) representing names defined lexically
-- before a given point.

qualify :: String -> Term -> Term
qualify ns t = fst $ qualifyTop S.empty t
  where
    p s = ns ++ "::" ++ s

    qualifyName :: S.Set String -> String -> String
    qualifyName wl s
      | s `S.member` wl = p s
      | otherwise = s

    -- qualifyTop exposes definitions occurring at top-level sequencing
    -- so that later terms in a Seq can see earlier definitions.
    qualifyTop :: S.Set String -> Term -> (Term, S.Set String)
    qualifyTop wl curr_term = case curr_term of
      Seq a b ->
        let (a', defsA) = qualifyTop wl a
            wl' = S.union wl defsA
            (b', defsB) = qualifyTop wl' b
         in (Seq a' b', S.union defsA defsB)
      Let name expr ->
        case expr of
          Fun params body ->
            let (body', _) = qualifyGeneral (S.union wl (S.fromList params)) body
             in (Let (p name) (Fun params body'), S.singleton name)
          _ ->
            let (expr', _) = qualifyGeneral wl expr
             in (Let (p name) expr', S.singleton name)
      If c t1 t2 ->
        let (c', _) = qualifyGeneral wl c
            (t1', _) = qualifyGeneral wl t1
            (t2', _) = qualifyGeneral wl t2
         in (If c' t1' t2', S.empty)
      Literal n -> (Literal n, S.empty)
      StringLiteral s -> (StringLiteral s, S.empty)
      Read s -> (Read (qualifyName wl s), S.empty)
      Skip -> (Skip, S.empty)
      BinaryOps op l r ->
        let (l', _) = qualifyGeneral wl l
            (r', _) = qualifyGeneral wl r
         in (BinaryOps op l' r', S.empty)
      UnaryOps op x -> let (x', _) = qualifyGeneral wl x in (UnaryOps op x', S.empty)
      Var s -> (Var (qualifyName wl s), S.empty)
      While c b ->
        let (c', _) = qualifyGeneral wl c
            (b', _) = qualifyGeneral wl b
         in (While c' b', S.empty)
      Write x -> let (x', _) = qualifyGeneral wl x in (Write x', S.empty)
      BoolLit b -> (BoolLit b, S.empty)
      TupleTerm elems -> (TupleTerm (map (fst . qualifyGeneral wl) elems), S.empty)
      NewDictionary -> (NewDictionary, S.empty)
      AccessBracket t1 t2 ->
        let (t1', _) = qualifyGeneral wl t1
            (t2', _) = qualifyGeneral wl t2
         in (AccessBracket t1' t2', S.empty)
      SetBracket name idx val ->
        let (idx', _) = qualifyGeneral wl idx
            (val', _) = qualifyGeneral wl val
            name' = qualifyName wl name
         in (SetBracket name' idx' val', S.empty)
      Fun params body ->
        let (body', _) = qualifyGeneral (S.union wl (S.fromList params)) body
         in (Fun params body', S.empty)
      ApplyFun f args ->
        let (f', _) = qualifyGeneral wl f
            args' = map (fst . qualifyGeneral wl) args
         in (ApplyFun f' args', S.empty)
      BreakSignal -> (BreakSignal, S.empty)
      ContinueSignal -> (ContinueSignal, S.empty)

    -- qualifyGeneral does NOT expose names defined inside the term to the
    -- outside; it's used for nested contexts.
    qualifyGeneral :: S.Set String -> Term -> (Term, S.Set String)
    qualifyGeneral wl curr_term = case curr_term of
      Seq a b ->
        let (a', defsA) = qualifyTop wl a
            (b', _) = qualifyGeneral (S.union wl defsA) b
         in (Seq a' b', S.empty)
      Let name expr ->
        case expr of
          Fun params body ->
            let (body', _) = qualifyGeneral (S.union wl (S.fromList params)) body
             in (Let (p name) (Fun params body'), S.empty)
          _ ->
            let (expr', _) = qualifyGeneral wl expr
             in (Let (p name) expr', S.empty)
      If c t1 t2 ->
        let (c', _) = qualifyGeneral wl c
            (t1', _) = qualifyGeneral wl t1
            (t2', _) = qualifyGeneral wl t2
         in (If c' t1' t2', S.empty)
      Literal n -> (Literal n, S.empty)
      StringLiteral s -> (StringLiteral s, S.empty)
      Read s -> (Read (qualifyName wl s), S.empty)
      Skip -> (Skip, S.empty)
      BinaryOps op l r ->
        let (l', _) = qualifyGeneral wl l
            (r', _) = qualifyGeneral wl r
         in (BinaryOps op l' r', S.empty)
      UnaryOps op x -> let (x', _) = qualifyGeneral wl x in (UnaryOps op x', S.empty)
      Var s -> (Var (qualifyName wl s), S.empty)
      While c b ->
        let (c', _) = qualifyGeneral wl c
            (b', _) = qualifyGeneral wl b
         in (While c' b', S.empty)
      Write x -> let (x', _) = qualifyGeneral wl x in (Write x', S.empty)
      BoolLit b -> (BoolLit b, S.empty)
      TupleTerm elems -> (TupleTerm (map (fst . qualifyGeneral wl) elems), S.empty)
      NewDictionary -> (NewDictionary, S.empty)
      AccessBracket t1 t2 ->
        let (t1', _) = qualifyGeneral wl t1
            (t2', _) = qualifyGeneral wl t2
         in (AccessBracket t1' t2', S.empty)
      SetBracket name idx val ->
        let (idx', _) = qualifyGeneral wl idx
            (val', _) = qualifyGeneral wl val
            name' = qualifyName wl name
         in (SetBracket name' idx' val', S.empty)
      Fun params body ->
        let (body', _) = qualifyGeneral (S.union wl (S.fromList params)) body
         in (Fun params body', S.empty)
      ApplyFun f args ->
        let (f', _) = qualifyGeneral wl f
            args' = map (fst . qualifyGeneral wl) args
         in (ApplyFun f' args', S.empty)
      BreakSignal -> (BreakSignal, S.empty)
      ContinueSignal -> (ContinueSignal, S.empty)

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
