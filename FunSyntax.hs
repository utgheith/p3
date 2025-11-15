{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module FunSyntax (parse, prog, term) where

import qualified Control.Monad as M
import Control.Monad.State.Lazy (runStateT)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import FunLexer (Token (Ident, Keyword, Num, StringLiteralLexed, Symbol), lexer)
import ParserCombinators (Parser, between, oneof, opt, rpt, rptDropSep, satisfy, token, (<|>))
import Result (Result (..))
import Term (BinaryOp (..), ErrorKind (..), ErrorKindOrAny (..), Term (..), UnaryOp (..))
import TypeSignature (TypeSignature (..), TypedName)

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

typeSignature :: Parser Token TypeSignature
typeSignature =
  oneof
    [ [TUnknown | _ <- keyword "auto"],
      [TInt | _ <- keyword "int"],
      [TBool | _ <- keyword "bool"],
      [TString | _ <- keyword "string"],
      [TUnit | _ <- keyword "unit"],
      [ TFun [] retType
        | _ <- keyword "fun",
          _ <- symbol "(",
          _ <- symbol ")",
          _ <- symbol "->",
          retType <- typeSignature
      ],
      [ TFun paramTypes retType
        | _ <- keyword "fun",
          paramTypes <- between (symbol "(") (symbol ")") (rptDropSep typeSignature (symbol ",")),
          _ <- symbol "->",
          retType <- typeSignature
      ],
      [Poly name | name <- ident],
      [TDictionary valType | valType <- between (symbol "[") (symbol "]") typeSignature],
      [TTuple types | types <- between (symbol "(") (symbol ")") (rptDropSep typeSignature (symbol ","))],
      [TSum types | types <- rptDropSep typeSignature (symbol "|")]
    ]

optTypeSignature :: Parser Token TypeSignature
optTypeSignature = do
  col <- opt (symbol ":")
  case col of
    Just _ -> typeSignature
    Nothing -> return TUnknown

typedIdent :: Parser Token TypedName
typedIdent = do
  name <- ident
  tSig <- optTypeSignature
  return (name, tSig)

-- symbol
checkSymbol :: (String -> Bool) -> Parser Token String
checkSymbol predicate = satisfy $ \case
  Symbol s | predicate s -> Just s
  _ -> Nothing

----------
-- term --
----------

term :: Parser Token Term
term = [t | t <- binaryExp precedence] -- [ t |
-- t <- trace "[term] looking for ternaryExp" ternaryExp
-- _ <- trace ("[term] looking for opt; after " ++ show t) (opt $ symbol ";")
-- ]

------------------- ternary operator --------------------------

-- Ternary operator has lower precedence than binary operators
-- Right-associative: a ? b : c ? d : e parses as a ? b : (c ? d : e)
-- ternaryExp :: Parser Token Term
-- ternaryExp =
--   [ If cond trueBranch falseBranch
--    | cond <- binaryExp precedence, -- Only allow binary expressions in condition
--      _ <- symbol "?",
--      trueBranch <- refassign, -- Only allow binary expressions and assignments in true branch
--      _ <- symbol ":",
--      falseBranch <- ternaryExp -- Allow ternary in false branch for right-associativity
--  ]
--    <|> refassign

------------------- assignment --------------------------

-- refassign :: Parser Token Term
-- refassign =
--  [Let ref expr | ref <- reference, _ <- symbol "=", expr <- term]
--    <|> binaryExp precedence

reference :: Parser Token Term
reference = do
  -- very similar to chainl1
  x <- OnlyStr <$> typedIdent
  rest x
  where
    rest x =
      ( do
          _ <- symbol "["
          idx <- term
          _ <- symbol "]"
          rest (Bracket x idx)
      )
        <|> return x

------------------- binary operators (left associative) -------------------

-- precedence levels, from lowest to highest
precedence :: [S.Set String]
precedence = map S.fromList [["||"], ["^"], ["&&"], ["==", "!="], ["<", ">", "<=", ">="], ["+", "-"], ["*", "/", "%"], ["**"]]

binaryExp :: [S.Set String] -> Parser Token Term
binaryExp [] = methodCall
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

------------------- method calls -----------------------

methodCall :: Parser Token Term
methodCall =
  [ ApplyFun (Var (OnlyStr name)) (caller : args)
    | caller <- varRef <|> parens,
      _ <- symbol ".",
      name <- typedIdent,
      _ <- symbol "(",
      args <- rptDropSep term (symbol ","),
      _ <- symbol ")"
  ]
    <|> unaryExp

------------------- unary operators  -------------------

-- We can use monad comprehensions (GHC extension) to make parsers more concise
minus :: Parser Token Term
minus = [UnaryOps Neg e | _ <- symbol "-", e <- unaryExp]

bitnot :: Parser Token Term
bitnot = [UnaryOps BitNot e | _ <- symbol "~", e <- unaryExp]

preIncrement :: Parser Token Term
preIncrement = [PreIncrement var | _ <- symbol "++", var <- typedIdent]

preDecrement :: Parser Token Term
preDecrement = [PreDecrement var | _ <- symbol "--", var <- typedIdent]

postIncrement :: Parser Token Term
postIncrement = [PostIncrement var | var <- typedIdent, _ <- symbol "++"]

postDecrement :: Parser Token Term
postDecrement = [PostDecrement var | var <- typedIdent, _ <- symbol "--"]

num :: Parser Token Term
num =
  [ Literal n
    | n <- satisfy $ \case
        Num n -> Just n
        _ -> Nothing
  ]

string :: Parser Token Term
string =
  [ StringLiteral s
    | s <- satisfy $ \case
        StringLiteralLexed s -> Just s
        _ -> Nothing
  ]

bool :: Parser Token Term
bool = [BoolLit b | b <- oneof [keyword "true" >> return True, keyword "false" >> return False]]

tuple :: Parser Token Term
tuple = [TupleTerm elems | _ <- symbol "[", elems <- rptDropSep term (symbol ","), _ <- symbol "]"]

dictionary :: Parser Token Term
dictionary = [NewDictionary | _ <- symbol "#", _ <- symbol "[", _ <- symbol "]"]

parens :: Parser Token Term
parens = [t | _ <- symbol "(", t <- term, _ <- symbol ")"]

funDef :: Parser Token Term
funDef =
  -- a named function

  [ Let (OnlyStr (name, TFun (snd <$> params) TUnknown)) (Fun params body)
    | _ <- keyword "fun",
      name <- ident,
      _ <- symbol "(",
      params <- rptDropSep typedIdent (symbol ","),
      _ <- symbol ")",
      body <- term
  ]
    <|>
    -- an anonymous function

    [ Fun params body
      | _ <- keyword "fun",
        _ <- symbol "(",
        params <- rptDropSep typedIdent (symbol ","),
        _ <- symbol ")",
        body <- term
    ]

varRef :: Parser Token Term
varRef = Var <$> reference

block :: Parser Token Term
block = [Block (seqToTerm ts) | _ <- token $ Symbol "{", ts <- rpt term, _ <- token $ Symbol "}"]

varDef :: Parser Token Term
varDef =
  [ Let (OnlyStr name) (fromMaybe (Literal 0) expr)
    | _ <- keyword "var",
      name <- typedIdent,
      expr <- opt $ symbol "=" >> term
  ]

metric :: Parser Token Term
metric = [t | _ <- keyword "metric", t <- term]

invariant :: Parser Token Term
invariant = [t | _ <- keyword "invariant", t <- term]

ifStmt :: Parser Token Term
ifStmt =
  [ If cond thenTerm (fromMaybe Skip elseTerm)
    | _ <- keyword "if",
      cond <- term,
      thenTerm <- term,
      elseTerm <- opt $ keyword "else" >> term
  ]

whileStmt :: Parser Token Term
whileStmt =
  [ While cond body m i
    | _ <- keyword "while",
      cond <- term,
      m <- opt metric,
      i <- opt invariant,
      body <- term
  ]

forStmt :: Parser Token Term
forStmt =
  [ For var start end body m i
    | _ <- keyword "for",
      var <- typedIdent,
      start <- term,
      end <- term,
      m <- opt metric,
      i <- opt invariant,
      body <- term
  ]

tryCatch :: Parser Token Term
tryCatch =
  [ Try tryBranch (errorType err) catchBranch
    | _ <- keyword "try",
      tryBranch <- term,
      _ <- keyword "catch",
      err <- ident,
      catchBranch <- term
  ]
  where
    errorType err = case err of
      "Any" -> Any
      "Arithmetic" -> Specific Arithmetic
      "Type" -> Specific Type
      "Input" -> Specific Input
      "VariableNotFound" -> Specific VariableNotFound
      "Arguments" -> Specific Arguments
      _ -> error "Invalid Error Type Provided"

funCall :: Parser Token Term
funCall =
  [ ApplyFun (Var (OnlyStr (name, TUnknown))) args
    | _ <- keyword "call",
      name <- typedIdent,
      _ <- symbol "(",
      args <- rptDropSep term (symbol ","),
      _ <- symbol ")"
  ]

writeStmt :: Parser Token Term
writeStmt =
  [ Write expr
    | _ <- keyword "write",
      expr <- term
  ]

assertStmt :: Parser Token Term
assertStmt = [Assert expr | _ <- keyword "assert", expr <- term]

breakStmt :: Parser Token Term
breakStmt = [BreakSignal | _ <- keyword "break"]

continueStmt :: Parser Token Term
continueStmt = [ContinueSignal | _ <- keyword "continue"]

letStmt :: Parser Token Term
letStmt =
  [ Let lhs expr
    | _ <- keyword "let",
      lhs <- reference,
      _ <- symbol "=",
      expr <- term
  ]

unaryExp :: Parser Token Term
unaryExp =
  oneof
    [ -- terms that start with keywords go first
      letStmt,
      tryCatch,
      assertStmt,
      breakStmt,
      continueStmt,
      ifStmt,
      whileStmt,
      forStmt,
      writeStmt,
      block,
      funDef,
      funCall,
      -- terms that start with symbols go next
      minus,
      bitnot,
      preIncrement,
      preDecrement,
      dictionary,
      -- longer terms first
      tuple,
      parens,
      -- literals
      num,
      string,
      bool,
      varDef,
      postIncrement,
      postDecrement,
      varRef
    ]

----------- prog ----------

seqToTerm :: [Term] -> Term
seqToTerm [] = Skip
seqToTerm [t] = t
seqToTerm (t : ts) = Seq t (seqToTerm ts)

prog :: Parser Token Term
prog = seqToTerm <$> rpt term

----------- parse ----------

parse :: String -> Parser Token a -> Result String (a, [Token])
parse input p =
  let tokens = lexer input
   in runStateT p tokens
