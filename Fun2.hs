{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}

module Fun2 (decompile, lexer, parser, Token (..), term) where

import Control.Monad.Except (throwError)
import qualified Control.Monad.State.Lazy as SM
import Data.Char (isAlpha, isAlphaNum, isDigit)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import ParserCombinators (Parser, between, eof, opt, rpt, rptDropSep, satisfy, (<|>))
import qualified Term as T

-------------
-- Helpers --
-------------

-- Should this be in ParserCombinators?
cond :: (Show a) => (a -> Bool) -> Parser a a
cond predicate = satisfy $ \c -> if predicate c then Just c else Nothing

-- What about rpt1? Should it be in ParserCombinators? Should we generalize it to rptN?
-- Should we generalize it to be rptMinMax?
-- Ah, the beauty of combinators! They allow you to raise the level of abstraction as needed.
-- Ah, the danger of combinators! They could obscure simple things that could be done directly.
rpt1 :: (Show a) => Parser a b -> Parser a [b]
rpt1 p = [x : xs | x <- p, xs <- rpt p]

-- more combinators
memberOf :: (Show a, Ord a) => S.Set a -> Parser a a
memberOf s = cond (`S.member` s)

------------------------------------------------
-- Common settings for FunLexer and FunSyntax --
------------------------------------------------

special2set :: S.Set String
special2set = S.fromList ["<=", ">=", "==", "||", "&&"]

special1set :: S.Set Char
special1set = S.fromList "-*+/(){}=,<>!;"

spaceChars :: S.Set Char
spaceChars = S.fromList " \n\t\r"

---------------
-- The Lexer --
---------------
data Token
  = TNum Integer -- integer literals
  | TIdent String -- identifiers
  | TSpecial String -- special characters
  | TIgnore String -- whitespaces and comments
  deriving (Show, Eq)

isIgnored :: Token -> Bool
isIgnored (TIgnore _) = True
isIgnored _ = False

integer :: Parser Char Token
integer = [TNum (read ds) | ds <- rpt1 (cond isDigit)]

ident :: Parser Char Token
ident = [TIdent (c : cs) | c <- cond isAlpha, cs <- rpt (cond isAlphaNum)]

special2 :: Parser Char Token
special2 = do
  state <- SM.get
  case state of
    (c1 : c2 : rest) | S.member [c1, c2] special2set -> do
      SM.put rest
      return (TSpecial [c1, c2])
    _ -> throwError "no match"

special1 :: Parser Char Token
special1 = [TSpecial [c] | c <- memberOf special1set]

special :: Parser Char Token
special = special2 <|> special1

spaces :: Parser Char Token
spaces = [TIgnore cs | cs <- rpt1 (memberOf spaceChars)]

lineComment :: Parser Char Token
lineComment = [TIgnore ("//" ++ cs) | _ <- cond (== '/'), _ <- cond (== '/'), cs <- rpt (cond (/= '\n'))]

lexer :: Parser Char [Token]
lexer = do [ts | ts <- rpt (lineComment <|> integer <|> ident <|> special <|> spaces), _ <- eof]

----------------
-- The Parser --
----------------

str :: String -> Parser Token Token
str k =
  cond
    ( \case
        TIdent name | name == k -> True
        TSpecial s | s == k -> True
        _ -> False
    )

strs :: S.Set String -> Parser Token String
strs ks = satisfy $ \case
  TIdent name | S.member name ks -> Just name
  TSpecial s | S.member s ks -> Just s
  _ -> Nothing

---------- Unary operators -----------

unaryExp :: Parser Token T.Term
unaryExp = [t | t <- between (str "(") (str ")") term] <|> [T.UnaryOps T.Neg n | _ <- str "-", n <- unaryExp] <|> num <|> varRef

------------------- binary operators (left associative) -------------------

-- precedence levels, from lowest to highest
precedence :: [S.Set String]
precedence = [S.fromList ["+"], S.fromList ["*", "/"]]

binaryOpMap :: M.Map String T.BinaryOp
binaryOpMap =
  M.fromList
    [ ("+", T.Add),
      ("*", T.Mul),
      ("/", T.Div)
    ]

binaryExp :: [S.Set String] -> Parser Token T.Term
binaryExp [] = unaryExp
binaryExp (ops : rest) = do
  -- lhs
  lhs <- binaryExp rest

  -- find the longest sequence of (op, subexpression) at this precedence level
  -- then combine them left to right
  rhss <- rpt $ do
    op <- strs ops
    case M.lookup op binaryOpMap of
      Nothing -> throwError $ "unknown binary operator: " ++ op
      Just op' -> do
        rhs <- binaryExp rest
        return (op', rhs)

  -- combine results left to right
  return $ foldl (\acc (op, rhs) -> T.BinaryOps op acc rhs) lhs rhss

num :: Parser Token T.Term
num =
  satisfy
    ( \case
        TNum n -> Just (T.Literal n)
        _ -> Nothing
    )

varRef :: Parser Token T.Term
varRef =
  satisfy
    ( \case
        TIdent name -> Just (T.Var name)
        _ -> Nothing
    )

ifTerm :: Parser Token T.Term
ifTerm = do
  _ <- str "if"
  c <- term
  _ <- str "then"
  tThen <- term
  tElse <-
    opt
      ( do
          _ <- str "else"
          term
      )
  return $ T.If c tThen (fromMaybe T.Skip tElse)

statements :: Parser Token T.Term
statements = [pack s | s <- rptDropSep term (str ";")]
  where
    pack [] = T.Skip
    pack [x] = x
    pack (x : xs) = T.Seq x (pack xs)

block :: Parser Token T.Term
block = between (str "{") (str "}") statements

letTerm :: Parser Token T.Term
letTerm = do
  _ <- str "let"
  vr <- varRef
  let name = case vr of
        T.Var n -> n
        _ -> error "internal error: expected Var in letTerm"
  _ <- str "="
  T.Let name <$> term

term :: Parser Token T.Term
term = [T.Skip | _ <- str "skip"] <|> letTerm <|> block <|> ifTerm <|> binaryExp precedence

prog :: Parser Token T.Term
prog = statements <* eof

----------- Top-level parser ----------

parser :: [Char] -> T.Term
parser input =
  case SM.runStateT lexer input of
    Left err -> error $ "Lexing error: " ++ err
    Right (tokens, []) ->
      case SM.runStateT prog [t | t <- tokens, not (isIgnored t)] of
        Left err -> error $ "Parsing error: " ++ err
        Right (ast, []) -> ast
        Right (_, ts) -> error $ "Parsing error: unconsumed tokens: " ++ show ts
    Right (_, ts) -> error $ "Lexing error: unconsumed input: " ++ show ts

decompileBinaryOp :: T.BinaryOp -> String
decompileBinaryOp T.Add = "+"
decompileBinaryOp T.Mul = "*"
decompileBinaryOp T.Sub = "-"
decompileBinaryOp T.Div = "/"
decompileBinaryOp T.Mod = "%"
decompileBinaryOp T.Lt = "<"
decompileBinaryOp T.Gt = ">"
decompileBinaryOp T.Lte = "<="
decompileBinaryOp T.Gte = ">="
decompileBinaryOp T.Eq = "=="
decompileBinaryOp T.Neq = "!="
decompileBinaryOp T.And = "&&"
decompileBinaryOp T.Or = "||"

decompileUnaryOp :: T.UnaryOp -> String
decompileUnaryOp T.Neg = "-"
decompileUnaryOp T.Not = "!"

decompile :: T.Term -> String
decompile T.Skip = "skip"
decompile (T.Literal n) = show n
decompile (T.Var x) = x
decompile (T.Seq t1 t2) = decompile t1 ++ " ; " ++ decompile t2
decompile (T.If c tThen tElse) =
  "if " ++ decompile c ++ " then " ++ decompile tThen ++ " else " ++ decompile tElse
decompile (T.BinaryOps op t1 t2) =
  "(" ++ decompile t1 ++ " " ++ decompileBinaryOp op ++ " " ++ decompile t2 ++ ")"
decompile (T.UnaryOps op t) = "(" ++ decompileUnaryOp op ++ decompile t ++ ")"
decompile (T.Let x t) = "let " ++ x ++ " = " ++ decompile t
decompile x = "<unhandled term: " ++ show x ++ ">"
