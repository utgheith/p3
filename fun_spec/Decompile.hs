module Decompile (decompile) where

import Data.Char (isSpace)
import Data.Functor.Foldable (cata)
import Data.List (intercalate)
import Sprintf (sprintf)
import Term (BinaryOp (..), ErrorKindOrAny (..), Term, TermF (..))
import TypeSignature (TypeSignature (..))

disp :: String -> Maybe String -> String
disp _ Nothing = ""
disp label (Just t) = sprintf " %s %s" [label, t]

dispBinaryOp :: Term.BinaryOp -> String
dispBinaryOp Add = "+"
dispBinaryOp Sub = "-"
dispBinaryOp Mul = "*"
dispBinaryOp Div = "/"
dispBinaryOp Mod = "%"
dispBinaryOp And = "&&"
dispBinaryOp Or = "||"
dispBinaryOp Lt = "<"
dispBinaryOp Gt = ">"
dispBinaryOp Lte = "<="
dispBinaryOp Gte = ">="
dispBinaryOp Eq = "=="
dispBinaryOp Neq = "!="
dispBinaryOp Pow = "**"
dispBinaryOp Xor = "^"

dispTypeSig :: TypeSignature -> String
dispTypeSig TUnit = "unit"
dispTypeSig TInt = "int"
dispTypeSig TBool = "bool"
dispTypeSig TString = "str"
dispTypeSig (TTuple types) = "(" ++ intercalate ", " (map dispTypeSig types) ++ ")"
dispTypeSig (TFun paramTypes retType) =
  "fun(" ++ intercalate ", " (map dispTypeSig paramTypes) ++ ") -> " ++ dispTypeSig retType
dispTypeSig (TSum types) = intercalate " | " (map dispTypeSig types)
dispTypeSig (TDictionary valType) = "[" ++ dispTypeSig valType ++ "]"
dispTypeSig TUnknown = "auto"
dispTypeSig (Poly name) = name
dispTypeSig (TTypeError _) = "auto" -- treat type erros as unknown for display purposes

dispOptTypeSig :: TypeSignature -> String
dispOptTypeSig TUnknown = ""
dispOptTypeSig tSig = ": " ++ dispTypeSig tSig

dispArg :: (String, TypeSignature) -> String
dispArg (name, tSig) = sprintf "%s%s" [name, dispOptTypeSig tSig]

decompile :: Term -> String
decompile = cata go
  where
    go :: TermF String -> String
    go (IfF cond thenBranch elseBranch) =
      sprintf "if (%s) %s else %s" [cond, ensureBlock thenBranch, ensureBlock elseBranch]
    go (TryF tryBlock errKindOrAny catchBlock) =
      sprintf "try %s catch %s %s" [ensureBlock tryBlock, dispErrorKind errKindOrAny, ensureBlock catchBlock]
    go (LiteralF n) =
      show n
    go (StringLiteralF s) =
      "\"" ++ s ++ "\""
    go (ReadF (v, _)) =
      sprintf "read (%s)" [v]
    go (SeqF t1 t2) =
      sprintf "%s\n%s" [t1, t2]
    go SkipF =
      "skip"
    go (BinaryOpsF op t1 t2) =
      sprintf "(%s %s %s)" [t1, dispBinaryOp op, t2]
    go (UnaryOpsF op t) =
      sprintf "%s(%s)" [show op, t]
    go (VarF r) =
      r
    go (OnlyStrF (name, tSig)) =
      sprintf "%s%s" [name, dispOptTypeSig tSig]
    go (BracketF t1 t2) =
      sprintf "%s[%s]" [t1, t2]
    go (WhileF cond body metric invariant) =
      sprintf "while (%s)%s%s %s" [cond, disp "metric" metric, disp "invariant" invariant, body]
    go (WhileBodyF cond current original metric invariant) =
      sprintf "while_body (%s)%s%s (%s -> %s)" [cond, disp "metric" metric, disp "invariant" invariant, current, original]
    go (ForF (var, _) start end body metric invariant) =
      sprintf "for %s %s %s%s%s %s" [var, start, end, disp "metric" metric, disp "invariant" invariant, body]
    go (ScopedF body) =
      sprintf "scoped {%s}" [body]
    go (ScopedBodyF body) =
      sprintf "scoped_body {%s}" [body]
    go (WriteF t) =
      sprintf "write (%s)" [t]
    go (BoolLitF b) =
      if b then "true" else "false"
    go (TupleTermF terms) =
      sprintf "[%s]" [intercalate ", " terms]
    go NewDictionaryF =
      "#[]"
    go (RetrieveF dict index) =
      sprintf "%s[%s]" [dict, index]
    go (MergeF current index value) =
      sprintf "Merge (%s) (%s) (%s)" [current, index, value]
    go (FunF args body) =
      sprintf "fun (%s) (%s)" [intercalate ", " (dispArg <$> args), body]
    go (ApplyFunF fun args) =
      sprintf "call %s(%s)" [fun, intercalate ", " args]
    go (PreIncrementF (varName, _)) =
      sprintf "++ %s" [varName]
    go (PreDecrementF (varName, _)) =
      sprintf "-- %s" [varName]
    go (PostIncrementF (varName, _)) =
      sprintf "%s++" [varName]
    go (PostDecrementF (varName, _)) =
      sprintf "%s--" [varName]
    go BreakSignalF =
      "break"
    go ContinueSignalF =
      "continue"
    go (LetF r t) =
      sprintf "let %s = %s" [r, t]
    go (AssertF cond) =
      sprintf "assert(%s)" [cond]
    go (BlockF term) =
      sprintf "{%s}" [term]

    ensureBlock :: String -> String
    ensureBlock s =
      let trimmed = dropWhile isSpace s
       in case trimmed of
            ('{' : _) -> s
            _ -> sprintf "{%s}" [s]

dispErrorKind :: ErrorKindOrAny -> String
dispErrorKind Any = "Any"
dispErrorKind (Specific k) = show k
