module Decompile (decompile) where

import Data.Functor.Foldable (cata)
import Data.List (intercalate)
import Sprintf (sprintf)
import Term (BinaryOp (..), Term, TermF (..))

disp :: String -> Maybe String -> String
disp _ Nothing = ""
disp label (Just t) = sprintf " %s (%s) " [label, t]

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

decompile :: Term -> String
decompile = cata go
  where
    go :: TermF String -> String
    go (IfF cond thenBranch elseBranch) =
      sprintf "if (%s) {%s} else {%s}" [cond, thenBranch, elseBranch]
    go (TryF tryBlock errKindOrAny catchBlock) =
      sprintf "try {%s} catch (%s) {%s}" [tryBlock, show errKindOrAny, catchBlock]
    go (LiteralF n) =
      show n
    go (StringLiteralF s) =
      show s
    go (ReadF (v, _)) =
      sprintf "read (%s)" [v]
    go (SeqF t1 t2) =
      sprintf "(%s) ; (%s)" [t1, t2]
    go SkipF =
      "skip"
    go (BinaryOpsF op t1 t2) =
      sprintf "(%s %s %s)" [t1, dispBinaryOp op, t2]
    go (UnaryOpsF op t) =
      sprintf "%s(%s)" [show op, t]
    go (VarF r) =
      r
    go (OnlyStrF (name, _)) =
      sprintf "%s" [name]
    go (BracketF t1 t2) =
      sprintf "%s(%s)" [t1, t2]
    go (WhileF cond body metric invariant) =
      sprintf "while (%s) %s %s (%s)" [cond, disp "metric" metric, disp "invariant" invariant, body]
    go (ForF (var, _) start end body metric invariant) =
      sprintf "for %s=(%s);(%s) %s %s {%s}" [var, start, end, disp "metric" metric, disp "invariant" invariant, body]
    go (WriteF t) =
      sprintf "write (%s)" [t]
    go (BoolLitF b) =
      if b then "true" else "false"
    go (TupleTermF terms) =
      sprintf "(%s)" [intercalate ", " terms]
    go NewDictionaryF =
      "NewDictionary"
    go (RetrieveF dict index) =
      sprintf "Retrieve (%s) (%s)" [dict, index]
    go (MergeF current index value) =
      sprintf "Merge (%s) (%s) (%s)" [current, index, value]
    go (FunF args body) =
      sprintf "Fun [%s] (%s)" [intercalate ", " (show <$> args), body]
    go (ApplyFunF fun args) =
      sprintf "ApplyFun (%s) [%s]" [fun, intercalate ", " args]
    go (PreIncrementF (varName, _)) =
      sprintf "++ %s" [varName]
    go (PreDecrementF (varName, _)) =
      sprintf "-- %s" [varName]
    go (PostIncrementF (varName, _)) =
      sprintf "%s++" [varName]
    go (PostDecrementF (varName, _)) =
      sprintf "%s--" [varName]
    go BreakSignalF =
      "BreakSignal"
    go ContinueSignalF =
      "ContinueSignal"
    go (LetF r t) =
      sprintf "%s = %s;" [r, t]
    go (AssertF cond) =
      sprintf "assert(%s)" [cond]
