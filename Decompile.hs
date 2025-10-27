module Decompile (decompile) where

import Data.Functor.Foldable (cata)
import Data.List (intercalate)
import Sprintf (sprintf)
import Term (Term, TermF (..))

decompile :: Term -> String
decompile = cata go
  where
    go :: TermF String -> String
    go (IfF cond thenBranch elseBranch) =
      sprintf "If (%s) (%s) (%s)" [cond, thenBranch, elseBranch]
    go (TryF tryBlock errKindOrAny catchBlock) =
      sprintf "Try (%s) (%s) (%s)" [tryBlock, show errKindOrAny, catchBlock]
    go (LiteralF n) =
      show n
    go (StringLiteralF s) =
      show s
    go (ReadF s) =
      sprintf "Read %s" [show s]
    go (SeqF t1 t2) =
      sprintf "(%s) ; (%s)" [t1, t2]
    go SkipF =
      "Skip"
    go (BinaryOpsF op t1 t2) =
      sprintf "(%s %s %s)" [t1, show op, t2]
    go (UnaryOpsF op t) =
      sprintf "%s(%s)" [show op, t]
    go (VarF r) =
      sprintf "Var %s" [r]
    go (OnlyStrF s) =
      sprintf "OnlyStr %s" [show s]
    go (BracketF t1 t2) =
      sprintf "Bracket (%s) (%s)" [t1, t2]
    go (WhileF cond body) =
      sprintf "While (%s) (%s)" [cond, body]
    go (ForF var start end body) =
      sprintf "For %s (%s) (%s) (%s)" [show var, start, end, body]
    go (WriteF t) =
      sprintf "Write (%s)" [t]
    go (BoolLitF b) =
      show b
    go (TupleTermF terms) =
      sprintf "TupleTerm [%s]" [intercalate ", " terms]
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
    go (PreIncrementF varName) =
      sprintf "PreIncrement %s" [show varName]
    go (PreDecrementF varName) =
      sprintf "PreDecrement %s" [show varName]
    go (PostIncrementF varName) =
      sprintf "PostIncrement %s" [show varName]
    go (PostDecrementF varName) =
      sprintf "PostDecrement %s" [show varName]
    go BreakSignalF =
      "BreakSignal"
    go ContinueSignalF =
      "ContinueSignal"
    go (LetF r t) =
      sprintf "Let (%s) (%s)" [r, t]
