{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Small (reduceFully) where

-- import Data.Either

import Control.Applicative (Alternative, asum)
import Control.Monad (MonadPlus)
import qualified Control.Monad.State as S
import qualified Data.Map as M
import Debug.Trace (trace)
import Machine (Env, Error, Machine (..), Result (..))
import Term (BinaryOp (..), ErrorKind (..), ErrorKindOrAny (..), Term (..), UnaryOp (..))
import Value (Value (..))

-- Helper for try-catch statement
errorShouldBeCaught :: ErrorKind -> ErrorKindOrAny -> Bool
errorShouldBeCaught _ Any = True
errorShouldBeCaught resultErrorKind (Specific catchableErrorKind) = resultErrorKind == catchableErrorKind

-- Polymorphic reduction monad: carries state + rule-applicability (Maybe)
newtype Reduction m a = Reduction {runRed :: S.StateT m Maybe a}
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, S.MonadState m, MonadFail)

-- one small step of a subterm; fails unless it was Continue
step :: (Machine m, Show m, V m ~ Value) => Term -> Reduction m Term
-- step t = [ t' | Continue t' <- reduce t ]
step t =
  asum
    [ [BreakSignal | Continue BreakSignal <- reduce t],
      [ContinueSignal | Continue ContinueSignal <- reduce t],
      [t' | Continue t' <- reduce t]
    ]

-- -- extract a signal; fails unless it was BreakSignal or ContinueSignal
-- signal :: (Machine m, Show m, V m ~ Value) => Term -> Reduction m Term
-- signal t = asum
--   [ [ BreakSignal   | BreakSignal   <- pure t ]
--   , [ ContinueSignal | ContinueSignal <- pure t ]
--   ]

-- extract a value; fails unless it was Happy
val :: (Machine m, Show m, V m ~ Value) => Term -> Reduction m (V m)
val t = [v | Happy v <- reduce t]

-- extract an error; fails unless it was Sad
fault :: (Machine m, Show m, V m ~ Value) => Rule m
fault t = [Sad e | Sad e <- reduce t]

-- run an Env m inside Reduction m (syntactic sugar for liftEnv)
envR :: Env m -> Reduction m (Result (V m))
envR e = Reduction $ S.StateT $ \s ->
  let (r, s') = S.runState e s in Just (r, s')

-- pick between branches using your fixed selectValue, but stay in Reduction
selectR ::
  (Machine m, V m ~ Value) =>
  V m ->
  Term ->
  Term ->
  Reduction m (Result (V m))
selectR cond tThen tElse =
  envR
    ( selectValue
        cond
        (return (Continue tThen)) -- :: Env m
        (return (Continue tElse)) -- :: Env m
    )

getTruthinessR ::
  (Machine m, V m ~ Value) =>
  V m ->
  Reduction m Bool
getTruthinessR cond = do
  r <-
    envR
      ( selectValue
          cond
          (return (Happy (BoolVal True)))
          (return (Happy (BoolVal False)))
      )
  case r of
    Happy (BoolVal b) -> pure b
    _ -> error "getTruthinessR: selectValue did not return BoolVal"

-- A small-step rule returns a Step
type Rule m = Term -> Reduction m (Result (V m))

tryRules :: (Machine m, Show m, V m ~ Value) => [Rule m] -> Rule m
tryRules rules t = asum [rule t | rule <- rules]

reduce_ :: (Machine m, Show m, V m ~ Value) => Rule m
reduce_ = tryRules rules
  where
    rules =
      [ -- rules go here
        reduceLiteral,
        reduceVar,
        reduceRetrieve,
        reduceLet,
        reduceMerge,
        reduceSeq,
        reduceIf,
        reduceTry,
        reduceWhile,
        reduceFor,
        reduceRead,
        reduceWrite,
        reduceSkip,
        reduceBinaryOps,
        reduceUnaryOps,
        reduceBreakContinue,
        reduceFun,
        reduceApplyFun,
        reduceIncDec,
        reduceTupleTerm,
        reduceDictionary
      ]

reduceLiteral :: (Machine m, Show m, V m ~ Value) => Rule m
reduceLiteral t =
  asum
    [ [Happy (IntVal n) | Literal n <- pure t],
      [Happy (StringVal s) | StringLiteral s <- pure t],
      [Happy (BoolVal b) | BoolLit b <- pure t]
    ]

reduceIf :: (Machine m, Show m, V m ~ Value) => Rule m
reduceIf t =
  asum
    [ -- step the condition
      [ Continue (If cond' tThen tElse)
        | If cond tThen tElse <- pure t,
          cond' <- step cond
      ],
      -- condition is a value: choose branch via selectValue
      [ r
        | If cond tThen tElse <- pure t,
          v <- val cond,
          r <- selectR v tThen tElse
      ],
      -- condition faults: propagate
      [ e
        | If cond _ _ <- pure t,
          e <- fault cond
      ]
    ]

reduceVar :: (Machine m, Show m, V m ~ Value) => Rule m
reduceVar t =
  asum
    [ -- plain variable lookup
      [ r
        | Var (OnlyStr (s, _)) <- pure t,
          r <- envR (getVar s)
      ],
      -- bracket form desugars to a Retrieve step
      [ Continue (Retrieve (Var ref) term)
        | Var (Bracket ref term) <- pure t
      ],
      -- normalize lone OnlyStr / Bracket into a Var node
      [Continue (Var (OnlyStr s)) | OnlyStr s <- pure t],
      [Continue (Var (Bracket a b)) | Bracket a b <- pure t],
      -- reference wrong type: error
      [ Sad (Type, "Operand of Var must be a reference")
        | Var _ <- pure t
      ]
    ]

reduceRetrieve :: (Machine m, Show m, V m ~ Value) => Rule m
reduceRetrieve t =
  asum
    [ -- step t1
      [ Continue (Retrieve t1' t2)
        | Retrieve t1 t2 <- pure t,
          t1' <- step t1
      ],
      -- t1 is a value: step t2
      [ Continue (Retrieve t1 t2')
        | Retrieve t1 t2 <- pure t,
          _ <- val t1,
          t2' <- step t2
      ],
      -- both t1 and t2 are values: perform getBracketValue
      [ r
        | Retrieve t1 t2 <- pure t,
          v1 <- val t1,
          v2 <- val t2,
          r <- envR (getBracketValue v1 v2)
      ],
      -- fault in t1: propagate
      [ e
        | Retrieve t1 _ <- pure t,
          e <- fault t1
      ],
      -- fault in t2: propagate
      [ e
        | Retrieve t1 t2 <- pure t,
          _ <- val t1,
          e <- fault t2
      ]
    ]

reduceLet :: (Machine m, Show m, V m ~ Value) => Rule m
reduceLet t =
  asum
    [ -- step the RHS
      [ Continue (Let ref rhs')
        | Let ref rhs <- pure t,
          rhs' <- step rhs
      ],
      -- LHS is a plain variable: perform setVar
      [ r
        | Let (OnlyStr (s, _)) rhs <- pure t,
          v <- val rhs,
          r <- envR (setVar s v)
      ],
      -- LHS is a bracketed reference: desugar to Merge
      [ Continue (Let ref' (Merge (Var ref') bTerm rhs))
        | Let (Bracket ref' bTerm) rhs <- pure t
      ],
      -- fault in RHS: propagate
      [ e
        | Let _ rhs <- pure t,
          e <- fault rhs
      ],
      -- LHS wrong type: error
      [ Sad (Type, "Left-hand side of Let must be reference")
        | Let _ _ <- pure t
      ]
    ]

reduceMerge :: (Machine m, Show m, V m ~ Value) => Rule m
reduceMerge t =
  asum
    [ -- step t1
      [ Continue (Merge t1' t2 t3)
        | Merge t1 t2 t3 <- pure t,
          t1' <- step t1
      ],
      -- t1 is a value: step t2
      [ Continue (Merge t1 t2' t3)
        | Merge t1 t2 t3 <- pure t,
          _ <- val t1,
          t2' <- step t2
      ],
      -- t1 and t2 are values: step t3
      [ Continue (Merge t1 t2 t3')
        | Merge t1 t2 t3 <- pure t,
          _ <- val t1,
          _ <- val t2,
          t3' <- step t3
      ],
      -- all three are values: perform setBracketValue
      [ r
        | Merge t1 t2 t3 <- pure t,
          v1 <- val t1,
          v2 <- val t2,
          v3 <- val t3,
          r <- envR (setBracketValue v1 v2 v3)
      ],
      -- fault in t1: propagate
      [ e
        | Merge t1 _ _ <- pure t,
          e <- fault t1
      ],
      -- fault in t2: propagate
      [ e
        | Merge t1 t2 _ <- pure t,
          _ <- val t1,
          e <- fault t2
      ],
      -- fault in t3: propagate
      [ e
        | Merge t1 t2 t3 <- pure t,
          _ <- val t1,
          _ <- val t2,
          e <- fault t3
      ]
    ]

reduceSeq :: (Machine m, Show m, V m ~ Value) => Rule m
reduceSeq t =
  asum
    [ -- step t1 (signal-aware when t2 is a While)
      [ res
        | Seq t1 t2 <- pure t,
          t1' <- step t1,
          let res = case (t1', t2) of
                -- left side signalled while we're sequencing into a loop on the right:
                (BreakSignal, While _ _) -> Happy (IntVal 0) -- break: exit loop
                (ContinueSignal, While c b) -> Continue (While c b) -- continue: next iter
                -- otherwise: propagate signals normally
                (BreakSignal, _) -> Continue BreakSignal
                (ContinueSignal, _) -> Continue ContinueSignal
                -- no signal: keep stepping the sequence
                _ -> Continue (Seq t1' t2)
      ],
      -- t1 is done: continue with t2
      [ Continue t2
        | Seq t1 t2 <- pure t,
          _ <- val t1
      ],
      -- fault in t1: propagate
      [ e
        | Seq t1 _ <- pure t,
          e <- fault t1
      ]
    ]

reduceTry :: (Machine m, Show m, V m ~ Value) => Rule m
reduceTry t =
  asum
    [ -- step tTry
      [ Continue (Try tTry' catchableErrorKindOrAny tCatch)
        | Try tTry catchableErrorKindOrAny tCatch <- pure t,
          tTry' <- step tTry
      ],
      -- tTry is done: handle according to result
      [ Happy vTry
        | Try tTry _ _ <- pure t,
          vTry <- val tTry
      ],
      -- tTry faulted: see if we catch
      [ Continue tCatch
        | Try tTry catchableErrorKindOrAny tCatch <- pure t,
          Sad (resultErrorKind, _) <- fault tTry,
          errorShouldBeCaught resultErrorKind catchableErrorKindOrAny
      ],
      -- tTry faulted: propagate
      [ Sad e
        | Try tTry catchableErrorKindOrAny _ <- pure t,
          Sad e <- fault tTry,
          let (resultErrorKind, _) = e,
          not (errorShouldBeCaught resultErrorKind catchableErrorKindOrAny)
      ]
    ]

reduceWhile :: (Machine m, Show m, V m ~ Value) => Rule m
reduceWhile t =
  asum
    [ -- step condition
      [ Continue (While cond' body)
        | While cond body <- pure t,
          cond' <- step cond
      ],
      -- condition is a value and is false
      [ Continue Skip
        | While cond _ <- pure t,
          condVal <- val cond,
          truthiness <- getTruthinessR condVal,
          not truthiness
      ],
      -- condition is a value and is true but body steps
      [ res
        | While cond body <- pure t,
          condVal <- val cond,
          truthiness <- getTruthinessR condVal,
          truthiness,
          body' <- step body,
          let res = case body' of
                BreakSignal -> Happy (IntVal 0)
                ContinueSignal -> Continue (While cond body)
                b -> Continue (Seq b (While cond body))
      ],
      -- condition is a value and it is true but body is value
      [ Continue (While cond body)
        | While cond body <- pure t,
          condVal <- val cond,
          truthiness <- getTruthinessR condVal,
          truthiness,
          _ <- val body
      ],
      -- fault in condition: propagate
      [ e
        | While cond _ <- pure t,
          e <- fault cond
      ],
      -- fault in body: propagate
      [ e
        | While cond body <- pure t,
          condVal <- val cond,
          truthiness <- getTruthinessR condVal,
          truthiness,
          e <- fault body
      ]
    ]

reduceFor :: (Machine m, Show m, V m ~ Value) => Rule m
reduceFor t =
  asum
    [ -- step start
      [ Continue (For var start' end body)
        | For var start end body <- pure t,
          start' <- step start
      ],
      -- start is a value: step end
      [ Continue (For var start end' body)
        | For var start end body <- pure t,
          _ <- val start,
          end' <- step end
      ],
      -- both start and end are values: desugar to Let + While
      [ Continue (Seq (Let (OnlyStr var) start) (While cond body'))
        | For var start end body <- pure t,
          _ <- val start,
          IntVal iEnd <- val end,
          let cond = BinaryOps Lt (Var (OnlyStr var)) (Literal iEnd),
          let body' = Seq body (Let (OnlyStr var) (BinaryOps Add (Var (OnlyStr var)) (Literal 1)))
      ],
      -- fault in start: propagate
      [ e
        | For _ start _ _ <- pure t,
          e <- fault start
      ],
      -- fault in end: propagate
      [ e
        | For _ start end _ <- pure t,
          _ <- val start,
          e <- fault end
      ]
    ]

reduceRead :: (Machine m, Show m, V m ~ Value) => Rule m
reduceRead t =
  asum
    [ -- Read operation
      [ r
        | Read (s, _) <- pure t,
          Happy input <- envR inputVal, -- Can inputVal fail?
          r <- envR (setVar s input)
      ]
    ]

reduceWrite :: (Machine m, Show m, V m ~ Value) => Rule m
reduceWrite t =
  asum
    [ -- step expr
      [ Continue (Write expr')
        | Write expr <- pure t,
          expr' <- step expr
      ],
      -- Write operation
      [ r
        | Write expr <- pure t,
          v <- val expr,
          r <- envR (outputVal v)
      ],
      -- fault in expr: propagate
      [ e
        | Write expr <- pure t,
          e <- fault expr
      ]
    ]

reduceSkip :: (Machine m, Show m, V m ~ Value) => Rule m
reduceSkip t = [Happy (IntVal 0) | Skip <- pure t]

reduceBinaryOps :: (Machine m, Show m, V m ~ Value) => Rule m
reduceBinaryOps t =
  asum
    [ -- step t1
      [ Continue (BinaryOps op t1' t2)
        | BinaryOps op t1 t2 <- pure t,
          t1' <- step t1
      ],
      -- t1 is a value: step t2
      [ Continue (BinaryOps op t1 t2')
        | BinaryOps op t1 t2 <- pure t,
          _ <- val t1,
          t2' <- step t2
      ],
      -- both t1 and t2 are values: perform operation
      [ r
        | BinaryOps op t1 t2 <- pure t,
          v1 <- val t1,
          v2 <- val t2,
          r <- envR (applyBinaryOp op v1 v2)
      ],
      -- fault in t1: propagate
      [ e
        | BinaryOps _ t1 _ <- pure t,
          e <- fault t1
      ],
      -- fault in t2: propagate
      [ e
        | BinaryOps _ t1 t2 <- pure t,
          _ <- val t1,
          e <- fault t2
      ]
    ]
  where
    applyBinaryOp Add = addVal
    applyBinaryOp Sub = subVal
    applyBinaryOp Mul = mulVal
    applyBinaryOp Div = divVal
    applyBinaryOp Mod = modVal
    applyBinaryOp Pow = powVal
    applyBinaryOp Lt = ltVal
    applyBinaryOp Gt = gtVal
    applyBinaryOp Lte = lteVal
    applyBinaryOp Gte = gteVal
    applyBinaryOp Eq = eqVal
    applyBinaryOp Neq = neqVal
    applyBinaryOp And = andVal
    applyBinaryOp Or = orVal
    applyBinaryOp Xor = xorVal

reduceUnaryOps :: (Machine m, Show m, V m ~ Value) => Rule m
reduceUnaryOps t =
  asum
    [ -- step t
      [ Continue (UnaryOps op rhs')
        | UnaryOps op rhs <- pure t,
          rhs' <- step rhs
      ],
      -- t is a value: perform operation
      [ r
        | UnaryOps op rhs <- pure t,
          v <- val rhs,
          r <- envR (applyUnaryOp op v)
      ],
      -- fault in t: propagate
      [ e
        | UnaryOps _ rhs <- pure t,
          e <- fault rhs
      ]
    ]
  where
    applyUnaryOp Neg = negVal
    applyUnaryOp Not = notVal
    applyUnaryOp BitNot = bitNotVal

reduceBreakContinue :: (Machine m, Show m, V m ~ Value) => Rule m
reduceBreakContinue t =
  asum
    [ [Continue BreakSignal | BreakSignal <- pure t],
      [Continue ContinueSignal | ContinueSignal <- pure t]
    ]

reduceIncDec :: (Machine m, Show m, V m ~ Value) => Rule m
reduceIncDec t =
  asum
    [ [r | PreIncrement x <- pure t, r <- envR (preIncrementVal x)],
      [r | PreDecrement x <- pure t, r <- envR (preDecrementVal x)],
      [r | PostIncrement x <- pure t, r <- envR (postIncrementVal x)],
      [r | PostDecrement x <- pure t, r <- envR (postDecrementVal x)]
    ]

-- reduce_ (Fun xs t) = do
--   env <- S.get
--   let vars = getScope env
--   return $ Happy (ClosureVal xs t vars)
reduceFun :: (Machine m, Show m, V m ~ Value) => Rule m
reduceFun t =
  asum
    [ [ Happy (ClosureVal xs body vars)
        | Fun xs body <- pure t,
          env <- S.get,
          let vars = getScope env
      ]
    ]

reduceApplyFun :: (Machine m, Show m, V m ~ Value) => Rule m
reduceApplyFun t =
  asum
    [ -- (1) Step the function position first
      [ Continue (ApplyFun tf' tas)
        | ApplyFun tf tas <- pure t,
          tf' <- step tf
      ],
      -- (2) Function position fault: propagate
      [ e
        | ApplyFun tf _ <- pure t,
          e <- fault tf
      ],
      -- (3) Function is a value, zero args: run applyFuncNoArg
      --     (handles both ClosureVal and non-function errors)
      [ r
        | ApplyFun tf [] <- pure t,
          fval <- val tf,
          r <- envR (applyFuncNoArg fval)
      ],
      -- (4) Function is a value, there are args: step the first arg (left-to-right)
      [ Continue (ApplyFun tf (a' : as))
        | ApplyFun tf (a : as) <- pure t,
          _ <- val tf, -- only step args once tf is a value
          a' <- step a
      ],
      -- (5) First arg is now a value: consume exactly one argument via applyFunArgList
      --     (this handles: too many args, non-function, partial application, body eval)
      [ r
        | ApplyFun tf (a : as) <- pure t,
          fval <- val tf,
          aval <- val a,
          r <- envR (applyFunArgList tf as fval aval)
      ]
    ]

-- Reduce/apply one argument at a time (left-to-right), small-step style.
-- This is what your rules should call.
reduceArgsAndApplyR ::
  (Machine m, Show m, V m ~ Value) =>
  -- | tf  (function term, used only to rebuild the frame)
  Term ->
  -- | args
  [Term] ->
  -- | funVal (already evaluated function value)
  Value ->
  Reduction m (Result (V m))
reduceArgsAndApplyR _ [] funVal =
  -- zero arguments: defer to your existing helper
  [r | r <- envR (applyFuncNoArg funVal)]
reduceArgsAndApplyR tf (a : rest) funVal =
  asum
    [ -- step the next arg
      [ Continue (ApplyFun tf (a' : rest))
        | a' <- step a
      ],
      -- arg is a value: consume exactly one argument via your helper
      [ r
        | aval <- val a,
          r <- envR (applyFunArgList tf rest funVal aval)
      ],
      -- propagate a fault from the arg
      [ e
        | e <- fault a
      ]
    ]

runRToEnv :: Reduction m (Result (V m)) -> Env m
runRToEnv r = S.state $ \s ->
  case S.runStateT (runRed r) s of
    Nothing -> (Sad (Internal, "stuck in reduceArgsAndApplyR"), s)
    Just (x, s') -> (x, s')

-- Back-compat name with original type:
reduceArgsAndApply ::
  (Machine m, Show m, V m ~ Value) =>
  Term ->
  [Term] ->
  Value ->
  Env m
reduceArgsAndApply tf args funVal =
  runRToEnv (reduceArgsAndApplyR tf args funVal)

applyFunArgList :: (Machine m, Show m, V m ~ Value) => Term -> [Term] -> Value -> Value -> Env m
applyFunArgList tf rest funVal argVal = do
  res1 <- applyFunArg funVal argVal
  case res1 of
    Happy v1 -> case rest of
      [] -> return (Happy v1)
      _ -> reduceArgsAndApply tf rest v1
    Continue t -> return (Continue t)
    Sad msg -> return (Sad msg)

applyFunArg :: (Machine m, Show m, V m ~ Value) => Value -> Value -> Env m
applyFunArg (ClosureVal [] _ _) _ = do
  return $ Sad (Arguments, "too many arguments: function takes 0 arguments")
applyFunArg (ClosureVal ((x, _) : xs) body caps) arg = do
  let newCaps = caps ++ [(x, arg)]
  if null xs
    then evalClosureBody body newCaps
    else return $ Happy (ClosureVal xs body newCaps)
applyFunArg _ _ = return $ Sad (Type, "attempt to call a non-function")

applyFuncNoArg :: (Machine m, Show m, V m ~ Value) => Value -> Env m
applyFuncNoArg (ClosureVal [] body caps) = evalClosureBody body caps
applyFuncNoArg (ClosureVal (_ : _) _ _) = return $ Sad (Arguments, "missing arguments: function requires parameters")
applyFuncNoArg _ = return $ Sad (Type, "attempt to call a non-function")

-- Bind captured args, evaluate body, restore machine state
evalClosureBody :: (Machine m, Show m, V m ~ Value) => Term -> [(String, Value)] -> Env m
evalClosureBody body caps = do
  m0 <- S.get
  let (_resPush, m1) = S.runState (pushScope []) m0
  case bindMany caps m1 of
    Left msg -> return $ Sad msg
    Right m2 -> do
      let (res, m3) = reduceFully body m2
      let (_resPop, m4) = S.runState popScope m3 -- Restore previous scope.
      S.put m4
      case res of
        Left msg -> return $ Sad (Arguments, msg)
        Right v -> return $ Happy v

bindMany :: (Machine m, V m ~ Value) => [(String, Value)] -> m -> Either Error m
bindMany [] m = Right m
bindMany ((k, v) : rest) m =
  case S.runState (setVar k v) m of
    (Sad msg, _m') -> Left msg
    (Continue _, _m') -> Left (Arguments, "internal: setVar requested Continue")
    (Happy _, m') -> bindMany rest m'

reduceTupleTerm :: (Machine m, Show m, V m ~ Value) => Rule m
reduceTupleTerm t =
  asum
    [ -- non-empty tuple: step first element
      [ Continue (TupleTerm (elm' : elms))
        | TupleTerm (elm : elms) <- pure t,
          elm' <- step elm
      ],
      -- first element is a value: step rest of tuple
      [ Continue (TupleTerm (elm : elms'))
        | TupleTerm (elm : elms) <- pure t,
          _ <- val elm,
          TupleTerm elms' <- step (TupleTerm elms)
      ],
      -- all elements are values: construct tuple value
      [ Happy (Tuple vs)
        | TupleTerm elms <- pure t,
          vs <- mapM val elms
      ],
      -- fault in first element: propagate
      [ e
        | TupleTerm (elm : _) <- pure t,
          e <- fault elm
      ],
      -- fault in rest of tuple: propagate
      [ e
        | TupleTerm (_ : elms) <- pure t,
          _ <- val (head elms),
          e <- fault (TupleTerm elms)
      ],
      -- empty tuple: return empty tuple value
      [ r
        | TupleTerm [] <- pure t,
          r <- envR (return $ Happy $ Tuple [])
      ]
    ]

reduceDictionary :: (Machine m, Show m, V m ~ Value) => Rule m
reduceDictionary t =
  asum
    [ [r | NewDictionary <- pure t, r <- envR (return $ Happy $ Dictionary M.empty)]
    ]

reduce :: (Machine m, Show m, V m ~ Value) => Rule m
reduce t = do
  e <- S.get
  trace ("Simulating: " ++ show t) () `seq`
    trace ("     Machine: " ++ show e) () `seq`
      reduce_ t

reduceFully :: (Machine m, Show m, V m ~ Value) => Term -> m -> (Either String (V m), m)
reduceFully term0 m0 = go m0 term0
  where
    go st t =
      case S.runStateT (runRed (reduce t)) st of
        -- show stuck term
        Nothing -> (Left ("stuck term: " ++ show t), st)
        Just (Happy v, st') -> (Right v, st')
        Just (Sad (_, msg), st') -> (Left msg, st')
        Just (Continue t', st') ->
          -- handle special signals (your AST constructors)
          case t' of
            BreakSignal -> (Left "unhandled break signal", st')
            ContinueSignal -> (Left "unhandled continue signal", st')
            _ -> go st' t'
