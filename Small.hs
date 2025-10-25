{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Small (reduceFully, Machine (..), Result (..), Error, Env) where

-- import Data.Either

import Control.Applicative
import Control.Monad
import qualified Control.Monad.State as S
import qualified Data.Map as M
import qualified Data.Set as DS
import Debug.Trace (trace)
import Term (BinaryOp (..), ErrorKind (..), ErrorKindOrAny (..), Term (..), UnaryOp (..))
import Value (Value (..))

----- The Machine type class -----

-- The micro-ops that a machine must support
-- Allow an implementation to define its own semantics

class Machine m where
  type V m -- The value type for this machine
  -- Uses associated an associated type family for the value type
  -- This requires the TypeFamilies extension
  -- The way you read the type signature is:
  --    for any type m that is an instance of Machine, there is an associated type (V m)

  -- Get and set variables
  getVar :: String -> Env m
  setVar :: String -> V m -> Env m

  -- Lexical scoping
  getScope :: m -> [(String, Value)] -- Variable bindings only.
  pushScope :: [(String, Value)] -> Env m
  popScope :: Env m

  -- I/O
  inputVal :: Env m
  outputVal :: V m -> Env m

  -- Arithmetic and control
  addVal :: V m -> V m -> Env m
  subVal :: V m -> V m -> Env m
  mulVal :: V m -> V m -> Env m
  divVal :: V m -> V m -> Env m
  modVal :: V m -> V m -> Env m
  powVal :: V m -> V m -> Env m
  negVal :: V m -> Env m

  -- Comparison operations (operate on integers, return booleans)
  ltVal :: V m -> V m -> Env m
  gtVal :: V m -> V m -> Env m
  lteVal :: V m -> V m -> Env m
  gteVal :: V m -> V m -> Env m
  eqVal :: V m -> V m -> Env m
  neqVal :: V m -> V m -> Env m

  -- Logical operations (operate on booleans)
  andVal :: V m -> V m -> Env m
  orVal :: V m -> V m -> Env m
  xorVal :: V m -> V m -> Env m
  notVal :: V m -> Env m
  bitNotVal :: V m -> Env m

  -- Increment/Decrement operations (modify variables)
  preIncrementVal :: String -> Env m -- ++x: increment then return new value
  preDecrementVal :: String -> Env m -- --x: decrement then return new value
  postIncrementVal :: String -> Env m -- x++: return old value then increment
  postDecrementVal :: String -> Env m -- x--: return old value then decrement

  -- Access/Manage Bracket Values
  getBracketValue :: V m -> V m -> Env m
  setBracketValue :: V m -> V m -> V m -> Env m

  -- Control flow - selectValue uses boolean semantics
  selectValue :: V m -> Env m -> Env m -> Env m

-- abstract semantics that glue micro-ops together

type Error = (ErrorKind, String)

-- Helper for try-catch statement
errorShouldBeCaught :: ErrorKind -> ErrorKindOrAny -> Bool
errorShouldBeCaught _ Any = True
errorShouldBeCaught resultErrorKind (Specific catchableErrorKind) = resultErrorKind == catchableErrorKind

------ Small-step reduction ------

data Result a
  = Happy a -- produced an answer
  | Continue Term -- need to keep going
  | Sad Error -- error
  deriving (Eq, Show)

type Env m = S.State m (Result (V m))

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
        reduceForIn,
        reduceForInLoop,
        reduceRange,
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
        reduceDictionary,
        reduceSet
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
        | Var (OnlyStr s) <- pure t,
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
        | Let (OnlyStr s) rhs <- pure t,
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
    [ -- step t1 (signal-aware when t2 is a While, ForIn, or ForInLoop)
      [ res
        | Seq t1 t2 <- pure t,
          t1' <- step t1,
          let res = case (t1', t2) of
                -- left side signalled while we're sequencing into a loop on the right:
                (BreakSignal, While _ _) -> Happy (IntVal 0) -- break: exit loop
                (ContinueSignal, While c b) -> Continue (While c b) -- continue: next iter
                (BreakSignal, ForIn _ _ _) -> Happy (IntVal 0) -- break: exit loop
                (ContinueSignal, ForIn v iter b) -> Continue (ForIn v iter b) -- continue: next iter
                (BreakSignal, ForInLoop _ _ _) -> Happy (IntVal 0) -- break: exit loop
                (ContinueSignal, ForInLoop v vs b) -> Continue (ForInLoop v vs b) -- continue: next iter
                -- also handle Seq body (ForIn/ForInLoop ...) pattern from multi-element iteration
                (BreakSignal, Seq _ (ForIn _ _ _)) -> Happy (IntVal 0)
                (ContinueSignal, Seq _ (ForIn v iter b)) -> Continue (ForIn v iter b)
                (BreakSignal, Seq _ (ForInLoop _ _ _)) -> Happy (IntVal 0)
                (ContinueSignal, Seq _ (ForInLoop v vs b)) -> Continue (ForInLoop v vs b)
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

reduceForIn :: (Machine m, Show m, V m ~ Value) => Rule m
reduceForIn t =
  asum
    [ -- iterator step
      [ Continue (ForIn var iter' body)
        | ForIn var iter body <- pure t,
          iter' <- step iter
      ],
      -- string: convert to tuple of single-character strings
      [ Continue (ForInLoop var (map (StringVal . (:[])) str) body)
        | ForIn var iter body <- pure t,
          StringVal str <- val iter
      ],
      -- dictionary: convert to tuple of keys which are integers
      [ Continue (ForInLoop var (map IntVal (M.keys dict)) body)
        | ForIn var iter body <- pure t,
          Dictionary dict <- val iter
      ],
      -- set: convert to list of integers
      [ Continue (ForInLoop var (map IntVal (DS.toList setVals)) body)
        | ForIn var iter body <- pure t,
          Set setVals <- val iter
      ],
      -- regular tuple (not [iterator, state]): convert to ForInLoop
      [ Continue (ForInLoop var vals body)
        | ForIn var iter body <- pure t,
          Tuple vals <- val iter,
          not (isIteratorTuple vals)
      ],
      -- custom iterator: [function with 1 param, initialState]
      [ r
        | ForIn var iter body <- pure t,
          Tuple [iterFunc@(ClosureVal [_] _ _), initialState] <- val iter,
          r <- envR (callCustomIterator var iterFunc initialState body)
      ],
      -- iterator with wrong arity: error
      [ Sad (Type, "iterator must take exactly 1 parameter")
        | ForIn _ iter _ <- pure t,
          Tuple [ClosureVal params _ _, _] <- val iter,
          length params /= 1
      ],
      -- other value: error
      [ Sad (Type, "for-in requires iterable (tuple, string, dictionary, set, or [iterator, state])")
        | ForIn _ iter _ <- pure t,
          v <- val iter,
          not (isIterableValue v)
      ],
      -- propagate error
      [ e
        | ForIn _ iter _ <- pure t,
          e <- fault iter
      ]
    ]

reduceForInLoop :: (Machine m, Show m, V m ~ Value) => Rule m
reduceForInLoop t =
  asum
    [ -- empty list: done
      [ Happy (IntVal 0)
        | ForInLoop _ [] _ <- pure t
      ],
      -- single element: bind and execute body (returns body result)
      [ r
        | ForInLoop var [v] body <- pure t,
          r <- envR $ do
            _ <- setVar var v
            return $ Continue body
      ],
      -- multiple elements: bind first, execute body, then continue with rest
      [ r
        | ForInLoop var (v : vs@(_ : _)) body <- pure t,
          r <- envR $ do
            _ <- setVar var v
            return $ Continue (Seq body (ForInLoop var vs body))
      ]
    ]

-- is the value iterable?
isIterableValue :: Value -> Bool
isIterableValue (Tuple [ClosureVal _ _ _, _]) = True -- [iterator, state] pattern
isIterableValue (Tuple _) = True
isIterableValue (StringVal _) = True -- strings are iterable
isIterableValue (Dictionary _) = True -- dictionaries are iterable
isIterableValue (Set _) = True -- sets are iterable
isIterableValue _ = False

-- which pattern is the iterator?
isIteratorTuple :: [Value] -> Bool
isIteratorTuple [ClosureVal _ _ _, _] = True
isIteratorTuple _ = False

-- Helper: convert Value back to Term for binding
valueToTerm :: Value -> Term
valueToTerm (IntVal n) = Literal n
valueToTerm (BoolVal b) = BoolLit b
valueToTerm (StringVal s) = StringLiteral s
valueToTerm (Tuple vals) = TupleTerm (map valueToTerm vals)
valueToTerm (Dictionary _) = NewDictionary -- simplified; loses data
valueToTerm (Set _) = NewSet -- simplified; loses data
valueToTerm (ClosureVal params body _) = Fun params body -- loses captures; not ideal

-- call custom iterator with state, returns [value, newState]
callCustomIterator :: (Machine m, Show m, V m ~ Value) => String -> Value -> Value -> Term -> Env m
callCustomIterator var iterFunc state body = do
  --cCall iterator(state)
  result <- applyFunArg iterFunc state
  case result of
    -- StopIteration → done (empty iterator)
    Continue StopIteration -> return (Happy (IntVal 0))
    
    -- bind and check if this is the last iteration
    Happy (Tuple [value, newState]) -> do
      _ <- setVar var value
      -- call iterator(newState) to see if there's a next value
      nextResult <- applyFunArg iterFunc newState
      case nextResult of
        -- call returns StopIteration → this is the last iteration
        Continue StopIteration -> return $ Continue body
        -- continue the loop
        Happy (Tuple _) -> return $ Continue (Seq body (ForIn var (TupleTerm [valueToTerm iterFunc, valueToTerm newState]) body))
        -- wrong format
        Happy _ -> return $ Sad (Type, "iterator must return [value, state] or StopIteration")
        -- error
        Sad err -> return $ Sad err
        Continue _ -> return $ Sad (Internal, "iterator returned Continue signal")
    
    -- wrong return format
    Happy (Tuple _) -> return $ Sad (Type, "iterator must return [value, state] tuple with 2 elements")
    Happy _ -> return $ Sad (Type, "iterator must return [value, state] or StopIteration")
    
    -- propagate errors
    Sad err -> return $ Sad err
    Continue _ -> return $ Sad (Internal, "iterator returned Continue signal")

reduceRange :: (Machine m, Show m, V m ~ Value) => Rule m
reduceRange t =
  asum
    [ -- step the argument
      [ Continue (Range n')
        | Range n <- pure t,
          n' <- step n
      ],
      -- create lazy tuple for range(n): [0, 1, 2, ..., n-1]
      [ Happy (Tuple (map IntVal [0 .. limit - 1]))
        | Range n <- pure t,
          IntVal limit <- val n
      ],
      -- non-integer argument: error
      [ Sad (Type, "range requires an integer argument")
        | Range n <- pure t,
          v <- val n,
          notInt v
      ],
      -- propagate error
      [ e
        | Range n <- pure t,
          e <- fault n
      ]
    ]
  where
    notInt (IntVal _) = False
    notInt _ = True

reduceRead :: (Machine m, Show m, V m ~ Value) => Rule m
reduceRead t =
  asum
    [ -- Read operation
      [ r
        | Read s <- pure t,
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
      [Continue ContinueSignal | ContinueSignal <- pure t],
      [Continue StopIteration | StopIteration <- pure t]
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
applyFunArg (ClosureVal (x : xs) body caps) arg = do
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
      let (res, m3) = reduceFullyAllowingSignals body m2
      let (_resPop, m4) = S.runState popScope m3 -- Restore previous scope.
      S.put m4
      case res of
        Left msg -> return $ Sad (Arguments, msg)
        Right (Left sig) -> return $ Continue sig  -- Signal (e.g., StopIteration)
        Right (Right v) -> return $ Happy v

-- Like reduceFully, but allows StopIteration to be returned as a valid signal
reduceFullyAllowingSignals :: (Machine m, Show m, V m ~ Value) => Term -> m -> (Either String (Either Term (V m)), m)
reduceFullyAllowingSignals term0 m0 = go m0 term0
  where
    go st t =
      case S.runStateT (runRed (reduce t)) st of
        Nothing -> (Left ("stuck term: " ++ show t), st)
        Just (Happy v, st') -> (Right (Right v), st')
        Just (Sad (_, msg), st') -> (Left msg, st')
        Just (Continue t', st') ->
          case t' of
            BreakSignal -> (Left "unhandled break signal", st')
            ContinueSignal -> (Left "unhandled continue signal", st')
            StopIteration -> (Right (Left StopIteration), st')  -- Allow StopIteration
            _ -> go st' t'

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

reduceSet :: (Machine m, Show m, V m ~ Value) => Rule m
reduceSet t =
  asum
    [ -- empty set
      [ r
        | NewSet <- pure t,
          r <- envR (return $ Happy $ Set DS.empty)
      ],
      -- non-empty set: step first element
      [ Continue (SetTerm (elm' : elms))
        | SetTerm (elm : elms) <- pure t,
          elm' <- step elm
      ],
      -- first element is a value: step rest of set
      [ Continue (SetTerm (elm : elms'))
        | SetTerm (elm : elms) <- pure t,
          _ <- val elm,
          SetTerm elms' <- step (SetTerm elms)
      ],
      -- all elements are values: construct set value (must be integers)
      [ r
        | SetTerm elms <- pure t,
          vs <- mapM val elms,
          r <- envR (constructSetValues vs)
      ],
      -- fault in first element: propagate
      [ e
        | SetTerm (elm : _) <- pure t,
          e <- fault elm
      ],
      -- fault in rest of set: propagate
      [ e
        | SetTerm (_ : elms) <- pure t,
          _ <- val (head elms),
          e <- fault (SetTerm elms)
      ],
      -- empty set literal: return empty set value
      [ r
        | SetTerm [] <- pure t,
          r <- envR (return $ Happy $ Set DS.empty)
      ]
    ]

constructSetValues :: (Machine m, V m ~ Value) => [Value] -> Env m
constructSetValues vs = do
  let extractInt (IntVal n) = Right n
      extractInt _ = Left "Set elements must be integers"
  case mapM extractInt vs of
    Right ints -> return $ Happy $ Set (DS.fromList ints)
    Left msg -> return $ Sad (Type, msg)

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
            StopIteration -> (Left "unhandled stop iteration", st')
            _ -> go st' t'
