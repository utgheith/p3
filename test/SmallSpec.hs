{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SmallSpec (spec) where

import qualified Control.Monad.State as S
import Data.Bits (complement)
import Data.Either (isLeft)
import qualified Data.Map as M
import Scope (Scope (..), emptyScope, getAllBindings, insertScope, lookupScope, scopeFromList)
import Small
import Term
import Test.Hspec
import Value (Value (..))

-- A mock machine for testing
data MockMachine = MockMachine {getMem :: Scope, getInput :: [Value], getOutput :: [Value]} deriving (Show, Eq)

instance Machine MockMachine where
  type V MockMachine = Value

  getVar x = do
    m <- S.get
    case lookupScope x (getMem m) of
      Just v -> return $ Happy v
      Nothing -> return $ Sad (VariableNotFound, "variable not found")

  setVar x v = do
    m <- S.get
    S.put (m {getMem = insertScope x v (getMem m)})
    return $ Happy v

  getScope m = getAllBindings (getMem m)

  pushScope vars = do
    m <- S.get
    S.put (m {getMem = Scope (M.fromList vars) (Just (getMem m))})
    return $ Happy (IntVal 0)

  popScope = do
    m <- S.get
    case getMem m of
      Scope _ (Just parent) -> S.put (m {getMem = parent})
      Scope _ Nothing -> S.put (m {getMem = emptyScope}) -- Reset to empty scope.
    return $ Happy (IntVal 0)

  inputVal = do
    m <- S.get
    case getInput m of
      (i : is) -> do
        S.put (m {getInput = is})
        return $ Happy i
      [] -> return $ Sad (Input, "end of input")

  outputVal v = do
    m <- S.get
    S.put (m {getOutput = getOutput m ++ [v]})
    return $ Happy v

  subVal (IntVal v1) (IntVal v2) = return $ Happy (IntVal (v1 - v2))
  subVal _ _ = return $ Sad (Type, "Type error in subtraction")

  addVal (IntVal v1) (IntVal v2) = return $ Happy (IntVal (v1 + v2))
  addVal _ _ = return $ Sad (Type, "Type error in addition")

  mulVal (IntVal v1) (IntVal v2) = return $ Happy (IntVal (v1 * v2))
  mulVal _ _ = return $ Sad (Type, "Type error in multiplication")

  divVal (IntVal v1) (IntVal v2) =
    if v2 == 0
      then return $ Sad (Arithmetic, "Cannot divide by 0")
      else return $ Happy (IntVal (v1 `div` v2)) -- I don't want the actual interpreter to crash
  divVal _ _ = return $ Sad (Type, "Type error in division")

  modVal (IntVal v1) (IntVal v2) =
    if v2 == 0
      then return $ Sad (Arithmetic, "Cannot mod by 0")
      else return $ Happy (IntVal (v1 `mod` v2)) -- I don't want the actual interpreter to crash
  modVal _ _ = return $ Sad (Type, "Type error in modulus")

  powVal (IntVal v1) (IntVal v2) =
    if v2 < 0
      then return $ Sad (Arithmetic, "Negative exponent not supported")
      else return $ Happy (IntVal (v1 ^ v2))
  powVal _ _ = return $ Sad (Type, "Type error in exponentiation")

  negVal (IntVal v) =
    return $ Happy (IntVal (-v))
  negVal _ = return $ Sad (Type, "Type error in neg")

  ltVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 < v2))
  ltVal _ _ = return $ Sad (Type, "Type error in <")

  gtVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 > v2))
  gtVal _ _ = return $ Sad (Type, "Type error in >")

  lteVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 <= v2))
  lteVal _ _ = return $ Sad (Type, "Type error in <=")

  gteVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 >= v2))
  gteVal _ _ = return $ Sad (Type, "Type error in >=")

  eqVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 == v2))
  eqVal (BoolVal v1) (BoolVal v2) = return $ Happy (BoolVal (v1 == v2))
  eqVal (StringVal v1) (StringVal v2) = return $ Happy (BoolVal (v1 == v2))
  eqVal _ _ = return $ Sad (Type, "Type error in ==")

  neqVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 /= v2))
  neqVal (BoolVal v1) (BoolVal v2) = return $ Happy (BoolVal (v1 /= v2))
  neqVal (StringVal v1) (StringVal v2) = return $ Happy (BoolVal (v1 /= v2))
  neqVal _ _ = return $ Sad (Type, "Type error in !=")

  andVal (BoolVal v1) (BoolVal v2) = return $ Happy (BoolVal (v1 && v2))
  andVal _ _ = return $ Sad (Type, "Type error in &&")

  orVal (BoolVal v1) (BoolVal v2) = return $ Happy (BoolVal (v1 || v2))
  orVal _ _ = return $ Sad (Type, "Type error in ||")

  xorVal (BoolVal v1) (BoolVal v2) = return $ Happy (BoolVal (v1 /= v2))
  xorVal _ _ = return $ Sad (Type, "Type error in ^")

  notVal (BoolVal v) = return $ Happy (BoolVal (not v))
  notVal _ = return $ Sad (Type, "Type error in !")

  bitNotVal (IntVal v) = return $ Happy (IntVal (complement v))
  bitNotVal _ = return $ Sad (Type, "Type error in ~")

  preIncrementVal x = do
    m <- S.get
    case lookupScope x (getMem m) of
      Just (IntVal v) -> do
        let newVal = IntVal (v + 1)
        S.put (m {getMem = insertScope x newVal (getMem m)})
        return $ Happy newVal
      Just _ -> return $ Sad (Type, "Type error: can only increment integers")
      Nothing -> return $ Sad (VariableNotFound, "Variable " ++ x ++ " not found")

  preDecrementVal x = do
    m <- S.get
    case lookupScope x (getMem m) of
      Just (IntVal v) -> do
        let newVal = IntVal (v - 1)
        S.put (m {getMem = insertScope x newVal (getMem m)})
        return $ Happy newVal
      Just _ -> return $ Sad (Type, "Type error: can only decrement integers")
      Nothing -> return $ Sad (VariableNotFound, "Variable " ++ x ++ " not found")

  postIncrementVal x = do
    m <- S.get
    case lookupScope x (getMem m) of
      Just (IntVal v) -> do
        let newVal = IntVal (v + 1)
        S.put (m {getMem = insertScope x newVal (getMem m)})
        return $ Happy (IntVal v) -- Return old value
      Just _ -> return $ Sad (Type, "Type error: can only increment integers")
      Nothing -> return $ Sad (VariableNotFound, "Variable " ++ x ++ " not found")

  postDecrementVal x = do
    m <- S.get
    case lookupScope x (getMem m) of
      Just (IntVal v) -> do
        let newVal = IntVal (v - 1)
        S.put (m {getMem = insertScope x newVal (getMem m)})
        return $ Happy (IntVal v) -- Return old value
      Just _ -> return $ Sad (Type, "Type error: can only decrement integers")
      Nothing -> return $ Sad (VariableNotFound, "Variable " ++ x ++ " not found")

  getBracketValue (Tuple (x : xs)) (IntVal pos) = if pos == 0 then return (Happy x) else getBracketValue (Tuple xs) (IntVal (pos - 1))
  getBracketValue (Dictionary d) (IntVal val) = case M.lookup val d of
    Just v -> return $ Happy v
    Nothing -> return $ Sad (VariableNotFound, "Unable to find element in dictionary")
  getBracketValue (Dictionary _) _ = return $ Sad (Type, "Unable to index into dictionary with type")
  getBracketValue (Tuple _) _ = return $ Sad (VariableNotFound, "Out of Bounds")
  getBracketValue _ _ = return $ Sad (Type, "Invalid Lookup Bad Input")

  setBracketValue (Dictionary current) (IntVal index) val =
    return $ Happy $ Dictionary (M.insert index val current)
  setBracketValue (Tuple t) (IntVal index) val =
    let returnVal = loop (Tuple t) (IntVal index) val
     in case returnVal of
          Left e -> return $ Sad e
          Right v -> return $ Happy v
    where
      loop :: Value -> Value -> Value -> Either Error Value
      loop (Tuple (x : xs)) (IntVal pos) setVal =
        if pos == 0
          then Right $ Tuple (setVal : xs)
          else
            let returnVal = loop (Tuple xs) (IntVal (pos - 1)) setVal
             in case returnVal of
                  Right (Tuple rest) -> Right $ Tuple (x : rest)
                  _ -> returnVal
      loop (Tuple []) (IntVal _) _ = Left (VariableNotFound, "Attempting to set value Out of Bounds")
      loop _ _ _ = error "unreachable hopefully"
  setBracketValue _ _ _ = return $ Sad (Type, "Had a Type Error")

  selectValue (BoolVal True) c _ = c
  selectValue (BoolVal False) _ t = t
  selectValue (IntVal n) c t = if n /= 0 then c else t
  selectValue (StringVal s) c t = if not (null s) then c else t
  selectValue (Tuple l) c t = if not (null l) then c else t
  selectValue ClosureVal {} _ _ = return $ Sad (Type, "Type error in select")
  selectValue (Dictionary _) _ _ = return $ Sad (Type, "Type error in select")

spec :: Spec
spec =
  describe "reduceFully" $ do
    let initialMachine = MockMachine {getMem = emptyScope, getInput = [], getOutput = []}

    it "reduces an integer literal" $ do
      let term = Literal 10
      reduceFully term initialMachine `shouldBe` (Right (IntVal 10), initialMachine)

    it "reduces a string literal" $ do
      let term = StringLiteral "hello"
      reduceFully term initialMachine `shouldBe` (Right (StringVal "hello"), initialMachine)

    it "reduces a variable" $ do
      let term = Var (OnlyStr "x")
      let machine = initialMachine {getMem = scopeFromList [("x", IntVal 5)]}
      reduceFully term machine `shouldBe` (Right (IntVal 5), machine)

    it "reduces a let expression" $ do
      let term = Seq (Let (OnlyStr "x") (Literal 5)) (Var (OnlyStr "x"))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 5)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 5), finalMachine)

    it "reduces a sequence" $ do
      let term = Seq (Let (OnlyStr "x") (Literal 5)) (BinaryOps Add (Var (OnlyStr "x")) (Literal 1))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 5)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 6), finalMachine)

    it "reduces an if expression (then)" $ do
      let term = If (BoolLit True) (Literal 10) (Literal 20)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 10), initialMachine)

    it "reduces an if expression (else)" $ do
      let term = If (BoolLit False) (Literal 10) (Literal 20)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 20), initialMachine)

    it "reduces a while loop" $ do
      let term = Seq (Let (OnlyStr "x") (Literal 3)) (While (Var (OnlyStr "x")) (Let (OnlyStr "x") (BinaryOps Sub (Var (OnlyStr "x")) (Literal 1))))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 0)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 0), finalMachine)

    it "reduces read and write" $ do
      let term = Seq (Read "x") (Write (Var (OnlyStr "x")))
      let machine = initialMachine {getInput = [IntVal 42]}
      let finalMachine = machine {getMem = scopeFromList [("x", IntVal 42)], getOutput = [IntVal 42], getInput = []}
      reduceFully term machine `shouldBe` (Right (IntVal 42), finalMachine)

    it "reduces subtraction" $ do
      let term = BinaryOps Sub (Literal 10) (Literal 3)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 7), initialMachine)

    it "reduces addition" $ do
      let term = BinaryOps Add (Literal 10) (Literal 3)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 13), initialMachine)

    it "reduces multiplication" $ do
      let term = BinaryOps Mul (Literal 10) (Literal 3)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 30), initialMachine)

    it "reduces division - nonzero denominator case" $ do
      let term = BinaryOps Div (Literal 12) (Literal 3)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 4), initialMachine)

    it "reduces division - zero denominator case" $ do
      let term = BinaryOps Div (Literal 12) (Literal 0)
      reduceFully term initialMachine `shouldBe` (Left "Cannot divide by 0", initialMachine)

    it "reduces modulus - nonzero denominator case" $ do
      let term = BinaryOps Mod (Literal 12) (Literal 3)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 0), initialMachine)

    it "reduces modulus - zero denominator case" $ do
      let term = BinaryOps Mod (Literal 12) (Literal 0)
      reduceFully term initialMachine `shouldBe` (Left "Cannot mod by 0", initialMachine)

    it "reduces skip" $ do
      let term = Skip
      reduceFully term initialMachine `shouldBe` (Right (IntVal 0), initialMachine)

    it "returns a Sad result for a type error" $ do
      let term = BinaryOps Sub (Literal 10) (StringLiteral "hello")
      let (result, _) = reduceFully term initialMachine
      result `shouldBe` Left "Type error in subtraction"

    it "try catch runs try statement if no error" $ do
      let term = Try (Literal 1) Any (Literal 2)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 1), initialMachine)

    it "try catch runs catch statement if error" $ do
      let term = Try (BinaryOps Div (Literal 1) (Literal 0)) (Specific Arithmetic) (Literal 2)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 2), initialMachine)

    it "try catch errors if both try and catch statements produce errors" $ do
      let term = Try (BinaryOps Add (Literal 1) (StringLiteral "a")) (Specific Type) (BinaryOps Div (Literal 1) (Literal 0))
      reduceFully term initialMachine `shouldBe` (Left "Cannot divide by 0", initialMachine)

    -- Logical Operations Tests
    it "reduces logical AND operation" $ do
      let term = BinaryOps And (BoolLit True) (BoolLit True)
      reduceFully term initialMachine `shouldBe` (Right (BoolVal True), initialMachine)

    it "reduces logical OR operation" $ do
      let term = BinaryOps Or (BoolLit False) (BoolLit True)
      reduceFully term initialMachine `shouldBe` (Right (BoolVal True), initialMachine)

    it "reduces logical NOT operation" $ do
      let term = UnaryOps Not (BoolLit False)
      reduceFully term initialMachine `shouldBe` (Right (BoolVal True), initialMachine)

    -- Complex Scenarios Tests
    it "reduces nested if statements" $ do
      let term =
            If
              (BinaryOps Gt (Literal 10) (Literal 5))
              ( If
                  (BinaryOps Lt (Literal 3) (Literal 7))
                  (Literal 1)
                  (Literal 2)
              )
              (Literal 3)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 1), initialMachine)

    it "reduces while loop with complex condition" $ do
      let term =
            Seq
              (Let (OnlyStr "x") (Literal 5))
              ( Seq
                  ( While
                      ( BinaryOps
                          And
                          (BinaryOps Gt (Var (OnlyStr "x")) (Literal 0))
                          (BinaryOps Lt (Var (OnlyStr "x")) (Literal 10))
                      )
                      (Let (OnlyStr "x") (BinaryOps Add (Var (OnlyStr "x")) (Literal 1)))
                  )
                  (Var (OnlyStr "x"))
              )
      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 10)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 10), finalMachine)

    it "reduces a while loop with a break statement" $ do
      let term =
            Seq
              (Let (OnlyStr "x") (Literal 5))
              ( Seq
                  ( While
                      (Var (OnlyStr "x"))
                      ( Seq
                          (If (BinaryOps Eq (Var (OnlyStr "x")) (Literal 3)) BreakSignal Skip)
                          (Let (OnlyStr "x") (BinaryOps Sub (Var (OnlyStr "x")) (Literal 1)))
                      )
                  )
                  (Var (OnlyStr "x"))
              )
      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 3)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 3), finalMachine)

    it "reduces a while loop with a complex break statement" $ do
      let term =
            Seq
              (Let (OnlyStr "x") (Literal 5))
              ( Seq
                  (Let (OnlyStr "y") (Literal 0))
                  ( Seq
                      ( While
                          (Var (OnlyStr "x"))
                          ( Seq
                              (Let (OnlyStr "x") (BinaryOps Sub (Var (OnlyStr "x")) (Literal 1)))
                              ( Seq
                                  (If (BinaryOps Eq (Var (OnlyStr "x")) (Literal 3)) BreakSignal Skip)
                                  (Let (OnlyStr "y") (BinaryOps Add (Var (OnlyStr "y")) (Var (OnlyStr "x"))))
                              )
                          )
                      )
                      (Var (OnlyStr "y"))
                  )
              )

      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 3), ("y", IntVal 4)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 4), finalMachine)

    it "reduces a while loop with a continue statement" $ do
      let term =
            Seq
              (Let (OnlyStr "x") (Literal 5))
              ( Seq
                  (Let (OnlyStr "y") (Literal 0))
                  ( Seq
                      ( While
                          (Var (OnlyStr "x"))
                          ( Seq
                              (Let (OnlyStr "x") (BinaryOps Sub (Var (OnlyStr "x")) (Literal 1)))
                              ( Seq
                                  (If (BinaryOps Eq (Var (OnlyStr "x")) (Literal 3)) ContinueSignal Skip)
                                  (Let (OnlyStr "y") (BinaryOps Add (Var (OnlyStr "y")) (Var (OnlyStr "x"))))
                              )
                          )
                      )
                      (Var (OnlyStr "y"))
                  )
              )

      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 0), ("y", IntVal 7)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 7), finalMachine)

    it "makes break signals outside of while loops invalid" $ do
      let term =
            Seq
              (Let (OnlyStr "x") (Literal 5))
              ( Seq
                  BreakSignal
                  (Let (OnlyStr "y") (BinaryOps Add (Var (OnlyStr "x")) (Literal 2)))
              )

      let (result, _) = reduceFully term initialMachine
      result `shouldBe` Left "unhandled break signal"

    it "makes continue signals outside of while loops invalid" $ do
      let term =
            Seq
              (Let (OnlyStr "x") (Literal 5))
              ( Seq
                  ContinueSignal
                  (Let (OnlyStr "y") (BinaryOps Add (Var (OnlyStr "x")) (Literal 2)))
              )

      let (result, _) = reduceFully term initialMachine
      result `shouldBe` Left "unhandled continue signal"

    it "break inside an if statement exits the while loop" $ do
      let term =
            Seq
              (Let (OnlyStr "x") (Literal 5))
              ( Seq
                  (Let (OnlyStr "y") (Literal 0))
                  ( While
                      (Var (OnlyStr "x"))
                      ( If
                          (BinaryOps Eq (Var (OnlyStr "x")) (Literal 3))
                          BreakSignal
                          ( Seq
                              (Let (OnlyStr "y") (BinaryOps Add (Var (OnlyStr "y")) (Var (OnlyStr "x"))))
                              (Let (OnlyStr "x") (BinaryOps Sub (Var (OnlyStr "x")) (Literal 1)))
                          )
                      )
                  )
              )

      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 3), ("y", IntVal 9)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 0), finalMachine)

    it "inner loop break exits only the inner loop" $ do
      let term =
            Seq
              (Let (OnlyStr "x") (Literal 3))
              ( Seq
                  (Let (OnlyStr "y") (Literal 0))
                  ( While
                      (Var (OnlyStr "x"))
                      ( Seq
                          (Let (OnlyStr "z") (Literal 2))
                          ( Seq
                              ( While
                                  (Var (OnlyStr "z"))
                                  ( If
                                      (BinaryOps Eq (Var (OnlyStr "z")) (Literal 1))
                                      BreakSignal
                                      ( Seq
                                          (Let (OnlyStr "y") (BinaryOps Add (Var (OnlyStr "y")) (Var (OnlyStr "z"))))
                                          (Let (OnlyStr "z") (BinaryOps Sub (Var (OnlyStr "z")) (Literal 1)))
                                      )
                                  )
                              )
                              (Let (OnlyStr "x") (BinaryOps Sub (Var (OnlyStr "x")) (Literal 1)))
                          )
                      )
                  )
              )

      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 0), ("y", IntVal 6), ("z", IntVal 1)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 0), finalMachine)

    it "inner loop continue skips to next iteration" $ do
      let term =
            Seq
              (Let (OnlyStr "x") (Literal 3))
              ( Seq
                  (Let (OnlyStr "y") (Literal 0))
                  ( While
                      (Var (OnlyStr "x"))
                      ( Seq
                          (Let (OnlyStr "z") (Literal 3))
                          ( Seq
                              ( While
                                  (Var (OnlyStr "z"))
                                  ( Seq
                                      (Let (OnlyStr "z") (BinaryOps Sub (Var (OnlyStr "z")) (Literal 1)))
                                      ( Seq
                                          (Let (OnlyStr "y") (BinaryOps Add (Var (OnlyStr "y")) (Var (OnlyStr "z"))))
                                          (If (BinaryOps Eq (Var (OnlyStr "z")) (Literal 2)) ContinueSignal Skip)
                                      )
                                  )
                              )
                              (Let (OnlyStr "x") (BinaryOps Sub (Var (OnlyStr "x")) (Literal 1)))
                          )
                      )
                  )
              )

      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 0), ("y", IntVal 9), ("z", IntVal 0)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 0), finalMachine)

    it "reduces combination of arithmetic and logical operations" $ do
      let term =
            BinaryOps
              And
              (BinaryOps Gt (BinaryOps Add (Literal 5) (Literal 5)) (Literal 8))
              (BinaryOps Lt (BinaryOps Mul (Literal 2) (Literal 3)) (Literal 7))
      reduceFully term initialMachine `shouldBe` (Right (BoolVal True), initialMachine)

    it "handles type error in comparison" $ do
      let term = BinaryOps Lt (Literal 5) (BoolLit True)
      let (result, _) = reduceFully term initialMachine
      result `shouldBe` Left "Type error in <"

    it "handles multiple variables in scope" $ do
      let term =
            Seq
              (Let (OnlyStr "x") (Literal 10))
              ( Seq
                  (Let (OnlyStr "y") (Literal 5))
                  (BinaryOps Add (Var (OnlyStr "x")) (Var (OnlyStr "y")))
              )
      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 10), ("y", IntVal 5)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 15), finalMachine)

    -- Function Application Tests
    it "invokes a zero-argument function" $ do
      let f0 = Fun [] (Literal 42)
      let term = ApplyFun f0 []
      reduceFully term initialMachine `shouldBe` (Right (IntVal 42), initialMachine)

    it "errors when invoking a function that expects arguments" $ do
      let f1 = Fun ["x"] (Var (OnlyStr "x"))
      let term = ApplyFun f1 []
      let (result, _) = reduceFully term initialMachine
      result `shouldBe` Left "missing arguments: function requires parameters"

    it "errors when applying an argument to a zero-arg function" $ do
      let f0 = Fun [] (Literal 1)
      let term = ApplyFun f0 [Literal 0]
      let (result, _) = reduceFully term initialMachine
      result `shouldBe` Left "too many arguments: function takes 0 arguments"

    it "applies a simple function" $ do
      let inc = Fun ["x"] (BinaryOps Add (Var (OnlyStr "x")) (Literal 1))
      let term = ApplyFun inc [Literal 41]
      reduceFully term initialMachine `shouldBe` (Right (IntVal 42), initialMachine)

    it "binds parameter in environment for body" $ do
      let f = Fun ["x"] (Var (OnlyStr "x"))
      let term = ApplyFun f [Literal 7]
      reduceFully term initialMachine `shouldBe` (Right (IntVal 7), initialMachine)

    it "applies a two-argument function" $ do
      let add2 = Fun ["x", "y"] (BinaryOps Add (Var (OnlyStr "x")) (Var (OnlyStr "y")))
      let term = ApplyFun add2 [Literal 2, Literal 3]
      reduceFully term initialMachine `shouldBe` (Right (IntVal 5), initialMachine)

    it "applies a three-argument function via currying" $ do
      let add3 = Fun ["x", "y", "z"] (BinaryOps Add (BinaryOps Add (Var (OnlyStr "x")) (Var (OnlyStr "y"))) (Var (OnlyStr "z")))
      let term = ApplyFun add3 [Literal 1, Literal 2, Literal 3]
      reduceFully term initialMachine `shouldBe` (Right (IntVal 6), initialMachine)

    it "errors when applying a non-function" $ do
      let term = ApplyFun (Literal 3) [Literal 4]
      let (result, _) = reduceFully term initialMachine
      result `shouldBe` Left "attempt to call a non-function"

    it "returns functions" $ do
      let f0 = Fun ["x"] (Literal 12)
      let f1 = Fun ["y"] f0
      let term = ApplyFun f1 [Literal 5]
      reduceFully term initialMachine `shouldBe` (Right (ClosureVal ["x"] (Literal 12) [("y", IntVal 5)]), initialMachine)

    it "creates local variables in functions" $ do
      let f = Fun ["y"] (Let (OnlyStr "x") (Var (OnlyStr "y"))) -- Should not affect outside x.
      let term = Seq (Let (OnlyStr "x") (Literal 1)) (ApplyFun f [Literal 99])
      let machine = initialMachine {getMem = scopeFromList [("x", IntVal 1)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 99), machine)

    -- Variable Capture Tests
    it "captures environment for zero-argument functions" $ do
      let f0 = Fun [] (Var (OnlyStr "outside"))
      let f1 = Fun ["y"] (Seq (Let (OnlyStr "outside") (Literal 99)) f0)
      let term = Seq (Let (OnlyStr "outside") (Literal 1)) (ApplyFun (ApplyFun f1 [Literal 0]) [])
      let machine = initialMachine {getMem = scopeFromList [("outside", IntVal 1)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 99), machine)

    it "captures environment for odd-argument functions" $ do
      let f0 = Fun ["x"] (Var (OnlyStr "outside")) -- Created inside of f1.
      let f1 = Fun ["y"] (Seq (Let (OnlyStr "outside") (Literal 99)) f0)
      -- (f1(0))(0) -> f0(0), where outside refers to the 99 captured in f1.
      let term = Seq (Let (OnlyStr "outside") (Literal 1)) (ApplyFun (ApplyFun f1 [Literal 0]) [Literal 0])
      let machine = initialMachine {getMem = scopeFromList [("outside", IntVal 1)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 99), machine)

    it "captures environment for even-argument functions" $ do
      let f0 = Fun ["x", "z"] (Var (OnlyStr "outside"))
      let f1 = Fun ["y"] (Seq (Let (OnlyStr "outside") (Literal 99)) f0)
      let term = Seq (Let (OnlyStr "outside") (Literal 1)) (ApplyFun (ApplyFun f1 [Literal 0]) [Literal 0, Literal 0])
      let machine = initialMachine {getMem = scopeFromList [("outside", IntVal 1)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 99), machine)

    it "captures only the environment at function creation time" $ do
      let f0 = Fun ["x"] (Var (OnlyStr "outside"))
      let setupTerm = Seq (Let (OnlyStr "outside") (Literal 1)) (Let (OnlyStr "f") f0) -- Function f created here.
      let f1 = Fun ["y"] (Seq (Let (OnlyStr "outside") (Literal 99)) (ApplyFun (Var (OnlyStr "f")) [Literal 0])) -- 99 should not be captured.
      let term = Seq setupTerm (ApplyFun f1 [Literal 0])
      let closureVal = ClosureVal ["x"] (Var (OnlyStr "outside")) [("outside", IntVal 1)] -- Captured 1 from outside.
      let machine = initialMachine {getMem = scopeFromList [("outside", IntVal 1), ("f", closureVal)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 1), machine)

    it "captures all variables in nested scopes" $ do
      let f0 = Fun ["x"] (BinaryOps Add (Var (OnlyStr "a")) (Var (OnlyStr "b")))
      let f1 = Fun ["y"] (Seq (Let (OnlyStr "b") (Literal 4)) f0) -- b created (parent of f0).
      let f2 = Fun ["z"] (Seq (Let (OnlyStr "a") (Literal 3)) f1) -- a created (parent of f1).
      let term = ApplyFun (ApplyFun (ApplyFun f2 [Literal 0]) [Literal 0]) [Literal 0]
      reduceFully term initialMachine `shouldBe` (Right (IntVal 7), initialMachine)

    it "handles parameter shadowing" $ do
      let f = Fun ["x"] (Var (OnlyStr "x"))
      let term = Seq (Let (OnlyStr "x") (Literal 1)) (ApplyFun f [Literal 5]) -- Parameter x is 5.
      let machine = initialMachine {getMem = scopeFromList [("x", IntVal 1)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 5), machine)

    it "errors on undefined variable in function scope" $ do
      let f1 = Fun [] (Seq (Let (OnlyStr "y") (Literal 3)) (ApplyFun (Var (OnlyStr "f")) []))
      let term = Seq (Let (OnlyStr "f") (Fun [] (Write (Var (OnlyStr "y"))))) (ApplyFun f1 []) -- f defined outside of f1.
      let machine = initialMachine {getMem = scopeFromList [("f", ClosureVal [] (Write (Var (OnlyStr "y"))) [])]}
      reduceFully term initialMachine `shouldBe` (Left "variable not found", machine) -- Does not capture y = 3.

    -- Comparison Operations Tests
    it "reduces less than comparison" $ do
      let term = BinaryOps Lt (Literal 5) (Literal 10)
      reduceFully term initialMachine `shouldBe` (Right (BoolVal True), initialMachine)

    it "reduces greater than comparison" $ do
      let term = BinaryOps Gt (Literal 10) (Literal 5)
      reduceFully term initialMachine `shouldBe` (Right (BoolVal True), initialMachine)

    it "reduces less than or equal comparison" $ do
      let term = BinaryOps Lte (Literal 5) (Literal 5)
      reduceFully term initialMachine `shouldBe` (Right (BoolVal True), initialMachine)

    it "reduces greater than or equal comparison" $ do
      let term = BinaryOps Gte (Literal 10) (Literal 5)
      reduceFully term initialMachine `shouldBe` (Right (BoolVal True), initialMachine)

    it "reduces equality comparison for integers" $ do
      let term = BinaryOps Eq (Literal 5) (Literal 5)
      reduceFully term initialMachine `shouldBe` (Right (BoolVal True), initialMachine)

    it "reduces equality comparison for booleans" $ do
      let term = BinaryOps Eq (BoolLit True) (BoolLit True)
      reduceFully term initialMachine `shouldBe` (Right (BoolVal True), initialMachine)

    it "reduces equality comparison for strings" $ do
      let term = BinaryOps Eq (StringLiteral "hello") (StringLiteral "hello")
      reduceFully term initialMachine `shouldBe` (Right (BoolVal True), initialMachine)

    it "reduces inequality comparison" $ do
      let term = BinaryOps Neq (Literal 5) (Literal 10)
      reduceFully term initialMachine `shouldBe` (Right (BoolVal True), initialMachine)

    -- Dictionary and Tuple Test Cases

    it "reduces a tuple" $ do
      let term = TupleTerm [Literal 10, StringLiteral "hello", BoolLit True]
      let (result, _) = reduceFully term initialMachine
      result `shouldBe` Right (Tuple [IntVal 10, StringVal "hello", BoolVal True])

    it "reduces a let tuple expression" $ do
      let term = Seq (Let (OnlyStr "x") (TupleTerm [Literal 10, StringLiteral "hello", BoolLit True])) (Let (Bracket (OnlyStr "x") (Literal 2)) (BoolLit False))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Tuple [IntVal 10, StringVal "hello", BoolVal False])]}
      reduceFully term initialMachine `shouldBe` (Right (Tuple [IntVal 10, StringVal "hello", BoolVal False]), finalMachine)

    it "reduces a let nested tuple expression" $ do
      let term = Seq (Let (OnlyStr "x") (TupleTerm [Literal 10, TupleTerm [StringLiteral "hello"], BoolLit True])) (Let (Bracket (Bracket (OnlyStr "x") (Literal 1)) (Literal 0)) (StringLiteral "goodbye"))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Tuple [IntVal 10, Tuple [StringVal "goodbye"], BoolVal True])]}
      reduceFully term initialMachine `shouldBe` (Right (Tuple [IntVal 10, Tuple [StringVal "goodbye"], BoolVal True]), finalMachine)

    it "let tuple above bounds" $ do
      let term = Seq (Let (OnlyStr "x") (TupleTerm [Literal 10, StringLiteral "hello", BoolLit True])) (Let (Bracket (OnlyStr "x") (Literal 3)) (BoolLit False))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Tuple [IntVal 10, StringVal "hello", BoolVal True])]}
      reduceFully term initialMachine `shouldBe` (Left "Attempting to set value Out of Bounds", finalMachine)

    it "access a tuple" $ do
      let term = Seq (Let (OnlyStr "x") (TupleTerm [Literal 10, StringLiteral "hello", BoolLit True])) (Var (Bracket (OnlyStr "x") (Literal 2)))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Tuple [IntVal 10, StringVal "hello", BoolVal True])]}
      reduceFully term initialMachine `shouldBe` (Right (BoolVal True), finalMachine)

    it "access a nested tuple" $ do
      let term = Seq (Let (OnlyStr "x") (TupleTerm [Literal 10, TupleTerm [Literal 1, StringLiteral "hello"], BoolLit True])) (Var (Bracket (Bracket (OnlyStr "x") (Literal 1)) (Literal 0)))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Tuple [IntVal 10, Tuple [IntVal 1, StringVal "hello"], BoolVal True])]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 1), finalMachine)

    it "access tuple above bounds" $ do
      let term = Seq (Let (OnlyStr "x") (TupleTerm [Literal 1, Literal 2, Literal 3])) (Var (Bracket (OnlyStr "x") (Literal 3)))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Tuple [IntVal 1, IntVal 2, IntVal 3])]}
      reduceFully term initialMachine `shouldBe` (Left "Out of Bounds", finalMachine)

    it "access tuple below bounds" $ do
      let term = Seq (Let (OnlyStr "x") (TupleTerm [Literal 1, Literal 2, Literal 3])) (Var (Bracket (OnlyStr "x") (Literal (-1))))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Tuple [IntVal 1, IntVal 2, IntVal 3])]}
      reduceFully term initialMachine `shouldBe` (Left "Out of Bounds", finalMachine)

    it "reduces new dictionary" $ do
      let term = NewDictionary
      reduceFully term initialMachine `shouldBe` (Right (Dictionary M.empty), initialMachine)

    it "set dictionary" $ do
      let term = Seq (Let (OnlyStr "x") NewDictionary) (Let (Bracket (OnlyStr "x") (Literal 3)) (StringLiteral "hello"))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Dictionary (M.fromList [(3, StringVal "hello")]))]}
      reduceFully term initialMachine `shouldBe` (Right (Dictionary (M.fromList [(3, StringVal "hello")])), finalMachine)

    it "set nested dictionary" $ do
      let term = Seq (Let (OnlyStr "x") NewDictionary) (Seq (Let (Bracket (OnlyStr "x") (Literal 3)) NewDictionary) (Let (Bracket (Bracket (OnlyStr "x") (Literal 3)) (Literal 4)) (StringLiteral "hello")))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Dictionary (M.fromList [(3, Dictionary (M.fromList [(4, StringVal "hello")]))]))]}
      reduceFully term initialMachine `shouldBe` (Right (Dictionary (M.fromList [(3, Dictionary (M.fromList [(4, StringVal "hello")]))])), finalMachine)

    it "access dictionary" $ do
      let term = Seq (Let (OnlyStr "x") NewDictionary) (Seq (Let (Bracket (OnlyStr "x") (Literal 3)) (StringLiteral "hello")) (Var (Bracket (OnlyStr "x") (Literal 3))))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Dictionary (M.fromList [(3, StringVal "hello")]))]}
      reduceFully term initialMachine `shouldBe` (Right (StringVal "hello"), finalMachine)

    it "access nested dictionary" $ do
      let term = Seq (Let (OnlyStr "x") NewDictionary) (Seq (Let (Bracket (OnlyStr "x") (Literal 3)) NewDictionary) (Seq (Let (Bracket (Bracket (OnlyStr "x") (Literal 3)) (Literal 4)) (StringLiteral "hello")) (Var (Bracket (Bracket (OnlyStr "x") (Literal 3)) (Literal 4)))))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Dictionary (M.fromList [(3, Dictionary (M.fromList [(4, StringVal "hello")]))]))]}
      reduceFully term initialMachine `shouldBe` (Right (StringVal "hello"), finalMachine)

    it "access dictionary nonexistant entry" $ do
      let term = Seq (Let (OnlyStr "x") NewDictionary) (Seq (Let (Bracket (OnlyStr "x") (Literal 3)) (StringLiteral "hello")) (Var (Bracket (OnlyStr "x") (Literal 1))))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Dictionary (M.fromList [(3, StringVal "hello")]))]}
      reduceFully term initialMachine `shouldBe` (Left "Unable to find element in dictionary", finalMachine)

    it "access nested dictionary-tuple" $ do
      let term = Seq (Let (OnlyStr "x") NewDictionary) (Seq (Let (Bracket (OnlyStr "x") (Literal 3)) (TupleTerm [Literal 0, Literal 1, StringLiteral "hello"])) (Var (Bracket (Bracket (OnlyStr "x") (Literal 3)) (Literal 2))))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Dictionary (M.fromList [(3, Tuple [IntVal 0, IntVal 1, StringVal "hello"])]))]}
      reduceFully term initialMachine `shouldBe` (Right (StringVal "hello"), finalMachine)

    -- Ternary Operator Tests
    it "reduces ternary operator with true condition" $ do
      let term = If (BoolLit True) (Literal 10) (Literal 20)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 10), initialMachine)

    it "reduces ternary operator with false condition" $ do
      let term = If (BoolLit False) (Literal 10) (Literal 20)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 20), initialMachine)

    it "reduces ternary operator with integer condition (non-zero)" $ do
      let term = If (Literal 5) (StringLiteral "true") (StringLiteral "false")
      reduceFully term initialMachine `shouldBe` (Right (StringVal "true"), initialMachine)

    it "reduces ternary operator with integer condition (zero)" $ do
      let term = If (Literal 0) (StringLiteral "true") (StringLiteral "false")
      reduceFully term initialMachine `shouldBe` (Right (StringVal "false"), initialMachine)

    it "reduces ternary operator with string condition (non-empty)" $ do
      let term = If (StringLiteral "hello") (Literal 100) (Literal 200)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 100), initialMachine)

    it "reduces ternary operator with string condition (empty)" $ do
      let term = If (StringLiteral "") (Literal 100) (Literal 200)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 200), initialMachine)

    it "reduces ternary operator with comparison condition" $ do
      let term = If (BinaryOps Gt (Literal 10) (Literal 5)) (StringLiteral "greater") (StringLiteral "not greater")
      reduceFully term initialMachine `shouldBe` (Right (StringVal "greater"), initialMachine)

    it "reduces nested ternary operators" $ do
      let term = If (BoolLit True) (If (BoolLit False) (Literal 1) (Literal 2)) (Literal 3)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 2), initialMachine)

    it "reduces ternary operator with variable access" $ do
      let term = Seq (Let (OnlyStr "x") (Literal 10)) (If (BinaryOps Gt (Var (OnlyStr "x")) (Literal 5)) (Var (OnlyStr "x")) (Literal 0))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 10)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 10), finalMachine)

    it "reduces ternary operator with complex expressions" $ do
      let term =
            If
              (BinaryOps And (BoolLit True) (BoolLit False))
              (BinaryOps Add (Literal 10) (Literal 5))
              (BinaryOps Mul (Literal 3) (Literal 4))
      reduceFully term initialMachine `shouldBe` (Right (IntVal 12), initialMachine)

    it "reduces right-associative ternary operators" $ do
      let term = If (BoolLit False) (Literal 1) (If (BoolLit True) (Literal 2) (Literal 3))
      reduceFully term initialMachine `shouldBe` (Right (IntVal 2), initialMachine)

    it "handles ternary operator precedence correctly" $ do
      let term = If (BinaryOps Gt (BinaryOps Add (Literal 1) (Literal 2)) (Literal 2)) (Literal 10) (Literal 20)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 10), initialMachine)

    it "evaluates chained ternary operators" $ do
      let chainedTernary = If (BoolLit True) (Literal 1) (If (BoolLit False) (Literal 2) (Literal 3))
      reduceFully chainedTernary initialMachine `shouldBe` (Right (IntVal 1), initialMachine)

    it "reduces ternary operator with complex expressions" $ do
      let term =
            If
              (BinaryOps And (BinaryOps Gt (Literal 10) (Literal 5)) (BinaryOps Lt (Literal 3) (Literal 7)))
              (BinaryOps Add (Literal 1) (Literal 2))
              (BinaryOps Mul (Literal 3) (Literal 4))
      reduceFully term initialMachine `shouldBe` (Right (IntVal 3), initialMachine)

    it "reduces right-associative ternary operators" $ do
      -- This represents: false ? 1 : (true ? 2 : 3) which should equal 2
      let term = If (BoolLit False) (Literal 1) (If (BoolLit True) (Literal 2) (Literal 3))
      reduceFully term initialMachine `shouldBe` (Right (IntVal 2), initialMachine)

    it "handles ternary operator precedence correctly" $ do
      -- This represents: (1 + 2) > 2 ? 10 : 20, which should equal 10
      let term = If (BinaryOps Gt (BinaryOps Add (Literal 1) (Literal 2)) (Literal 2)) (Literal 10) (Literal 20)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 10), initialMachine)

    it "reduces chained ternary operators (right-associative)" $ do
      -- This represents: true ? 1 : (false ? 2 : 3) which should equal 1
      let chainedTernary = If (BoolLit True) (Literal 1) (If (BoolLit False) (Literal 2) (Literal 3))
      reduceFully chainedTernary initialMachine `shouldBe` (Right (IntVal 1), initialMachine)

    it "handles invalid term in bracket lookup" $ do
      let term = Seq (Let (OnlyStr "x") (TupleTerm [Literal 10, StringLiteral "hello", BoolLit True])) (Var (Bracket (OnlyStr "x") (BinaryOps Div (Literal 2) (Literal 0))))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Tuple [IntVal 10, StringVal "hello", BoolVal True])]}
      reduceFully term initialMachine `shouldBe` (Left "Cannot divide by 0", finalMachine)

    -- ForIn Loop Tests
    describe "ForIn loops" $ do
      it "iterates over tuple elements" $ do
        let term =
              Seq
                (Let (OnlyStr "sum") (Literal 0))
                ( ForIn
                    "i"
                    (TupleTerm [Literal 1, Literal 2, Literal 3])
                    (Let (OnlyStr "sum") (BinaryOps Add (Var (OnlyStr "sum")) (Var (OnlyStr "i"))))
                )
        let finalMachine = initialMachine {getMem = scopeFromList [("sum", IntVal 6), ("i", IntVal 3)]}
        let (result, machine) = reduceFully term initialMachine
        result `shouldBe` Right (IntVal 6)
        getMem machine `shouldBe` getMem finalMachine

      it "handles empty tuple" $ do
        let term = ForIn "i" (TupleTerm []) (Write (Var (OnlyStr "i")))
        reduceFully term initialMachine `shouldBe` (Right (IntVal 0), initialMachine)

      it "iterates over nested tuples" $ do
        let term =
              Seq
                (Let (OnlyStr "result") (Literal 0))
                ( ForIn
                    "t"
                    (TupleTerm [TupleTerm [Literal 1, Literal 2], TupleTerm [Literal 3, Literal 4]])
                    (Let (OnlyStr "result") (BinaryOps Add (Var (OnlyStr "result")) (Literal 1)))
                )
        let (result, _) = reduceFully term initialMachine
        result `shouldBe` Right (IntVal 2)

      it "errors on non-iterable value" $ do
        let term = ForIn "i" (Literal 42) (Write (Var (OnlyStr "i")))
        let (result, _) = reduceFully term initialMachine
        result `shouldBe` Left "for-in requires iterable (tuple, string, dictionary, or [iterator, state])"

      it "errors on iterator with wrong arity" $ do
        let iterator = Fun ["x", "y"] (Var (OnlyStr "x"))  -- 2 params instead of 1
        let term = ForIn "i" (TupleTerm [iterator, Literal 0]) (Write (Var (OnlyStr "i")))
        let (result, _) = reduceFully term initialMachine
        result `shouldBe` Left "iterator must take exactly 1 parameter"

      it "supports break in for-in loop" $ do
        let term =
              Seq
                (Let (OnlyStr "sum") (Literal 0))
                ( ForIn
                    "i"
                    (TupleTerm [Literal 1, Literal 2, Literal 3, Literal 4])
                    ( Seq
                        (If (BinaryOps Eq (Var (OnlyStr "i")) (Literal 3)) BreakSignal Skip)
                        (Let (OnlyStr "sum") (BinaryOps Add (Var (OnlyStr "sum")) (Var (OnlyStr "i"))))
                    )
                )
        let finalMachine = initialMachine {getMem = scopeFromList [("sum", IntVal 3), ("i", IntVal 3)]}
        let (result, machine) = reduceFully term initialMachine
        result `shouldBe` Right (IntVal 0) -- break returns 0
        getMem machine `shouldBe` getMem finalMachine

      it "supports continue in for-in loop" $ do
        let term =
              Seq
                (Let (OnlyStr "sum") (Literal 0))
                ( ForIn
                    "i"
                    (TupleTerm [Literal 1, Literal 2, Literal 3, Literal 4])
                    ( Seq
                        (If (BinaryOps Eq (Var (OnlyStr "i")) (Literal 2)) ContinueSignal Skip)
                        (Let (OnlyStr "sum") (BinaryOps Add (Var (OnlyStr "sum")) (Var (OnlyStr "i"))))
                    )
                )
        let finalMachine = initialMachine {getMem = scopeFromList [("sum", IntVal 8), ("i", IntVal 4)]} -- 1+3+4
        let (result, machine) = reduceFully term initialMachine
        result `shouldBe` Right (IntVal 8)
        getMem machine `shouldBe` getMem finalMachine

      it "handles single element tuple" $ do
        let term = ForIn "x" (TupleTerm [Literal 42]) (Write (Var (OnlyStr "x")))
        let (result, machine) = reduceFully term initialMachine
        result `shouldBe` Right (IntVal 42)
        getMem machine `shouldBe` scopeFromList [("x", IntVal 42)]

      it "handles tuple with mixed types" $ do
        let term = ForIn "x" (TupleTerm [Literal 1, BoolLit True, StringLiteral "hi"]) Skip
        let (result, _) = reduceFully term initialMachine
        result `shouldBe` Right (IntVal 0)

      it "supports nested for-in loops" $ do
        let outer = ForIn "i" (TupleTerm [Literal 1, Literal 2]) innerLoop
            innerLoop = ForIn "j" (TupleTerm [Literal 10, Literal 20]) (Let (OnlyStr "count") (BinaryOps Add (Var (OnlyStr "count")) (Literal 1)))
            term = Seq (Let (OnlyStr "count") (Literal 0)) outer
        let (result, machine) = reduceFully term initialMachine
        result `shouldBe` Right (IntVal 4)
        getMem machine `shouldBe` scopeFromList [("count", IntVal 4), ("i", IntVal 2), ("j", IntVal 20)]

      it "propagates error from iterable expression" $ do
        let term = ForIn "i" (BinaryOps Div (Literal 1) (Literal 0)) (Write (Var (OnlyStr "i")))
        let (result, _) = reduceFully term initialMachine
        result `shouldSatisfy` isLeft

    describe "range built-in" $ do
      it "range(0) produces empty iteration" $ do
        let term = Seq (Let (OnlyStr "count") (Literal 0)) (ForIn "i" (Range (Literal 0)) (Let (OnlyStr "count") (BinaryOps Add (Var (OnlyStr "count")) (Literal 1))))
        let (result, machine) = reduceFully term initialMachine
        result `shouldBe` Right (IntVal 0)
        getMem machine `shouldBe` scopeFromList [("count", IntVal 0)]

      it "range(1) yields single value 0" $ do
        let term = Seq (Let (OnlyStr "sum") (Literal 0)) (ForIn "i" (Range (Literal 1)) (Let (OnlyStr "sum") (BinaryOps Add (Var (OnlyStr "sum")) (Var (OnlyStr "i")))))
        let (result, machine) = reduceFully term initialMachine
        result `shouldBe` Right (IntVal 0) -- sum = 0
        getMem machine `shouldSatisfy` \scope -> lookupScope "sum" scope == Just (IntVal 0) && lookupScope "i" scope == Just (IntVal 0)

      it "range(5) yields 0 through 4" $ do
        let term = Seq (Let (OnlyStr "sum") (Literal 0)) (ForIn "i" (Range (Literal 5)) (Let (OnlyStr "sum") (BinaryOps Add (Var (OnlyStr "sum")) (Var (OnlyStr "i")))))
        let (result, machine) = reduceFully term initialMachine
        result `shouldBe` Right (IntVal 10) -- 0+1+2+3+4 = 10
        getMem machine `shouldSatisfy` \scope -> lookupScope "sum" scope == Just (IntVal 10)

      it "range with break" $ do
        let term =
              Seq
                (Let (OnlyStr "sum") (Literal 0))
                ( ForIn
                    "i"
                    (Range (Literal 10))
                    (Seq (If (BinaryOps Eq (Var (OnlyStr "i")) (Literal 3)) BreakSignal Skip) (Let (OnlyStr "sum") (BinaryOps Add (Var (OnlyStr "sum")) (Var (OnlyStr "i")))))
                )
        let (result, machine) = reduceFully term initialMachine
        result `shouldBe` Right (IntVal 0) -- break returns 0
        getMem machine `shouldSatisfy` \scope -> lookupScope "sum" scope == Just (IntVal 3) -- 0+1+2

      it "range with continue" $ do
        let term =
              Seq
                (Let (OnlyStr "sum") (Literal 0))
                ( ForIn
                    "i"
                    (Range (Literal 5))
                    (Seq (If (BinaryOps Eq (Var (OnlyStr "i")) (Literal 2)) ContinueSignal Skip) (Let (OnlyStr "sum") (BinaryOps Add (Var (OnlyStr "sum")) (Var (OnlyStr "i")))))
                )
        let (result, machine) = reduceFully term initialMachine
        result `shouldBe` Right (IntVal 8) -- 0+1+3+4 = 8
        getMem machine `shouldSatisfy` \scope -> lookupScope "sum" scope == Just (IntVal 8)

      it "range with non-integer errors" $ do
        let term = ForIn "i" (Range (StringLiteral "hello")) (Write (Var (OnlyStr "i")))
        let (result, _) = reduceFully term initialMachine
        result `shouldBe` Left "range requires an integer argument"

      it "nested range loops" $ do
        let term =
              Seq
                (Let (OnlyStr "sum") (Literal 0))
                (ForIn "i" (Range (Literal 3)) (ForIn "j" (Range (Literal 2)) (Let (OnlyStr "sum") (BinaryOps Add (Var (OnlyStr "sum")) (Literal 1)))))
        let (result, machine) = reduceFully term initialMachine
        result `shouldBe` Right (IntVal 6) -- 3 * 2 = 6 iterations
        getMem machine `shouldSatisfy` \scope -> lookupScope "sum" scope == Just (IntVal 6)

      it "iterates using custom iterator protocol with [iterator, state]" $ do
        -- Iterator that counts from initialState+1 up to 3
        -- iterator(n) returns [n+1, n+1] if n+1 <= 3, else StopIteration
        let iterator =
              Fun
                ["n"]
                ( If
                    (BinaryOps Gt (BinaryOps Add (Var (OnlyStr "n")) (Literal 1)) (Literal 3))
                    StopIteration
                    (TupleTerm [BinaryOps Add (Var (OnlyStr "n")) (Literal 1), BinaryOps Add (Var (OnlyStr "n")) (Literal 1)])
                )
            loop =
              ForIn
                "i"
                (TupleTerm [iterator, Literal 0])  -- [iterator, initialState]
                (Let (OnlyStr "sum") (BinaryOps Add (Var (OnlyStr "sum")) (Var (OnlyStr "i"))))
            term = Seq (Let (OnlyStr "sum") (Literal 0)) loop
        -- Iterator yields 1, 2, 3, then StopIteration
        let (result, machine) = reduceFully term initialMachine
        result `shouldBe` Right (IntVal 6) -- 1 + 2 + 3 = 6
        getMem machine `shouldSatisfy` \scope -> lookupScope "sum" scope == Just (IntVal 6)

    describe "string iteration" $ do
      it "iterates over characters in a string" $ do
        let term =
              Seq
                (Let (OnlyStr "count") (Literal 0))
                (ForIn "c" (StringLiteral "abc") (Let (OnlyStr "count") (BinaryOps Add (Var (OnlyStr "count")) (Literal 1))))
        let (result, machine) = reduceFully term initialMachine
        result `shouldBe` Right (IntVal 3)
        getMem machine `shouldSatisfy` \scope -> lookupScope "count" scope == Just (IntVal 3) && lookupScope "c" scope == Just (StringVal "c")

      it "handles empty string" $ do
        let term =
              Seq
                (Let (OnlyStr "count") (Literal 0))
                (ForIn "c" (StringLiteral "") (Let (OnlyStr "count") (BinaryOps Add (Var (OnlyStr "count")) (Literal 1))))
        let (result, machine) = reduceFully term initialMachine
        result `shouldBe` Right (IntVal 0)
        getMem machine `shouldSatisfy` \scope -> lookupScope "count" scope == Just (IntVal 0)

      it "handles single character string" $ do
        let term = ForIn "c" (StringLiteral "x") (Write (Var (OnlyStr "c")))
        let (result, machine) = reduceFully term initialMachine
        result `shouldBe` Right (StringVal "x")
        getMem machine `shouldBe` scopeFromList [("c", StringVal "x")]

      it "supports break in string iteration" $ do
        let term =
              Seq
                (Let (OnlyStr "count") (Literal 0))
                ( ForIn
                    "c"
                    (StringLiteral "hello")
                    ( Seq
                        (If (BinaryOps Eq (Var (OnlyStr "c")) (StringLiteral "l")) BreakSignal Skip)
                        (Let (OnlyStr "count") (BinaryOps Add (Var (OnlyStr "count")) (Literal 1)))
                    )
                )
        let (result, machine) = reduceFully term initialMachine
        result `shouldBe` Right (IntVal 0) -- break returns 0
        getMem machine `shouldSatisfy` \scope -> lookupScope "count" scope == Just (IntVal 2) -- counted 'h' and 'e'

    describe "dictionary iteration" $ do
      it "iterates over dictionary keys" $ do
        let setupDict =
              Seq
                (Let (OnlyStr "d") NewDictionary)
                ( Seq
                    (Let (Bracket (OnlyStr "d") (Literal 1)) (Literal 10))
                    ( Seq
                        (Let (Bracket (OnlyStr "d") (Literal 2)) (Literal 20))
                        (Let (Bracket (OnlyStr "d") (Literal 3)) (Literal 30))
                    )
                )
            loop =
              ForIn
                "key"
                (Var (OnlyStr "d"))
                (Let (OnlyStr "count") (BinaryOps Add (Var (OnlyStr "count")) (Literal 1)))
            term = Seq setupDict (Seq (Let (OnlyStr "count") (Literal 0)) loop)
        let (result, machine) = reduceFully term initialMachine
        result `shouldBe` Right (IntVal 3)
        getMem machine `shouldSatisfy` \scope -> lookupScope "count" scope == Just (IntVal 3)

      it "handles empty dictionary" $ do
        let term =
              Seq
                (Let (OnlyStr "d") NewDictionary)
                ( Seq
                    (Let (OnlyStr "count") (Literal 0))
                    (ForIn "key" (Var (OnlyStr "d")) (Let (OnlyStr "count") (BinaryOps Add (Var (OnlyStr "count")) (Literal 1))))
                )
        let (result, machine) = reduceFully term initialMachine
        result `shouldBe` Right (IntVal 0)
        getMem machine `shouldSatisfy` \scope -> lookupScope "count" scope == Just (IntVal 0)


