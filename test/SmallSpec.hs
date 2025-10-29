{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SmallSpec (spec) where

import qualified Control.Monad.State as S
import Data.Bits (complement)
import qualified Data.Map as M
import Machine (Error, Machine (..), Result (..))
import Small
import Term
import Test.Hspec
import TypeSignature (TypeSignature (..), TypedName)
import Value (Scope (..), Value (..), emptyScope, insertScope, lookupScope, scopeFromList)

typedName :: String -> TypedName
typedName name = (name, TUnknown)

onlyStr :: String -> Term
onlyStr name = OnlyStr (name, TUnknown)

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

  getScope = getMem

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

  setScope newScope = do
    m <- S.get
    S.put (m {getMem = newScope})
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

  preIncrementVal (x, _) = do
    m <- S.get
    case lookupScope x (getMem m) of
      Just (IntVal v) -> do
        let newVal = IntVal (v + 1)
        S.put (m {getMem = insertScope x newVal (getMem m)})
        return $ Happy newVal
      Just _ -> return $ Sad (Type, "Type error: can only increment integers")
      Nothing -> return $ Sad (VariableNotFound, "Variable " ++ x ++ " not found")

  preDecrementVal (x, _) = do
    m <- S.get
    case lookupScope x (getMem m) of
      Just (IntVal v) -> do
        let newVal = IntVal (v - 1)
        S.put (m {getMem = insertScope x newVal (getMem m)})
        return $ Happy newVal
      Just _ -> return $ Sad (Type, "Type error: can only decrement integers")
      Nothing -> return $ Sad (VariableNotFound, "Variable " ++ x ++ " not found")

  postIncrementVal (x, _) = do
    m <- S.get
    case lookupScope x (getMem m) of
      Just (IntVal v) -> do
        let newVal = IntVal (v + 1)
        S.put (m {getMem = insertScope x newVal (getMem m)})
        return $ Happy (IntVal v) -- Return old value
      Just _ -> return $ Sad (Type, "Type error: can only increment integers")
      Nothing -> return $ Sad (VariableNotFound, "Variable " ++ x ++ " not found")

  postDecrementVal (x, _) = do
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
  selectValue UnitVal _ t = t -- unit is falsy
  selectValue (Tuple l) c t = if not (null l) then c else t
  selectValue ClosureVal {} _ _ = return $ Sad (Type, "Type error in select")
  selectValue (Dictionary _) _ _ = return $ Sad (Type, "Type error in select")

checkFailure :: (Either String Value, MockMachine) -> MockMachine -> IO ()
checkFailure (Left _, fm) machine = do
  fm `shouldBe` machine
checkFailure (Right _, _) _ =
  expectationFailure "Expected failure but got success"

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
      let term = Var (onlyStr "x")
      let machine = initialMachine {getMem = scopeFromList [("x", IntVal 5)]}
      reduceFully term machine `shouldBe` (Right (IntVal 5), machine)

    it "reduces a let expression" $ do
      let term = Seq (Let (onlyStr "x") (Literal 5)) (Var (onlyStr "x"))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 5)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 5), finalMachine)

    it "reduces a sequence" $ do
      let term = Seq (Let (onlyStr "x") (Literal 5)) (BinaryOps Add (Var (onlyStr "x")) (Literal 1))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 5)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 6), finalMachine)

    it "reduces an if expression (then)" $ do
      let term = If (BoolLit True) (Literal 10) (Literal 20)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 10), initialMachine)

    it "reduces an if expression (else)" $ do
      let term = If (BoolLit False) (Literal 10) (Literal 20)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 20), initialMachine)

    it "reduces a while loop" $ do
      let term = Seq (Let (onlyStr "x") (Literal 3)) (While (Var (onlyStr "x")) (Let (onlyStr "x") (BinaryOps Sub (Var (onlyStr "x")) (Literal 1))) Nothing Nothing)
      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 3)]}
      checkFailure (reduceFully term initialMachine) finalMachine

    it "reduces read and write" $ do
      let term = Seq (Read $ typedName "x") (Write (Var (onlyStr "x")))
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
      reduceFully term initialMachine `shouldBe` (Right UnitVal, initialMachine)

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
              (Let (onlyStr "x") (Literal 5))
              ( Seq
                  ( While
                      ( BinaryOps
                          And
                          (BinaryOps Gt (Var (onlyStr "x")) (Literal 0))
                          (BinaryOps Lt (Var (onlyStr "x")) (Literal 10))
                      )
                      (Let (onlyStr "x") (BinaryOps Add (Var (onlyStr "x")) (Literal 1)))
                      Nothing
                      Nothing
                  )
                  (Var (onlyStr "x"))
              )
      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 10)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 10), finalMachine)

    it "reduces a while loop with a break statement" $ do
      let term =
            Seq
              (Let (onlyStr "x") (Literal 5))
              ( Seq
                  ( While
                      (BinaryOps Gt (Var (onlyStr "x")) (Literal 0))
                      ( Seq
                          (If (BinaryOps Eq (Var (onlyStr "x")) (Literal 3)) BreakSignal Skip)
                          (Let (onlyStr "x") (BinaryOps Sub (Var (onlyStr "x")) (Literal 1)))
                      )
                      Nothing
                      Nothing
                  )
                  (Var (onlyStr "x"))
              )
      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 3)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 3), finalMachine)

    it "reduces a while loop with a complex break statement" $ do
      let term =
            Seq
              (Let (onlyStr "x") (Literal 5))
              ( Seq
                  (Let (onlyStr "y") (Literal 0))
                  ( Seq
                      ( While
                          (BinaryOps Gt (Var (onlyStr "x")) (Literal 0))
                          ( Seq
                              (Let (onlyStr "x") (BinaryOps Sub (Var (onlyStr "x")) (Literal 1)))
                              ( Seq
                                  (If (BinaryOps Eq (Var (onlyStr "x")) (Literal 3)) BreakSignal Skip)
                                  (Let (onlyStr "y") (BinaryOps Add (Var (onlyStr "y")) (Var (onlyStr "x"))))
                              )
                          )
                          Nothing
                          Nothing
                      )
                      (Var (onlyStr "y"))
                  )
              )

      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 3), ("y", IntVal 4)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 4), finalMachine)

    it "reduces a while loop with a continue statement" $ do
      let term =
            Seq
              (Let (onlyStr "x") (Literal 5))
              ( Seq
                  (Let (onlyStr "y") (Literal 0))
                  ( Seq
                      ( While
                          (BinaryOps Gt (Var (onlyStr "x")) (Literal 0))
                          ( Seq
                              (Let (onlyStr "x") (BinaryOps Sub (Var (onlyStr "x")) (Literal 1)))
                              ( Seq
                                  (If (BinaryOps Eq (Var (onlyStr "x")) (Literal 3)) ContinueSignal Skip)
                                  (Let (onlyStr "y") (BinaryOps Add (Var (onlyStr "y")) (Var (onlyStr "x"))))
                              )
                          )
                          Nothing
                          Nothing
                      )
                      (Var (onlyStr "y"))
                  )
              )

      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 0), ("y", IntVal 7)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 7), finalMachine)

    it "makes break signals outside of while loops invalid" $ do
      let term =
            Seq
              (Let (onlyStr "x") (Literal 5))
              ( Seq
                  BreakSignal
                  (Let (onlyStr "y") (BinaryOps Add (Var (onlyStr "x")) (Literal 2)))
              )

      let (result, _) = reduceFully term initialMachine
      result `shouldBe` Left "break used outside of loop"

    it "makes continue signals outside of while loops invalid" $ do
      let term =
            Seq
              (Let (onlyStr "x") (Literal 5))
              ( Seq
                  ContinueSignal
                  (Let (onlyStr "y") (BinaryOps Add (Var (onlyStr "x")) (Literal 2)))
              )

      let (result, _) = reduceFully term initialMachine
      result `shouldBe` Left "continue used outside of loop"

    it "for loop index does not leak outside the loop" $ do
      let term =
            Seq
              (For (typedName "i") (Literal 0) (Literal 3) Skip Nothing Nothing)
              (Var (onlyStr "i"))

      let (result, _) = reduceFully term initialMachine
      result `shouldBe` Left "variable not found"

    it "for loop restores any shadowed variable after completion" $ do
      let term =
            Seq
              ( For
                  (typedName "i")
                  (Literal 0)
                  (Literal 2)
                  (Let (onlyStr "i") (Literal 42))
                  Nothing
                  Nothing
              )
              (Var (onlyStr "i"))

      let machine = initialMachine {getMem = scopeFromList [("i", IntVal 7)]}
      reduceFully term machine `shouldBe` (Right (IntVal 7), machine)

    it "break inside an if statement exits the while loop" $ do
      let term =
            Seq
              (Let (onlyStr "x") (Literal 5))
              ( Seq
                  (Let (onlyStr "y") (Literal 0))
                  ( While
                      (BinaryOps Gt (Var (onlyStr "x")) (Literal 0))
                      ( If
                          (BinaryOps Eq (Var (onlyStr "x")) (Literal 3))
                          BreakSignal
                          ( Seq
                              (Let (onlyStr "y") (BinaryOps Add (Var (onlyStr "y")) (Var (onlyStr "x"))))
                              (Let (onlyStr "x") (BinaryOps Sub (Var (onlyStr "x")) (Literal 1)))
                          )
                      )
                      Nothing
                      Nothing
                  )
              )

      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 3), ("y", IntVal 9)]}
      reduceFully term initialMachine `shouldBe` (Right UnitVal, finalMachine)

    it "inner loop break exits only the inner loop" $ do
      let term =
            Seq
              (Let (onlyStr "x") (Literal 3))
              ( Seq
                  (Let (onlyStr "y") (Literal 0))
                  ( While
                      (BinaryOps Gt (Var (onlyStr "x")) (Literal 0))
                      ( Seq
                          (Let (onlyStr "z") (Literal 2))
                          ( Seq
                              ( While
                                  (BinaryOps Gt (Var (onlyStr "z")) (Literal 0))
                                  ( If
                                      (BinaryOps Eq (Var (onlyStr "z")) (Literal 1))
                                      BreakSignal
                                      ( Seq
                                          (Let (onlyStr "y") (BinaryOps Add (Var (onlyStr "y")) (Var (onlyStr "z"))))
                                          (Let (onlyStr "z") (BinaryOps Sub (Var (onlyStr "z")) (Literal 1)))
                                      )
                                  )
                                  Nothing
                                  Nothing
                              )
                              (Let (onlyStr "x") (BinaryOps Sub (Var (onlyStr "x")) (Literal 1)))
                          )
                      )
                      Nothing
                      Nothing
                  )
              )

      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 0), ("y", IntVal 6), ("z", IntVal 1)]}
      reduceFully term initialMachine `shouldBe` (Right UnitVal, finalMachine)

    it "inner loop continue skips to next iteration" $ do
      let term =
            Seq
              (Let (onlyStr "x") (Literal 3))
              ( Seq
                  (Let (onlyStr "y") (Literal 0))
                  ( While
                      (BinaryOps Gt (Var (onlyStr "x")) (Literal 0))
                      ( Seq
                          (Let (onlyStr "z") (Literal 3))
                          ( Seq
                              ( While
                                  (BinaryOps Gt (Var (onlyStr "z")) (Literal 0))
                                  ( Seq
                                      (Let (onlyStr "z") (BinaryOps Sub (Var (onlyStr "z")) (Literal 1)))
                                      ( Seq
                                          (Let (onlyStr "y") (BinaryOps Add (Var (onlyStr "y")) (Var (onlyStr "z"))))
                                          (If (BinaryOps Eq (Var (onlyStr "z")) (Literal 2)) ContinueSignal Skip)
                                      )
                                  )
                                  Nothing
                                  Nothing
                              )
                              (Let (onlyStr "x") (BinaryOps Sub (Var (onlyStr "x")) (Literal 1)))
                          )
                      )
                      Nothing
                      Nothing
                  )
              )

      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 0), ("y", IntVal 9), ("z", IntVal 0)]}
      reduceFully term initialMachine `shouldBe` (Right UnitVal, finalMachine)

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
              (Let (onlyStr "x") (Literal 10))
              ( Seq
                  (Let (onlyStr "y") (Literal 5))
                  (BinaryOps Add (Var (onlyStr "x")) (Var (onlyStr "y")))
              )
      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 10), ("y", IntVal 5)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 15), finalMachine)

    -- Function Application Tests
    it "invokes a zero-argument function" $ do
      let f0 = Fun [] (Literal 42)
      let term = ApplyFun f0 []
      reduceFully term initialMachine `shouldBe` (Right (IntVal 42), initialMachine)

    it "errors when invoking a function that expects arguments" $ do
      let f1 = Fun [typedName "x"] (Var (onlyStr "x"))
      let term = ApplyFun f1 []
      let (result, _) = reduceFully term initialMachine
      result `shouldBe` Left "missing arguments: function requires parameters"

    it "errors when applying an argument to a zero-arg function" $ do
      let f0 = Fun [] (Literal 1)
      let term = ApplyFun f0 [Literal 0]
      let (result, _) = reduceFully term initialMachine
      result `shouldBe` Left "too many arguments: function takes 0 arguments"

    it "applies a simple function" $ do
      let inc = Fun [typedName "x"] (BinaryOps Add (Var (onlyStr "x")) (Literal 1))
      let term = ApplyFun inc [Literal 41]
      reduceFully term initialMachine `shouldBe` (Right (IntVal 42), initialMachine)

    it "binds parameter in environment for body" $ do
      let f = Fun [typedName "x"] (Var (onlyStr "x"))
      let term = ApplyFun f [Literal 7]
      reduceFully term initialMachine `shouldBe` (Right (IntVal 7), initialMachine)

    it "applies a two-argument function" $ do
      let add2 = Fun [typedName "x", typedName "y"] (BinaryOps Add (Var (onlyStr "x")) (Var (onlyStr "y")))
      let term = ApplyFun add2 [Literal 2, Literal 3]
      reduceFully term initialMachine `shouldBe` (Right (IntVal 5), initialMachine)

    it "applies a three-argument function via currying" $ do
      let add3 = Fun [typedName "x", typedName "y", typedName "z"] (BinaryOps Add (BinaryOps Add (Var (onlyStr "x")) (Var (onlyStr "y"))) (Var (onlyStr "z")))
      let term = ApplyFun add3 [Literal 1, Literal 2, Literal 3]
      reduceFully term initialMachine `shouldBe` (Right (IntVal 6), initialMachine)

    it "errors when applying a non-function" $ do
      let term = ApplyFun (Literal 3) [Literal 4]
      let (result, _) = reduceFully term initialMachine
      result `shouldBe` Left "attempt to call a non-function"

    it "returns functions" $ do
      let f0 = Fun [typedName "x"] (Literal 12)
      let f1 = Fun [typedName "y"] f0
      let term = ApplyFun f1 [Literal 5]
      reduceFully term initialMachine `shouldBe` (Right (ClosureVal [typedName "x"] (Literal 12) (scopeFromList [("y", IntVal 5)])), initialMachine)

    it "creates local variables in functions" $ do
      let f = Fun [typedName "y"] (Let (onlyStr "x") (Var (onlyStr "y"))) -- Should not affect outside x.
      let term = Seq (Let (onlyStr "x") (Literal 1)) (ApplyFun f [Literal 99])
      let machine = initialMachine {getMem = scopeFromList [("x", IntVal 1)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 99), machine)

    -- Variable Capture Tests
    it "captures environment for zero-argument functions" $ do
      let f0 = Fun [] (Var (onlyStr "outside"))
      let f1 = Fun [typedName "y"] (Seq (Let (onlyStr "outside") (Literal 99)) f0)
      let term = Seq (Let (onlyStr "outside") (Literal 1)) (ApplyFun (ApplyFun f1 [Literal 0]) [])
      let machine = initialMachine {getMem = scopeFromList [("outside", IntVal 1)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 99), machine)

    it "captures environment for odd-argument functions" $ do
      let f0 = Fun [typedName "x"] (Var (onlyStr "outside")) -- Created inside of f1.
      let f1 = Fun [typedName "y"] (Seq (Let (onlyStr "outside") (Literal 99)) f0)
      -- (f1(0))(0) -> f0(0), where outside refers to the 99 captured in f1.
      let term = Seq (Let (onlyStr "outside") (Literal 1)) (ApplyFun (ApplyFun f1 [Literal 0]) [Literal 0])
      let machine = initialMachine {getMem = scopeFromList [("outside", IntVal 1)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 99), machine)

    it "captures environment for even-argument functions" $ do
      let f0 = Fun [typedName "x", typedName "z"] (Var (onlyStr "outside"))
      let f1 = Fun [typedName "y"] (Seq (Let (onlyStr "outside") (Literal 99)) f0)
      let term = Seq (Let (onlyStr "outside") (Literal 1)) (ApplyFun (ApplyFun f1 [Literal 0]) [Literal 0, Literal 0])
      let machine = initialMachine {getMem = scopeFromList [("outside", IntVal 1)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 99), machine)

    it "captures only the environment at function creation time" $ do
      let f0 = Fun [typedName "x"] (Var (onlyStr "outside"))
      let setupTerm = Seq (Let (onlyStr "outside") (Literal 1)) (Let (onlyStr "f") f0) -- Function f created here.
      let f1 = Fun [typedName "y"] (Seq (Let (onlyStr "outside") (Literal 99)) (ApplyFun (Var (onlyStr "f")) [Literal 0])) -- 99 should not be captured.
      let term = Seq setupTerm (ApplyFun f1 [Literal 0])
      let closureVal = ClosureVal [typedName "x"] (Var (onlyStr "outside")) (scopeFromList [("outside", IntVal 1)]) -- Captured 1 from outside.
      let machine = initialMachine {getMem = scopeFromList [("outside", IntVal 1), ("f", closureVal)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 1), machine)

    it "captures all variables in nested scopes" $ do
      let f0 = Fun [typedName "x"] (BinaryOps Add (Var (onlyStr "a")) (Var (onlyStr "b")))
      let f1 = Fun [typedName "y"] (Seq (Let (onlyStr "b") (Literal 4)) f0) -- b created (parent of f0).
      let f2 = Fun [typedName "z"] (Seq (Let (onlyStr "a") (Literal 3)) f1) -- a created (parent of f1).
      let term = ApplyFun (ApplyFun (ApplyFun f2 [Literal 0]) [Literal 0]) [Literal 0]
      reduceFully term initialMachine `shouldBe` (Right (IntVal 7), initialMachine)

    it "handles parameter shadowing" $ do
      let f = Fun [typedName "x"] (Var (onlyStr "x"))
      let term = Seq (Let (onlyStr "x") (Literal 1)) (ApplyFun f [Literal 5]) -- Parameter x is 5.
      let machine = initialMachine {getMem = scopeFromList [("x", IntVal 1)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 5), machine)

    it "errors on undefined variable in function scope" $ do
      let f1 = Fun [] (Seq (Let (onlyStr "y") (Literal 3)) (ApplyFun (Var (onlyStr "f")) []))
      let term = Seq (Let (onlyStr "f") (Fun [] (Write (Var (onlyStr "y"))))) (ApplyFun f1 []) -- f defined outside of f1.
      let machine = initialMachine {getMem = scopeFromList [("f", ClosureVal [] (Write (Var (onlyStr "y"))) emptyScope)]}
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
      let term = Seq (Let (onlyStr "x") (TupleTerm [Literal 10, StringLiteral "hello", BoolLit True])) (Let (Bracket (onlyStr "x") (Literal 2)) (BoolLit False))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Tuple [IntVal 10, StringVal "hello", BoolVal False])]}
      reduceFully term initialMachine `shouldBe` (Right (Tuple [IntVal 10, StringVal "hello", BoolVal False]), finalMachine)

    it "reduces a let nested tuple expression" $ do
      let term = Seq (Let (onlyStr "x") (TupleTerm [Literal 10, TupleTerm [StringLiteral "hello"], BoolLit True])) (Let (Bracket (Bracket (onlyStr "x") (Literal 1)) (Literal 0)) (StringLiteral "goodbye"))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Tuple [IntVal 10, Tuple [StringVal "goodbye"], BoolVal True])]}
      reduceFully term initialMachine `shouldBe` (Right (Tuple [IntVal 10, Tuple [StringVal "goodbye"], BoolVal True]), finalMachine)

    it "let tuple above bounds" $ do
      let term = Seq (Let (onlyStr "x") (TupleTerm [Literal 10, StringLiteral "hello", BoolLit True])) (Let (Bracket (onlyStr "x") (Literal 3)) (BoolLit False))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Tuple [IntVal 10, StringVal "hello", BoolVal True])]}
      reduceFully term initialMachine `shouldBe` (Left "Attempting to set value Out of Bounds", finalMachine)

    it "access a tuple" $ do
      let term = Seq (Let (onlyStr "x") (TupleTerm [Literal 10, StringLiteral "hello", BoolLit True])) (Var (Bracket (onlyStr "x") (Literal 2)))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Tuple [IntVal 10, StringVal "hello", BoolVal True])]}
      reduceFully term initialMachine `shouldBe` (Right (BoolVal True), finalMachine)

    it "access a nested tuple" $ do
      let term = Seq (Let (onlyStr "x") (TupleTerm [Literal 10, TupleTerm [Literal 1, StringLiteral "hello"], BoolLit True])) (Var (Bracket (Bracket (onlyStr "x") (Literal 1)) (Literal 0)))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Tuple [IntVal 10, Tuple [IntVal 1, StringVal "hello"], BoolVal True])]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 1), finalMachine)

    it "access tuple above bounds" $ do
      let term = Seq (Let (onlyStr "x") (TupleTerm [Literal 1, Literal 2, Literal 3])) (Var (Bracket (onlyStr "x") (Literal 3)))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Tuple [IntVal 1, IntVal 2, IntVal 3])]}
      reduceFully term initialMachine `shouldBe` (Left "Out of Bounds", finalMachine)

    it "access tuple below bounds" $ do
      let term = Seq (Let (onlyStr "x") (TupleTerm [Literal 1, Literal 2, Literal 3])) (Var (Bracket (onlyStr "x") (Literal (-1))))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Tuple [IntVal 1, IntVal 2, IntVal 3])]}
      reduceFully term initialMachine `shouldBe` (Left "Out of Bounds", finalMachine)

    it "reduces new dictionary" $ do
      let term = NewDictionary
      reduceFully term initialMachine `shouldBe` (Right (Dictionary M.empty), initialMachine)

    it "set dictionary" $ do
      let term = Seq (Let (onlyStr "x") NewDictionary) (Let (Bracket (onlyStr "x") (Literal 3)) (StringLiteral "hello"))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Dictionary (M.fromList [(3, StringVal "hello")]))]}
      reduceFully term initialMachine `shouldBe` (Right (Dictionary (M.fromList [(3, StringVal "hello")])), finalMachine)

    it "set nested dictionary" $ do
      let term = Seq (Let (onlyStr "x") NewDictionary) (Seq (Let (Bracket (onlyStr "x") (Literal 3)) NewDictionary) (Let (Bracket (Bracket (onlyStr "x") (Literal 3)) (Literal 4)) (StringLiteral "hello")))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Dictionary (M.fromList [(3, Dictionary (M.fromList [(4, StringVal "hello")]))]))]}
      reduceFully term initialMachine `shouldBe` (Right (Dictionary (M.fromList [(3, Dictionary (M.fromList [(4, StringVal "hello")]))])), finalMachine)

    it "access dictionary" $ do
      let term = Seq (Let (onlyStr "x") NewDictionary) (Seq (Let (Bracket (onlyStr "x") (Literal 3)) (StringLiteral "hello")) (Var (Bracket (onlyStr "x") (Literal 3))))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Dictionary (M.fromList [(3, StringVal "hello")]))]}
      reduceFully term initialMachine `shouldBe` (Right (StringVal "hello"), finalMachine)

    it "access nested dictionary" $ do
      let term = Seq (Let (onlyStr "x") NewDictionary) (Seq (Let (Bracket (onlyStr "x") (Literal 3)) NewDictionary) (Seq (Let (Bracket (Bracket (onlyStr "x") (Literal 3)) (Literal 4)) (StringLiteral "hello")) (Var (Bracket (Bracket (onlyStr "x") (Literal 3)) (Literal 4)))))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Dictionary (M.fromList [(3, Dictionary (M.fromList [(4, StringVal "hello")]))]))]}
      reduceFully term initialMachine `shouldBe` (Right (StringVal "hello"), finalMachine)

    it "access dictionary nonexistant entry" $ do
      let term = Seq (Let (onlyStr "x") NewDictionary) (Seq (Let (Bracket (onlyStr "x") (Literal 3)) (StringLiteral "hello")) (Var (Bracket (onlyStr "x") (Literal 1))))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Dictionary (M.fromList [(3, StringVal "hello")]))]}
      reduceFully term initialMachine `shouldBe` (Left "Unable to find element in dictionary", finalMachine)

    it "access nested dictionary-tuple" $ do
      let term = Seq (Let (onlyStr "x") NewDictionary) (Seq (Let (Bracket (onlyStr "x") (Literal 3)) (TupleTerm [Literal 0, Literal 1, StringLiteral "hello"])) (Var (Bracket (Bracket (onlyStr "x") (Literal 3)) (Literal 2))))
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
      checkFailure (reduceFully term initialMachine) initialMachine

    it "reduces ternary operator with string condition (empty)" $ do
      let term = If (StringLiteral "") (Literal 100) (Literal 200)
      checkFailure (reduceFully term initialMachine) initialMachine

    it "reduces ternary operator with comparison condition" $ do
      let term = If (BinaryOps Gt (Literal 10) (Literal 5)) (StringLiteral "greater") (StringLiteral "not greater")
      reduceFully term initialMachine `shouldBe` (Right (StringVal "greater"), initialMachine)

    it "reduces nested ternary operators" $ do
      let term = If (BoolLit True) (If (BoolLit False) (Literal 1) (Literal 2)) (Literal 3)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 2), initialMachine)

    it "reduces ternary operator with variable access" $ do
      let term = Seq (Let (onlyStr "x") (Literal 10)) (If (BinaryOps Gt (Var (onlyStr "x")) (Literal 5)) (Var (onlyStr "x")) (Literal 0))
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
      let term = Seq (Let (onlyStr "x") (TupleTerm [Literal 10, StringLiteral "hello", BoolLit True])) (Var (Bracket (onlyStr "x") (BinaryOps Div (Literal 2) (Literal 0))))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Tuple [IntVal 10, StringVal "hello", BoolVal True])]}
      reduceFully term initialMachine `shouldBe` (Left "Cannot divide by 0", finalMachine)
