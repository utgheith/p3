{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SmallSpec (spec) where

import qualified Control.Monad.State as S
import Data.Bits (complement)
import Data.List (isInfixOf)
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
  getBracketValue _ _ = return $ Sad (Type, "Tuple Lookup Bad Input")

  setBracketValue n t v = do
    m <- S.get
    case lookupScope n (getMem m) of
      Just oldVal -> case oldVal of
        Tuple _ ->
          let newVal = updateBracket oldVal t v
           in case newVal of
                Right newVal' -> do
                  S.put (m {getMem = insertScope n newVal' (getMem m)})
                  return $ Happy v
                Left e -> return $ Sad e
        Dictionary _ ->
          let newVal = updateBracket oldVal t v
           in case newVal of
                Right newVal' -> do
                  S.put (m {getMem = insertScope n newVal' (getMem m)})
                  return $ Happy v
                Left e -> return $ Sad e
        _ -> return $ Sad (Type, "Attempting to Index but didn't find Tuple")
      Nothing -> return $ Sad (VariableNotFound, "Attempting to Set Tuple That Doesn't Exist")
    where
      updateBracket :: Value -> Value -> Value -> Either Error Value
      updateBracket (Tuple (x : xs)) (Tuple (y : ys)) val = case y of
        IntVal index ->
          if index == 0
            then
              let returnVal = updateBracket x (Tuple ys) val
               in case returnVal of
                    Right a -> Right $ Tuple (a : xs)
                    _ -> returnVal
            else
              let returnVal = updateBracket (Tuple xs) (Tuple (IntVal (index - 1) : ys)) val
               in case returnVal of
                    Right (Tuple a) -> Right $ Tuple (x : a)
                    Left _ -> returnVal
                    _ -> error "Unable to rebuild tuple"
        _ -> Left (Type, "Provided non integer to index by")
      updateBracket (Dictionary d) (Tuple (y : ys)) val = case y of
        IntVal index -> case M.lookup index d of
          Just r ->
            let returnVal = updateBracket r (Tuple ys) val
             in case returnVal of
                  Right w -> Right (Dictionary (M.insert index w d))
                  _ -> returnVal
          Nothing ->
            let returnVal = updateBracket (IntVal 0) (Tuple ys) val
             in case returnVal of
                  Right w -> Right (Dictionary (M.insert index w d))
                  _ -> returnVal
        _ -> Left (Type, "Provided non integer to index by")
      updateBracket _ (Tuple []) val = Right val
      updateBracket _ _ _ = Left (Type, "Out of Bounds")

  selectValue (BoolVal True) c _ = c
  selectValue (BoolVal False) _ t = t
  selectValue (IntVal n) c t = if n /= 0 then c else t
  selectValue (StringVal s) c t = if not (null s) then c else t
  selectValue (Tuple l) c t = if not (null l) then c else t
  selectValue (ClosureVal {}) _ _ = return $ Sad (Type, "Type error in select")
  selectValue (Dictionary _) _ _ = return $ Sad (Type, "Type error in select")

spec :: Spec
spec = do
  describe "reduceFully" $ do
    let initialMachine = MockMachine {getMem = emptyScope, getInput = [], getOutput = []}

    it "reduces an integer literal" $ do
      let term = Literal 10
      reduceFully term initialMachine `shouldBe` (Right (IntVal 10), initialMachine)

    it "reduces a string literal" $ do
      let term = StringLiteral "hello"
      reduceFully term initialMachine `shouldBe` (Right (StringVal "hello"), initialMachine)

    it "reduces a variable" $ do
      let term = Var "x"
      let machine = initialMachine {getMem = scopeFromList [("x", IntVal 5)]}
      reduceFully term machine `shouldBe` (Right (IntVal 5), machine)

    it "reduces a let expression" $ do
      let term = Seq (Let "x" (Literal 5)) (Var "x")
      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 5)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 5), finalMachine)

    it "reduces a sequence" $ do
      let term = Seq (Let "x" (Literal 5)) (Var "x")
      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 5)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 5), finalMachine)

    it "reduces an if expression (then)" $ do
      let term = If (BoolLit True) (Literal 10) (Literal 20)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 10), initialMachine)

    it "reduces an if expression (else)" $ do
      let term = If (BoolLit False) (Literal 10) (Literal 20)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 20), initialMachine)

    it "reduces a while loop" $ do
      let term = Seq (Let "x" (Literal 3)) (While (Var "x") (Let "x" (BinaryOps Sub (Var "x") (Literal 1))))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 0)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 0), finalMachine)

    it "reduces read and write" $ do
      let term = Seq (Read "x") (Write (Var "x"))
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

    it "reduces a Tuple" $ do
      let term = TupleTerm [Literal 10, StringLiteral "hello", BoolLit True]
      let (result, _) = reduceFully term initialMachine
      result `shouldBe` Right (Tuple [IntVal 10, StringVal "hello", BoolVal True])

    it "access a Tuple" $ do
      let term = AccessBracket (TupleTerm [Literal 10, StringLiteral "hello", BoolLit True]) (Literal 1)
      let (result, _) = reduceFully term initialMachine
      result `shouldBe` Right (StringVal "hello")

    it "reduces a let tuple expression" $ do
      let term = Seq (Let "x" (TupleTerm [Literal 10, StringLiteral "hello", BoolLit True])) (SetBracket "x" (TupleTerm [Literal 2]) (BoolLit False))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Tuple [IntVal 10, StringVal "hello", BoolVal False])]}
      reduceFully term initialMachine `shouldBe` (Right (BoolVal False), finalMachine)

    it "reduces a let nested tuple expression" $ do
      let term = Seq (Let "x" (TupleTerm [Literal 10, TupleTerm [StringLiteral "hello"], BoolLit True])) (SetBracket "x" (TupleTerm [Literal 1, Literal 0]) (StringLiteral "goodbye"))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Tuple [IntVal 10, Tuple [StringVal "goodbye"], BoolVal True])]}
      reduceFully term initialMachine `shouldBe` (Right (StringVal "goodbye"), finalMachine)

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
              (Let "x" (Literal 5))
              ( Seq
                  ( While
                      ( BinaryOps
                          And
                          (BinaryOps Gt (Var "x") (Literal 0))
                          (BinaryOps Lt (Var "x") (Literal 10))
                      )
                      (Let "x" (BinaryOps Add (Var "x") (Literal 1)))
                  )
                  (Var "x")
              )
      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 10)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 10), finalMachine)

    it "reduces a while loop with a break statement" $ do
      let term =
            Seq
              (Let "x" (Literal 5))
              ( Seq
                  ( While
                      (Var "x")
                      ( Seq
                          (If (BinaryOps Eq (Var "x") (Literal 3)) BreakSignal Skip)
                          (Let "x" (BinaryOps Sub (Var "x") (Literal 1)))
                      )
                  )
                  (Var "x")
              )
      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 3)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 3), finalMachine)

    it "reduces a while loop with a complex break statement" $ do
      let term =
            Seq
              (Let "x" (Literal 5))
              ( Seq
                  (Let "y" (Literal 0))
                  ( Seq
                      ( While
                          (Var "x")
                          ( Seq
                              (Let "x" (BinaryOps Sub (Var "x") (Literal 1)))
                              ( Seq
                                  (If (BinaryOps Eq (Var "x") (Literal 3)) BreakSignal Skip)
                                  (Let "y" (BinaryOps Add (Var "y") (Var "x")))
                              )
                          )
                      )
                      (Var "y")
                  )
              )

      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 3), ("y", IntVal 4)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 4), finalMachine)

    it "reduces a while loop with a continue statement" $ do
      let term =
            Seq
              (Let "x" (Literal 5))
              ( Seq
                  (Let "y" (Literal 0))
                  ( Seq
                      ( While
                          (Var "x")
                          ( Seq
                              (Let "x" (BinaryOps Sub (Var "x") (Literal 1)))
                              ( Seq
                                  (If (BinaryOps Eq (Var "x") (Literal 3)) ContinueSignal Skip)
                                  (Let "y" (BinaryOps Add (Var "y") (Var "x")))
                              )
                          )
                      )
                      (Var "y")
                  )
              )

      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 0), ("y", IntVal 7)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 7), finalMachine)

    it "makes break signals outside of while loops invalid" $ do
      let term =
            Seq
              (Let "x" (Literal 5))
              ( Seq
                  BreakSignal
                  (Let "y" (BinaryOps Add (Var "x") (Literal 2)))
              )

      let (result, _) = reduceFully term initialMachine
      result `shouldBe` Left "unhandled break signal"

    it "makes continue signals outside of while loops invalid" $ do
      let term =
            Seq
              (Let "x" (Literal 5))
              ( Seq
                  ContinueSignal
                  (Let "y" (BinaryOps Add (Var "x") (Literal 2)))
              )

      let (result, _) = reduceFully term initialMachine
      result `shouldBe` Left "unhandled continue signal"

    it "break inside an if statement exits the while loop" $ do
      let term =
            Seq
              (Let "x" (Literal 5))
              ( Seq
                  (Let "y" (Literal 0))
                  ( While
                      (Var "x")
                      ( If
                          (BinaryOps Eq (Var "x") (Literal 3))
                          BreakSignal
                          ( Seq
                              (Let "y" (BinaryOps Add (Var "y") (Var "x")))
                              (Let "x" (BinaryOps Sub (Var "x") (Literal 1)))
                          )
                      )
                  )
              )

      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 3), ("y", IntVal 9)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 0), finalMachine)

    it "inner loop break exits only the inner loop" $ do
      let term =
            Seq
              (Let "x" (Literal 3))
              ( Seq
                  (Let "y" (Literal 0))
                  ( While
                      (Var "x")
                      ( Seq
                          (Let "z" (Literal 2))
                          ( Seq
                              ( While
                                  (Var "z")
                                  ( If
                                      (BinaryOps Eq (Var "z") (Literal 1))
                                      BreakSignal
                                      ( Seq
                                          (Let "y" (BinaryOps Add (Var "y") (Var "z")))
                                          (Let "z" (BinaryOps Sub (Var "z") (Literal 1)))
                                      )
                                  )
                              )
                              (Let "x" (BinaryOps Sub (Var "x") (Literal 1)))
                          )
                      )
                  )
              )

      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 0), ("y", IntVal 6), ("z", IntVal 1)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 0), finalMachine)

    it "inner loop continue skips to next iteration" $ do
      let term =
            Seq
              (Let "x" (Literal 3))
              ( Seq
                  (Let "y" (Literal 0))
                  ( While
                      (Var "x")
                      ( Seq
                          (Let "z" (Literal 3))
                          ( Seq
                              ( While
                                  (Var "z")
                                  ( Seq
                                      (Let "z" (BinaryOps Sub (Var "z") (Literal 1)))
                                      ( Seq
                                          (Let "y" (BinaryOps Add (Var "y") (Var "z")))
                                          (If (BinaryOps Eq (Var "z") (Literal 2)) ContinueSignal Skip)
                                      )
                                  )
                              )
                              (Let "x" (BinaryOps Sub (Var "x") (Literal 1)))
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
              (Let "x" (Literal 10))
              ( Seq
                  (Let "y" (Literal 5))
                  (BinaryOps Add (Var "x") (Var "y"))
              )
      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 10), ("y", IntVal 5)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 15), finalMachine)

    -- Function Application Tests
    it "invokes a zero-argument function" $ do
      let f0 = Fun [] (Literal 42)
      let term = ApplyFun f0 []
      reduceFully term initialMachine `shouldBe` (Right (IntVal 42), initialMachine)

    it "errors when invoking a function that expects arguments" $ do
      let f1 = Fun ["x"] (Var "x")
      let term = ApplyFun f1 []
      let (result, _) = reduceFully term initialMachine
      result `shouldBe` Left "missing arguments: function requires parameters"

    it "errors when applying an argument to a zero-arg function" $ do
      let f0 = Fun [] (Literal 1)
      let term = ApplyFun f0 [Literal 0]
      let (result, _) = reduceFully term initialMachine
      result `shouldBe` Left "too many arguments: function takes 0 arguments"

    it "applies a simple function" $ do
      let inc = Fun ["x"] (BinaryOps Add (Var "x") (Literal 1))
      let term = ApplyFun inc [Literal 41]
      reduceFully term initialMachine `shouldBe` (Right (IntVal 42), initialMachine)

    it "binds parameter in environment for body" $ do
      let f = Fun ["x"] (Var "x")
      let term = ApplyFun f [Literal 7]
      reduceFully term initialMachine `shouldBe` (Right (IntVal 7), initialMachine)

    it "applies a two-argument function" $ do
      let add2 = Fun ["x", "y"] (BinaryOps Add (Var "x") (Var "y"))
      let term = ApplyFun add2 [Literal 2, Literal 3]
      reduceFully term initialMachine `shouldBe` (Right (IntVal 5), initialMachine)

    it "applies a three-argument function via currying" $ do
      let add3 = Fun ["x", "y", "z"] (BinaryOps Add (BinaryOps Add (Var "x") (Var "y")) (Var "z"))
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
      let f = Fun ["y"] (Let "x" (Var "y")) -- Should not affect outside x.
      let term = Seq (Let "x" (Literal 1)) (ApplyFun f [Literal 99])
      let machine = initialMachine {getMem = scopeFromList [("x", IntVal 1)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 99), machine)

    -- Variable Capture Tests
    it "captures environment for zero-argument functions" $ do
      let f0 = Fun [] (Var "outside")
      let f1 = Fun ["y"] (Seq (Let "outside" (Literal 99)) f0)
      let term = Seq (Let "outside" (Literal 1)) (ApplyFun (ApplyFun f1 [Literal 0]) [])
      let machine = initialMachine {getMem = scopeFromList [("outside", IntVal 1)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 99), machine)

    it "captures environment for odd-argument functions" $ do
      let f0 = Fun ["x"] (Var "outside") -- Created inside of f1.
      let f1 = Fun ["y"] (Seq (Let "outside" (Literal 99)) f0)
      -- (f1(0))(0) -> f0(0), where outside refers to the 99 captured in f1.
      let term = Seq (Let "outside" (Literal 1)) (ApplyFun (ApplyFun f1 [Literal 0]) [Literal 0])
      let machine = initialMachine {getMem = scopeFromList [("outside", IntVal 1)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 99), machine)

    it "captures environment for even-argument functions" $ do
      let f0 = Fun ["x", "z"] (Var "outside")
      let f1 = Fun ["y"] (Seq (Let "outside" (Literal 99)) f0)
      let term = Seq (Let "outside" (Literal 1)) (ApplyFun (ApplyFun f1 [Literal 0]) [Literal 0, Literal 0])
      let machine = initialMachine {getMem = scopeFromList [("outside", IntVal 1)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 99), machine)

    it "captures only the environment at function creation time" $ do
      let f0 = Fun ["x"] (Var "outside")
      let setupTerm = Seq (Let "outside" (Literal 1)) (Let "f" f0) -- Function f created here.
      let f1 = Fun ["y"] (Seq (Let "outside" (Literal 99)) (ApplyFun (Var "f") [Literal 0])) -- 99 should not be captured.
      let term = Seq setupTerm (ApplyFun f1 [Literal 0])
      let closureVal = ClosureVal ["x"] (Var "outside") [("outside", IntVal 1)] -- Captured 1 from outside.
      let machine = initialMachine {getMem = scopeFromList [("outside", IntVal 1), ("f", closureVal)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 1), machine)

    it "captures all variables in nested scopes" $ do
      let f0 = Fun ["x"] (BinaryOps Add (Var "a") (Var "b"))
      let f1 = Fun ["y"] (Seq (Let "b" (Literal 4)) f0) -- b created (parent of f0).
      let f2 = Fun ["z"] (Seq (Let "a" (Literal 3)) f1) -- a created (parent of f1).
      let term = ApplyFun (ApplyFun (ApplyFun f2 [Literal 0]) [Literal 0]) [Literal 0]
      reduceFully term initialMachine `shouldBe` (Right (IntVal 7), initialMachine)

    it "handles parameter shadowing" $ do
      let f = Fun ["x"] (Var "x")
      let term = Seq (Let "x" (Literal 1)) (ApplyFun f [Literal 5]) -- Parameter x is 5.
      let machine = initialMachine {getMem = scopeFromList [("x", IntVal 1)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 5), machine)

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

    it "reduces new dictionary" $ do
      let term = NewDictionary
      reduceFully term initialMachine `shouldBe` (Right (Dictionary M.empty), initialMachine)

    it "set dictionary" $ do
      let term = Seq (Let "x" NewDictionary) (SetBracket "x" (TupleTerm [Literal 3]) (StringLiteral "hello"))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Dictionary (M.fromList [(3, StringVal "hello")]))]}
      reduceFully term initialMachine `shouldBe` (Right (StringVal "hello"), finalMachine)

    it "access dictionary" $ do
      let term = Seq (Let "x" NewDictionary) (Seq (SetBracket "x" (TupleTerm [Literal 3]) (StringLiteral "hello")) (AccessBracket (Var "x") (Literal 3)))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", Dictionary (M.fromList [(3, StringVal "hello")]))]}
      reduceFully term initialMachine `shouldBe` (Right (StringVal "hello"), finalMachine)

    -- Tests for new loop constructs and compound assignment
    it "reduces a traditional for loop" $ do
      let term =
            Seq
              (Let "sum" (Literal 0))
              ( ForLoop
                  "i"
                  (Literal 0)
                  (BinaryOps Lt (Var "i") (Literal 3))
                  (Let "i" (BinaryOps Add (Var "i") (Literal 1)))
                  (Let "sum" (BinaryOps Add (Var "sum") (Var "i")))
              )
      let finalMachine = initialMachine {getMem = scopeFromList [("sum", IntVal 3), ("i", IntVal 3)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 0), finalMachine)

    it "reduces a for-each loop over list" $ do
      let term =
            Seq
              (Let "sum" (Literal 0))
              ( ForEach
                  "x"
                  (TupleTerm [Literal 10, Literal 20, Literal 30])
                  (Let "sum" (BinaryOps Add (Var "sum") (Var "x")))
              )
      let finalMachine = initialMachine {getMem = scopeFromList [("sum", IntVal 60), ("x", IntVal 30)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 0), finalMachine)

    it "reduces a for-each loop over empty list" $ do
      let term =
            Seq
              (Let "sum" (Literal 0))
              ( ForEach
                  "x"
                  (TupleTerm [])
                  (Let "sum" (BinaryOps Add (Var "sum") (Var "x")))
              )
      let finalMachine = initialMachine {getMem = scopeFromList [("sum", IntVal 0)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 0), finalMachine)

    it "reduces += compound assignment" $ do
      let term = Seq (Let "x" (Literal 5)) (AddAssign "x" (Literal 3))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 8)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 8), finalMachine)

    it "reduces -= compound assignment" $ do
      let term = Seq (Let "x" (Literal 10)) (SubAssign "x" (Literal 4))
      let finalMachine = initialMachine {getMem = scopeFromList [("x", IntVal 6)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 6), finalMachine)

    it "handles type error in += with non-integer" $ do
      let term = Seq (Let "x" (StringLiteral "hello")) (AddAssign "x" (Literal 3))
      let (result, _) = reduceFully term initialMachine
      result
        `shouldSatisfy` \case
          Left msg -> "Type error in addition" `isInfixOf` msg
          _ -> False

    it "handles type error in for-each with non-tuple" $ do
      let term = ForEach "x" (Literal 42) (Write (Var "x"))
      let (result, _) = reduceFully term initialMachine
      result
        `shouldSatisfy` \case
          Left msg -> "for-each requires a tuple/list" `isInfixOf` msg
          _ -> False
