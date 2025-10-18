{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module SmallSpec (spec) where

import qualified Control.Monad.State as S
import qualified Data.Map as M
import Small
import Term
import Test.Hspec
import Value (Value (..))

-- A mock machine for testing
data MockMachine = MockMachine {getMem :: M.Map String Value, getInput :: [Value], getOutput :: [Value]} deriving (Show, Eq)

instance Machine MockMachine where
  type V MockMachine = Value

  getVar x = do
    m <- S.get
    case M.lookup x (getMem m) of
      Just v -> return $ Happy v
      Nothing -> return $ Sad "variable not found"

  setVar x v = do
    m <- S.get
    S.put (m {getMem = M.insert x v (getMem m)})
    return $ Happy v

  inputVal = do
    m <- S.get
    case getInput m of
      (i : is) -> do
        S.put (m {getInput = is})
        return $ Happy i
      [] -> return $ Sad "end of input"

  outputVal v = do
    m <- S.get
    S.put (m {getOutput = getOutput m ++ [v]})
    return $ Happy v

  subVal (IntVal v1) (IntVal v2) = return $ Happy (IntVal (v1 - v2))
  subVal _ _ = return $ Sad "Type error in subtraction"

  addVal (IntVal v1) (IntVal v2) = return $ Happy (IntVal (v1 + v2))
  addVal _ _ = return $ Sad "Type error in addition"

  mulVal (IntVal v1) (IntVal v2) = return $ Happy (IntVal (v1 * v2))
  mulVal _ _ = return $ Sad "Type error in multiplication"

  divVal (IntVal v1) (IntVal v2) =
    if v2 == 0
      then return $ Sad "Cannot divide by 0"
      else return $ Happy (IntVal (v1 `div` v2)) -- I don't want the actual interpreter to crash
  divVal _ _ = return $ Sad "Type error in division"

  modVal (IntVal v1) (IntVal v2) =
    if v2 == 0
      then return $ Sad "Cannot mod by 0"
      else return $ Happy (IntVal (v1 `mod` v2)) -- I don't want the actual interpreter to crash
  modVal _ _ = return $ Sad "Type error in modulus"

  ltVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 < v2))
  ltVal _ _ = return $ Sad "Type error in <"

  gtVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 > v2))
  gtVal _ _ = return $ Sad "Type error in >"

  lteVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 <= v2))
  lteVal _ _ = return $ Sad "Type error in <="

  gteVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 >= v2))
  gteVal _ _ = return $ Sad "Type error in >="

  eqVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 == v2))
  eqVal (BoolVal v1) (BoolVal v2) = return $ Happy (BoolVal (v1 == v2))
  eqVal (StringVal v1) (StringVal v2) = return $ Happy (BoolVal (v1 == v2))
  eqVal _ _ = return $ Sad "Type error in =="

  neqVal (IntVal v1) (IntVal v2) = return $ Happy (BoolVal (v1 /= v2))
  neqVal (BoolVal v1) (BoolVal v2) = return $ Happy (BoolVal (v1 /= v2))
  neqVal (StringVal v1) (StringVal v2) = return $ Happy (BoolVal (v1 /= v2))
  neqVal _ _ = return $ Sad "Type error in !="

  andVal (BoolVal v1) (BoolVal v2) = return $ Happy (BoolVal (v1 && v2))
  andVal _ _ = return $ Sad "Type error in &&"

  orVal (BoolVal v1) (BoolVal v2) = return $ Happy (BoolVal (v1 || v2))
  orVal _ _ = return $ Sad "Type error in ||"

  notVal (BoolVal v) = return $ Happy (BoolVal (not v))
  notVal _ = return $ Sad "Type error in !"

  selectValue (BoolVal True) c _ = c
  selectValue (BoolVal False) _ t = t
  selectValue (IntVal n) c t = if n /= 0 then c else t
  selectValue (StringVal s) c t = if not (null s) then c else t
  selectValue (Tuple l) c t = if not (null l) then c else t

spec :: Spec
spec = do
  describe "reduceFully" $ do
    let initialMachine = MockMachine {getMem = M.empty, getInput = [], getOutput = []}

    it "reduces an integer literal" $ do
      let term = Literal 10
      reduceFully term initialMachine `shouldBe` (Right (IntVal 10), initialMachine)

    it "reduces a string literal" $ do
      let term = StringLiteral "hello"
      reduceFully term initialMachine `shouldBe` (Right (StringVal "hello"), initialMachine)

    it "reduces a variable" $ do
      let term = Var "x"
      let machine = initialMachine {getMem = M.fromList [("x", IntVal 5)]}
      reduceFully term machine `shouldBe` (Right (IntVal 5), machine)

    it "reduces a let expression" $ do
      let term = Seq (Let "x" (Literal 5)) (Var "x")
      let finalMachine = initialMachine {getMem = M.fromList [("x", IntVal 5)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 5), finalMachine)

    it "reduces a sequence" $ do
      let term = Seq (Let "x" (Literal 5)) (Var "x")
      let finalMachine = initialMachine {getMem = M.fromList [("x", IntVal 5)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 5), finalMachine)

    it "reduces an if expression (then)" $ do
      let term = If (BoolLit True) (Literal 10) (Literal 20)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 10), initialMachine)

    it "reduces an if expression (else)" $ do
      let term = If (BoolLit False) (Literal 10) (Literal 20)
      reduceFully term initialMachine `shouldBe` (Right (IntVal 20), initialMachine)

    it "reduces a while loop" $ do
      let term = Seq (Let "x" (Literal 3)) (While (Var "x") (Let "x" (BinaryOps Sub (Var "x") (Literal 1))))
      let finalMachine = initialMachine {getMem = M.fromList [("x", IntVal 0)]}
      reduceFully term initialMachine `shouldBe` (Right (IntVal 0), finalMachine)

    it "reduces read and write" $ do
      let term = Seq (Read "x") (Write (Var "x"))
      let machine = initialMachine {getInput = [IntVal 42]}
      let finalMachine = machine {getMem = M.fromList [("x", IntVal 42)], getOutput = [IntVal 42], getInput = []}
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

    it "reduces a Tuple" $ do
      let term = TupleTerm (Literal 10) (TupleTerm (StringLiteral "hello") (TupleTerm (BoolLit True) TupleEnd))
      let (result, _) = reduceFully term initialMachine
      result `shouldBe` Right (Tuple [IntVal 10, StringVal "hello", BoolVal True])
