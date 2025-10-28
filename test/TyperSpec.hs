module TyperSpec (spec) where

import Term
import Test.Hspec
import TypeError (TypeError (..))
import TypeSignature (TypeSignature (..), TypedName)
import Typer

vx :: TypedName
vx = ("x", TUnknown)

vxInt :: TypedName
vxInt = ("x", TInt)

spec :: Spec
spec = do
  describe "typer" $ do
    it "types an integer literal" $
      typer (Literal 10) `shouldBe` Right TInt

    it "types a boolean literal" $
      typer (BoolLit True) `shouldBe` Right TBool

    it "types a string literal" $
      typer (StringLiteral "hello") `shouldBe` Right TString

    it "types a variable with unknown annotation" $
      typer (Var (OnlyStr vx)) `shouldBe` Right TUnknown

    it "types a variable with annotated type" $
      typer (Var (OnlyStr vxInt)) `shouldBe` Right TInt

    it "types a let expression" $
      typer (Let (OnlyStr vx) (Literal 5)) `shouldBe` Right TInt

    it "types a variable reference after let binding" $
      typer (Seq (Let (OnlyStr ("x", TUnknown)) (Literal 5)) (Var (OnlyStr ("x", TUnknown)))) `shouldBe` Right TInt

    it "types an addition" $
      typer (BinaryOps Add (Literal 5) (Literal 10)) `shouldBe` Right TInt

    it "reports error for adding non-integers" $
      typer (BinaryOps Add (StringLiteral "hello") (StringLiteral " world"))
        `shouldBe` Left (BinaryOpError Add TString TString)

    it "reports error for subtraction with wrong types" $
      typer (BinaryOps Sub (Literal 5) (BoolLit True))
        `shouldBe` Left (BinaryOpError Sub TInt TBool)

    it "types an if expression" $
      typer (If (BoolLit True) (Literal 10) (Literal 20)) `shouldBe` Right TInt

    it "types an if expression with different types" $
      typer (If (BoolLit True) (Literal 10) (StringLiteral "hello")) `shouldBe` Right (TSum [TInt, TString])

    it "accepts int condition (truthy)" $
      typer (If (Literal 5) (Literal 10) (Literal 20)) `shouldBe` Right TInt

    it "accepts string condition (truthy)" $
      typer (If (StringLiteral "hello") (Literal 10) (Literal 20)) `shouldBe` Right TInt

    it "accepts tuple condition (truthy)" $
      typer (If (TupleTerm [Literal 1, Literal 2]) (Literal 10) (Literal 20)) `shouldBe` Right TInt

    it "accepts unit condition (truthy)" $
      typer (If Skip (Literal 10) (Literal 20)) `shouldBe` Right TInt

    it "reports error for function condition (not truthy)" $
      typer (If (Fun [vxInt] (Var (OnlyStr vxInt))) (Literal 10) (Literal 20))
        `shouldBe` Left (ConditionNotBool (TFun [TInt] TInt))

    it "reports error for dictionary condition (not truthy)" $
      typer (If NewDictionary (Literal 10) (Literal 20))
        `shouldBe` Left (ConditionNotBool (TDictionary TUnknown))

    it "types a function with parameter type flowing through" $
      typer (Fun [vxInt] (Var (OnlyStr vxInt))) `shouldBe` Right (TFun [TInt] TInt)

    it "types a function application" $
      typer (ApplyFun (Fun [vxInt] (Var (OnlyStr vxInt))) [Literal 5]) `shouldBe` Right TInt

    it "reports error for function application with arity mismatch" $
      typer (ApplyFun (Fun [vxInt] (Var (OnlyStr vxInt))) [])
        `shouldBe` Left (ArityMismatch 1 0)

    it "reports error for function application with type mismatch" $
      typer (ApplyFun (Fun [vxInt] (Var (OnlyStr vxInt))) [BoolLit True])
        `shouldBe` Left (ArgumentTypeMismatch 0 TInt TBool)

    it "reports error for applying non-function" $
      typer (ApplyFun (Literal 5) [Literal 5])
        `shouldBe` Left (NotAFunction TInt)

    it "types a tuple" $
      typer (TupleTerm [Literal 1, BoolLit True, StringLiteral "test"]) `shouldBe` Right (TTuple [TInt, TBool, TString])

    it "types an empty tuple" $
      typer (TupleTerm []) `shouldBe` Right (TTuple [])

    it "types a nested tuple" $
      typer (TupleTerm [TupleTerm [Literal 1, Literal 2, Literal 3], BoolLit True])
        `shouldBe` Right (TTuple [TTuple [TInt, TInt, TInt], TBool])

    it "types a new dictionary" $
      typer NewDictionary `shouldBe` Right (TDictionary TUnknown)

    it "types Skip as TUnit" $
      typer Skip `shouldBe` Right TUnit

    it "types a while loop" $
      typer (While (BoolLit True) (Literal 42)) `shouldBe` Right (TSum [TUnit, TInt])

    it "types a for loop" $
      typer (For vxInt (Literal 0) (Literal 10) (Var (OnlyStr vxInt))) `shouldBe` Right (TSum [TUnit, TInt])

    it "types increment/decrement operations as TInt" $ do
      typer (PreIncrement vxInt) `shouldBe` Right TInt
      typer (PreDecrement vxInt) `shouldBe` Right TInt
      typer (PostIncrement vxInt) `shouldBe` Right TInt
      typer (PostDecrement vxInt) `shouldBe` Right TInt

    it "types control flow signals as TUnit" $ do
      typer BreakSignal `shouldBe` Right TUnit
      typer ContinueSignal `shouldBe` Right TUnit

    it "types tuple indexing with literal index - exact type" $
      typer (Bracket (TupleTerm [Literal 1, BoolLit True, StringLiteral "hi"]) (Literal 0))
        `shouldBe` Right TInt

    it "types tuple indexing with literal index - middle element" $
      typer (Bracket (TupleTerm [Literal 1, BoolLit True, StringLiteral "hi"]) (Literal 1))
        `shouldBe` Right TBool

    it "types tuple indexing with literal index - last element" $
      typer (Bracket (TupleTerm [Literal 1, BoolLit True, StringLiteral "hi"]) (Literal 2))
        `shouldBe` Right TString

    it "types tuple indexing with non-literal index - sum type" $
      typer (Bracket (TupleTerm [Literal 1, BoolLit True, StringLiteral "hi"]) (Var (OnlyStr vxInt)))
        `shouldBe` Right (TSum [TInt, TBool, TString])

    it "types tuple indexing with out of bounds literal - sum type" $
      typer (Bracket (TupleTerm [Literal 1, BoolLit True]) (Literal 5))
        `shouldBe` Right (TSum [TInt, TBool])

    it "types tuple Retrieve with literal index" $
      typer (Retrieve (TupleTerm [Literal 1, BoolLit True]) (Literal 0))
        `shouldBe` Right TInt

    it "reports error for indexing non-collection" $
      typer (Bracket (Literal 42) (Literal 0))
        `shouldBe` Left (NotIndexable TInt)

    it "reports error for indexing with wrong type" $
      typer (Bracket (TupleTerm [Literal 1, Literal 2]) (StringLiteral "bad"))
        `shouldBe` Left (InvalidIndexType (TTuple [TInt, TInt]) TString)

    it "types dictionary after inserting int" $
      typer (Merge NewDictionary (Literal 0) (Literal 42))
        `shouldBe` Right (TDictionary TInt)

    it "types dictionary after inserting string" $
      typer (Merge NewDictionary (Literal 0) (StringLiteral "hello"))
        `shouldBe` Right (TDictionary TString)

    it "types dictionary after inserting mixed types" $
      typer (Merge (Merge NewDictionary (Literal 0) (Literal 42)) (Literal 1) (StringLiteral "hello"))
        `shouldBe` Right (TDictionary (TSum [TInt, TString]))

    it "types dictionary after inserting same type twice" $
      typer (Merge (Merge NewDictionary (Literal 0) (Literal 42)) (Literal 1) (Literal 99))
        `shouldBe` Right (TDictionary TInt)

    it "types dictionary retrieval after refinement" $
      typer (Retrieve (Merge NewDictionary (Literal 0) (Literal 42)) (Literal 0))
        `shouldBe` Right TInt

    it "accepts int condition in while loop (truthy)" $
      typer (While (Literal 42) (Literal 10))
        `shouldBe` Right (TSum [TUnit, TInt])

    it "accepts string condition in while loop (truthy)" $
      typer (While (StringLiteral "go") (Literal 10))
        `shouldBe` Right (TSum [TUnit, TInt])

    it "reports error for function condition in while loop (not truthy)" $
      typer (While (Fun [vxInt] (Var (OnlyStr vxInt))) (Literal 10))
        `shouldBe` Left (ConditionNotBool (TFun [TInt] TInt))

    it "reports error for dictionary condition in while loop (not truthy)" $
      typer (While NewDictionary (Literal 10))
        `shouldBe` Left (ConditionNotBool (TDictionary TUnknown))

    it "reports error for for loop with non-int range" $
      typer (For vxInt (BoolLit True) (Literal 10) Skip)
        `shouldBe` Left (ForRangeNotInt TBool TInt)

    it "reports error for unary operation with wrong type" $
      typer (UnaryOps Not (Literal 5))
        `shouldBe` Left (UnaryOpError Not TInt)
