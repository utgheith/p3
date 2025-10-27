module TyperSpec (spec) where

import Term
import Test.Hspec
import TypeSignature (TypeSignature (..), TypedName)
import Typer

vx :: TypedName
vx = ("x", TUnknown)

spec :: Spec
spec = do
  describe "typer" $ do
    it "types an integer literal" $
      typer (Literal 10) `shouldBe` TInt

    it "types a boolean literal" $
      typer (BoolLit True) `shouldBe` TBool

    it "types a string literal" $
      typer (StringLiteral "hello") `shouldBe` TString

    it "types a variable" $
      typer (Var (OnlyStr vx)) `shouldBe` TUnit

    it "types a let expression" $
      typer (Let (OnlyStr vx) (Literal 5)) `shouldBe` TInt

    it "types an addition" $
      typer (BinaryOps Add (Literal 5) (Literal 10)) `shouldBe` TInt

    it "types a subtraction with a type error" $
      typer (BinaryOps Sub (Literal 5) (BoolLit True)) `shouldBe` TUnknown

    it "types an if expression" $
      typer (If (BoolLit True) (Literal 10) (Literal 20)) `shouldBe` TInt

    it "types an if expression with different types" $
      typer (If (BoolLit True) (Literal 10) (StringLiteral "hello")) `shouldBe` TSum [TInt, TString]

    it "types an if expression with a non-boolean condition" $
      typer (If (Literal 5) (Literal 10) (Literal 20)) `shouldBe` TUnknown

    it "types a function" $
      typer (Fun [vx] (Var (OnlyStr vx))) `shouldBe` TFun [TUnknown] TUnit

    it "types a function application" $
      typer (ApplyFun (Fun [vx] (Var (OnlyStr vx))) [Literal 5]) `shouldBe` TUnit

    it "types a function application with a type error" $
      typer (ApplyFun (Literal 5) [Literal 5]) `shouldBe` TUnknown
