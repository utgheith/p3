module TyperSpec (spec) where

import System.IO (stderr)
import Term
import Test.Hspec
import TypeSignature (TypeSignature (..), TypedName)
import Typer

vx :: TypedName
vx = ("x", TUnknown)

spec :: Spec
spec = do
  describe "typer" $ do
    it "types an integer literal" $ do
      out <- typer stderr (Literal 10)
      out `shouldBe` Right TInt

    it "types a boolean literal" $ do
      out <- typer stderr (BoolLit True)
      out `shouldBe` Right TBool

    it "types a string literal" $ do
      out <- typer stderr (StringLiteral "hello")
      out `shouldBe` Right TString

    it "types a variable" $ do
      out <- typer stderr (Var (OnlyStr vx))
      out `shouldBe` Right TUnit

    it "types a let expression" $ do
      out <- typer stderr (Let (OnlyStr vx) (Literal 5))
      out `shouldBe` Right TInt

    it "types an addition" $ do
      out <- typer stderr (BinaryOps Add (Literal 5) (Literal 10))
      out `shouldBe` Right TInt

    it "types a subtraction with a type error" $ do
      out <- typer stderr (BinaryOps Sub (Literal 5) (BoolLit True))
      out `shouldBe` Right TUnknown

    it "types an if expression" $ do
      out <- typer stderr (If (BoolLit True) (Literal 10) (Literal 20))
      out `shouldBe` Right TInt

    it "types an if expression with different types" $ do
      out <- typer stderr (If (BoolLit True) (Literal 10) (StringLiteral "hello"))
      out `shouldBe` Right (TSum [TInt, TString])

    it "types an if expression with a non-boolean condition" $ do
      out <- typer stderr (If (Literal 5) (Literal 10) (Literal 20))
      out `shouldBe` Right TUnknown

    it "types a function" $ do
      out <- typer stderr (Fun [vx] (Var (OnlyStr vx)))
      out `shouldBe` Right (TFun [TUnknown] TUnit)

    it "types a function application" $ do
      out <- typer stderr (ApplyFun (Fun [vx] (Var (OnlyStr vx))) [Literal 5])
      out `shouldBe` Right TUnit

    it "types a function application with a type error" $ do
      out <- typer stderr (ApplyFun (Literal 5) [Literal 5])
      out `shouldBe` Right TUnknown
