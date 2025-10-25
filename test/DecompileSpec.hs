module DecompileSpec (spec) where

import Control.Monad.State.Lazy (evalStateT)
import Decompile (decompile)
import FunLexer (lexer)
import FunSyntax (term)
import ParserCombinators (eof)
import Result (Result (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "decompile" $ do
    it "decompiles a simple expression" $ do
      let expected = "1"
      case evalStateT (term <* eof) (lexer expected) of
        Ok term' -> decompile term' `shouldBe` expected
        Err e -> expectationFailure $ "Parsing failed with: " ++ e

    it "decompiles an if expression" $ do
      let input = "if (true) (1) else (2)"
      let expected = "If (True) (1) (2)"
      case evalStateT (term <* eof) (lexer input) of
        Ok term' -> decompile term' `shouldBe` expected
        Err e -> expectationFailure $ "Parsing failed with: " ++ e

    it "decompiles a try-catch expression" $ do
      let input = "try (1) catch Any (2)"
      let expected = "Try (1) (Any) (2)"
      case evalStateT (term <* eof) (lexer input) of
        Ok term' -> decompile term' `shouldBe` expected
        Err e -> expectationFailure $ "Parsing failed with: " ++ e
