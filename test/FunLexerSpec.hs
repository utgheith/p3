module FunLexerSpec (spec) where

import FunLexer
import Test.Hspec

spec :: Spec
spec =
  describe "lexer" $ do
    it "handles an empty string" $
      lexer "" `shouldBe` []

    it "handles whitespace" $
      lexer "  \t\n  " `shouldBe` []

    it "lexes a number" $
      lexer "123" `shouldBe` [Num 123]

    it "lexes an identifier" $
      lexer "abc" `shouldBe` [Ident "abc"]

    it "lexes a keyword" $
      lexer "if" `shouldBe` [Keyword "if"]

    it "lexes a symbol" $
      lexer "=" `shouldBe` [Symbol "="]

    it "lexes a multi-character symbol" $
      lexer "<=" `shouldBe` [Symbol "<="]

    it "lexes a simple expression" $
      lexer "var x = 1" `shouldBe` [Keyword "var", Ident "x", Symbol "=", Num 1]

    it "lexes a string" $
      lexer "\"hello\"" `shouldBe` [StringLiteralLexed "hello"]

    it "lexes a string with single quotes" $
      lexer "'\"'" `shouldBe` [StringLiteralLexed "\""]

    it "handles an unexpected character" $
      lexer "@" `shouldBe` [Error "Unexpected character: @"]
