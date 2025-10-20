module FunLexerSpec (spec) where

import FunLexer
import Test.Hspec

spec :: Spec
spec = do
  describe "lexer" $ do
    it "handles an empty string" $ do
      lexer "" `shouldBe` []

    it "handles whitespace" $ do
      lexer "  \t\n  " `shouldBe` []

    it "lexes a number" $ do
      lexer "123" `shouldBe` [Num 123]

    it "lexes an identifier" $ do
      lexer "abc" `shouldBe` [Ident "abc"]

    it "lexes a keyword" $ do
      lexer "if" `shouldBe` [Keyword "if"]

    it "lexes a symbol" $ do
      lexer "=" `shouldBe` [Symbol "="]

    it "lexes a multi-character symbol" $ do
      lexer "<=" `shouldBe` [Symbol "<="]

    it "lexes a simple expression" $ do
      lexer "var x = 1" `shouldBe` [Keyword "var", Ident "x", Symbol "=", Num 1]

    it "handles an unexpected character" $ do
      lexer "@" `shouldBe` [Error "Unexpected character: @"]

    it "handles an empty class" $ do
      lexer "class MyClass {}" `shouldBe` [Keyword "class", Ident "MyClass", Symbol "{", Symbol "}"]
    
    it "handles a class with a method" $ do
      lexer "class MyClass { fun myMethod() {} }" `shouldBe` 
        [ Keyword "class", Ident "MyClass", Symbol "{"
        , Keyword "fun", Ident "myMethod", Symbol "(", Symbol ")", Symbol "{", Symbol "}", Symbol "}"
        ]
    it "handles a class with an instance variable and a method" $ do
      lexer "class MyClass { var x = 10 fun getX() { return x; } }" `shouldBe` 
        [ Keyword "class", Ident "MyClass", Symbol "{"
        , Keyword "var", Ident "x", Symbol "=", Num 10
        , Keyword "fun", Ident "getX", Symbol "(", Symbol ")", Symbol "{"
        , Keyword "return", Ident "x", Symbol ";"
        , Symbol "}", Symbol "}"
        ]
