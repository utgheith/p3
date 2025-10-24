{-# LANGUAGE LambdaCase #-}

module FunSyntaxSpec (spec) where

import FunLexer (Token (..))
import FunSyntax (parse, prog)
import ParserCombinators (eof)
import Term (BinaryOp (..), ErrorKind (..), ErrorKindOrAny (..), Term (..))
import Test.Hspec

-- Helper function to parse from a string
parseString :: String -> Either String (Term, [Token])
parseString input = parse input $ do
  t <- prog
  _ <- eof
  return t

-- Helper function to parse from a file
parseFile :: FilePath -> IO (Either String (Term, [Token]))
parseFile path = do
  content <- readFile path
  return $ parseString content

spec :: Spec
spec = do
  describe "Parser" $ do
    describe "basic literals" $ do
      it "parses integer literals" $
        parseString "42" `shouldBe` Right (Literal 42, [])

      it "parses string literals" $
        parseString "\"hello\"" `shouldBe` Right (StringLiteral "hello", [])

      it "parses boolean literals" $
        parseString "true false" `shouldBe` Right (Seq (BoolLit True) (BoolLit False), [])

    describe "variables" $ do
      it "parses variable references" $
        parseString "x" `shouldBe` Right (Var (OnlyStr "x"), [])

      it "parses variable assignments" $
        parseString "x = 42" `shouldBe` Right (Let (OnlyStr "x") (Literal 42), [])

    describe "arithmetic expressions" $ do
      it "parses addition" $
        parseString "x + y" `shouldBe` Right (BinaryOps Add (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses subtraction" $
        parseString "x - y" `shouldBe` Right (BinaryOps Sub (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses multiplication" $
        parseString "x * y" `shouldBe` Right (BinaryOps Mul (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses division" $
        parseString "x / y" `shouldBe` Right (BinaryOps Div (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses modulus" $
        parseString "x % y" `shouldBe` Right (BinaryOps Mod (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses less than" $
        parseString "x < y" `shouldBe` Right (BinaryOps Lt (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses greater than" $
        parseString "x > y" `shouldBe` Right (BinaryOps Gt (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses less than equal" $
        parseString "x <= y" `shouldBe` Right (BinaryOps Lte (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses greater than equal" $
        parseString "x >= y" `shouldBe` Right (BinaryOps Gte (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses equal to" $
        parseString "x == y" `shouldBe` Right (BinaryOps Eq (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses not equal" $
        parseString "x != y" `shouldBe` Right (BinaryOps Neq (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses and" $
        parseString "x && y" `shouldBe` Right (BinaryOps And (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses or" $
        parseString "x || y" `shouldBe` Right (BinaryOps Or (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses pow" $
        parseString "x ** y" `shouldBe` Right (BinaryOps Pow (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses xor" $
        parseString "x ^ y" `shouldBe` Right (BinaryOps Xor (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses complex expressions with precedence" $
        parseString "x + y * z" `shouldBe` Right (BinaryOps Add (Var (OnlyStr "x")) (BinaryOps Mul (Var (OnlyStr "y")) (Var (OnlyStr "z"))), [])

    describe "pre/post inc/dec" $ do
      it "parses pre increment" $
        parseString "++x" `shouldBe` Right (PreIncrement "x", [])

      it "parses post increment" $
        parseString "x++" `shouldBe` Right (PostIncrement "x", [])

      it "parses pre decrement" $
        parseString "--x" `shouldBe` Right (PreDecrement "x", [])

      it "parses post decrement" $
        parseString "x--" `shouldBe` Right (PostDecrement "x", [])

    describe "control flow" $ do
      it "parses if statements" $
        parseString "if (x > 0) { y = 1 } else { y = 0 }" `shouldBe` Right (If (BinaryOps Gt (Var (OnlyStr "x")) (Literal 0)) (Let (OnlyStr "y") (Literal 1)) (Let (OnlyStr "y") (Literal 0)), [])

      it "parses while loops" $
        parseString "while (x > 0) { x = x - 1 }" `shouldBe` Right (While (BinaryOps Gt (Var (OnlyStr "x")) (Literal 0)) (Let (OnlyStr "x") (BinaryOps Sub (Var (OnlyStr "x")) (Literal 1))), [])

    describe "functions" $ do
      it "parses function definitions" $
        parseString "fun f() { print 42 }" `shouldBe` Right (Let (OnlyStr "f") (Fun [] (Write (Literal 42))), [])

      it "parses function calls" $
        parseString "f()" `shouldBe` Right (ApplyFun (Var (OnlyStr "f")) [], [])

      it "parses function calls with arguments" $
        parseString "f(x, y)" `shouldBe` Right (ApplyFun (Var (OnlyStr "f")) [Var (OnlyStr "x"), Var (OnlyStr "y")], [])

      it "parses method calls" $ do
        parseString "m.invoke(x, y)" `shouldBe` Right (ApplyFun (Var (OnlyStr "invoke")) $ map (Var . OnlyStr) ["m", "x", "y"], [])

    describe "tuples" $ do
      it "parses tuple creation" $
        parseString "x = [1, 2, 3]" `shouldBe` Right (Let (OnlyStr "x") (TupleTerm [Literal 1, Literal 2, Literal 3]), [])

      it "parses tuple access" $
        parseString "t[0]" `shouldBe` Right (Var (Bracket (OnlyStr "t") (Literal 0)), [])

    describe "dictionary" $
      it "parses dictionary creation" $
        parseString "#[]" `shouldBe` Right (NewDictionary, [])

    describe "try-catch creation" $ do
      it "parses try-catch any" $
        parseString "try x catch Any 1" `shouldBe` Right (Try (Var (OnlyStr "x")) Any (Literal 1), [])

      it "parses try-catch arithmetic" $
        parseString "try x catch Arithmetic 1" `shouldBe` Right (Try (Var (OnlyStr "x")) (Specific Arithmetic) (Literal 1), [])

      it "parses try-catch type" $
        parseString "try x catch Type 1" `shouldBe` Right (Try (Var (OnlyStr "x")) (Specific Type) (Literal 1), [])

      it "parses try-catch input" $
        parseString "try x catch Input 1" `shouldBe` Right (Try (Var (OnlyStr "x")) (Specific Input) (Literal 1), [])

      it "parses try-catch variable not found" $
        parseString "try x catch VariableNotFound 1" `shouldBe` Right (Try (Var (OnlyStr "x")) (Specific VariableNotFound) (Literal 1), [])

      it "parses try-catch arguments" $
        parseString "try x catch Arguments 1" `shouldBe` Right (Try (Var (OnlyStr "x")) (Specific Arguments) (Literal 1), [])

    describe "statement separators" $ do
      it "separates statements" $ do
        parseString "1 + 2; if 3 4 else 5; " `shouldBe` Right (Seq (BinaryOps Add (Literal 1) (Literal 2)) (If (Literal 3) (Literal 4) (Literal 5)), [])

    describe "error cases" $
      it "fails on invalid syntax" $ do
        let result = parseString "x +"
        case result of
          Left _ -> True `shouldBe` True
          Right _ -> False `shouldBe` True

  describe "Integration tests" $ do
    it "parses a complete program" $ do
      let program = "x = 10\n y = x + 5\n print y"
      let result = parseString program
      case result of
        Left err -> fail $ "Parse error: " ++ err
        Right (ast, _) ->
          ast `shouldSatisfy` \case
            Seq _ _ -> True
            _ -> False

    it "parses from test files" $ do
      let testFiles = ["test/t1.fun"]
      mapM_
        ( \file -> do
            result <- parseFile file
            case result of
              Left err -> fail $ "Failed to parse " ++ file ++ ": " ++ err
              Right (ast, _) ->
                ast `shouldSatisfy` const True -- Just check it parses successfully
        )
        testFiles

  describe "File-based tests" $
    it "can parse complex programs from files" $ do
      let complexProgram =
            unlines
              [ "var x = 10",
                "fun f(n) {",
                "    return n * 2",
                "}",
                "y = f(x)",
                "if (y > 15) {",
                "    print y",
                "} else {",
                "    print 0",
                "}"
              ]
      let result = parseString complexProgram
      case result of
        Left err -> fail $ "Parse error: " ++ err
        Right (ast, _) ->
          ast `shouldSatisfy` const True
