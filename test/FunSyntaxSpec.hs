{-# LANGUAGE LambdaCase #-}

module FunSyntaxSpec (spec) where

import FunLexer (Token (..))
import FunSyntax (parse, prog)
import ParserCombinators (eof)
import Term (BinaryOp (..), ErrorKind (..), ErrorKindOrAny (..), Ref (..), Term (..))
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
      it "parses integer literals" $ do
        let result = parse "42" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (Literal 42, [])

      it "parses string literals" $ do
        let result = parse "\"hello\"" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (StringLiteral "hello", [])

      it "parses boolean literals" $ do
        let result = parse "true false" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (Seq (BoolLit True) (BoolLit False), [])

    describe "variables" $ do
      it "parses variable references" $ do
        let result = parse "x" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (Var (OnlyStr "x"), [])

      it "parses variable assignments" $ do
        let result = parse "x = 42" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (Let (OnlyStr "x") (Literal 42), [])

    describe "arithmetic expressions" $ do
      it "parses addition" $ do
        let result = parse "x + y" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (BinaryOps Add (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses subtraction" $ do
        let result = parse "x - y" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (BinaryOps Sub (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses multiplication" $ do
        let result = parse "x * y" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (BinaryOps Mul (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses division" $ do
        let result = parse "x / y" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (BinaryOps Div (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses modulus" $ do
        let result = parse "x % y" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (BinaryOps Mod (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses less than" $ do
        let result = parse "x < y" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (BinaryOps Lt (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses greater than" $ do
        let result = parse "x > y" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (BinaryOps Gt (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses less than equal" $ do
        let result = parse "x <= y" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (BinaryOps Lte (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses greater than equal" $ do
        let result = parse "x >= y" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (BinaryOps Gte (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses equal to" $ do
        let result = parse "x == y" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (BinaryOps Eq (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses not equal" $ do
        let result = parse "x != y" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (BinaryOps Neq (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses and" $ do
        let result = parse "x && y" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (BinaryOps And (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses or" $ do
        let result = parse "x || y" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (BinaryOps Or (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses pow" $ do
        let result = parse "x ** y" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (BinaryOps Pow (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses xor" $ do
        let result = parse "x ^ y" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (BinaryOps Xor (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses complex expressions with precedence" $ do
        let result = parse "x + y * z" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (BinaryOps Add (Var (OnlyStr "x")) (BinaryOps Mul (Var (OnlyStr "y")) (Var (OnlyStr "z"))), [])

    describe "pre/post inc/dec" $ do
      it "parses pre increment" $ do
        let result = parse "++x" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (PreIncrement "x", [])

      it "parses post increment" $ do
        let result = parse "x++" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (PostIncrement "x", [])

      it "parses pre decrement" $ do
        let result = parse "--x" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (PreDecrement "x", [])

      it "parses post decrement" $ do
        let result = parse "x--" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (PostDecrement "x", [])

    describe "control flow" $ do
      it "parses if statements" $ do
        let result = parse "if (x > 0) { y = 1 } else { y = 0 }" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (If (BinaryOps Gt (Var (OnlyStr "x")) (Literal 0)) (Let (OnlyStr "y") (Literal 1)) (Let (OnlyStr "y") (Literal 0)), [])

      it "parses while loops" $ do
        let result = parse "while (x > 0) { x = x - 1 }" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (While (BinaryOps Gt (Var (OnlyStr "x")) (Literal 0)) (Let (OnlyStr "x") (BinaryOps Sub (Var (OnlyStr "x")) (Literal 1))), [])

    describe "functions" $ do
      it "parses function definitions" $ do
        let result = parse "fun f() { print 42 }" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (Let (OnlyStr "f") (Fun [] (Write (Literal 42))), [])

      it "parses function calls" $ do
        let result = parse "f()" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (ApplyFun (Var (OnlyStr "f")) [], [])

      it "parses function calls with arguments" $ do
        let result = parse "f(x, y)" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (ApplyFun (Var (OnlyStr "f")) [Var (OnlyStr "x"), Var (OnlyStr "y")], [])

    describe "tuples" $ do
      it "parses tuple creation" $ do
        let result = parse "x = [1, 2, 3]" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (Let (OnlyStr "x") (TupleTerm [Literal 1, Literal 2, Literal 3]), [])

      it "parses tuple access" $ do
        let result = parse "t[0]" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (Var (Bracket (OnlyStr "t") (Literal 0)), [])

    describe "dictionary" $ do
      it "parses dictionary creation" $ do
        let result = parse "#[]" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (NewDictionary, [])

    describe "try-catch creation" $ do
      it "parses try-catch any" $ do
        let result = parse "try x catch Any 1" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (Try (Var (OnlyStr "x")) (Any) (Literal 1), [])

      it "parses try-catch arithmetic" $ do
        let result = parse "try x catch Arithmetic 1" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (Try (Var (OnlyStr "x")) (Specific Arithmetic) (Literal 1), [])

      it "parses try-catch type" $ do
        let result = parse "try x catch Type 1" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (Try (Var (OnlyStr "x")) (Specific Type) (Literal 1), [])

      it "parses try-catch input" $ do
        let result = parse "try x catch Input 1" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (Try (Var (OnlyStr "x")) (Specific Input) (Literal 1), [])

      it "parses try-catch variable not found" $ do
        let result = parse "try x catch VariableNotFound 1" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (Try (Var (OnlyStr "x")) (Specific VariableNotFound) (Literal 1), [])

      it "parses try-catch arguments" $ do
        let result = parse "try x catch Arguments 1" $ do
              t <- prog
              _ <- eof
              return t
        result `shouldBe` Right (Try (Var (OnlyStr "x")) (Specific Arguments) (Literal 1), [])

    describe "error cases" $ do
      it "fails on invalid syntax" $ do
        let result = parse "x +" $ do
              t <- prog
              _ <- eof
              return t
        case result of
          Left _ -> True `shouldBe` True
          Right _ -> False `shouldBe` True

  describe "Integration tests" $ do
    it "parses a complete program" $ do
      let program = "x = 10\n y = x + 5\n print y"
      let result = parseString program
      case result of
        Left err -> fail $ "Parse error: " ++ err
        Right (ast, _) -> do
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
              Right (ast, _) -> do
                ast `shouldSatisfy` const True -- Just check it parses successfully
        )
        testFiles

  describe "File-based tests" $ do
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
        Right (ast, _) -> do
          ast `shouldSatisfy` const True
