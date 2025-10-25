{-# LANGUAGE LambdaCase #-}

module FunSyntaxSpec (spec) where

import FunLexer (Token (..))
import FunSyntax (parse, prog)
import ParserCombinators (eof)
import Result (Result (..))
import Term (BinaryOp (..), ErrorKind (..), ErrorKindOrAny (..), Term (..))
import Test.Hspec

-- Helper function to parse from a string
parseString :: String -> Result String (Term, [Token])
parseString input = parse input $ do
  t <- prog
  _ <- eof
  return t

-- Helper function to parse from a file
parseFile :: FilePath -> IO (Result String (Term, [Token]))
parseFile path = do
  content <- readFile path
  return $ parseString content

spec :: Spec
spec = do
  describe "Parser" $ do
    describe "basic literals" $ do
      it "parses integer literals" $
        parseString "42" `shouldBe` Ok (Literal 42, [])

      it "parses string literals" $
        parseString "\"hello\"" `shouldBe` Ok (StringLiteral "hello", [])

      it "parses boolean literals" $
        parseString "true false" `shouldBe` Ok (Seq (BoolLit True) (BoolLit False), [])

    describe "variables" $ do
      it "parses variable references" $
        parseString "x" `shouldBe` Ok (Var (OnlyStr "x"), [])

      it "parses variable assignments" $
        parseString "x = 42" `shouldBe` Ok (Let (OnlyStr "x") (Literal 42), [])

    describe "arithmetic expressions" $ do
      it "parses addition" $
        parseString "x + y" `shouldBe` Ok (BinaryOps Add (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses subtraction" $
        parseString "x - y" `shouldBe` Ok (BinaryOps Sub (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses multiplication" $
        parseString "x * y" `shouldBe` Ok (BinaryOps Mul (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses division" $
        parseString "x / y" `shouldBe` Ok (BinaryOps Div (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses modulus" $
        parseString "x % y" `shouldBe` Ok (BinaryOps Mod (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses less than" $
        parseString "x < y" `shouldBe` Ok (BinaryOps Lt (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses greater than" $
        parseString "x > y" `shouldBe` Ok (BinaryOps Gt (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses less than equal" $
        parseString "x <= y" `shouldBe` Ok (BinaryOps Lte (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses greater than equal" $
        parseString "x >= y" `shouldBe` Ok (BinaryOps Gte (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses equal to" $
        parseString "x == y" `shouldBe` Ok (BinaryOps Eq (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses not equal" $
        parseString "x != y" `shouldBe` Ok (BinaryOps Neq (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses and" $
        parseString "x && y" `shouldBe` Ok (BinaryOps And (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses or" $
        parseString "x || y" `shouldBe` Ok (BinaryOps Or (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses pow" $
        parseString "x ** y" `shouldBe` Ok (BinaryOps Pow (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses xor" $
        parseString "x ^ y" `shouldBe` Ok (BinaryOps Xor (Var (OnlyStr "x")) (Var (OnlyStr "y")), [])

      it "parses complex expressions with precedence" $
        parseString "x + y * z" `shouldBe` Ok (BinaryOps Add (Var (OnlyStr "x")) (BinaryOps Mul (Var (OnlyStr "y")) (Var (OnlyStr "z"))), [])

    describe "pre/post inc/dec" $ do
      it "parses pre increment" $
        parseString "++x" `shouldBe` Ok (PreIncrement "x", [])

      it "parses post increment" $
        parseString "x++" `shouldBe` Ok (PostIncrement "x", [])

      it "parses pre decrement" $
        parseString "--x" `shouldBe` Ok (PreDecrement "x", [])

      it "parses post decrement" $
        parseString "x--" `shouldBe` Ok (PostDecrement "x", [])

    describe "control flow" $ do
      it "parses if statements" $
        parseString "if (x > 0) { y = 1 } else { y = 0 }" `shouldBe` Ok (If (BinaryOps Gt (Var (OnlyStr "x")) (Literal 0)) (Let (OnlyStr "y") (Literal 1)) (Let (OnlyStr "y") (Literal 0)), [])

      it "parses while loops" $
        parseString "while (x > 0) { x = x - 1 }" `shouldBe` Ok (While (BinaryOps Gt (Var (OnlyStr "x")) (Literal 0)) (Let (OnlyStr "x") (BinaryOps Sub (Var (OnlyStr "x")) (Literal 1))), [])

    describe "for-in loops" $ do
      it "parses for-in with tuple" $
        parseString "for i in [1,2,3] print i" `shouldBe` Right (ForIn "i" (TupleTerm [Literal 1, Literal 2, Literal 3]) (Write (Var (OnlyStr "i"))), [])

      it "parses for-in with variable" $
        parseString "for x in items { x }" `shouldBe` Right (ForIn "x" (Var (OnlyStr "items")) (Var (OnlyStr "x")), [])

      it "parses for-in with range built-in" $
        parseString "for i in range(5) print i"
          `shouldBe` Right (ForIn "i" (Range (Literal 5)) (Write (Var (OnlyStr "i"))), [])

      it "parses for-in with block body" $
        parseString "for x in list { var y = x print y }"
          `shouldBe` Right (ForIn "x" (Var (OnlyStr "list")) (Seq (Let (OnlyStr "y") (Var (OnlyStr "x"))) (Write (Var (OnlyStr "y")))), [])

    describe "functions" $ do
      it "parses function definitions" $
        parseString "fun f() { print 42 }" `shouldBe` Ok (Let (OnlyStr "f") (Fun [] (Write (Literal 42))), [])

      it "parses function calls" $
        parseString "f()" `shouldBe` Ok (ApplyFun (Var (OnlyStr "f")) [], [])

      it "parses function calls with arguments" $
        parseString "f(x, y)" `shouldBe` Ok (ApplyFun (Var (OnlyStr "f")) [Var (OnlyStr "x"), Var (OnlyStr "y")], [])

      it "parses method calls" $
        parseString "m.invoke(x, y)" `shouldBe` Ok (ApplyFun (Var (OnlyStr "invoke")) $ map (Var . OnlyStr) ["m", "x", "y"], [])

      it "parses method calls on complex expressions" $
        parseString "(1 + 2).invoke(3, 4)" `shouldBe` Ok (ApplyFun (Var (OnlyStr "invoke")) [BinaryOps Add (Literal 1) (Literal 2), Literal 3, Literal 4], [])

    describe "tuples" $ do
      it "parses tuple creation" $
        parseString "x = [1, 2, 3]" `shouldBe` Ok (Let (OnlyStr "x") (TupleTerm [Literal 1, Literal 2, Literal 3]), [])

      it "parses tuple access" $
        parseString "t[0]" `shouldBe` Ok (Var (Bracket (OnlyStr "t") (Literal 0)), [])

    describe "dictionary" $
      it "parses dictionary creation" $
        parseString "#[]" `shouldBe` Ok (NewDictionary, [])

    describe "try-catch creation" $ do
      it "parses try-catch any" $
        parseString "try x catch Any 1" `shouldBe` Ok (Try (Var (OnlyStr "x")) Any (Literal 1), [])

      it "parses try-catch arithmetic" $
        parseString "try x catch Arithmetic 1" `shouldBe` Ok (Try (Var (OnlyStr "x")) (Specific Arithmetic) (Literal 1), [])

      it "parses try-catch type" $
        parseString "try x catch Type 1" `shouldBe` Ok (Try (Var (OnlyStr "x")) (Specific Type) (Literal 1), [])

      it "parses try-catch input" $
        parseString "try x catch Input 1" `shouldBe` Ok (Try (Var (OnlyStr "x")) (Specific Input) (Literal 1), [])

      it "parses try-catch variable not found" $
        parseString "try x catch VariableNotFound 1" `shouldBe` Ok (Try (Var (OnlyStr "x")) (Specific VariableNotFound) (Literal 1), [])

      it "parses try-catch arguments" $
        parseString "try x catch Arguments 1" `shouldBe` Ok (Try (Var (OnlyStr "x")) (Specific Arguments) (Literal 1), [])

    describe "statement separators" $
      it "separates statements" $
        parseString "1 + 2; if 3 4 else 5; " `shouldBe` Ok (Seq (BinaryOps Add (Literal 1) (Literal 2)) (If (Literal 3) (Literal 4) (Literal 5)), [])

    describe "error cases" $
      it "fails on invalid syntax" $ do
        let result = parseString "x +"
        case result of
          Err _ -> True `shouldBe` True
          Ok _ -> False `shouldBe` True

  describe "Integration tests" $ do
    it "parses a complete program" $ do
      let program = "x = 10\n y = x + 5\n print y"
      let result = parseString program
      case result of
        Err err -> fail $ "Parse error: " ++ err
        Ok (ast, _) ->
          ast `shouldSatisfy` \case
            Seq _ _ -> True
            _ -> False

    it "parses from test files" $ do
      let testFiles = ["test/t1.fun"]
      mapM_
        ( \file -> do
            result <- parseFile file
            case result of
              Err err -> fail $ "Failed to parse " ++ file ++ ": " ++ err
              Ok (ast, _) ->
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
        Err err -> fail $ "Parse error: " ++ err
        Ok (ast, _) ->
          ast `shouldSatisfy` const True
