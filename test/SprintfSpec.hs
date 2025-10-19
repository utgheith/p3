module SprintfSpec (spec) where

import Sprintf
import Test.Hspec

spec :: Spec
spec = do
  describe "sprintf" $ do
    it "acts as the identity when given no format specifiers" $ do
      sprintf "string" [] `shouldBe` "string"
    it "handles double percents" $ do
      sprintf "%%%%" [] `shouldBe` "%%"
    it "substitutes words correctly" $ do
      sprintf
        "a %s is a %s in the %s of %s"
        ["", "", "image", "I"]
        `shouldBe` "a  is a  in the image of I"
  describe "the percent operator" $ do
    it "does sprintf" $ do
      "%s%s" % ["a", "b"] `shouldBe` "ab"
  describe "the left-shift operator" $ do
    it "converts singleton lists" $ do
      "a" << [] `shouldBe` ["\"a\""]
    it "converts pairs" $ do
      "a" << "b" << [] `shouldBe` ["\"a\"", "\"b\""]
    it "converts triples" $ do
      "a" << "b" << "c" << [] `shouldBe` ["\"a\"", "\"b\"", "\"c\""]
    it "shows numbers" $ do
      (1 :: Int) << (0 :: Int) << (2 :: Int) << (4 :: Int) << [] `shouldBe` ["1", "0", "2", "4"]
    it "works with sprintf" $ do
      "%s number %s" % "project" << (3 :: Int) << [] `shouldBe` "\"project\" number 3"
