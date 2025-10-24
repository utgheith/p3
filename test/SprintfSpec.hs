module SprintfSpec (spec) where

import Sprintf
import Test.Hspec

spec :: Spec
spec = do
  describe "sprintf" $ do
    it "acts as the identity when given no format specifiers" $
      sprintf "string" [] `shouldBe` "string"
    it "handles double percents" $
      sprintf "%%%%" [] `shouldBe` "%%"
    it "substitutes words correctly" $
      sprintf
        "a %s is a %s in the %s of %s"
        ["", "", "image", "I"]
        `shouldBe` "a  is a  in the image of I"
  describe "the percent operator" $ do
    it "does sprintf" $
      "%s%s" % ["a", "b"] `shouldBe` "ab"
    it "has correct precedence" $
      "%s" % (5 :: Int) << [] `shouldBe` "5"
  describe "the left-shift operator" $ do
    it "converts singleton lists" $
      "a" << [] `shouldBe` ["\"a\""]
    it "converts pairs" $
      "a" << "b" << [] `shouldBe` ["\"a\"", "\"b\""]
    it "converts triples" $
      "a" << "b" << "c" << [] `shouldBe` ["\"a\"", "\"b\"", "\"c\""]
    it "shows numbers" $
      (1 :: Int) << (0 :: Int) << (2 :: Int) << (4 :: Int) << [] `shouldBe` ["1", "0", "2", "4"]
    it "works with sprintf" $
      "%s number %s" % "project" << (3 :: Int) << [] `shouldBe` "\"project\" number 3"
