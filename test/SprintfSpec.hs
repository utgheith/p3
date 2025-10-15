module SprintfSpec (spec) where

import Sprintf
import Test.Hspec

spec :: Spec
spec = do
  describe "sprintf" $ do
    it "acts as the identity when given no format specifiers" $ do
      sprintf "string" [] `shouldBe` "string"
    it "handles " $ do
      sprintf "%%%%" [] `shouldBe` "%%"
    it "substitutes words correctly" $ do
      sprintf 
        "a %s is a %s in the %s of %s" 
        ["", "", "image", "I"] 
        `shouldBe` "a  is a  in the image of I"
