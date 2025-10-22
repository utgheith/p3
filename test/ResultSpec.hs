module ResultSpec (spec) where

import Control.Exception (evaluate)
import Control.Monad.Except
import Result
import Test.Hspec

forceResult :: Result e a -> ()
forceResult (Ok _) = ()
forceResult (Err e) = e `seq` ()

spec :: Spec
spec = do
  describe "Result" $ do
    it "should be a Functor" $ do
      fmap (+ 1) (Ok (5 :: Int)) `shouldBe` (Ok 6 :: Result String Int)
      fmap (+ 1) (Err ("error" :: String)) `shouldBe` (Err "error" :: Result String Int)

    it "should be an Applicative" $ do
      (pure (5 :: Int) :: Result String Int) `shouldBe` (Ok 5 :: Result String Int)
      (Ok (+ 1) :: Result String (Int -> Int)) <*> (Ok 5 :: Result String Int) `shouldBe` (Ok 6 :: Result String Int)
      (Err ("error" :: String) :: Result String (Int -> Int)) <*> (Ok 5 :: Result String Int) `shouldBe` (Err "error" :: Result String Int)
      (Ok (+ 1) :: Result String (Int -> Int)) <*> (Err ("error" :: String) :: Result String Int) `shouldBe` (Err "error" :: Result String Int)

    it "should be a Monad" $ do
      (Ok (5 :: Int) >>= (\x -> Ok (x + 1))) `shouldBe` (Ok 6 :: Result String Int)
      (Err ("error" :: String) >>= (\x -> Ok (x + 1))) `shouldBe` (Err "error" :: Result String Int)
      (Ok (5 :: Int) >> Err ("another error" :: String)) `shouldBe` (Err "another error" :: Result String Int)

    it "should be a MonadFail" $ do
      evaluate (forceResult (fail "monad fail" :: Result String Int)) `shouldThrow` errorCall "monad fail"

    it "should be a MonadError" $ do
      (throwError ("error" :: String) :: Result String Int) `shouldBe` (Err "error" :: Result String Int)
      catchError (Ok (5 :: Int) :: Result String Int) (\_ -> Ok 0) `shouldBe` (Ok 5 :: Result String Int)
      catchError (Err ("error" :: String) :: Result String Int) (\_ -> Ok 0) `shouldBe` (Ok 0 :: Result String Int)
      catchError (Err ("error" :: String) :: Result String Int) (\e -> Err (e ++ " handled")) `shouldBe` (Err "error handled" :: Result String Int)
