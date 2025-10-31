{-# LANGUAGE OverloadedStrings #-}

module EndToEndSpec (spec) where

import Runner (runner)
import System.Directory (listDirectory)
import System.FilePath (replaceExtension, takeExtension, (</>))
import System.IO (IOMode (..), withFile)
import Test.Hspec

dir :: String
dir = "./"

spec :: Spec
spec = do
  describe "End to end tests" $ do
    entries <- runIO $ listDirectory dir
    let fileNames = [entry | entry <- entries, takeExtension entry == ".fun"]
    mapM_ (\fileName -> it fileName (oneFile fileName)) fileNames
  where
    oneFile fileName =
      let outFile = replaceExtension fileName ".out"
          debugFile = replaceExtension fileName ".debug"
          okFile = dir </> replaceExtension fileName ".ok"
          funFile = dir </> replaceExtension fileName ".fun"
       in do
            code <- readFile funFile
            _ <- withFile outFile WriteMode $ \outHandle ->
              withFile debugFile WriteMode $ \debugHandle ->
                runner outHandle debugHandle code
            expectedOut <- readFile okFile
            actualOut <- readFile outFile
            actualOut `shouldBe` expectedOut
