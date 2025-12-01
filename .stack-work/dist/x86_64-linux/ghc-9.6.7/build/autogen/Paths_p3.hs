{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_p3 (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/mjcs27/haskell/p3/.stack-work/install/x86_64-linux/1f4f57381e272e735eb8f5240f5fff90e590d67901489b72ba5f8e92eefe28ad/9.6.7/bin"
libdir     = "/home/mjcs27/haskell/p3/.stack-work/install/x86_64-linux/1f4f57381e272e735eb8f5240f5fff90e590d67901489b72ba5f8e92eefe28ad/9.6.7/lib/x86_64-linux-ghc-9.6.7/p3-0.1.0.0-75U2a6G3mHfIFZxtosBpB4"
dynlibdir  = "/home/mjcs27/haskell/p3/.stack-work/install/x86_64-linux/1f4f57381e272e735eb8f5240f5fff90e590d67901489b72ba5f8e92eefe28ad/9.6.7/lib/x86_64-linux-ghc-9.6.7"
datadir    = "/home/mjcs27/haskell/p3/.stack-work/install/x86_64-linux/1f4f57381e272e735eb8f5240f5fff90e590d67901489b72ba5f8e92eefe28ad/9.6.7/share/x86_64-linux-ghc-9.6.7/p3-0.1.0.0"
libexecdir = "/home/mjcs27/haskell/p3/.stack-work/install/x86_64-linux/1f4f57381e272e735eb8f5240f5fff90e590d67901489b72ba5f8e92eefe28ad/9.6.7/libexec/x86_64-linux-ghc-9.6.7/p3-0.1.0.0"
sysconfdir = "/home/mjcs27/haskell/p3/.stack-work/install/x86_64-linux/1f4f57381e272e735eb8f5240f5fff90e590d67901489b72ba5f8e92eefe28ad/9.6.7/etc"

getBinDir     = catchIO (getEnv "p3_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "p3_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "p3_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "p3_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "p3_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "p3_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
