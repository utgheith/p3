{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}

module PackageInfo_p3
  ( name,
    version,
    synopsis,
    copyright,
    homepage,
  )
where

import Data.Version (Version (..))
import Prelude

name :: String
name = "p3"

version :: Version
version = Version [0, 1, 0, 0] []

synopsis :: String
synopsis = ""

copyright :: String
copyright = "2025 utgheith"

homepage :: String
homepage = "https://github.com/utgheith/p3#readme"
