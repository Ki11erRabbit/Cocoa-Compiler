module Compiler.SSA.File where

import Compiler.SSA.Shared
import Compiler.SSA.Class

data File = File
  { packageDec :: PackageDec
  , imports :: [ImportDec]
  , primaryClass :: Class
  } deriving (Show, Eq)


data PackageDec = PackageDec Path
  deriving (Show, Eq)

data ImportDec = ImportDec Path
  deriving (Show, Eq)
