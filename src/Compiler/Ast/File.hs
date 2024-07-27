module Compiler.Ast.File where

import Compiler.Ast.Shared
import Compiler.Ast.Class

data File = File
  { packageDec :: PackageDec
  , imports :: [ImportDec]
  , primaryClass :: Class
  } deriving (Show, Eq)

data PackageDec = PackageDec Path
  deriving (Show, Eq)

data ImportDec = ImportDec Path
  deriving (Show, Eq)

