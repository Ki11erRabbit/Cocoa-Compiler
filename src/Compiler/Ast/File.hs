module Compiler.Ast.File where

import Compiler.Ast.Shared
import Compiler.Ast.Class

data File = File
  { packageDec :: PackageDec
  , imports :: [ImportAst]
  , primaryClass :: Class
  } deriving (Show)

data PackageDec = PackageDec Path
  deriving (Show)

data ImportAst = ImportAst Path
  deriving (Show)

