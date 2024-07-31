module Compiler.SSA.File where

import Compiler.Ast.File as Ast
import Compiler.SSA.Shared
import Compiler.SSA.Class

data File = File
  { packageDec :: Ast.PackageDec
  , imports :: [Ast.ImportDec]
  , primaryClass :: Compiler.SSA.Class.Class
  } deriving (Show, Eq)

convertFile :: Ast.File -> Compiler.SSA.File.File
convertFile Ast.File{ Ast.packageDec=packageDec, Ast.imports=imports, Ast.primaryClass=primaryClass} = Compiler.SSA.File.File
  { Compiler.SSA.File.packageDec=packageDec
  , Compiler.SSA.File.imports=imports
  , Compiler.SSA.File.primaryClass=convertClass primaryClass
  }

